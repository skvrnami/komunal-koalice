# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c("tibble", "here", "dplyr", "readxl", "readr",
               "igraph", "ggraph", "ggplot2", "tidygraph", 
               "rvest", "ggrepel", "extrafont", "readr", 
               "lme4", "Matrix", "cem", "MatchIt", "estimatr", 
               "cobalt"), 
  format = "rds" # default storage format
)

# Load the R scripts with your custom functions:
for (file in list.files("R", full.names = TRUE)) source(file)

# Replace the target list below with your own:
list(
  
  # load data ---------------------------------------------
  
  # candidates running in 2018 municipal election
  tar_target(candidates_2018, {
    parties <- read_municipal_parties(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvros.xlsx"))
    cpp <- read_cpp(here("data", "KV2018", "KV2018ciselniky20181004", "cpp.xlsx"))
    cns <- read_cns(here("data", "KV2018", "KV2018ciselniky20181004", "cns.xlsx"))
    read_municipal_candidates(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>%
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "N", 0, 1), 
             JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI)) %>% 
      filter(PLATNOST == 0)
  }), 
  
  # candidates running in 2022 municipal election
  tar_target(candidates_2022, {
    parties <- read_municipal_parties(here("data", "KV2022", "KV2022reg20220929_xlsx", "kvros.xlsx"))
    cpp <- read_cpp(here("data", "KV2022", "KV2022ciselniky20220810a", "cpp.xlsx"))
    cns <- read_cns(here("data", "KV2022", "KV2022ciselniky20220810a", "cns.xlsx"))
    read_municipal_candidates(here("data", "KV2022", "KV2022reg20220929_xlsx", "kvrk.xlsx"), 
                              parties, cpp, cns) %>% 
      mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
             MANDAT = ifelse(MANDAT == "N", 0, 1), 
             JMENO = ifelse(JMENO == toupper(JMENO), 
                            stringr::str_to_title(JMENO, locale = "cs"), 
                            JMENO),
             PRIJMENI = ifelse(PRIJMENI == toupper(PRIJMENI), 
                               stringr::str_to_title(PRIJMENI, locale = "cs"), 
                               PRIJMENI)) %>% 
      filter(PLATNOST == 0)
  }),
  
  # parties in 2018 election and their composition
  tar_target(parties_2018, {
    cns <- read_cns(here("data", "KV2018", "KV2018ciselniky20181004", "cns.xlsx"))
    
    read_excel(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvros.xlsx")) %>% 
      select(KODZASTUP, VSTRANA, NAZEVCELK, SLOZENI) %>% 
      unique() %>% # get rid of duplicities because of multiple electoral districts in some  municipalities
      mutate(SLOZENI = strsplit(SLOZENI, ",")) %>% 
      tidyr::unnest(., SLOZENI) %>% 
      mutate(SLOZENI = as.numeric(SLOZENI)) %>% 
      left_join(., cns, by = c("SLOZENI"="NSTRANA"))
  }),
  
  # parties in 2022 election and their composition
  tar_target(parties_2022, {
    cns <- read_cns(here("data", "KV2022", "KV2022ciselniky20220810a", "cns.xlsx"))
    
    read_excel(here("data", "KV2022", "KV2022reg20220929_xlsx", "kvros.xlsx")) %>% 
      select(KODZASTUP, VSTRANA, NAZEVCELK, SLOZENI) %>% 
      unique() %>% # get rid of duplicities because of multiple electoral districts in some  municipalities
      mutate(SLOZENI = strsplit(SLOZENI, ",")) %>% 
      tidyr::unnest(., SLOZENI) %>% 
      mutate(SLOZENI = as.numeric(SLOZENI)) %>% 
      left_join(., cns, by = c("SLOZENI"="NSTRANA"))
  }), 
  
  # Nominace stranami v obcích
  tar_target(party_candidates_2018, {
    count_candidates_in_party(candidates_2018) %>% 
      filter(party != "NK")
  }), 
  
  tar_target(party_candidates_2022, {
    count_candidates_in_party(candidates_2022) %>% 
      filter(party != "NK")
  }),
  
  # Nominations in municipalities
  tar_target(party_municipalities_2018, {
    summarise_top_municipal_parties(party_candidates_2018)
  }),
  
  tar_target(party_municipalities_2022, {
    summarise_top_municipal_parties(party_candidates_2022)
  }),
  
  # vote share in PS 2021
  
  
  # co-occurrence matrices --------------------------------
  
  # Co-occurrence matrices
  # tar_target(matrix_coalitions_2018, {
  #   party_candidates_2018 %>% 
  #     mutate(party = ifelse(party %in% party_municipalities_2018$party, 
  #                           party, "Ostatní")) %>% 
  #     create_cooccurrence_matrix
  # }),
  # 
  # tar_target(matrix_coalitions_2022, {
  #   party_candidates_2022 %>% 
  #     mutate(party = ifelse(party %in% party_municipalities_2022$party, 
  #                           party, "Ostatní")) %>% 
  #     create_cooccurrence_matrix
  # }),
  
  tar_target(matrix_coalitions_2018, {
    parties_2018 %>% 
      mutate(coalition_name = paste0(KODZASTUP, "_", NAZEVCELK)) %>% 
      rename(party = ZKRATKAN8) %>% 
      mutate(party = ifelse(party %in% party_municipalities_2018$party, 
                            party, "Ostatní")) %>% 
      create_cooccurrence_matrix
  }),
  
  tar_target(matrix_coalitions_2022, {
    parties_2022 %>% 
      mutate(coalition_name = paste0(KODZASTUP, "_", NAZEVCELK)) %>% 
      rename(party = ZKRATKAN8) %>% 
      mutate(party = ifelse(party %in% party_municipalities_2022$party, 
                            party, "Ostatní")) %>% 
      create_cooccurrence_matrix
  }),
  
  # coalitions graphs -------------------------------------
  tar_target(coalitions_graph_2018_en, {
    graph <- graph_from_adjacency_matrix(matrix_coalitions_2018, 
                                         mode = 'undirected', 
                                         weighted = TRUE)
    V(graph)$n_municipalities <- party_municipalities_2018$n
    V(graph)$coalition <- case_when(
      names(V(graph)) %in% c("ODS", "TOP 09", "KDU-ČSL") ~ "Spolu", 
      names(V(graph)) %in% c("Piráti", "STAN") ~ "PirSTAN", 
      names(V(graph)) %in% c("Trikolora", "Svobodní", "Soukromníci") ~ "TSS",
      TRUE ~ "No national PEC"
    )
    
    tbl_g <- as_tbl_graph(graph)
    
    tbl_g %>% 
      activate(nodes) %>% 
      filter(!name %in% c("DSZ", "STO", "SLK", 
                          "SD-SN", "S.cz", "JAUNER", 
                          "Hora 2014", "Ostatní")) %>% 
      ggraph(., layout = "auto") + 
      geom_node_text(aes(label = name), repel = TRUE) + 
      geom_node_point(aes(size = n_municipalities, colour = coalition)) +
      geom_edge_fan(aes(edge_width = weight), alpha = 0.3) + 
      scale_colour_viridis_d(end = 0.8) + 
      theme_graph() + 
      labs(
        title = "Pre-electoral coalitions in the 2018 local elections",
        size = "N of municipalities in which the party run", 
        colour = "PEC in 2021 parliamentary election",
        edge_width = "N of pre-electoral coalitions", 
        caption = "Data: Czech Statistical Office (volby.cz), calculations by author, parties which did not form PECs excluded (except for ANO)"
      )
  }), 
  
  tar_target(coalitions_graph_2018_file_en, {
    ggsave(filename = "output/koalice_2018_en.png", coalitions_graph_2018_en, 
           width = 12, height = 8)
  }),
  
  tar_target(coalitions_graph_2022_en, {
    graph <- graph_from_adjacency_matrix(matrix_coalitions_2022, 
                                         mode = 'undirected', 
                                         weighted = TRUE)
    V(graph)$n_municipalities <- party_municipalities_2022$n
    V(graph)$coalition <- case_when(
      names(V(graph)) %in% c("ODS", "TOP 09", "KDU-ČSL") ~ "Spolu", 
      names(V(graph)) %in% c("Piráti", "STAN") ~ "PirSTAN", 
      names(V(graph)) %in% c("Trikolora", "Svobodní", "Soukromníci") ~ "TSS",
      TRUE ~ "No national PEC"
    )
    
    tbl_g <- as_tbl_graph(graph)
    
    tbl_g %>% 
      activate(nodes) %>% 
      filter(!name %in% c("DSZ", "STO", "SLK", "SD-SN", "S.cz", "JAUNER", 
                          "Hora 2014", "Ostatní", "Ostravak", "VOK", 
                          "NV", "PRO PLZEŇ", "MHNHRM", "COEX", 
                          "DSZ-ZA PR.ZVÍŘ.")) %>% 
      ggraph(., layout = "auto") + 
      geom_node_point(aes(size = n_municipalities, colour = coalition)) +
      geom_edge_fan(aes(edge_width = weight), alpha = 0.3) + 
      geom_node_text(aes(label = name), repel = TRUE) + 
      theme_graph() + 
      scale_colour_viridis_d(end = 0.8) + 
      labs(
        title = "Pre-electoral coalitions in the 2022 local election",
        size = "N of municipalities in which the party run", 
        colour = "PEC in 2021 parliamentary election",
        edge_width = "N of pre-electoral coalitions", 
        caption = "Data: Czech Statistical Office (volby.cz), calculations by author, parties which did not form PECs excluded"
      )
  }), 
  
  tar_target(coalitions_graph_2022_file_en, {
    ggsave(filename = "output/koalice_2022_en.png", coalitions_graph_2022_en, 
           width = 12, height = 8)
  }),
  
  # dyads - possible coalitions ---------------------------
  
  # city districts (městské části) are filtered out, 
  # they are probably dependent on the coalitions on the city level
  
  tar_target(city_districts, {
    read_csv(here("data", "CIS0044_CS_mestske_casti.csv"), 
             locale = locale(encoding = "WINDOWS-1250"))
  }),
  
  # find eligible municipalities
  tar_target(eligible_municipalities_2018, {
    parties_2018 %>% 
      # filter out city districts
      filter(!KODZASTUP %in% city_districts$CHODNOTA) %>% 
      group_by(KODZASTUP) %>% 
      summarise(
        any_cssd = any(ZKRATKAN8 == "ČSSD"), 
        spolu_n = sum(ZKRATKAN8 %in% c("ODS", "TOP 09", "KDU-ČSL"))
      ) %>% 
      filter(any_cssd | spolu_n >= 2)
  }),
  
  tar_target(eligible_municipalities_2022, {
    parties_2022 %>% 
      # filter out city districts
      filter(!KODZASTUP %in% city_districts$CHODNOTA) %>% 
      group_by(KODZASTUP) %>% 
      summarise(
        any_cssd = any(ZKRATKAN8 == "ČSSD"), 
        spolu_n = sum(ZKRATKAN8 %in% c("ODS", "TOP 09", "KDU-ČSL"))
      ) %>% 
      filter(any_cssd | spolu_n >= 2)
  }),
  
  # is there a difference between selection based on parties in 2018 and 2022?
  tar_target(eligible_comparison, {
    full_join(
      eligible_municipalities_2018 %>% 
        rename(any_cssd_2018 = any_cssd, 
               spolu_n_2018 = spolu_n), 
      eligible_municipalities_2022 %>% 
        rename(any_cssd_2022 = any_cssd, 
               spolu_n_2022 = spolu_n), 
      by = "KODZASTUP"
    ) %>% 
      mutate(
        across(where(is.logical), ~ifelse(is.na(.x), FALSE, .x)), 
        across(where(is.numeric), ~ifelse(is.na(.x), 0, .x))
      ) %>% 
      mutate(
        cssd_diff = any_cssd_2022 - any_cssd_2018, 
        spolu_n_diff = spolu_n_2022 - spolu_n_2018
      )
  }),
  
  # yes, CSSD run in fewer municipalities in 2022
  tar_target(cssd_change, {
    eligible_comparison %>% 
      filter(any_cssd_2018 | any_cssd_2022) %>% 
      count(any_cssd_2018, any_cssd_2022, sort = TRUE)
  }), 
  
  # there are some changes within SPOLU parties too 
  tar_target(spolu_change, {
    eligible_comparison %>% 
      filter(spolu_n_2018 > 1 | spolu_n_2022 > 1) %>% 
      count(spolu_n_2018, spolu_n_2022, sort = TRUE)
  }),
  
  # instead of CSSD/Spolu, all municipalities w/ 2 and more parties
  # from national level
  tar_target(eligible_municipalities_2parties_and_more, {
    parties_2022 %>% 
      # filter out city districts
      filter(!KODZASTUP %in% city_districts$CHODNOTA) %>% 
      filter(ZKRATKAN8 != "NK") %>% 
      count(KODZASTUP) %>% 
      filter(n > 1)
  }),
  
  ## all possible dyads -----------------------------------
  tar_target(possible_dyads_2022, {
     municipalities_parties <- parties_2022 %>% 
      filter(KODZASTUP %in% 
               eligible_municipalities_2parties_and_more$KODZASTUP) %>% 
      filter(ZKRATKAN8 != "NK") %>%
      group_by(KODZASTUP) %>% 
      mutate(n = n()) %>% 
      ungroup() %>% 
      # there need to be at least 2 parties for a possibility to create coalition
      filter(n > 1)
       
     unique_municipalities <- unique(
       eligible_municipalities_2parties_and_more$KODZASTUP)
     purrr::map_df(unique_municipalities, function(x) {
       municipalities_parties %>% 
         filter(KODZASTUP == x) %>% 
         create_dyad() %>% 
         mutate(KODZASTUP = x) %>% 
         rename(party1 = V1, party2 = V2)
     })
  }),
  
  ## created coalitions -----------------------------------
  tar_target(created_dyads_2022, {
    coalitions_2022 <- parties_2022 %>% 
      filter(KODZASTUP %in% 
               eligible_municipalities_2parties_and_more$KODZASTUP) %>% 
      filter(ZKRATKAN8 != "NK") %>% 
      group_by(KODZASTUP, NAZEVCELK) %>% 
      mutate(n = n()) %>% 
      filter(n > 1)
    
    unique_municipalities <- unique(coalitions_2022$KODZASTUP)
    
    purrr::map_df(unique_municipalities, function(x) {
      coalitions_2022 %>% 
        filter(KODZASTUP == x) %>% 
        group_by(NAZEVCELK) %>% 
        group_map(~create_dyad(.x)) %>% 
        bind_rows() %>% 
        mutate(KODZASTUP = x) %>% 
        rename(party1 = V1, party2 = V2)
    })
  }),
  
  ## coalitions in 2018 -----------------------------------
  # continuation of coalition from 2018 => higher prob. of el. coalition
  tar_target(created_dyads_2018, {
    coalitions_2018 <- parties_2018 %>% 
      filter(KODZASTUP %in% 
               eligible_municipalities_2parties_and_more$KODZASTUP) %>% 
      filter(ZKRATKAN8 != "NK") %>% 
      group_by(KODZASTUP, NAZEVCELK) %>% 
      mutate(n = n()) %>% 
      filter(n > 1)
    
    unique_municipalities <- unique(coalitions_2018$KODZASTUP)
    
    purrr::map_df(unique_municipalities, function(x) {
      coalitions_2018 %>% 
        filter(KODZASTUP == x) %>% 
        group_by(NAZEVCELK) %>% 
        group_map(~create_dyad(.x)) %>% 
        bind_rows() %>% 
        mutate(KODZASTUP = x) %>% 
        rename(party1 = V1, party2 = V2)
    })
  }),
  
  # Data on candidates in 2018 local election
  tar_target(candidates_stats_2018, {
    party_votes_2018 <- candidates_2018 %>% 
      filter(KODZASTUP %in% eligible_municipalities_2parties_and_more$KODZASTUP) %>% 
      group_by(KODZASTUP, ZKRATKAN8) %>% 
      summarise(n_votes = sum(POCHLASU), .groups = "drop") %>% 
      group_by(KODZASTUP) %>% 
      mutate(pct_votes = n_votes / sum(n_votes) * 100)
    
    candidates_2018 %>% 
      filter(KODZASTUP %in% eligible_municipalities_2parties_and_more$KODZASTUP) %>% 
      filter(ZKRATKAN8 != "NK") %>% 
      group_by(KODZASTUP, ZKRATKAN8) %>% 
      summarise(
        # no of mandates gained in the election
        n_mandates = sum(MANDAT), 
        has_mandate = n_mandates > 0,
        # no of party members nominated by the party
        n_party_members = sum(ZKRATKAP8 == ZKRATKAN8),
        # pct of party members nominated by the party
        pct_party_members = mean(ZKRATKAP8 == ZKRATKAN8) * 100, 
        # mean age
        mean_age = mean(VEK),
        # share of university educated
        pct_uni_education = mean(TITUL_KATEGORIE > "No title") * 100, 
        .groups = "drop"
      ) %>% 
      left_join(., party_votes_2018, by = c("KODZASTUP", "ZKRATKAN8"))
  }),
  
  # composition of local government -----------------------
  # => higher prob. of electoral coalition
  tar_target(paired_cro, {
    readRDS("data/paired_2018.rds")
  }),
  
  tar_target(local_government_parties, {
    cro_data <- paired_cro %>% 
      # FIXME: reshuffles during the term
      select(municipality_id, party_name) %>% 
      unique() %>% 
      left_join(., parties_2018 %>% select(NAZEVCELK, ZKRATKAN8, KODZASTUP), 
                by = c("party_name"="NAZEVCELK", 
                       "municipality_id"="KODZASTUP")) %>% 
      filter(ZKRATKAN8 != "NK") %>% 
      rename(KODZASTUP = municipality_id) %>% 
      mutate(local_government = 1)
    
    missing_loc <- read_excel(here("data", "local_government_missing.xlsx")) %>% 
      mutate(ZKRATKAN8 = strsplit(ZKRATKAN8, ",")) %>% 
      tidyr::unnest(., ZKRATKAN8) %>% 
      mutate(ZKRATKAN8 = stringr::str_trim(ZKRATKAN8), 
             local_government = 1) %>% 
      select(-c(NAZ_OBEC, NAZ_CZNUTS3))
    
    bind_rows(cro_data, 
              missing_loc)
  }),
  
  tar_target(ano_in_local_government, {
    local_government_parties %>% 
      group_by(KODZASTUP) %>% 
      summarise(ano_government = any(ZKRATKAN8 == "ANO"), 
                spolu_government = any(ZKRATKAN8 %in% c(
                  "ODS", "KDU-ČSL", "TOP 09")), 
                kscm_government = any(ZKRATKAN8 == "KSČM"),
                left_government = any(ZKRATKAN8 %in% c("KSČM", "ČSSD", "SPOZ")),
                # kscm_spoz_government = any(ZKRATKAN8 %in% c("KSČM", "SPOZ")),
                far_right_government = any(ZKRATKAN8 %in% c("DSSS", "SPD")), 
                opponent_government = any(ZKRATKAN8 %in% c("ANO", "KSČM", "SPOZ", 
                                                           "DSSS", "SPD")),
                ano_spolu_government = ano_government & spolu_government)
  }),
  
  ## municipality size ------------------------------------
  # - [x] number of inhabitants, 
  # - [x] n of mandates in 2018 => share of mandates in the municipality)
  tar_target(municipality_size, {
    
    mun_size <- read_excel(here("data", "KV2022", "KV2022reg20220810a_xlsx", 
                                "kvrzcoco.xlsx")) %>% 
      select(KODZASTUP, municipality_size = POCOBYV) %>% 
      unique() # %>% 
      # mutate(municipality_size_norm = municipality_)
    
    candidates_2018 %>% 
      group_by(KODZASTUP) %>% 
      summarise(municipality_seats = sum(MANDAT, na.rm = TRUE)) %>% 
      left_join(., mun_size, by = "KODZASTUP")
    
  }),
  
  # number of parties in the municipality
  # higher fragmentation => higher prob. of coalition
  ## effective number of electoral parties ----------------
  tar_target(enep, {
    party_data <- read_excel(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvros.xlsx"))
    
    enep_votes <- party_data %>% 
      group_by(KODZASTUP) %>% 
      mutate(pct_votes = PROCHLSTR / 100) %>% 
      summarise(enep_votes = 1/sum(pct_votes^2))
    
    enep_seats <- party_data %>% 
      group_by(KODZASTUP) %>% 
      filter(MAND_STR > 0) %>% 
      mutate(pct_seats = MAND_STR / sum(MAND_STR)) %>% 
      summarise(enep_seats = 1/sum(pct_seats^2))
    
    full_join(enep_votes, enep_seats, by = "KODZASTUP")
  }),
  
  ## ideological position of parties ----------------------
  tar_target(ches_data, {
    read_csv(here("data", "1999-2019_CHES_dataset_means(v3).csv")) %>% 
      filter(country == 21) %>% 
      group_by(party) %>% 
      filter(year == max(year)) %>% 
      ungroup() %>% 
      select(party, lrgen, lrecon, galtan, people_vs_elite, 
             antielite_salience) %>% 
      mutate(party = recode(party, 
                            "Pirates" = "Piráti", 
                            "SNK ED"="SNK-ED", 
                            "Nezavisl" = "NEZ", 
                            "SZ" = "Zelení", 
                            "SVOBODNI" ="Svobodní",
                            "KDU-CSL"="KDU-ČSL",
                            "TOP09"="TOP 09", 
                            "KSCM"="KSČM", 
                            "ANO2011"="ANO", 
                            "CSSD"="ČSSD"))
  }),
  
  # tar_target(
  #   v_dem_data, {
  #     readRDS("data/CPD_V-Party_R_v2/V-Dem-CPD-Party-V2.rds") %>% 
  #       filter(country_text_id == "CZE") %>% 
  #       filter(!is.na(v2xpa_popul)) %>% 
  #       select(party = v2pashname, year, populism = v2xpa_popul, 
  #              popul_rhetoric = ep_v8_popul_rhetoric) %>% 
  #       filter(party %in% c("ODS", "CSSD", "TOP09", "KDU/CSL", 
  #                           "SZ", "KSCM", "Pi", "ANO", "SPD"))
  #   }
  # ),
  
  # proxies for ideology
  # education of party members for each party in 2018
  # age of party members for each party in 2018
  
  # party resources = pct_party_members
  # number of party members on the list (members / nominated)
  # => more members => lower prob. of coalition
  
  # new party
  
  # senate election ---------------------------------------
  # were Senate election held in the municipality
  tar_target(municipalities_senate, {
    serk <- read_excel("data/senate_2022/serk.xlsx")
    
    read_csv("data/VAZ0043_1057_CS.csv", 
             locale = locale(encoding = "WINDOWS-1250")) %>% 
      select(obec = CHODNOTA1, senat_obvod = CHODNOTA2) %>% 
      mutate(senate_election2022 = senat_obvod %in% serk$OBVOD) %>% 
      group_by(obec) %>% 
      summarise(senate_election2022 = any(senate_election2022))
  }),
  
  # cooperation in the senate election
  tar_target(senate_dyads, {
    serk <- read_excel(here("data", "senate_2022", "serk.xlsx"))
    cvs <- read_excel(here("data", "senate_2022", "SE2022ciselniky20220916", "cvs.xlsx"))
    cns <- read_excel(here("data", "senate_2022", "SE2022ciselniky20220916", "cns.xlsx")) %>% 
      select(NSTRANA, ZKRATKAN8)
    
    senate_coalitions <- cvs %>% 
      filter(VSTRANA %in% serk$VSTRANA) %>% 
      select(VSTRANA, SLOZENI) %>% 
      mutate(SLOZENI = strsplit(SLOZENI, ",")) %>% 
      tidyr::unnest(., SLOZENI) %>% 
      mutate(SLOZENI = as.numeric(SLOZENI)) %>% 
      left_join(., cns, by = c("SLOZENI"="NSTRANA")) %>% 
      group_by(VSTRANA) %>% 
      mutate(n_parties = n()) %>% 
      filter(n_parties > 1)
    
    obce_senat <- read_csv("data/VAZ0043_1057_CS.csv", 
                           locale = locale(encoding = "WINDOWS-1250")) %>% 
      select(KODZASTUP = CHODNOTA1, senat_obvod = CHODNOTA2) %>% 
      mutate(senat_obvod = as.numeric(senat_obvod))
    
    senate_coalition_candidates <- inner_join(serk %>% select(OBVOD, VSTRANA), 
                                              senate_coalitions) 
    
    unique_senate_districts <- unique(senate_coalition_candidates$OBVOD)
    
    purrr::map_df(unique_senate_districts, function(x) {
      senate_coalition_candidates %>% 
        filter(OBVOD == x) %>% 
        group_by(VSTRANA) %>% 
        group_map(~create_dyad(.x)) %>% 
        bind_rows() %>% 
        mutate(OBVOD = x) %>% 
        rename(party1 = V1, party2 = V2)
    }) %>% left_join(., obce_senat, by = c("OBVOD"="senat_obvod")) %>% 
      count(party1, party2, KODZASTUP) %>% 
      rename(senate_dyad_n = n)
  }),
  
  # final data --------------------------------------------
  tar_target(final_df, {
    SPOLU <- c("ODS", "KDU-ČSL", "TOP 09")
    PIRSTAN <- c("Piráti", "STAN")
    TSS <- c("Trikolora", "Svobodní", "Soukromníci")
    
    possible_dyads_2022 %>% 
      left_join(., created_dyads_2022 %>% mutate(created = 1), 
                by = c("party1", "party2", "KODZASTUP")) %>% 
      # DV
      mutate(
        created = ifelse(is.na(created), 0, created), 
      ) %>% 
      left_join(., created_dyads_2018 %>% mutate(created_2018_a = 1), 
                by = c("party1", "party2", "KODZASTUP")) %>% 
      left_join(., created_dyads_2018 %>% mutate(created_2018_b = 1), 
                by = c("party2"="party1", "party1"="party2", "KODZASTUP")) %>% 
      left_join(., senate_dyads %>% mutate(created_senate_a = 1), 
                by = c("party1", "party2", "KODZASTUP")) %>% 
      left_join(., senate_dyads %>% mutate(created_senate_b = 1), 
                by = c("party2"="party1", "party1"="party2", "KODZASTUP")) %>% 
      left_join(., ches_data %>% rename_with(., ~paste0(.x, "_a"), .cols = -party), 
                by = c("party1"="party")) %>% 
      left_join(., ches_data %>% rename_with(., ~paste0(.x, "_b"), .cols = -party), 
                by = c("party2"="party")) %>% 
      left_join(., municipality_size, by = "KODZASTUP") %>% 
      left_join(., ano_in_local_government, by = "KODZASTUP") %>% 
      mutate(across(matches("created_2018_[a-b]"), ~ifelse(is.na(.x), 0, .x)), 
             across(matches("created_senate_[a-b]"), ~ifelse(is.na(.x), 0, .x)), 
             across(matches("senate_dyad_n"), ~ifelse(is.na(.x), 0, .x))) %>% 
      # IV
      mutate(created_2018 = created_2018_a + created_2018_b, 
             created_senate = created_senate_a + created_senate_b,
             senate_dyad_n = senate_dyad_n.x + senate_dyad_n.y, 
             # coalitions running in 2021 parliamentary election
             spolu = party1 %in% SPOLU & 
               party2 %in% SPOLU, 
             pirstan = party1 %in% PIRSTAN & 
               party2 %in% PIRSTAN, 
             tss = party1 %in% TSS & 
               party2 %in% TSS) %>% 
      left_join(., candidates_stats_2018 %>%
                  mutate(contested_2018 = 1) %>% 
                  rename_with(., ~paste0(.x, "_a"),
                              .cols = -c(KODZASTUP, ZKRATKAN8)),
                by = c("KODZASTUP", "party1"="ZKRATKAN8")) %>%
      left_join(., candidates_stats_2018 %>%
                  mutate(contested_2018 = 1) %>% 
                  rename_with(., ~paste0(.x, "_b"),
                              .cols = -c(KODZASTUP, ZKRATKAN8)),
                by = c("KODZASTUP", "party2"="ZKRATKAN8")) %>%
      left_join(., local_government_parties %>% 
                  rename_with(., ~paste0(.x, "_a"),
                              .cols = -c(KODZASTUP, ZKRATKAN8)), 
                by = c("KODZASTUP", "party1"="ZKRATKAN8")) %>% 
      left_join(., local_government_parties %>% 
                  rename_with(., ~paste0(.x, "_b"),
                              .cols = -c(KODZASTUP, ZKRATKAN8)), 
                by = c("KODZASTUP", "party2"="ZKRATKAN8")) %>% 
      left_join(., enep, by = "KODZASTUP") %>% 
      left_join(., municipalities_senate, by = c("KODZASTUP"="obec")) %>% 
      mutate(across(matches("n_mandate"), ~ifelse(is.na(.x), 0, .x)),
             across(matches("n_party_members"), ~ifelse(is.na(.x), 0, .x)),
             across(matches("contested_2018_[a-b]{1}"), ~ifelse(is.na(.x), 0, .x)), 
             across(matches("local_government_[a-b]{1}"), ~ifelse(is.na(.x), 0, .x)), 
             across(matches("has_mandate"), ~ifelse(is.na(.x), FALSE, .x)), 
             across(matches("pct_votes_[a-b]"), ~ifelse(is.na(.x), 0, .x))) %>% 
      mutate(diff_mean_age = abs(mean_age_a - mean_age_b), 
             diff_pct_uni_education = abs(pct_uni_education_a - pct_uni_education_b), 
             share_mandates_a = n_mandates_a / municipality_seats * 100,
             share_mandates_b = n_mandates_b / municipality_seats * 100, 
             coalition_size_votes = pct_votes_a + pct_votes_b,
             coalition_size_seats = share_mandates_a + share_mandates_b,
             any_new = contested_2018_a == 0 | contested_2018_b == 0,
             no_mandates_a = n_mandates_a == 0, 
             no_mandates_b = n_mandates_b == 0, 
             no_mandates = case_when(
               no_mandates_a & no_mandates_b ~ "Both without a mandate", 
               no_mandates_a | no_mandates_b ~ "One without a mandate", 
               TRUE ~ "Both with mandates"
             ),
             local_government_fct = case_when(
               local_government_a == 1 & local_government_b == 1 ~ 
                 "Both in government", 
               local_government_a == 0 & local_government_b == 0 ~ 
                 "Both in opposition",
               TRUE ~ "Different position in local government"
             ) %>% factor(., levels = c(
               "Different position in local government", 
               "Both in government", 
               "Both in opposition"
             )),
             local_government_dummy = as.numeric(
               local_government_a == local_government_b
             ),
             r_pct_party_members_a = if_else(is.na(pct_party_members_a), 
                                             0, pct_party_members_a),
             r_pct_party_members_b = if_else(is.na(pct_party_members_b), 
                                             0, pct_party_members_b),
             pct_party_members = (r_pct_party_members_a + r_pct_party_members_b) / 2,
             n_party_members = (n_party_members_a + n_party_members_b) / municipality_seats,
             diff_lrgen = abs(lrgen_a - lrgen_b), 
             diff_lrecon = abs(lrecon_a - lrecon_b), 
             diff_galtan = abs(galtan_a - galtan_b), 
             diff_antielite = abs(antielite_salience_a - antielite_salience_b),
             diff_position_euclid = sqrt((diff_lrgen - diff_galtan) ^ 2)
             ) %>% 
      mutate(
        asymmetry = case_when(
          share_mandates_a == 0 & share_mandates_b == 0 ~ 0,
          share_mandates_a >= share_mandates_b ~ 
            ((share_mandates_a / (share_mandates_a + share_mandates_b)) - 0.5) * 2, 
          TRUE ~ ((1 - (share_mandates_a / (share_mandates_a + share_mandates_b))) - 0.5) * 2
        ), 
        coalition_size_seats_norm = (coalition_size_seats - mean(coalition_size_seats)) / 
          sd(coalition_size_seats), 
        coalition_size_votes_norm = (coalition_size_votes - mean(coalition_size_votes)) / 
          sd(coalition_size_votes)
      ) %>% 
      select(-c(created_2018_a, created_2018_b, 
                created_senate_a, created_senate_b, 
                senate_dyad_n.x, senate_dyad_n.y)) %>% 
      mutate(across(where(is.logical), ~as.numeric(.x))) %>% 
      # Create dyad name
      mutate(party1b = party1, 
             party2b = party2) %>% 
      mutate(
        party1b = if_else(
          party1b %in% c("ODS", "KDU-ČSL", "TOP 09", 
                         "STAN", "Piráti", 
                         "ANO", "SPD",  "KSČM", 
                         "PŘÍSAHA", "ČSSD", 
                         "Svobodní", "Soukromníci",
                         "Trikolora", # "VOLNÝ blok",
                         "Zelení"), 
          party1b, "Others"), 
        party2b = if_else(
          party2b %in% c("ODS", "KDU-ČSL", "TOP 09", 
                         "STAN", "Piráti", 
                         "ANO", "SPD", "KSČM", 
                         "PŘÍSAHA", "ČSSD", 
                         "Svobodní", "Soukromníci",
                         "Trikolora", # "VOLNÝ blok",
                         "Zelení"), 
          party2b, "Others")
      ) %>% 
      mutate(party1 = if_else(party1b < party2b, party1b, party2b), 
             party2 = if_else(party1b < party2b, party2b, party1b)) %>% 
      select(-c(party1b, party2b)) %>% 
      mutate(dyad_name = paste0(party1, "+", party2))
  }),
  
  tar_target(checks, {
    stopifnot(nrow(final_df) == nrow(possible_dyads_2022))
  }),
  
  tar_target(desc_table, {
    tmp <- final_df %>% 
      group_by(created) %>% 
      select(created, created_2018, spolu, pirstan, tss, created_senate, 
             ano_government, kscm_government, spolu_government, 
             coalition_size_votes_norm, 
             diff_lrgen, local_government_dummy, asymmetry, enep_votes) %>% 
      summarise_all(list(mean = mean, sd = sd), na.rm = TRUE) %>% 
      tidyr::pivot_longer(., cols = 2:ncol(.)) %>% 
      mutate(type = gsub("_", "", stringr::str_extract(name, "_sd|_mean")), 
             name = gsub("_sd|_mean", "", name)) %>% 
      tidyr::pivot_wider(., id_cols = c("created", "name"), 
                         names_from = "type", values_from = "value")
    
    left_join(
      tmp %>% filter(created == 1) %>% 
        rename(mean_pec = mean, sd_pec = sd) %>% 
        select(-created), 
      tmp %>% filter(created == 0) %>% 
        rename(mean_no_pec = mean, sd_no_pec = sd) %>% 
        select(-created), 
      by = "name"
    )
  }),
  
  # tar_target(
  #   dyad_desc_table, {
  #     tmp <- final_df %>% 
  #       group_by(dyad_name) %>% 
  #       summarise(n = n(), n_created = sum(created)) %>% 
  #       mutate(pct_created = n_created / n * 100) %>% 
  #       filter(grepl("ODS|KDU|TOP|ANO|STAN|Piráti|SPD", dyad_name)) 
  #     
  #     tmp %>% 
  #       mutate(spolu = dyad_name %in% c("KDU-ČSL+TOP 09", "KDU-ČSL+ODS", 
  #                                       "ODS+TOP 09")) %>% 
  #       filter(spolu) %>% 
  #       summarise(n = sum(n), n_created = sum(n_created)) %>% 
  #       mutate(pct_created = n_created / n * 100)
  #   }
  # ),
  
  # charts ------------------------------------------------
  tar_target(dyad_heatmap, {
    tmp1 <- final_df %>% 
      group_by(party1, party2) %>% 
      summarise(n = n(), n_created = sum(created), 
                pct_created = n_created / n * 100)
    
    bind_rows(
      tmp1, 
      tmp1 %>% 
        rename(party1 = party2, 
               party2 = party1) 
    ) %>% 
      mutate(label = paste0(n_created, "/", n), 
             colour = if_else(pct_created < 30, "white", "black")) %>% 
      ggplot(., aes(x = party1, y = party2, fill = pct_created)) + 
      geom_tile() + 
      geom_text(aes(label = label, colour = colour)) + 
      theme_minimal() + 
      scale_fill_viridis_c() + 
      scale_colour_manual(values = c("black"="black", "white"="white")) + 
      labs(x = "", y = "", fill = "% of dyads created") + 
      guides(colour = "none") + 
      theme(legend.position = "top", 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }),
  
  tar_target(dyad_heatmap_png, {
    ggsave(plot = dyad_heatmap, "paper2/dyad_heatmap.png", height = 9, width = 9)
  }),
  
  tar_target(diff_parl_coalitions, {
    final_df %>% 
      mutate(coalition = case_when(
        spolu == 1 ~ "SPOLU", 
        pirstan == 1~ "PirSTAN",
        tss == 1 ~ "TSS", 
        TRUE ~ "No parl. coalition"
      ) %>% factor(., levels = c("TSS", "SPOLU", "PirSTAN", 
                                 "No parl. coalition")), 
        created = factor(created, levels = c(0, 1), 
                         labels = c("Dyad not created", "Dyad created"))
      ) %>% 
      count(coalition, created) %>% 
      group_by(coalition) %>% 
      mutate(share = n / sum(n)) %>% 
      ungroup %>% 
      ggplot(., aes(x = factor(coalition), y = share, 
                    fill = factor(created))) + 
      geom_bar(stat = "identity") + 
      scale_fill_viridis_d() + 
      scale_y_continuous(labels = scales::percent) + 
      theme_minimal() + 
      theme(legend.position = "top") + 
      labs(title = "Creation of pre-electoral coalitions in local election",
           subtitle = "depending on coalitions in parl. election",
           x = "Coalition", 
           y = "",
           fill = "",
           caption = "Note: Missing observations excluded")
    # geom_violin(draw_quantiles = 0.5) 
  }),
  
  tar_target(diff_local_government, {
    final_df %>% 
      # filter(municipality_seats >= 15) %>% 
      count(local_government_fct, created) %>% 
      group_by(local_government_fct) %>% 
      mutate(share = n / sum(n)) %>% 
      ungroup %>% 
      ggplot(., aes(x = factor(local_government_fct), y = share, 
                    fill = factor(created, levels = c(0, 1), 
                                  labels = c("Pre-electoral coalition NOT created", 
                                             "Pre-electoral coalition created")))) + 
      geom_bar(stat = "identity") + 
      scale_fill_viridis_d() + 
      scale_y_continuous(labels = scales::percent) + 
      theme_minimal() + 
      theme(legend.position = "top") + 
      labs(title = "Creation of pre-electoral coalitions in local election",
           subtitle = "depending on position in local government",
           x = "Position in local government", 
           y = "",
           fill = "")
  }),
  
  tar_target(diff_lrgen_chart, {
    ggplot(final_df, aes(x = factor(created), y = diff_lrgen)) + 
      geom_boxplot() + 
      theme_minimal() + 
      labs(title = "Creation of pre-electoral coalition",
           subtitle = "depending on ideological position difference",
           x = "Created dyad", 
           y = "Difference in left-right position",
           caption = "Note: Missing observations excluded")
      # geom_violin(draw_quantiles = 0.5) 
  }),
  
  tar_target(diff_lrecon_chart, {
    ggplot(final_df, aes(x = factor(created), y = diff_lrecon)) + 
      geom_boxplot() + 
      theme_minimal() + 
      labs(title = "Creation of pre-electoral coalition",
           subtitle = "depending on ideological position difference",
           x = "Created dyad", 
           y = "Difference in left-right position (econ)",
           caption = "Note: Missing observations excluded")
    # geom_violin(draw_quantiles = 0.5) 
  }),
  
  tar_target(diff_galtan_chart, {
    ggplot(final_df, aes(x = factor(created), y = diff_galtan)) + 
      geom_boxplot() + 
      theme_minimal() + 
      labs(title = "Creation of pre-electoral coalition",
           subtitle = "depending on ideological position difference",
           x = "Created dyad", 
           y = "Difference in GAL-TAN position",
           caption = "Note: Missing observations excluded")
    # geom_violin(draw_quantiles = 0.5) 
  }),
  
  tar_target(diff_euclid_chart, {
    ggplot(final_df, aes(x = factor(created), y = diff_position_euclid)) + 
      geom_boxplot() + 
      theme_minimal() + 
      labs(title = "Creation of pre-electoral coalition",
           subtitle = "depending on ideological position difference",
           x = "Created dyad", 
           y = "Euclid distance in ideological position (LR gen & GAL-TAN)",
           caption = "Note: Missing observations excluded")
    # geom_violin(draw_quantiles = 0.5) 
  }),
  
  # # models ---------------------------------------
  tar_target(m0, {
    glmer(created ~ created_2018 +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m1, {
    glmer(created ~ created_2018 +
            spolu + pirstan + tss +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),
  
  tar_target(m2, {
    glmer(created ~ created_2018 + created_senate +
            spolu + pirstan + tss +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m3, {
    glmer(created ~ created_2018 + created_senate +
            spolu * ano_government +
            pirstan + tss +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m3_kscm, {
    glmer(created ~ created_2018 + created_senate +
            spolu * ano_government +
            spolu * kscm_government +
            pirstan + tss +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m3_left, {
    glmer(created ~ created_2018 + created_senate +
            spolu * ano_government +
            spolu * left_government +
            pirstan + tss +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m4, {
    glmer(created ~ created_2018 + created_senate +
            spolu * spolu_government +
            pirstan + tss +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  # local_government_dummy
  tar_target(m5, {
    glmer(created ~ created_2018 + created_senate +
            local_government_dummy +
            spolu + pirstan + tss +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m6, {
    glmer(created ~ created_2018 + created_senate +
            local_government_fct +
            spolu + pirstan + tss +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),
  
  # ## robustness checks ------------------------------------
  # ### with ideological distance ---------------------------
  tar_target(m0b, {
    glmer(created ~ created_2018 +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            diff_lrgen + diff_antielite + 
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m1b, {
    glmer(created ~ created_2018 +
            spolu + pirstan +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            diff_lrgen + diff_antielite + 
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m2b, {
    glmer(created ~ created_2018 + created_senate +
            spolu + pirstan +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            diff_lrgen + diff_antielite + 
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m3b, {
    glmer(created ~ created_2018 + created_senate +
            spolu * ano_government +
            pirstan +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            diff_lrgen + diff_antielite + 
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m3b_kscm, {
    glmer(created ~ created_2018 + created_senate +
            spolu * ano_government +
            spolu * kscm_government +
            pirstan +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            diff_lrgen + diff_antielite + 
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m4b, {
    glmer(created ~ created_2018 + created_senate +
            spolu * spolu_government +
            pirstan +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            diff_lrgen + diff_antielite + 
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m5b, {
    glmer(created ~ created_2018 + created_senate +
            local_government_dummy +
            spolu + pirstan +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            diff_lrgen + diff_antielite + 
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m6b, {
    glmer(created ~ created_2018 + created_senate +
            spolu + pirstan +
            diff_lrgen * local_government_dummy +
            diff_antielite * local_government_dummy + 
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),

  tar_target(m7b, {
    glmer(created ~ created_2018 + created_senate +
            spolu * local_government_dummy +
            coalition_size_votes_norm + I(coalition_size_votes_norm^2) +
            coalition_size_votes_norm * asymmetry +
            enep_votes + municipality_seats + 
            (1 | KODZASTUP) + (1 | dyad_name),
          family = binomial(link = "probit"),
          glmerControl(optimizer = "bobyqa"),
          data = final_df)
  }),
  # 
  # ## logit models -----------------------------------------
  # tar_target(log_m0, {
  #   glmer(created ~ created_2018 + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m1, {
  #   glmer(created ~ created_2018 + 
  #           spolu + pirstan + tss + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m2, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu + pirstan + tss + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m3, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu * ano_government + 
  #           pirstan + tss + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m3_kscm, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu * ano_government + 
  #           spolu * kscm_government + 
  #           pirstan + tss + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m3_left, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu * ano_government + 
  #           spolu * left_government + 
  #           pirstan + tss + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m4, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu * spolu_government + 
  #           pirstan + tss + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # # local_government_dummy
  # tar_target(log_m5, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           local_government_dummy + 
  #           spolu + pirstan + tss + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m6, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           local_government_fct + 
  #           spolu + pirstan + tss + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # # robustness checks ------------------------------------
  # ## with ideological distance ---------------------------
  # tar_target(log_m0b, {
  #   glmer(created ~ created_2018 + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           diff_lrgen + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m1b, {
  #   glmer(created ~ created_2018 + 
  #           spolu + pirstan +  
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           diff_lrgen + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m2b, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu + pirstan + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           diff_lrgen + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m3b, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu * ano_government + 
  #           pirstan + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           diff_lrgen + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m3b_kscm, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu * ano_government + 
  #           spolu * kscm_government + 
  #           pirstan + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           diff_lrgen + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m4b, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu * spolu_government + 
  #           pirstan + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           diff_lrgen + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m5b, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           local_government_dummy + 
  #           spolu + pirstan + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           diff_lrgen + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m6b, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu + pirstan + 
  #           diff_lrgen * local_government_dummy + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # tar_target(log_m7b, {
  #   glmer(created ~ created_2018 + created_senate + 
  #           spolu * local_government_dummy + 
  #           coalition_size_votes_norm + I(coalition_size_votes_norm^2) + 
  #           coalition_size_votes_norm * asymmetry + 
  #           enep_votes + 
  #           (1 | KODZASTUP), 
  #         family = binomial(link = "logit"), 
  #         glmerControl(optimizer = "bobyqa"),
  #         data = final_df)
  # }),
  # 
  # ## matching senate --------------------------------------
  tar_target(match_senate1, {
    matchit(created_senate ~ diff_lrgen + ano_government + spolu_government +
              kscm_government +
              coalition_size_votes_norm + asymmetry + enep_votes +
              municipality_size + local_government_dummy + created_2018,
            data = final_df %>% filter(!is.na(diff_lrgen)),
            method = "cem")
  }),
  
  tar_target(match_senate_data, {
    match.data(match_senate1)
    
  }),
  
  tar_target(match_model1, {
    lm_robust(created ~ created_senate, data = match_senate_data, 
              weights = match_senate_data$weights)
  }),
  
  # Matching dyads where only in one of them senate election were held
  tar_target(match_senate2, {
    matchit(created_senate ~ diff_lrgen + ano_government + 
              kscm_government + spolu_government + 
              coalition_size_votes_norm + asymmetry + enep_votes + 
              municipality_size + local_government_dummy + created_2018, 
            k2k = TRUE, 
            data = final_df %>% filter(!is.na(diff_lrgen)), 
            method = "cem")
  }), 
  
  tar_target(match_senate_data2, {
    match.data(match_senate2) %>% 
      group_by(subclass) %>% 
      filter(sum(senate_election2022) == 1) %>% 
      ungroup()
  }),
  
  tar_target(match_senate_data2_summary, {
    covs <- match_senate_data2 %>% select(diff_lrgen, ano_government, spolu_government, 
                                          kscm_government, coalition_size_votes_norm, asymmetry, enep_votes, 
                                          municipality_size, local_government_dummy)
    bal.tab(covs, treat = match_senate_data2$created_senate, 
            weights = match_senate_data2$weights, 
            disp = "means",
            stats = c("mean.diffs", "variance.ratios") #, 
            # thresholds = c(m = .1, v = 2)
            )
  }),
  
  tar_target(match_senate_data2_summary_tab, {
    match_senate_data2_summary$Balance %>% 
      purrr::keep(~!all(is.na(.x))) %>% 
      knitr::kable(., format = "latex") %>% 
      writeLines(., "paper/cov_balance.tex")
  }),
  
  tar_target(match_model2, {
    lm_robust(created ~ created_senate, data = match_senate_data2, 
              weights = match_senate_data2$weights)
  }),
  
  tar_target(match_model2b, {
    lm_robust(created ~ created_senate + municipality_size, 
              data = match_senate_data2, 
              weights = match_senate_data2$weights)
  }),
  
  ## Ad hoc calculations for reviewers
  tar_target(
    ches_correlations, {
      lag_ches <- read_csv(here("data", "1999-2019_CHES_dataset_means(v3).csv")) %>% 
        filter(country == 21) %>% 
        select(year, party, lrgen) %>% 
        group_by(party) %>% 
        mutate(lag_lrgen = lag(lrgen)) 
      
      cor(lag_ches$lrgen, lag_ches$lag_lrgen, use = "complete.obs")
    }
  ),
  
  NULL
)
