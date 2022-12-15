# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c("tibble", "here", "dplyr", "readxl", "Matrix", 
               "igraph", "ggraph", "ggplot2", "tidygraph", 
               "rvest", "ggrepel", "extrafont", "readr", 
               "lme4"), 
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
  tar_target(coalitions_graph_2018, {
    graph <- graph.adjacency(matrix_coalitions_2018, 
                             mode = 'undirected', 
                             weighted = TRUE)
    V(graph)$n_municipalities <- party_municipalities_2018$n
    
    tbl_g <- as_tbl_graph(graph)
    
    tbl_g %>% 
      activate(nodes) %>% 
      filter(!name %in% c("DSZ", "STO", "SLK", 
                          "SD-SN", "S.cz", "JAUNER", 
                          "Hora 2014", "Ostatní")) %>% 
      ggraph(., layout = "auto") + 
      geom_node_point(aes(size = n_municipalities), alpha = 0.5, colour = "black") +
      geom_edge_link(aes(edge_width = weight), alpha = 0.3) + 
      geom_node_text(aes(label = name)) + 
      theme_graph() + 
      labs(
        title = "Předvolební koalice během obecních voleb 2018",
        size = "Počet obcí, kde strana kandidovala", 
        edge_width = "Počet koalic mezi stranami", 
        caption = "Data: ČSÚ (volby.cz), výpočet autor, bez nekoalujících stran (kromě ANO)"
      )
  }), 
  
  tar_target(coalitions_graph_2018_file, {
    ggsave(filename = "output/koalice_2018.png", coalitions_graph_2018, 
           width = 12, height = 8)
  }),
  
  tar_target(coalitions_graph_2022, {
    graph <- graph.adjacency(matrix_coalitions_2022, 
                             mode = 'undirected', 
                             weighted = TRUE)
    V(graph)$n_municipalities <- party_municipalities_2022$n
    
    tbl_g <- as_tbl_graph(graph)
    
    tbl_g %>% 
      activate(nodes) %>% 
      filter(!name %in% c("DSZ", "STO", "SLK", "SD-SN", "S.cz", "JAUNER", 
                          "Hora 2014", "Ostatní", "Ostravak", "VOK", 
                          "NV", "PRO PLZEŇ", "MHNHRM", "COEX", 
                          "DSZ-ZA PR.ZVÍŘ.")) %>% 
      ggraph(., layout = "auto") + 
      geom_node_point(aes(size = n_municipalities), alpha = 0.5, colour = "black") +
      geom_edge_link(aes(edge_width = weight), alpha = 0.3) + 
      geom_node_text(aes(label = name)) + 
      theme_graph() + 
      labs(
        title = "Předvolební koalice během obecních voleb 2022",
        size = "Počet obcí, kde strana kandidovala", 
        edge_width = "Počet koalic mezi stranami", 
        caption = "Data: ČSÚ (volby.cz), výpočet autor, bez nekoalujících stran"
      )
  }), 
  
  tar_target(coalitions_graph_2022_file, {
    ggsave(filename = "output/koalice_2022.png", coalitions_graph_2022, 
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
    candidates_2018 %>% 
      filter(KODZASTUP %in% created_dyads_2022$KODZASTUP) %>% 
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
      )
  }),
  
  # TODO: composition of local government => higher prob. of electoral coalition
  
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
      summarise(municipality_seats = sum(MANDAT)) %>% 
      left_join(., mun_size, by = "KODZASTUP")
    
  }),
  
  ## ideological position of parties ----------------------
  tar_target(ches_data, {
    read_csv(here("data", "1999-2019_CHES_dataset_means(v3).csv")) %>% 
      filter(country == 21) %>% 
      group_by(party) %>% 
      filter(year == max(year)) %>% 
      ungroup() %>% 
      select(party, lrgen, lrecon, galtan) %>% 
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
  
  # proxies for ideology
  # education of party members for each party in 2018
  # age of party members for each party in 2018
  
  # party resources
  # number of party members on the list (members / nominated)
  # => more members => lower prob. of coalition
  
  # TODO: number of parties in the municipality
  # higher fragmentation => higher prob. of coalition

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
      left_join(., ches_data %>% rename_with(., ~paste0(.x, "_a"), .cols = -party), 
                by = c("party1"="party")) %>% 
      left_join(., ches_data %>% rename_with(., ~paste0(.x, "_b"), .cols = -party), 
                by = c("party2"="party")) %>% 
      left_join(., municipality_size, by = "KODZASTUP") %>% 
      mutate(across(matches("created_2018_[a-b]"), ~ifelse(is.na(.x), 0, .x))) %>% 
      # IV
      mutate(created_2018 = created_2018_a + created_2018_b, 
             # coalitions running in 2021 parliamentary election
             spolu = party1 %in% SPOLU & 
               party2 %in% SPOLU, 
             pirstan = party1 %in% PIRSTAN & 
               party2 %in% PIRSTAN, 
             tss = party1 %in% TSS & 
               party2 %in% TSS) %>% 
      left_join(., candidates_stats_2018 %>%
                  rename_with(., ~paste0(.x, "_a"),
                              .cols = -c(KODZASTUP, ZKRATKAN8)),
                by = c("KODZASTUP", "party1"="ZKRATKAN8")) %>%
      left_join(., candidates_stats_2018 %>%
                  rename_with(., ~paste0(.x, "_b"),
                              .cols = -c(KODZASTUP, ZKRATKAN8)),
                by = c("KODZASTUP", "party2"="ZKRATKAN8")) %>%
      mutate(across(matches("n_mandate"), ~ifelse(is.na(.x), 0, .x)), 
             across(matches("has_mandate"), ~ifelse(is.na(.x), FALSE, .x))) %>% 
      mutate(diff_mean_age = abs(mean_age_a - mean_age_b), 
             diff_pct_uni_education = abs(pct_uni_education_a - pct_uni_education_b), 
             share_mandates_a = n_mandates_a / municipality_seats * 100,
             share_mandates_b = n_mandates_b / municipality_seats * 100, 
             diff_lrgen = abs(lrgen_a - lrgen_b), 
             diff_lrecon = abs(lrecon_a - lrecon_b), 
             diff_galtan = abs(galtan_a - galtan_b), 
             diff_position_euclid = sqrt((diff_lrgen - diff_galtan) ^ 2)
             ) %>% 
      select(-c(created_2018_a, created_2018_b))
  }),
  
  tar_target(checks, {
    stopifnot(nrow(final_df) == nrow(possible_dyads_2022))
  }),
  
  # charts ------------------------------------------------
  
  tar_target(diff_parl_coalitions, {
    final_df %>% 
      mutate(coalition = case_when(
        spolu ~ "SPOLU", 
        pirstan ~ "PirSTAN",
        tss ~ "TSS", 
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
  
  # estimate models ---------------------------------------
  # TODO: models
  tar_target(m1, {
    glmer(created ~ created_2018 + 
            spolu + pirstan + tss + 
            (1 | KODZASTUP), 
          family = binomial(link = "probit"), 
          data = final_df)
  }),
  
  tar_target(m2, {
    glmer(created ~ created_2018 + 
            spolu + pirstan + tss + 
            log(municipality_size) + 
            (1 | KODZASTUP), 
          family = binomial(link = "probit"), 
          data = final_df)
  }),
  
  tar_target(m3, {
    glmer(created ~ created_2018 + 
            spolu * log(municipality_size) + 
            (1 | KODZASTUP), 
          family = binomial(link = "probit"), 
          data = final_df)
  }),
  
  ## robustness checks ------------------------------------
  tar_target(m2_bez_prahy, {
    glmer(created ~ created_2018 + 
            spolu + pirstan + tss + 
            log(municipality_size) + 
            (1 | KODZASTUP), 
          family = binomial(link = "probit"), 
          data = final_df %>% filter(KODZASTUP != 554782))
  }),
  
  # tar_target(tex_models, {
  #   modelsummary(m1, stars = TRUE,
  #                fmt = "%.2f",
  #                output = "latex")
  # }),
  
  NULL
)
