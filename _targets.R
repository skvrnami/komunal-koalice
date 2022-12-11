# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c("tibble", "here", "dplyr", "readxl", "Matrix", 
               "igraph", "ggraph", "ggplot2", "tidygraph", 
               "rvest", "ggrepel", "extrafont", "readr"), 
  format = "rds" # default storage format
)

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
for (file in list.files("R", full.names = TRUE)) source(file)
# source("other_functions.R") # Source other scripts as needed. # nolint

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
  
  # TODO: all dyads for all municipalities in which CSSD 
  # and at least two of SPOLU (ODS, KDU-CSL, TOP 09) run.
  
  # city districts (městské části) are filtered out, 
  # they are probably dependent on the coalitions on the city level
  
  tar_target(city_districts, {
    read_csv(here("data", "CIS0044_CS_mestske_casti.csv"), 
             locale = locale(encoding = "WINDOWS-1250"))
  }),
  
  # find eligible municipalities
  # FIXME: instead of CSSD/Spolu, all municipalities w/ 2 and more parties
  # from national level
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
  
  # all possible dyads
  # TODO: do I have to include dyad - party + no party ?
  tar_target(possible_dyads_2022, {
     municipalities_parties <- parties_2022 %>% 
      filter(KODZASTUP %in% eligible_municipalities_2022$KODZASTUP) %>% 
      filter(ZKRATKAN8 != "NK") %>%
      group_by(KODZASTUP) %>% 
      mutate(n = n()) %>% 
      ungroup() %>% 
      # there need to be at least 2 parties for a possibility to create coalition
      filter(n > 1)
       
     unique_municipalities <- unique(municipalities_parties$KODZASTUP)
     purrr::map_df(unique_municipalities, function(x) {
       municipalities_parties %>% 
         filter(KODZASTUP == x) %>% 
         create_dyad() %>% 
         mutate(KODZASTUP = x) %>% 
         rename(party1 = V1, party2 = V2)
     })
  }),
  
  # created coalitions 
  tar_target(created_dyads_2022, {
    coalitions_2022 <- parties_2022 %>% 
      filter(KODZASTUP %in% eligible_municipalities_2022$KODZASTUP) %>% 
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
  
  # TODO: mandates gained in 2018 for each party
  # TODO: continuation of coalition from 2018 => higher prob. of el. coalition
  # TODO: composition of local government => higher prob. of electoral coalition
  # TODO: velikost obce
  # TODO: ideological position of parties
  
  # proxies for ideology
  # TODO: education of party members for each party in 2018
  # TODO: age of party members for each party in 2018
  
  # party resources
  # TODO: number of party members on the list (members / nominated)
  # => more members => lower prob. of coalition
  
  # TODO: number of parties in the municipality
  # higher fragmentation => higher prob. of coalition
  
  tar_target(final_df, {
    possible_dyads_2022 %>% 
      left_join(., created_dyads_2022 %>% mutate(created = 1), 
                by = c("party1", "party2", "KODZASTUP")) %>% 
      mutate(
        created = ifelse(is.na(created), 0, created), 
        spolu = party1 %in% c("ODS", "KDU-ČSL", "TOP 09") & 
          party2 %in% c("ODS", "KDU-ČSL", "TOP 09")
      )
  }),
  
  NULL
)
