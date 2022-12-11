pass <- function(x){x}

parse_cpp_row <- function(x){
    tibble::tibble(
        PSTRANA = x %>% html_node("pstrana") %>% html_text() %>% as.numeric(), 
        NAZEV_STRP = x %>% html_node("nazev_strp") %>% html_text(), 
        ZKRATKAP30 = x %>% html_node("zkratkap30") %>% html_text(),
        ZKRATKAP8 = x %>% html_node("zkratkap8") %>% html_text() 
    )
}

read_cpp_xml <- function(path){
    read_html(path, encoding = "WINDOWS-1250") %>%
        html_nodes("cpp_row") %>%
        purrr::map_df(., parse_cpp_row) %>%
        select(PSTRANA, ZKRATKAP8)
}

parse_cns_row <- function(x){
    tibble::tibble(
        NSTRANA = x %>% html_node("nstrana") %>% html_text() %>% as.numeric(), 
        NAZEV_STRN = x %>% html_node("nazev_strn") %>% html_text(), 
        ZKRATKAN30 = x %>% html_node("zkratkan30") %>% html_text(),
        ZKRATKAN8 = x %>% html_node("zkratkan8") %>% html_text() 
    )
}

read_cns_xml <- function(path){
    read_html(path, encoding = "WINDOWS-1250") %>%
        html_nodes("cns_row") %>%
        purrr::map_df(., parse_cns_row) %>%
        select(NSTRANA, ZKRATKAN8)
}

read_municipal_parties <- function(path, cleanup_f = pass){
    read_excel(path) %>%
        cleanup_f %>%
        select(KODZASTUP, COBVODU, POR_STR_HL, OSTRANA, NAZEVCELK)
}

merge_and_recode_titles <- function(df){
    df %>%
        mutate(TITULY = case_when(!is.na(TITULPRED) & !is.na(TITULZA) ~ paste(TITULPRED, ", ", TITULZA), 
                                  !is.na(TITULPRED) ~ TITULPRED, 
                                  !is.na(TITULZA) ~ TITULZA,
                                  TRUE ~ NA_character_), 
               TITUL_KATEGORIE = categorize_titles(TITULY))
}

categorize_titles <- function(x){
    x <- tolower(x)
    x <- dplyr::case_when(grepl("\\bprof\\b", x) ~ "Professor", # prof.
                          grepl("\\bdoc\\b", x) ~ "Associate Professor (docent)", # doc.
                          grepl("([a-z]+dr|ph\\.+d|phd|th\\.d|csc|drsc|dr)\\b", x) ~ "Doctor",
                          grepl("\\b(ma|m[a-z]{2}|ing)\\b", x) ~ "Master", #Mgr, MgA, MA
                          grepl("\\b(bc|ba|bsc)\\b", x) ~ "Bachelor", #Bc, BcA, BA, BSc
                          TRUE ~ "No title")
    factor(x, levels = c("No title", "Bachelor", "Master",
                         "Doctor", "Associate Professor (docent)", "Professor"),
           ordered = TRUE)
}

read_cpp <- function(path, cleanup_f = pass){
    read_excel(path) %>%
        cleanup_f %>%
        select(PSTRANA, ZKRATKAP8)
}

read_cns <- function(path, cleanup_f = pass){
    read_excel(path) %>%
        cleanup_f %>%
        select(NSTRANA, ZKRATKAN8)
}
read_municipal_candidates <- function(list_path, parties_df, cpp_df, cns_df, 
                                      cleanup_f = pass){
    year <- as.numeric(stringr::str_extract(list_path, "[0-9]{4}"))
    read_excel(list_path) %>%
        cleanup_f %>%
        left_join(., parties_df, by = c("KODZASTUP", "COBVODU", "POR_STR_HL", "OSTRANA")) %>%
        left_join(., cpp_df, by = "PSTRANA") %>%
        left_join(., cns_df, by = "NSTRANA") %>%
        mutate(row_id = row_number(), 
               ROK_NAROZENI = year - VEK) %>%
        merge_and_recode_titles
}

# Počty obcí, ve kterých strana někoho nominovala
count_candidates_in_party <- function(candidates_df){
    candidates_df %>% 
        ungroup %>% 
        count(KODZASTUP, NAZEVCELK, ZKRATKAN8) %>% 
        mutate(coalition_name = paste0(KODZASTUP, "_", NAZEVCELK)) %>% 
        select(municipality_id = KODZASTUP, coalition_name, party = ZKRATKAN8, n) %>% 
        filter(!is.na(party))
}

summarise_top_municipal_parties <- function(df){
    df %>% 
        count(party, sort = TRUE) %>% 
        mutate(party = ifelse(n >= 10, 
                              party, "Ostatní")) %>% 
        group_by(party) %>% 
        summarise(n = sum(n)) %>% 
        arrange(party)
}

create_cooccurrence_matrix <- function(coalitions_df){
    coalition <- factor(coalitions_df$coalition_name)
    party <- factor(coalitions_df$party)
    
    s <- sparseMatrix(
        as.numeric(coalition), 
        as.numeric(party),
        dimnames = list(
            as.character(levels(coalition)), 
            as.character(levels(party))),
        x = 1)
    
    # calculating co-occurrences
    crossprod(s)
}

create_dyad <- function(df){
    df %>% 
        pull(ZKRATKAN8) %>% 
        combn(., m = 2) %>% 
        t() %>% 
        as.data.frame()
}
