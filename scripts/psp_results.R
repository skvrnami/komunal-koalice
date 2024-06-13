# Script for downloading data from CZSO

library(rvest)
library(readxl)

okresy <- read_excel("data/PS_nuts.xlsx", skip = 3) %>% 
    janitor::clean_names() %>% 
    filter(nchar(nuts) == 6)

nuts <- okresy$nuts[1]

parse_obec <- function(obec){
    obec_details <- tibble(kod_zastup = html_attr(obec, "cis_obec"), 
           naz_obec = html_attr(obec, "naz_obec"), 
           typ_obec = html_attr(obec, "typ_obec"))
    
    vysledky <- obec %>% 
        html_nodes("hlasy_strana") %>% 
        purrr::map_df(., function(strana) {
            tibble(
                kstrana = html_attr(strana, "kstrana"), 
                hlasy = html_attr(strana, "hlasy"), 
                proc_hlasu = html_attr(strana, "proc_hlasu")
            )
        })
    
    bind_cols(obec_details, vysledky)
}

get_results <- function(okres){
    url <- glue::glue("https://volby.cz/pls/ps2021/vysledky_okres?nuts={okres}")
    read_html(url) %>% 
        html_nodes("obec") %>% 
        purrr::map_df(., parse_obec)
}

kstrany <- read_excel("data/PS2021/psrkl.xlsx") %>% 
    janitor::clean_names() %>% 
    select(kstrana, nazev_strk)
psp_2021_results <- purrr::map_df(okresy$nuts, get_results)

mcasti <- read_csv(here("data", "VAZ0044_0043_CS.csv")) %>% 
    select(kod_mc = chodnota1, kod_zastup2 = chodnota2, obec = text2) %>% 
    mutate(kod_mc = as.character(kod_mc), 
           kod_zastup2 = as.character(kod_zastup2))

psp_2021 <- psp_2021_results %>% 
    left_join(., kstrany %>% mutate(kstrana = as.character(kstrana)), by = "kstrana") %>% 
    filter(typ_obec != "MCMO")

saveRDS(psp_2021, "data/psp_2021.rds")

get_results_2017 <- function(okres){
    url <- glue::glue("https://volby.cz/pls/ps2017nss/vysledky_okres?nuts={okres}")
    read_html(url) %>% 
        html_nodes("obec") %>% 
        purrr::map_df(., parse_obec)
}

kstrany_2017 <- read_excel("data/PS2017/psrkl.xlsx") %>% 
    janitor::clean_names() %>% 
    select(kstrana, nazev_strk)
psp_2017_results <- purrr::map_df(okresy$nuts, get_results_2017)
psp_2017 <- psp_2017_results %>% 
    left_join(., kstrany_2017 %>% mutate(kstrana = as.character(kstrana)), by = "kstrana") %>% 
    filter(typ_obec != "MCMO")

saveRDS(psp_2017, "data/psp_2017.rds")

psp_2017_recoded <- psp_2017 %>% 
    mutate(koalice = case_when(
        nazev_strk %in% c("Občanská demokratická strana", 
                       "TOP 09", "Křesťan.a demokrat.unie-Českosl.strana lidová") ~ "SPOLU", 
        nazev_strk %in% c("STAROSTOVÉ A NEZÁVISLÍ", 
                       "Česká pirátská strana") ~ "PirSTAN", 
        nazev_strk == "Strana svobodných občanů" ~ "TSS",
        TRUE ~ "Ostatní"
        ), 
        hlasy = as.numeric(hlasy)
    ) %>% 
    group_by(koalice, kod_zastup) %>% 
    summarise(hlasy = sum(hlasy)) %>% 
    ungroup %>% 
    group_by(kod_zastup) %>% 
    mutate(proc_hlasu = round(hlasy / sum(hlasy) * 100, 2)) 
