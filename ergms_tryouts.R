library(dplyr)
library(ggraph)
library(tibble)
library(targets)
library(ergmito)
library(tidygraph)

data("fivenets")
fivenets[[1]]

tar_load(parties_2022)
tar_load(eligible_municipalities_2parties_and_more)

parties_2022 %>% 
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

d1 <- parties_2022 %>% 
    filter(KODZASTUP == 500496)

nodes <- tibble(
    node_key = parties_2022 %>% 
        filter(ZKRATKAN8 != "NK") %>% 
        pull(ZKRATKAN8) %>% 
        unique(), 
)

edges1 <- d1 %>% 
    filter(ZKRATKAN8 != "NK") %>% 
    group_by(KODZASTUP, NAZEVCELK) %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    group_by(NAZEVCELK) %>% 
    group_map(~create_dyad(.x)) %>% 
    bind_rows() %>% 
    rename(from = V1, to = V2)

nodes1 <- nodes %>% 
    filter(node_key %in% d1$ZKRATKAN8)

g1 <- tbl_graph(
    nodes = nodes1, 
    edges = edges1, 
    directed = FALSE
) %>% as.igraph()

n1 <- as.network(
    edges1, 
    vertices = nodes1, 
    directed = FALSE
)

ergm(n1 ~ edges + triangle, force = TRUE)

