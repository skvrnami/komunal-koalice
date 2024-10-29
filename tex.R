library(targets)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(coefplot)
library(patchwork)
library(modelsummary)

tar_load(matches("m[0-7]{1}"))
tar_load(desc_table)
tar_load(desc_table2)
tar_load(match_model2)
tar_load(match_model2b)

desc_table_tex <- desc_table %>% 
    mutate(name = recode(name, 
                         "created_2018"="Local PEC (2018)", 
                         "spolu" = "Together", 
                         "pirstan" = "PirSTAN", 
                         "tss" = "TSS", 
                         "created_senate" = "Senate PEC (2022)", 
                         "ano_government" = "ANO in local gov.", 
                         "kscm_government" = "KSČM in local gov.",
                         "spolu_government" = "Together in local gov.", 
                         "coalition_size_votes_norm"="Coalition size (normalized)", 
                         "diff_lrgen"="Ideological distance (left-right scale)",
                         "diff_antielite"="Ideological distance (anti-elite scale)",
                         "asymmetry"="Asymmetry",
                         "local_government_dummy"="Same local gov. position",
                         "enep_votes"="ENEP")) %>% 
    knitr::kable(., format = "latex", 
                 digits = 2, 
                 col.names = c("Variable", 
                               "mean (PEC)", 
                               "SD (PEC)", 
                               "N (PEC)",
                               "mean (No PEC)", 
                               "SD (No PEC)", 
                               "N (No PEC)"))

writeLines(desc_table_tex, "paper/desc_table.tex")

desc_table2 %>% 
  knitr::kable(., format = "latex", digits = 2) %>% 
  writeLines(., "paper/desc_table2.tex")

coefs <- c(
    "spolu"="Together", 
    "pirstan"="PirSTAN", 
    "tss"="TSS",
    "created_senate"="Senate PEC (2022)",
    "ano_government"="ANO in local government",
    "kscm_government"="KSČM in local government",
    "spolu:ano_government"="Together × ANO in local gov.",
    "spolu:kscm_government"="Together × KSČM in local gov.",
    "spolu_government"="Together party in local gov.",
    "spolu:spolu_government"="Together × Together party in local gov.",
    "local_government_dummy"="Same local gov. position",
    "spolu:local_government_dummy"="Together × Same local gov. position",
    "created_2018"="Local PEC (2018)",
    "coalition_size_votes_norm"="Coalition size", 
    "log1p(coalition_size_votes)"="Coalition size",
    "coalition_size_votes_norm:asymmetry"="Coalition size × asymmetry", 
    "log1p(coalition_size_votes):asymmetry"="Coalition size × asymmetry", 
    "I(coalition_size_votes_norm^2)"="Coalition size (sq.)",
    "diff_lrgen"="Distance (left-right positions)",
    "diff_antielite"="Distance (anti-elite positions)",
    "diff_lrgen:local_government_dummy"="Ideological distance × Same local gov. position",
    "asymmetry"="Asymmetry",
    "enep_votes"="ENEP", 
    "municipality_size"="Municipality size",
    "coalition" = "National PEC",
    "pct_change" = "Vote share change in GE",
    "local_government_fctBoth in government" = "Local gov. position: Both in government",
    "local_government_fctBoth in opposition" = "Local gov. position: Both in opposition",
    "(Intercept)"="(Intercept)"
)

# Final models --------------------------------------------

# National coalition
# National coalition x electoral results

var_cols <- bind_cols(
  data.frame(x = c("Municipality-level variance", "Dyad-level variance")),
  # as.data.frame(round(get_variance(m0_dyad)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m2_dyad)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m3_dyad)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m3_only_kscm_dyad)$var.intercept, 2)),
  as.data.frame(round(get_variance(m5_dyad)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m6_dyad)$var.intercept, 2))
)

n_groups <- bind_cols(
  data.frame(x = c("N municipalities", "N party dyads")),
  as.data.frame(summary(m2_dyad)$ngrps), 
  as.data.frame(summary(m3_dyad)$ngrps),
  as.data.frame(summary(m3_only_kscm_dyad)$ngrps),
  as.data.frame(summary(m5_dyad)$ngrps),
  as.data.frame(summary(m6_dyad)$ngrps)
)

colnames(var_cols) <- c("x", paste0("m", 1:5))
colnames(n_groups) <- c("x", paste0("m", 1:5))

modelsummary(
    list(m0_dyad, m2_dyad,
         m3_dyad, m3_only_kscm_dyad, 
         # m3_kscm_dyad, 
         m5_dyad, m6_dyad), 
    stars = TRUE,
    fmt = "%.2f",
    metrics = "common", 
    coef_rename = coefs
)

tab1 <- modelsummary(
    list(m2_dyad,
         m3_dyad, m3_only_kscm_dyad, 
         m5_dyad, m6_dyad), 
    stars = TRUE,
    fmt = "%.2f",
    output = "latex_tabular",
    metrics = "common", 
    coef_rename = coefs, 
    add_rows = bind_rows(var_cols, n_groups)
) 
tinytable::save_tt(tab1, "paper/tab1_models_update.tex")

# Robustness checks

# Only national PECs
modelsummary(
    list(
        m1b_dyad_coalitions, 
        m3b_dyad_coalitions,
        m5b_dyad_coalitions, 
        m6b_dyad_coalitions
    ), 
    stars = TRUE,
    fmt = "%.2f",
    metrics = "common", 
    coef_rename = coefs
)

var_cols <- bind_cols(
  data.frame(x = c("Municipality-level variance", "Dyad-level variance")),
  as.data.frame(round(get_variance(m1b_dyad_coalitions)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m5b_dyad_coalitions)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m6b_dyad_coalitions)$var.intercept, 2))
)

n_groups <- bind_cols(
  data.frame(x = c("N municipalities", "N party dyads")),
  as.data.frame(summary(m1b_dyad_coalitions)$ngrps), 
  as.data.frame(summary(m5b_dyad_coalitions)$ngrps),
  as.data.frame(summary(m6b_dyad_coalitions)$ngrps)
)

colnames(var_cols) <- c("x", paste0("m", 1:3))
colnames(n_groups) <- c("x", paste0("m", 1:3))

only_pecs <- modelsummary(
    list(
        m1b_dyad_coalitions, 
        # m3b_dyad_coalitions,
        m5b_dyad_coalitions, 
        m6b_dyad_coalitions
    ), 
    stars = TRUE,
    fmt = "%.2f",
    metrics = "common", 
    output = "latex_tabular",
    coef_rename = coefs, 
    add_rows = bind_rows(var_cols, n_groups)
)

tinytable::save_tt(only_pecs, "paper/only_pecs.tex", overwrite = TRUE)

# Models with ideological distance
modelsummary(
    list(m2b,
         m3b, 
         m3b_kscm, 
         m5b, m6b), 
    stars = TRUE,
    fmt = "%.2f",
    metrics = "common", 
    coef_rename = coefs
)

var_cols <- bind_cols(
  data.frame(x = c("Municipality-level variance", "Dyad-level variance")),
  as.data.frame(round(get_variance(m2b)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m3b)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m3b_kscm)$var.intercept, 2)),
  as.data.frame(round(get_variance(m5b)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m6b)$var.intercept, 2))
)

n_groups <- bind_cols(
  data.frame(x = c("N municipalities", "N party dyads")),
  as.data.frame(summary(m2b)$ngrps), 
  as.data.frame(summary(m3b)$ngrps),
  as.data.frame(summary(m3b_kscm)$ngrps),
  as.data.frame(summary(m5b)$ngrps),
  as.data.frame(summary(m6b)$ngrps)
)

colnames(var_cols) <- c("x", paste0("m", 1:5))
colnames(n_groups) <- c("x", paste0("m", 1:5))


app_tab1 <- modelsummary(
    list(m2b,
         m3b, 
         m3b_kscm, 
         m5b, m6b), 
    stars = TRUE,
    fmt = "%.2f",
    metrics = "common", 
    output = "latex_tabular",
    coef_rename = coefs, 
    add_rows = bind_rows(var_cols, n_groups)
)

tinytable::save_tt(app_tab1, "paper/app_tab1_models_updated.tex", overwrite = TRUE)

var_cols <- bind_cols(
  data.frame(x = c("Dyad-level variance", "Municipality-level variance")),
  as.data.frame(round(get_variance(m2_dyad_small)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m3_dyad_small)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m5_dyad_small)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m6_dyad_small)$var.intercept, 2)),
  as.data.frame(round(get_variance(m2_dyad_large)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m3_dyad_large)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m5_dyad_large)$var.intercept, 2)), 
  as.data.frame(round(get_variance(m6_dyad_large)$var.intercept, 2))
)

n_groups <- purrr::reduce(list(
  as.data.frame(summary(m2_dyad_small)$ngrps) %>% tidyr::pivot_longer(1:2), 
  as.data.frame(summary(m3_dyad_small)$ngrps) %>% tidyr::pivot_longer(1:2), 
  as.data.frame(summary(m5_dyad_small)$ngrps) %>% tidyr::pivot_longer(1:2), 
  as.data.frame(summary(m6_dyad_small)$ngrps) %>% tidyr::pivot_longer(1:2), 
  as.data.frame(summary(m2_dyad_large)$ngrps) %>% tidyr::pivot_longer(1:2), 
  as.data.frame(summary(m3_dyad_large)$ngrps) %>% tidyr::pivot_longer(1:2), 
  as.data.frame(summary(m5_dyad_large)$ngrps) %>% tidyr::pivot_longer(1:2), 
  as.data.frame(summary(m6_dyad_large)$ngrps) %>% tidyr::pivot_longer(1:2)
), ~left_join(.x, .y, by = "name"))


colnames(var_cols) <- c("x", paste0("m", 1:8))
colnames(n_groups) <- c("x", paste0("m", 1:8))


tar_load(c(
  m2_dyad_small, 
  m3_dyad_small, 
  m5_dyad_small, 
  m6_dyad_small,
  m2_dyad_large, 
  m3_dyad_large, 
  m5_dyad_large, 
  m6_dyad_large
))

app_tab2 <- modelsummary(
  list(
    m2_dyad_small, 
    m3_dyad_small, 
    m5_dyad_small, 
    m6_dyad_small,
    m2_dyad_large, 
    m3_dyad_large, 
    m5_dyad_large, 
    m6_dyad_large
  ), 
  fmt = "%.2f",
  estimate  = "{estimate} [{conf.low}, {conf.high}]",
  metrics = "common", 
  output = "latex_tabular",
  coef_rename = coefs, 
  add_rows = bind_rows(var_cols, n_groups)
)

tinytable::save_tt(app_tab2, "paper/app_tab2_models.tex", overwrite = TRUE)

ano_interaction_g <- ggeffects::ggeffect(m3_dyad, terms = c("spolu", "ano_government")) %>% 
    as.data.frame() %>% 
    # filter(x == 1) %>% 
    mutate(x = if_else(x == 1, "Together dyad", "NOT Together dyad"),
           group = if_else(group == 1, "ANO government", 
                           "ANO NOT in government")) %>% 
    ggplot(., aes(x = x, y = predicted, colour = group)) + 
    geom_point(position = position_dodge(width = 0.1)) + 
    geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                   position = position_dodge(width = 0.1)) + 
    theme_minimal(base_size = 15) + 
    theme(legend.position = "top") + 
    scale_colour_viridis_d(end = 0.8) + 
    scale_y_continuous(labels = scales::label_percent(), 
                       limits = c(0, 0.8)) + 
    labs(x = "",
         y = "Predicted probability", 
         colour = "") + 
    guides(colour = guide_legend(nrow = 2))

kscm_interaction_g <- ggeffects::ggeffect(m3_only_kscm_dyad, terms = c("spolu", "kscm_government")) %>% 
    as.data.frame() %>% 
    # filter(x == 1) %>% 
    mutate(x = if_else(x == 1, "Together dyad", "NOT Together dyad"),
           group = if_else(group == 1, "KSČM government", 
                           "KSČM NOT in government"), 
           type = "KSČM") %>% 
    ggplot(., aes(x = x, y = predicted, colour = group)) + 
    geom_point(position = position_dodge(width = 0.1)) + 
    geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                   position = position_dodge(width = 0.1)) + 
    theme_minimal(base_size = 15) + 
    theme(legend.position = "top") + 
    scale_y_continuous(labels = scales::label_percent(), 
                       limits = c(0, 0.8)) + 
    scale_colour_viridis_d(end = 0.8, 
                           option = "C") + 
    labs(x = "",
         y = "", 
         colour = "") + 
    guides(colour = guide_legend(nrow = 2))

ano_interaction_g + kscm_interaction_g

ggsave("paper/gov_interaction.png", dpi = 300, width = 8, height = 4)


ggeffects::ggeffect(m3b_dyad_coalitions, terms = c("ano_government")) %>% 
    as.data.frame() %>% 
    mutate(x = if_else(x == 1, "ANO government", 
                           "ANO NOT in government")) %>% 
    ggplot(., aes(x = 1, y = predicted, colour = x)) + 
    geom_point(position = position_dodge(width = 0.1)) + 
    geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                   position = position_dodge(width = 0.1)) + 
    theme_minimal() + 
    theme(legend.position = "top") + 
    scale_colour_viridis_d(end = 0.8) + 
    scale_y_continuous(labels = scales::label_percent(), 
                       limits = c(0, 0.8)) + 
    labs(x = "",
         y = "Predicted probability", 
         colour = "")

ggeffects::ggeffect(m3b_dyad_coalitions, terms = c("kscm_government")) %>% 
    as.data.frame() %>% 
    mutate(x = if_else(x == 1, "KSČM government", 
                       "KSČM NOT in government")) %>% 
    ggplot(., aes(x = 1, y = predicted, colour = x)) + 
    geom_point(position = position_dodge(width = 0.1)) + 
    geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                   position = position_dodge(width = 0.1)) + 
    theme_minimal() + 
    theme(legend.position = "top") + 
    scale_colour_viridis_d(end = 0.8) + 
    scale_y_continuous(labels = scales::label_percent(), 
                       limits = c(0, 0.8)) + 
    labs(x = "",
         y = "Predicted probability", 
         colour = "")

