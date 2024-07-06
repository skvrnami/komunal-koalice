library(targets)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(coefplot)
library(patchwork)
library(modelsummary)

tar_load(matches("m[0-7]{1}"))
tar_load(desc_table)
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
                         "diff_lrgen"="Ideological distance",
                         "asymmetry"="Asymmetry",
                         "local_government_dummy"="Same local gov. position",
                         "enep_votes"="ENEP")) %>% 
    knitr::kable(., format = "latex", 
                 digits = 2, 
                 col.names = c("Variable", 
                               "mean (PEC)", 
                               "SD (PEC)", 
                               "mean (No PEC)", 
                               "SD (No PEC)"))

writeLines(desc_table_tex, "paper/desc_table.tex")

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
    coef_rename = coefs
) 
tinytable::save_tt(tab1, "paper2/tab1_models.tex")

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
    coef_rename = coefs
)

tinytable::save_tt(only_pecs, "paper2/only_pecs.tex", overwrite = TRUE)

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

app_tab1 <- modelsummary(
    list(m2b,
         m3b, 
         m3b_kscm, 
         m5b, m6b), 
    stars = TRUE,
    fmt = "%.2f",
    metrics = "common", 
    output = "latex_tabular",
    coef_rename = coefs
)

tinytable::save_tt(app_tab1, "paper2/app_tab1_models.tex", overwrite = TRUE)

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
    theme_minimal() + 
    theme(legend.position = "top") + 
    scale_colour_viridis_d(end = 0.8) + 
    scale_y_continuous(labels = scales::label_percent(), 
                       limits = c(0, 0.8)) + 
    labs(x = "",
         y = "Predicted probability", 
         colour = "")

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
    theme_minimal() + 
    theme(legend.position = "top") + 
    scale_y_continuous(labels = scales::label_percent(), 
                       limits = c(0, 0.8)) + 
    scale_colour_viridis_d(end = 0.8, 
                           option = "C") + 
    labs(x = "",
         y = "", 
         colour = "")

ano_interaction_g + kscm_interaction_g

ggsave("paper2/gov_interaction.png", dpi = 300, width = 8, height = 4)


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

## Old
modelsummary(list(m0_dyad, m1_dyad, m0b, m1b), stars = TRUE, 
             fmt = "%.2f",
             metrics = "common", 
             title = "Baseline models",
             output = "html",
             coef_rename = coefs)


baseline_models <- modelsummary(list(m0_dyad, m1_dyad, m0b, m1b), stars = TRUE, 
                                fmt = "%.2f",
                                output = "latex",
                                metrics = "common", 
                                title = "Baseline models",
                                coef_rename = coefs)
writeLines(baseline_models, "paper/baseline_models.tex")

tab_model(list("Model 1" = m0_dyad, "Model 2" = m0b, 
               "Model 3" = m1_dyad, "Model 4" = m1b), 
          pred.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          show.p = FALSE)

modelsummary(list(m0_dyad, m1_dyad, m1b_dyad, m1b_dyad_coalitions), 
             stars = TRUE, 
             fmt = "%.2f",
             metrics = "common", 
             title = "Baseline models",
             output = "html",
             coef_rename = coefs)


m1c_coalitions2 <- glmer(created ~ created_2018 +
          coalition_fct * pct_change_norm + 
          log1p(coalition_size_votes) + 
          log1p(coalition_size_votes) * asymmetry +
          enep_votes + # log1p(municipality_seats) + 
          (1 | KODZASTUP) + (1 | dyad_name),
      family = binomial(link = "probit"),
      glmerControl(optimizer = "bobyqa"),
      data = final_df_results %>% 
          mutate(coalition_fct = case_when(
              spolu == 1 ~ "Spolu", 
              pirstan == 1 ~ "PirSTAN",
              tss == 1 ~ "TSS",
              TRUE ~ "No coalition"
          )))

m3_ano_kscm <- glmer(created ~ created_2018 + created_senate +
          spolu * ano_government +
          spolu * kscm_government +
          pirstan + tss +
          log1p(coalition_size_votes) + 
          log1p(coalition_size_votes) * asymmetry +
          enep_votes + # log1p(municipality_seats) + 
          (1 | KODZASTUP) + (1 | dyad_name),
      family = binomial(link = "probit"),
      glmerControl(optimizer = "bobyqa"),
      data = final_df)

m3_ano_kscm_all <- allFit(m3_ano_kscm)

# Local government position
modelsummary(list(m5_dyad, m6_dyad), 
             stars = TRUE)


# Old models ----------------------------------------------
baseline_models <- modelsummary(list(m0, m0b, m1, m1b), stars = TRUE, 
                                fmt = "%.2f",
                                output = "latex",
                                metrics = "common", 
                                title = "Baseline models",
                                coef_map = coefs)
writeLines(baseline_models, "paper/baseline_models.tex")

tab_model(list("Model 1" = m0, "Model 2" = m0b, 
               "Model 3" = m1, "Model 4" = m1b), 
          pred.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          show.p = FALSE)

tex <- modelsummary(list(m2, m3_kscm, m4, m5), stars = FALSE,
                    fmt = "%.2f",
                    output = "latex",
                    metrics = "common", 
                    coef_map = coefs)
writeLines(tex, "paper/models1.tex")

tab_model(list(m2, m3_kscm, m4, m5), 
          # pred.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          show.p = FALSE)


tex2 <- modelsummary(list(m2b, m3b_kscm, m4b, m5b), stars = TRUE,
                    fmt = "%.2f",
                    output = "latex",
                    metrics = "common", 
                    coef_map = coefs)
writeLines(tex2, "paper/models2.tex")

tex3 <- modelsummary(list(match_model2, match_model2b), 
                     fmt = "%.2f",
                     output = "latex",
                     metrics = "common", 
                     stars = TRUE,
                     coef_map = coefs)
writeLines(tex3, "paper/matching_models.tex")

plot_model(m5, show.values = TRUE, value.offset = .3)
coefplot(m4, 
         newNames = c("tss"="TSS", "pirstan"="PirSTAN", "spolu"="Spolu", 
                      "enep_votes"="ENEP", "asymmetry"="Asymmetry", 
                      "spolu_government"="Spolu party in local government", 
                      "created_senate"="PEC in Senate election (2022)", 
                      "created_2018"="PEC in local election (2018)", 
                      "coalition_size_votes_norm"="Coalition size", 
                      "I(coalition_size_votes_norm^2)"="Coalition size (sq.)", 
                      "spoluTRUE:spolu_governmentTRUE"="Spolu x Spolu in local government"), 
         intercept = FALSE, 
         plot = FALSE) %>% 
    # as.data.frame() %>% 
    filter(Coefficient %in% c("Spolu", "PirSTAN", "TSS", 
                             "PEC in Senate election (2022)", 
                             "Spolu x Spolu in local government")) %>% 
    ggplot(., aes(y = factor(Coefficient, levels = rev(c("Spolu", "PirSTAN", "TSS", 
                             "PEC in Senate election (2022)", 
                             "Spolu x Spolu in local government"))), x = Value)) + 
    geom_point() + 
    geom_linerange(aes(xmin = LowOuter, xmax = HighOuter)) + 
    # scale_y_discrete(breaks = c("(Intercept)", "Spolu")) + 
    geom_vline(xintercept = 0, colour = "gray40") + 
    theme_bw() +
    labs(x = "", y = "Coefficients", title = "Coefficient plot", 
         caption = "Note: Horizontal lines indicate 95% confidence interval. Control variables not shown.")

ggsave("output/coef_plot.png", width = 8, height = 4)    

kscm_interaction <- ggeffects::ggeffect(m3_only_kscm_dyad, terms = c("spolu", "kscm_government"), 
                                        ci_level = 0.95)
kscm_interaction %>% 
    as.data.frame() %>% 
    mutate(
        x = case_when(x == 1 ~ "Together coalition dyad", 
                      x == 0 ~ "Other dyad") %>% factor(),
        group = case_when(group == 1 ~ "KSČM in local government", 
                          group == 0 ~ "KSČM NOT in local government") %>% factor()
    ) %>% 
    ggplot(., aes(x = x, y = predicted, colour = group)) + 
    geom_point(position=position_dodge(0.5)) + 
    geom_linerange(aes(ymin = conf.low, ymax = conf.high), 
                   position=position_dodge(0.5)) + 
    theme_bw(base_size = 13) + 
    theme(legend.position = "top") + 
    scale_colour_viridis_d(option = "D", end = 0.8) + 
    scale_y_continuous(labels = scales::label_percent()) + 
    labs(x = "", y = "Predicted probability", 
         # title = "Predicted formation of PEC", 
         # subtitle = "by Communist Party participation in local government", 
         caption = "Note: Predicted probabilities based on Model 2. Vertical line show 95% confidence interval.",
         colour = "")

ggsave("paper/kscm_interaction.png", dpi = 300, width = 6, height = 4)

ano_interaction <- ggeffects::ggeffect(m3_kscm_dyad, terms = c("spolu", "ano_government"))
ano_interaction %>% 
    as.data.frame() %>% 
    mutate(
        x = case_when(x == 1 ~ "Together coalition dyad", 
                      x == 0 ~ "Other dyad") %>% factor(),
        group = case_when(group == 1 ~ "KSČM in local government", 
                          group == 0 ~ "KSČM NOT in local government") %>% factor()
    ) %>% 
    ggplot(., aes(x = x, y = predicted, colour = group)) + 
    geom_point(position=position_dodge(0.5)) + 
    geom_linerange(aes(ymin = conf.low, ymax = conf.high), 
                   position=position_dodge(0.5)) + 
    theme_bw(base_size = 13) + 
    theme(legend.position = "top") + 
    scale_colour_viridis_d(option = "D", end = 0.8) + 
    scale_y_continuous(labels = scales::label_percent()) + 
    labs(x = "", y = "Predicted probability", 
         # title = "Predicted formation of PEC", 
         # subtitle = "by Communist Party participation in local government", 
         caption = "Note: Predicted probabilities based on Model 2. Vertical line show 95% confidence interval.",
         colour = "")


tar_load(parties_2022)
parties_2022 %>% 
    filter(ZKRATKAN8 != "NK") %>% 
    group_by(KODZASTUP, NAZEVCELK) %>% 
    summarise(n_parties = n()) %>% 
    ungroup %>% 
    count(n_parties)

tar_load(final_df)
ggeffects::ggeffect(m2, "spolu")
ggeffects::ggeffect(m2, "tss")
summary(m2)
