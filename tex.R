library(targets)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(coefplot)
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
    "coalition_size_votes_norm:asymmetry"="Coalition size × asymmetry", 
    "I(coalition_size_votes_norm^2)"="Coalition size (sq.)",
    "diff_lrgen"="Ideological distance",
    "diff_lrgen:local_government_dummy"="Ideological distance × Same local gov. position",
    "asymmetry"="Asymmetry",
    "enep_votes"="ENEP", 
    "municipality_size"="Municipality size",
    "(Intercept)"="(Intercept)"
)

baseline_models <- modelsummary(list(m0, m0b, m1, m1b), stars = TRUE, 
                                fmt = "%.2f",
                                output = "latex",
                                metrics = "common", 
                                title = "Baseline models",
                                coef_map = coefs)
writeLines(baseline_models, "paper/baseline_models.tex")

tex <- modelsummary(list(m2, m3_kscm, m4, m5), stars = TRUE,
                    fmt = "%.2f",
                    output = "latex",
                    metrics = "common", 
                    coef_map = coefs)
writeLines(tex, "paper/models1.tex")

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

kscm_interaction <- ggeffects::ggeffect(m3_kscm, terms = c("spolu", "kscm_government"))
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
