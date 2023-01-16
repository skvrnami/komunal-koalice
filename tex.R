library(targets)
library(modelsummary)

tar_load(matches("m[1-7]{1}"))
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

writeLines(desc_table_tex, "output/desc_table.tex")

coefs <- c(
    "spoluTRUE"="Together", 
    "pirstanTRUE"="PirSTAN", 
    "tssTRUE"="TSS",
    "created_senate"="Senate PEC (2022)",
    "ano_governmentTRUE"="ANO in local government",
    "spoluTRUE:ano_governmentTRUE"="Together × ANO in local gov.",
    "spolu_governmentTRUE"="Together party in local gov.",
    "spoluTRUE:spolu_governmentTRUE"="Together × Together party in local gov.",
    "local_government_dummy"="Same local gov. position",
    "spoluTRUE:local_government_dummy"="Together × Same local gov. position",
    "created_2018"="Local PEC (2018)",
    "coalition_size_votes_norm"="Coalition size", 
    "I(coalition_size_votes_norm^2)"="Coalition size (sq.)",
    "diff_lrgen"="Ideological distance",
    "diff_lrgen:local_government_dummy"="Ideological distance × Same local gov. position",
    "asymmetry"="Asymmetry",
    "enep_votes"="ENEP", 
    "municipality_size"="Municipality size",
    "(Intercept)"="(Intercept)"
)

tex <- modelsummary(list(m2, m3, m4, m5), stars = TRUE,
                    fmt = "%.2f",
                    output = "latex",
                    metrics = "common", 
                    # stars = TRUE,
                    coef_map = coefs)
writeLines(tex, "output/models1.tex")

tex2 <- modelsummary(list(m2b, m3b, m4b, m5b), stars = TRUE,
                    fmt = "%.2f",
                    output = "latex",
                    metrics = "common", 
                    coef_map = coefs)
writeLines(tex2, "output/models2.tex")

tex3 <- modelsummary(list(match_model2, match_model2b), 
                     fmt = "%.2f",
                     output = "latex",
                     metrics = "common", 
                     stars = TRUE,
                     coef_map = coefs)
writeLines(tex3, "output/matching_models.tex")
