library(targets)
tar_load(matches("m[1-4]{1}"))

coefs <- c(
    "spoluTRUE"="Together", 
    "pirstanTRUE"="PirSTAN", 
    "tssTRUE"="TSS",
    "created_senate"="Senate PEC (2022)",
    "ano_governmentTRUE"="ANO in local government",
    "spoluTRUE:ano_governmentTRUE"="Together x ANO in local gov.",
    "spolu_governmentTRUE"="Together party in local gov.",
    "spoluTRUE:spolu_governmentTRUE"="Together x Together party in local gov.",
    "created_2018"="Local PEC (2018)",
    "coalition_size_votes_norm"="Coalition size", 
    "I(coalition_size_votes_norm^2)"="Coalition size (sq.)",
    "diff_lrgen"="Ideological distance",
    "asymmetry"="Asymmetry",
    "enep_votes"="ENEP", 
    "(Intercept)"="(Intercept)"
)

tex <- modelsummary(list(m1, m2, m3, m4), stars = TRUE,
                    fmt = "%.2f",
                    output = "latex",
                    metrics = "common", 
                    coef_map = coefs)
writeLines(tex, "output/models1.tex")

tex2 <- modelsummary(list(m1b, m2b, m3b, m4b), stars = TRUE,
                    fmt = "%.2f",
                    output = "latex",
                    metrics = "common", 
                    coef_map = coefs)
writeLines(tex2, "output/models2.tex")
