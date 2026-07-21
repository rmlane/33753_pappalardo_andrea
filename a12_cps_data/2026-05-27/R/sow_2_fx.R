
school_tby <- function(
        y_var,
        df,
        x_var = c(
            "notest(n_schools)", "wave",  "grade_level", "notest(region)",
            "region_3grp", "region_2grp",
            "student_count", "average_attendance",
            "pct_student_race_american_indian",
            "pct_student_race_asian_hawaiian_pacific_islander",
            "pct_student_race_black_non_hispanic",
            "pct_student_race_hispanic", "pct_student_race_multi",
            "pct_student_race_unknown", "pct_student_race_white_non_hispanic",
            "notest(pct_student_race_middle_eastern_or_north_african)",
            "pct_student_gender_female", "pct_student_gender_male",
            "pct_student_gender_non_binary", "pct_student_gender_unknown",
            "pct_asthma_yes", "pct_food_allergy_yes",
            "pct_non_food_allergy_yes",
            "pct_student_504_yes", "pct_iep_yes", "pct_esl_yes",
            "pct_frm_yes", "pct_homelessness_yes",
            "n_events", "n_events_cumulative",
            "any_events", "any_events_cumulative",
            "any_events_l3", "any_events_cumulative_l3"
        ),
        tby_ctrl = arsenal::tableby.control(
            test           = FALSE,
            digits.n	      = NA,
            numeric.stats  = c("medianrange", "q1q3",  "meansd")
        ),
        labels    = NULL,
        x_exclude = NULL,
        ...
) {

    arsenal::tableby(
        formula = arsenal::formulize(y_var, setdiff(x_var, x_exclude)),
        data    = df,
        subset  = (!is.na(df$student_count)),
        strata  = data_schoolyear,
        control = tby_ctrl,
        ...
        ) |>
        arsenal:::summary.tableby(text = TRUE, labelTranslations = labels) |>
        as.data.frame()
}



test_categ <- function(x, groups, data) {
    df <- data[, c(groups, x)]

    counts <-  table(df)
    fisher <- fisher.test(counts, simulate.p.value = TRUE, B = 1e5)

    pw_fisher <- combn(levels(df[[2]]), 2, simplify = FALSE) |>
        map_dfr(function(pairs) {
            rstatix::pairwise_fisher_test(
                counts[,pairs],
                p.adjust.method = "holm"
            ) |>
                mutate(
                    x_pair = paste(pairs, collapse = ", ")
                )
        })

    full_join(
        broom::tidy(fisher) |>
            mutate(group1 = "Global", group2 = "Global"),
        pw_fisher |>
            mutate(
                method          = "Pairwise Fisher's Exact Test for Count Data",
                p.adjust.method = "holm"
            ) |>
            rename(p.value = p, p.adjust = p.adj)
    )
}


test_ordered <- function(x, groups, data) {

    kwt <- eval(bquote(
        kruskal.test(x = data[[.(x)]], g = data[[groups]])
    ))
    pw_wilcox <- eval(bquote(
        pairwise.wilcox.test(
            x = data[[.(x)]], g = data[[groups]],
            p.adjust.method = "holm"
        )
    ))
    full_join(
        broom::tidy(kwt) |>
            mutate(group1 = "Global", group2 = "Global"),
        broom::tidy(pw_wilcox) |>
            rename(p.adjust = p.value) |>
            mutate(
                method          = pw_wilcox[["method"]],
                p.adjust.method = pw_wilcox[["p.adjust.method"]]
            )
    )
}
