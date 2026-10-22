
# Describe Schools, Overall and By Year -----------------------------------

# schools overall, annual data
school_tby(
  y_var = "",
  df    = read_rds(nested_here("data", "analytic_school_lvl.rds")),
) |> 
  structure(
    title       = "School Characteristics by School Year",
    sheetname   = "descr_school_char_by_year",
    description = "Summary statistics of school-level characteristics and demographics. Variables from CPS (e.g., % with asthma) are tabulated separately for each school year (2021-2022 to 2024-2025)."
  ) |>
  
  write_table("school_chars_by_sy")

# test changes from year to year (all pairwise)
read_rds(nested_here("data", "analytic_school_lvl.rds")) |> 
  filter(!is.na(student_count)) |> 
  droplevels() |>
  pivot_longer(
    c(student_count, average_attendance, matches("pct")),
    names_to = "x"
  ) |> 
  
  pivot_wider(
    names_from = data_schoolyear,
    id_cols    = c(school_id, x)
  ) |> 
  
  mutate(
    `(2022-2023) - (2021-2022)` = `2022-2023` - `2021-2022`,
    `(2023-2024) - (2021-2022)` = `2023-2024` - `2021-2022`,
    `(2023-2024) - (2022-2023)` = `2023-2024` - `2022-2023`,
    `(2024-2025) - (2021-2022)` = `2024-2025` - `2021-2022`,
    `(2024-2025) - (2022-2023)` = `2024-2025` - `2022-2023`,
    `(2024-2025) - (2023-2024)` = `2024-2025` - `2023-2024`
  ) |> 
  
  select(school_id, x, matches("\\(")) |> 
  pivot_longer(where(is.numeric), names_to = "contrast", values_to = "abs_change") |> 
  
  summarise(
    n_schools = sum(!is.na(abs_change)),
    broom::tidy(wilcox.test(abs_change, exact = FALSE, correct = TRUE, conf.int = TRUE)),
    .by       = c(x, contrast)
  ) |> 
  rename(med.change = estimate, med.change.conf.low = conf.low, med.change.conf.high = conf.high) |> 
  mutate(
    p.adjust        = p.adjust(p.value, "holm"), 
    p.adjust.n      = sum(!is.na(p.value)),
    p.adjust.method = "holm",
    .after          = p.value,
    .by             = x
  )  |> 
  
  structure(
    title       = "Test Change in School Characteristics Over Time",
    sheetname   = "test_school_char_change",
    description = "Annual school-level numeric variables (e.g., attendance, % with asthma) were tested for evidence of change over time. For each variable, within-school change was calculated from schoolyear to schoolyear (all pairwise schoolyear comparisons). School-level change between each pair of years was then tested for symmetry around zero (i.e., no change over time) by a one-sample Wilcoxon signed-rank test. p-values for the set of 6 pairwise year comparisons for each variable were adjusted by the Holm method."
  ) |>
  
  write_table("test_school_chars_by_sy.rds")


# Describe School Chars (2023-2024) by Inhaler Events ---------------------

# any events, 2023-2024 only, separating late implementers
school_tby(
  y_var = "any_events_l3",
  df    = read_rds(nested_here("data", "analytic_school_lvl.rds")) |> 
    filter(data_schoolyear %in% c("2023-2024")),
  tby_ctrl = arsenal::tableby.control(
    test           = FALSE,
    digits.n	      = NA,
    numeric.stats  = c("medianrange", "q1q3",  "meansd")
  ),
) |> 
  
  structure(
    title       = "School Characteristics by Leven of Inhaler Utilization, SY 2023-2024",
    sheetname   = "descr_util_groups_23_24",
    description = "Summary statistics of school-level characteristics and demographics in the 2023-2024 schoolyear. Schools are grouped by inhaler utilization level in the 2023-2024 school year (0 events vs. 1+ events vs. N/A).") |> 
  
  write_table("school_chars_by_util.rds")


# test assn between school chars and school-level utilization
full_join(
  
  c("grade_level", "region_3grp",  "region_2grp" ) |> 
    set_names() |> 
    map_dfr(~{
      test_categ(.x, groups = "any_events_l3", data = read_df_sy("2023-2024"))
    }, .id = "x"),
  
  c("student_count", "average_attendance", 
    grep("pct", names(read_df_sy("2023-2024")), value = TRUE)) |> 
    set_names() |>  
    map_dfr(~{
      test_ordered(.x, groups = "any_events_l3", data = read_df_sy("2023-2024"))
    }, .id = "x")
  
) |> 
  mutate(data_schoolyear = "2023-2024", .before = 1) |> 
  relocate(data_schoolyear, x, x_pair, group1, group2, n, p.value, matches("p.adj")) |> 
  
  structure(
    title       = "Test Differences in School Characteristics by Level of Inhaler Utilization, SY 2023-2024",
    sheetname   = "test_util_group_diffs_23_24",
    description = "Three-group and pairwise comparisons of school characteristics by inhaler utilization level in the SY 2023-2024 schoolyear. Numeric variables are compared across groups (0 events, 1+ events, NA) by Kruskal-Wallis tests (all 3 groups) and pairwise Wilcoxon rank-sum tests (each pair of 2 groups). Categorical variables (e.g., grade level) are compared across groups (0 events, 1+ events, NA) Fisher's exact tests (all 3 groups and each pair of 2 groups). p-values for the set of three pairwise comparisons for each variable were adjusted by the Holm method.") |> 
  
  write_table("test_school_chars_by_util.rds")
