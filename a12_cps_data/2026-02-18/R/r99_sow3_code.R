# Define Global Variables ----------------------------------------------------
# load function to access variables in globals.yaml
source(here::here("a11_sow3_longitudinal", "get_global.R"))

# Create lists to hold results
out_tables <- list()
out_plots  <- list()


data_dir <- dir_create_if(
  file.path(get_global("project_dir"), "data", Sys.Date())
)
output_dir <- dir_create_if(
  file.path(get_global("project_dir"), "output", Sys.Date())
  )

# Set universal meta values
# options(knitr.kable.NA = '')


# -------------------------------------------------------------------------


# Table 2: Utilization






t2_df <-left_join(
    use_df,
    school_df |>
        distinct(school_id, grade_level, region, region_3grp, wave)
)
names(t2_df)

tableby(
  year(adm_date) ~
    # recipient
    rcp_sex + rcp_race + rcp_eth + rcp_age +
    rcp_role + sym_new + rcp_dx_pre + inh_use_prev +

    # incident
    adm_season + why_given +

    # school chars
    wave + grade_level +

    # disposition
    hypo_disp_rcd + true_disp_rcd,
  data = t2_df |> filter(adm_year %in% c("23-24", "24-25")),
  total = FALSE
  ) |>
  summary(text = TRUE, labelTranslations = list(rcp_age = "Age")) |>
  as.data.frame() |>
  write_output("Table2_Utilization")


# Table 3: Attendance
t3a_df <- cps_df |>
  summarise(
    n_students         = sum(student_count),
    average_attendance = weighted.mean(average_attendance, student_count),
    across(starts_with("student_days"), ~ round(sum(.x))),
    .by = c(
      # school level
      school_id, wave, grade_level, region, region_2grp, region_3grp,
      pilot, impl_year, impl_date, data_year, data_date, active_months,

      # student level
      asthma_status, student_gender, student_race
      )
    ) |>

  droplevels()

add_todo(paste(
  "Consider addl interactions. Possible to collapse race and gender?",
  "Currently rank defficient if including all student-level 2-way",
  "interactions with year."
  ))

summary(t3a_mod <- geepack::geeglm(
  cbind(student_days_present, student_days_absent) ~
    asthma_status*(factor(data_year) + student_race + student_gender) +

    active_months + wave + grade_level + region_3grp,
  family = binomial(link = "logit"),
  data   = na.omit(t3a_df),
  id     = factor(school_id),
  waves  = data_year,
  corstr = "independence"
  ) |>
  write_data("t3a_mod_attd_asthma"))


t3a_est <- set_names(c("student_gender", "student_race")) |>
  map(~{
    emmeans::emmeans(
      t3a_mod,
      ~ data_year | asthma_status,
      by = .x,
      type = "response",
      weights = "proportional"
    )
  }) |>
  map_dfr(data.frame, .id = "byvar") |>
  mutate(level = coalesce(student_gender, student_race),
         .keep = "unused",
         .after = byvar)

t3a_or <- set_names(c("student_gender", "student_race")) |>
  map(~{
    emmeans::contrast(emmeans::emmeans(
      t3a_mod,
      ~ data_year | asthma_status,
      by = .x,
      weights = "proportional"
    ), "trt.vs.ctrl", type = "response", infer = TRUE)
  }) |>
  map_dfr(data.frame, .id = "byvar") |>
  mutate(level = coalesce(student_gender, student_race),
         .keep = "unused",
         .after = byvar)


# -------------------------------------------------------------------------

t3b_df <- cps_df |>
  select(-student_count) |>
  pivot_longer(
    c("iep_count_yes", "iep_count_no"),
    names_to = "iep_status",
    values_to = "student_count"
    ) |>
  mutate(
    student_days_expected = student_count*cps_base_days,
    student_days_present = student_days_expected*(average_attendance/100),
    student_days_absent  = student_days_expected - student_days_present,
    .after               = average_attendance
  ) |>
  summarise(
    n_students         = sum(student_count),
    average_attendance = weighted.mean(average_attendance, student_count),
    across(starts_with("student_days"), ~ round(sum(.x))),
    .by = c(
      # school level
      school_id, wave, grade_level, region, region_2grp, region_3grp,
      pilot, impl_year, impl_date, data_year, data_date, active_months,

      # student level
      iep_status, student_gender, student_race
    )
  ) |>

  droplevels()

summary(t3b_mod <- geepack::geeglm(
  cbind(student_days_present, student_days_absent) ~
    iep_status*(factor(data_year) + student_race + student_gender) +

    active_months + wave + grade_level + region_3grp,
  family = binomial(link = "logit"),
  data   = na.omit(t3b_df),
  id     = factor(school_id),
  waves  = data_year,
  corstr = "independence"
  ) |>
  write_data("t3b_mod_attd_iep"))



t3b_est <- set_names(c("student_gender", "student_race")) |>
  map(~{
    emmeans::emmeans(
      t3b_mod,
      ~ data_year | iep_status,
      by = .x,
      type = "response",
      weights = "proportional"
    )
  }) |>
  map_dfr(data.frame, .id = "byvar") |>
  mutate(level = coalesce(student_gender, student_race),
         .keep = "unused",
         .after = byvar)

t3b_or <- set_names(c("student_gender", "student_race")) |>
  map(~{
    emmeans::contrast(emmeans::emmeans(
      t3b_mod,
      ~ data_year | iep_status,
      by = .x,
      weights = "proportional"
    ), "trt.vs.ctrl", type = "response", infer = TRUE)
  }) |>
  map_dfr(data.frame, .id = "byvar") |>
  mutate(level = coalesce(student_gender, student_race),
         .keep = "unused",
         .after = byvar)





# -------------------------------------------------------------------------


t3c_df <- cps_df |>
  select(-student_count) |>
  pivot_longer(
    c("student_504_count_yes", "student_504_count_no"),
    names_to = "x504_status",
    values_to = "student_count"
  ) |>
  mutate(
    student_days_expected = student_count*cps_base_days,
    student_days_present = student_days_expected*(average_attendance/100),
    student_days_absent  = student_days_expected - student_days_present,
    .after               = average_attendance
  ) |>
  summarise(
    n_students         = sum(student_count),
    average_attendance = weighted.mean(average_attendance, student_count),
    across(starts_with("student_days"), ~ round(sum(.x))),
    .by = c(
      # school level
      school_id, wave, grade_level, region, region_2grp, region_3grp,
      pilot, impl_year, impl_date, data_year, data_date, active_months,

      # student level
      x504_status, student_gender, student_race
    )
  ) |>

  droplevels()

summary(t3c_mod <- geepack::geeglm(
  cbind(student_days_present, student_days_absent) ~
    x504_status*(factor(data_year) + student_gender) +
  student_race  +
    active_months + wave + grade_level + region_3grp,
  family = binomial(link = "logit"),
  data   = na.omit(t3c_df),
  id     = factor(school_id),
  waves  = data_year,
  corstr = "independence"
  ) |>
  write_data("t3c_mod_attd_504"))


t3c_est <- set_names(c("student_gender", "student_race")) |>
  map(~{
    emmeans::emmeans(
      t3c_mod,
      ~ data_year | x504_status,
      by = .x,
      type = "response",
      weights = "proportional"
    )
  }) |>
  map_dfr(data.frame, .id = "byvar") |>
  mutate(level = coalesce(student_gender, student_race),
         .keep = "unused",
         .after = byvar)

t3c_or <- set_names(c("student_gender", "student_race")) |>
  map(~{
    emmeans::contrast(emmeans::emmeans(
      t3c_mod,
      ~ data_year | x504_status,
      by = .x,
      weights = "proportional"
    ), "trt.vs.ctrl", type = "response", infer = TRUE)
  }) |>
  map_dfr(data.frame, .id = "byvar") |>
  mutate(level = coalesce(student_gender, student_race),
         .keep = "unused",
         .after = byvar)

# -------------------------------------------------------------------------





t3_ests <- list(
  t3a_est,
  t3b_est,
  t3c_est
  ) |>

  map_dfr(~{
    pivot_longer(
      .x,
      matches("status"), values_to = "status", names_to = "strata")
  }) |>
  pivot_wider(
    names_from = data_year,
    values_from = c(prob, lower.CL, upper.CL),
    id_cols = c(strata, status, byvar, level)
  ) |>
  relocate(
    matches("2022"), matches("2023"), matches("2024"), matches("2025"),
    .after = level
    )



t3_ors <- list(
  t3a_or,
  t3b_or,
  t3c_or
) |>
  map_dfr(~{
    pivot_longer(
      .x,
      matches("status"), values_to = "status", names_to = "strata")
  }) |>
  pivot_wider(
    names_from = contrast,
    values_from = c(odds.ratio, lower.CL, upper.CL, p.value),
    id_cols = c(strata, status, byvar, level)
  ) |>
  relocate(
    matches("2023"), matches("2024"), matches("2025"),
    .after = level
  )

t3 <- full_join(
  t3_ests,
  t3_ors
  ) |>
  arrange(fct_inorder(strata), desc(status)) |>

  write_output("Table3_Atttendance")

# -------------------------------------------------------------------------




# Table 4: Asthma
t4a_df <- cps_df |>
  summarise(
    student_count = sum(student_count),
    .by = c(
      school_id,
      data_year, impl_year, impl_date, active_months, #util,
      student_race, student_gender, asthma_status_lab
    )) |>
  pivot_wider(
    names_from = asthma_status_lab,
    values_from = student_count,
    id_cols = c(
      school_id,
      data_year, impl_year, impl_date, active_months, #util,
      student_race, student_gender
      ),
    values_fill = 0
    ) |>
  mutate(
    util   = fct_relevel(case_when(
      school_id %in% inh_df$school_id ~ "1+",
      .default = "None"
    ), "None"),
    student_race = fct_relevel(student_race, "Black Non-Hispanic"),
    wave         = fct_relevel(as.character(impl_date), "2024-09-01")
    ) |>
  droplevels()


summary(t4_mod <- geepack::geeglm(
  cbind(`Asthma Dx`, `No Asthma Dx`) ~
    student_race +
    student_gender +
    factor(data_year)*(

    wave +
    util),
  family = binomial(link = "logit"),
  data   = na.omit(t4a_df),
  id     = factor(school_id),
  waves  = data_year,
  corstr = "independence"
))


t4a_est <- set_names(c("student_gender", "student_race", "util", "wave")) |>
    map(~{
      emmeans::emmeans(
        t4_mod,
        ~ data_year,
        by = .x,
        type = "response",
        weights = "proportional"
      )
    }) |>
    map_dfr(data.frame, .id = "byvar") |>
    mutate(level = coalesce(student_gender, student_race, util, wave),
           .keep = "unused",
           .after = byvar)


t4b_or <- set_names(c("student_gender", "student_race", "util", "wave")) |>
    map(~{
      emmeans::contrast(emmeans::emmeans(
        t4_mod,
        ~ data_year,
        by = .x,
        weights = "proportional"
      ),
      "trt.vs.ctrl",
      type = "response",
      infer = TRUE
      )
    }) |>
    map_dfr(data.frame, .id = "byvar") |>
    mutate(level = coalesce(student_gender, student_race, util, wave),
           .keep = "unused",
           .after = byvar)


t4 <- full_join(


  t4a_est |>
    pivot_wider(
      names_from = data_year,
      values_from = c(prob, lower.CL, upper.CL),
      id_cols = c(byvar, level)
    ) |>
    relocate(
      matches("2022"), matches("2023"), matches("2024"), matches("2025"),
      .after = level
    ),


  t4b_or |>
    pivot_wider(
      names_from = contrast,
      values_from = c(odds.ratio, lower.CL, upper.CL, p.value),
      id_cols = c(byvar, level)
    ) |>
    relocate(
      matches("2023"), matches("2024"), matches("2025"),
      .after = level
    )

) |>
  write_output(("Table4_Asthma"))

# Utilization data --------------------------------------------------------







# # Read and modify data
# if(!exists("inh_use_raw")) {
#     inh_use_raw <- boxr::box_read_excel(
#         file_id = get_global("raw_files")$utilization$box_id,
#         sheet   = get_global("raw_files")$utilization$sheet
#     )
# }
#
# inh_use <- inh_use_raw |>
#     filter(!is.na(ID)) |>
#     drop_empty_cols() |>
#     detect_factors(titlize = FALSE) |>
#     rename_with(str_trim) |>
#     set_colnames_as_labels() |>
#     (\(.)
#      . |> set_names(sprintf("x%03d", 1:ncol(.)))
#      )()





# # match inhaler use to school names and ids
# inh_use <- inh_use |>
#
#     rename_by_label(
#         label = "At what school was the stock inhaler given?",
#         newname = "school_name"
#         ) |>
#     mutate(
#         school_match_name = gsub("_", "", snakecase::to_snake_case(school_name)),
#         school_match_name = case_when(
#             school_match_name  == "kenwood" ~ "kenwoodhs",
#             school_match_name  == "lindbloom" ~ "lindblomhs",
#             school_match_name  == "cant" ~ "canty",
#             .default = school_match_name
#         ),
#         .after = 1
#         ) |>
#
#     mutate(across(where(is.POSIXt), as.Date)) |>
#
#     left_join(
#         school_names_ids |>
#             distinct(school_match_name, school_id),
#         by = "school_match_name"
#     ) |>
#
#     write_data(get_global("analytic_files")$inh_use, csv = TRUE)




# Apply factor codes from dataframe/list
# factor_codes = list(
#   vname_1 = data.frame(
#     levels = c(1, 2, 3),
#     labels = c("a", "b", "c")
#     )
#   )
# my_cleaned_data <- my_raw_data
# for (i in intersect(names(factor_codes), names(my_raw_data))) {
#   my_cleaned_data[[i]] <- factor(
#     my_raw_data[[i]],
#     levels = factor_codes[[i]]$level,
#     labels = factor_codes[[i]]$label
#   )
# }

##########################################################################
# Save output
# out_tables[["My Data"]] <- my_data |>
#     write_data("my_data")







inh_use |>
    rename_by_label(label = "ID", newname = "row_id") |>
    rename_by_label("Recipient Sex",  "rcp_sex") |>
    rename_by_label("Recipient Race", "rcp_race") |>
    rename_by_label("Recipient Ethnicity", "rcp_eth") |>
    rename_by_label("Recipient age (If recipient is a visitor over the age of 18, student teacher, or school personnel select \"Adult\")", "rcp_age") |>
    rename_by_label("On what date the stock inhaler given?", "adm_date") |>
    rename_by_label("Location of Incident (Playground, gym, nurse's office, specific classroom number, etc.)", "adm_loc") |>
    rename_by_label("Under what protocol was the stock inhaler given?", "adm_protocol") |>
    rename_by_label("Was the school nurse present or notified?", "nurse_present") |>
    rename_by_label("Exposure to pollen allergen", "exp_pollen") |>
    rename_by_label("Exposure to dust allergen", "exp_dust") |>
    rename_by_label("Exposure to animal dander", "exp_anim") |>
    rename_by_label("Exposure to smoke in the environment", "exp_smok") |>
    rename_by_label("Exposure to air pollution in the environment", "exp_air") |>
    rename_by_label("Exposure to hot weather", "exp_heat") |>
    rename_by_label("Exposure to cold weather", "exp_cold") |>
    rename_by_label("Physical activity or exercise", "exp_phys") |>
    rename_by_label("Unknown", "exp_unk") |>
    rename_by_label("No exposure", "exp_none") |>
    rename_by_label("Other...29", "exp_other") |>
    rename_by_label("Shortness of breath", "sym_sob") |>
    rename_by_label("Wheezing", "sym_wheeze") |>
    rename_by_label("Coughing", "sym_cough") |>
    rename_by_label("Chest tightness", "sym_tight") |>
    rename_by_label("Rapid breathing", "sym_rapid") |>
    rename_by_label("Difficulty speaking or completing sentences", "sym_speak") |>
    rename_by_label("Chest pain", "sym_chpain") |>
    rename_by_label("Flaring of the nostrils", "sym_flare") |>
    rename_by_label("Retraction of neck and chest muscles", "sym_musc") |>
    rename_by_label("Bluish or pale coloration of the lips or fingertips", "sym_blue") |>
    rename_by_label("Anxiety or restlessness", "sym_anx") |>
    rename_by_label("breathing difficulty", "sym_brdiff") |>
    rename_by_label("No symptoms", "sym_none") |>
    rename_by_label("Other...46", "sym_other") |>
    rename_by_label("Correct Pathway", "pathway_true") |>
    rename_by_label("Chosen right pathway?", "pathway_chosen") |>
    rename_by_label("What is the name of the inhaler/medication given? (HFA Albuterol, ProAir RespiClick, etc.)", "inh_med") |>
    rename_by_label("Was a spacer device used? (Either plastic or paper)", "spacer") |>
    rename_by_label("How many puffs were administered using the HFA Inhaler?", "n_puffs_hfa") |>
    rename_by_label("Compliant with nurse-selected pathway", "pathway_chosen_compl") |>
    rename_by_label("compliant with correct pathway", "pathway_true_compl") |>
    rename_by_label("Was a disposable mouthpiece used?", "mouthpiece") |>
    rename_by_label("How many puffs were administered using the ProAir RespiClick?", "n_puffs_respi") |>
    rename_by_label("Type of person administered", "rcp_role") |>
    rename_by_label("Previous Dagnosis", "rcp_dx_pre") |>
    rename_by_label("Has the student since received an asthma diagnosis?", "rcp_dx_post") |>
    rename_by_label("Do they need assistance connecting with a healthcare provider or finding insurance coverage?", "hc_help") |>
    rename_by_label("Any Previous Usage", "inh_use_prev") |>
    rename_by_label("Was inhaler administered to prevent symptoms or given due to breathing difficulties?", "why_given") |>
    rename_by_label("According to the protocol, was the individual exhibiting mild-moderate symptoms or severe symptoms?", "severity") |>
    rename_by_label("As far as you are aware, is this the first time the individual has ever experienced these kind of symptoms at school?", "sym_new") |>
    rename_by_label("In your professional opinion, what would have happened if the student/individual did not have access to stock inhaler medication?", "hypo_disp") |>
    rename_by_label("Hypothetical_Disposition_simplified", "hypo_disp_rcd") |>
    rename_by_label("What DID happen to the student/individual after the inhaler was given?", "true_disp") |>
    rename_by_label("Actual_Disposition_simplified", "true_disp_rcd") |>
    rename_by_label("Did the individual go to the hospital after emergency services came?", "hosp") |>
    rename_by_label("How did they get to the hospital?", "transport") |>
    relocate(-starts_with("x"))
school_names_ids <- list.files(file.path(get_global("project_dir"), "data-raw"), "xls", full.names = TRUE) |>
    set_names() |>
    map(~{
        nms <- readxl::read_excel(.x, sheet = "Schools", col_names = FALSE, n_max = 4) |>
            filter(!grepl("\\*", ...1)) |>
            slice(1:2) |>
            summarise(across(everything(), ~ paste(na.omit(unlist(.x)), collapse = "; "))) |>
            unlist()

        readxl::read_excel(.x, sheet = "Schools", col_names = nms)
    }) |>
    map_dfr(~{
        .x |>
            janitor::clean_names() |>
            select(matches("school_id"), school_name, total) |>
            set_names(c("school_id", "school_name", "total"))
    }, .id = "source") |>
    mutate(
        source = basename(source),
        school_match_name = gsub("_", "", snakecase::to_snake_case(school_name))
        ) |>
    filter(!is.na(school_id)) |>
    filter(!grepl("school", tolower(school_id)))











# TODO: investigate, deal with missing data
