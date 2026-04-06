na_view <- function(df) {
    anti_join(
        df,
        na.omit(df),
        by = names(df)
    )
}

set_names_from_child <- function(x, child_name) {
    names(x) <- transpose(x) |> purrr::pluck(child_name)
    x
}

# cross-sectional (2023-2024 only):
# compare characteristics of schools in event grouping
read_df_sy <- function(sy, data = nested_here("data", "analytic_school_lvl.rds")) {
    read_rds(data) |>
        filter(data_schoolyear == sy)
}
