na_view <- function(df) {
    anti_join(
        df,
        na.omit(df),
        by = names(df)
    )
}

view_incomplete_cases <- function(data) {
    na_view(data)
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



find_unique_by <- function(data, identifier, limit_to = NULL, exclude = NULL) {
    if(!is.null(limit_to)) {
        data <- data |> 
            select(any_of(c(identifier, limit_to))) 
    }
    
    data <- data |> 
        filter(if_all(all_of(c(identifier)), ~ !is.na(.x))) |> 
        select(-any_of(c(exclude)))
    
    n_unique_ids <- nrow(distinct(data[, identifier]))
    
    res <- c(identifier)
    
    for (vname in names(data)) {
        n_unique_combos <- nrow(distinct(data[, unique(c(identifier, vname))]))
        
        if (n_unique_combos == n_unique_ids) {
            res <- c(res, vname)
        }
    }
    
    unique(res)
}

distinct_by <- function(data, identifier, limit_to = NULL, exclude = NULL) {
    vars <- find_unique_by(
        data       = data, 
        identifier = identifier, 
        limit_to   = limit_to,
        exclude    = exclude
    )
    
    distinct(data[, c(vars)])
}

write_table <- function(
        x, 
        name,
        loc = nested_here("output", "tables")
) {
    
    # first, check if file already exists
    name              <- tools::file_path_sans_ext(name)
    search_string     <- glue::glue("^t[0-9]+_{name}\\.rds$")
    extant_filenames  <- list.files(
        loc, 
        "^t.*rds$", 
        full.names = FALSE
    )
    matched_filename  <- grep(
        search_string, 
        extant_filenames, 
        value = TRUE
    )
    
    # use existing filename or create a new one (next in sequence)
    new_filename <- coalesce(
        pluck(matched_filename, 1),
        glue::glue(
            "t{N}_{name}.rds", 
            N = sprintf('%03d', length(extant_filenames) + 1)
        )
    )
    print(file.path(loc, new_filename))
    
    # write to file
    write_rds(
        x,
        file.path(loc, new_filename)
    )
}
