# Define custom functions


write_to_log <- function(..., log_file) {
  write_lines(
    c("\n\n############################################",
      as.character(Sys.time()),
      ...,
      "\n\n"),
    file   = log_file,
    append = TRUE
  )
}



# for merging across files
standardize_school_names <- function(vec) {
  v2 <- vec
  
  v2 <- gsub("0 Pilot", "", v2)
  v2 <- snakecase::to_snake_case(v2)
  v2 <- gsub("_", "", v2)
  
  case_when(
    tolower(v2) == "cant" ~ "canty",
    tolower(v2) == "kenwood" ~ "kenwoodhs",
    tolower(v2) == "lindbloom" ~ "lindblomhs",
    toupper(v2) %in% c("MULTICULTURALARTSHIGHSCHOOL",	"MULTICULTURALARTSHS") ~
      "MULTICULTURALARTSHS",
    toupper(v2) %in% c("COLLINSHS",	"COLLINSSTEAMHS") ~ 
      "COLLINSHS",
    toupper(v2) %in% c("NORTHWESTECC", "CATALPA") ~ "CATALPA",
    toupper(v2) %in% c("LINCOLNPARKELC", "FISHER") ~ "FISHER",
    .default = v2
  ) |> 
    toupper()
}


# # get attributes of a file in a location on box (if it exists)
# box_file_attr <- function(f_name, dir_id) {
#   f_list <- boxr::box_ls(dir_id = dir_id)
#   if(length(f_list) == 0) {return(NULL)}
#   
#   f_list |> 
#     as.data.frame() |> 
#     filter(name == f_name) |> 
#     as.list() |> 
#     compact()
# }
# 
# 
# # add a timestamped `comment` about `data_file` to the top of a `log_file` csv
# # on box
# box_add_to_log <- function(
#     data_file, comment, ...,
#     log_file,
#     dir_id,
#     ts       = Sys.time(),
#     silent   = FALSE
# ) {
#   
#   # get attributes of log file (if it exists)
#   log_attr <- box_file_attr(f_name = log_file, dir_id = dir_id)
#   
#   # read or create log file
#   if(length(log_attr) == 0) {
#     log_df <- data.frame(
#       timestamp = character(), 
#       file      = character(), 
#       comment   = character()
#     )
#   } else {
#     log_df <- boxr::box_read_csv(log_attr$id) |> 
#       mutate(across(everything(), as.character))
#   }
#   
#   # add new entry to top of log
#   log_df <- full_join(
#     data.frame(
#       timestamp = as.character(ts),
#       file      = as.character(data_file),
#       comment   = as.character(comment),
#       ...
#     ),
#     
#     log_df,
#     
#     by = c("timestamp", "file", "comment")
#   )
#   
#   # write to box
#   boxr::box_write(
#     log_df,
#     file_name = log_file,
#     dir_id    = dir_id
#   )
#   
#   # (maybe) return full log
#   if(!silent) {return(log_df)}
# }
# 
# # compare the contents of two data frames (ignoring R class, etc)
# equal_dfs <- function(a, b) {
#   a <- data.frame(a) |> mutate(across(everything(), as.character)) |> type_convert()
#   b <- data.frame(b) |> mutate(across(everything(), as.character)) |> type_convert()  
#   all.equal(a, b)
# }
# 
# 
# # check if data exists in its current form on box; 
# # if not, write new version and log the change
# box_write_if_diff <- function(
#     object, f_name, dir_id, 
#     comment, log_file, 
#     force = FALSE
# ) {
#   
#   # compare old and new data
#   new      <- object
#   old_attr <-  box_file_attr(f_name, dir_id)
#   if(length(old_attr) == 0) {
#     has_changed <- TRUE
#   } else {
#     old         <- boxr::box_read(old_attr$id)
#     has_changed <- !isTRUE(equal_dfs(old, new))
#   }
#   
#   # (maybe) write to file
#   if(has_changed | force) {
#     boxr::box_write(new, file_name = f_name, dir_id = dir_id)
#     
#     # log with comment
#     box_add_to_log(
#       data_file = f_name,
#       comment   = gsub("\\s+", " ", comment), 
#       log_file  = log_file,
#       dir_id    = dir_id,
#       silent    = TRUE
#     )
#     cat(paste0("New version uploaded to ", dir_id, "/", f_name))
#     
#   } else {
#     cat("\nNo change from saved file.\n")
#   }
#   
#   
#   return(new)
# }
# 
# 

write_ext <- function(obj, path, ...) {
  if(tools::file_ext(path) == "csv") {
    readr::write_csv(obj, path, na = "", ...)
  } else if(tools::file_ext(path) == "xlsx") {
    openxlsx::write.xlsx(obj, path, ...)
  } else if(tools::file_ext(path) == "rds") {
    readr::write_rds(obj, path, ...)  
  } else {
    readr::write_rds(obj, paste0(path, ".rds"), ...)  
  }
}


# check if data exists in its current form;
# if not, write new version and log the change
write_if_diff <- function(
    object, 
    f_name, 
    comment  = NULL, 
    log_file = file.path(dirname(f_name), "log.txt"),
    force    = FALSE
    ) {
  
  # check whether object is already saved in its current form
  has_changed <- TRUE
  if(file.exists(f_name)) {
    f_tmp <- file.path((tmpd <- tempdir()), basename(f_name))
    file.copy(f_name, f_tmp, overwrite = TRUE)
    ext_hash <- tools::md5sum(f_tmp)
    
    write_ext(object, f_tmp)
    new_hash <- tools::md5sum(f_tmp)
    
    if(identical(ext_hash, new_hash)) {
      has_changed <- FALSE
      }
  }
  
  # (maybe) write to file
  if(has_changed | force) {
    write_ext(object, f_name)
    write_to_log(
      paste0("Data written to ", basename(f_name)),
      comment,
      log_file = log_file
    )
  }
  
  return(object)
}







find_unique_by <- function(df, by_vars) {
  df2 <- df |> 
    mutate(
      across(
        everything(), 
        ~ sum(!duplicated(unlist(.x)))
      ), 
      .by = all_of(c(by_vars))
    )
  
  
  names(df2)[which(unlist(map(df2, ~ all(.x == 1))))]
}
distinct_unique_by <- function(df, by_vars) {
  df |> 
    select(all_of(c(
      by_vars,
      find_unique_by(df, by_vars)
    ))) |> 
    distinct()
}