#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/enoe/join-enoe/src/join-tables.R

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, digest, here, googledrive, data.table)

# Clear environment
rm(list = ls())

# Files
paths <- list(
  input = here("enoe/join-enoe/input/"),
  output = here("enoe/join-enoe/output/"))

# Create list of files to read
file_names <- list.files(paths$input, pattern = ".csv")

# Define vars for merge
vars_merge <- c("tipo", "mes_cal", "cd_a", "ent", "con", "v_sel", "n_hog", "h_mud")

# Read data
for (file_name in file_names) {
  file_data <- fread(paste0(paths$input, file_name)) %>% 
    mutate(across(everything(), as.character))
  
  # Assert data is not all NA
  stopifnot(nrow(file_data) > 0,
            !all(is.na(file_data)))
  
  # Paste variables for merge to create key for each hogar
  file_data$hogar_key <- do.call(paste, c(file_data[, names(file_data) %in% vars_merge, with = FALSE],
                                          sep = "|"))
  
  # Create hash for each hogar
  file_data$hogar_hash <- sapply(file_data$hogar_key, digest, algo = "sha1")
  
  # Check that each unique hash represents a unique key
  stopifnot(length(unique(file_data$hogar_hash)) == length(unique(file_data$hogar_key)))
  
  # Delete key column & vars for merge (replaced by hash)
  file_data <- file_data %>%
    select(-c(hogar_key, any_of(vars_merge[vars_merge != "ent"])))
  
  assign(str_remove(file_name, "_filtered.csv"), file_data)
  
  message(paste0(file_name, " read."))
}

# Merge tables for each period ====
df_names_to_join <- ls(pattern = "t\\d{3}$") 

periods <- unique(substr(df_names_to_join,
                         nchar(df_names_to_join) - 3,
                         nchar(df_names_to_join)))

for (period in periods) {
  df_names_from_period <- df_names_to_join[grep(paste0(period, "$"),
                                                df_names_to_join)]
  
  list_dfs_from_period <- mget(df_names_from_period)
  
  # Remove string of period from names of tables
  names(list_dfs_from_period) <- str_replace(names(list_dfs_from_period), period, "")
  
  # Check that each of the 4 tablas is in the list of dfs
  stopifnot(all(c("coe1", "coe2", "hog", "sdem") %in% names(list_dfs_from_period)))
  
  # Define variables used for join
  vars_all_tablas <- list_dfs_from_period[["coe1"]] %>% 
    select(hogar_hash, year, ent, contains("fac")) %>% 
    names()
  
  var_t_loc <- list_dfs_from_period[["hog"]] %>%
    select(contains("t_loc")) %>% 
    names()
  
  # Join tables
  joined_dfs <- list_dfs_from_period[["hog"]] %>% 
    right_join(list_dfs_from_period[["sdem"]], by = c(vars_all_tablas, var_t_loc)) %>% 
    right_join(list_dfs_from_period[["coe1"]], by = c(vars_all_tablas, "n_ren")) %>% 
    right_join(list_dfs_from_period[["coe2"]], by = c(vars_all_tablas, "n_ren")) %>% 
    mutate(across(.cols = everything(), ~ as.character(.)))
  
  joined_dfs_name <- paste0(period, "_joined")
  
  assign(joined_dfs_name, joined_dfs)
  
  message(paste0(period, " joined."))
}

# Bind dfs from different periods into a single df
df_names_to_bind <- ls(pattern = "^t[0-9]{3}_joined$")

list_dfs_to_bind <- mget(df_names_to_bind)

dfs_bound <- bind_rows(list_dfs_to_bind)

# Export ====
# Save local CSV
write_csv(dfs_bound, paste0(paths$output, "enoe_joined.csv"))

# Write hash
hash <- digest(dfs_bound, algo = "sha1")
writeLines(hash, paste0(paths$output, "enoe_joined.txt"))

# done.
