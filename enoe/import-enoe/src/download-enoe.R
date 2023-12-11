#
# Author: SW
# Maintainer(s): SW, AF
# License:  Data Cívica 2023 ©
# ---------------------------------------------
# abogadas-mx/enoe/import-enoe/src/download-enoe.R

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive)

# Return current year
year <- as.numeric(format(Sys.Date(), "%Y"))

# Download zip file from INEGI
link <- paste0(
  "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/enoe_",
  year, "_trim1_csv.zip")

dest_path <- paste0(here("enoe/import-enoe/input/"), "t1_", year, ".zip")

download.file(url = link, destfile = dest_path)

# Unzip file
unzip(zipfile = dest_path, exdir = here("enoe/import-enoe/input/"))

# Upload files to Drive
file_list <- list.files(here("enoe/import-enoe/input"), pattern = ".csv",
                        full.names = TRUE)

drive_out <- as_id("1CfL43fMp7D_xwa4PThsysk3aNfbTFpFQ")

for (file_name in file_list) {
  new_name <- gsub("enoe_", "", tolower(file_name))
  
  file.rename(file_name, new_name)
  
  # Upload only non-vivienda tablas
  if(!str_detect(new_name, "viv")){
    drive_upload(new_name, path = drive_out, overwrite = TRUE)
  }
  
}

# Delete local files
files_to_delete <- list.files(here("enoe/import-enoe/input"), full.names = TRUE)

for (file_name in files_to_delete) {
  file.remove(file_name)
}

stopifnot(length(list.files(here("enoe/import-enoe/input"))) == 0)

# done.
