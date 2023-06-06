library(tidyverse)
library(sf)
library(rvest)
library(usdata)
library(nhdR)

abbrs <- state_stats$abbr

# wetland urls
wetfls <- 'https://www.fws.gov/program/national-wetlands-inventory/download-state-wetlands-data' %>%
  read_html() %>%
  html_elements("table") %>%
  html_elements("tr") %>%
  html_elements("a") %>%
  html_attr("href") %>%
  .[grepl('shapefile', .)] %>%
  .[gsub('(^.*)\\_shapefile.*$', '\\1', basename(.)) %in% abbrs]

str <- Sys.time()
for(i in wetfls){

  fl <- basename(i)

  cat(i, '\n')
  print(Sys.time() - str)

  stateabbr <- gsub('(^.*)\\_shapefile.*$', '\\1', basename(fl))

  # download file
  tmp1 <- tempfile(fileext = ".zip")
  download.file(url = i, destfile = tmp1)

  # unzip file
  tmp2 <- tempdir()
  utils::unzip(tmp1, exdir = tmp2)

  # get the layers from the gdb
  gdbpth <- list.files(tmp2, pattern = stateabbr, full.names = T)
  lyrs <- st_layers(gdbpth)$name
  lyr <- lyrs[grepl('Wetlands$', lyrs)]

  # read the wetland layer
  wetdat <- st_read(dsn = gdbpth, layer = lyr)

  unlink(tmp1, recursive = T)
  unlink(tmp2, recursive = T)

  # get the corresponding NHD state layer
  nhddat <- nhd_get(state = stateabbr)

  # get data from above, use find-nearest for wetland layer to nhddat, those greater than x are "isolated"
  # redo by NHD?

}
