library(dplyr)
library(sf)
library(datasets)
library(usdata)
library(here)
library(archive)
library(gdalUtilities)
library(arrow)
library(units)

# to avoid error ParseException: Unknown WKB type 12.
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1, quiet = T)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON", progress = F)
  Y <- st_read(tmp2, quiet = T)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

# find nearest nhd to nwi, save as state files ------------------------------------------------

##
# base urls

# https://www.fws.gov/program/national-wetlands-inventory/download-state-wetlands-data
nwibaseurl <- 'https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/'

# https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHD/State/GDB/
nhdbaseurl <- 'https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/'

options(timeout = 3600)


# save as parquet -----------------------------------------------------------------------------

fls <- list.files(here('data'), full.names = T, recursive = F)

alldat <- NULL
for(fl in fls){

  cat(fl, '\n')

  # load file

  load(here(fl))
  nm <- gsub('\\.RData$', '', basename(fl))
  wetdat <- get(n                                                                                                                       m)

  # get isolated

  out <- wetdat %>%
    mutate(
      state = gsub('^wet', '', nm)
    )

  alldat <- rbind(alldat, out)

}

# save as parquet format by state and wetland type
pq_path <- here('data-parquet')

alldat %>%
  mutate(
    WETLAND_TYPE = ifelse(WETLAND_TYPE == 'Lakes', 'Lake', WETLAND_TYPE)
  ) %>%
  group_by(state, WETLAND_TYPE) %>%
  write_dataset(path = pq_path)

# # add PAD status column to existing state files -----------------------------------------------
#
# # parquet path
# pqdata <- open_dataset(here('data-parquet'))
#
# # PAD status data
# # https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download
# # padfull <- st_read('T:/05_GIS/PADUS3/PADUS3_fulldata.shp') %>%
# #   select(GAP_Sts, State_Nm) %>%
# #   ensure_multipolygons()
# # save(padfull, file = 'T:/05_GIS/PADUS3/padfull.RData', compress = 'xz')
# load(file = 'T:/05_GIS/PADUS3/padfull.RData')
#
# # unique state abbr
# sts <- pqdata %>%
#   dplyr::select(state) %>%
#   distinct() %>%
#   collect() %>%
#   pull()
#
# for(st in sts){
#
#   cat(st, '\n')
#
#   # load state data
#   wetdat <- pqdata %>%
#     filter(state == st) %>%
#     collect() %>%
#     st_as_sf(coords = c('LON', 'LAT'), crs = 4326) %>%
#     st_transform(crs = st_crs(padfull)) %>%
#     mutate(
#       id = 1:n()
#     )
#
#   # filter pad data by state
#   padtmp <- padfull %>%
#     filter(State_Nm %in% st)
#
#   # intersect wetdat with padtmp, accounts for overlappin polys in padtmp, gets min GAP
#   wetint <- st_intersection(wetdat, padtmp) %>%
#     st_set_geometry(NULL) %>%
#     select(id, GAP_Sts) %>%
#     filter(GAP_Sts == min(GAP_Sts), .by = id) %>%
#     unique()
#
#   # combine wetdat with gap intersection
#   wetdat <- wetdat %>%
#     left_join(wetint, by = 'id')
#
#   # save
#
# }

