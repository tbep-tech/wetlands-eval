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

# get state abbs already done
fls <- gsub('^wet|\\.RData$', '', list.files('data'))
sts <- state.abb[!state.abb %in% fls]

str <- Sys.time()
for(i in sts){

  cat(i, '\n')

  ##
  # subroutine for MN because it's problematic...
  if(i == 'MN'){
    source(here('R/mn_proc.R'))
    next()
  }

  ##
  # get NWI

  cat('\tGet NWI data...\n')

  # get file names
  nwizip <- paste0(i, "_geodatabase_wetlands.zip")
  nwiurl <- paste0(nwibaseurl, nwizip)
  nwigdb <- gsub('\\.zip$', '.gdb', nwizip)
  nwishp <- paste0(i, '_Wetlands')

  # download file
  wetdl <- try(download.file(url = nwiurl, destfile = here(nwizip), mode = 'wb', quiet = T), silent = T)
  tryi <- 1
  while(class(wetdl) == 'try-error' & tryi <= 10){
    cat('\t\t', 'nwi download failed, retrying...\n')
    tryi <- tryi + 1
    wetdl <- try(download.file(url = nwiurl, destfile = here(nwizip), mode = 'wb', quiet = T), silent = T)
  }

  if(tryi > 10)
    next()

  # unzip file
  archive_extract(nwizip)

  # read the wetland layer
  wetdatraw <- st_read(dsn = nwigdb, layer = nwishp, quiet = T) %>%
    st_zm() %>%
    mutate(ind = 1:n()) %>%
    ensure_multipolygons()

  # check for missing wetland type
  chk <- is.na(wetdatraw$WETLAND_TYPE)
  if(any(chk)){
    cnt <- sum(chk)
    cat('\t\t', paste0('missing wetland type (', paste(cnt, collapse = ','), '), removing...\n'))
    wetdatraw <- wetdatraw[!chk, ]
  }

  ##
  # get NHD

  cat('\tGet NHD data...\n')

  # get dl url
  state <- gsub('\\s', '_', abbr2state(i))
  nhdzip <- paste0("NHD_H_", state, "_State_GDB.zip")
  nhdurl <- paste0(nhdbaseurl, "GDB/", nhdzip)
  nhdgdb <- gsub('\\.zip$', '.gdb', nhdzip)

  # download file
  nhddl <- try(download.file(url = nhdurl, destfile = here(nhdzip), mode = 'wb', quiet = T), silent = T)
  tryi <- 1
  while(class(nhddl) == 'try-error' & tryi <= 10){
    cat('\t\t', 'nhd download failed, retrying...\n')
    tryi <- tryi + 1
    nhddl <- try(download.file(url = nhdurl, destfile = here(nhdzip), mode = 'wb', quiet = T), silent = T)
  }

  if(tryi > 10)
    next()

  # unzip file
  archive_extract(nhdzip)

  # get flowline, subset relevant ftype
  # types https://www.usgs.gov/ngp-standards-and-specifications/national-hydrography-dataset-nhd-data-dictionary-feature-classes
  flodat <- st_read(nhdgdb, layer = 'NHDFlowline', quiet = T) %>%
    st_zm() %>%
    filter(ftype %in% c(336, 460, 566, 558)) # canal/ditches, streams/rivers, coastlines, artificial paths

  # get waterbody, subset relevant ftype
  # types https://www.usgs.gov/ngp-standards-and-specifications/national-hydrography-dataset-nhd-data-dictionary-feature-classes
  wbddat <- st_read(nhdgdb, layer = 'NHDWaterBody', quiet = T) %>%
    st_zm() %>%
    filter(ftype %in% c(390, 436, 493)) # lakes/ponds, reservoirs, estuaries

  # transform nhd data crs to crs of wetdatraw
  flodat <- st_transform(flodat, crs = st_crs(wetdatraw))
  wbddat <- st_transform(wbddat, crs = st_crs(wetdatraw))

  ##
  # wetland preprocessing

  cat('\tWetland preprocessing...\n')

  # id those that immediately intersect with flodat or wbddat
  flowetdatraw <- wetdatraw[flodat, ]
  wbdwetdatraw <- wetdatraw %>%
    filter(!ind %in% flowetdatraw$ind) %>%
    .[wbddat, ]

  # unique index of those that already overlap
  indrm <- unique(c(flowetdatraw$ind, wbdwetdatraw$ind)) %>%
    sort()

  # remove those that already intersect with flodat or wbddat
  # remove those that are de facto connected (riverine)
  wetdatrawflt <- wetdatraw %>%
    filter(!ind %in% indrm) %>%
    filter(!WETLAND_TYPE %in% c('Estuarine and Marine Deepwater', 'Estuarine and Marine Wetland', 'Riverine'))

  cat('\t\t', nrow(wetdatraw), 'in original\n')
  cat('\t\t', nrow(wetdatrawflt), 'in filtered to combine\n')

  cat('\t\t buffer and combine complexes...\n')

  # combine adjacent polygons by 0.5 m buffer distance
  wetbuff <- st_buffer(wetdatrawflt, dist = 0.5) %>%
    st_union() %>%
    st_cast('POLYGON') %>%
    st_sf() %>%
    st_buffer(dist = -0.5)

  # get index of majority wetland type for unioned wetland layer, number of types
  typ_fun <- function(x) names(table(x))[which.max(table(x))]
  ints <- st_intersects(wetbuff, wetdatrawflt) %>%
    as.data.frame() %>%
    mutate(
      WETLAND_TYPE = st_set_geometry(wetdatrawflt[.$col.id, 'WETLAND_TYPE'], NULL)
    ) %>%
    summarise(
      WETLAND_TYPE = typ_fun(WETLAND_TYPE),
      njoin = n(),
      .by = row.id
    )

  # assign wetland type from ints and calculate area
  wetdat <- wetbuff %>%
    mutate(
      WETLAND_TYPE = ints$WETLAND_TYPE,
      njoin = ints$njoin,
      ACRES = as.numeric(set_units(st_area(.), 'acre'))
    )

  cat('\t\t', nrow(wetdat), 'in complexes combined\n')

  ##
  # get nearest and distance

  cat('\tGet wetland distance to NHD...\n')

  # get index of nearest nhd features to wetdat features
  nearestflo <- try({st_nearest_feature(st_geometry(wetdat), st_geometry(flodat))}, silent = F)
  nearestwbd <- try({st_nearest_feature(st_geometry(wetdat), st_geometry(wbddat))}, silent = F)

  if(inherits(nearestflo, 'try-error') | inherits(nearestwbd, 'try-error')){
    wetdat <- ensure_multipolygons(wetdat)
    nearestflo <- try({st_nearest_feature(st_geometry(wetdat), st_geometry(flodat))}, silent = F)
    nearestwbd <- try({st_nearest_feature(st_geometry(wetdat), st_geometry(wbddat))}, silent = F)
  }

  # get distance of nearest nhd to wet
  neardistflo <- try({st_distance(st_geometry(wetdat), st_geometry(flodat[nearestflo, ]), by_element = T)})
  neardistwbd <- try({st_distance(st_geometry(wetdat), st_geometry(wbddat[nearestwbd, ]), by_element = T)})

  if(inherits(neardistflo, 'try-error') | inherits(neardistwbd, 'try-error'))
    next()

  ##
  # save

  cat('\tGetting centroids and saving output...\n')

  # those not calculated
  out1 <- wetdatraw %>%
    filter(!ind %in% wetdatrawflt$ind) %>%
    st_centroid() %>%
    st_transform(crs = 4326) %>%
    mutate(
      LON = st_coordinates(.)[, 1],
      LAT = st_coordinates(.)[, 2]
    ) %>%
    st_set_geometry(NULL) %>%
    select(ACRES, WETLAND_TYPE, LON, LAT) %>%
    mutate(
      nearest_m = 0,
      state = i,
      calculated = F
    )

  # those calculated
  out2 <- wetdat %>%
    st_centroid() %>%
    st_transform(crs = 4326) %>%
    mutate(
      LON = st_coordinates(.)[, 1],
      LAT = st_coordinates(.)[, 2]
    ) %>%
    st_set_geometry(NULL) %>%
    select(ACRES, WETLAND_TYPE, LON, LAT) %>%
    mutate(
      nearest_m = as.numeric(pmin(neardistflo, neardistwbd)),
      state = i,
      calculated = T
    )

  # combine
  out <- bind_rows(out1, out2) %>%
    arrange(WETLAND_TYPE)
  names(out) <- tolower(names(out))

  cat('\t\t', nrow(out), 'in output\n')

  outnm <- paste0('wet', i)
  outfl <- paste0(here('data'), '/', outnm, '.RData')
  assign(outnm, out)
  save(list = outnm, file = outfl)

  ##
  # clean up files

  cat('\tRemoving files...\n')

  unlink(nwizip)
  unlink(nhdzip)
  unlink(gsub('\\.gdb$', '.jpg', nhdgdb))
  unlink(gsub('\\.gdb$', '.xml', nhdgdb))
  unlink(nwigdb, recursive = T)
  unlink(nhdgdb, recursive = T)
  rm(list = c('flodat', 'wbddat', 'wetdat', 'wetdatraw', 'wetdatrawflt', 'wetbuff', 'ints', 'nearestflo', 'nearestwbd', 'neardistflo', 'neardistwbd', 'out', 'out1', 'out2', 'outfl', 'outnm', 'indrm'))

  print(Sys.time() - str)

}


# save as parquet -----------------------------------------------------------------------------

fls <- list.files(here('data'), full.names = T, recursive = F)

alldat <- NULL
for(fl in fls){

  cat(fl, '\n')

  # load file

  load(here(fl))
  nm <- gsub('\\.RData$', '', basename(fl))
  wetdat <- get(nm)

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
    wetland_type = ifelse(wetland_type == 'Lakes', 'Lake', wetland_type)
  ) %>%
  group_by(state, wetland_type) %>%
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

