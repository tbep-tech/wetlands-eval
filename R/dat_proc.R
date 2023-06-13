library(dplyr)
library(sf)
library(datasets)
library(usdata)
library(here)
library(archive)
library(gdalUtilities)

# to avoid error ParseException: Unknown WKB type 12.
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

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
  # NWI

  cat('\tGet NWI data...\n')

  # get file names
  nwizip <- paste0(i, "_geodatabase_wetlands.zip")
  nwiurl <- paste0(nwibaseurl, nwizip)
  nwigdb <- gsub('\\.zip$', '.gdb', nwizip)
  nwishp <- paste0(i, '_Wetlands')

  # download file
  download.file(url = nwiurl, destfile = here(nwizip), mode = 'wb', quiet = T)

  # unzip file
  archive_extract(nwizip)

  # read the wetland layer
  wetdat <- st_read(dsn = nwigdb, layer = nwishp, quiet = T) %>%
    st_zm()

  ##
  # NHD

  cat('\tGet NHD data...\n')

  # get dl url
  state <- gsub('\\s', '_', abbr2state(i))
  nhdzip <- paste0("NHD_H_", state, "_State_GDB.zip")
  nhdurl <- paste0(nhdbaseurl, "GDB/", nhdzip)
  nhdgdb <- gsub('\\.zip$', '.gdb', nhdzip)

  # download file
  download.file(url = nhdurl, destfile = here(nhdzip), mode = 'wb', quiet = T)

  # unzip file
  archive_extract(nhdzip)

  # get flowline
  flodat <- st_read(nhdgdb, layer = 'NHDFlowline', quiet = T) %>%
    st_zm()

  # get waterbody
  wbddat <- st_read(nhdgdb, layer = 'NHDWaterBody', quiet = T) %>%
    st_zm()

  ##
  # get nearest and distance

  cat('\tGet wetland distance to NHD...\n')

  # transform nhd data crs to crs of wetdat
  flodat <- st_transform(flodat, crs = st_crs(wetdat))
  wbddat <- st_transform(wbddat, crs = st_crs(wetdat))

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

  cat('\tSaving output...\n')

  out <- wetdat %>%
    st_set_geometry(NULL) %>%
    select(ATTRIBUTE, ACRES, WETLAND_TYPE) %>%
    mutate(
      neardist = pmin(neardistflo, neardistwbd),
      state = i
    )

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
  rm(list = c('flodat', 'wbddat', 'wetdat'))

  print(Sys.time() - str)

}
