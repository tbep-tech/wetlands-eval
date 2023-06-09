library(dplyr)
library(sf)
library(datasets)
library(usdata)
library(here)
library(archive)
library(gdalUtilities)

sf_use_s2(FALSE)

# base urls
nwibaseurl <- 'https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/'
nhdbaseurl <- 'https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/'

# to avoid error ParseException: Unknown WKB type 12.
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

options(timeout = 3600)

str <- Sys.time()
for(i in state.abb[29:length(state.abb)]){

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
    st_zm() #%>%
    # ensure_multipolygons()

  ##
  # NHD

  cat('\tGet NHD data...\n')

  # get dl url
  state <- abbr2state(i)
  nhdzip <- paste0("NHD_H_", state, "_State_GDB.zip")
  nhdurl <- paste0(nhdbaseurl, "GDB/", nhdzip)
  nhdgdb <- gsub('\\.zip$', '.gdb', nhdzip)

  # download file
  download.file(url = nhdurl, destfile = here(nhdzip), mode = 'wb', quiet = T)

  # unzip file
  archive_extract(nhdzip)

  # get flowline
  flodat <- st_read(nhdgdb, layer = 'NHDFlowline', quiet = T)

  # get waterbody
  wbddat <- st_read(nhdgdb, layer = 'NHDWaterBody', quiet = T)

  # combine flowline and waterbody
  nhddat <- bind_rows(flodat, wbddat) %>%
    st_zm() # %>%
    # ensure_multipolygons()

  ##
  # get nearest and distance

  cat('\tGet wetland distance to NHD...\n')

  # transform nhddat crs to crs of wetdat
  nhddat <- st_transform(nhddat, crs = st_crs(wetdat))

  # get index of nearest nhddat features to wetdat features
  nearest <- try({st_nearest_feature(st_geometry(wetdat), st_geometry(nhddat))}, silent = T)

  if(inherits(nearest, 'try-error'))
    next()

  # add distance to nearest nhd to wet
  neardist <- try({st_distance(st_geometry(wetdat), st_geometry(nhddat[nearest,]), by_element = T)})

  if(inherits(neardist, 'try-error'))
    next()

  ##
  # save

  cat('\tSaving output...\n')

  out <- wetdat %>%
    st_set_geometry(NULL) %>%
    select(ACRES) %>%
    mutate(
      neardest = neardist,
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
  rm(list = c('flodat', 'wbddat', 'nhddat', 'wetdat'))

  print(Sys.time() - str)

}
