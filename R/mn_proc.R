# special routine for processing MN -----------------------------------------------------------

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

mnsubset <- st_read(here('data-raw/mnsubset.shp'), quiet = T)

# find nearest nhd to nwi, save as state files ------------------------------------------------

##
# base urls

# https://www.fws.gov/program/national-wetlands-inventory/download-state-wetlands-data
nwibaseurl <- 'https://documentst.ecosphere.fws.gov/wetlands/data/State-Downloads/'

# https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHD/State/GDB/
nhdbaseurl <- 'https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/'

options(timeout = 3600)

i <- 'MN'

cat(i, '\n')

##
# get NWI

cat('\tGet NWI data...\n')

# get file names
nwizip <- paste0(i, "_geodatabase_wetlands.zip")
nwiurl <- paste0(nwibaseurl, nwizip)
nwigdb <- gsub('\\.zip$', '.gdb', nwizip)
nwishp <- paste0(i, '_Wetlands')

# # download file
# wetdl <- try(download.file(url = nwiurl, destfile = here(nwizip), mode = 'wb', quiet = T), silent = T)
# tryi <- 1
# while(class(wetdl) == 'try-error' & tryi <= 10){
#   cat('\t\t', 'nwi download failed, retrying...\n')
#   tryi <- tryi + 1
#   wetdl <- try(download.file(url = nwiurl, destfile = here(nwizip), mode = 'wb', quiet = T), silent = T)
# }
#
# if(tryi > 10)
#   next()
#
# # unzip file
# archive_extract(nwizip)

# read the wetland layer
wetdatraw <- st_read(dsn = nwigdb, layer = nwishp, quiet = T) %>%
  st_zm() %>%
  mutate(ind = 1:n()) %>%
  ensure_multipolygons()

##
# get NHD

cat('\tGet NHD data...\n')

# get dl url
state <- gsub('\\s', '_', abbr2state(i))
nhdzip <- paste0("NHD_H_", state, "_State_GDB.zip")
nhdurl <- paste0(nhdbaseurl, "GDB/", nhdzip)
nhdgdb <- gsub('\\.zip$', '.gdb', nhdzip)

# # download file
# nhddl <- try(download.file(url = nhdurl, destfile = here(nhdzip), mode = 'wb', quiet = T), silent = T)
# tryi <- 1
# while(class(nhddl) == 'try-error' & tryi <= 10){
#   cat('\t\t', 'nhd download failed, retrying...\n')
#   tryi <- tryi + 1
#   nhddl <- try(download.file(url = nhdurl, destfile = here(nhdzip), mode = 'wb', quiet = T), silent = T)
# }
#
# if(tryi > 10)
#   next()
#
# # unzip file
# archive_extract(nhdzip)

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

# subset wetdatrawflt into two
mnsubsetprj <- st_transform(mnsubset, crs = st_crs(wetdatrawflt))

wetdatrawflt1 <- wetdatrawflt[mnsubsetprj, ]
wetdatrawflt2 <- wetdatrawflt[!wetdatrawflt$ind %in% wetdatrawflt1$ind, ]

wetdatls <- lapply(list(wetdatrawflt1, wetdatrawflt2), function(x){

  wetbuff <- st_buffer(x, dist = 0.5) %>%
    st_union() %>%
    st_cast('POLYGON') %>%
    st_sf() %>%
    st_buffer(dist = -0.5)

  # get index of majority wetland type for unioned wetland layer, number of types
  typ_fun <- function(x) names(table(x))[which.max(table(x))]
  ints <- st_intersects(wetbuff, x) %>%
    as.data.frame() %>%
    mutate(
      WETLAND_TYPE = st_set_geometry(x[.$col.id, 'WETLAND_TYPE'], NULL)
    ) %>%
    summarise(
      WETLAND_TYPE = typ_fun(WETLAND_TYPE),
      njoin = n(),
      .by = row.id
    )

  # assign wetland type from ints and calculate area
  out <- wetbuff %>%
    mutate(
      WETLAND_TYPE = ints$WETLAND_TYPE,
      njoin = ints$njoin,
      ACRES = as.numeric(set_units(st_area(.), 'acre'))
    )

  return(out)

})

wetdat <- do.call(rbind, wetdatls)

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




