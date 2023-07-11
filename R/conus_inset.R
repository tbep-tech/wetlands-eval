##
# add alaska and hawaii as conus inset
# currently just plots state boundaries

library(tmap)
library(tidyverse)
library(sf)

# function to obtain US county shape
get_US_county_2010_shape <- function() {
  dir <- tempdir()
  download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = file.path(dir, "gz_2010_us_050_00_20m.zip"))
  unzip(file.path(dir, "gz_2010_us_050_00_20m.zip"), exdir = dir)
  US <- read_sf(file.path(dir, "gz_2010_us_050_00_20m.shp"))
  return(US)
}

US <- get_US_county_2010_shape()

US_cont <- US %>%
  dplyr::filter(!STATE %in% c("02","15","72")) %>%
  summarize(
    geometry = st_union(geometry),
    .by = 'STATE'
  )
US_AK <- US %>% dplyr::filter(STATE == "02") %>%
  summarize(
    geometry = st_union(geometry),
    .by = 'STATE'
  )
US_HI <- US %>% dplyr::filter(STATE == "15") %>%
  summarize(
    geometry = st_union(geometry),
    .by = 'STATE'
  )

# contiguous US
m_cont <- tm_shape(US_states, projection = 2163) +
  tm_borders(lwd = 1, col = "black", alpha = .5) +
  tm_layout(frame = FALSE,
            inner.margins = c(0.1, 0.1, 0.05, 0.05))

# Alaska inset
m_AK <- tm_shape(US_AK, projection = 3338) +
  tm_borders(lwd = 1, col = "black", alpha = .5) +
  tm_layout(frame = FALSE, bg.color = NA)
# Hawaii inset
m_HI <- tm_shape(US_HI, projection = 3759) +
  tm_borders(lwd = 1, col = "black", alpha = .5) +
  tm_layout(frame = FALSE, bg.color = NA)

# specify viewports for Alaska and Hawaii
vp_AK <- viewport(x = 0.25, y = 0.15, width = 0.3, height = 0.3)
vp_HI <- viewport(x = 0.45, y = 0.1, width = 0.2, height = 0.1)


# plot map
m_cont
print(m_AK, vp = vp_AK)
print(m_HI, vp = vp_HI)
