library(arrow)
library(dplyr)
library(here)

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
    WETLAND_TYPE = ifelse(WETLAND_TYPE == 'Lakes', 'Lake', WETLAND_TYPE)
  ) %>%
  group_by(state, WETLAND_TYPE) %>%
  write_dataset(path = pq_path)

# pqdat <- open_dataset(sources = pq_path) %>%
#   collect()
#
# tibble(
#   files = list.files(pq_path, recursive = TRUE),
#   size_MB = file.size(file.path(pq_path, files)) / 1024^2
# )

