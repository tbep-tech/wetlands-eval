library(tidyverse)
library(here)

fls <- list.files(here('data'), full.names = T)

distthr <- 10

ests <- NULL
for(fl in fls){

  cat(basename(fl), '\n')

  # load file

  load(here(fl))
  nm <- gsub('\\.RData$', '', basename(fl))
  wetdat <- get(nm)

  # summarize

  out <- wetdat %>%
    mutate(
      isolated = neardist > distthr
    ) %>%
    summarise(
      cnt = n(),
      acres = sum(ACRES),
      .by = c(isolated, WETLAND_TYPE)
    ) %>%
    mutate(
      state = gsub('^wet', '', nm)
    )

  ests <- rbind(ests, out)

}

toplo1 <- ests %>%
  summarize(
    cnt = sum(cnt),
    acres = sum(acres),
    .by = c(isolated, state)
  ) %>%
  mutate(
    state = factor(state, levels = unique(state)[order(cnt[isolated])]),
    isolated = ifelse(isolated, 'yes', 'no'),
    cnt = cnt / 1e6
  )

p1 <- ggplot(toplo1, aes(y = state, x = cnt, fill = isolated)) +
  geom_col(alpha = 0.8) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c('lightblue', 'red')) +
  guides(fill = guide_legend(reverse=T)) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    title = 'Wetlands at risk by state under new WOTUS definition',
    subtitle = 'At risk are those greater than 10 meters from existing surface water',
    caption = 'Source: National Hydrography Dataset, National Wetland Inventory',
    y = NULL,
    fill = 'At risk?',
    x = 'Number of wetlands (x 1million)'
  )

png(here('figs/wetlandscnt.png'), res = 500, height = 7, width = 5, units = 'in')
print(p1)
dev.off()

toplo2 <- ests %>%
  summarize(
    cnt = sum(cnt),
    acres = sum(acres),
    .by = c(isolated, state)
  ) %>%
  mutate(
    state = factor(state, levels = unique(state)[order(acres[isolated])]),
    isolated = ifelse(isolated, 'yes', 'no'),
    cnt = cnt / 1e6,
    acres = acres / 1e6
  )

p2 <- ggplot(toplo2, aes(y = state, x = acres, fill = isolated)) +
  geom_col(alpha = 0.8) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c('lightblue', 'red')) +
  guides(colour = guide_legend(reverse=T))
  # facet_wrap(~WETLAND_TYPE, scales = 'free_x') +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    y = NULL,
    fill = 'At risk?',
    x = 'Acres of wetlands (x 1million)'
  )
