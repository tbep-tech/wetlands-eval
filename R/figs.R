library(tidyverse)
library(here)
library(arrow)

pq_data <- open_dataset(here('data-parquet'))
distthr <- 50

# get wetland data
ests <- pq_data %>%
  filter(ACRES >= 0.25) %>%
  filter(!WETLAND_TYPE %in% 'Estuarine and Marine Deepwater') %>%
  collect() %>%
  mutate(
    isolated = as.numeric(neardist) > distthr
  ) %>%
  summarise(
    cnt = n(),
    acres = sum(ACRES),
    .by = c(isolated, WETLAND_TYPE, state)
  )

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
    subtitle = paste('At risk are those greater than', distthr, 'meters from existing surface water'),
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
  guides(fill = guide_legend(reverse=T)) +
  # facet_wrap(~WETLAND_TYPE, scales = 'free_x') +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    title = 'Wetlands at risk by state under new WOTUS definition',
    subtitle = paste('At risk are those greater than', distthr, 'meters from existing surface water'),
    caption = 'Source: National Hydrography Dataset, National Wetland Inventory',
    y = NULL,
    fill = 'At risk?',
    x = 'Acres of wetlands (x 1million)'
  )

png(here('figs/wetlandsacre.png'), res = 500, height = 7, width = 5, units = 'in')
print(p2)
dev.off()
