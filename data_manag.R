library(tidyverse)

load(file = 'data/wetAK.RData')
load(file = 'data/wetAL.RData')
load(file = 'data/wetAR.RData')
load(file = 'data/wetAZ.RData')
load(file = 'data/wetCA.RData')
load(file = 'data/wetCO.RData')
load(file = 'data/wetCT.RData')
load(file = 'data/wetDE.RData')
load(file = 'data/wetFL.RData')
load(file = 'data/wetGA.RData')
load(file = 'data/wetHI.RData')
load(file = 'data/wetIA.RData')
load(file = 'data/wetID.RData')
load(file = 'data/wetIL.RData')
load(file = 'data/wetIN.RData')
load(file = 'data/wetKS.RData')
load(file = 'data/wetKY.RData')
load(file = 'data/wetLA.RData')
load(file = 'data/wetMA.RData')
load(file = 'data/wetMD.RData')
load(file = 'data/wetME.RData')
load(file = 'data/wetMI.RData')
load(file = 'data/wetMN.RData')
load(file = 'data/wetMO.RData')
load(file = 'data/wetMS.RData')
load(file = 'data/wetMT.RData')
load(file = 'data/wetNC.RData')
load(file = 'data/wetND.RData')
load(file = 'data/wetNE.RData')
load(file = 'data/wetNH.RData')
load(file = 'data/wetNJ.RData')
load(file = 'data/wetNM.RData')
load(file = 'data/wetNV.RData')
load(file = 'data/wetNY.RData')
load(file = 'data/wetOH.RData')
load(file = 'data/wetOK.RData')
load(file = 'data/wetOR.RData')
load(file = 'data/wetPA.RData')
load(file = 'data/wetRI.RData')
load(file = 'data/wetSC.RData')
load(file = 'data/wetSD.RData')
load(file = 'data/wetTN.RData')
load(file = 'data/wetTX.RData')
load(file = 'data/wetUT.RData')
load(file = 'data/wetVA.RData')
load(file = 'data/wetVT.RData')
load(file = 'data/wetWA.RData')
load(file = 'data/wetWI.RData')
load(file = 'data/wetWV.RData')
load(file = 'data/wetWY.RData')

states <- rbind(wetAK,
                wetAL,
                wetAR,
                wetAZ,
                wetCA,
                wetCO,
                wetCT,
                wetDE,
                wetFL,
                wetGA,
                wetHI,
                wetIA,
                wetID,
                wetIL,
                wetIN,
                wetKS,
                wetKY,
                wetLA,
                wetMA,
                wetMD,
                wetME,
                wetMI,
                wetMN,
                wetMO,
                wetMS,
                wetMT,
                wetNC,
                wetND,
                wetNE,
                wetNH,
                wetNJ,
                wetNM,
                wetNV,
                wetNY,
                wetOH,
                wetOK,
                wetOR,
                wetPA,
                wetRI,
                wetSC,
                wetSD,
                wetTN,
                wetTX,
                wetUT,
                wetVA,
                wetVT,
                wetWA,
                wetWI,
                wetWV,
                wetWY)

# Combine the Lake and Lakes wetland types
states$WETLAND_TYPE <- ifelse(states$WETLAND_TYPE == "Lakes", "Lake", states$WETLAND_TYPE)

# Remove deepwater
states <- states %>%
  filter(WETLAND_TYPE != "Estuarine and Marine Deepwater")

# Keep wetlands >= 0.25 acres
states0.25 <- states %>%
  filter(ACRES >= 0.25)



########### BY STATE ##############

# stats for at-risk wetlands > 1 m from hydological feature
states_1m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>1),
            acres_atrisk = sum(ACRES[neardist>1]),
            pct_n_atrisk = sum(neardist>1)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>1])/sum(ACRES)*100) %>%
  mutate(distthreshold = 1)

# stats for at-risk wetlands > 10 m from hydological feature
states_10m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>10),
            acres_atrisk = sum(ACRES[neardist>10]),
            pct_n_atrisk = sum(neardist>10)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>10])/sum(ACRES)*100) %>%
  mutate(distthreshold = 10)

# stats for at-risk wetlands > 20 m from hydological feature
states_20m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>20),
            acres_atrisk = sum(ACRES[neardist>20]),
            pct_n_atrisk = sum(neardist>20)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>20])/sum(ACRES)*100) %>%
  mutate(distthreshold = 20)

# stats for at-risk wetlands > 30 m from hydological feature
states_30m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>30),
            acres_atrisk = sum(ACRES[neardist>30]),
            pct_n_atrisk = sum(neardist>30)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>30])/sum(ACRES)*100) %>%
  mutate(distthreshold = 30)

# stats for at-risk wetlands > 40 m from hydological feature
states_40m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>40),
            acres_atrisk = sum(ACRES[neardist>40]),
            pct_n_atrisk = sum(neardist>40)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>40])/sum(ACRES)*100) %>%
  mutate(distthreshold = 40)

# stats for at-risk wetlands > 50 m from hydological feature
states_50m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>50),
            acres_atrisk = sum(ACRES[neardist>50]),
            pct_n_atrisk = sum(neardist>50)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>50])/sum(ACRES)*100) %>%
  mutate(distthreshold = 50)

# stats for at-risk wetlands > 60 m from hydological feature
states_60m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>60),
            acres_atrisk = sum(ACRES[neardist>60]),
            pct_n_atrisk = sum(neardist>60)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>60])/sum(ACRES)*100) %>%
  mutate(distthreshold = 60)

# stats for at-risk wetlands > 70 m from hydological feature
states_70m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>70),
            acres_atrisk = sum(ACRES[neardist>70]),
            pct_n_atrisk = sum(neardist>70)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>70])/sum(ACRES)*100) %>%
  mutate(distthreshold = 70)

# stats for at-risk wetlands > 80 m from hydological feature
states_80m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>80),
            acres_atrisk = sum(ACRES[neardist>80]),
            pct_n_atrisk = sum(neardist>80)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>80])/sum(ACRES)*100) %>%
  mutate(distthreshold = 80)

# stats for at-risk wetlands > 90 m from hydological feature
states_90m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>90),
            acres_atrisk = sum(ACRES[neardist>90]),
            pct_n_atrisk = sum(neardist>90)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>90])/sum(ACRES)*100) %>%
  mutate(distthreshold = 90)

# stats for at-risk wetlands > 100 m from hydological feature
states_100m <- states0.25 %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(neardist>100),
            acres_atrisk = sum(ACRES[neardist>100]),
            pct_n_atrisk = sum(neardist>100)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>100])/sum(ACRES)*100) %>%
  mutate(distthreshold = 100)


states_thresholds <- rbind(states_1m,states_10m,states_20m,states_30m,states_40m,states_50m,
                           states_60m,states_70m,states_80m,states_90m,states_100m)



########### BY STATE & WETLAND_TYPE ##############

# stats for at-risk wetlands > 1 m from hydological feature
states_1m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>1),
            acres_atrisk = sum(ACRES[neardist>1]),
            pct_n_atrisk = sum(neardist>1)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>1])/sum(ACRES)*100) %>%
  mutate(distthreshold = 1)

# stats for at-risk wetlands > 10 m from hydological feature
states_10m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>10),
            acres_atrisk = sum(ACRES[neardist>10]),
            pct_n_atrisk = sum(neardist>10)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>10])/sum(ACRES)*100) %>%
  mutate(distthreshold = 10)

# stats for at-risk wetlands > 20 m from hydological feature
states_20m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>20),
            acres_atrisk = sum(ACRES[neardist>20]),
            pct_n_atrisk = sum(neardist>20)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>20])/sum(ACRES)*100) %>%
  mutate(distthreshold = 20)

# stats for at-risk wetlands > 30 m from hydological feature
states_30m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>30),
            acres_atrisk = sum(ACRES[neardist>30]),
            pct_n_atrisk = sum(neardist>30)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>30])/sum(ACRES)*100) %>%
  mutate(distthreshold = 30)

# stats for at-risk wetlands > 40 m from hydological feature
states_40m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>40),
            acres_atrisk = sum(ACRES[neardist>40]),
            pct_n_atrisk = sum(neardist>40)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>40])/sum(ACRES)*100) %>%
  mutate(distthreshold = 40)

# stats for at-risk wetlands > 50 m from hydological feature
states_50m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>50),
            acres_atrisk = sum(ACRES[neardist>50]),
            pct_n_atrisk = sum(neardist>50)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>50])/sum(ACRES)*100) %>%
  mutate(distthreshold = 50)

# stats for at-risk wetlands > 60 m from hydological feature
states_60m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>60),
            acres_atrisk = sum(ACRES[neardist>60]),
            pct_n_atrisk = sum(neardist>60)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>60])/sum(ACRES)*100) %>%
  mutate(distthreshold = 60)

# stats for at-risk wetlands > 70 m from hydological feature
states_70m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>70),
            acres_atrisk = sum(ACRES[neardist>70]),
            pct_n_atrisk = sum(neardist>70)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>70])/sum(ACRES)*100) %>%
  mutate(distthreshold = 70)

# stats for at-risk wetlands > 80 m from hydological feature
states_80m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>80),
            acres_atrisk = sum(ACRES[neardist>80]),
            pct_n_atrisk = sum(neardist>80)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>80])/sum(ACRES)*100) %>%
  mutate(distthreshold = 80)

# stats for at-risk wetlands > 90 m from hydological feature
states_90m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>90),
            acres_atrisk = sum(ACRES[neardist>90]),
            pct_n_atrisk = sum(neardist>90)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>90])/sum(ACRES)*100) %>%
  mutate(distthreshold = 90)

# stats for at-risk wetlands > 100 m from hydological feature
states_100m <- states0.25 %>%
  group_by(state, WETLAND_TYPE) %>%
  summarise(n_atrisk = sum(neardist>100),
            acres_atrisk = sum(ACRES[neardist>100]),
            pct_n_atrisk = sum(neardist>100)/n()*100,
            pct_acres_atrisk = sum(ACRES[neardist>100])/sum(ACRES)*100) %>%
  mutate(distthreshold = 100)


states_thresholds_wetlands <- rbind(states_1m,states_10m,states_20m,states_30m,states_40m,states_50m,
                                    states_60m,states_70m,states_80m,states_90m,states_100m)


################# BY THRESHOLD ###############

# national stats by threshold
national <- states_thresholds %>%
  mutate(threshold_cat = as.factor(distthreshold)) %>%
  group_by(threshold_cat) %>%
  summarise(n_atrisk_total = sum(n_atrisk),
            acres_atrisk_total = sum(acres_atrisk),
            pct_n_atrisk_total = sum(n_atrisk)/nrow(states0.25)*100,
            pct_acres_atrisk_total = sum(acres_atrisk)/sum(states0.25$ACRES)*100)

# national stats by threshold and WETLAND_TYPE
national_wetlands <- states_thresholds_wetlands %>%
  mutate(threshold_cat = as.factor(distthreshold)) %>%
  group_by(threshold_cat, WETLAND_TYPE) %>%
  summarise(n_atrisk_total = sum(n_atrisk),
            acres_atrisk_total = sum(acres_atrisk)) %>%
  as.data.frame()

wetland_numbers <- states0.25 %>%
  group_by(WETLAND_TYPE) %>%
  summarise(total_number = n(),
            total_acreage = sum(ACRES)) %>%
  as.data.frame()

national_wetlands <- merge(national_wetlands, wetland_numbers, by = "WETLAND_TYPE", all.x = TRUE)

national_wetlands <- national_wetlands %>%
  mutate(pct_natrisk_total = n_atrisk_total/total_number*100,
         pct_acres_atrisk_total = acres_atrisk_total/total_acreage*100) %>%
  select(-c(total_number, total_acreage))

state_numbers <- states0.25 %>%
  group_by(state) %>%
  summarise(total_number = n(),
            total_acreage = sum(ACRES)) %>%
  as.data.frame()


############## save datasets ##############


write.csv(states_thresholds, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/state_thresholds.csv')
write.csv(states_thresholds_wetlands, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/state_thresholds_types.csv')

write.csv(national, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_thresholds.csv')
write.csv(national_wetlands, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_thresholds_types.csv')

write.csv(state_numbers, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_stats_state.csv')
write.csv(wetland_numbers, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_stats_type.csv')

#############################


############### Adding State Protections #######################

protections <- read.csv(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/state_protections.csv')

state_thresholds_protection <- merge(states_thresholds, protections, by = "state", all.x = TRUE)
state_thresholds_wetlands_protection <- merge(states_thresholds_wetlands, protections, by = "state", all.x = TRUE)


# national stats by threshold & protection
national_isolated_protection <- state_thresholds_protection %>%
  mutate(threshold_cat = as.factor(distthreshold)) %>%
  group_by(threshold_cat, GIW_protection) %>%
  summarise(n_atrisk_total = sum(n_atrisk),
            acres_atrisk_total = sum(acres_atrisk),
            pct_n_atrisk_total = sum(n_atrisk)/nrow(states0.25)*100,
            pct_acres_atrisk_total = sum(acres_atrisk)/sum(states0.25$ACRES)*100)

national_isolated_protection$threshold <- as.numeric(national_isolated_protection$threshold_cat)
national_isolated_protection$threshold <- ifelse(national_isolated_protection$threshold > 1, (national_isolated_protection$threshold-1)*10,national_isolated_protection$threshold)

# national stats by threshold & wetland & protection
national_isolated_type_protection <- state_thresholds_wetlands_protection %>%
  mutate(threshold_cat = as.factor(distthreshold)) %>%
  group_by(threshold_cat, WETLAND_TYPE, GIW_protection) %>%
  summarise(n_atrisk_total = sum(n_atrisk),
            acres_atrisk_total = sum(acres_atrisk),
            pct_n_atrisk_total = sum(n_atrisk)/nrow(states0.25)*100,
            pct_acres_atrisk_total = sum(acres_atrisk)/sum(states0.25$ACRES)*100)

national_isolated_type_protection$threshold <- as.numeric(national_isolated_type_protection$threshold_cat)
national_isolated_type_protection$threshold <- ifelse(national_isolated_type_protection$threshold > 1, (national_isolated_type_protection$threshold-1)*10,national_isolated_type_protection$threshold)

# national stats by threshold & wetland (limited/no protection only)

national_isolated_type_unprotected <- state_thresholds_wetlands_protection %>%
  filter(GIW_protection == "No" | GIW_protection == "Some") %>%
  mutate(threshold_cat = as.factor(distthreshold)) %>%
  group_by(threshold_cat, WETLAND_TYPE) %>%
  summarise(n_atrisk_total = sum(n_atrisk),
            acres_atrisk_total = sum(acres_atrisk),
            pct_n_atrisk_total = sum(n_atrisk)/nrow(states0.25)*100,
            pct_acres_atrisk_total = sum(acres_atrisk)/sum(states0.25$ACRES)*100)


national_isolated_type_unprotected$threshold <- as.numeric(national_isolated_type_unprotected$threshold_cat)
national_isolated_type_unprotected$threshold <- ifelse(national_isolated_type_unprotected$threshold > 1, (national_isolated_type_unprotected$threshold-1)*10,national_isolated_type_unprotected$threshold)


#write.csv(national_isolated_protection, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_type_protections.csv')
write.csv(national_isolated_type_protection, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_type_protections.csv')
write.csv(national_isolated_type_unprotected, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_type_unprotected.csv')


test <- state_thresholds_wetlands_protection %>%
  mutate(threshold_cat = as.factor(distthreshold)) %>%
  group_by(threshold_cat, WETLAND_TYPE, GIW_protection) %>%
  summarise(n_atrisk_total = sum(n_atrisk),
            acres_atrisk_total = sum(acres_atrisk))
test2 <- merge(test, wetland_numbers, by = "WETLAND_TYPE", all.x = TRUE) %>%
  mutate(pct_n_atrisk = n_atrisk_total/total_number*100,
         pct_acres_atrisk = acres_atrisk_total/total_acreage*100)

test2 %>%
  filter(threshold_cat == "50") %>%
  ggplot(aes(fill = forcats::fct_rev(GIW_protection), y=pct_n_atrisk, x=WETLAND_TYPE)) +
  geom_bar(position="stack", stat="identity") +
  ylim(0,50)

test2 %>%
  filter(threshold_cat == "50") %>%
  ggplot(aes(fill = forcats::fct_rev(GIW_protection), y=pct_acres_atrisk, x=WETLAND_TYPE)) +
  geom_bar(position="stack", stat="identity") +
  ylim(0,50)


############ PLOTS #############

mid <- 35

ggplot(states_thresholds, aes(x = distthreshold, y = pct_n_atrisk, colour = pct_n_atrisk)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 75, len = 4)) +
  xlab("Distance to Nearest Hydrological Feature (m)") +
  ylab("Share of State's Wetlands Considered Isolated (%)") +
  labs(colour='%') +
  facet_wrap(vars(state), nrow = 10) +
  scale_colour_gradient2(midpoint = mid, low = "black", mid = "blue", high = "red", na.value = NA)

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/state_thresholds_pctN.png', width = 158, height = 196, dpi = 500, units = "mm", bg = "transparent")


mid <- 20

ggplot(states_thresholds, aes(x = distthreshold, y = pct_acres_atrisk, colour = pct_acres_atrisk)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 40, len = 3)) +
  xlab("Distance to Nearest Hydrological Feature (m)") +
  ylab("Share of State's Wetland Area Considered Isolated (%)") +
  labs(colour='%') +
  facet_wrap(vars(state), nrow = 10) +
  scale_colour_gradient2(midpoint = mid, low = "black", mid = "blue", high = "red", na.value = NA)

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/state_thresholds_pctAC.png', width = 158, height = 196, dpi = 500, units = "mm", bg = "transparent")



national$threshold <- as.numeric(national$threshold_cat)
national$threshold <- ifelse(national$threshold > 1, (national$threshold-1)*10,national$threshold)

ggplot() +
  geom_line(data = national, aes(x = threshold, y = pct_n_atrisk_total), color = "blue", linewidth = 1) +
  geom_line(data = national, aes(x = threshold, y = pct_acres_atrisk_total), color = "red", linewidth = 1) +
  xlab("Distance to Nearest Hydrological Feature (m)") +
  ylab("Percent At Risk") +
  ylim(0,40) +
  geom_point(data = national, aes(x = threshold, y = pct_n_atrisk_total), color = "blue") +
  geom_point(data = national, aes(x = threshold, y = pct_acres_atrisk_total), color = "red")

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_thresholds.png', width = 158, height = 158, dpi = 500, units = "mm", bg = "transparent")



national_wetlands$threshold <- as.numeric(national_wetlands$threshold_cat)
national_wetlands$threshold <- ifelse(national_wetlands$threshold > 1, (national_wetlands$threshold-1)*10,national_wetlands$threshold)

ggplot(national_wetlands, aes(x=threshold, y=pct_natrisk_total, group=WETLAND_TYPE, color=WETLAND_TYPE)) +
  geom_line(linewidth = 1) +
  xlab("Distance to Nearest Hydrological Feature (m)") +
  ylab("Percent At Risk") +
  geom_point() +
  ylim(0,60)

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_thresholds_types.png', width = 200, height = 158, dpi = 500, units = "mm", bg = "transparent")



ggplot(national_isolated_protection, aes(x = threshold, y = n_atrisk_total, fill = forcats::fct_rev(GIW_protection))) +
  geom_area(position = 'stack') +
  scale_y_continuous(name = "Number", sec.axis = sec_axis(trans = ~./253844.5, name = "Pct")) +
  geom_point(position = 'stack')

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_n_protections.png', width = 200, height = 158, dpi = 500, units = "mm", bg = "transparent")

ggplot(national_isolated_protection, aes(x = threshold, y = acres_atrisk_total, fill = forcats::fct_rev(GIW_protection))) +
  geom_area(position = 'stack') +
  scale_y_continuous(name = "Number", sec.axis = sec_axis(trans = ~./2914783, name = "Pct")) +
  geom_point(position = 'stack')

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_acres_protections.png', width = 200, height = 158, dpi = 500, units = "mm", bg = "transparent")


national_isolated_type_protection$WETLAND_TYPE <- factor(national_isolated_type_protection$WETLAND_TYPE,
                                                         levels = c("Estuarine and Marine Wetland","Freshwater Forested/Shrub Wetland",
                                                                    "Freshwater Emergent Wetland","Freshwater Pond","Lake","Riverine","Other"))
national_isolated_type_unprotected$WETLAND_TYPE <- factor(national_isolated_type_unprotected$WETLAND_TYPE,
                                                         levels = c("Estuarine and Marine Wetland","Freshwater Forested/Shrub Wetland",
                                                                    "Freshwater Emergent Wetland","Freshwater Pond","Lake","Riverine","Other"))
national_wetlands$WETLAND_TYPE <- factor(national_wetlands$WETLAND_TYPE,
                                                          levels = c("Estuarine and Marine Wetland","Freshwater Forested/Shrub Wetland",
                                                                     "Freshwater Emergent Wetland","Freshwater Pond","Lake","Riverine","Other"))

ggplot(national_isolated_type_unprotected, aes(x = threshold, y = n_atrisk_total, fill = WETLAND_TYPE)) +
  geom_area(position = 'stack') +
  scale_y_continuous(name = "Number", sec.axis = sec_axis(trans = ~./254165.8, name = "Pct")) +
  geom_point(position = 'stack')

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_n_type_unprotected.png', width = 200, height = 158, dpi = 500, units = "mm", bg = "transparent")

ggplot(national_isolated_type_unprotected, aes(x = threshold, y = acres_atrisk_total, fill = WETLAND_TYPE)) +
  geom_area(position = 'stack') +
  scale_y_continuous(name = "Acres", sec.axis = sec_axis(trans = ~./2918865, name = "Pct")) +
  geom_point(position = 'stack')

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_acres_type_unprotected.png', width = 200, height = 158, dpi = 500, units = "mm", bg = "transparent")





state_medians <- states0.25 %>%
  group_by(state) %>%
  summarise(ac_1m = median(ACRES[neardist>1]),
            ac_10m = median(ACRES[neardist>10]),
            ac_20m = median(ACRES[neardist>20]),
            ac_30m = median(ACRES[neardist>30]),
            ac_40m = median(ACRES[neardist>40]),
            ac_50m = median(ACRES[neardist>50]),
            ac_60m = median(ACRES[neardist>60]),
            ac_70m = median(ACRES[neardist>70]),
            ac_80m = median(ACRES[neardist>80]),
            ac_90m = median(ACRES[neardist>90]),
            ac_100m = median(ACRES[neardist>100]))


mid <- 37

ggplot(states_thresholds, aes(x = distthreshold, y = pct_n_atrisk, colour = pct_n_atrisk)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 75, len = 4)) +
  xlab("Distance to Nearest Hydrological Feature (m)") +
  ylab("Share of State's Wetlands Considered Isolated (%)") +
  labs(colour='%') +
  facet_wrap(vars(state), nrow = 10) +
  scale_colour_gradient2(midpoint = mid, low = "black", mid = "blue", high = "red", na.value = NA)

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/state_thresholds_pctN.png', width = 158, height = 196, dpi = 500, units = "mm", bg = "transparent")








