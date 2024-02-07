library(tidyverse)
library(svglite)

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

# Combine the Lake and Lakes wetland types (same with RIverine)
states$wetland_type <- ifelse(states$wetland_type == "Lakes", "Lake", states$wetland_type)
states$wetland_type <- ifelse(states$wetland_type == "RIverine", "Riverine", states$wetland_type)

# Remove deepwater, estuaries, riverine & wetlands < 0.25 acres
states <- states %>%
  filter(wetland_type != "Estuarine and Marine Deepwater" & wetland_type != "Estuarine and Marine Wetland" &
           wetland_type != "Riverine" & acres >= 0.25) # %>%
  #select(-c(ATTRIBUTE, wetland_type, acres, nearest_m))

# Assign GAP status 9 to NAs
states <- states %>%
  mutate(GAP_Sts = coalesce(GAP_Sts, 9))

states$GAP_Sts_cat <- as.factor(states$GAP_Sts)

# Give each wetland a unique identifier
states$UniqueID = seq.int(nrow(states))





########### ALL WETLANDS (IGNORE GAP STATUS) ##############

###### BY STATE #########

# stats for at-risk wetlands > 1 m from hydrological feature
states_1m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>1),
            acres_atrisk = sum(acres[nearest_m>1]),
            pct_n_atrisk = sum(nearest_m>1)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>1])/sum(acres)*100) %>%
  mutate(distthreshold = 1)

# stats for at-risk wetlands > 10 m from hydrological feature
states_10m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>10),
            acres_atrisk = sum(acres[nearest_m>10]),
            pct_n_atrisk = sum(nearest_m>10)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>10])/sum(acres)*100) %>%
  mutate(distthreshold = 10)

# stats for at-risk wetlands > 20 m from hydrological feature
states_20m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>20),
            acres_atrisk = sum(acres[nearest_m>20]),
            pct_n_atrisk = sum(nearest_m>20)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>20])/sum(acres)*100) %>%
  mutate(distthreshold = 20)

# stats for at-risk wetlands > 30 m from hydrological feature
states_30m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>30),
            acres_atrisk = sum(acres[nearest_m>30]),
            pct_n_atrisk = sum(nearest_m>30)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>30])/sum(acres)*100) %>%
  mutate(distthreshold = 30)

# stats for at-risk wetlands > 40 m from hydrological feature
states_40m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>40),
            acres_atrisk = sum(acres[nearest_m>40]),
            pct_n_atrisk = sum(nearest_m>40)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>40])/sum(acres)*100) %>%
  mutate(distthreshold = 40)

# stats for at-risk wetlands > 50 m from hydrological feature
states_50m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>50),
            acres_atrisk = sum(acres[nearest_m>50]),
            pct_n_atrisk = sum(nearest_m>50)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>50])/sum(acres)*100) %>%
  mutate(distthreshold = 50)

# stats for at-risk wetlands > 60 m from hydrological feature
states_60m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>60),
            acres_atrisk = sum(acres[nearest_m>60]),
            pct_n_atrisk = sum(nearest_m>60)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>60])/sum(acres)*100) %>%
  mutate(distthreshold = 60)

# stats for at-risk wetlands > 70 m from hydrological feature
states_70m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>70),
            acres_atrisk = sum(acres[nearest_m>70]),
            pct_n_atrisk = sum(nearest_m>70)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>70])/sum(acres)*100) %>%
  mutate(distthreshold = 70)

# stats for at-risk wetlands > 80 m from hydrological feature
states_80m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>80),
            acres_atrisk = sum(acres[nearest_m>80]),
            pct_n_atrisk = sum(nearest_m>80)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>80])/sum(acres)*100) %>%
  mutate(distthreshold = 80)

# stats for at-risk wetlands > 90 m from hydrological feature
states_90m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>90),
            acres_atrisk = sum(acres[nearest_m>90]),
            pct_n_atrisk = sum(nearest_m>90)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>90])/sum(acres)*100) %>%
  mutate(distthreshold = 90)

# stats for at-risk wetlands > 100 m from hydrological feature
states_100m <- states %>%
  group_by(state) %>%
  summarise(n_atrisk = sum(nearest_m>100),
            acres_atrisk = sum(acres[nearest_m>100]),
            pct_n_atrisk = sum(nearest_m>100)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>100])/sum(acres)*100) %>%
  mutate(distthreshold = 100)


states_thresholds <- rbind(states_1m,states_10m,states_20m,states_30m,states_40m,states_50m,
                           states_60m,states_70m,states_80m,states_90m,states_100m)



########### BY STATE & wetland_type ##############

# stats for at-risk wetlands > 1 m from hydrological feature
states_1m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>1),
            acres_atrisk = sum(acres[nearest_m>1]),
            pct_n_atrisk = sum(nearest_m>1)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>1])/sum(acres)*100) %>%
  mutate(distthreshold = 1)

# stats for at-risk wetlands > 10 m from hydrological feature
states_10m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>10),
            acres_atrisk = sum(acres[nearest_m>10]),
            pct_n_atrisk = sum(nearest_m>10)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>10])/sum(acres)*100) %>%
  mutate(distthreshold = 10)

# stats for at-risk wetlands > 20 m from hydrological feature
states_20m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>20),
            acres_atrisk = sum(acres[nearest_m>20]),
            pct_n_atrisk = sum(nearest_m>20)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>20])/sum(acres)*100) %>%
  mutate(distthreshold = 20)

# stats for at-risk wetlands > 30 m from hydrological feature
states_30m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>30),
            acres_atrisk = sum(acres[nearest_m>30]),
            pct_n_atrisk = sum(nearest_m>30)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>30])/sum(acres)*100) %>%
  mutate(distthreshold = 30)

# stats for at-risk wetlands > 40 m from hydrological feature
states_40m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>40),
            acres_atrisk = sum(acres[nearest_m>40]),
            pct_n_atrisk = sum(nearest_m>40)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>40])/sum(acres)*100) %>%
  mutate(distthreshold = 40)

# stats for at-risk wetlands > 50 m from hydrological feature
states_50m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>50),
            acres_atrisk = sum(acres[nearest_m>50]),
            pct_n_atrisk = sum(nearest_m>50)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>50])/sum(acres)*100) %>%
  mutate(distthreshold = 50)

# stats for at-risk wetlands > 60 m from hydrological feature
states_60m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>60),
            acres_atrisk = sum(acres[nearest_m>60]),
            pct_n_atrisk = sum(nearest_m>60)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>60])/sum(acres)*100) %>%
  mutate(distthreshold = 60)

# stats for at-risk wetlands > 70 m from hydrological feature
states_70m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>70),
            acres_atrisk = sum(acres[nearest_m>70]),
            pct_n_atrisk = sum(nearest_m>70)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>70])/sum(acres)*100) %>%
  mutate(distthreshold = 70)

# stats for at-risk wetlands > 80 m from hydrological feature
states_80m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>80),
            acres_atrisk = sum(acres[nearest_m>80]),
            pct_n_atrisk = sum(nearest_m>80)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>80])/sum(acres)*100) %>%
  mutate(distthreshold = 80)

# stats for at-risk wetlands > 90 m from hydrological feature
states_90m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>90),
            acres_atrisk = sum(acres[nearest_m>90]),
            pct_n_atrisk = sum(nearest_m>90)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>90])/sum(acres)*100) %>%
  mutate(distthreshold = 90)

# stats for at-risk wetlands > 100 m from hydrological feature
states_100m <- states %>%
  group_by(state, wetland_type) %>%
  summarise(n_atrisk = sum(nearest_m>100),
            acres_atrisk = sum(acres[nearest_m>100]),
            pct_n_atrisk = sum(nearest_m>100)/n()*100,
            pct_acres_atrisk = sum(acres[nearest_m>100])/sum(acres)*100) %>%
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
            pct_n_atrisk_total = sum(n_atrisk)/nrow(states)*100,
            pct_acres_atrisk_total = sum(acres_atrisk)/sum(states$acres)*100)

# national stats by threshold and wetland_type
national_wetlands <- states_thresholds_wetlands %>%
  mutate(threshold_cat = as.factor(distthreshold)) %>%
  group_by(threshold_cat, wetland_type) %>%
  summarise(n_atrisk_type = sum(n_atrisk),
            acres_atrisk_type = sum(acres_atrisk)) %>%
  as.data.frame()

wetland_numbers <- states %>%
  group_by(wetland_type) %>%
  summarise(total_number_type = n(),
            total_acreage_type = sum(acres)) %>%
  as.data.frame()

national_wetlands <- merge(national_wetlands, wetland_numbers, by = "wetland_type", all.x = TRUE)

national_wetlands <- national_wetlands %>%
  mutate(total_number_wetlands = sum(wetland_numbers$total_number_type),
         total_acreage_wetlands = sum(wetland_numbers$total_acreage_type),
         pct_natrisk_type = n_atrisk_type/total_number_type*100,
         pct_acres_atrisk_type = acres_atrisk_type/total_acreage_type*100,
         pct_natrisk_total = n_atrisk_type/total_number_wetlands*100,
         pct_acres_atrisk_total = acres_atrisk_type/total_acreage_wetlands*100)

state_numbers <- states %>%
  group_by(state) %>%
  summarise(total_number = n(),
            total_acreage = sum(acres)) %>%
  as.data.frame()


############## save datasets ##############


write.csv(states_thresholds, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/state_thresholds.csv')
write.csv(states_thresholds_wetlands, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/state_thresholds_types.csv')

write.csv(national, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_thresholds.csv')
write.csv(national_wetlands, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_thresholds_types.csv')

write.csv(state_numbers, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_stats_state.csv')
write.csv(wetland_numbers, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_stats_type.csv')

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
            pct_n_atrisk_total = sum(n_atrisk)/nrow(states)*100,
            pct_acres_atrisk_total = sum(acres_atrisk)/sum(states$acres)*100)

national_isolated_protection$threshold <- as.numeric(national_isolated_protection$threshold_cat)
national_isolated_protection$threshold <- ifelse(national_isolated_protection$threshold > 1, (national_isolated_protection$threshold-1)*10,national_isolated_protection$threshold)

# national stats by threshold & wetland & protection
national_isolated_type_protection <- state_thresholds_wetlands_protection %>%
  mutate(threshold_cat = as.factor(distthreshold)) %>%
  group_by(threshold_cat, wetland_type, GIW_protection) %>%
  summarise(n_atrisk_total = sum(n_atrisk),
            acres_atrisk_total = sum(acres_atrisk),
            pct_n_atrisk_total = sum(n_atrisk)/nrow(states)*100,
            pct_acres_atrisk_total = sum(acres_atrisk)/sum(states$acres)*100)

national_isolated_type_protection$threshold <- as.numeric(national_isolated_type_protection$threshold_cat)
national_isolated_type_protection$threshold <- ifelse(national_isolated_type_protection$threshold > 1, (national_isolated_type_protection$threshold-1)*10,national_isolated_type_protection$threshold)

# national stats by threshold & wetland (limited/no protection only)

national_isolated_type_unprotected <- state_thresholds_wetlands_protection %>%
  filter(GIW_protection == "No" | GIW_protection == "Some") %>%
  mutate(threshold_cat = as.factor(distthreshold)) %>%
  group_by(threshold_cat, wetland_type) %>%
  summarise(n_atrisk_total = sum(n_atrisk),
            acres_atrisk_total = sum(acres_atrisk),
            pct_n_atrisk_total = sum(n_atrisk)/nrow(states)*100,
            pct_acres_atrisk_total = sum(acres_atrisk)/sum(states$acres)*100)


national_isolated_type_unprotected$threshold <- as.numeric(national_isolated_type_unprotected$threshold_cat)
national_isolated_type_unprotected$threshold <- ifelse(national_isolated_type_unprotected$threshold > 1, (national_isolated_type_unprotected$threshold-1)*10,national_isolated_type_unprotected$threshold)


#write.csv(national_isolated_protection, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_type_protections.csv')
write.csv(national_isolated_type_protection, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_type_protections.csv')
write.csv(national_isolated_type_unprotected, file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_type_unprotected.csv')




# DELETE I THINK
test <- state_thresholds_wetlands_protection %>%
  mutate(threshold_cat = as.factor(distthreshold)) %>%
  group_by(threshold_cat, wetland_type, GIW_protection) %>%
  summarise(n_atrisk_total = sum(n_atrisk),
            acres_atrisk_total = sum(acres_atrisk))
test2 <- merge(test, wetland_numbers, by = "wetland_type", all.x = TRUE) %>%
  mutate(pct_n_atrisk = n_atrisk_total/total_number*100,
         pct_acres_atrisk = acres_atrisk_total/total_acreage*100)

test2 %>%
  filter(threshold_cat == "50") %>%
  ggplot(aes(fill = forcats::fct_rev(GIW_protection), y=pct_n_atrisk, x=wetland_type)) +
  geom_bar(position="stack", stat="identity") +
  ylim(0,50)

test2 %>%
  filter(threshold_cat == "50") %>%
  ggplot(aes(fill = forcats::fct_rev(GIW_protection), y=pct_acres_atrisk, x=wetland_type)) +
  geom_bar(position="stack", stat="identity") +
  ylim(0,50)


############ PLOTS #############

ggplot(states_thresholds, aes(x = distthreshold, y = pct_n_atrisk, colour = pct_n_atrisk)) +
  geom_line(linewidth = 1) +
  #scale_y_continuous(breaks = seq(0, 75, len = 4)) +
  xlab("Distance to Nearest Hydrological Feature (m)") +
  ylab("Share of State's Wetlands Considered Isolated (%)") +
  labs(colour='%') +
  facet_wrap(vars(state), nrow = 10) +
  scale_colour_gradient2(midpoint = median(states_thresholds$pct_n_atrisk), low = "black", mid = "blue", high = "red", na.value = NA)

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/state_thresholds_pctN.svg', width = 158, height = 196, dpi = 500, units = "mm", bg = "transparent")


ggplot(states_thresholds, aes(x = distthreshold, y = pct_acres_atrisk, colour = pct_acres_atrisk)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 40, len = 3)) +
  xlab("Distance to Nearest Hydrological Feature (m)") +
  ylab("Share of State's Wetland Area Considered Isolated (%)") +
  labs(colour='%') +
  facet_wrap(vars(state), nrow = 10) +
  scale_colour_gradient2(midpoint = median(states_thresholds$pct_acres_atrisk), low = "black", mid = "blue", high = "red", na.value = NA)

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/state_thresholds_pctAC.svg', width = 158, height = 196, dpi = 500, units = "mm", bg = "transparent")



national$threshold <- as.numeric(national$threshold_cat)
national$threshold <- ifelse(national$threshold > 1, (national$threshold-1)*10,national$threshold)

ggplot() +
  geom_line(data = national, aes(x = threshold, y = pct_n_atrisk_total), color = "blue", linewidth = 1) +
  geom_line(data = national, aes(x = threshold, y = pct_acres_atrisk_total), color = "red", linewidth = 1) +
  xlab("Distance to Nearest Hydrological Feature (m)") +
  ylab("Percent At Risk") +
  ylim(0,50) +
  geom_point(data = national, aes(x = threshold, y = pct_n_atrisk_total), color = "blue") +
  geom_point(data = national, aes(x = threshold, y = pct_acres_atrisk_total), color = "red")

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_thresholds.svg', width = 158, height = 158, dpi = 500, units = "mm", bg = "transparent")



# FYI - to get the transformation value, divide n_atrisk_total by pct_n_atrisk_total
plot1 = ggplot(national_isolated_protection, aes(x = threshold, y = n_atrisk_total, fill = forcats::fct_rev(GIW_protection))) +
  geom_area(position = 'stack') +
  scale_y_continuous(name = "Number", sec.axis = sec_axis(trans = ~./170765.1, name = "Pct"))

ggsave(file='/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_n_protections.svg', plot=plot1, width=200, height=158, units = "mm", bg = "transparent")

plot2 = ggplot(national_isolated_protection, aes(x = threshold, y = acres_atrisk_total, fill = forcats::fct_rev(GIW_protection))) +
  geom_area(position = 'stack') +
  scale_y_continuous(name = "Area", sec.axis = sec_axis(trans = ~./2599282, name = "Pct"))

ggsave(file='/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_acres_protections.svg', plot=plot2, width=200, height=158, units = "mm", bg = "transparent")



national_wetlands$threshold <- as.numeric(national_wetlands$threshold_cat)
national_wetlands$threshold <- ifelse(national_wetlands$threshold > 1, (national_wetlands$threshold-1)*10,national_wetlands$threshold)

national_wetlands$wetland_type <- factor(national_wetlands$wetland_type, levels = c("Freshwater Forested/Shrub Wetland",
                                                                                    "Freshwater Emergent Wetland","Freshwater Pond","Lake","Other"))

plot1.1 = ggplot(national_wetlands, aes(x = threshold, y = n_atrisk_type, fill = wetland_type)) +
  geom_area(position = 'stack') +
  scale_y_continuous(name = "Number", sec.axis = sec_axis(trans = ~./170765.1, name = "Pct"))

ggsave(file='/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_n_type.svg', plot=plot1.1, width=200, height=158, units = "mm", bg = "transparent")

plot2.1 = ggplot(national_wetlands, aes(x = threshold, y = acres_atrisk_type, fill = wetland_type)) +
  geom_area(position = 'stack') +
  scale_y_continuous(name = "Area", sec.axis = sec_axis(trans = ~./2599282, name = "Pct"))

ggsave(file='/Users/bsimm/Dropbox/Tampa Bay Estuary Program/Research/SCOTUS WOTUS/national_acres_type.svg', plot=plot2.1, width=200, height=158, units = "mm", bg = "transparent")






# PIE TESTS
states_thresholds_wetlands$wetland_type <- factor(states_thresholds_wetlands$wetland_type, levels = c("Estuarine and Marine Wetland","Freshwater Forested/Shrub Wetland",
                                                                                    "Freshwater Emergent Wetland","Freshwater Pond","Lake","Riverine","Other"))
states_thresholds_wetlands$state <- factor(states_thresholds_wetlands$state)

forpies1 <- states_thresholds_wetlands %>%
  filter(distthreshold == 50) %>%
  group_by(wetland_type, state, .drop = FALSE) %>%
  summarise(n_atrisk_total = sum(n_atrisk))


pieplot1 <- ggplot(forpies1, aes(x="", y=n_atrisk_total, fill=wetland_type)) +
  geom_bar(width = 1, stat = "identity", position = position_fill()) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("Estuarine and Marine Wetland" = "#7B76AD",
                               "Freshwater Forested/Shrub Wetland" = "#017B7B",
                               "Freshwater Emergent Wetland" = "#02ADAB",
                               "Freshwater Pond" = "#015D7E",
                               "Lake" = "#58A6C3",
                               "Riverine" = "#A68461",
                               "Other" = "#4A4A4A")) +
  facet_wrap(~state, nrow = 5) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

ggsave(file='/Users/bsimm/Dropbox/Tampa Bay Estuary Program/state_n_type_isolated.svg', plot=pieplot1, width=200, height=90, units = "mm", bg = "transparent")

forpies2 <- states_thresholds_wetlands %>%
  filter(distthreshold == 50) %>%
  group_by(wetland_type, state, .drop = FALSE) %>%
  summarise(acres_atrisk_total = sum(acres_atrisk))


pieplot2 <- ggplot(forpies2, aes(x="", y=acres_atrisk_total, fill=wetland_type)) +
  geom_bar(width = 1, stat = "identity", position = position_fill()) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("Estuarine and Marine Wetland" = "#7B76AD",
                               "Freshwater Forested/Shrub Wetland" = "#017B7B",
                               "Freshwater Emergent Wetland" = "#02ADAB",
                               "Freshwater Pond" = "#015D7E",
                               "Lake" = "#58A6C3",
                               "Riverine" = "#A68461",
                               "Other" = "#4A4A4A")) +
  facet_wrap(~state, nrow = 5) +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

ggsave(file='/Users/bsimm/Dropbox/Tampa Bay Estuary Program/state_acres_type_isolated.svg', plot=pieplot2, width=200, height=90, units = "mm", bg = "transparent")









national_wetlands$threshold <- as.numeric(national_wetlands$threshold_cat)
national_wetlands$threshold <- ifelse(national_wetlands$threshold > 1, (national_wetlands$threshold-1)*10,national_wetlands$threshold)

plot1 = ggplot(national_wetlands, aes(x=threshold, y=pct_natrisk_total, group=wetland_type, color=wetland_type)) +
  geom_line(linewidth = 1) +
  xlab("Distance to Nearest Hydrological Feature (m)") +
  ylab("Percent At Risk") +
  ylim(0,15)

ggsave(file='/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_thresholds_n_types.svg', plot=plot1, width=200, height=158, units = "mm", bg = "transparent")

plot2 = ggplot(national_wetlands, aes(x=threshold, y=pct_acres_atrisk_total, group=wetland_type, color=wetland_type)) +
  geom_line(linewidth = 1) +
  xlab("Distance to Nearest Hydrological Feature (m)") +
  ylab("Percent Area At Risk") +
  ylim(0,15)

ggsave(file='/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_thresholds_acres_types.svg', plot=plot2, width=200, height=158, units = "mm", bg = "transparent")



#~~~~~~~~~~~~~~~~~~~~~~





national_isolated_type_protection$wetland_type <- factor(national_isolated_type_protection$wetland_type,
                                                         levels = c("Estuarine and Marine Wetland","Freshwater Forested/Shrub Wetland",
                                                                    "Freshwater Emergent Wetland","Freshwater Pond","Lake","Riverine","Other"))
national_isolated_type_unprotected$wetland_type <- factor(national_isolated_type_unprotected$wetland_type,
                                                         levels = c("Estuarine and Marine Wetland","Freshwater Forested/Shrub Wetland",
                                                                    "Freshwater Emergent Wetland","Freshwater Pond","Lake","Riverine","Other"))
national_wetlands$wetland_type <- factor(national_wetlands$wetland_type,
                                                          levels = c("Estuarine and Marine Wetland","Freshwater Forested/Shrub Wetland",
                                                                     "Freshwater Emergent Wetland","Freshwater Pond","Lake","Riverine","Other"))

ggplot(national_isolated_type_unprotected, aes(x = threshold, y = n_atrisk_total, fill = wetland_type)) +
  geom_area(position = 'stack') +
  scale_y_continuous(name = "Number", sec.axis = sec_axis(trans = ~./254165.8, name = "Pct")) +
  geom_point(position = 'stack')

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_n_type_unprotected.png', width = 200, height = 158, dpi = 500, units = "mm", bg = "transparent")

ggplot(national_isolated_type_unprotected, aes(x = threshold, y = acres_atrisk_total, fill = wetland_type)) +
  geom_area(position = 'stack') +
  scale_y_continuous(name = "acres", sec.axis = sec_axis(trans = ~./2918865, name = "Pct")) +
  geom_point(position = 'stack')

ggsave(file = '/Users/bsimm/Dropbox/Tampa Bay Estuary Program/national_acres_type_unprotected.png', width = 200, height = 158, dpi = 500, units = "mm", bg = "transparent")





state_medians <- states %>%
  group_by(state) %>%
  summarise(ac_1m = median(acres[nearest_m>1]),
            ac_10m = median(acres[nearest_m>10]),
            ac_20m = median(acres[nearest_m>20]),
            ac_30m = median(acres[nearest_m>30]),
            ac_40m = median(acres[nearest_m>40]),
            ac_50m = median(acres[nearest_m>50]),
            ac_60m = median(acres[nearest_m>60]),
            ac_70m = median(acres[nearest_m>70]),
            ac_80m = median(acres[nearest_m>80]),
            ac_90m = median(acres[nearest_m>90]),
            ac_100m = median(acres[nearest_m>100]))


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








