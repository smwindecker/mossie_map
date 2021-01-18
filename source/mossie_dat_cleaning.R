### This is the script to explore/clean the mosquito data so that is ready for the SDM models

library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(raster)

####################################
### Mornington Dataset from Jane ###
####################################
mozdat.raw <- read.csv("data-raw/mossie-data/MozData.csv")
trapdat.raw <- read.csv("data-raw/mossie-data/TrapData.csv")


## A lot of the columns were set up for weather conditions but never had anything entered, so I am going to rempve those
na_cols <- function(x) any(!is.na(x))

trapdat.raw %>% 
  select_if(na_cols) -> trapdat.raw #That removed 22 columns with no info

table(trapdat.raw$TRAP_PICKUP_TRAP_STATUS) 
#I am also taking out the 13 failed trap records: 
trapdat.raw <- trapdat.raw[trapdat.raw$TRAP_PICKUP_TRAP_STATUS ==  "Successful",]

trapdat.stat <- data.frame("n_traps_set" = nrow(trapdat.raw))
trapdat.stat$n_sites <- length(unique(trapdat.raw$SITE_NAME))

trapdat.stat$unique_coords <- nrow(unique(trapdat.raw[,5:6]))

trapdat.stat$n_days <- length(unique(a.noto.final$set_date))

trapdat.stat$mos.pres <- nrow(trapdat.raw[!trapdat.raw$TRAPPED_SPECIES == "None", ])
trapdat.stat$mos.abs <- nrow(trapdat.raw[trapdat.raw$TRAPPED_SPECIES == "None", ])
trapdat.stat$a.noto.pres <- nrow(filter(all.mos.dat, SCIENTIFIC_NAME == "Aedes notoscriptus"))
trapdat.stat$a.noto.abs <- (trapdat.stat$mos.abs + trapdat.stat$mos.pres - trapdat.stat$a.noto.pres)

write.csv(trapdat.stat, file= "data-raw/mossie-data/adult.stat.csv")


#I am going to remove irrelavent columns take a clean version
trapdat.clean <- trapdat.raw[, c("ID", "SITE_NAME", "SITE_LONGITUDE", "SITE_LATITUDE", "TRAP_SET_DATETIME_LOCAL", "TRAPPED_SPECIES", "hours")]
trapdat.clean <- rename(trapdat.clean, trap_ID = ID)

all.mos.dat <- merge(trapdat.clean, mozdat.raw, by.x= "trap_ID", by.y= "ADULT_RECORD_ID", all.x = TRUE)


a.noto.pres <- filter(all.mos.dat, SCIENTIFIC_NAME == "Aedes notoscriptus") #these are just the Aedes notoscriptus presence record

#there are 120 traps which caught mossies that weren't a.noto, plus 661 sites with no mosquitos, so I want to create an a.noto absences dataframe. 

all.mos.dat %>% 
  group_by(trap_ID) %>% 
  filter(all(is.na(SCIENTIFIC_NAME) | !SCIENTIFIC_NAME == "Aedes notoscriptus")) %>% 
  distinct(trap_ID, .keep_all = TRUE) -> a.noto.abs

#just to check that there is no overlap
nrow(merge(a.noto.pres, a.noto.abs, by= "trap_ID")) # BINGO !

#because the retained species and counts aren't meaningful, I am going to change them to avoid confusion. 

a.noto.abs$SCIENTIFIC_NAME <- "absence"
a.noto.abs$TOTAL_NUM_INSECTS <- 0

#All right, lets combine the presences & absences to make a final dataframe
a.noto.final <- rbind(a.noto.pres, a.noto.abs)

#I am also going to add an explicit presence (1) absence (0) column incase we don't want count numbers.

a.noto.final$PA <- if_else(a.noto.final$SCIENTIFIC_NAME == "Aedes notoscriptus", 1, 0)


#I want to convert the dates an then create a separate year/month/day column because I don't care about the exact minutes
a.noto.final$TRAP_SET_DATETIME_LOCAL <- dmy_hm(a.noto.final$TRAP_SET_DATETIME_LOCAL)
a.noto.final$set_date <- as.Date(a.noto.final$TRAP_SET_DATETIME_LOCAL)
a.noto.final <- subset(a.noto.final, select = -c(ID, TRAPPED_SPECIES, TRAP_SET_DATETIME_LOCAL))
write.csv(a.noto.final, file= "data-raw/mossie-data/adult.a.noto.final.csv")

### Lets try some more visualisation
ggplot(data = a.noto.final, aes(x= SITE_LONGITUDE, y= SITE_LATITUDE)) +
  geom_point(aes(shape= SCIENTIFIC_NAME, colour= TOTAL_NUM_INSECTS)) + 
  scale_shape_manual(values = c(4, 19)) +
  theme_classic()
ggsave("mossie_SDM/output/a.noto_pres.abs.png")

head(a.noto.final)

a.noto.final %>% 
  group_by(set_date) %>% 
  summarise("total.per.survey" = sum(TOTAL_NUM_INSECTS), "n.sites" = length(trap_ID)) %>% mutate("mos.per.site" = total.per.survey/n.sites) %>% 
  ggplot() +
  geom_point(aes(x= set_date, y= mos.per.site)) +
  geom_line(aes(x= set_date, y= mos.per.site), colour = "grey") +
  theme_bw()


ggplot(data = a.noto.final) +
  geom_point(aes(x= set_date, y= TOTAL_NUM_INSECTS))

############################
### Veronique count data ###
############################
egg.dat.raw <- read.csv("data-raw/mossie-data/Egg data Veronique.csv")
egg.dat.raw$trap <- duplicated(egg.dat.raw[,3:4])
egg.dat.raw$trap <- if_else(egg.dat.raw$trap== "FALSE", "A", "B")

# You know the drill by now, this is the exploration part

egg.stat <- data.frame("raw_traps" = nrow(egg.dat.raw))
egg.stat$n_locations <- length(unique(egg.dat.raw$location))

egg.stat$n_sites_raw <- nrow(egg.dat.raw[egg.dat.raw$trap == "A",])

# I am trying to make an index of sites, because two traps were put at the same gps locations
egg.dat.raw %>% 
  group_by(lat) %>% 
  group_by(long, add= TRUE) %>% 
  mutate("site"= cur_group_id()) %>% 
  ungroup() -> egg.dat.cleanish


#removing NAs
egg.dat.cleanish <- na.omit(egg.dat.cleanish)
egg.dat.cleanish <- egg.dat.cleanish[!egg.dat.cleanish$eggs == "yes",]
egg.dat.cleanish <- egg.dat.cleanish[!egg.dat.cleanish$eggs == "",]
egg.dat.cleanish$eggs <- as.numeric(egg.dat.cleanish$eggs)

egg.stat$successful_traps <- (nrow(egg.dat.cleanish))
egg.stat$traps_lost <- (egg.stat$raw_traps - egg.stat$successful_traps)


egg.stat$sites_lost <- (egg.stat$n_sites_raw- length(unique(egg.dat.cleanish$site)))
egg.stat$locations_lost <- egg.stat$n_locations - length(unique(egg.dat.cleanish$location))

egg.stat$pres <- nrow(egg.dat.cleanish[!egg.dat.cleanish$eggs == 0,])
egg.stat$abs <- nrow(egg.dat.cleanish[egg.dat.cleanish$eggs == 0,])

egg.stat$mean_eggs <- mean(unlist(egg.dat.cleanish[egg.dat.cleanish$eggs > 0, "eggs"]))
write.csv(egg.stat, file= "data-raw/mossie-data/egg.stat.csv")
  
##### Lets see if the two traps at each site have similar values #####

egg.dat.cleanish %>% 
  subset(eggs < 100) %>% 
  ggplot(., aes(x= site, y= eggs)) +
  geom_bar(stat = "identity", aes(fill= trap), position= "dodge")

egg.dat.cleanish %>% 
  tidyr::spread(key= trap, value= eggs) %>% 
  select(site, A, B) %>% 
  aggregate(x= ., by =list(egg.dat.cleanish$site), FUN = max, na.rm= TRUE) %>% 
  filter(A > -1, B > -1) -> egg.site.comp

#Is there a relationship between sites
summary(lm(data= egg.site.comp, A~ B))
    
ggplot(egg.site.comp, aes(x= A, y= B)) +
    geom_point(aes(colour= site)) +
    geom_smooth(method = "lm")
ggsave("mossie_SDM/output_mossie/A~B.lm.all.png")


#what if we excluded the sites with absences

egg.dat.cleanish %>% 
  filter(eggs > 0) %>% 
  tidyr::spread(key= trap, value= eggs)  %>% 
  select(site, A, B)  %>%  
  aggregate(x= ., by = list(pres.site.list$site), FUN = max, na.rm= TRUE) %>% 
  filter(A > -1, B > -1) -> egg.pres.only

ggplot(egg.pres.only, aes(x= A, y= B)) +
  geom_point(aes(colour= site))
summary(lm(data= egg.pres.only, A~ B))

#it is ver weighted by the outliers at the end so what if we exclude those
egg.dat.cleanish %>% 
  filter(eggs > 0) %>% 
  filter(eggs <= 500) -> egg.list_500

egg.dat.cleanish %>% 
  filter(eggs > 0) %>% 
  filter(eggs <= 500) %>% 
  tidyr::spread(key= trap, value= eggs) %>% 
  select(site, A, B) %>% 
  aggregate(x= ., by =list(egg.list_500$site), FUN = max, na.rm= TRUE) %>% 
  filter(A > -1, B > -1) %>% 
  ggplot(., aes(x= A, y= B)) +
  geom_point(aes(colour= site)) +
  geom_smooth(method = "lm")
ggsave("mossie_SDM/output_mossie/A~B.lm.500.png")

#what if we go down to 200
egg.dat.cleanish %>% 
  filter(eggs <= 200) -> egg.list_200

egg.dat.cleanish %>% 
  filter(eggs <= 200) %>% 
  tidyr::spread(key= trap, value= eggs) %>% 
  select(site, A, B) %>% 
  aggregate(x= ., by =list(egg.list_200$site), FUN = max, na.rm= TRUE) %>% 
  filter(A > -1, B > -1) %>% 
  ggplot(., aes(x= A, y= B)) +
  geom_point(aes(colour= site)) +
  geom_smooth(method = "lm")

# I want to look at individual counts and the average counts per site (mean of trap A and B)

egg.dat.cleanish %>% 
  group_by(site) %>% 
  summarise(mean_eggs = mean(eggs), site = site, location= location, lat= lat, long= long, trap= "mean") -> egg.clean.mean.site 

ggplot(egg.dat.cleanish, aes(x= eggs, y= trap, colour= trap)) +
  geom_boxplot() +
  geom_boxplot(data = egg.clean.mean.site, aes(x= mean_eggs)) +
  labs(x= "egg count per week", subtitle =  "All data", y= "Trap per site") +
  theme_bw()

#### Comparing the time and spatial distribution of the egg/adult data ####

egg.dat.cleanish %>% 
  group_by(site) %>% 
  summarise(total_eggs = sum(eggs), site = site, location= location, long= long, lat= lat, trap= "total", traps.set = traps.set, traps.pick.up = traps.pick.up) %>% 
  distinct(site, .keep_all= TRUE) -> egg.total.site 
egg.total.site$traps.set <- as.Date(egg.total.site$traps.set, format= "%d/%m/%Y")
egg.total.site$traps.pick.up <- as.Date(egg.total.site$traps.pick.up, format= "%d/%m/%Y")


#Spatial distribution of sites
VIC.shape <- st_read("../VIC shape/VIC_GDA94LL_ADMIN_STATE.shp")

ggplot() +
  geom_sf(data= VIC.shape) +
  geom_point(data= trapdat.clean, aes(x= SITE_LONGITUDE, y= SITE_LATITUDE, colour= "Adult")) +
  geom_point(data= egg.total.site, aes(x= long, y= lat, colour= "Egg")) +
  coord_sf(xlim = c(143.5, 146), ylim = c(-38.8, -37.4)) +
  theme_classic()
ggsave("mossie_SDM/output_mossie/spatial_overlap.png")

# Temporal overlap of sites
a.noto.final %>% 
  group_by(set_date) %>% 
  summarise("total.per.survey" = sum(TOTAL_NUM_INSECTS), "n.sites" = length(trap_ID)) %>% mutate("mos.per.site" = total.per.survey/n.sites, "data" = "adult_count") %>% 
  dplyr ::select(set_date, mos.per.site, data) -> time.count.adults
  
egg.total.site %>% 
  group_by(traps.set) %>%
  summarise("total.per.location" = sum(total_eggs), "n.sites" = length(location)) %>%  
  mutate("mos.per.site" = total.per.location/n.sites, "data"= "egg_count") %>% 
  summarise("set_date"= traps.set, mos.per.site, data)  %>% 
  rbind(., time.count.adults) -> time.count.total

ggplot(time.count.total) +
  geom_point(aes(x= set_date, y= mos.per.site)) +
  geom_line(aes(x= set_date, y= mos.per.site, group= data), colour = "grey") +
  facet_grid(vars(data), scale= "free_y") +
  labs(y= "mean mosquitos per site", x= "setup date 2019-2020") +
  theme_bw()
ggsave("mossie_SDM/output_mossie/temporal_overlap.png")

#### Final egg data cleaning ####
egg.dat.final <- egg.dat.cleanish
egg.dat.final$traps.set <- as.Date(egg.dat.final$traps.set, format= "%d/%m/%Y")
egg.dat.final$traps.pick.up <- as.Date(egg.dat.final$traps.pick.up, format= "%d/%m/%Y")

egg.dat.final <- subset(egg.dat.final, select= c("location", "traps.set", "traps.pick.up", "long", "lat", "eggs", "site", "trap"))

write.csv(egg.dat.final, file= "data-raw/mossie-data/egg.dat.final.csv")


#### Adding the adult and egg data together ####

#Adding columns to distuinguish between eggs (1) and adults (0), and the number of traps 
a.noto.final$egg <- 0
a.noto.final$n_traps <- 1

#The egg.final still has the unique egg counts per trap at a site so we are going to sum those together

egg.dat.final %>% 
  group_by(site) %>% 
  add_tally(name = "n_traps") %>% 
  summarise(total_eggs = sum(eggs), site = site, location= location, long= long, lat= lat, traps.set = traps.set, traps.pick.up = traps.pick.up, n_traps= n_traps, egg = 1) %>% 
  distinct(site, .keep_all= TRUE) -> egg.sum

egg.sum$PA <- if_else(egg.sum$total_eggs > 0, 1, 0)

#taking the important columns and giving consistant column names
adult.count <- subset(a.noto.final, select= c("set_date", "SITE_LONGITUDE", "SITE_LATITUDE", "TOTAL_NUM_INSECTS", "PA", "egg", "n_traps"))
names(adult.count) <- c("set_date", "longitude", "latitude", "count_data", "PA", "egg", "n_traps")

egg.count <- subset(egg.sum, select= c("traps.set", "long", "lat", "total_eggs", "PA", "egg", "n_traps"))
names(egg.count) <- c("set_date", "longitude", "latitude", "count_data", "PA", "egg", "n_traps")

mos.data <- rbind(adult.count, egg.count) #binding the egg and adult data together

#creating date columns to flag when the temporal/static layers should be extracted
mos.data$collect_date <- if_else(mos.data$egg== 1, mos.data$set_date + 7, mos.data$set_date +1)

mos.data$meteo_start <- if_else(mos.data$egg== 1, mos.data$collect_date - (4*7), mos.data$collect_date - (6*7))

mos.data$meteo_end <- mos.data$meteo_start + (4*7)

mos.data$static_date <- rollback(mos.data$collect_date, roll_to_first = TRUE)

write.csv(mos.data, file= "mossie_SDM/final.mosquito.data.csv", row.names = FALSE)