library(tidyverse)
library(haven)
library(stargazer)
library(data.table)
library(dtplyr)
library(geosphere)
library(readxl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(performance)
library(lmtest)

# Key variables:
#   conflicts: conflicts with fatal level in a dyad
#   dyads: all state dyads 1816 -- 2016
#   states: coding system used in COW project

setwd("~/Workspace/repo-graduate-ruc/IR_Theory/data/")

dmid <- read_dta("dyadic_mid_4.02.dta")
# MIDB <- read_csv("MIDB_3.10.csv")
MIDB <- read_dta("MIDB 5.0.dta")

aggr <- dmid %>% filter(sideaa == 1) %>%
  select(disno, namea, nameb, strtyr, endyear, fatlev)

conflict_1y <- aggr %>% filter(strtyr == endyear) %>%
  mutate(year = strtyr, dura = 1) %>%
  select(-c(strtyr, endyear))

conflict_my <- aggr %>% filter(strtyr != endyear) %>%
  group_by(disno, namea, nameb) %>%
  summarize(fatlev = max(fatlev),
            strtyr = max(strtyr),
            endyear = max(endyear)) %>%
  mutate(dura = endyear - strtyr + 1)

mytmp <- data.frame()

for(i in 1:nrow(conflict_my)) {
  submy <- conflict_my[i,]
  multiyear <- submy$dura
  submy <- submy %>% slice(rep(1:n(), each = multiyear))
  submy$year <- submy$strtyr:submy$endyear
  mytmp <- rbind(mytmp,
                 submy)
}

conflict_my <- mytmp %>% select(-c(strtyr, endyear))

conflicts <- rbind(conflict_1y, conflict_my)

polity <- read_xls("p5v2018d.xls")%>%
  select(c(scode, polity, byear, eyear)) %>%
  mutate(eyear = replace(eyear, eyear >= 2017, 2017)) %>%
  filter(eyear >= 1816, byear <= 2016) %>%
  mutate(dura = eyear - byear + 1)

pol_tmp <- data.frame()

for(i in 1:nrow(polity)) {
  subpol <- polity[i,]
  years <- as.numeric(subpol$dura)
  byear <- as.numeric(subpol$byear)
  eyear <- as.numeric(subpol$eyear)
  subpol <- subpol %>% slice(rep(1:n(), each = years))
  subpol$year <- byear:eyear 
  pol_tmp <- rbind(pol_tmp, subpol[1:nrow(subpol) - 1,])
}

polity <- pol_tmp %>% 
  select(c(scode, polity, year)) %>%
  mutate(scode = replace(scode, scode == "USR", "RUS"))

conflicts <- left_join(conflicts, polity,
                       by = c("namea" = "scode", "year" = "year")) %>%
  rename(polity_A = polity)

conflicts <- left_join(conflicts, polity,
                       by = c("nameb" = "scode", "year" = "year")) %>%
  rename(polity_B = polity)


states <- read_csv("states2016.csv") %>% 
  select(stateabb, ccode, statenme, styear, endyear)

st_names <- unique(states$stateabb)
len <- length(st_names)

st_A <- rep(st_names, each = len)
st_A <- rep(st_A, times = 201)
st_B <- rep(st_names, times = len)
st_B <- rep(st_B, times = 201)

year <- rep(1816:2016, each = len^2)

dyads <- as_tibble(cbind(st_A, st_B, year)) %>%
  mutate(valid = st_A != st_B) %>%
  filter(valid) %>%
  select(-valid) %>%
  mutate(valid_A = F, valid_B = F)

states_vivo <- rep(st_names, times = 201)
states_vivo <- cbind(st_name = states_vivo,
                     year = rep(1816:2016, each = len),
                     country_exists = F)
states_vivo <- as_tibble(states_vivo)

for(i in 1:nrow(states_vivo)) {
  country <- states_vivo[i,]$st_name
  year <- states_vivo[i,]$year
  sub_states <- states[states$stateabb == country,]
  country_exists <- F
  for(j in 1:nrow(sub_states)){
    country_exists <- year >= sub_states[j,]$styear & 
      year <= sub_states[j,]$endyear
    if(country_exists) break
  }
  states_vivo$country_exists[i] <- country_exists
}

states_vivo <- states_vivo %>% filter(country_exists == T) %>%
  select(-country_exists)

tmp <- states_vivo %>% group_by(year) %>%
  summarize(count = n())

dyads <- data.frame()

for (i in 1816:2016) {
  subst <- states_vivo %>% filter(year == i)
  ttl <- nrow(subst)
  countries <- subst$st_name
  st_A <- rep(countries, each = ttl)
  st_B <- rep(countries, times = ttl)
  res <- data.frame(st_A = st_A, st_B = st_B, year = i)
  dyads <- rbind(dyads, res)
}

dyads <- dyads %>% filter(st_A != st_B)

# Adding conflict levels in dyads
# TODO: Rewrite with left-join
dyads$MID <- 0
dyads$fatlev <- 0
for(i in 1:nrow(conflicts)) {
  subconfl <- conflicts[i,]
  index <- which(dyads$st_A == subconfl$namea &
                   dyads$st_B == subconfl$nameb &
                   dyads$year == subconfl$year)
  dyads$MID[index] <- 1
  dyads$fatlev[index] <- subconfl$fatlev
}

dists <- read_csv("dists.csv") %>%
  mutate(dura = endDate - startDate) %>%
  group_by(ccode, Name) %>%
  summarize(lat = weighted.mean(lat, dura),
            lng = weighted.mean(lng, dura))

dt_dyads <- as.data.table(dyads)

capitals <- left_join(states, dists, by = "ccode") %>%
  select(c(stateabb, lat, lng)) %>%
  group_by(stateabb) %>%
  summarize(lat = mean(lat),
            lng = mean(lng))

dyads_dist <- left_join(dyads, capitals, by = c("st_A" = "stateabb")) %>%
  rename(lat_A = lat, lng_A = lng)

dyads_dist <- left_join(dyads_dist, capitals, by = c("st_B" = "stateabb")) %>%
  rename(lat_B = lat, lng_B = lng)

geodist <- function(lng1, lat1, lng2, lat2) {
  pt1 <- matrix(c(lng1,lat1), ncol = 2)
  pt2 <- matrix(c(lng2,lat2), ncol = 2)
  return(distGeo(pt1, pt2))
}

dyads_dist$cap_dist <- geodist(dyads_dist$lng_A, dyads_dist$lat_A,
                               dyads_dist$lng_B, dyads_dist$lat_B)

dyads_dist <- dyads_dist %>%
  mutate(cap_dist = geodist(lng_A, lat_A, lng_B, lat_B)) %>%
  mutate(lndist = log(cap_dist)) %>%
  filter(cap_dist != 0)
  # select(-c(lat_A, lng_A, lat_B, lng_B))

dyads_dist_polity <- left_join(dyads_dist, polity,
                               c("st_A" = "scode", "year" = "year")) %>%
  rename(polity_A = polity)

dyads_dist_polity <- left_join(dyads_dist_polity, polity,
                               c("st_B" = "scode", "year" = "year")) %>%
  rename(polity_B = polity)

dyads_dist <- dyads_dist_polity %>%
  mutate(polity_A = na_if(polity_A, -88),
         polity_A = na_if(polity_A, -77),
         polity_A = na_if(polity_A, -66),
         polity_B = na_if(polity_B, -88),
         polity_B = na_if(polity_B, -77),
         polity_B = na_if(polity_B, -66)) %>%
  mutate(polity = pmin(polity_A, polity_B, na.rm = T))

contiguity <- read_csv("contdird.csv") %>%
  select(state1ab, state2ab, year, conttype) %>%
  mutate(conttype = replace(conttype, conttype == 5, 0)) %>%
  mutate(conttype = replace(conttype, conttype >= 1, 1)) %>%
  rename(contiguity = conttype)

dyads_dist <- left_join(dyads_dist, contiguity,
                        c("st_A" = "state1ab",
                          "st_B" = "state2ab",
                          "year" = "year")) %>%
  mutate(contiguity = replace(contiguity, is.na(contiguity), 0))

great_power <- read_csv("majors2016.csv") %>% 
  select(stateabb, styear, endyear) %>%
  mutate(dura = endyear - styear + 1)

gp_tmp <- data.frame()

for(i in 1:nrow(great_power)) {
  subgp <- great_power[i,]
  years <- subgp$dura
  gp_dura <- subgp$styear:subgp$endyear
  subgp <- subgp %>% slice(rep(1:n(), each = years))
  subgp$year <- gp_dura
  gp_tmp <- rbind(gp_tmp, subgp)
}

great_power <- gp_tmp %>%
  select(stateabb, year) %>%
  mutate(great_power = 1)

dyads_gp <- left_join(dyads_dist, great_power,
                      c("st_A" = "stateabb",
                        "year" = "year")) %>%
  mutate(great_power = replace(great_power, is.na(great_power), 0))

alliance <- read_csv("alliance_v4.1_by_directed_yearly.csv") %>%
  select(ccode1, ccode2, year)

alliance <- left_join(alliance, states[, 1:2], by = c("ccode1" = "ccode")) %>%
  rename(st_A = stateabb)
alliance <- left_join(alliance, states[, 1:2], by = c("ccode2" = "ccode")) %>%
  rename(st_B = stateabb) %>%
  select(st_A, st_B, year)

gp_alliance <- left_join(alliance, great_power,
                         by = c("st_A" = "stateabb", "year" = "year")) %>%
  rename(gp_alliance_A = great_power) %>%
  mutate(gp_alliance_A = replace(gp_alliance_A, is.na(gp_alliance_A), 0))
gp_alliance <- left_join(gp_alliance, great_power,
                         by = c("st_B" = "stateabb", "year" = "year")) %>%
  rename(gp_alliance_B = great_power) %>%
  mutate(gp_alliance_B = replace(gp_alliance_B, is.na(gp_alliance_B), 0)) %>%
  mutate(gp_alliance = pmax(gp_alliance_A, gp_alliance_B)) %>%
  select(-c(gp_alliance_A, gp_alliance_B))

dyads_dy_alliance <- left_join(dyads_gp, gp_alliance,
                               by = c("st_A" = "st_A",
                                      "st_B" = "st_B",
                                      "year" = "year")) %>%
  mutate(gp_alliance = replace(gp_alliance, is.na(gp_alliance), 0))

alliance$alliance <- 1

dyads_dy_alliance <- left_join(dyads_dy_alliance, alliance,
                               by = c("st_A" = "st_A",
                                      "st_B" = "st_B",
                                      "year" = "year"))
dyads_dy_alliance <-dyads_dy_alliance %>%
  mutate(alliance = replace(alliance, is.na(alliance), 0))

# write_csv(dyads_dy_alliance, "dyads_dy_alliance.csv")
# dyads_dy_alliance <- read_csv("dyads_dy_alliance.csv")

CINC <- read_csv("NMC-60-abridged.csv") %>%
  select(stateabb, year, cinc)

dyads_dy_alliance <- left_join(dyads_dy_alliance, CINC,
                               by = c("st_A" = "stateabb",
                                      "year" = "year")) %>% 
  rename(cinc_A = cinc)

dyads_dy_alliance <- left_join(dyads_dy_alliance, CINC,
                               by = c("st_B" = "stateabb",
                                      "year" = "year")) %>% 
  rename(cinc_B = cinc)

dyads_dy_alliance <- dyads_dy_alliance %>%
  mutate(capratio = log(pmax(cinc_A, cinc_B) / pmin(cinc_A, cinc_B)))

polity_ww1 <- polity %>% filter(year >= 1918 & year <= 1922)

# Start modeling: baseline models

mdl1 <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power +
              alliance + capratio,
            data = dyads_dy_alliance, family = "binomial")
dyads_preww1 <- dyads_dy_alliance %>% filter(year <= 1918)
mdl2 <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power +
              alliance + capratio,
            data = dyads_preww1, family = "binomial")
dyads_interwar <- dyads_dy_alliance %>% filter(year > 1918 & year <= 1945)
mdl3 <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power +
              alliance + capratio,
            data = dyads_interwar, family = "binomial")
dyads_coldwar <- dyads_dy_alliance %>% filter(year > 1945 & year <= 1991)
mdl4 <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power +
              alliance + capratio,
            data = dyads_coldwar, family = "binomial")
dyads_end <- dyads_dy_alliance %>% filter(year > 1991)
mdl5 <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power +
              alliance + capratio,
            data = dyads_end, family = "binomial")
stargazer(mdl1, mdl2, mdl3, mdl4, mdl5,
          type = "text")

stargazer(mdl1, mdl2, mdl3, mdl4, mdl5,
          type = "text", out = "./output/dis_MID.tex", no.space = T,
          font.size = "tiny",
          order = c("polity", "great_power", "alliance"),
          covariate.labels = c("Democracy", "Great Power",
                               "Dyadic Alliance", "$ln(Intercapital Distance)$",
                               "Contiguity", "Capacity Ratio", "Intercept"),
          dep.var.caption = "Outcome Variable",
          column.labels = c("All Years", "Pre-WW1",
                            "Interwar and WW2", "Cold War", "Post-1992"),
          title = "Disaggregated MIDs 1816 -- 2016")

mdl1b <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power +
             gp_alliance + capratio,
            data = dyads_dy_alliance, family = "binomial")
stargazer(mdl1, mdl1b,
          type = "text")

# Modeling great power supported democracies and democratic peace

# Democratized countries after WW1
polity_gp_interwar <- polity %>% filter(year == 1917 | year == 1922) %>%
  group_by(scode) %>%
  summarize(pre = polity[1],
            post = polity[2])

polity_gp_interwar$democratized[which(is.na(polity_gp_interwar$post))] <-
  if_else(polity_gp_interwar$pre[which(is.na(polity_gp_interwar$post))] >= 6,
          1, 0)
polity_gp_interwar$democratized[which(!is.na(polity_gp_interwar$post))] <-
  if_else(polity_gp_interwar$post[which(!is.na(polity_gp_interwar$post))] >= 6 &
            polity_gp_interwar$pre[which(!is.na(polity_gp_interwar$post))] < 6,
          1, 0)
polity_gp_interwar <- polity_gp_interwar %>% group_by(scode) %>%
  summarize(democratized = max(democratized))

dyads_interwar <- left_join(dyads_interwar, polity_gp_interwar,
                           by = c("st_A" = "scode"))
dyads_interwar <- left_join(dyads_interwar, polity_gp_interwar,
                           by = c("st_B" = "scode"))

dyads_interwar <- dyads_interwar %>%
  mutate(gpdem = pmax(democratized.x, democratized.y)) %>%
  mutate(gpdem = replace(gpdem, is.na(gpdem), 0)) %>%
  mutate(both_dem = if_else(polity_A >= 6 & polity_B >= 6, 1, 0)) %>%
  mutate(both_dem = replace(both_dem, is.na(both_dem), 0)) %>%
  mutate(gpdem = gpdem * both_dem) %>%
  mutate(gpdem = gpdem * gp_alliance)

mdl3b <- glm(MID ~ log(cap_dist) + polity * gpdem + contiguity + great_power +
               alliance + capratio,
             data = dyads_interwar, family = "binomial")
mdl3c <- glm(MID ~ log(cap_dist) + polity * gpdem + contiguity +
               capratio,
             data = dyads_interwar, family = "binomial")
stargazer(mdl3, mdl3b, mdl3c, type = "text")

# Democratized countries after WW2
polity_gp_coldwar <- polity %>% filter(year == 1942 | year == 1947) %>%
  group_by(scode) %>%
  summarize(pre = polity[1],
            post = polity[2])

polity_gp_coldwar$democratized[which(is.na(polity_gp_coldwar$post))] <-
  if_else(polity_gp_coldwar$pre[which(is.na(polity_gp_coldwar$post))] >= 6,
          1, 0)
polity_gp_coldwar$democratized[which(!is.na(polity_gp_coldwar$post))] <-
  if_else(polity_gp_coldwar$post[which(!is.na(polity_gp_coldwar$post))] >= 6 &
            polity_gp_coldwar$pre[which(!is.na(polity_gp_coldwar$post))] < 6,
          1, 0)
polity_gp_coldwar <- polity_gp_coldwar %>% group_by(scode) %>%
  summarize(democratized = max(democratized))

dyads_coldwar <- left_join(dyads_coldwar, polity_gp_coldwar,
                            by = c("st_A" = "scode"))
dyads_coldwar <- left_join(dyads_coldwar, polity_gp_coldwar,
                            by = c("st_B" = "scode"))

dyads_coldwar <- dyads_coldwar %>%
  mutate(gpdem = pmax(democratized.x, democratized.y)) %>%
  mutate(gpdem = replace(gpdem, is.na(gpdem), 0)) %>%
  mutate(both_dem = if_else(polity_A >= 6 & polity_B >= 6, 1, 0)) %>%
  mutate(both_dem = replace(both_dem, is.na(both_dem), 0)) %>%
  mutate(gpdem = gpdem * both_dem) %>%
  mutate(gpdem = gpdem * gp_alliance)
mdl4b <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power +
               alliance + capratio + gpdem,
             data = dyads_coldwar, family = "binomial")
mdl4c <- glm(MID ~ log(cap_dist) + polity * gpdem,
             data = dyads_coldwar, family = "binomial")
stargazer(mdl4, mdl4b, mdl4c, type = "text")

# Democratized countries after the Cold War
polity_gp_post1992 <- polity %>% filter(year == 1988 | year == 1993) %>%
  group_by(scode) %>%
  summarize(pre = polity[1],
            post = polity[2])

polity_gp_post1992$democratized[which(is.na(polity_gp_post1992$post))] <-
  if_else(polity_gp_post1992$pre[which(is.na(polity_gp_post1992$post))] >= 6,
          1, 0)
polity_gp_post1992$democratized[which(!is.na(polity_gp_post1992$post))] <-
  if_else(polity_gp_post1992$post[which(!is.na(polity_gp_post1992$post))] >= 6 &
            polity_gp_post1992$pre[which(!is.na(polity_gp_post1992$post))] < 6,
          1, 0)
polity_gp_post1992 <- polity_gp_post1992 %>% group_by(scode) %>%
  summarize(democratized = max(democratized))

dyads_end <- left_join(dyads_end, polity_gp_post1992,
                           by = c("st_A" = "scode"))
dyads_end <- left_join(dyads_end, polity_gp_post1992,
                           by = c("st_B" = "scode"))

dyads_end <- dyads_end %>%
  mutate(gpdem = pmax(democratized.x, democratized.y)) %>%
  mutate(gpdem = replace(gpdem, is.na(gpdem), 0)) %>%
  mutate(both_dem = if_else(polity_A >= 6 & polity_B >= 6, 1, 0)) %>%
  mutate(both_dem = replace(both_dem, is.na(both_dem), 0)) %>%
  mutate(gpdem = gpdem * both_dem) %>%
  mutate(gpdem = gpdem * gp_alliance)
mdl5b <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power +
               alliance + capratio + gpdem,
             data = dyads_end, family = "binomial")
mdl5c <- glm(MID ~ log(cap_dist) + polity * gpdem,
             data = dyads_end, family = "binomial")
stargazer(mdl5, mdl5b, mdl5c, type = "text")
# Visualizing MIDs on the world map

conflicts_coord <- dyads_dy_alliance %>% filter(MID == 1) %>%
  select(year, lat_A, lat_B, lng_A, lng_B, polity)

jitter_sd <- 0.5
conflicts_coord$lat_A <- conflicts_coord$lat_A +
  rnorm(nrow(conflicts_coord), 0, jitter_sd)
conflicts_coord$lng_A <- conflicts_coord$lng_A +
  rnorm(nrow(conflicts_coord), 0, jitter_sd)
conflicts_coord$lat_B <- conflicts_coord$lat_B +
  rnorm(nrow(conflicts_coord), 0, jitter_sd)
conflicts_coord$lng_B <- conflicts_coord$lng_B +
  rnorm(nrow(conflicts_coord), 0, jitter_sd)

pair_A <- conflicts_coord %>% select(year, lat_A, lng_A, polity)
pair_A <- pair_A %>% mutate(pair = 1:nrow(pair_A)) %>%
  rename(lat = lat_A, lng = lng_A)
pair_B <- conflicts_coord %>% select(year, lat_B, lng_B, polity)
pair_B <- pair_B %>% mutate(pair = 1:nrow(pair_B)) %>%
  rename(lat = lat_B, lng = lng_B)

pairs <- rbind(pair_A, pair_B)

pairs_preww1 <- pairs %>% filter(year <= 1918) %>%
  mutate(cat = "Pre-WW1")
pairs_interwar <- pairs %>% filter(year > 1918 & year <= 1945) %>%
  mutate(cat = "Interwar")
pairs_coldwar <- pairs %>% filter(year > 1945 & year <= 1991) %>%
  mutate(cat = "Cold War")
pairs_post1992 <- pairs %>% filter(year >= 1992) %>%
  mutate(cat = "Post-1992")
pairs <- rbind(pairs_preww1, pairs_interwar, pairs_coldwar, pairs_post1992)


world <- ne_countries(scale = "medium", returnclass = "sf")

plot_all <- ggplot(data = world) +
  geom_sf(size = 0.2) +
  geom_line(data = pairs, aes(x = lng, y = lat, group = pair, color = cat),
            size = .1) +
  theme_bw() +
  ggtitle("Global Militarized Interstate Disputes",
          subtitle = "1816 - 2016") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = F) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white")) +
  scale_colour_viridis_d(limits = c("Pre-WW1", "Interwar",
                                    "Cold War", "Post-1992"), direction = -1) +
  guides(color = guide_legend(override.aes = list(size = 1), 
                              title = "Time Scope")) +
  xlab("Longitude") +
  ylab("Latitude")

# Pre WWI
plot_preww1 <- ggplot(data = world) +
  geom_sf(size = 0.2) +
  geom_line(data = pairs_preww1, aes(x = lng, y = lat,
                                     group = pair, color = polity),
            size = .1) +
  theme_bw() +
  ggtitle("Global Militarized Interstate Disputes",
          subtitle = "1816 - 1918") +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") + 
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = F) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white")) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "blue") +
  guides(color = guide_legend(override.aes = list(size = 1), 
                              title = "Polity Score")) +
  xlab("Longitude") +
  ylab("Latitude")

# Interwar and WWII
plot_interwar <- ggplot(data = world) +
  geom_sf(size = 0.2) +
  geom_line(data = pairs_interwar, aes(x = lng, y = lat,
                                     group = pair, color = polity),
            size = .1) +
  theme_bw() +
  ggtitle("Global Militarized Interstate Disputes",
          subtitle = "Interwar and WWII") +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") + 
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = F) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white")) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "blue") +
  guides(color = guide_legend(override.aes = list(size = 1), 
                              title = "Polity Score")) +
  xlab("Longitude") +
  ylab("Latitude")

# Cold War
plot_coldwar <- ggplot(data = world) +
  geom_sf(size = 0.2) +
  geom_line(data = pairs_coldwar, aes(x = lng, y = lat,
                                       group = pair, color = polity),
            size = .1) +
  theme_bw() +
  ggtitle("Global Militarized Interstate Disputes",
          subtitle = "Cold War") +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") + 
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = F) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white")) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "blue") +
  guides(color = guide_legend(override.aes = list(size = 1), 
                              title = "Polity Score")) +
  xlab("Longitude") +
  ylab("Latitude")

# Post 1992
plot_post1992 <- ggplot(data = world) +
  geom_sf(size = 0.2) +
  geom_line(data = pairs_post1992, aes(x = lng, y = lat,
                                      group = pair, color = polity),
            size = .1) +
  theme_bw() +
  ggtitle("Global Militarized Interstate Disputes",
          subtitle = "Post-1992") +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") + 
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = F) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white")) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "blue") +
  guides(color = guide_legend(override.aes = list(size = 1), 
                              title = "Polity Score")) +
  xlab("Longitude") +
  ylab("Latitude")

ggsave("./output/plots/all.pdf", plot = plot_all,
       width = 10, height = 5)
ggsave("./output/plots/preww1.pdf", plot = plot_preww1,
       width = 10, height = 5)
ggsave("./output/plots/interwar.pdf", plot = plot_interwar,
       width = 10, height = 5)
ggsave("./output/plots/coldwar.pdf", plot = plot_coldwar,
       width = 10, height = 5)
ggsave("./output/plots/post1992.pdf", plot = plot_post1992,
       width = 10, height = 5)


