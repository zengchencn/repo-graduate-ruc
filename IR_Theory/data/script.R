library(tidyverse)
library(haven)
library(stargazer)
library(data.table)
library(dtplyr)
library(geosphere)
library(readxl)

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
  filter(cap_dist != 0) %>%
  select(-c(lat_A, lng_A, lat_B, lng_B))

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
         polity_B = na_if(polity_A, -88),
         polity_B = na_if(polity_A, -77),
         polity_B = na_if(polity_A, -66)) %>%
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
  select(st_A, st_B, year) %>%
  mutate(dyadic_alliance = 1)

dyads_dy_alliance <- left_join(dyads_gp, alliance, by = c(st_A, st_B, year))

mdl1 <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power,
            data = dyads_gp, family = "binomial")
dyads_preww1 <- dyads_gp %>% filter(year <= 1918)
mdl2 <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power,
            data = dyads_preww1, family = "binomial")
dyads_interwar <- dyads_gp %>% filter(year > 1918 & year <= 1945)
mdl3 <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power,
            data = dyads_interwar, family = "binomial")
dyads_coldwar <- dyads_gp %>% filter(year > 1945 & year <= 1991)
mdl4 <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power,
            data = dyads_coldwar, family = "binomial")
dyads_end <- dyads_gp %>% filter(year > 1991)
mdl5 <- glm(MID ~ log(cap_dist) + polity + contiguity + great_power,
            data = dyads_end, family = "binomial")
stargazer(mdl1, mdl2, mdl3, mdl4, mdl5,
          type = "text")
summary(mdl4)


