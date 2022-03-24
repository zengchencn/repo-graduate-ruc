library(tidyverse)
library(haven)

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


