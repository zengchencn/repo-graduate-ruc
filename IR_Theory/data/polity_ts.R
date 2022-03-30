library(tidyverse)
library(ggthemes)

setwd("~/Workspace/repo-graduate-ruc/IR_Theory/data")

polity <- read_csv("polity_by_year.csv")

polity$cat <- NA

polity <- polity %>%
  mutate(cat = if_else(polity >= 6,
                       "Democracy",
                       if_else(polity >= -10 & polity <= -6,
                               "Autocracy",
                               "Anocracy")))

polity_year <- polity %>% count(year, cat)

total <- polity_year %>% group_by(year) %>%
  summarize(total = sum(n))

polity_year <- left_join(polity_year, total, by = "year") %>%
  mutate(prp = n / total)


cnt <- ggplot(polity_year, aes(x = year, y = n, color = cat)) +
  geom_line() +
  geom_vline(xintercept = c(1918, 1945, 1991),
             size = .25) +
  theme_bw() +
  labs(x = "Year", y = "Number of Countries") +
  guides(color = guide_legend(title = "Regime Type")) +
  scale_color_manual(breaks=c("Democracy", "Anocracy", "Autocracy"),
                     values = c("#2E9FDF", "#E7B800", "#FC4E07"))

pption <- ggplot(polity_year, aes(x = year, y = prp, color = cat)) +
  geom_line() +
  geom_vline(xintercept = c(1918, 1945, 1991),
             size = .25) +
  theme_bw() +
  labs(x = "Year", y = "Proportion") +
  guides(color = guide_legend(title = "Regime Type")) +
  scale_color_manual(breaks=c("Democracy", "Anocracy", "Autocracy"),
                     values = c("#2E9FDF", "#E7B800", "#FC4E07"))

ggsave("./output/plots/polity_cnt.pdf", plot = cnt,
       width = 9, height = 5)
ggsave("./output/plots/polity_prp.pdf", plot = pption,
       width = 9, height = 5)
