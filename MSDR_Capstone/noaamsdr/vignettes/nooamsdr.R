## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(noaamsdr)

eq_get_data() %>% glimpse

## ------------------------------------------------------------------------
eq_get_data() %>% eq_clean_data %>% head

## ------------------------------------------------------------------------
df <- eq_get_data() %>%
  eq_clean_data %>%
  filter(!is.na(EQ_PRIMARY)) %>%
  filter(year(DATE) > 2000) %>%
  filter(COUNTRY %in% c("CHINA", "USA"))

ggplot(df) +
  aes(
    x = DATE,
    y = COUNTRY,
    size = EQ_PRIMARY,
    colour = DEATHS,
    label = LOCATION_NAME,
    by = EQ_PRIMARY
  ) +
  geom_timeline() +
  geom_timeline_label(n_max = 5) +
  theme_timeline

## ---- eval=F-------------------------------------------------------------
#  eq_get_data() %>%
#    eq_clean_data %>%
#    filter(COUNTRY == "MEXICO", year(DATE) >= 2000) %>%
#    eq_map(annot_col = "DATE")
#  
#  eq_get_data() %>%
#    eq_clean_data %>%
#    filter(COUNTRY == "MEXICO", year(DATE) >= 2000) %>%
#    mutate(popup_text = eq_create_label(.)) %>%
#    eq_map(annot_col = "popup_text")

