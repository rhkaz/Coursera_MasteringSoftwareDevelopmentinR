# Part 1: Cleaning
df <- eq_get_data() %>%
  eq_clean_data %>%
  filter(!is.na(EQ_PRIMARY)) %>%
  filter(year(date) > 2000) %>%
  filter(COUNTRY %in% c("CHINA", "USA"))

# Part 2: Vizualization
ggplot(df) +
  aes(
    x = date,
    y = COUNTRY,
    size = EQ_PRIMARY,
    colour = DEATHS,
    label = LOCATION_NAME,
    by = EQ_PRIMARY
  ) +
  geom_timeline() +
  geom_timeline_label(n_max = 5) +
  theme_timeline

# Part 3: Leaflet
eq_get_data() %>%
  eq_clean_data %>%
  filter(COUNTRY == "MEXICO", year(date) >= 2000) %>%
  eq_map(annot_col = "date")

eq_get_data() %>%
  eq_clean_data %>%
  filter(COUNTRY == "MEXICO", year(date) >= 2000) %>%
  mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")
