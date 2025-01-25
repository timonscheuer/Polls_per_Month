library(tidyverse)
library(tidylog, warn.conflicts = F)
library(ggtext)
library(foreign)
library(dawumr)
library(here)
#library(extrafont)

dawum_data <- pull_dawum_dataframe()


# Tabelle -----------------------------------------------------------------

Institute <- c(
  "Allensbach", "Verian (Emnid)", "Forsa",
  "Forschungsgruppe Wahlen", "GMS", "Infratest dimap",
  "INSA"
)

start_date_dawum <- dmy("01.02.2017")
end_date_dawum <- dmy("31.01.2024")

# Häufigkeiten für die gewünschten Kriterien berechnen
tabelle <- dawum_data %>%
  mutate(
    Date = as.Date(Date),
    Month_Year = format(Date, "%Y-%m"),
    Month_Year = as.Date(paste0(Month_Year, "-01"))
  ) %>%
  filter(Election == "Bundestagswahl" &
    Parliament_Shortcut == "Bundestag" &
    Institute_Name %in% Institute,
    Date >= start_date_dawum,
    Date <= end_date_dawum) %>%
  group_by(Month_Year) %>%
  summarise(Count = n_distinct(ID)) %>% 
  ungroup()

# Month_Year in ein Datum umwandeln, um es für Zeitreihenanalysen zu verwenden



# Scrape wahlrecht.de -----------------------------------------------------

source(here("scrape-polls.R"))

table(df_wahlrecht$Institut)

df_wahlrecht_renamed <- df_wahlrecht %>%
  rename(
    Institute_Name = Institut,
    Date = Datum,
    Surveyed_Persons = Befragte,
    Party_Shortcut = Partei,
    Share = Pct
  ) %>%
  mutate(Institute_Name = fct_recode(Institute_Name,
    "Allensbach" = "allensbach",
    "Infratest dimap" = "dimap",
    "Verian (Emnid)" = "emnid",
    "Forsa" = "forsa",
    "GMS" = "gms",
    "INSA" = "insa",
    "Forschungsgruppe Wahlen" = "politbarometer",
    "YouGov" = "yougov"
  )) %>%
  glimpse()

table(df_wahlrecht_renamed$Institute_Name)

start_date_wahlrecht <- dmy("01.01.2013")
end_date_wahlrecht <- dmy("31.01.2017")


df_wahlrecht_filtered <- df_wahlrecht_renamed %>%
  mutate(
    Month_Year = format(Date, "%Y-%m"),
    Month_Year = as.Date(paste0(Month_Year, "-01")),
    Combined_ID = paste(Institute_Name, format(Date, "%Y-%m-%d"), sep = "_")
  ) %>%
  filter(
    Institute_Name %in% Institute,
    Date >= start_date_wahlrecht,
    Date <= end_date_wahlrecht
  ) %>%
  group_by(Month_Year) %>%
  summarise(Count = n_distinct(Combined_ID)) %>%  
  ungroup()

df_final <- rbind(df_wahlrecht_filtered, tabelle)

# Plot Häufigkeit ---------------------------------------------------------

btw13_colour <- "#4C9A2A"
btw17_colour <- "#324CA8"
btw21_colour <- "#9E0B0F"
background_colour <- "#EAE4D3"
grid_colour <- "#D0C6B0"
caption_colour <- "#979692"
linewidth <- 0.8
font <- "Futura"

#
plot_1 <- 
  ggplot(df_final, aes(x = Month_Year, y = Count)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2013-09-22"), linetype = "dashed", color = btw13_colour, linewidth = linewidth) +
  geom_vline(xintercept = as.Date("2017-09-24"), linetype = "dashed", color = btw17_colour, linewidth = linewidth) +
  geom_vline(xintercept = as.Date("2021-09-26"), linetype = "dashed", color = btw21_colour, linewidth = linewidth) +
  scale_x_date(
    date_breaks = "4 months",
    date_labels = "%m.%Y",
    expand = expansion(mult = c(0, .02))
  ) +
   scale_y_continuous(
     breaks = seq(14, 26, by = 2),
     limits = c(14, 26)
    ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.title = element_blank(),
    legend.position = "",
    panel.grid.minor.x = element_blank(),
    panel.grid = element_line(colour = grid_colour),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = background_colour, colour = background_colour),
    panel.background = element_rect(fill = background_colour, colour = background_colour),
    plot.title = element_markdown(face = "bold", hjust = .5, size = 16),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 12, b = 8),
      padding = margin(2, 2, 2, 2),
      lineheight = 1.5,
      size = 12,
      halign = 0.5
    ),
    plot.title.position = "plot",
    plot.caption = element_textbox_simple(
      margin = margin(t = 12, b = 8),
      padding = margin(2, 2, 2, 2),
      lineheight = 1.5,
      halign = 0.5
    ),
    text = element_text(font)
  ) +
  labs(
    x = "",
    y = "Umfragen pro Monat",
    title = "Umfragen pro Monat seit 2013",
    caption = glue::glue("Visualisierung: Timon Scheuer<br/><span style='color: {caption_colour};'>Quellen: 01.01.2013 - 31.01.2017: wahlrecht.de, ab 01.02.2017: dawum.de (Lizenz: ODC-ODbL).<br/>Alle genannten Institute werden von beiden Quellen erfasst.</span>"),
    subtitle = glue::glue("Die Anzahl der monatlichen Umfragen ist seit der <span style='color:{btw13_colour}; font-weight:bold;'>Bundestagswahl 2013</span> tendenziell angestiegen. Nach der <span style='color:{btw21_colour}; font-weight:bold;'>Bundestagswahl 2021</span> wurden zwar wieder weniger Umfragen durchgeführt, die Anzahl der Umfragen ist jedoch nicht so stark zurückgegangen wie nach der <span style='color:{btw17_colour}; font-weight:bold;'>Bundestagswahl 2017</span>.<br/>Einbezogen wurden Umfragen zu Bundestagswahlen von Allensbach, Verian (Emnid), Forsa, der Forschungsgruppe Wahlen, GMS, Infratest dimap und INSA.")
  )



ggsave(
  plot = plot_1,
  filename = "Plot_Häufigkeit_Twitter.png",
  width = 9, # Breite in Zoll
  height = 8, # Höhe in Zoll
  dpi = 300
  )

# Facets nach Instituten --------------------------------------------------



tabelle2 <- dawum_data %>%
  mutate(
    Date = as.Date(Date),
    Month_Year = format(Date, "%Y-%m"),
    Month_Year = as.Date(paste0(Month_Year, "-01"))
  ) %>%
  filter(Election == "Bundestagswahl" &
           Parliament_Shortcut == "Bundestag" &
           Institute_Name %in% Institute,
         Date >= start_date_dawum,
         Date <= end_date_dawum) %>%
  group_by(Month_Year, Institute_Name) %>%
  summarise(Count = n_distinct(ID)) %>% 
  ungroup()

df_wahlrecht_filtered2 <- df_wahlrecht_renamed %>%
  mutate(
    Month_Year = format(Date, "%Y-%m"),
    Month_Year = as.Date(paste0(Month_Year, "-01")),
    Combined_ID = paste(Institute_Name, format(Date, "%Y-%m-%d"), sep = "_")
  ) %>%
  filter(
    Institute_Name %in% Institute,
    Date >= start_date_wahlrecht,
    Date <= end_date_wahlrecht
  ) %>%
  group_by(Month_Year, Institute_Name) %>%
  summarise(Count = n_distinct(Combined_ID)) %>%  
  ungroup()


df_final2 <- rbind(df_wahlrecht_filtered2, tabelle2)


#
plot_2 <-
  ggplot(df_final2, aes(x = Month_Year, y = Count)) +
  geom_line() +
  facet_wrap(~ Institute_Name, ncol = 2) +
  # geom_vline(xintercept = as.Date("2002-09-22"), linetype = "dashed", color = btw17_colour) +
  # geom_vline(xintercept = as.Date("2005-09-18"), linetype = "dashed", color = btw17_colour) +
  # geom_vline(xintercept = as.Date("2009-09-27"), linetype = "dashed", color = btw17_colour) +
  geom_vline(xintercept = as.Date("2013-09-22"), linetype = "dashed", color = btw13_colour, linewidth = linewidth) +
  geom_vline(xintercept = as.Date("2017-09-24"), linetype = "dashed", color = btw17_colour, linewidth = linewidth) +
  geom_vline(xintercept = as.Date("2021-09-26"), linetype = "dashed", color = btw21_colour, linewidth = linewidth) +
  scale_x_date(
    date_breaks = "8 months",
    date_labels = "%m.%Y",
    expand = expansion(mult = c(0, .02))
  ) +
   scale_y_continuous(
     limits = c(0, 10),
     breaks = seq(0, 10, by = 2)
   ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.title = element_blank(),
    legend.position = "",
    panel.grid.minor.x = element_blank(),
    panel.grid = element_line(colour = grid_colour),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = background_colour, colour = background_colour),
    panel.background = element_rect(fill = background_colour, colour = background_colour),
    plot.title = element_markdown(face = "bold", hjust = .5, size = 16),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 12, b = 8),
      padding = margin(2, 2, 2, 2),
      lineheight = 1.5,
      size = 12,
      halign = 0.5
    ),
    plot.title.position = "plot",
    plot.caption = element_textbox_simple(
      margin = margin(t = 12, b = 8),
      padding = margin(2, 2, 2, 2),
      lineheight = 1.5,
      halign = 0.5
    ),
    text = element_text("Futura")
  ) +
  labs(
    x = "",
    y = "Umfragen pro Monat",
    title = "Umfragen pro Monat seit 2013 nach Instituten",
    caption = glue::glue("Visualisierung: Timon Scheuer<br/><span style='color: {caption_colour};'>Quellen: 01.01.2013 - 31.01.2017: wahlrecht.de, ab 01.02.2017: dawum.de (Lizenz: ODC-ODbL).<br/>Alle genannten Institute werden von beiden Quellen erfasst.</span>"),
    subtitle = glue::glue("Die Anzahl der monatlichen Umfragen ist seit der <span style='color:{btw13_colour}; font-weight:bold;'>Bundestagswahl 2013</span> tendenziell angestiegen. Nach der <span style='color:{btw21_colour}; font-weight:bold;'>Bundestagswahl 2021</span> wurden zwar wieder weniger Umfragen durchgeführt, die Anzahl der Umfragen ist jedoch nicht so stark zurückgegangen wie nach der <span style='color:{btw17_colour}; font-weight:bold;'>Bundestagswahl 2017</span>.<br/>Insbesondere INSA veröffentlicht seit 2021 deutlich mehr Umfragen als bisher.")
  )

  ggsave(
    plot = plot_2,
    filename = "Plot_Häufigkeit_Twitter_Institute.png",
    width = 9, # Breite in Zoll
    height = 8, # Höhe in Zoll
    dpi = 300
  )
  
  test <- dawum_data %>%
    mutate(
      Date = as.Date(Date),
      Month_Year = format(Date, "%Y-%m"),
      Month_Year = as.Date(paste0(Month_Year, "-01"))
    ) %>%
    filter(Election == "Bundestagswahl" &
             Parliament_Shortcut == "Bundestag" &
             Institute_Name == "Allensbach",
           Date >= start_date_dawum,
           Date <= end_date_dawum) %>%
    group_by(Month_Year, Institute_Name, Tasker_Name) %>%
    summarise(Count = n_distinct(ID)) %>% 
    ungroup()
  
  table(test$Tasker_Name)