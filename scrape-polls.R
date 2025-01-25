##' Source: https://github.com/ZeitOnline/
##' https://github.com/ZeitOnline/wahltrend/blob/main/get_polls.R
##' Adapted and added query for earlier polls (pre 2017)


library(tidyverse)
library(rvest)
library(xml2)

base_url <- "http://wahlrecht.de/umfragen/"

df.wahlrecht <-
  tibble(
    'ID' = integer(),
    'Institut' = character(),
    'Befragte'= character(),
    'Datum' = character(),
    'Partei' = character(),
    'Pct' = character()
  )

pollsterPages <- 
  ("http://www.wahlrecht.de/umfragen/index.htm" %>% 
     read_html() %>% 
     html_node('table.wilko') %>% 
     html_node("thead") %>% 
     html_nodes("a") %>% 
     map(xml_attrs) %>% 
     map_df(~as.list(.))
  )$href


get_pollster_subpage_urls <- function(url) {
  read_html(url) %>% 
    html_nodes(css = "p.navi a") %>% 
    html_attr("href")
}

pollsterSubpages <- map(pollsterPages, ~get_pollster_subpage_urls(paste0(base_url, .x)))
pollsterSubpages <- reduce(pollsterSubpages, c)

pollsterPages <- c(pollsterPages, pollsterSubpages)


for (i in c(1:length(pollsterPages))){
  institute <- pollsterPages[i]
  url <- paste0(base_url,institute)
  
  table <-
    tibble(
      'ID' = integer(),
      'Datum' = character(),
      'Befragte' = character(),
      'Zeitraum' = character(),
      'Partei' = character(),
      'Pct' = character()
    )
  
  url %>% 
    read_html() %>% 
    html_node("table.wilko") %>% 
    html_table(fill = T) %>% 
    as_tibble(.name_repair = "unique") ->
    t
  
  t[[1,1]] <- "Datum"
  t[,2] <- as.character(t[,2])
  t[[1,2]] <- "X1"
  names(t) <- as.character(as.vector(t[1,]))
  for (k in c(1:length(names(t)))){if(is.na(names(t)[k])){names(t)[k] <- paste0("NULL",k)}}
  for (k in c(1:length(names(t)))){if(names(t)[k] == "NA"){names(t)[k] <- paste0("NULL",k)}}
  for (k in c(1:length(names(t)))){if(names(t)[k] == ""){names(t)[k] <- paste0("NULL",k)}}
  if(!("Befragte" %in% names(t))){t %>% mutate(Befragte = NA) -> t}
  if(!("Zeitraum" %in% names(t))){t %>% mutate(Zeitraum = NA) -> t}
  table <-
    t %>% 
    select(-X1) %>% 
    unique() %>% 
    mutate(ID = nrow(.) + 1 - row_number()) %>% 
    select(ID, Datum, Befragte, Zeitraum,everything()) %>% 
    gather(key = "Partei", value = "Pct", 5:ncol(.)) %>% 
    mutate(
      Institut = institute,
      Land = "Bundestag"
    ) %>% 
    select(ID, Institut,Datum,Befragte,Partei,Pct)
  
  df.wahlrecht <-
    df.wahlrecht %>% 
    rbind(table)
}

rm(t,table,i,pollsterPages,institute,k,url)

################
### regex to clean data
################
df_wahlrecht <-
  df.wahlrecht %>%
  unique() %>% 
  separate(Partei, into = c("Partei"), sep = "[.]", extra = "drop") %>% 
  mutate(
    Institut = str_remove(Institut,".htm"),
    Datum = dmy(Datum),
    Pct = Pct %>% str_remove(" %") %>% str_replace(",","."),
    Partei = ifelse(Partei %in% c("CDU","CSU","CDU/CSU"),"CDUCSU",Partei),
    Pct = Pct %>% str_remove_all("[*]") %>%
      str_remove("%") %>% str_remove(" ")
  ) %>%
  filter(
    !is.na(Datum),
    !str_detect(Befragte, "wahl"),
    Partei %in% c("CDUCSU","SPD","GRÜNE","FDP","LINKE","AfD","PDS"),
    !str_detect(Pct,"[?]"),
    !str_detect(Pct,"[a-z]"),
    Pct != "–"
  ) %>%
  mutate(Pct = as.numeric(Pct)) %>% 
  mutate(
    Befragte = Befragte %>% 
      str_remove("[A-Z]+") %>% 
      str_remove(" • ") %>% 
      str_remove('[.]') %>% 
      as.integer(),
    Institut = str_extract(Institut, "([^/]+)", group = 1)
  ) %>%
  select(ID, Institut, Datum, Befragte,Partei,Pct)



# Institute mit Zeitraum
df_wahlrecht %>% 
  group_by(Institut) %>% 
  summarize(erste = min(Datum), letzte = max(Datum), anzahl = n()) %>% 
  knitr::kable()
