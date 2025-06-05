library(tidyverse)
library(tidycensus)
library(sf)
library(cityforwardcollective)
library(glue)
library(scales)
library(htmltools)

# demo data from John Johnson
# https://github.com/jdjohn215/wisc-election-night-data/blob/main/2024-nov/wec/reporting-unit-demographics-2020census.csv

w_demo <- read_csv("data/reporting-unit-demographics-2020census.csv") |> 
  mutate()

cols <- c(
  "#1F77B4",
  "#FF7F0E"
  # "#2CA02C"
)

all <- read_rds("data/cleaned_supe_race_data.rda") |> 
  clean_names() |> 
  pivot_longer(cols = c(1:3), names_to = "candidate", values_to = "votes") |> 
  group_by(new_lab) |> 
  mutate(perc = votes / sum(votes),
         col = case_when(
           candidate == "brittany_kinser" ~ cols[2],
           candidate == "jill_underly" ~ cols[1],
           TRUE ~ cols[3]
         )) |> 
  st_transform(crs = 4326)

both <- left_join(
  wider_n |> 
    mutate(reporting_unit = str_to_upper(original_ward) |> 
             str_trim()),
  w_demo
)

both |> 
  filter(city == "Milwaukee") |> 
  mutate(perc_black = pop_black / pop,
         perc_white = pop_white / pop,
         ju = jill_underly / (brittany_kinser +
                                jill_underly +
                                write_in.x),
         bk = brittany_kinser / (brittany_kinser +
                                jill_underly +
                                write_in.x),
         bs = brad_schimel / (brad_schimel +
                                susan_crawford+ 
                                write_in.y)) |> 
  ggplot(aes(kinser_metric, perc_black)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(aspect.ratio = 1)

# > 50% black
maj_wards <- both |> 
  filter(city == "Milwaukee") |> 
  mutate(perc_black = pop_black / pop,
         perc_white = pop_white / pop,
         perc_hisp = pop_hisp / pop,
         ju = jill_underly / (brittany_kinser +
                                jill_underly +
                                write_in.x),
         bk = brittany_kinser / (brittany_kinser +
                                   jill_underly +
                                   write_in.x),
         bs = brad_schimel / (brad_schimel +
                                susan_crawford+ 
                                write_in.y)) |> 
  group_by(bw = perc_black >= .5,
           ww = perc_white >= .5,
           hw = perc_hisp >= .5,
           none = perc_black < .5 &
             perc_white < .5 &
             perc_hisp < .5) |> 
  summarise(kinser = sum(brittany_kinser),
            underly = sum(jill_underly),
            write_in.x = sum(write_in.x),
            crawford = sum(susan_crawford),
            schimel = sum(brad_schimel),
            write_in.y = sum(write_in.y),
            total = (schimel + crawford + write_in.y),
            kinser_m = kinser / (underly + kinser + write_in.x),
            schimel_m = schimel / (schimel + crawford + write_in.y),
            diff = kinser_m - schimel_m,
            kinser_over = kinser - schimel,
            kinser_metric = kinser_over / (crawford +
                                     schimel +
                                     write_in.y)) 

maj_wards |> 
  as_tibble() |> 
  select(1:4, diff, total) |> 
  pivot_longer(cols = 1:4) |> 
  filter(!is.na(value) & value) |> 
  mutate(name = case_when(
    name == "bw" ~ "Black",
    name == "hw" ~ "Hispanic",
    name == "ww" ~ "White",
    name == "none" ~ "None"
  ),
  ward_col = case_when(
    name == "Black" ~ cfc_darkblue,
    name == "White" ~ cfc_skyblue,
    name == "Hispanic" ~ cfc_orange,
    name == "None" ~ cfc_teal
  )) |> 
  ggplot(aes(name, diff)) +
  geom_col(aes(fill = ward_col)) +
  geom_text(aes(label = glue(
    "{label_percent(.1)(diff)}"
  )), vjust = -1, size = 5, fontface = "bold") +
  geom_text(aes(y = 0, label = glue(
    "{label_comma()(total)}\ntotal votes"
  ), color = ifelse(name %in% c("Black", "Hispanic"), "white", cfc_darkblue)), 
  vjust = -.5, size = 5,
  fontface = "bold") +
  coord_cartesian(clip = "off") +
  scale_fill_identity() + 
  scale_color_identity() +
  scale_y_continuous(labels = label_percent()) +
  labs(y = "Percent of All Votes",
       x = "Majority Race/Ethnicity in Ward") +
  theme(axis.title.x = element_text(margin = margin(t = 10)))

ggsave("plots/crawford_kinser_split_vote.png", bg = "transparent",
       width = 9, h = 6)

maj_wards |> 
  as_tibble() |> 
  select(1:4, diff, total) |> 
  pivot_longer(cols = 1:4) |> 
  filter(!is.na(value) & value) |> 
  mutate(name = case_when(
    name == "bw" ~ "Black",
    name == "hw" ~ "Hispanic",
    name == "ww" ~ "White",
    name == "none" ~ "None"
  ),
  ward_col = case_when(
    name == "Black" ~ cfc_darkblue,
    name == "White" ~ cfc_skyblue,
    name == "Hispanic" ~ cfc_orange,
    name == "None" ~ cfc_teal
  )) |> 
  ggplot(aes(diff, total)) +
  geom_point() 




race_wards <- both |> 
  filter(city == "Milwaukee") |> 
  mutate(perc_black = pop_black / pop,
         perc_white = pop_white / pop,
         perc_hisp = pop_hisp / pop,
         ju = jill_underly / (brittany_kinser +
                                jill_underly +
                                write_in.x),
         bk = brittany_kinser / (brittany_kinser +
                                   jill_underly +
                                   write_in.x),
         bs = brad_schimel / (brad_schimel +
                                susan_crawford+ 
                                write_in.y),
         maj_eth = case_when(
           perc_black >= .5 ~ "Black",
           perc_white >= .5 ~ "White",
           perc_hisp >= .5 ~ "Hispanic",
           TRUE ~ "None"
         ),
         ward_col = case_when(
           maj_eth == "Black" ~ cfc_darkblue,
           maj_eth == "White" ~ cfc_skyblue,
           maj_eth == "Hispanic" ~ cfc_orange,
           maj_eth == "None" ~ cfc_teal
         ))


leaflet(race_wards) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 1.5,
              fillColor = race_wards$ward_col, 
              fillOpacity = .9, 
              color = "white",
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE)) |> 
  addLegend(colors = c(cfc_darkblue, cfc_orange, cfc_skyblue, cfc_teal), 
            opacity = 1, 
            labels = c("Black", "Hispanic", "White", "None"), 
            position = "topright") |> 
  addControl(
    tags$style(HTML("
      .info.legend.leaflet-control {
        font-size: 24pt;
        line-height: 28pt;
      }
      .leaflet .legend i {
        vertical-align: middle;
      }
    "))
  )
