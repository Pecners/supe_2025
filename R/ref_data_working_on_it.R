library(tidyverse)
library(sf)
library(cityforwardcollective)
library(glue)
library(scales)
library(htmltools)
library(janitor)
# demo data from John Johnson
# https://github.com/jdjohn215/wisc-election-night-data/blob/main/2024-nov/wec/reporting-unit-demographics-2020census.csv

w_demo <- read_csv("data/reporting-unit-demographics-2020census.csv")

cols <- c(
  "#1F77B4",
  "#FF7F0E"
  # "#2CA02C"
)

all <- read_rds("data/cleaned_ref_data.rda") |> 
  clean_names() |> 
  pivot_longer(cols = c(1:2), names_to = "candidate", values_to = "votes") |> 
  group_by(new_lab) |> 
  mutate(perc = votes / sum(votes),
         col = case_when(
           candidate == "ref_no" ~ cols[2],
           candidate == "ref_yes" ~ cols[1],
           TRUE ~ cols[3]
         )) |> 
  st_transform(crs = 4326)

wins <- all |> 
  as_tibble() |> 
  ungroup() |> 
  group_by(new_lab) |> 
  filter(perc == max(perc)) |> 
  mutate(rn = row_number()) |> 
  filter(rn == 1) |> 
  ungroup() |> 
  select(new_lab, 
         winner = candidate,
         perc,
         col)

wider_n <- all |> 
  select(-c(perc, col, county)) |> 
  pivot_wider(names_from = candidate, values_from = votes) |> 
  left_join(wins)

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
         no_margin = ref_no / (ref_no + ref_yes)) |> 
  ggplot(aes(no_margin, perc_black)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(aspect.ratio = 1)

# > 50% black
maj_wards <- both |> 
  filter(city == "Milwaukee") |> 
  mutate(perc_black = pop_black / pop,
         perc_white = pop_white / pop,
         perc_hisp = pop_hisp / pop,
         no_margin = ref_no / (ref_no + ref_yes)) |> 
  group_by(bw = perc_black >= .5,
           ww = perc_white >= .5,
           hw = perc_hisp >= .5,
           none = perc_black < .5 &
             perc_white < .5 &
             perc_hisp < .5) |> 
  summarise(total = sum(ref_no) + sum(ref_yes),
            no_vote = sum(ref_no),
            yes_vote = sum(ref_yes)) |> 
  mutate(yes_margin = yes_vote / total)


maj_wards |> 
  as_tibble() |> 
  select(1:4, total, yes_margin) |> 
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
  ggplot(aes(y = 1, x = yes_margin, 
             color = name,
             size = total)) +
  geom_vline(xintercept = .5, linetype = 2, color = "grey80") +
  geom_point() +
  
  scale_x_continuous(limits = c(.45, .55), 
                     labels = label_percent(1),
                     breaks = c(.45, .5, .55)) +
  scale_y_continuous(breaks = 1) +
  scale_size_continuous(range = c(1, 25)) +
  theme(legend.position = "none")

ggsave("plots/crawford_kinser_split_vote.png", bg = "transparent",
       width = 9, h = 6)

maj_wards |> 
  as_tibble() |> 
  select(1:4, total) |> 
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
