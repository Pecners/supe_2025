library(tidyverse)
library(wisconsink12)
library(sf)
library(tigris)

mke <- places(state = "wi") |> 
  filter(NAME == "Milwaukee")

bw_counts <- make_mke_schools() |> 
  filter(school_year == "2023-24" &
           broad_agency_type == "District Operated") |> 
  left_join(est_subgroup_enrollment() |> 
              filter(group_by_value %in% c("count_b_aa", "count_white"))) |> 
  group_by(dpi_true_id, group_by_value) |> 
  summarise(total = sum(student_count, na.rm = TRUE))

lq <- c("Fails to Meet Expectations",
        "Meets Few Expectations")

bw_counts |> 
  left_join(report_cards |> 
              filter(school_year == "2023-24")) |> 
  group_by(group_by_value, overall_rating) |> 
  summarise(total = sum(total)) |> 
  pivot_wider(names_from = group_by_value, values_from = total) |> 
  mutate(clean_or = factor(overall_rating, levels = c(
    # "Alternate Rating - Needs Improvement",
    # "Alternate Rating - Satisfactory Progress",
    "Fails to Meet Expectations",
    "Meets Few Expectations",
    "Meets Expectations",
    "Exceeds Expectations",
    "Significantly Exceeds Expectations"
  ))) |> 
  filter(!is.na(clean_or)) |> 
  group_by(hq = ifelse(clean_or %in% lq, FALSE, TRUE)) |> 
  summarise(total_white = sum(count_white),
            total_black = sum(count_b_aa)) |> 
  mutate_at(c("total_white", "total_black"), function(x) x / sum(x))


whites <- bw_counts |> 
  filter(group_by_value == "count_white") |> 
  arrange(desc(total)) |> 
  left_join(make_mke_rc() |> 
              filter(school_year == "2023-24") |> 
              select(dpi_true_id, school_name, accurate_agency_type, 
                     overall_rating)) |> 
  ungroup() |> 
  mutate(cumsum = cumsum(total) / sum(total)) |> 
  left_join(geocodes |> 
              filter(school_year == "2023-24"))

whites |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  head(10) |> 
  ggplot() +
  geom_sf(data = mke) +
  geom_sf()

           