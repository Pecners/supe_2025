# Spring 2025 votes
cols <- c(
  "#1F77B4",
  "#FF7F0E"
  # "#2CA02C"
)

scowis <- read_csv("data/scowis_results_2025.csv") |> 
  clean_names()


all <- read_rds("data/cleaned_supe_race_data.rda") |> 
  clean_names() |> 
  filter(city == "Milwaukee") |> 
  pivot_longer(cols = c(1:3), names_to = "candidate", values_to = "votes") |> 
  mutate(new_lab = case_when(
    new_lab == "Milwaukee - C 0355" ~ "Milwaukee - C 0348", 
    new_lab == "Milwaukee - C 0356" ~ "Milwaukee - C 0282",
    TRUE ~ new_lab)) |> 
  group_by(new_lab) |> 
  mutate(perc = votes / sum(votes),
         col = case_when(
           candidate == "brittany_kinser" ~ cols[1],
           candidate == "jill_underly" ~ cols[2],
           TRUE ~ cols[3]
         )) |> 
  st_transform(crs = 4326)

a1 <- all |> 
  filter(new_lab %in% c("Milwaukee - C 0348",
                  "Milwaukee - C 0282")) |> 
  group_by(new_lab, 
           candidate) |> 
  summarise(votes = sum(votes)) |> 
  mutate(perc = votes / sum(votes),
         col = case_when(
           candidate == "brittany_kinser" ~ cols[1],
           candidate == "jill_underly" ~ cols[2],
           TRUE ~ cols[3]
         ),
         city = "Milwaukee")

fixed_wards <- all |> 
  filter(!new_lab %in%  c("Milwaukee - C 0348",
                    "Milwaukee - C 0282")) |> 
  bind_rows(a1)

wins <- fixed_wards |> 
  as_tibble() |> 
  group_by(new_lab) |> 
  filter(perc == max(perc)) |> 
  ungroup() |> 
  select(new_lab, 
         winner = candidate,
         perc,
         col)


wider_n <- fixed_wards |> 
  select(-c(perc, col)) |> 
  pivot_wider(names_from = candidate, values_from = votes) |> 
  left_join(wins) |> 
  left_join(scowis, by = c("original_ward" = "ward")) |> 
  mutate(u_marg = jill_underly - brittany_kinser,
         under_under = susan_crawford - jill_underly,
         kinser_over = brittany_kinser - brad_schimel,
         kinser_metric = kinser_over / (susan_crawford+brad_schimel+write_in.y),
         sc_col = case_when(
           susan_crawford > brad_schimel ~ cols[1],
           brad_schimel > susan_crawford ~ cols[2],
           TRUE ~ "grey"
         ),
         new_lab = case_when(
           new_lab == "Milwaukee - C 0355" ~ "Milwaukee - C 0348", 
           new_lab == "Milwaukee - C 0356" ~ "Milwaukee - C 0282",
           TRUE ~ new_lab)) |> 
  filter(city == "Milwaukee")

saveRDS(wider_n, "data/2025_elex_for_jack.rda")
