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
  ggplot(aes(ymin = 0 - (total/2),
             ymax = 0 + (total/2),
             x = yes_margin, 
             color = name)) +
  geom_vline(xintercept = .5, linetype = 2, color = "grey80") +
  geom_errorbar(size = 2) +
  scale_x_continuous(limits = c(.45, .55), 
                     labels = label_percent(1),
                     breaks = c(.45, .5, .55)) +
  scale_y_continuous(breaks = 0) +
  scale_size_continuous(range = c(1, 25)) +
  theme(legend.position = "none")

these_maj_wards <- maj_wards |> 
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
  ))
  
these_maj_wards |> 
  ggplot(aes(y = total,x = yes_margin, 
             fill = name)) +
  geom_vline(xintercept = .5, linetype = 2, color = "grey80") +
  geom_col() +
  annotate(geom = "segment", x = .49, xend = .51,
           y = 42000, yend = 42000,
           color = "grey80", arrow = arrow(ends = "both", 
                                           length = unit(3, "mm"),
                                           type = "closed")) +
  annotate(geom = "text", label = "Majority voted No",
           y = 42000, x = .485)
  scale_x_continuous(labels = label_percent(.1),
                     breaks = c(these_maj_wards$yes_margin)) +
  scale_y_continuous(breaks = pretty_breaks(n = 3),
                     labels = label_comma()) +
  scale_size_continuous(range = c(1, 25)) +
  theme(legend.position = "top") +
  labs(x = "Share Voting Yes",
       y = "Total Votes",
       fill = "")
