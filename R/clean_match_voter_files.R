library(tidyverse)
library(janitor)

# id: state_voter_id
# murm <- read_csv("data/voter_files/ModLikely+Charter.csv") |> 
#   janitor::clean_names() 

m1 <- read_csv("data/voter_files/Voted 21+23 Charter 55-64.csv") |> 
  clean_names()

m2 <- read_csv("data/voter_files/Voted 21+23 Charter 65+.csv") |> 
  clean_names()

murm <- bind_rows(m1, m2) |> 
  group_by(state_voter_id) |> 
  mutate(rn = row_number()) |> 
  filter(rn == 1) |> 
  select(-rn)


# id: Voters_StateVoterID
nap <- read_csv("data/voter_files/napcsa_master.csv") |> 
  mutate(state_voter_id = str_pad(Voters_StateVoterID, 
                                  side = "left",
                                  pad = "0", 
                                  width = 10))


these <- inner_join(murm, nap)

write_csv(these, "data/voter_files/matched_napcsa.csv")

just_murm_70 <- murm |> 
  filter(charter_support_score > 70.5)

write_csv(just_murm_70, "data/voter_files/cfc_murm_likely_and_charter.csv")



hist(these$charter_support_score)

bend <- read_csv("data/voter_files/bender.csv") |> 
  # filter(!EMAIL %in% these$email) |> 
  pull(EMAIL) |> 
  unique()

murm |> 
  filter(email %in% bend)



