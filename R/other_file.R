library(tidyverse)
library(janitor)

# id: state_voter_id
# murm <- read_csv("data/voter_files/ModLikely+Charter.csv") |> 
#   janitor::clean_names() 

murm <- read_csv("data/voter_files/Voted 24 Age 67+.csv") |> 
  bind_rows(
    read_csv("data/voter_files/Voted 24 Age 51-66.csv")
  ) |> 
  bind_rows(
    read_csv("data/voter_files/Voted 24 Age _51.csv")
  ) |> 
  bind_rows(
    read_csv("data/voter_files/Voted 21+23 Charter 55-64.csv")
  ) |> 
  bind_rows(
    read_csv("data/voter_files/Voted 21+23 Charter 65+.csv")
  ) |> 
  clean_names()



murm <- murm |> 
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


nap |> 
  transmute(state_voter_id,
            first_name = Voters_FirstName,
            last_name = Voters_LastName,
            state = "WI",
            zip = str_extract(`Full Address`, "\\d{5}$"),
            voter_status = ifelse(Voters_Active == "Active", 'true', 'false')) |> 
  write_csv("data/voter_files/napcsa_skinny.csv")

#####

new <- read_csv("data/voter_files/id_only_murm_matches.csv")

nn <- read_csv("data/voter_files/napcsa_master.csv") 

nn |> 
  filter(Voters_StateVoterID %in% new$`State Voter ID`) |> 
  write_csv("data/voter_files/last_three_elex_and_charter_72+.csv")

#####

new <- read_csv("data/voter_files/w_partisan.csv") |> 
  unique()

nn <- read_csv("data/voter_files/napcsa_master.csv") 

nn |> 
  filter(Voters_StateVoterID %in% new$`State Voter ID`) |>
  inner_join(new |> 
               rename(Voters_StateVoterID = `State Voter ID`)) |> 
  group_by(Voters_StateVoterID) |> 
  mutate(n = row_number()) |> 
  filter(n == 1) |> 
  select(-n) |> 
  select(`Partisan Score`, everything()) |> 
  write_csv("data/voter_files/last_three_elex_and_charter_72+_w_partisan.csv")
