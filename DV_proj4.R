library(readr)
library(tidyr)
require(data.world)

# 1. read csv
call_data	=	read_csv("/Users/amandale/Downloads/Call_Data.csv", col_types = cols(
  YR_MO = col_integer(),
  CALL_DATE = col_date(format = ""),
  AGENT_KEY = col_integer(),
  CALLS = col_integer(),
  HANDLE_TIME = col_integer(),
  CALL_REGEN = col_integer(),
  CALLS_WITH_OFFER = col_integer(),
  CALLS_WITH_ACCEPT = col_integer(),
  CALLS_OFFER_APPLIED = col_integer(),
  TRANSFERS = col_integer()
))

hierarchy = read_csv("/Users/amandale/Downloads/Hierarchy.csv", col_types = cols(
  AGENT_KEY = col_integer(),
  EFF_DT = col_date(format = ""),
  TERM_DT = col_date(format = ""),
  AGENT_ID = col_integer(),
  AGENT_NAME = col_character(),
  TEAM_LEAD_ID = col_integer(),
  TEAM_LEAD_NAME = col_character(),
  CALL_CENTER = col_character()
))

targets = read_csv("/Users/amandale/Downloads/Targets.csv", col_types = cols(
  YR_MO = col_integer(),
  METRIC = col_character(),
  TARGET = col_double()
))

# I don't know what he expects to change just by specifying data type if we're loading back in from data.world - Could someone specify for me? 

# 2. non select * SQL 
project <- "https://data.world/rc38246/f-17-edv-project-4/"
data.world::set_config(cfg_env("DW_API"))
data <- data.world::query(
  data.world::qry_sql("SELECT call_data.agent_key, call_data.calls, call_data.handle_time, call_data.call_regen, call_data.calls_with_offer,
call_data.calls_with_accept, call_data.calls_offer_applied, call_data.transfers, hierarchy.agent_name, hierarchy.team_lead_id, hierarchy.team_lead_name, hierarchy.call_center
FROM call_data join hierarchy on call_data.agent_key = hierarchy.agent_key"),
  dataset = project
  )

#3. gather - idk what to do here lol
by_agent <- data %>% dplyr::group_by(agent_key) %>% View()
data1 <- tidyr::gather(data, key="key", value = "cases", na.rm = TRUE)

#4. dpylr 
# Looking into most successful agents and teams
# topics coverered - piplines. select, mutate , group_by, summarize, arrange 
data_SA = data %>% dplyr::select(agent_key, agent_name, calls_with_accept, calls, team_lead_id) %>% dplyr::mutate(call_acceptance_rate = calls_with_accept / calls) %>% dplyr::group_by(agent_key, agent_name,team_lead_id) %>% dplyr::summarize(avg_calls_with_a = mean(calls_with_accept), avg_calls = mean(calls), avg_call_acceptance_rate = mean(call_acceptance_rate))

data_SA_sort = data_SA %>% dplyr::arrange(desc(avg_call_acceptance_rate))
# Notice the top 7 are all on the same team 

data_SA_sort = data_SA %>% dplyr::arrange(avg_call_acceptance_rate)
#while the bottom 9 are on the same team 

# Is there a significant difference in between the teams? 
teams = data_SA %>% dplyr::select(team_lead_id, avg_calls, avg_call_acceptance_rate) %>% dplyr::group_by(team_lead_id) %>% dplyr::summarize(mean(avg_calls), mean(avg_call_acceptance_rate))
teams

# teams are getting significantly better acceptance rates when making more calls overall. 
ggplot(data = teams, aes(x=`mean(avg_calls)`, y = `mean(avg_call_acceptance_rate)`)) + geom_line()
