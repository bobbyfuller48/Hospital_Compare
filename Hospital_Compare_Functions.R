# Functions work on "outcome-of-care-measures.csv" file containing data from
# Hospital Compare web site (http://hospitalcompare.hhs.gov) run by 
# the U.S. Department of Health and Human Services. 


# Pre-conditions: None 
# Post-conditions: Function returns a tibble ("hospital_table") containing data 
# regarding hospital 30 day death rates for heart attack, heart failure,
# and pneumonia.
# Each row contains a state, a hospital within that state, a cause of death, 
# the hospital's 30 day death rate for the cause of death, how the hospital's 
# death rate ranks to other hospitals in the same state (where it ranks on  
# the list of lowest and the list of highest hospital death rates), and the 
# total number of hospitals it is being compared to in the state.
make_hospital_table <- function()
{
  # data is loaded into a dataframe from "outcome-of-care-measures.csv" file 
  hospital_table <- suppressMessages(read_csv('outcome-of-care-measures.csv', 
                                              na = 'Not Available')) 
  # relevant columns from dataframe are given more concise names
  hospital_table <- rename(hospital_table, 
                           Hospital = colnames(hospital_table[2]),
                           Heart_Attack_Death_Rate = colnames(hospital_table[11]), 
                           Heart_Failure_Death_Rate = colnames(hospital_table[17]),
                           Pneumonia_Death_Rate = colnames(hospital_table[23])) 
  
  # alternative method to changing column names   
  #  names(hospital_table) <- str_replace_all(names(hospital_table), 
  #                               c('\\(' = '', '\\)' = '', ' ' = '_', '-' = ''))
  #  
  #  hospital_table <- rename(hospital_table, Hospital = Hospital_Name, 
  #               Heart_Attack_Death_Rate 
  #               = Hospital_30Day_Death_Mortality_Rates_from_Heart_Attack, 
  #               Heart_Failure_Death_Rate 
  #               = Hospital_30Day_Death_Mortality_Rates_from_Heart_Failure, 
  #               Pneumonia_Death_Rate 
  #              = Hospital_30Day_Death_Mortality_Rates_from_Pneumonia)
  
  
  # data is cleaned
  hospital_table <- hospital_table %>% 
    
    # relevant columns are selected
    select(State, Hospital, Heart_Attack_Death_Rate, Heart_Failure_Death_Rate, 
           Pneumonia_Death_Rate) %>% 
    
    # dataframe reshaped to ensure tidiness
    gather('Heart_Failure_Death_Rate', 'Heart_Attack_Death_Rate', 
           'Pneumonia_Death_Rate', key = 'Cause_Of_Death', 
           value = 'Death_Rates_30_Day') %>% 
    
    # rows containing missing values are removed
    drop_na() %>%
    
    # table is ordered and rank columns are added
    arrange(Cause_Of_Death, State, Death_Rates_30_Day, Hospital) %>% 
    group_by(Cause_Of_Death, State) %>%
    mutate(State_Rank_Best = rank(Death_Rates_30_Day, ties.method = 'first'), 
           State_Rank_Worst = rank(desc(Death_Rates_30_Day), 
                                   ties.method = 'last'), 
           Out_Of = n())
  
  
  # tibble is returned
  hospital_table
} 


# Pre-conditions: Function takes in tibble created by make_hospital_table
# Post-conditions: Function returns a view of hospital_table
view_hospital_table <- function(hospital_table) view(hospital_table) 


# Pre-conditions: The 'hospital_table' argument must be a tibble created by 
# make_hospital_table(). The 'state' argument must be a string containing the
# abbreviation of a state (i.e. 'FL' or 'TX'). The 'outcome' argument must be 
# one of three strings specifying a disease outcome: 'heart failure', 
# 'heart attack', or 'pneumonia'.

# Post-conditions: Function returns string specifying the name of the hospital 
# with the best disease outcome (the lowest 30 day mortality rate) for the 
# state and outcome inputted into the function.
best <- function(hospital_table, state, outcome)
{
  # function haults and throws an error if preconditions are not met.
  valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  death_rates <- c('Heart_Attack_Death_Rate', 'Heart_Failure_Death_Rate',
                   'Pneumonia_Death_Rate')
  
  states <- state.abb
  states <- sort(append(states, c('DC', 'GU', 'PR', 'VI')))
  death <- match(outcome, valid_outcomes, nomatch = NA)
  
  if (!state%in%states) stop('invalid state')
  
  if (is.na(death)) stop('invalid outcome')
  
  
  # string is returned specifying hospital in 'state' with lowest 30 day death
  # rate for 'outcome'
  best_hospital <- hospital_table %>% 
    filter(State == state, 
           Cause_Of_Death == death_rates[death], 
           State_Rank_Best == 1)
  
  best_hospital$Hospital
} 


# Pre-conditions: The 'state' argument must be a string containing the 
# abbreviation of a state (i.e. 'FL' or 'TX'). The 'outcome' argument must be 
# one of three strings specifying a disease outcome: 'heart failure', 
# 'heart attack', or 'pneumonia'. The 'num' argument must be an integer or 
# the strings 'best' or 'worst'. 

# Post-conditions: Function returns string specifying the name of the hospital 
# with the best disease outcome (the lowest 30 day mortality rate) for the 
# state and outcome inputted into the function. If 'num' is greater than
# the number of hospitals in the specified state, function returns NA.
rankhospital <- function(hospital_table, state, outcome, num = 'best')
{
  
  # function haults and throws an error if preconditions are not met.
  valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  valid_num_str <- c('best', 'worst')
  death_rates <- c('Heart_Attack_Death_Rate', 'Heart_Failure_Death_Rate',
                   'Pneumonia_Death_Rate')
  states <- state.abb
  states <- sort(append(states, c('DC', 'GU', 'PR', 'VI')))
  
  death <- match(outcome, valid_outcomes, nomatch = NA)
  
  if (!state%in%states) stop('invalid state')
  if (is.na(death)) stop('invalid outcome')
  if (!is.numeric(num) & !num%in%valid_num_str) stop('invalid num input') 
  
  rank <- ifelse(num == 'best' || num == 'worst', 1, num)
  
  # create table containing hospitals of the specified 'outcome' 
  # in the specified 'state'
  h_table <- hospital_table %>% 
    filter(State == state, Cause_Of_Death == death_rates[death])
  
  # return NA if 'num' greater than number of hospitals in 'state'
  if (num > as.numeric(h_table[1, 'Out_Of']) & !num %in% valid_num_str) 
    return(NA)
  
  # select rows of specified rank
  if (num == 'worst') 
  {
    h_table <- filter(h_table, State_Rank_Worst == rank) 
  }
  else  
  {
    h_table <- filter(h_table, State_Rank_Best == rank) 
  }
  
  # return hospital name
  h_table$Hospital
} 


# Pre-conditions: The 'hospital_table' argument must be a tibble created by 
# make_hospital_table(). The 'outcome' argument must be 
# one of three strings specifying a disease outcome: 'heart failure', 
# 'heart attack', or 'pneumonia'. The 'num' argument must be an integer or 
# the strings 'best' or 'worst'. 

# Post-conditions: Function returns a table specifying the hospitals that 
# are ranked 'num' for each 'outcome' in their respective state. If 
# 'outcome' = 'best', table specifies hospitals ranked number 1.
# If 'outcome' = 'worst', table specifies hospitals with lowest rank. 
rankall <- function(hospital_table, outcome, num = 'best')
{
  
  # function haults and throws an error if preconditions are not met.
  valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  valid_num_str <- c('best', 'worst')
  death_rates <- c('Heart_Attack_Death_Rate', 'Heart_Failure_Death_Rate',
                   'Pneumonia_Death_Rate')
  states <- state.abb
  states <- sort(append(states, c('DC', 'GU', 'PR', 'VI')))
  
  death <- match(outcome, valid_outcomes, nomatch = NA)
  
  if (is.na(death)) stop('invalid outcome')
  if (!is.numeric(num) & !num%in%valid_num_str) stop('invalid num input') 
  
  death <- death_rates[death] 
  rank <- ifelse(num == 'best' || num == 'worst', 1, num)
  
  
  # create table containing hospitals of the specified rank in each state
  h_table <- hospital_table
  
  if (num == 'worst') 
  {
    h_table <- filter(h_table, State_Rank_Worst == rank, 
                      Cause_Of_Death == death)
  }
  else  
  {
    h_table <- filter(h_table, State_Rank_Best == rank, 
                      Cause_Of_Death == death)
  }
  
  
  # create table specifying the hospitals that are ranked 'num' for 
  # each 'outcome' in their respective state.
  modified_h_table <- tibble(hospital = NA, state = states) 
  
  for (i in 1:54) 
  {
    index <- match(states[i], unique(h_table[[1]]), nomatch = NA)
    if (!is.na(index))
      modified_h_table[[1]][[i]] <- h_table[[2]][[index]] 
    
  }
  
  
  # table is returned
  modified_h_table
} 