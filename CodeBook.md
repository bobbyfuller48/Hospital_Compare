Repository contains functions that work on "outcome-of-care-measures.csv" file containing data from the Hospital Compare web site (http://hospitalcompare.hhs.gov) run by the U.S. Department of Health and Human Services. File contains data for every major hospital in every state in the USA. Functions create a table with the following variables: 
1. State: character - lists state of hospital
2. Hospital: character - lists hospital name 
3. Cause_Of_Death: character - lists the type of death rate. The values are: Heart_Attack_Death_Rate, Heart_Failure_Death_Rate, and Pneumonia_Death_Rate 
4. Death_Rates_30_Day: numeric - lists the risk adjusted rate (percentage) for each hospital
5. State_Rank_Best   : integer - lists how each hospital ranks in its state in terms of best outcome for a given 30_day_death_rate
6. State_Rank_Worst  : integer - lists how each hospital ranks in its state in terms of worst outcome for a given 30_day_death_rate
7. Out_Of : integer - lists how many hospitals each hospital is being compared to for State_Rank_Best and State_Rank_Worst

