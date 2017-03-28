# how many pit tags did we use each year?

source("../Phil_code/conleyte.R")
source("../Phil_code/dateanemobs.R")

leyte <- conleyte()

# find the fish that do not have a Y for recap (no tag) and a pit tag value (were then tagged)
tagged <- leyte %>% 
  tbl("clownfish") %>% 
  filter((recap != "Y" | is.na(recap)) & !is.na(tagid)) %>%
  collect()

### MUST BE CONNECTED TO LEYTE FIRST ###
# get dates of capture for these fish (want year) 
dates <- dateanem(tagged$anem_table_id)

tagged <- left_join(tagged, dates, by = "anem_table_id") 
tagged$date <- lubridate::ymd(tagged$date)
  
tagged <- tagged %>% mutate(year = lubridate::year(date))

count <- tagged %>% 
  group_by(year) %>% 
  summarise(
    count = n())

count %>% summarise(mean = mean(count))
      
      
