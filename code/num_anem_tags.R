# how many anem tags have we been using each visit?

source("../Phil_code/conleyte.R")
source("../Phil_code/dateanemobs.R")

leyte <- conleyte()

# find anemones with tags that are the metal tags (>2000)
tagged <- leyte %>% 
  tbl("anemones") %>% 
  filter(anem_id > 2000) %>%
  collect()

# get dates for the anemones
dates <- dateanem(tagged$anem_table_id)

# add dates to anem table
tagged <- left_join(tagged, dates, by = "anem_table_id")
tagged$date <- lubridate::ymd(tagged$date)

# a table of anem_ids observed in 2015
two015 <- tagged %>% 
  filter(date >= "2015-01-01" & date <= "2015-12-31")

# a table of anem_ids observed in 2016
two016 <- tagged %>% 
  filter(date > "2016-01-01") 

# which tags are new this year
two016 <- anti_join(two016, two015, by = "anem_id") 

# because we used 342 in 2016, make sure there are at least 400 for 2017 (hopefully will need fewer)


