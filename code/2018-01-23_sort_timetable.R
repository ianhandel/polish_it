# Take copied text from Web Timetable and make nice


library(tidyverse)
library(glue)
library(lubridate)
library(here)


days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

starts <- tibble(Weeks = glue("Sem2 wk{1:10}"),
                 date = seq(ymd("20180114"), length.out = 10, by = 7))

header <- "Activity\tDescription\tType\tStart\tEnd\tWeeks\tBuilding\tRoom\tStaff" 
header_vec <- names(read.delim(text = header))

raw <- read_lines(here::here("data/2018-01-23_timetable_raw")) %>% 
  keep(~ .x != "") %>% 
  keep(~ !str_detect(.x, "^Weeks")) %>% 
  keep(~ .x != header) %>% 
  "["(-1)

days_col <- match(raw, days) %>% tidyr:::fillDown() %>% tibble() %>% set_names("day_number")

other_cols <- read.delim(text = raw, header = FALSE) %>% set_names(header_vec)

ttable <- bind_cols(days_col, other_cols) %>% 
  filter(!Activity %in% days) %>%
  inner_join(starts, by = c("Weeks" = "Weeks")) %>% 
  mutate(start_date = date + days(day_number)) %>% 
  arrange(as.numeric(date), as.numeric(start_date)) %>% 
  mutate(nice_date = format(start_date, "%d %B %Y"),
         nice_day = format(start_date, "%A")) %>% 
  select(nice_day, nice_date, Start, End, Activity, Type, Building, Room, Staff)

write_csv(ttable, "my_timetable.csv")

