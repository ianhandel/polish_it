# Take copied text from Web Timetable and make nice


library(tidyverse)
library(glue)
library(lubridate)
library(here)


days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")


starts <- read_lines(here::here("data/week_lookup")) %>% 
  str_split("\tw/c ") %>%
  map_df(~ tibble(Weeks = .x[[1]], date = dmy(.x[[2]])))


header <- "Activity\tDescription\tType\tStart\tEnd\tWeeks\tBuilding\tRoom\tStaff" 
header_vec <- names(read.delim(text = header))

raw <- read_lines(here::here("data/2018_01-23_rw_raw")) %>% 
  keep(~ .x != "") %>% 
  keep(~ !str_detect(.x, "^Weeks")) %>% 
  keep(~ .x != header) %>% 
  "["(-1)

days_col <- match(raw, days) %>% tidyr:::fillDown() %>% tibble() %>% set_names("day_number")

other_cols <- read.delim(text = raw, header = FALSE) %>% set_names(header_vec)

ttable <- bind_cols(days_col, other_cols) %>% 
  filter(!Activity %in% days) %>%
  inner_join(starts, by = c("Weeks" = "Weeks")) %>% 
  mutate(start_date = date + days(day_number) - 1) %>% 
  arrange(as.numeric(date), as.numeric(start_date)) %>% 
  mutate(nice_date = format(start_date, "%d %B %Y"),
         nice_day = format(start_date, "%A")) %>% 
  select(nice_day, nice_date, Start, End, Activity, Type, Building, Room, Staff)

write_csv(ttable, "my_timetable.csv")

