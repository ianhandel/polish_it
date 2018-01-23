# Take copied text from Web Timetable and make nice


library(tidyverse)
library(here)

days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

header <- "Activity\tDescription\tType\tStart\tEnd\tWeeks\tBuilding\tRoom\tStaff" 
header_vec <- names(read.delim(text = header))

raw <- read_lines(here("data/2018-01-23_timetable_raw")) %>% 
  keep(~ .x != "") %>% 
  keep(~ !str_detect(.x, "^Weeks")) %>% 
  keep(~ .x != header) %>% 
  "["(-1)

# create an empty dataframe with headers
header_vec <- c(header_vec, "day")
ttable <- do.call(data.frame, as.list(header_vec) %>% set_names())[-1,] %>% as_tibble()
for(ii in seq_along(raw)){
  if(raw[[ii]] %in% days) {
    # ttable$day[[ii]] <- raw[ii]
  } else {
    cells <- str_split(raw[ii], "\t")[[1]]
      print(cells)
    ttable[ii, ] <- c(cells, NA)
  }
}
