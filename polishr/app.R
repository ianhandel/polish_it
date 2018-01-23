#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(glue)
library(lubridate)
library(here)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Timetable Polisher"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "text", "Upload grabbed text"
      )
    ),

    # Polished timetable
    mainPanel(
      dataTableOutput("timetable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$timetable <- renderDataTable({
    days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

    starts <- read_lines(here::here("data/week_lookup")) %>%
      str_split("\tw/c ") %>%
      map_df(~ tibble(Weeks = .x[[1]], date = dmy(.x[[2]])))

    print(starts)

    header <- "Activity\tDescription\tType\tStart\tEnd\tWeeks\tBuilding\tRoom\tStaff"
    header_vec <- names(read.delim(text = header))

    raw <- read_lines(input$text$datapath[[1]]) %>%
      keep(~ .x != "") %>%
      keep(~ !str_detect(.x, "^Weeks")) %>%
      keep(~ .x != header) %>%
      "["(-1)

    days_col <- match(raw, days) %>%
      tidyr:::fillDown() %>%
      tibble() %>%
      set_names("day_number")

    other_cols <- read.delim(text = raw, header = FALSE) %>%
      set_names(header_vec)

    ttable <- bind_cols(days_col, other_cols) %>%
      filter(!Activity %in% days) %>%
      inner_join(starts, by = c("Weeks" = "Weeks")) %>%
      mutate(start_date = date + days(day_number) - 1) %>%
      arrange(as.numeric(date), as.numeric(start_date)) %>%
      mutate(
        nice_date = format(start_date, "%d %B %Y"),
        nice_day = format(start_date, "%A")
      ) %>%
      select(nice_day, nice_date, Start, End, Activity, Type, Building, Room, Staff)
    
    ttable
  })
}

# Run the application
shinyApp(ui = ui, server = server)