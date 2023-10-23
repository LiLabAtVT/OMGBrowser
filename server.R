library(shiny)

# Source your module files
source("new_heatmap_server.R")

# Define the main server function
server <- function(input, output, session) {
  # Call the server functions from the sourced files
  new_heatmap_server(input, output, session)
}
