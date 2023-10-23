library(shiny)

# Define the species names that we support
species_names <- c("ATH", "Maize", "Rice")

source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
