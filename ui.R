# Creating the UI part of the app
library(DT)
library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        .navbar-brand {
          font-weight: bold;
        }
        .jumbotron {
          padding: 1em;
        }
        .nav-link {
          color: white !important;
        }
        .nav-link.active {
          font-weight: bold;
          background-color: white !important;
          color: #4285F4 !important;
        }
        .footer {
          background-color: #343a40;
          color: white;
          position: fixed;
          left: 0;
          bottom: 0;
          width: 100%;
          padding: 10px;
          text-align: center;
        }
        .footer a {
          color: white;
        }
      ")
    )
  ),
  tags$div(
    class = "jumbotron text-center",
    style = "background-color: #333; color: white; padding: 10px; margin-bottom: 0;",
    tags$h1("OMG Browser", style = "margin-bottom: 5px;"),
    tags$p("Cross Species Single Cell Annotation", style = "font-size: 18px; margin: 0;")
  ),
  navbarPage(
    title = "",
    theme = "bootstrap",
    tabPanel(
      title = "Introduction",
      icon = icon("info-circle"),
      fluidPage(
        titlePanel("Orthologous Marker Genes Browser"),
        fluidRow(
          column(
            width = 6,
            tags$h3("Cross-Species Single Cell Annotation Made Easy"),
            tags$hr(),
            tags$h4("About OMG Browser"),
            tags$p("OMG Browser is a sophisticated tool designed for biologists, genomics researchers, 
                    and data scientists to annotate and explore single-cell RNA-seq data across various species."),
            tags$h4("Quick Start Guide"),
            tags$ol(
              tags$li("Navigate to ", tags$b("New Heatmap Comparison"), " tab."),
              tags$li("Upload your RNA-seq data."),
              tags$li("Select a reference species."),
              tags$li("Explore the heatmap and table."),
              tags$li("Download the results."),
              tags$li("Get sample files for practice.")
            ),
            tags$hr(),
            tags$h4("Contact Us"),
            tags$p("For support, feature requests, or contributions:"),
            tags$ul(
              tags$li(tags$a(href = "mailto:tnchau@vt.edu", "tnchau@vt.edu")),
              tags$li(tags$a(href = "mailto:bspavan25@vt.edu", "bspavan25@vt.edu"))
            ),
            br()
          ),
          column(
            width = 6,
            tags$h4("OMG Annotation"),
            tags$img(src = "heatmap_before.png", width = "100%"),
            tags$h4("Actual Annotation"),
            tags$img(src = "heatmap_after.png", width = "100%"),
            tags$p("Comparison of OMG Browser annotation and actual annotation.")
          )
        )
      )
    ),
    tabPanel(
      title = "New Heatmap Comparison",
      icon = icon("chart-bar"),
      sidebarLayout(
        sidebarPanel(
          width = 2,
          fileInput("file1", "Choose CSV File for Species 1",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    )
          ),
          selectInput("species2", "Select Species 2", ""),
          verbatimTextOutput("new_matched_genes")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "New Heatmap Comparision",
              plotOutput("new_heatmap_plot")
            ),
            tabPanel(
              "Common OG's - More Information",
              fluidRow(
                column(
                  width = 12, align = "center",
                  h3("More information"),
                  DTOutput("comparison_table"),
                  downloadButton("download_button", "Download"),
                  br(),
                  br(),
                  h4("Gene information - Select a row to get more information"),
                  DT::dataTableOutput("new_table"),
                  downloadButton("download_common_genes", "Download Gene Information"),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Download Sample Files",
      icon = icon("download"),
      fluidRow(
        uiOutput("downloadButtons")
      )
    )
  ),
  tags$div(
    class = "footer",
    HTML(paste0(
      "Made in <a href='https://lilabatvt.github.io/research/'>Li Lab, Virginia Tech</a>. ",
      "By: <a href='mailto:tnchau@vt.edu'>Tran Chau</a> & <a href='mailto:bspavan25@vt.edu'>Sai Pavan Bathala</a>."
    ))
  )
)
