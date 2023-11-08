# Creating the UI part of the app
library(DT)
library(shiny)
library(shinyHeatmaply)
library(plotly)

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
        # body {
        #   zoom: 75%;
        # }
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
    style = "background-color: #333; color: white; padding: 7px; margin: 2px;",
    tags$h2("OMG Browser", style = "margin: 8px;"),
    tags$p("Cross Species Single Cell Annotation", style = "font-size: 16px; margin: 2px;")
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
            tags$p("OMG Browser is designed for biologists, genomics researchers,
                    and data scientists interested in annotating and exploring
                    single-cell RNA-seq data across various species. This tool,
                    equipped with a user-friendly interface, aids in the visualization,
                    analysis, and comparison of gene expression patterns and cell clusters."),
            tags$h4("Quick Start Guide"),
            tags$ol(
              tags$li(tags$b("New Heatmap Comparison"), ": Navigate to this tab, upload your unannotated single-cell RNA-seq data in CSV format, choose a reference species from the dropdown list, explore the generated heatmap and table for common orthogroups, and download the comparison table and gene information for further analysis."),
              tags$li(tags$b("Download Sample Files"), ": Visit this tab to download sample files for practice and exploration of the OMG Browser’s capabilities.")
            ),
            tags$hr(),
            tags$h4("Tables and Common Orthogroups (OGs)"),
            tags$p("In the 'New Heatmap Comparison' tab, after uploading the RNA-seq data and selecting a reference species, users are presented with a heatmap and a table of common orthogroups (OGs). This table is a powerful resource for in-depth analysis. Users can explore common OGs, download specific data, and gain additional gene information for a comprehensive understanding of the biological context."),
            tags$ul(
              tags$li("The table lists common OGs, providing insights into shared gene groups among different species."),
              tags$li("Users can download the entire comparison table or select specific rows for detailed gene information."),
              tags$li("Selecting a row offers additional information, aiding in the understanding of genes within each orthogroup, their expressions, and potential roles in biological processes.")
            ),
            tags$h4("Contact Us"),
            tags$p("For support, feature requests, or contributions, reach out through the provided contact emails:"),
            tags$ul(
              tags$li(tags$a(href = "mailto:tnchau@vt.edu", "tnchau@vt.edu")),
              tags$li(tags$a(href = "mailto:bspavan25@vt.edu", "bspavan25@vt.edu"))
            ),
            br(),
            tags$a(href = "https://github.com/LiLabAtVT/OMGBrowser", "GitHub Repository", target = "_blank")
          ),
          column(
            width = 6,
            tags$h4("OMG Annotation"),
            tags$img(src = "heatmap_before.png", width = "100%"),
            tags$h4("Actual Annotation"),
            tags$img(src = "heatmap_after.png", width = "100%"),
            tags$p("Comparison of OMG Browser annotation and actual annotation."),
            br(),
            br(),
            br()
          )
        )
      )
    ),
    tabPanel(
      title = "New Heatmap Comparison",
      icon = icon("chart-bar"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fileInput("file1", "Upload CSV File - Query Species",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          selectInput("species2", "Select Reference Species(Tissue)", ""),
          tags$strong("Dataset Reference:"), # This is the label for the dataset reference
          textOutput("dataset_info") # New UI element for dataset info
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "New Heatmap Comparision",
              align = "center",
              width = 11,
              br(),
              plotlyOutput("new_heatmap_plot"), # Display heatmap
              br(),
              h3("Upload and Select a cell to know more"),
              DTOutput("hovered_info_table"),
              downloadButton("download_info_table", "Download Table"),
              br(),
              br(),
              br(),
              br(),
              br()
            ),
            tabPanel(
              "Common OG's - More Information",
              fluidRow(
                column(
                  width = 12, align = "center",
                  h3("Exploring the Table"),
                  DTOutput("comparison_table"),
                  downloadButton("download_button", "Download Comparison Table"),
                  br(),
                  br(),
                  h4("Gene Information - Select a Row for Details"),
                  DT::dataTableOutput("new_table"),
                  downloadButton("download_common_genes", "Download Gene Information"),
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
      title = "References",
      icon = icon("book"),
      fluidRow(
        DT::dataTableOutput("references_table")
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
