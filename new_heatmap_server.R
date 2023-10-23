library(shiny)
library(tidyverse)
library(reshape2)
library(openxlsx)

# Function to clean gene names in orthogroups dataset to match species dataset
clean_orthogroups_gene_names <- function(orthogroups, species_name, species_genes) {
  if (species_name == "Catharanthus_roseus") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := paste0("gene-", !!sym(species_name)))
  }
  if (species_name == "Fragaria_vesca") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := gsub("\\.1$", "", !!sym(species_name)))
  }
  if (species_name == "Gossypium_bickii") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := gsub("\\.[0-9]+$", "", !!sym(species_name)))
  }
  if (species_name == "Gossypium_hirsutum") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := gsub("\\.[0-9]+$", "", !!sym(species_name)))
  }
  if (species_name == "Populus_alba_x_populus_glandulosa") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := gsub("\\..*", "", !!sym(species_name)))
  }
  if (species_name == "Arabidopsis") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := gsub("\\..*", "", !!sym(species_name)))
  }
  if (species_name == "Solanum_lycopersicum") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := gsub("\\..*", "", !!sym(species_name)))
  }
  if (species_name == "MtrunA17r5") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := gsub("_", "", !!sym(species_name)))
  }

  orthogroups <- orthogroups %>%
    filter(!!sym(species_name) %in% species_genes)

  return(orthogroups)
}

calculate_overlaps <- function(Species1, Species2, uploaded_species_name, ref_species_name) {
  clusters_S1 <- unique(Species1$clusterName)
  clusters_S2 <- unique(Species2$clusterName)

  overlaps <- data.frame(
    species1_cluster = character(0),
    species1_no_of_OMGs = integer(0),
    species2_cluster = character(0),
    species2_no_of_OMGs = integer(0),
    common_OMGs = integer(0),
    stringsAsFactors = FALSE
  )

  for (i in clusters_S1) {
    species1_no_of_OMGs <- nrow(Species1[Species1$clusterName == i, ])

    for (j in clusters_S2) {
      species2_no_of_OMGs <- nrow(Species2[Species2$clusterName == j, ])

      common_OMGs <- length(intersect(
        Species1[Species1$clusterName == i, ]$Orthogroup,
        Species2[Species2$clusterName == j, ]$Orthogroup
      ))

      overlaps <- rbind(overlaps, data.frame(
        species1_cluster = i,
        species1_no_of_OMGs = species1_no_of_OMGs,
        species2_cluster = j,
        species2_no_of_OMGs = species2_no_of_OMGs,
        common_OMGs = common_OMGs,
        stringsAsFactors = FALSE
      ))
    }
  }

  colnames(overlaps) <- c(
    paste0(uploaded_species_name, "_cluster - Query Cluster"),
    paste0(uploaded_species_name, "_no_of_OMGs_Q"),
    paste0(ref_species_name, "_cluster - Ref Cluster"),
    paste0(ref_species_name, "_no_of_OMGs_R"),
    "common_OMGs"
  )

  return(overlaps)
}

create_heatmap_and_overlaps <- function(species1_data, species2_data, orthogroups, species1_name, species2_name) {
  # Clean orthogroups gene names for each species
  orthogroups_species1 <- clean_orthogroups_gene_names(orthogroups, species1_name, species1_data$gene)
  orthogroups_species2 <- clean_orthogroups_gene_names(orthogroups, species2_name, species2_data$gene)

  # Merge with orthogroups
  species1_data <- merge(species1_data, orthogroups_species1, by.x = "gene", by.y = species1_name)
  species2_data <- merge(species2_data, orthogroups_species2, by.x = "gene", by.y = species2_name)

  # Calculate overlaps and get additional information
  overlaps <- calculate_overlaps(species1_data, species2_data, species1_name, species2_name)

  # Create a heatmap
  heatmap <- ggplot(overlaps, aes(
    y = reorder(as.factor(.data[[paste0(species1_name, "_cluster - Query Cluster")]]), -common_OMGs),
    x = reorder(as.factor(.data[[paste0(species2_name, "_cluster - Ref Cluster")]]), -common_OMGs),
    fill = common_OMGs
  )) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    geom_text(aes(label = sprintf("%.0f", common_OMGs)), color = "black", size = 4) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
      axis.text.y = element_text(angle = 0, size = 10)
    ) +
    labs(fill = "Common OMGs", y = paste(species1_name, "- Query Data"), x = paste(species2_name, "- Reference Data"))

  list(
    heatmap = heatmap,
    overlaps = overlaps,
    species1_data = species1_data,
    species2_data = species2_data
  )
}

# Server part for the heatmap
new_heatmap_server <- function(input, output, session) {
  # Load species data dynamically
  species_data_path <- "input_data/species_data"
  species_files <- list.files(species_data_path)
  species_names <- gsub("\\.csv$", "", species_files)

  placeholder_heatmap <- ggplot() +
    geom_tile(
      data = data.frame(
        x = rep(1:10, each = 10),
        y = rep(1:10, times = 10),
        val = runif(100, 0, 5)
      ),
      aes(x = x, y = y, fill = val)
    ) +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(fill = "Value", x = "Upload Query Data", y = "Select Reference Data")

  updateSelectInput(session, "species2", choices = species_names)

  process_data <- function() {
    # Ensure the uploaded file and selected species are available
    if (is.null(input$file1) || is.null(input$species2)) {
      return(list(heatmap = placeholder_heatmap, overlaps = NULL)) # Changed this line
    }

    # Extract the species names
    uploaded_species_name <- sub("(__.*|_[0-9]+)$", "", tools::file_path_sans_ext(basename(input$file1$name)))
    selected_species2_name <- sub("(__.*|_[0-9]+)$", "", input$species2)

    # Read the data files
    species1_data <- read.csv(input$file1$datapath)
    species2_data <- read.csv(file.path(species_data_path, paste0(input$species2, ".csv")))
    orthogroups <- read.delim("input_data/Orthogroups_091023_cleaned.tsv", header = TRUE, sep = "\t")

    # Create the heatmap and overlaps data
    create_heatmap_and_overlaps(species1_data, species2_data, orthogroups, uploaded_species_name, selected_species2_name)
  }

  values <- reactiveValues(
    processed_data = NULL,
    comparison_table = NULL,
    common_genes_data = NULL
  )

  observe({
    values$processed_data <- req(process_data())
  })

  output$new_heatmap_plot <- renderPlot({
    req(values$processed_data)
    values$processed_data$heatmap
  })

  # Adjust the comparison table rendering to use the overlaps data from processed_data
  output$comparison_table <- renderDT({
    req(values$processed_data)

    # Render the table
    datatable(
      values$processed_data$overlaps,
      rownames = FALSE, # Hide rownames
      selection = "single",
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50),
        dom = "Blfrtip", # 'B' is for buttons
        initComplete = JS(
          "function(settings, json) {",
          # Remove default search label
          "$('div.dataTables_filter label').text('');",

          # Create a new div for custom filters and append above the table
          "var filterDiv = $('<div id=\"customFilters\" style=\"padding: 5px 0;\"></div>');",
          "$('#comparison_table').before(filterDiv);",

          # Clear the customFilters div before adding new elements
          "$('#customFilters').empty();",

          # Add search boxes to the new div with some margins
          "$('<input type=\"text\" placeholder=\"Query Cluster\" id=\"col1-filter\" style=\"margin-right: 10px;\">').appendTo('#customFilters').on('keyup', function() {",
          "var table = settings.oInstance.api();",
          "table.columns(0).search(this.value).draw();",
          "});",
          "$('<input type=\"text\" placeholder=\"Reference Cluster\" id=\"col3-filter\" style=\"margin-right: 10px;\">').appendTo('#customFilters').on('keyup', function() {",
          "var table = settings.oInstance.api();",
          "table.columns(2).search(this.value).draw();",
          "});",

          # Move the download button to the customFilters div and align it to the right
          # If the download button is not already in the customFilters div, move it there
          # "if($('#customFilters').has('#download_button').length === 0) {",
          # "$('#download_button').appendTo('#customFilters').css({'float': 'right'});",
          # "}",
          "}"
        )
      )
    )
  })

  output$download_button <- downloadHandler(
    filename = function() {
      "comparison_table.xlsx"
    },
    content = function(file) {
      # Capture the filtered data
      filtered_data <- values$processed_data$overlaps[input$comparison_table_rows_all, ]
      write.xlsx(filtered_data, file, row.names = FALSE)
    }
  )

  observeEvent(input$comparison_table_rows_selected, {
    selected_row <- req(values$processed_data$overlaps[input$comparison_table_rows_selected, ])
    species1_data <- req(values$processed_data$species1_data)
    species2_data <- req(values$processed_data$species2_data)

    common_genes_data <- populate_new_table(selected_row, species1_data, species2_data)
    values$common_genes_data <- common_genes_data

    output$new_table <- DT::renderDataTable({
      datatable(common_genes_data, caption = "Common Genes Comparison")
    })
  })

  output$download_common_genes <- downloadHandler(
    filename = function() {
      paste("common_genes_comparison-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data <- req(values$common_genes_data) # Make sure to create this reactive value to store the common genes data

      # Writing the data to an Excel file
      write.xlsx(data, file, row.names = FALSE)
    }
  )

  populate_new_table <- function(selected_row, Species1, Species2) {
    # Extract the selected row's information
    uploaded_species_cluster <- as.character(selected_row[1])
    ref_cluster <- as.character(selected_row[3])

    # Filter the data from Species1 and Species2 based on the selected clusters
    species1_data <- Species1[as.character(Species1$cluster) == uploaded_species_cluster, ]
    species2_data <- Species2[as.character(Species2$cluster) == ref_cluster, ]

    # Find the common OGs between the two species
    common_OGs <- intersect(species1_data$Orthogroup, species2_data$Orthogroup)

    # Check if there are no common OGs
    if (length(common_OGs) == 0) {
      return(data.frame()) # Return an empty data frame
    }

    # Filter the genes associated with the common OGs, select the one with highest avg_log2FC, and drop the avg_log2FC column
    species1_genes <- species1_data %>%
      filter(Orthogroup %in% common_OGs) %>%
      arrange(desc(avg_log2FC)) %>%
      distinct(Orthogroup, .keep_all = TRUE)

    species2_genes <- species2_data %>%
      filter(Orthogroup %in% common_OGs) %>%
      arrange(desc(avg_log2FC)) %>%
      distinct(Orthogroup, .keep_all = TRUE)

    # Merge the genes from both species and select certain columns from .x and .y
    common_genes <- merge(species1_genes, species2_genes, by = "Orthogroup") %>%
      arrange(desc(avg_log2FC.x)) %>%
      select(Orthogroup, clusterName.x, gene.x, avg_log2FC.x, clusterName.y, gene.y, avg_log2FC.y) # Add or remove the columns you want here

    return(common_genes)
  }

  output$downloadButtons <- renderUI({
      # Get a list of files in the sample_data folder
      files <- list.files(path = "sample_data", full.names = FALSE)
      
      # Create a download button for each file
      buttons <- lapply(files, function(file) {
        downloadButton(paste0("download_", gsub("\\.", "_", file)), file)
      })
      
      do.call(tagList, buttons)
    })

  # Create a download handler for each file in the sample_data folder
  observe({
    files <- list.files(path = "sample_data", full.names = FALSE)

    lapply(files, function(file) {
      outputId <- paste0("download_", gsub("\\.", "_", file))

      output[[outputId]] <- downloadHandler(
        filename = function() {
          file
        },
        content = function(fileContent) {
          file.copy(paste0("sample_data/", file), fileContent)
        }
      )
    })
  })
  
}
