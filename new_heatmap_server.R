library(shiny)
library(tidyverse)
library(reshape2)
library(readxl)
library(shinyHeatmaply)
library(plotly)
library(RColorBrewer)
library(DT)
library(shinyalert)


# Function to clean gene names in orthogroups dataset to match species dataset
clean_orthogroups_gene_names <- function(orthogroups, species_name, species_genes) {
  if (species_name == "Arabidopsis") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := gsub("\\..*$", "", !!sym(species_name)))
  }
  if (species_name == "Zeamays") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := gsub("_.*$", "", !!sym(species_name)))
  }
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
  if (species_name == "Oryza") {
    orthogroups <- orthogroups %>%
      mutate(!!species_name := gsub("t(\\d+)-.*$", "g\\1", !!sym(species_name)))
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
  # Attempt to merge and catch any errors
  tryCatch(
    {
      # Clean orthogroups gene names for each species
      orthogroups_species1 <- clean_orthogroups_gene_names(orthogroups, species1_name, species1_data$gene)
      orthogroups_species2 <- clean_orthogroups_gene_names(orthogroups, species2_name, species2_data$gene)

      # Merge with orthogroups
      species1_data <- merge(species1_data, orthogroups_species1, by.x = "gene", by.y = species1_name)
      species2_data <- merge(species2_data, orthogroups_species2, by.x = "gene", by.y = species2_name)
    },
    error = function(e) {
      # If there's an error, show an alert
      shinyalert::shinyalert(
        title = "Error, Please Refresh the page",
        text = "1. Make sure your CSV file name follows the same name from the reference dropdown and use the same species name from it. 2. Please make sure your uploaded data has the same columns as: gene, clusterName, avg_log2FC",
        type = "error"
      )
      return(NULL)
    }
  )

  # After merging, check if the resulting dataframe is empty
  if (nrow(species1_data) == 0) {
    shinyalert::shinyalert(
      title = "Marker genes mismatch Error",
      text = "Please make sure your uplaoded genes match the gene format of our reference genes from the referenc tab",
      type = "error"
    )

    # Return from function early since there's an error
    return(NULL)
  }

  # Calculate overlaps and get additional information
  overlaps <- calculate_overlaps(species1_data, species2_data, species1_name, species2_name)

  # Identify columns
  all_columns <- colnames(overlaps)
  cluster_columns <- all_columns[!all_columns %in% c("common_OMGs")]

  query_col_name <- cluster_columns[1]
  ref_col_name <- cluster_columns[3]

  # Pivot to matrix format
  overlaps_matrix <- dcast(overlaps,
    overlaps[[query_col_name]] ~ overlaps[[ref_col_name]],
    value.var = "common_OMGs",
    fill = 0
  )

  # Ordering rows consistently
  overlaps_matrix <- overlaps_matrix[order(overlaps_matrix[[1]]), ]

  # Remove row names for heatmaply
  rownames(overlaps_matrix) <- overlaps_matrix[[1]]
  overlaps_matrix <- overlaps_matrix[, -1]

  custom_hover <- function(mat, x, y, z) {
    paste0(
      "Query Cluster: ", x, "<br>",
      "Reference Cluster: ", y, "<br>",
      "Common OMGs: ", z
    )
  }

  heatmap <- heatmaply(overlaps_matrix,
    showticklabels = c(TRUE, TRUE),
    show_row_dendrogram = FALSE,
    show_col_dendrogram = FALSE,
    cellnote = overlaps_matrix,
    main = " ",
    xlab = paste0(species2_name, " - Reference Cluster"),
    ylab = paste0(species1_name, " - Query Cluster"),
    colors = colorRampPalette(brewer.pal(9, "Greens"))(256),
    custom_hoverinfo = custom_hover,
    dendrogram = "none"
  )

  list(
    heatmap = heatmap,
    overlaps = overlaps,
    overlaps_matrix = overlaps_matrix,
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

  updateSelectInput(session, "species2", choices = species_names)

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
    labs(fill = "Value", x = "Select Reference Data", y = "Upload Query Data")

  process_data <- function() {
    # Ensure the uploaded file and selected species are available
    if (is.null(input$file1) || is.null(input$species2)) {
      return(list(heatmap = placeholder_heatmap, overlaps = NULL)) # Changed this line
    }

    # Extract the species names
    uploaded_species_name <- sub("(__.*|_[0-9].*)$", "", tools::file_path_sans_ext(basename(input$file1$name)))
    selected_species2_name <- sub("(__.*|_[0-9].*)$", "", input$species2)

    # validation step
    # Generate a list of base species names from species_names
    species_basename_list <- unique(sub("(__.*|_[0-9].*)$", "", species_names))

    # Check if uploaded species basename is in the list of base species names
    if (!(uploaded_species_name %in% species_basename_list)) {
      shinyalert::shinyalert(
        title = "Species Name Error",
        text = "The uploaded file's species name does not match any known species names in the reference dropdown, please rename the file as in dropdown like 'Arabidopsis, Brassica_rapa, Oryza, Zeamays ..('_'  and tissue type not necessay)'",
        type = "error"
      )
      return(NULL)
    }

    # Read the data files
    species1_data <- read.csv(input$file1$datapath)

    # Standardize the case of the column names
    column_names_lower <- tolower(names(species1_data))

    # Check if there's a 'cluster' column regardless of the case and rename it
    if ("cluster" %in% column_names_lower) {
      # Find the original name of the 'cluster' column (with original case)
      original_cluster_name <- names(species1_data)[grepl("cluster", names(species1_data), ignore.case = TRUE)]

      # Rename the 'cluster' column to 'clusterName'
      names(species1_data)[names(species1_data) == original_cluster_name] <- "clusterName"
    }

    # Check if file reading was successful and has required columns
    required_columns <- c("gene", "clusterName", "avg_log2FC")
    if (!all(required_columns %in% names(species1_data))) {
      shinyalert::shinyalert(
        title = "File Content Error",
        text = "The uploaded data file is missing required columns: 'gene', 'clusterName', and 'avg_log2FC'. Please update the file and try again.",
        type = "error"
      )
      return(NULL)
    }

    species1_data <- species1_data %>%
      group_by(clusterName) %>%
      arrange(desc(avg_log2FC)) %>%
      slice_head(n = 200) %>%
      ungroup()

    species2_data <- read.csv(file.path(species_data_path, paste0(input$species2, ".csv")))
    species2_data <- species2_data %>%
      group_by(clusterName) %>%
      arrange(desc(avg_log2FC)) %>%
      slice_head(n = 200) %>%
      ungroup()

    orthogroups <- read.delim("input_data/Orthogroups_091023_cleaned.tsv", header = TRUE, sep = "\t")

    # Create the heatmap and overlaps data
    create_heatmap_and_overlaps(species1_data, species2_data, orthogroups, uploaded_species_name, selected_species2_name)
  }

  values <- reactiveValues(
    processed_data = NULL,
    comparison_table = NULL,
    common_genes_data = NULL,
    first_page = NULL
  )

  observe({
    processed_data <- req(process_data())
    values$processed_data <- processed_data
  })

  output$new_heatmap_plot <- renderPlotly({
    req(values$processed_data)
    p <- values$processed_data$heatmap

    # Assign a source to the plot
    p$x$source <- "newHeatmapSource"

    return(p)
  })

  observe({
    clicked_info <- event_data(event = "plotly_click", source = "newHeatmapSource")

    if (!is.null(clicked_info)) {
      # Extract the x and y positions
      x_index <- clicked_info$x
      y_index <- clicked_info$y

      # Extract cluster names using indices
      query_cluster <- rownames(values$processed_data$overlaps_matrix)[length(rownames(values$processed_data$overlaps_matrix)) - y_index + 1]
      ref_cluster <- colnames(values$processed_data$overlaps_matrix)[x_index] # No need to exclude the first column since it's matrix

      species1_data <- req(values$processed_data$species1_data)
      species2_data <- req(values$processed_data$species2_data)

      first_page <- populate_new_table(query_cluster, ref_cluster, species1_data, species2_data)
      values$first_page <- first_page

      output$hovered_info_table <- renderDT({
        datatable(values$first_page, caption = "Cell Data")
      })
    }
  })

  output$download_info_table <- downloadHandler(
    filename = function() {
      paste("comparison_table-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Capture the filtered data
      data_to_download <- values$first_page # Ensure this is the full dataset you want to download
      write.xlsx(data_to_download, file, row.names = FALSE)
    }
  )

  # Adjust the comparison table rendering to use the overlaps data from processed_data
  output$comparison_table <- renderDT({
    req(values$processed_data)

    # Assuming 'values$processed_data$overlaps' is your data frame and columns at index 1 and 3 need to be converted to character

    values$processed_data$overlaps[, 1] <- as.character(values$processed_data$overlaps[, 1])
    values$processed_data$overlaps[, 3] <- as.character(values$processed_data$overlaps[, 3])

    # Now your columns are explicitly character strings, which should prevent the DataTables error


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
          "$('<input type=\"text\" placeholder=\"Query Cluster\" id=\"col0-filter\" style=\"margin-right: 10px;\">').appendTo('#customFilters').on('keyup', function() {",
          "var table0 = settings.oInstance.api();",
          "table0.columns(0).search(this.value.toString()).draw();",
          "});",
          "$('<input type=\"text\" placeholder=\"Reference Cluster\" id=\"col3-filter\" style=\"margin-right: 10px;\">').appendTo('#customFilters').on('keyup', function() {",
          "var table = settings.oInstance.api();",
          "table.columns(2).search(this.value).draw();",
          "});",
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

    common_genes_data <- populate_new_table(as.character(selected_row[1]), as.character(selected_row[3]), species1_data, species2_data)
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

  populate_new_table <- function(uploaded_species_cluster, ref_cluster, Species1, Species2) {
    # Extract the selected row's information
    # uploaded_species_cluster <- as.character(selected_row[1])
    # ref_cluster <- as.character(selected_row[3])

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
    files <- list.files(path = "input_data/species_data", full.names = FALSE)

    # Create a download button for each file
    buttons <- lapply(files, function(file) {
      downloadButton(paste0("download_", gsub("\\.", "_", file)), file)
    })

    do.call(tagList, buttons)
  })

  # Create a download handler for each file in the sample_data folder
  observe({
    files <- list.files(path = "input_data/species_data", full.names = FALSE)

    lapply(files, function(file) {
      outputId <- paste0("download_", gsub("\\.", "_", file))

      output[[outputId]] <- downloadHandler(
        filename = function() {
          file
        },
        content = function(fileContent) {
          file.copy(paste0("input_data/species_data/", file), fileContent)
        }
      )
    })
  })

  references_data <- readxl::read_excel("input_data/TableForReferenceScData_11072023.xlsx", sheet = "Sheet1")

  # Function to check the dataset for a given species file
  get_dataset_info <- function(species_file) {
    # Find the row with the matching "Reference File Name"
    matched_row <- references_data[references_data$`Reference File Name` == species_file, ]

    # Extract the "Data Source" if there's a match
    if (nrow(matched_row) > 0) {
      data_source <- matched_row$`Original Publication DOI`
    } else {
      data_source <- NULL
    }

    list(data_source = data_source)
  }

  # Inside server function
  # Server
  output$dataset_info <- renderText({
    req(input$species2)
    species_file <- paste0(input$species2, ".csv")
    dataset_info <- get_dataset_info(species_file)

    if (!is.null(dataset_info$data_source)) {
      paste("DataSource:", dataset_info$data_source)
    } else {
      "Source not found for the selected species."
    }
  })



  output$references_table <- DT::renderDataTable({
    # Read the Excel file. Adjust the path and sheet name as needed.
    # Render the table with DT::datatable for the UI to display
    DT::datatable(
      references_data,
      options = list(pageLength = 5, autoWidth = TRUE),
      # Style as needed to match your app's theme
      class = "cell-border stripe"
    )
  })
}
