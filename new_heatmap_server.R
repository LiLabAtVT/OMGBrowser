library(shiny)
library(tidyverse)
library(reshape2)
library(readxl)
library(shinyHeatmaply)
library(plotly)
library(RColorBrewer)
library(DT)
library(shinyalert)
library(openxlsx)

# Function to clean gene names in orthogroups dataset to match species dataset
clean_orthogroups_gene_names <- function(orthogroups, species_name) {
  # Check if the species_name column exists in the orthogroups DataFrame
  if (!species_name %in% colnames(orthogroups)) {
    stop("Species name is not in the orthogroups table")
  } else {
    # Selecting the Orthogroup and species_name columns
    orthogroups <- orthogroups[, c("Orthogroup", species_name)] %>%
      mutate_at(vars(species_name), list(~strsplit(as.character(.), ","))) %>%
      unnest(cols = species_name) %>%
      mutate(across(where(is.character), str_trim)) %>% # Remove white spaces
      dplyr:::distinct(!!sym(species_name), .keep_all = TRUE)
    
    return(orthogroups)
  }
}

calculate_overlaps <- function(Species1, Species2, uploaded_species_name, ref_species_name) {
  clusters_S1 <- unique(Species1$clusterName)
  clusters_S2 <- unique(Species2$clusterName)

  two_plants <- matrix(nrow=length(clusters_S1), ncol=length(clusters_S2))
  for(i in 1:length(clusters_S1)){
    for(j in 1:length(clusters_S2)){
      list_overlap = intersect(Species1[Species1$clusterName == clusters_S1[i],]$Orthogroup, Species2[Species2$clusterName == clusters_S2[j],]$Orthogroup)
      num_overlap = length(list_overlap)
      
      two_plants[i,j] = num_overlap
    }
  }
  rownames(two_plants) = unique(Species1$clusterName)
  colnames(two_plants) = unique(Species2$clusterName) 
  df = two_plants[order(rownames(two_plants)), order(colnames(two_plants))]
  df = as.data.frame(df)
  table_count = as.matrix(df)
  df$cell_type <- rownames(df) # Add column to make 2 variables when using melt function
  print(df)
  # Add a row that sums up the values in all other rows
  library(tidyverse)
  sumrow = df %>% dplyr:::select(-cell_type) %>% colSums() # sum all numeric row in the dataframe
  sum_h = c(sumrow, "sum_h") 
  df = rbind(df, sum_h) # After merging two data frames, datatype in dataframe will be changed into character
  
  library(hablar)
  df = df %>% retype()  # This library and function retype will change the data into the correct type
  
  # Add a column to sum up all values in other columns
  df$sum_v = df %>% dplyr:::select(-cell_type) %>% rowSums() # sum all numeric columns in the dataframe
  rownames(df) <- df$cell_type  
  
  
  p_value_dataframe = df[1: (nrow(df) - 1), 1: (ncol(df) -2)]

  
  conclusionTable_0.01 = p_value_dataframe # Reject the null hypothesis if p-value < 0.05

  for (i in rownames(p_value_dataframe)){
    for (j in colnames(p_value_dataframe)){
      frame = df[c(i, "sum_h"), c(j, "sum_v")]
      frame[2,2] = frame[2,2] - frame[1,2] - frame[2,1] + frame[1,1]
      frame[1,2] = frame[1,2] - frame[1,1]
      frame[2,1] = frame[2,1] - frame[1,1]
      p_value_dataframe[i,j] = fisher.test(frame, alternative = "greater")$p.value
    }
  }
  
  
  adjusted_pvalue_dataframe <- p_value_dataframe
  adjusted_pvalue_dataframe[] = p.adjust(unlist(p_value_dataframe), method = "BH")
  conclusionTable_0.01 = ifelse(adjusted_pvalue_dataframe < 0.01, "Reject", "Fail")
  
  #-----
  df1 = melt(table_count)
  df1$p_value = melt(adjusted_pvalue_dataframe)$value
  df1$negative_log = -log10(df1$p_value)
  df1$transform = ifelse(df1$negative_log > 10, 10, ifelse(df1$negative_log > 3, 3, ifelse(df1$negative_log > 2, 2, ifelse(df1$negative_log > 1.3, 1.3, 0))))
  df1$test = melt(conclusionTable_0.01)$value
  colnames(df1) <- c("Query_clusters", "Reference_clusters", "common_OMGs", "p_value", "negative_log", "transform","test")
  print(df1)
  return(df1)
  
}

create_heatmap_and_overlaps <- function(species1_data, species2_data, orthogroups, species1_name, species2_name) {
  # Attempt to merge and catch any errors
  tryCatch(
    {
      # Clean orthogroups gene names for each species
      orthogroups_species1 <- clean_orthogroups_gene_names(orthogroups, species1_name)
      orthogroups_species2 <- clean_orthogroups_gene_names(orthogroups, species2_name)

      # Merge with orthogroups
      species1_data <- merge(species1_data, orthogroups_species1, by.x = "gene", by.y = species1_name) %>% arrange(clusterName, desc(avg_log2FC)) %>% dplyr:::select("gene", "Orthogroup", "clusterName", "avg_log2FC") %>% group_by(clusterName) %>% top_n(200)
      species2_data <- merge(species2_data, orthogroups_species2, by.x = "gene", by.y = species2_name) %>% arrange(clusterName, desc(avg_log2FC)) %>% dplyr:::select("gene", "Orthogroup", "clusterName", "avg_log2FC") %>% group_by(clusterName) %>% top_n(200)

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
      title = "Incorrect Species / Marker genes mismatch Error",
      text = "Please make sure you selected the right species and uploaded genes match the gene format of our reference genes from the referenc tab",
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
  ref_col_name <- cluster_columns[2]

  # Pivot to matrix format
  overlaps_matrix <- dcast(overlaps,
    overlaps[[query_col_name]] ~ overlaps[[ref_col_name]],
    value.var = "common_OMGs",
    fill = 0
  )

  # Ordering rows consistently
  # overlaps_matrix <- overlaps_matrix[order(overlaps_matrix[[1]]), ]

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
  ##########################
  # Matrix of Annotation
  pvalues_matrix <- dcast(overlaps,
                           overlaps[[query_col_name]] ~ overlaps[[ref_col_name]],
                           value.var = "p_value",
                           fill = 0
  )
  rownames(pvalues_matrix) <- pvalues_matrix[[1]]
  pvalues_matrix <- pvalues_matrix[, -1]
  
  annotated_overlaps_matrix = overlaps_matrix
  for (row in 1:nrow(overlaps_matrix)) {
    for (col in 1:ncol(overlaps_matrix)) {
      if (pvalues_matrix[row, col] < 0.001) {
        annotated_overlaps_matrix[row, col] <- paste0(as.character(overlaps_matrix[row, col]), "**")
      } else if (pvalues_matrix[row, col] < 0.01) {
        annotated_overlaps_matrix[row, col] <- paste0(as.character(overlaps_matrix[row, col]), "*")
      } 
    }
  }
  print(annotated_overlaps_matrix)
  ##########################
  # Matrix of color
  color_matrix <- dcast(overlaps,
                          overlaps[[query_col_name]] ~ overlaps[[ref_col_name]],
                          value.var = "transform",
                          fill = 0
  )
  rownames(color_matrix) <- color_matrix[[1]]
  color_matrix <- color_matrix[, -1]
  print(color_matrix)
  ##########################
  
  heatmap <- heatmaply(
    color_matrix,
    showticklabels = c(TRUE, TRUE),
    show_row_dendrogram = FALSE,
    show_col_dendrogram = FALSE,
    cellnote = annotated_overlaps_matrix,  # Display the overlap values in the cells
    main = " ",
    xlab = paste0("Reference: ", species2_name),
    ylab = paste0("Query: ", species1_name),
    colors = colorRampPalette(brewer.pal(9, "Blues"))(256),
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
  references_data <- readxl::read_excel("www/input_data/TableForReferenceScData_11072023.xlsx", sheet = "Sheet1")
  species_data_path <- "www/input_data/species_data"
  species_files <- list.files(species_data_path)
  species_names <- gsub("\\.csv$", "", species_files)

  #updateSelectInput(session, "species2", choices = species_names)
  reference_file_names <- references_data %>%
    filter(`Reference_Available` == "Yes") %>%
    distinct(`Reference File Name`) %>%
    pull(`Reference File Name`)
  reference_file_names <- gsub("\\.csv$", "", reference_file_names)
  all_supported_species <- unique(references_data$`Species`)

  # Now use the extracted file names to update the selectInput choices
  updateSelectInput(session, "species2", choices = reference_file_names)
  updateSelectInput(session, "selected_species1", choices = all_supported_species)

  # Reactive expression to filter references_data based on selected species and extract Gene Pattern
  selected_gene_pattern <- reactive({
    req(input$selected_species1) # Require that a species has been selected
    # Filter references_data for the selected species
    filtered_data <- references_data[references_data$`Species` == input$selected_species1, ]
    # Return the Gene Pattern value
    if (nrow(filtered_data) > 0) {
      return(filtered_data$`Gene Pattern`[1])
    } else {
      return(NA) # Return NA if the species is not found
    }
  })

  # Output the Gene Pattern to the UI
  output$gene_pattern_output <- renderText({
    gene_pattern <- selected_gene_pattern() # Get the reactive value
    if (is.na(gene_pattern)) {
      return("Gene Pattern not found for the selected species.")
    } else {
      return(gene_pattern)
    }
  })


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

    # Ensure the species selected is from the dropdown and the file is uploaded
    if (is.null(input$selected_species1) || is.null(input$file1)) {
      shinyalert::shinyalert(
        title = "Missing Information",
        text = "Please make sure you've uploaded a file and selected a species.",
        type = "error"
      )
      return(NULL)
    }

    uploaded_species_name <- input$selected_species1
  
    # Extract the species names
    selected_species2_name <- sub("(__.*|_[0-9].*)$", "", input$species2)

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

    species2_data <- read.csv(file.path(species_data_path, paste0(input$species2, ".csv")))
    
    orthogroups <- read.delim("www/input_data/Orthogroups_website.tsv", header = TRUE, sep = "\t")

    # Create the heatmap and overlaps data
    create_heatmap_and_overlaps(species1_data, species2_data, orthogroups, uploaded_species_name, selected_species2_name)
  }

  values <- reactiveValues(
    processed_data = NULL,
    comparison_table = NULL,
    common_genes_data = NULL,
    first_page = NULL
  )

  #This will get trigerred
  observe({
    processed_data <- req(process_data())
    values$processed_data <- processed_data
  })

  output$new_heatmap_plot <- renderPlotly({
    req(values$processed_data)
    p <- values$processed_data$heatmap
    event_register(p, 'plotly_click')
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
    values$processed_data$overlaps[, 2] <- as.character(values$processed_data$overlaps[, 2])

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
          "table.columns(1).search(this.value).draw();",
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

    common_genes_data <- populate_new_table(as.character(selected_row[1]), as.character(selected_row[2]), species1_data, species2_data)
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
    species1_data <- Species1[as.character(Species1$clusterName) == uploaded_species_cluster, ]
    species1_data$Species = "Query"
    species1_data$clusterName <- as.character(species1_data$clusterName)
    
    species2_data <- Species2[as.character(Species2$clusterName) == ref_cluster, ]
    species2_data$Species = "Reference"
    
    # Find the common OGs between the two species
    common_OGs <- intersect(species1_data$Orthogroup, species2_data$Orthogroup)
    
    # Check if there are no common OGs
    if (length(common_OGs) == 0) {
      return(data.frame()) # Return an empty data frame
    }

    # Filter the genes associated with the common OGs, select the one with highest avg_log2FC, and drop the avg_log2FC column
    species1_genes <- species1_data %>%
      filter(Orthogroup %in% common_OGs) 

    species2_genes <- species2_data %>%
      filter(Orthogroup %in% common_OGs) 

    # Merge the genes from both species and select certain columns from .x and .y
    common_genes <- rbind(species1_genes, species2_genes)
    return(common_genes)
  }

  output$downloadButtons <- renderUI({
    # Get a list of files in the sample_data folder
    files <- list.files(path = "www/sample_data", full.names = FALSE)

    # Create a download button for each file
    buttons <- lapply(files, function(file) {
      downloadButton(paste0("download_", gsub("\\.", "_", file)), file)
    })

    do.call(tagList, buttons)
  })

  # Create a download handler for each file in the sample_data folder
  observe({
    files <- list.files(path = "www/sample_data", full.names = FALSE)

    lapply(files, function(file) {
      outputId <- paste0("download_", gsub("\\.", "_", file))

      output[[outputId]] <- downloadHandler(
        filename = function() {
          file
        },
        content = function(fileContent) {
          file.copy(paste0("www/sample_data/", file), fileContent)
        }
      )
    })
  })

  # Function to check the dataset for a given species file
  get_dataset_info <- function(species_file) {
    # Find the row with the matching "Reference File Name"
    matched_row <- references_data[references_data$`Reference File Name` == species_file, ]

    # Extract the "Original Publication DOI" if there's a match
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
      paste( dataset_info$data_source, "\n")
    } else {
      "Source not found for the selected species."
    }
  })

  output$references_table <- DT::renderDataTable({

  # Create a new 'Download' column with hyperlinks for each row
  references_data$Download <- sapply(references_data$`Reference File Name`, function(filename) {
    # Construct the download URL for each file
    file_url <- paste0("input_data/species_data/",filename)
    # Create a hyperlink HTML tag as a character string
    as.character(shiny::a("Download", href = file_url, download = filename, target = "_blank"))
  })
  
  # Render the modified data frame as a DataTable
  DT::datatable(
    references_data,
    escape = FALSE, # IMPORTANT: Prevent escaping HTML content
    options = list(pageLength = 5, autoWidth = TRUE),
    class = "cell-border stripe"
  )
}, server = FALSE) # IMPORTANT: server-side processing must be set to FALSE for escape=FALSE to work
}

# testing from new mac
# end of the code