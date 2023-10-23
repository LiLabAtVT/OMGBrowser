# OMG Browser

## Introduction:

OMG Browser is designed for biologists, genomics researchers, and data scientists interested in annotating and exploring single-cell RNA-seq data across various species. This tool, equipped with a user-friendly interface, aids in the visualization, analysis, and comparison of gene expression patterns and cell clusters.

### Quick Start Guide:

1. **New Heatmap Comparison:**
   - Navigate to the "New Heatmap Comparison" tab.
   - Upload your unannotated single-cell RNA-seq data in CSV format.
   - Choose a reference species from the dropdown list.
   - Explore the generated heatmap and table for common orthogroups.
   - Download the comparison table and gene information for further analysis.
2. **Download Sample Files:**
   - Visit this tab to download sample files for practice and exploration of the OMG Browser’s capabilities.
3. **Contact Us:**
   - Reach out through the provided contact emails on the "Introduction" tab for support, feature requests, or contributions.

### Tables and Common Orthogroups (OGs):

In the "New Heatmap Comparison" tab, after uploading the RNA-seq data and selecting a reference species, users will be presented with a heatmap and a table of common orthogroups (OGs). This table is a powerful resource for in-depth analysis.

- **Exploring the Table:**
  The table lists common OGs, providing detailed insights into the shared gene groups among different species. Each row represents an orthogroup, with corresponding data on gene counts and other relevant information.
- **Download Options:**
  Users can download the entire comparison table or select specific rows to obtain gene information pertinent to their study. This feature allows for a tailored analysis, focusing on specific genes or orthogroups of interest.
- **More Information:**
  By selecting a row in the table, users gain access to additional gene information. This detailed view aids in understanding the genes within each orthogroup, their expressions, and potential roles in biological processes.

---

## For Team Members:

### Project Structure:

The application’s structure is primarily based on key R scripts and a dedicated data folder:

1. **UI.R:** Houses the user interface code of the application, outlining its aesthetic and layout.
2. **Server.R:** Handles the server-side logic, connecting the UI with reactive and dynamic functionalities.
3. **New_Heatmap_Server.R:** Specific to the New Heatmap Comparison feature, containing the necessary server functions.
4. **App.R:** The main application script that initiates the Shiny application.
5. **Input_Data Folder/Species_Data:** This folder should contain data for each supported species. Ensure new species data is compatible with existing data structures.

### Supporting New Species:

To add a new species:

- Insert the species data into the `species_data` folder.
- Make sure the "species" column is included in the `orthogroups.tsv` file in the `input_data` folder.
- Follow the same format for marker genes as existing species data.

### Executing the App:

Running the application is straightforward. Ensure that all required libraries are installed, and then execute the `app.R` script to launch the OMG Browser.

```r
#load shiny library
library(shiny)

# Execute the app
runApp("app.R")
```

### Contribution:

Maintain the existing code and data structure standards when contributing to the project.

---
