#libraries 
library(Rmisc)  
library(stringi)
library(stringr)
library(gtools)
library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(zoo)
library(caTools) 
library(lubridate)
library(grid)
library(gridExtra)
library(webshot)
library(htmlwidgets)
library(timevis)
library(tibble)
library(ggpubr)
library(LambertW)
library(GFE)
library(nnet)
library(LaplacesDemon)
library(seqHMM)
library(RColorBrewer)
library(alluvial)
library(Rcpp)
library(pracma)
library(tidyverse)
library(vctrs)

library(shiny)
library(bslib)
library(DT)
#library(shinylive)
#library(httpuv)

setwd("~/Library/CloudStorage/OneDrive-UniversityofWyoming/Work/R/ShinyApp/ShinyTest1")
getwd()


#Improvement: Only display columns of correct column type for graph axis
#Improvement: Keep selection in drop down menus when switching files
#Improvement: Have option to merge data sets

#Increase possible file upload size
options(shiny.maxRequestSize=500*1024^2)

#Create UI
ui <- fluidPage(
  #Design templates
  tags$head(
    tags$style(HTML("
      .dt-output {
        height: 25vh; /* Takes up the top quarter of the screen */
        overflow-y: auto; /* Allow scrolling if content overflows */
      }
      .grid-container {
        display: grid;
        grid-template-columns: repeat(2, 1fr); /* 2 columns */
        grid-gap: 20px; /* Space between grid items */
        margin-top: 20px; /* Space above the grid */
      }
      .grid-item {
        padding: 10px;
        border: 1px solid #ccc; /* Optional: border for clarity */
        background-color: #f9f9f9; /* Optional: background color */
      }
      .dynamic-plot {
        grid-column: span 2; /* Make plot take full width */
        height: 60vh; /* Adjust height as needed */
      }
      
      .grid-container2 {
        display: grid;
        grid-template-columns: repeat(2, 1fr); /* 2 columns */
        grid-template-rows: repeat(2, 1fr); /* 2 rows */
        height: calc(100vh - 56px - 20px); /* Adjust height for navbar and margin */
        width: calc(100% - 40px); /* Full width minus side margins */
        max-width: 1200px; /* Set a maximum width for the grid */
        margin: 20px auto; /* Center the grid and add vertical margins */
        position: relative; /* Position for overlay */
        padding: 10px; /* Optional: Inner padding for aesthetics */
      }
      .grid-item2 {
        width: 100%;
        height: 100%; /* Full height */
        background-size: cover; /* Cover the entire area */
        background-position: center; /* Center the image */
      }
      .overlay-card2 {
        position: absolute; /* Position it in relation to the grid */
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%); /* Center the card */
        background-color: rgba(255, 255, 255, 0.8); /* Semi-transparent background */
        padding: 20px;
        border-radius: 8px; /* Rounded corners */
        text-align: center; /* Center the text */
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); /* Shadow for depth */
      }
      .dt-output3 {
      height: 75vh; /* Set the height for scrolling */
      overflow-y: auto; /* Allow vertical scrolling */
      }
    .custom-datatable tbody tr {
        height: 10px; /* Set a smaller height for table rows */
      }
      .custom-datatable tbody td {
        max-width: 150px; /* Set max width for cells */
        overflow: hidden; /* Hide overflow */
        white-space: nowrap; /* Prevent line breaks */
        text-overflow: ellipsis; /* Add ellipsis */
        height: 10px; /* Set fixed height for cells */
      }
    "))
  ),
  page_navbar(
    id = "pages",
    title = "QuickLook",
    bg = "#2D89C8",
    inverse = TRUE,
    nav_panel(title = "Home", #Make homepage
              div(class = "grid-container2",
                  div(class = "grid-item2", 
                      img(src = "chipmunk.png", style = "width: 100%; height: 100%;")),
                  div(class = "grid-item2", 
                      img(src = "mouse.png", style = "width: 100%; height: 100%;")),
                  div(class = "grid-item2", 
                      img(src = "mouse2.png", style = "width: 100%; height: 100%;")),
                  div(class = "grid-item2", 
                      img(src = "chipmunk2.png", style = "width: 100%; height: 100%;")),
                  div(class = "overlay-card2",
                      h3("Welcome to QuickLook!"),
                      p("Click the tabs to take a peek at your data")
                  )
              )),
    nav_panel(title = "Graph", #Make graphing page
              #fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
              #div(class = "dt-output", DT::DTOutput("files")),
              
              # Grid Layout
              div(class = "grid-container",
                  div(class = "grid-item", uiOutput("GGPick")),
                  div(class = "grid-item", 
                      div(class = "row",
                          div(class = "col", uiOutput("column_selector_x")),
                          div(class = "col", uiOutput("column_selector_y"))
                      )
                  ),
                  div(class = "grid-item", 
                      uiOutput("LD1"), 
                      uiOutput("bins1"), 
                      uiOutput("linreg")  # These occupy the same space
                  ),
                  div(class = "grid-item", 
                      uiOutput("LD2"), 
                      uiOutput("LD3")   # These occupy the same space
                  ),
                  div(class = "grid-item dynamic-plot", plotOutput("dynamic_plot")), 
                  downloadButton("save_plot", "Save Plot", class = "btn btn-primary")
                  #div(class = "grid-item dynamic-plot", plotOutput("dynamic_plot")) # Single plot output
              )
    )
    ,
    nav_panel(title = "Analyze", #Make analysis page
              # Grid Layout for Analyze
              #fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
              #div(class = "dt-output", DT::DTOutput("files")),
              
              # Grid Layout
              div(class = "grid-container",
                  div(class = "grid-item", uiOutput("column_selector")),
                  div(class = "grid-item", textOutput("print")),
                  div(class = "grid-item dynamic-plot", DT::DTOutput("statistics")) # Single plot output
              )
    ),
    nav_panel(title = "Data Frame", #Make data frame page
              #Display data frame
              div(class = "dt-output3", 
                  DT::dataTableOutput("data_table"))
    ), 
    nav_spacer(),
    nav_menu(
      title = "Links", #Add links to lab
      align = "right",
      nav_item(tags$a("Nelson Lab", href = "https://www.nelsonlabuwyoming.com")),
      nav_item(tags$a("Bedford Lab", href = "https://thebedfordlab.com"))
    )
  ), 
  uiOutput("conditional_ui")
)


#Create server
server <- function(input, output, session) { 
  
  # Read all the uploaded files 
  all_files <- reactive({ 
    req(input$upload) 
    if (input$pages != "Home") { 
      purrr::map(input$upload$datapath, read_csv) %>% 
        purrr::set_names(input$upload$name) 
    }
  }) 
  
  # Render the uploaded files as a DataTable 
  output$files <- DT::renderDT({ 
    req(all_files())
    DT::datatable(input$upload, selection = 'single') 
  }) 
  
  #Make conditional upload button and data table
  output$conditional_ui <- renderUI({
    print(input$pages)
    if (input$pages != "Home") {
      tagList(
        fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
        div(class = "dt-output", DT::DTOutput("files"))
      )
    }
  })
  
  output$data_table <- DT::renderDataTable({
    req(all_files())  # Ensure files are uploaded
    req(input$files_rows_selected)  # Ensure a file is selected
    selected_file <- all_files()[[input$files_rows_selected]]  # Get the selected data frame
    DT::datatable(selected_file, options = list(
      autoWidth = TRUE,
      paging = TRUE,
      searching = TRUE, 
      dom = 't'
    ))  # Render it as a DataTable
  })
  
  #Make graph selection
  output$GGPick <- renderUI({ 
    req(input$upload) 
    req(input$files_rows_selected) 
    selected_file <- all_files()[[input$files_rows_selected]] 
    if (input$pages == "Graph") { 
      radioButtons("Graphpick", "Select plot type...", 
                   choiceNames = c("Ethogram", "Histogram", "Scatter Plot"), 
                   choiceValues = c("Ethogram", "Histogram", "Scatter_Plot")) 
    } 
  }) 
  
  #Column selection x
  output$column_selector_x <- renderUI({ 
    req(input$upload) 
    req(input$files_rows_selected) 
    selected_file <- all_files()[[input$files_rows_selected]] 
    if (input$pages == "Graph") { 
      selectInput("x", "Select x variable...", choices = sort(colnames(selected_file))) 
    } 
  }) 
  
  #Column selection y
  output$column_selector_y <- renderUI({ 
    req(input$upload) 
    req(input$files_rows_selected) 
    selected_file <- all_files()[[input$files_rows_selected]] 
    #req(input$Graphpick) 
    if (input$pages == "Graph") { 
      if (grepl("Histogram", input$Graphpick) == FALSE) { 
        selectInput("y", "Select y variable...", choices = sort(colnames(selected_file))) 
      } 
    }
  }) 
  
  #Column selection unspecified for analysis
  output$column_selector <- renderUI({ 
    req(input$upload) 
    req(input$files_rows_selected) 
    selected_file <- all_files()[[input$files_rows_selected]] 
    if (input$pages == "Analyze") { 
      selectInput("y", "Select column...", choices = sort(colnames(selected_file)))  
    }
  }) 
  
  #Shading selections
  output$LD1 <- renderUI({ 
    req(input$upload) 
    req(input$files_rows_selected) 
    req(input$Graphpick) 
    selected_file <- all_files()[[input$files_rows_selected]] 
    if (input$pages == "Graph") { 
      if (grepl("Ethogram", input$Graphpick) == TRUE) { 
        checkboxInput("LDcheck", "Shade", value = FALSE) 
      } 
    } 
  }) 
  
  output$LD2 <- renderUI({ 
    req(input$LDcheck) 
    req(input$Graphpick) 
    if (grepl("Ethogram", input$Graphpick) == TRUE) { 
      if(input$LDcheck == TRUE) { 
        req(input$upload) 
        req(input$files_rows_selected) 
        selected_file <- all_files()[[input$files_rows_selected]] 
        selectInput("WhichLD", "Select shade column...", choices = sort(colnames(selected_file))) 
      } 
    } 
  }) 
  
  output$LD3 <- renderUI({ 
    req(input$WhichLD) 
    req(input$Graphpick) 
    if (grepl("Ethogram", input$Graphpick) == TRUE) { 
      if(input$LDcheck == TRUE) { 
        selected_file <- all_files()[[input$files_rows_selected]] 
        dark_values <- unique(selected_file[[input$WhichLD]]) 
        radioButtons("DefineLD", "Select shade area...", choiceNames = dark_values, choiceValues = dark_values) 
      } 
    } 
  }) 
  
  #Print unique values in character columns
  output$print <- renderText({ 
    req(input$upload) 
    req(input$files_rows_selected) 
    selected_file <- all_files()[[input$files_rows_selected]] 
    if (input$pages == "Analyze") { 
      if (is.character(selected_file[[input$y]]) == TRUE) { 
        unique_values <- sort(unique(selected_file[[input$y]])) 
        paste("Unique values in column", input$y,":", paste(unique_values, collapse = ", ")) 
      } 
    } 
  }) 
  
  #Generate statistics for numeric columns
  output$statistics <- DT::renderDT({ 
    req(input$upload) 
    req(input$files_rows_selected) 
    req(input$y) 
    selected_file <- all_files()[[input$files_rows_selected]] 
    if (input$pages == "Analyze") { 
      if (is.numeric(selected_file[[input$y]]) == TRUE) { 
        NoNA <- na.omit(selected_file[[input$y]]) 
        meany <- mean(NoNA) 
        mediany <- median(NoNA)
        maxy <- max(NoNA) 
        miny <- min(NoNA) 
        sdy <- sd(NoNA)
        modey <- Mode(NoNA)
        stats <- matrix(c("Mean", meany, "Median", mediany, "Mode", modey, "Max", maxy, "Min", miny, "SD", sdy), nrow = 6, ncol = 2, byrow = TRUE) 
        colnames(stats) <- c("Statistic", "Value") 
        stats_df <- as.data.frame(stats, stringsAsFactors = FALSE) 
        DT::datatable(stats_df, selection = 'none') 
      } 
    } 
  }) 
  
  #Generate ethogram, histogram, and scatter plot conditionally
  generate_plot <- function() {
    req(input$upload)
    req(input$files_rows_selected)
    req(input$x)
    
    selected_file <- all_files()[[input$files_rows_selected]]
    
    if (grepl("Ethogram", input$Graphpick)) {
      req(input$y)
      aes_string_x <- sym(input$x)
      aes_string_y <- sym(input$y)
      g <- ggplot(data = selected_file, aes(x = !!aes_string_x, y = !!aes_string_y))
      if (input$LDcheck) {
        g <- g + geom_vline(data = selected_file[selected_file[[input$WhichLD]] %in% input$DefineLD, ], aes(xintercept = !!aes_string_x), color = "grey80", linewidth = 0.5)
      }
      g <- g + geom_tile(aes(fill = !!aes_string_y)) +
        scale_y_discrete(expand = c(0, 0)) +
        labs(x = input$x, y = input$y) +
        ggtitle(paste("Ethogram of", input$y)) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position = "none")
      return(g)
    } else if (grepl("Histogram", input$Graphpick)) {
      req(input$bins)
      hist_data <- na.omit(selected_file[[input$x]])
      bins <- seq(min(hist_data), max(hist_data), length.out = input$bins + 1)
      g <- ggplot(data = data.frame(x = hist_data), aes(x = x)) +
        geom_histogram(breaks = bins, fill = "purple", color = "white") +
        labs(x = input$x, y = "Count") +
        ggtitle(paste("Histogram of", input$x))
      return(g)
    } else if (grepl("Scatter_Plot", input$Graphpick)) {
      req(input$y)
      aes_string_y <- sym(input$y)
      g <- ggplot(data = selected_file, aes(x = !!sym(input$x), y = !!aes_string_y)) +
        geom_point() +
        labs(x = input$x, y = input$y) +
        ggtitle(paste("Scatter plot of", input$x, "vs", input$y)) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      if (input$Regcheck) {
        g <- g + geom_smooth(method = "lm", formula = y ~ x)
      }
      return(g)
    }
  }
  
  #Create bins for histogram
  output$bins1 <- renderUI({ 
    req(input$upload) 
    req(input$files_rows_selected) 
    req(input$x) 
    if (input$pages == "Graph") { 
      if (grepl("Histogram", input$Graphpick) == TRUE) { 
        sliderInput( 
          inputId = "bins", 
          label = "Number of bins:", 
          min = 1, 
          max = 50, 
          value = 30 
        ) 
      } 
    } 
  }) 
  
  #Create linear regression for scatter plot
  output$linreg <- renderUI({ 
    req(input$upload) 
    req(input$files_rows_selected) 
    req(input$x) 
    req(input$y) 
    if (grepl("Scatter_Plot", input$Graphpick) == TRUE) { 
      checkboxInput("Regcheck", "Linear Regression", value = FALSE) 
    } 
  }) 
  
  #Render the plot depending on selection (see generate conditional plots above)
  output$dynamic_plot <- renderPlot({
    generate_plot()
  })
  
  #Save plot button
  output$save_plot <- downloadHandler(
    filename = function() { paste("plot_",Sys.time(), ".png", sep = "") },
    content = function(file) {
      g <- generate_plot()  # Call the function to generate the plot
      ggsave(file, plot = g, device = "png")
    }
  )
  
}

#Generate app
shinyApp(ui, server)

