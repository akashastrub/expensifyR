# Personal finances app

# Setup                                                                     ####
# Load required libraries
libraries <- c(
  "dplyr", 
  "readxl",
  "lubridate", 
  "stringr",
  "shinydashboard",
  "shiny",
  "reactable",
  "rhandsontable",
  "plotly"
)

for (l in libraries) {
  suppressPackageStartupMessages(library(l, character.only = TRUE))
}

# Load required functions
import_src_functions <- list.files(
  here::here("src"),
  # ignore potential non .R files, for example, .Rmd
  pattern = "\\.R$", 
  full.names = TRUE,
  recursive = TRUE)

lapply(import_src_functions, source)

# Clean-up
rm(import_src_functions, libraries, l)

# UI                                                                        ####
## Header                                                                   ####
header <- dashboardHeader(title = "expensifyR")

## Sidebar                                                                  ####
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home Page", tabName = "home", icon = icon("house")),
    menuItem("Add new expenses", tabName = "add_new_expenses", icon = icon("plus")),
    menuItem("Analyse past expenses", tabName = "analyse_past_expenses", icon = icon("chart-line"))
  )
)

## Body                                                                     ####
body <- dashboardBody(
  tabItems(
    ### Home Page                                                           ####
    tabItem(tabName = "home",
            h2("Welcome to expensifyR!"),
            fluidRow(
              
              # Explanatory text
              box("The expensifyR app allows expats with expenses across 
              multiple countries, banks, currencies and formats to easily manage
              their expenses. 
              
              
              In order to do add new expenses to your 'master' file, please
              navigate to the 'Add new expenses' tab and follow the instructions. 
              
              
              In order to visualise past expenses, e.g. for budgeting and 
              exploratory purposes, please navigate to the 'Analyse past 
              expenses' tab and upload the relevant 'master' file. 
              
              
              No personal or financial data is collected from the package 
              developers, or app hosting platform. 
              
              
              Please note that as of yet:
              - Users are required to be onboarded by the app developers in order
              to begin using the app (WIP)
              - The following raw bank data uploads are supported: Natwest (GBP),
              DanskeBank (DKK), Revolut (DKK), Revolut (USD), Revolut (GBP), 
              Revolut (EUR). Additional banks can be added should users need.
              
              Please report any issues (bugs and/or requests for additional 
              features [here](https://github.com/akashastrub/expensifyR/issues).",
                  width = 12)
            )
    ),
    ### Addition of new expenses                                            ####
    tabItem(tabName = "add_new_expenses",
            h2("Add new expenses"),
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Load",
                                 box(
                                   # Left hand side only
                                   width = 3,
                                   
                                   # Text 
                  "This is the 'Add new expenses' tab. Select all relevant files 
                  and options that are requested of you on the right hand side 
                  of this page, and click the 'Merge and categorise my new 
                  expenses!' button on the bottom of the panel. This will merge 
                  all new expenses into one file and automatically categorise 
                  these, using past data. Head to the 'Edit' page when you are 
                  notified to do so."),
                                 box(
                                   # Left hand side only
                                   width = 9,
                                   
                                   # Input 1 - master file
                                   fileInput("master_file", 'Select your latest master file'),
                                   
                                   # Input 2 - dictionary file
                                   fileInput("dictionary_file", 'Select your dictionary file'),
                                   
                                   # Input 3 - raw bank data
                                   fileInput("temp_bank_data_files", 
                                             'Select your new bank data files', 
                                             multiple = TRUE
                                   ),
                                   
                                   # Input 4 - currency of choice
                                   selectInput("master_currency", 
                                               "What is your currency of choice?",
                                               c("EUROs" = "eur",
                                                 "US Dollars" = "usd",
                                                 "GB Pounds" = "gbp",
                                                 "Swiss Francs" = "chf")
                                   ),
                                   
                                   # Action button to run script
                                   actionButton("add_new_expenses_button", 
                                                "Merge and categorise my new expenses!",
                                                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                   )
                                 )
                        ),
                        tabPanel("Edit", 

                                 box(
                                   # Left hand side
                                    width = 3,
                                    
                        "Modify the predicted 'subcategory' field in the table 
                        on the right, such that the subcategory matches the 
                        subcategory that you would place the expenses in 
                        manually. Note: right-clicking on rows allows you to 
                        add/remove rows. All cells can be modified, much like an
                        Excel program. Once you are satisfied with the new data,
                        click the 'Save my edits and download new master file!'
                        button at the bottom of the table to download your new 
                        master file (the old one will not be overwritten)."
                        ),
                                 
                                 box(
                                   # Right hand side
                                   width = 9,
                                   
                                   # Modifiable box
                                   rHandsontableOutput("unverified_expenses_table"),
                                   
                                   # Action button to save modifications
                                   downloadButton(
                                     "download_new_master_file", 
                                     "Save my edits and download new master file!",
                                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                 )
                        )
            )
    ),
    
    ### Analysis of past expenses                                           ####
    tabItem(tabName = "analyse_past_expenses",
            h2("Analyse past expenses"),
            fluidRow(
              
              # Analytics text
              box(
                # Left hand side
                width = 3,
                
                # Text
                "This is the 'Analyse past expenses' tab. The purpose of this 
                tab is to allow you to select a specific 'master' file you would 
                like to analyse, and use the interactive graphs to to the right
                to understand your personal finances.",
                
                # Spacer
                h4(""),
                
                # User inputs
                # Input 5 - master file
                fileInput("analytics_master_file", 
                          'Select the master file you wish to analyse'),
                
                # Input 6 - time period desired
                uiOutput("analytics_date_range"),
                
                # Input 7 - categories
                uiOutput("analytics_categories"),
                
                # Input 8 - subcategories
                uiOutput("analytics_subcategories")
              ),
              
              # Modifiable table
              box(
                # Right hand side only
                width = 9,
                
                # Waterfall plotly
                plotlyOutput("waterfall_plot")
              )
            )
    )
  )
)


## Finalise UI                                                              ####
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  # Control notification
  df_new_expenses_classified <- NULL
  
  # When user clicks add_new_expenses_button, merge and categorise expenses.
  # When completed, show user the resulting table.
  observeEvent(input$add_new_expenses_button, {
    
    ### Input, modify, and combine bank data                                ####
    # Location of temp files with new expenses
    path_raw_bank_data_files <- as.character(input$temp_bank_data_files$datapath)
    
    # Create empty dataframe to populate iteratively
    df_temp <- data.frame()
    
    # Bind all data together with same format
    # Loop over all filenames individually
    for (i in seq(1:length(path_raw_bank_data_files))) {
      
      # Extract filename, to be used for relevant information
      filename <- input$temp_bank_data_files$name[i]
      
      # Extract filepath, to be used for import
      filepath <- stringr::str_replace_all(input$temp_bank_data_files$datapath[i],
                                           "\\\\", "/")
      
      # Extract relevant information from filename
      y <- str_split(filename, "_")[[1]]
      bank <- y[1]
      currency <- tolower(y[2])
      
      # Create expression to run bank and currency specific function
      expr <- str_c(str_c("import", bank, sep = "_"),
                    "('",
                    filepath,
                    "', '",
                    currency,
                    "')")
      
      # Run function
      df_temp_addon <- eval(parse(text = expr))
      
      # Convert amounts
      if (nrow(df_temp_addon) > 0) {
        df_temp_addon <- convert_amount(df = df_temp_addon, 
                                        currency_in = currency, 
                                        currency_out = input$master_currency)
        
        # Bind bank-currency specific data to other data
        df_temp <- df_temp %>% bind_rows(df_temp_addon)
      }
    }
    
    # Arrange by date
    df_temp <- df_temp %>% arrange(date)
    
    print(df_temp)
    
    ### Ensure no the new bank data overlaps with data already in master    ####
    
    # Location of master files
    path_latest_master_file <- as.character(input$master_file$datapath)
    
    # Load latest master file
    latest_master_file <- readr::read_csv(path_latest_master_file)
    
    # Filter df_temp to only include expenses that occurred after the latest date
    # on the latest master file
    # TODO: make this better by filtering out rows that have already been processed
    latest_master_file_max_date = latest_master_file %>% summarise(max_date = max(date))
    df_new_expenses <- df_temp %>% filter(date > latest_master_file_max_date[[1]])
    
    print(df_new_expenses)
    
    ### Classify new expenses                                               ####
    # Location of category classifier
    path_category_dict <- as.character(input$dictionary_file$datapath)
    
    # Classify new expenses' subcategories
    df_new_expenses_classified <- classify_subcategories(
      df_new_expenses,
      path_category_dict = path_category_dict,
      latest_master_file = latest_master_file)
    
    print(df_new_expenses_classified)
    
    # Save master currency for column selection in rhandsontable
    amount_var <- str_c("amount", input$master_currency, sep = "_")
    
    # Show user the output on a table
    output$unverified_expenses_table <- renderRHandsontable({
      rhandsontable(
        df_new_expenses_classified %>% 
          mutate(subcategory = as.factor(subcategory)) %>% 
          select(date, description, amount_var, bank, subcategory),
        height = 500) %>%
        hot_cols(colWidths = c(100, 150, 100, 100, 100)) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
      
    })
    
    # Give user a notification if table has appeared
    if(nrow(df_new_expenses_classified) > 1) {
      showNotification(
        "Script ran successfully! Now head to the 'Edit' tab",
        type = "warning", duration = NULL)
    }
    
    ### Save verified new expenses to master upon user click                  ####
    
    # Save edits button
    output$download_new_master_file <- downloadHandler(
      filename = function() {
        str_c('master_', 
              format(Sys.Date(), "%Y%m%d"),
              '.csv', 
              sep = '')
      },
      content = function(file) {
        
        # Convert hands-on-table to R dataframe
        saved_df <- isolate(hot_to_r(input$unverified_expenses_table)) %>% 
          mutate(subcategory = as.character(subcategory)) %>% 
          left_join(df_new_expenses_classified,
                    by = c("date", "description", amount_var, "bank", "subcategory"))
        
        # Load category dictionary
        category_dict <- readr::read_csv(as.character(input$dictionary_file$datapath))
        
        # Load latest master file
        latest_master_file <- readr::read_csv(as.character(input$master_file$datapath))
        
        # Bind new expenses (with category & direction) to previous version of master
        new_master <- rbind(latest_master_file, 
                            saved_df %>% left_join(category_dict, by = "subcategory")) %>% 
          arrange(date) %>% 
          as.data.frame()
        
        # Save file
        readr::write_csv(new_master, file)
        
        # Give user a notification if save has been successful
        # showNotification(
        #   stringr::str_c(
        #     "Edits saved successfully! A new 'master_", 
        #     format(Sys.Date(), "%Y%m%d"),
        #     ".csv'", "file has been saved to the data/masters directory."),
        #     type = "warning", duration = NULL)
      }
    )
    
  })
  
  ### Analytics graphs                                                      ####
  
  # When new file is selected for analytics, add variable filtering UIs
  observeEvent(input$analytics_master_file, {
    
    # Wait for input to run script
    req(input$analytics_master_file)
    
    # Location of file
    path_analytics_master_file <- as.character(input$analytics_master_file$datapath)
    
    # Dates to select from
    min_max_dates <- isolate(readr::read_csv(path_analytics_master_file) %>% 
                               summarise(
                                 min_date = min(date),
                                 max_date = max(date)
                               ))
    min_date <- min_max_dates$min_date
    max_date <- min_max_dates$max_date
    
    # Add date selection - based on data selected
    output$analytics_date_range <- renderUI({
      dateRangeInput("analytics_date_range", 
                     "What range of dates do you want to visualise?",
                     start = min_date,
                     end   = max_date)
    })
    
    # Categories to select from
    categories <- isolate(readr::read_csv(path_analytics_master_file) %>% 
                            distinct(category) %>% 
                            as.list())
    categories <- categories$category
    
    # Add category selection - based on data
    output$analytics_categories <- renderUI({
      selectInput(inputId ="analytics_selected_categories", 
                  label = "Choose which categories to include", 
                  choices = categories,
                  multiple = TRUE,
                  selected = categories
      )
    })
    
    # Subcategories to select from
    subcategories <- isolate(readr::read_csv(path_analytics_master_file) %>% 
                               distinct(subcategory) %>% 
                               as.list())
    subcategories <- subcategories$subcategory
    
    # Add subcategory selection - based on data
    output$analytics_subcategories <- renderUI({
      selectInput(inputId ="analytics_selected_subcategories", 
                  label = "Choose which subcategories to include", 
                  choices = subcategories,
                  multiple = TRUE,
                  selected = subcategories
      )
    })
  })
  
  # Plot graphs
  observe({
    
    # Wait for input to run script
    req(input$analytics_master_file)
    req(input$analytics_date_range)
    
    # Location of file
    path_analytics_master_file <- as.character(input$analytics_master_file$datapath)
    print(path_analytics_master_file)
    
    # Load master file
    analytics_master_file <- reactive(readr::read_csv(path_analytics_master_file))
    
    # Filter master file
    analytics_master_file <- analytics_master_file() %>%
      # Filter to desired date range
      filter(date > input$analytics_date_range[1]) %>%
      filter(date < input$analytics_date_range[2])
    
    # Filter master file to desired categories and subcategories
    analytics_master_file <- analytics_master_file %>%
      filter(category %in% input$analytics_selected_categories) %>% 
      filter(subcategory %in% input$analytics_selected_subcategories)
    
    #### Process master file for waterfall graph                            ####
    
    # Find order of variables for master file
    df_variable_order <- analytics_master_file %>%
      mutate(month = lubridate::month(date)) %>%
      mutate(category = stringr::str_replace(category, ' ', '_')) %>%
      group_by(category) %>%
      summarise(amount = round(sum(amount_eur)/n_distinct(month), 0)) %>%
      ungroup() %>% 
      arrange(desc(amount))
    
    df_variable_order_in <- df_variable_order %>% filter(amount > 0)
    
    df_variable_order_out <- df_variable_order %>% filter(amount <= 0) %>% 
      arrange(amount)
    
    l_variable_order <- df_variable_order_in$category %>% 
      append(df_variable_order_out$category)
    
    print(l_variable_order)
    
    # Plot graph
    df_waterfall <- analytics_master_file %>%
      mutate(month = lubridate::month(date)) %>%
      mutate(category = stringr::str_replace(category, ' ', '_')) %>%
      group_by(category) %>%
      summarise(amount = round(sum(amount_eur)/n_distinct(month), 0)) %>%
      ungroup() %>%
      mutate(category = factor(category,
                               l_variable_order#,
                               # levels = c("in", "accommodation", "mum_payment",
                               #            "food", "holidays", "living", "extras",
                               #            "commuting", "sports", "medical",
                               #            "other", "student_loan"))
      )) %>%
      mutate(measure = "relative") %>%
      mutate(text = case_when(
        amount > 0 ~ stringr::str_c('+', as.character(amount), sep = ''),
        TRUE ~ as.character(amount))
      ) %>%
      rbind(data.frame(category = "Monthly profit after tax",
                       amount = 0,
                       measure = "total",
                       text = "Total")) %>%
      arrange(category,
              l_variable_order %>% append("Monthly profit after tax")#,
              # levels = c("in", "accommodation", "mum_payment",
              #            "food", "holidays", "living", "extras",
              #            "commuting", "sports", "medical",
              #            "other", "student_loan", "Monthly profit after tax")
      )
    
    # Create plot
    fig_waterfall <- plot_ly(
      df_waterfall, name = "20", type = "waterfall", measure = ~ measure,
      x = ~category, textposition = "outside", y= ~amount, text =~text,
      connector = list(line = list(color= "rgb(63, 63, 63)")))
    
    fig_waterfall <- fig_waterfall %>%
      layout(title = "Profit and loss statement",
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             autosize = TRUE,
             showlegend = TRUE)
    
    # Show table
    output$waterfall_plot <- renderPlotly({
      fig_waterfall
    })
  })
  
}

shinyApp(ui, server)
