#' Spin up expensifyR app
#'
#' @return Shiny app
#' @import dplyr readxl lubridate stringr shinydashboard shiny reactable rhandsontable plotly readr
#' @export
#'
run_app <- function() {

  # UI                                                                      ####
  ui <- shinydashboard::dashboardPage(
    ## Header                                                               ####
    header = shinydashboard::dashboardHeader(title = shiny::icon("house")),
    ## Sidebar                                                              ####
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Home Page", tabName = "home", icon = shiny::icon("house")),
        shinydashboard::menuItem("Add new expenses", tabName = "add_new_expenses", icon = shiny::icon("plus")),
        shinydashboard::menuItem("Analyse expenses", tabName = "analyse_expenses", icon = shiny::icon("chart-line")),
        shinydashboard::menuItem("Update balances", tabName = "update_balances", icon = shiny::icon("plus")),
        shinydashboard::menuItem("Analyse balances", tabName = "analyse_balances", icon = shiny::icon("chart-line"))

      )
    ),
    ## Body                                                                 ####
    body = shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        ### Home Page tab                                                   ####
        shinydashboard::tabItem(tabName = "home",
                                shiny::h2("Welcome to expensifyR!"),
                                shiny::fluidRow(

                                  # Explanatory text
                                  shinydashboard::box(
                shiny::HTML("The expensifyR app allows expats with expenses across
                multiple countries, banks, currencies and formats to easily manage
                their expenses. <br/>
                <br/>

                In order to add new expenses to your 'master' file, please
                navigate to the 'Add new expenses' tab and follow the instructions. <br/>
                <br/>

                In order to visualise past expenses, e.g. for budgeting and
                exploratory purposes, please navigate to the 'Analyse past
                expenses' tab and upload the relevant 'master' file. <br/>
                <br/>

                Please note that as of yet: <br/>
                - Users are required to be onboarded by the app developers in order
                to begin using the app (WIP) <br/>
                - The following raw bank data uploads are supported: Natwest (GBP),
                DanskeBank (DKK), Revolut (DKK), Revolut (USD), Revolut (GBP),
                Revolut (EUR). Additional banks can be added should users need.<br/>
                <br/>

                Please report any issues (bugs and/or requests for additional
                features) <a href='https://github.com/akashastrub/expensifyR/issues'> here.</a>"),
                width = 12)
                                )
        ),
        ### Add new expenses tab                                            ####
        shinydashboard::tabItem(
          tabName = "add_new_expenses",
          shiny::h2("Add new expenses"),
        #### Load data & set inputs                                         ####
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("Load",
                                     shinydashboard::box(
                                       shiny::HTML(
                                         # Text
                                         "This is the 'Add new expenses' tab. <br/>
                    <br/>

                    Select all relevant files and options that are requested
                    on this page, and click the
                    'Merge and categorise my new expenses!' button on the bottom
                    of the panel. This will merge all new expenses into one file and automatically categorise
                    these, using past data. <br/>
                    <br/>

                    Head to the 'Edit' page when you are notified to do so."),
                    # Left hand side only
                    width = 3),
                    shinydashboard::box(
                      # Left hand side only
                      width = 9,

                      # Input 1 - master file
                      shiny::fileInput("master_file", 'Select your latest master file'),

                      # Input 2 - dictionary file
                      shiny::fileInput("dictionary_file", 'Select your dictionary file'),

                      # Input 3 - personal raw bank data
                      shiny::fileInput("temp_personal_bank_data_files",
                                       'Select your new personal bank data files',
                                       multiple = TRUE),

                      # Input 4 - shared raw bank data
                      shiny::fileInput("temp_shared_bank_data_files",
                                       'Select your new shared bank data files',
                                       multiple = TRUE),

                      # Input 5 - shared raw bank data multiplier
                      shiny::numericInput("shared_perc",
                                          'Select the percentage of shared expenses you paid for',
                                          value = 60, min = 1, max = 99, step = 1),

                      # Input 6 - currency of choice
                      shiny::selectInput("master_currency_new_expenses",
                                         "What is your currency of choice?",
                                         c("US Dollars" = "usd",
                                           "EUROs" = "eur",
                                           "GB Pounds" = "gbp",
                                           "Swiss Francs" = "chf")
                      ),

                      # Action button to run script
                      shiny::actionButton("add_new_expenses_button",
                                          "Merge and categorise my new expenses!",
                                          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                      )
                    )
            ),
            #### Editing of new expenses                                    ####
            shiny::tabPanel("Edit",

                                     shinydashboard::box(

                                       shiny::HTML(
                        "Modify the predicted 'subcategory' field in the table
                        on the right, such that the subcategory matches the
                        subcategory that you would place the expenses in
                        manually. <br/>
                        <br/>

                        Note: right-clicking on cells allows you to
                        add or remove rows & columns. All cells can be modified, much like an
                        Excel sheet. <br/>
                        <br/>

                        Once you are satisfied with the new data,
                        click the 'Save my edits and download new master file!'
                        button at the bottom of the table to download your new
                        master file (the old one will not be overwritten)."),
                        # Left hand side
                        width = 3
                                     ),

                        shinydashboard::box(
                          # Right hand side
                          width = 9,

                          # Modifiable box
                          rhandsontable::rHandsontableOutput("unverified_expenses_table"),

                          # Action button to save modifications
                          shiny::downloadButton(
                            "download_new_master_file",
                            "Save my edits and download new master file!",
                            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        )
            )
          )
        ),

        ### Analysis of past expenses tab                                   ####
        shinydashboard::tabItem(tabName = "analyse_expenses",
                                shiny::h2("Analyse expenses"),
                                shiny::fluidRow(

                                  # Analytics text
                                  shinydashboard::box(

                                    shiny::HTML("This is the 'Analyse expenses' tab. <br/>
                <br/>

                The purpose of this tab is to allow you to select a specific
                'master' file you would like to analyse, and use the interactive
                graphs to explore your personal finances. <br/>
                <br/>"),

                # Left hand side
                width = 3,

                # User inputs
                # Input 5 - master file
                shiny::fileInput("analytics_master_file",
                                 'Select the master file you wish to analyse'),

                # Input 6 - currency of choice
                shiny::selectInput("master_currency_analytics",
                                   "What is your currency of choice?",


                                   c("US Dollars" = "usd",
                                     "EUROs" = "eur",
                                     "GB Pounds" = "gbp",
                                     "Swiss Francs" = "chf")
                ),

                # Input 7 - time period desired
                shiny::uiOutput("analytics_date_range"),

                # Input 8 - categories
                shiny::uiOutput("analytics_categories"),

                # Input 9 - subcategories
                shiny::uiOutput("analytics_subcategories")
                                  ),

                # Modifiable table
                shinydashboard::box(
                  # Right hand side only
                  width = 9,

                  # Waterfall plotly
                  plotly::plotlyOutput("waterfall_plot")
                )
                                )
        ),

        ### Update balances tab                                             ####
        shinydashboard::tabItem(
          tabName = "update_balances",
          shiny::h2("Update balances"),
          shiny::fluidRow(

          # Update balances info
          shinydashboard::box(
            shiny::HTML(
              "This is the 'Update balances' tab. <br/>
              <br/>

              The purpose of this tab is to allow you to load and edit
              your balances.<br/>
              <br/>"),

            # Left hand side
            width = 3,

            # User inputs balances file
            shiny::fileInput("balances_file",
                             'Select the balances file you wish to edit')
            ),

            shinydashboard::box(
              # Right hand side
              width = 9,

              # Modifiable box
              rhandsontable::rHandsontableOutput("balances_table"),

              # Action button to save modifications
              shiny::downloadButton(
                "download_new_balances_file",
                "Save my edits and download new balances file!",
                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      )
            )
          ),

        ### Analysis of balances tab                                        ####
        shinydashboard::tabItem(tabName = "analyse_balances",
                                shiny::h2("Analyse balances"),
                                shiny::fluidRow(

                                  # Analytics text
                                  shinydashboard::box(

                                    shiny::HTML("This is the 'Analyse balances' tab. <br/>
                <br/>

                Here is some text describing it. <br/>
                <br/>"),

                # Left hand side
                width = 3,

                # User inputs
                # Input 5 - master file
                shiny::fileInput("analytics_balances_file",
                                 'Select the balances file you wish to analyse'),

                # Input 6 - currency of choice
                shiny::selectInput("master_currency_analytics",
                                   "What is the currency of this file?",


                                   c("US Dollars" = "usd",
                                     "EUROs" = "eur",
                                     "GB Pounds" = "gbp",
                                     "Swiss Francs" = "chf")
                ),

                # Input 7 - time period desired
                shiny::uiOutput("analytics_balances_date_range"),

                # Input 8 - categories
                shiny::uiOutput("analytics_balances_accounts")
                                  ),

                # Modifiable table
                shinydashboard::box(
                  # Right hand side only
                  width = 9,

                  # Waterfall plotly
                  plotly::plotlyOutput("balances_plot")
                )
                                )
        )
      )
    ),
    # Skin
    skin = "black")

  # Server                                                                  ####
  server <- function(input, output, session) {

    # Control notification
    df_new_expenses_classified <- NULL

    ## Add new expenses tab                                                 ####
    ### Loop through new expenses & convert to common data model            ####
    # When user clicks add_new_expenses_button, merge new expenses.
    shiny::observeEvent(input$add_new_expenses_button, {

      #### Personal new bank files                                          ####
      # Location of temp files with new personal expenses
      path_raw_personal_bank_data_files <- as.character(input$temp_personal_bank_data_files$datapath)

      # Create empty dataframe to populate iteratively
      df_temp_personal <- data.frame()

      # Bind all personal data together with the same format
      # Loop over all filenames individually
      for (i in seq(1:length(path_raw_personal_bank_data_files))) {

        # Extract filename, to be used for relevant information
        filename <- input$temp_personal_bank_data_files$name[i]

        # Extract filepath, to be used for import
        filepath <- stringr::str_replace_all(input$temp_personal_bank_data_files$datapath[i],
                                             "\\\\", "/")

        # Extract relevant information from filename
        y <- stringr::str_split(filename, "_")[[1]]
        bank <- y[1]
        if (bank == "boa") {
          bank <- stringr::str_c(y[1], "_", y[2])
          currency <- tolower(y[3])
        } else {
          currency <- tolower(y[2])
        }

        # Create expression to run bank and currency specific function
        expr <- stringr::str_c(stringr::str_c("expensifyR::import", bank, sep = "_"),
                               "('",
                               filepath,
                               "', '",
                               currency,
                               "')")

        # Run function
        df_temp_personal_addon <- eval(parse(text = expr))

        # Convert amounts, if rows present and currencies not aligned
        if (nrow(df_temp_personal_addon) > 0) {
          if (currency != input$master_currency_new_expenses) {
            df_temp_personal_addon <- expensifyR::convert_amount(
              df = df_temp_personal_addon,
              currency_in = currency,
              currency_out = input$master_currency_new_expenses)
          }

          # Bind bank-currency specific data to other new data
          df_temp_personal <- dplyr::bind_rows(df_temp_personal, df_temp_personal_addon)
        }

        #### Shared new bank files                                          ####
        # Location of temp files with new personal expenses
        path_raw_shared_bank_data_files <- as.character(input$temp_shared_bank_data_files$datapath)

        # Create empty dataframe to populate iteratively
        df_temp_shared <- data.frame()

        # Bind all personal data together with the same format
        # Loop over all filenames individually
        for (i in seq(1:length(path_raw_shared_bank_data_files))) {

          # Extract filename, to be used for relevant information
          filename <- input$temp_shared_bank_data_files$name[i]

          # Extract filepath, to be used for import
          filepath <- stringr::str_replace_all(input$temp_shared_bank_data_files$datapath[i],
                                               "\\\\", "/")

          # Extract relevant information from filename
          y <- stringr::str_split(filename, "_")[[1]]
          bank <- y[1]
          if (bank == "boa") {
            bank <- stringr::str_c(y[1], "_", y[2])
            currency <- tolower(y[3])
          } else {
            currency <- tolower(y[2])
          }

          # Create expression to run bank and currency specific function
          expr <- stringr::str_c(stringr::str_c("expensifyR::import", bank, sep = "_"),
                                 "('",
                                 filepath,
                                 "', '",
                                 currency,
                                 "')")

          # Run function
          df_temp_shared_addon <- eval(parse(text = expr))

          # Convert amounts, if rows present and currencies not aligned
          if (nrow(df_temp_shared_addon) > 0) {
            if (currency != input$master_currency_new_expenses) {
              df_temp_shared_addon <- expensifyR::convert_amount(
                df = df_temp_shared_addon,
                currency_in = currency,
                currency_out = input$master_currency_new_expenses)
            }

            # Bind bank-currency specific data to other new data
            df_temp_shared <- dplyr::bind_rows(df_temp_shared, df_temp_shared_addon)
          }
          browser()

          # Multiply by user-inputted percentage
          df_temp_shared <- df_temp_shared |>
            dplyr::mutate(dplyr::across(dplyr::starts_with('amount'),
                                        ~ dplyr::case_when(is.na(.) ~ NA,
                                                           TRUE ~ . * 0.6)))
        }
      }

      # Join dataframes
      df_temp <- dplyr::bind_rows(df_temp_personal, df_temp_shared)

      # Arrange by date
      df_temp <- dplyr::arrange(df_temp, date)

      ### Ensure new bank data does not overlap with data already in master ####

      # Location of master files
      path_latest_master_file <- as.character(input$master_file$datapath)

      # Load the latest master file
      df_old_master_file <- readr::read_csv(path_latest_master_file)

      # Compare rows by date, description, currency, etc. to ensure new
      # expenses do not overlap old expenses
      df_new_expenses <- df_temp |>
        dplyr::anti_join(
          df_old_master_file,
          by = c("date", "description", "amount_chf", "amount_dkk",
                 "amount_eur", "amount_usd", "amount_gbp", "bank"))

      ### Classify new expenses & output rhandsontable                      ####
      # Location of category classifier
      path_category_dict <- as.character(input$dictionary_file$datapath)

      # Classify new expenses' subcategories
      df_new_expenses_classified <- expensifyR::classify_subcategories(
        df_new_expenses = df_new_expenses,
        path_category_dict = path_category_dict,
        df_old_master_file = df_old_master_file)

      # Save the master currency for column selection in rhandsontable
      amount_var <- stringr::str_c("amount", input$master_currency_new_expenses, sep = "_")

      # Load the category dictionary
      category_dict <- readr::read_csv(as.character(input$dictionary_file$datapath))

      # Show the user the output on a table
      output$unverified_expenses_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(
          df_new_expenses_classified  |>
            dplyr::mutate(date = as.character(date),
                          subcategory = as.factor(subcategory)) |>
            dplyr::select(date, description, amount_var, bank, subcategory),
          height = 500) |>
          rhandsontable::hot_col("subcategory",
                                 allowInvalid = FALSE,
                                 type = "dropdown",
                                 source = (category_dict |> dplyr::pull(subcategory))) |>
          rhandsontable::hot_cols(colWidths = c(100, 150, 100, 100, 100)) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE)
      })

      # Give the user a notification if the table has appeared
      if(nrow(df_new_expenses_classified) > 1) {
        shiny::showNotification(
          "Script ran successfully! Now head to the 'Edit' tab",
          type = "warning", duration = NULL)
      }

      ### Download new, verified master file upon user click                ####

      # Save edits button
      output$download_new_master_file <- shiny::downloadHandler(
        filename = function() {
          stringr::str_c('master_',
                         format(Sys.Date(), "%Y%m%d"),
                         '.csv',
                         sep = '')
        },
        content = function(file) {

          # Convert hands-on-table to R dataframe
          saved_df <- shiny::isolate(
            rhandsontable::hot_to_r(
              input$unverified_expenses_table)) |>
            dplyr::mutate(
              subcategory = as.character(subcategory)) |>
            dplyr::left_join(
              df_new_expenses_classified |>
                dplyr::mutate(date = as.character(date)),
              by = c("date", "description", amount_var, "bank", "subcategory")
              )

          # Bind new expenses (with category & direction) to the previous version of the master
          new_master <- dplyr::bind_rows(
            df_old_master_file |>
              dplyr::mutate(date = as.character(date)),
            saved_df |>
              dplyr::left_join(category_dict, by = "subcategory")) |>
            dplyr::mutate(date = lubridate::as_date(date)) |>
            dplyr::arrange(date) |>
            as.data.frame()

          # Save the file
          readr::write_csv(new_master, file)
        }
      )

    })

    ## Analyse expenses tab                                            ####
    ### Add UI elements for user input                                      ####

    # Run code after user selects analytics master file
    shiny::observeEvent(input$analytics_master_file, {

      # Wait for analytics master file input to run the script
      shiny::req(input$analytics_master_file)

      # Location of the file
      path_analytics_master_file <- as.character(input$analytics_master_file$datapath)

      # Import file
      df_analytics_master_file <- readr::read_csv(path_analytics_master_file)

      # Dates to select from
      min_max_dates <- df_analytics_master_file |>
        dplyr::summarise(
          min_date = min(date),
          max_date = max(date)
        )
      min_date <- min_max_dates$min_date
      max_date <- min_max_dates$max_date

      # Add date selection - based on the data
      output$analytics_date_range <- shiny::renderUI({
        shiny::dateRangeInput("analytics_date_range",
                              "What range of dates do you want to visualize?",
                              start = min_date,
                              end   = max_date)
      })

      # Categories to select from
      categories <- df_analytics_master_file |>
        dplyr::distinct(category) |>
        dplyr::pull(category)

      # Add category selection - based on the data
      output$analytics_categories <- shiny::renderUI({
        shiny::selectInput(inputId ="analytics_selected_categories",
                           label = "Choose which categories to include",
                           choices = categories,
                           multiple = TRUE,
                           selected = categories
        )
      })

      # Subcategories to select from
      subcategories <- df_analytics_master_file |>
        dplyr::distinct(subcategory) |>
        dplyr::pull(subcategory)

      # Add subcategory selection - based on the data
      output$analytics_subcategories <- shiny::renderUI({
        shiny::selectInput(inputId ="analytics_selected_subcategories",
                           label = "Choose which subcategories to include",
                           choices = subcategories,
                           multiple = TRUE,
                           selected = subcategories
        )
      })
    })

    ### Plot waterfall graph                                                ####
    observe({

      # Wait for user inputs to run the script
      shiny::req(input$analytics_master_file)
      shiny::req(input$analytics_date_range)

      #### Filter data based on user input                                  ####
      # Location of the file
      path_analytics_master_file <- as.character(input$analytics_master_file$datapath)

      # Load master file
      df_analytics_master_file_reactive <- shiny::reactive(readr::read_csv(path_analytics_master_file))

      # Filter master file to desired date range
      df_analytics_master_file_reactive <- df_analytics_master_file_reactive() |>
        # Filter to the desired date range
        dplyr::filter(date >= input$analytics_date_range[1]) |>
        dplyr::filter(date <= input$analytics_date_range[2])

      # Filter master file to desired categories and subcategories
      df_analytics_master_file_reactive <- df_analytics_master_file_reactive |>
        dplyr::filter(category %in% input$analytics_selected_categories) |>
        dplyr::filter(subcategory %in% input$analytics_selected_subcategories)

      ### Manipulate data in preparation for plot                           ####

      # Create dataframe for waterfall plot
      df_waterfall <- expensifyR::transform_master_for_waterfall(
        df = df_analytics_master_file_reactive,
        master_currency_analytics = stringr::str_c("amount", input$master_currency_analytics, sep = "_"))

      ### Create & show plot                                                ####
      fig_waterfall <- expensifyR::plot_waterfall(df = df_waterfall)

      # Show plot
      output$waterfall_plot <- plotly::renderPlotly({
        fig_waterfall
      })
    })

    ## Update balances tab                                                  ####
    # When user uploads balances_file, load it in
    shiny::observeEvent(input$balances_file, {

      # Load the balances file dictionary
      df_balances_file <- readr::read_csv(
        as.character(input$balances_file$datapath)) |>
        mutate(date = as.character(date))

      # Show the user the balances file on a table
      output$balances_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(
          df_balances_file,
          height = 500) |>
          rhandsontable::hot_cols(colWidths = c(100, 150, 100)) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE)
      })

      ### Download new balances file upon user click                        ####

      # Save edits button
      output$download_new_balances_file <- shiny::downloadHandler(
        filename = function() {
          stringr::str_c('balances_',
                         format(Sys.Date(), "%Y%m%d"),
                         '.csv',
                         sep = '')
        },
        content = function(file) {

          # Convert hands-on-table to R dataframe
          df_saved_balances <- shiny::isolate(
            rhandsontable::hot_to_r(input$balances_table)) |>
            dplyr::mutate(date = as.character(date))

          # Save the file
          readr::write_csv(df_saved_balances, file)
        }
      )

    })

    ## Analyse balances tab                                                 ####

    # Run code after user selects analytics balances file
    shiny::observeEvent(input$analytics_balances_file, {

      # Wait for analytics balances file input to run the script
      shiny::req(input$analytics_balances_file)

      # Import balances file
      df_analytics_balances_file <- readr::read_csv(
        as.character(input$analytics_balances_file$datapath))

      # Dates to select from
      analytics_balances_min_max_dates <- df_analytics_balances_file |>
        dplyr::summarise(
          min_date = min(date),
          max_date = max(date)
        )
      analytics_balances_min_date <- analytics_balances_min_max_dates$min_date
      analytics_balances_max_date <- analytics_balances_min_max_dates$max_date

      ### Add UI elements for user input                                      ####
      # Add date selection - based on the data
      output$analytics_balances_date_range <- shiny::renderUI({
        shiny::dateRangeInput("analytics_balances_selected_date_range",
                              "What range of dates do you want to visualize?",
                              start = analytics_balances_min_date,
                              end   = analytics_balances_max_date)
      })

      # Categories to select from
      analytics_balances_accounts <- df_analytics_balances_file |>
        dplyr::distinct(account) |>
        dplyr::pull(account)

      # Add category selection - based on the data
      output$analytics_balances_accounts <- shiny::renderUI({
        shiny::selectInput(inputId ="analytics_balances_selected_accounts",
                           label = "Choose which accounts to include",
                           choices = analytics_balances_accounts,
                           multiple = TRUE,
                           selected = analytics_balances_accounts
        )
      })
    })

    ### Plot net worth tracker graph                                        ####
    observe({

      # Wait for user inputs to run the script
      shiny::req(input$analytics_balances_file)
      shiny::req(input$analytics_balances_selected_accounts)

      #### Filter data based on user input                                  ####
      # Location of the file
      path_analytics_balances_file <- as.character(input$analytics_balances_file$datapath)

      # Load balances file
      df_analytics_balances_file_reactive <- shiny::reactive(
        readr::read_csv(path_analytics_balances_file))

      # Filter balances file to desired date range
      df_analytics_balances_file_reactive <- df_analytics_balances_file_reactive() |>
        # Filter to the desired date range
        dplyr::filter(date >= input$analytics_balances_selected_date_range[1]) |>
        dplyr::filter(date <= input$analytics_balances_selected_date_range[2])

      # Filter balances file to desired categories and subcategories
      df_analytics_balances_file_reactive <- df_analytics_balances_file_reactive |>
        dplyr::filter(account %in% input$analytics_balances_selected_accounts)

      ### Create & show plot                                                ####
      fig_balances <- expensifyR::plot_balances(df = df_analytics_balances_file_reactive)

      # Show plot
      output$balances_plot <- plotly::renderPlotly({
        fig_balances
      })
    })


  }
  shiny::shinyApp(ui, server)
}
