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
        shinydashboard::menuItem("Analyse past expenses", tabName = "analyse_past_expenses", icon = shiny::icon("chart-line"))
      )
    ),
    ## Body                                                                 ####
    body = shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        ### Home Page                                                       ####
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
        ### Addition of new expenses                                            ####
        shinydashboard::tabItem(
          tabName = "add_new_expenses",
          shiny::h2("Add new expenses"),
          # Output: Tabset w/ plot, summary, and table ----
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("Load",
                                     shinydashboard::box(
                                       shiny::HTML(
                                         # Text
                                         "This is the 'Add new expenses' tab. <br/>
                    <br/>

                    Select all relevant files and options that are requested
                    on the right hand side of this page, and click the
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

                      # Input 3 - raw bank data
                      shiny::fileInput("temp_bank_data_files",
                                       'Select your new bank data files',
                                       multiple = TRUE),

                      # Input 4 - currency of choice
                      shiny::selectInput("master_currency_new_expenses",
                                         "What is your currency of choice?",
                                         c("EUROs" = "eur",
                                           "US Dollars" = "usd",
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

        ### Analysis of past expenses                                           ####
        shinydashboard::tabItem(tabName = "analyse_past_expenses",
                                shiny::h2("Analyse past expenses"),
                                shiny::fluidRow(

                                  # Analytics text
                                  shinydashboard::box(

                                    shiny::HTML("This is the 'Analyse past expenses' tab. <br/>
                <br/>

                The purpose of this tab is to allow you to select a specific
                'master' file you would like to analyse, and use the interactive
                graphs to the right to explore your personal finances. <br/>
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


                                   c("EUROs" = "eur",
                                     "US Dollars" = "usd",
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
        )
      )
    ),
    # Skin
    skin = "black")

  # Server                                                                  ####

  server <- function(input, output, session) {

    # Control notification
    df_new_expenses_classified <- NULL

    # When user clicks add_new_expenses_button, merge and categorise expenses.
    # When completed, show user the resulting table.
    shiny::observeEvent(input$add_new_expenses_button, {

      # Location of temp files with new expenses
      ### Input, modify, and combine bank data                                ####
      path_raw_bank_data_files <- as.character(input$temp_bank_data_files$datapath)

      # Create empty dataframe to populate iteratively
      df_temp <- data.frame()

      # Bind all data together with the same format
      # Loop over all filenames individually
      for (i in seq(1:length(path_raw_bank_data_files))) {

        # Extract filename, to be used for relevant information
        filename <- input$temp_bank_data_files$name[i]

        # Extract filepath, to be used for import
        filepath <- stringr::str_replace_all(input$temp_bank_data_files$datapath[i],
                                             "\\\\", "/")

        # Extract relevant information from filename
        y <- stringr::str_split(filename, "_")[[1]]
        bank <- y[1]
        currency <- tolower(y[2])

        # Create expression to run bank and currency specific function
        expr <- stringr::str_c(stringr::str_c("expensifyR::import", bank, sep = "_"),
                               "('",
                               filepath,
                               "', '",
                               currency,
                               "')")

        # Run function
        df_temp_addon <- eval(parse(text = expr))

        # Convert amounts, if rows present and currencies not aligned
        if (nrow(df_temp_addon) > 0) {
          if (currency != input$master_currency_new_expenses) {
            df_temp_addon <- expensifyR::convert_amount(
              df = df_temp_addon,
              currency_in = currency,
              currency_out = input$master_currency_new_expenses)
          }

          # Bind bank-currency specific data to other new data
          df_temp <- dplyr::bind_rows(df_temp, df_temp_addon)
        }
      }

      # Arrange by date
      df_temp <- dplyr::arrange(df_temp, date)

      ### Ensure the new bank data does not overlap with data already in the master    ####

      # Location of master files
      path_latest_master_file <- as.character(input$master_file$datapath)

      # Load the latest master file
      df_old_master_file <- readr::read_csv(path_latest_master_file)

      # Compare rows by date, description, currency, etc.
      df_new_expenses <- df_temp |>
        dplyr::anti_join(
          df_old_master_file,
          by = c("date", "description", "amount_chf", "amount_dkk",
                 "amount_eur", "amount_usd", "amount_gbp", "bank"))

      ### Classify new expenses                                               ####
      # Location of category classifier
      path_category_dict <- as.character(input$dictionary_file$datapath)

      # Classify new expenses' subcategories
      df_new_expenses_classified <- expensifyR::classify_subcategories(
        df_new_expenses,
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

      ### Save verified new expenses to master upon user click                  ####

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
          saved_df <- shiny::isolate(rhandsontable::hot_to_r(input$unverified_expenses_table)) |>
            dplyr::mutate(subcategory = as.character(subcategory)) |>
            dplyr::left_join(df_new_expenses_classified |>
                               dplyr::mutate(date = as.character(date)),
                             by = c("date", "description", amount_var, "bank", "subcategory"))

          # Load the category dictionary
          category_dict <- readr::read_csv(as.character(input$dictionary_file$datapath))

          # Load the latest master file
          df_old_master_file <- readr::read_csv(as.character(input$master_file$datapath))

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

    ### Analytics graphs                                                      ####

    # When a new file is selected for analytics, add variable filtering UIs
    shiny::observeEvent(input$analytics_master_file, {

      # Wait for input to run the script
      shiny::req(input$analytics_master_file)

      # Location of the file
      path_analytics_master_file <- as.character(input$analytics_master_file$datapath)

      # Dates to select from
      min_max_dates <- readr::read_csv(path_analytics_master_file) |>
        dplyr::summarise(
          min_date = min(date),
          max_date = max(date)
        )
      min_date <- min_max_dates$min_date
      max_date <- min_max_dates$max_date

      # Add date selection - based on the data selected
      output$analytics_date_range <- shiny::renderUI({
        shiny::dateRangeInput("analytics_date_range",
                              "What range of dates do you want to visualize?",
                              start = min_date,
                              end   = max_date)
      })

      # Categories to select from
      categories <- readr::read_csv(path_analytics_master_file) |>
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
      subcategories <- readr::read_csv(path_analytics_master_file) |>
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

    # Plot graphs
    observe({

      # Wait for input to run the script
      shiny::req(input$analytics_master_file)
      shiny::req(input$analytics_date_range)

      # Location of the file
      path_analytics_master_file <- as.character(input$analytics_master_file$datapath)

      # Load master file
      analytics_master_file <- shiny::reactive(readr::read_csv(path_analytics_master_file))

      # Filter master file
      analytics_master_file <- analytics_master_file() |>
        # Filter to the desired date range
        dplyr::filter(date >= input$analytics_date_range[1]) |>
        dplyr::filter(date <= input$analytics_date_range[2])

      # Filter master file to desired categories and subcategories
      analytics_master_file <- analytics_master_file |>
        dplyr::filter(category %in% input$analytics_selected_categories) |>
        dplyr::filter(subcategory %in% input$analytics_selected_subcategories)

      #### Process master file for a waterfall graph                            ####

      # Find number of distinct months
      n_distinct_months <- analytics_master_file |>
        dplyr::summarise(n_distinct_months = dplyr::n_distinct(lubridate::month(date))) |>
        pull(n_distinct_months)

      # Find the order of variables for the master file
      df_variable_order <- analytics_master_file |>
        dplyr::mutate(month = lubridate::month(date)) |>
        dplyr::rename(amount_currency = stringr::str_c(
          "amount", input$master_currency_analytics, sep = "_")) |>
        dplyr::mutate(category = stringr::str_replace(category, ' ', '_')) |>
        dplyr::group_by(category) |>
        dplyr::summarise(amount = round(sum(amount_currency)/n_distinct_months, 1)) |>
        dplyr::ungroup() |>
        dplyr::arrange(dplyr::desc(amount))

      df_variable_order_in <- df_variable_order |> dplyr::filter(amount > 0)

      df_variable_order_out <- df_variable_order |> dplyr::filter(amount <= 0) |>
        dplyr::arrange(amount)

      l_variable_order <- df_variable_order_in$category |>
        append(df_variable_order_out$category)

      # Plot graph
      df_waterfall <- df_variable_order |>
        dplyr::mutate(category = factor(category,
                                        l_variable_order
        )) |>
        dplyr::mutate(measure = "relative") |>
        dplyr::mutate(text = dplyr::case_when(
          amount > 0 ~ stringr::str_c('+', as.character(amount), sep = ''),
          TRUE ~ as.character(amount))
        ) |>
        rbind(data.frame(category = "Monthly profit after tax",
                                amount = 0,
                                measure = "total",
                                text = "Total")) |>
        dplyr::arrange(
          factor(
            category,
            levels = (l_variable_order |> append("Monthly profit after tax"))
          )
        )

      # Create plot
      fig_waterfall <- plotly::plot_ly(
        df_waterfall, name = "20", type = "waterfall", measure = ~ measure,
        x = ~category, textposition = "outside", y= ~amount, text =~text,
        connector = list(line = list(color= "rgb(63, 63, 63)")))

      fig_waterfall <- fig_waterfall |>
        plotly::layout(title = "Profit and loss statement",
                       xaxis = list(title = ""),
                       yaxis = list(title = ""),
                       autosize = TRUE,
                       showlegend = TRUE)

      # Show table
      output$waterfall_plot <- plotly::renderPlotly({
        fig_waterfall
      })
    })

  }

  shiny::shinyApp(ui, server)

}
