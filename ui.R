
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