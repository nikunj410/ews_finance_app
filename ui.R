#Code written by Nikunj Goel, Department of Ecology and Evolutionary Biology,Yale University, 06511, New Haven, USA

# Define UI for application that draws a histogram
source("functions_stock_analysis.R")
shinyUI
(
  fluidPage
  (
   navbarPage
   ("Financial Systemic Indicators",
   tabPanel
   ("About",includeMarkdown("about.Rmd")),
   tabPanel
   ("Discription"),
   tabPanel
   (
    "Historical Meltdowns",
    tabsetPanel
    (
     tabPanel("DJI",
      fluidRow
      ( 
       column(7,img(src="dji.jpg", height = 634, width = 475, align = "center")),
       column(4,includeText("caption_dji.txt"))
      )
     ), 
     
     tabPanel("S&P500",
      fluidRow
      ( 
       column(7,img(src="sp500.jpg", height = 634, width = 385, align = "center")),
       column(4,includeText("caption_sp500.txt"))
      )
     ), 
    
     tabPanel("NASDAQ",
      fluidRow
      ( 
       column(7,img(src="nasdaq.jpg", height = 634, width = 385, align = "center")),
       column(4,includeText("caption_nasdaq.txt"))
      )
     ), 
    
     tabPanel("DAX and FTSE",
      fluidRow
      ( 
       column(7,img(src="dax&ftse.jpg", height = 634, width = 475, align = "center")),
       column(4,includeText("caption_dax_ftse.txt"))
      )
     )
    )
    
   ),

   tabPanel
   ("Current Trends",
    fluidRow(
     column(4,
      h4("Choose a Stock Index"),
      helpText("Format: Stock Index (Country, Continent)"
      ),
      selectInput ### Ireland, Italy, UK "FTSE 100 Index (UK, Europe)" = , Ecuador, Philippines, Israel
      ("current_market", "", choices = list("S&P500 Index (USA, North America)" = 1,
                                                 "NASDAQ Composite Index (USA, North America)" = 2,
                                                 "Dow Jones Index (USA, North America)" = 3,
                                                 "NYSE Composite Index (USA, North America)" = 4,
                                                 "Russell 1000 Index (USA, North America)" = 5,
                                                 "Wilshire 5000 Index (USA, North America)" = 6,
                                                 "S&P TSX Composite Index (Canada, North America)" = 7,
                                                 "Mexbol IPC Index (Mexico, North America)" = 8,
                                                 "Bovespa Index (Brazil, South America)" = 9,
                                                 "MERVAL Index (Argentina, South America)" = 10,
                                                 "DAX Index (Germany, Europe)" = 11,
                                                 "CAC-40 Index (France, Europe)"= 12,
                                                 "RTSI Index (Russia, Europe)" = 13,
                                                 "IBEX 35 Index (Spain, Europe)" = 14,
                                                 "ATX Index (Austria, Europe)" = 15,
                                                 "Euronext BEL-20 Index (Belgium, Europe)" = 16,
                                                 "CROBEX Index (Croatia, Europe)" = 17,
                                                 "PS Index (Czech Republic, Europe)" = 18,
                                                 "OMX Copenhagen 20 Index (Denmark, Europe)" = 19,
                                                 "OMX Tallinn Index (Estonia, Europe)" = 20,
                                                 "OMX Helsinki 25 Index (Finland, Europe)" = 21,
                                                 "Athens Composite Index (Greece, Europe)" = 22,
                                                 "BUX Blue Chip Index (Hungary, Europe)" = 23,
                                                 "OMX Iceland All-Share Index (Iceland, Europe)" = 24,
                                                 "OMX Riga Index (Latvia, Europe)" = 25,
                                                 "OMX Vilnius Index (Lithuania, Europe)" = 26,
                                                 "Lux General Index (Luxembourg, Europe)" = 27,
                                                 "AEX Amsterdam Index (Netherlands, Europe)" = 28,
                                                 "OMX Oslo 20 Index (Norway, Europe)" = 29,
                                                 "BET Index (Romania, Europe)" = 30,
                                                 "BELEX 15 Index (Serbia, Europe)" = 31,
                                                 "SBITOP Index (Slovenia, Europe)" = 32,
                                                 "OMX Stockholm 30 Index (Sweden, Europe)" = 33,
                                                 "Swiss Market Index (Switzerland, Europe)" = 34,
                                                 "UX Index (Ukraine, Europe)" = 35,
                                                 "Shanghai Composite Index (China, Asia)" = 36,
                                                 "Hang Seng Index (Hong Kong, Asia)" = 37,
                                                 "Nikkei 225 Index (Japan, Asia)" = 38,
                                                 "Bombay Stock Exchange (India, Asia)" = 39,
                                                 "KOSPI Composite Index (South Korea, Asia)" = 40,
                                                 "Jakarta Composite Index (Indonesia, Asia)" = 41,
                                                 "Amman General Index (Jordan, Asia)" = 42,
                                                 "Straits Times Index (Singapore, Asia)" = 43,
                                                 "Colombo All Shares Index (Sri Lanka, Asia)" = 44,
                                                 "Taiwan Weighted Index (Taiwan, Asia)" = 45,
                                                 "All Ordinaries Index (Australia, Oceania)" = 46,
                                                 "NZSE 50 Index (New Zealand, Oceania)" = 47,
                                                 "Johannesburg Stock Exchange (South Africa, Africa)" = 48
                                                 ),
      selected = 1, width  = 440),
      verbatimTextOutput("summary_current"),
      checkboxInput("sensitivity", label = "Click here to see sensitivity analysis", value = FALSE),
      verbatimTextOutput("summary_current_sensitivity")),
      column(8,

         plotOutput("ews_current", width = 690, height = 350),
         plotOutput("sensitivity_plot", width = 690, height = 350)

      )
    )
   ),
   tabPanel
   ("Analyze Yourself",  
    # Application title
    fluidRow(
     column(4,
      # titlePanel("Early Warning Signals"),
      h4("Choose a stock index"),
      helpText("Format: Stock Index (Country, Continent)"
              ),
      selectInput
      ("market", "", choices = list("S&P500 Index (USA, North America)" = 1,
                                                 "NASDAQ Composite Index (USA, North America)" = 2,
                                                 "Dow Jones Index (USA, North America)" = 3,
                                                 "NYSE Composite Index (USA, North America)" = 4,
                                                 "Russell 1000 Index (USA, North America)" = 5,
                                                 "Wilshire 5000 Index (USA, North America)" = 6,
                                                 "S&P TSX Composite Index (Canada, North America)" = 7,
                                                 "Mexbol IPC Index (Mexico, North America)" = 8,
                                                 "Bovespa Index (Brazil, South America)" = 9,
                                                 "MERVAL Index (Argentina, South America)" = 10,
                                                 "DAX Index (Germany, Europe)" = 11,
                                                 "CAC-40 Index (France, Europe)"= 12,
                                                 "RTSI Index (Russia, Europe)" = 13,
                                                 "IBEX 35 Index (Spain, Europe)" = 14,
                                                 "ATX Index (Austria, Europe)" = 15,
                                                 "Euronext BEL-20 Index (Belgium, Europe)" = 16,
                                                 "CROBEX Index (Croatia, Europe)" = 17,
                                                 "PS Index (Czech Republic, Europe)" = 18,
                                                 "OMX Copenhagen 20 Index (Denmark, Europe)" = 19,
                                                 "OMX Tallinn Index (Estonia, Europe)" = 20,
                                                 "OMX Helsinki 25 Index (Finland, Europe)" = 21,
                                                 "Athens Composite Index (Greece, Europe)" = 22,
                                                 "BUX Blue Chip Index (Hungary, Europe)" = 23,
                                                 "OMX Iceland All-Share Index (Iceland, Europe)" = 24,
                                                 "OMX Riga Index (Latvia, Europe)" = 25,
                                                 "OMX Vilnius Index (Lithuania, Europe)" = 26,
                                                 "Lux General Index (Luxembourg, Europe)" = 27,
                                                 "AEX Amsterdam Index (Netherlands, Europe)" = 28,
                                                 "OMX Oslo 20 Index (Norway, Europe)" = 29,
                                                 "BET Index (Romania, Europe)" = 30,
                                                 "BELEX 15 Index (Serbia, Europe)" = 31,
                                                 "SBITOP Index (Slovenia, Europe)" = 32,
                                                 "OMX Stockholm 30 Index (Sweden, Europe)" = 33,
                                                 "Swiss Market Index (Switzerland, Europe)" = 34,
                                                 "UX Index (Ukraine, Europe)" = 35,
                                                 "Shanghai Composite Index (China, Asia)" = 36,
                                                 "Hang Seng Index (Hong Kong, Asia)" = 37,
                                                 "Nikkei 225 Index (Japan, Asia)" = 38,
                                                 "Bombay Stock Exchange (India, Asia)" = 39,
                                                 "KOSPI Composite Index (South Korea, Asia)" = 40,
                                                 "Jakarta Composite Index (Indonesia, Asia)" = 41,
                                                 "Amman General Index (Jordan, Asia)" = 42,
                                                 "Straits Times Index (Singapore, Asia)" = 43,
                                                 "Colombo All Shares Index (Sri Lanka, Asia)" = 44,
                                                 "Taiwan Weighted Index (Taiwan, Asia)" = 45,
                                                 "All Ordinaries Index (Australia, Oceania)" = 46,
                                                 "NZSE 50 Index (New Zealand, Oceania)" = 47,
                                                 "Johannesburg Stock Exchange (South Africa, Africa)" = 48
      ), 
      selected = 1, width  = 440),
      h4("Choose a Date"),
      helpText("We use last four years from the provided date as our rolling window period"
              ),
      uiOutput("date_limit"),
      h4("Choose parameters values"),
      sidebarLayout
      (
       sidebarPanel
       (
        sliderInput
        ("bw","Bandwidth (bw)" , min = 2, max = 100, value = 25),
        sliderInput
        ("rw", "Rolling window (l_rw)", min = 350, max = 650, value = 500),
        sliderInput
        ("kw", "Kendall window (l_kw)", min = 150, max = 350, value = 250),
        width = 12
       ),

       mainPanel
       ( 
        width = 0
       )
      )
     ),
     column
     ( 4,
      plotOutput("ews_finance",width = 341, height = 661),
     downloadButton(outputId="Analyze_yourself", label = "Download the Plot")
     ),
     column
     ( 4, 
      verbatimTextOutput("summary_analyse")
     )
    )
   )
  )
 )
)


