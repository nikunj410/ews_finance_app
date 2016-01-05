#Written by Nikunj Goel, Department of Ecology and Evolutionary Biology,Yale University, 06511, New Haven, USA

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  N = 1000
  bw = 25
  rw = 500
  kw = 250
  kend = 0
  
  Stock_Market = c("S&P_500_USA",
                   "NASDAQ_Composite_Index_USA",
                   "Dow_Jones_Industrial_Average_USA",
                   "New_York_Stock_Exchange_USA",
                   "Russell_1000_Index_USA",
                   "Wilshire_5000_Index_USA",
                   "S&P_TSX_Composite_Index_Canada",
                   "Mexbol_IPC_Index_Mexico",
                   "Bovespa_Index_Brazil",
                   "MERVAL_Index_Argentina",
                   "DAX_Index_Germany",
                   "CAC_40_Index_France",
                   "RTSI_Index_Russia",
                   "IBEX_35_Index_Spain",
                   "ATX_Index_Austria",
                   "Euronext_BEL_20_Index_Belgium",
                   "CROBEX_Index_Croatia",
                   "PS_Index_Czech_Republic",
                   "OMX_Copenhagen_20_Index_Denmark",
                   "OMX_Tallinn_Index_Estonia",
                   "OMX_Helsinki_25_Index_Finland",
                   "Athens_Composite_Index_Greece",
                   "BUX_Blue_Chip_Index_Hungary",
                   "OMX_Iceland_All_Share_Index_Iceland",
                   "OMX_Riga_Index_Latvia",
                   "OMX_Vilnius_Index_Lithuania",
                   "Lux_General_Index_Luxembourg",
                   "AEX_Amsterdam_Index_Netherlands",
                   "OMX_Oslo_20_Index_Norway",
                   "BET_Index_Romania",
                   "BELEX_15_Index_Serbia",
                   "SBITOP_Index_Slovenia",
                   "OMX_Stockholm_30_Index_Sweden",
                   "Swiss_Market_Index_Switzerland",
                   "UX_Index_Ukraine",
                   "Shanghai_Composite_Index_China",
                   "Hang_Seng_Index_China",
                   "Nikkei_225_Index_Japan",
                   "BSE_Sensex_India",
                   "KOSPI_Composite_Index_South_Korea",
                   "Jakarta_Composite_Index_Indonesia",
                   "Amman_General_Index_Jordan",
                   "Straits_Times_Index_Singapore",
                   "Colombo_All_Shares_Index_Sri_Lanka",
                   "Taiwan_Weighted_Index_Taiwan",
                   "All_Ordinaries_Index_Australia",
                   "NZSE_50_Index_New_Zealand",
                   "Johannesburg_Stock_Exchange_South_Africa",
                   "Milano_Italia_Borsa_Italy"
  )
  
  Stock_Index = c("S&P 500",
                   "NASDAQ Composite Index",
                   "Dow Jones Industrial Average USA",
                   "New_York_Stock_Exchange_USA",
                   "Russell_1000_Index_USA",
                   "Wilshire_5000_Index_USA",
                   "S&P_TSX_Composite_Index_Canada",
                   "Mexbol_IPC_Index_Mexico",
                   "Bovespa_Index_Brazil",
                   "MERVAL_Index_Argentina",
                   "DAX_Index_Germany",
                   "CAC_40_Index_France",
                   "RTSI_Index_Russia",
                   "IBEX_35_Index_Spain",
                   "ATX_Index_Austria",
                   "Euronext_BEL_20_Index_Belgium",
                   "CROBEX_Index_Croatia",
                   "PS_Index_Czech_Republic",
                   "OMX_Copenhagen_20_Index_Denmark",
                   "OMX_Tallinn_Index_Estonia",
                   "OMX_Helsinki_25_Index_Finland",
                   "Athens_Composite_Index_Greece",
                   "BUX_Blue_Chip_Index_Hungary",
                   "OMX_Iceland_All_Share_Index_Iceland",
                   "OMX_Riga_Index_Latvia",
                   "OMX_Vilnius_Index_Lithuania",
                   "Lux_General_Index_Luxembourg",
                   "AEX_Amsterdam_Index_Netherlands",
                   "OMX_Oslo_20_Index_Norway",
                   "BET_Index_Romania",
                   "BELEX_15_Index_Serbia",
                   "SBITOP_Index_Slovenia",
                   "OMX_Stockholm_30_Index_Sweden",
                   "Swiss_Market_Index_Switzerland",
                   "UX_Index_Ukraine",
                   "Shanghai_Composite_Index_China",
                   "Hang_Seng_Index_China",
                   "Nikkei_225_Index_Japan",
                   "BSE_Sensex_India",
                   "KOSPI_Composite_Index_South_Korea",
                   "Jakarta_Composite_Index_Indonesia",
                   "Amman_General_Index_Jordan",
                   "Straits_Times_Index_Singapore",
                   "Colombo_All_Shares_Index_Sri_Lanka",
                   "Taiwan_Weighted_Index_Taiwan",
                   "All_Ordinaries_Index_Australia",
                   "NZSE_50_Index_New_Zealand",
                   "Johannesburg_Stock_Exchange_South_Africa",
                   "Milano_Italia_Borsa_Italy"
  )
  
  
  output$summary_current_sensitivity <- renderText({ 
    market = Stock_Market[as.numeric(input$current_market)]
    kt_last = tail(read.csv(paste(c("data_files/",market,"_rolling_kt.txt"),collapse = ""),stringsAsFactors=FALSE),1)
    
    if (input$sensitivity == TRUE)

    {
      paste("bla bla bla")
    }
  })
  
  output$summary_current <- renderText({ 
    market = Stock_Market[as.numeric(input$current_market)]
    kt_last = tail(read.csv(paste(c("data_files/",market,"_rolling_kt.txt"),collapse = ""),stringsAsFactors=FALSE),1)
    
    if (kt_last$var > 0.9 || kt_last$spec > 0.9 )
    {
      
      if (kt_last$var > 0.9 && kt_last$spec > 0.9)
      {
        paste("Kendall tau coeffecients for both variance and power spectrum are high", collapse = " to ")
      }
      else if (kt_last$var > 0.9 & kt_last$spec < 0.9)
      {
        paste("Kendall tau coeffecients for variance is high", collapse = " to ")
      }
      else (kt_last$var < 0.9 & kt_last$spec > 0.9)
      {
      paste("Kendall tau coeffecients for power spectrum is high", collapse = " to ")
      }
    }
    
    else if (kt_last$var < 0.7 & kt_last$spec < 0.7 )
    {
      paste("Low variability suggests a stable system")
    }
    
    else
    {
      paste("Intermediate variability")
    }

  })
  

  
  output$ews_current <- renderPlot({
    
    market = Stock_Market[as.numeric(input$current_market)]
    Stock = read.csv(paste(c("data_files/",market,"_data.txt"),collapse = ""),stringsAsFactors=FALSE)
    Stock$Date = as.Date(Stock$Date)
    currentdate = tail(Stock$Date,1)
    
    curr_stock_precrash = precrashdata_Shiny(Sys.Date()-years(4),Sys.Date(),Stock,bw);
    ews_trends = ews(curr_stock_precrash$residuals, rw)
    kendalls = kendall_coefficient(ews_trends,kw,kend,curr_stock_precrash$N)
    
    ############# Plotting ##########
    
    # http://www.r-bloggers.com/labeling-the-vertical-axis-in-r-plots/
    ### down left up right 
    par(mfrow=c(2,2),mai=c(.65,1.5,0.1,0.25))

    # Historical data
    axis_font = 1.4
    x_dist = 2.2
    y_dist = 5.5
    plot_timeseries(Stock$Date, Stock$Close, "black",axis_font, " ", "Historical\nStock Index", 3,T,x_dist,y_dist)
    points(curr_stock_precrash$dates,curr_stock_precrash$smooth,type='l',lwd = 2,col='red')
    points(curr_stock_precrash$dates[1],curr_stock_precrash$smooth[1],lwd = 8,col = 'deepskyblue')
    points(curr_stock_precrash$dates[curr_stock_precrash$N],curr_stock_precrash$smooth[curr_stock_precrash$N],
           lwd = 8,col = 'deepskyblue')

    # Resisduals
    plot_timeseries(curr_stock_precrash$dates,curr_stock_precrash$residuals, "blue",axis_font, " ",
                    "Residuals", 2,F,x_dist,y_dist)
    axis(2,at=pretty(curr_stock_precrash$residuals,n=2),
         labels=format(pretty(curr_stock_precrash$residuals,n=2), scientific=F),
         las=1,cex.axis=1.4,tck=0)
    draw_rw_arrow(curr_stock_precrash$dates,curr_stock_precrash$residuals,rw,1.5)

    
    # Variance
    plot_timeseries(curr_stock_precrash$dates,ews_trends$var_residuals, 'chartreuse4',
                    axis_font, "Date", "Variance", 2,T,x_dist,y_dist)
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kw-kend + 1],
           ews_trends$var_residuals[curr_stock_precrash$N-kw-kend + 1],lwd = 8,col = 'darkviolet')
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kend],
           ews_trends$var_residuals[curr_stock_precrash$N-kend],lwd = 8,col = 'darkviolet')
    kendall_text(curr_stock_precrash$dates,ews_trends$var_residuals,rw,kendalls$var, 1.2)

    
    # Power Spectrum
    plot_timeseries(curr_stock_precrash$dates,ews_trends$spec_residuals, 'deeppink',
                    axis_font, "Date", "Power\nSpectrum", 3,T,x_dist,y_dist)
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kw-kend + 1],
           ews_trends$spec_residuals[curr_stock_precrash$N-kw-kend + 1],lwd = 8,col = 'darkviolet')
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kend],
           ews_trends$spec_residuals[curr_stock_precrash$N-kend],lwd = 8,col = 'darkviolet')
    draw_kw_arrow(curr_stock_precrash$dates,ews_trends$spec_residuals,curr_stock_precrash$N,kw,kend,1.5)
    kendall_text(curr_stock_precrash$dates,ews_trends$spec_residuals,rw,kendalls$spec, 1.2)
      
  }
  )
  
  output$sensitivity_plot <- renderPlot({
    
    if (input$sensitivity == TRUE)
    {
      axis_font = 1.4
      x_dist=2.2
      y_dist = 4.5
      market = Stock_Market[as.numeric(input$current_market)]
      kt_series = read.csv(paste(c("data_files/",market,"_rolling_kt.txt"),collapse = ""),stringsAsFactors=FALSE)
      kt_series$Date = as.Date(kt_series$Date)
      kt_dates_index = which(kt_series$Date <= Sys.Date() & kt_series$Date >= Sys.Date()-years(25))
      
      kendall_histograms = read.csv(paste(c("data_files/",market,"_histograms.txt"),collapse = ""),
                                    stringsAsFactors=FALSE)
      par(mfrow=c(2,2),mai=c(.65,1.4,0.1,0.25))
      # Variance kenall time series
      rolling_kt_plot(kt_series$Date[kt_dates_index],kt_series$var[kt_dates_index],"Date",
                      'Rolling \n Kendall-t',axis_font,x_dist,y_dist)
      
      # Power spectrum kenall time series
      rolling_kt_plot(kt_series$Date[kt_dates_index],kt_series$spec[kt_dates_index],"Date",
                      'Rolling \n Kendall-t',axis_font,x_dist,y_dist)

      # Variance histograms
      kendall_histogram_plot(kendall_histograms$var,axis_font,'Kendall-Tau',
                             "Normalized \n Frequency",x_dist,y_dist)
      
      # Power spectrum histograms
      kendall_histogram_plot(kendall_histograms$spec,axis_font,'Kendall-Tau',
                             "Normalized \n Frequency",x_dist,y_dist)
    }
    
  })
  
  ########                       ########
  ######## Analyse your self Tab ########
  ########                       ########
  output$summary_analyse <- renderText({ 
    market = Stock_Market[as.numeric(input$market)]
    kt_last = tail(read.csv(paste(c("data_files/",market,"_rolling_kt.txt"),collapse = ""),stringsAsFactors=FALSE),1)
    if (kt_last$var > 0.9 || kt_last$spec > 0.9 )
    {
      
      if (kt_last$var > 0.9 && kt_last$spec > 0.9)
      {
        paste("Kendall tau coeffecients for both variance and power spectrum are high", collapse = " to ")
      }
      else if (kt_last$var > 0.9 & kt_last$spec < 0.9)
      {
        paste("Kendall tau coeffecients for variance is high", collapse = " to ")
      }
      else (kt_last$var < 0.9 & kt_last$spec > 0.9)
      {
      paste("Kendall tau coeffecients for power spectrum is high", collapse = " to ")
      }
    }
  
    else if (kt_last$var < 0.7 & kt_last$spec < 0.7 )
      {
        paste("Low variability suggests a stable system")
      }
  
    else
    {
      paste("Intermediate variability")
    }
  })
  
  output$ews_finance <- renderPlot({

    market = Stock_Market[as.numeric(input$market)]
    Stock = read.csv(paste(c("data_files/",market,"_data.txt"),collapse = ""),stringsAsFactors=FALSE)
    Stock$Date = as.Date(Stock$Date)
    currentdate = tail(Stock$Date,1)
   
    stock_precrash = precrashdata_Shiny(input$date-years(4),input$date,Stock,input$bw);
    ews_trends = ews(stock_precrash$residuals, input$rw)
    kendalls = kendall_coefficient(ews_trends,input$kw,0,stock_precrash$N)
    
    ############# Plotting ##########
    
    # http://www.r-bloggers.com/labeling-the-vertical-axis-in-r-plots/
    ### down left up right 
    par(mfrow=c(4,1),mai=c(.5,1.5,0.1,0.2))
    axis_font = 1.7
    x_dist = 2.8
    y_dist = 7
    
    # Historical data
    plot_timeseries(Stock$Date, Stock$Close, "black",axis_font, " ", "Historical\nStock Index", 3,T,x_dist,y_dist)
    points(stock_precrash$dates,stock_precrash$smooth,type='l',lwd = 2,col='red')
    points(stock_precrash$dates[1],stock_precrash$smooth[1],lwd = 8,col = 'deepskyblue')
    points(stock_precrash$dates[stock_precrash$N],stock_precrash$smooth[stock_precrash$N],
           lwd = 8,col = 'deepskyblue')

    # Resisduals
    plot_timeseries(stock_precrash$dates,stock_precrash$residuals, "blue",axis_font, " ",
                    "Residuals", 2,F,x_dist,y_dist)
    draw_rw_arrow(stock_precrash$dates,stock_precrash$residuals,input$rw,2)
    
    # Variance
    plot_timeseries(stock_precrash$dates,ews_trends$var_residuals, 'chartreuse4',
                    axis_font, "Date", "Variance", 2,T,x_dist,y_dist)
    points(stock_precrash$dates[stock_precrash$N-input$kw + 1],
           ews_trends$var_residuals[stock_precrash$N-input$kw + 1],lwd = 8,col = 'darkviolet')
    points(stock_precrash$dates[stock_precrash$N],
           ews_trends$var_residuals[stock_precrash$N],lwd = 8,col = 'darkviolet')
    draw_kw_arrow(stock_precrash$dates,ews_trends$var_residuals,stock_precrash$N,input$kw,kend,2)
    kendall_text(stock_precrash$dates,ews_trends$var_residuals,input$rw,kendalls$spec, 1.5)
        
    # Power Spectrum
    plot_timeseries(stock_precrash$dates,ews_trends$spec_residuals, 'deeppink',
                    axis_font, "Date", "Power\nSpectrum", 3,T,x_dist,y_dist)
    points(stock_precrash$dates[stock_precrash$N-input$kw + 1],
           ews_trends$spec_residuals[stock_precrash$N-input$kw + 1],lwd = 8,col = 'darkviolet')
    points(stock_precrash$dates[stock_precrash$N],
           ews_trends$spec_residuals[stock_precrash$N],lwd = 8,col = 'darkviolet')
    kendall_text(stock_precrash$dates,ews_trends$spec_residuals,input$rw,kendalls$spec, 1.5)
    
    }
    
    )
})