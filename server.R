#Code written by Nikunj Goel, Department of Ecology and Evolutionary Biology,Yale University, 06511, New Haven, USA

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  N = 1000
  bw = 25
  rw = 500
  kw = 250
  kend = 0
  
  Stock_Market = StockMarketList()
  Stock_Index = StockIndexList()
  
  output$summary_current <- renderText({ 
    market = Stock_Market[as.numeric(input$current_market)]
    kt_last = tail(read.csv(paste(c("data_files/",market,"_rolling_kt.txt"),collapse = ""),stringsAsFactors=FALSE),1)

    if (kt_last$var > 0.85 || kt_last$spec > 0.85 )
    {
      paste("The system is exhibiting strong trends of increasing variability. One must check",
            "if these trends die away soon or whether they persist. All major stock market crashes",
            "were preceded by strong trends of increasing variability in the past, but there could",
            "also be chances of false alarms. This is an automatically generated message.", sep = " ")
    }
    else if (kt_last$var > 0.5 || kt_last$spec > 0.5)
    {
      paste("The system is showing increasing trends in variability but the strength is",
            "moderate. If these trends become stronger (Kendall-tau values more than 0.85 or so), there could be a possibility of",
            "systemic risk. Therefore, one must check these trends again to see if the strength",
            "of rising variability, as denoted by Kendall-tau, going to increase further. This is an automatically generated message.",sep = " ") 
    }
    else if (kt_last$var > -0.5 & kt_last$spec > -0.5 )
    {
      paste("The current trends of variability for variance and power-spectrum are relatively",
            "weak. Such trends in the major markets such as DJI, SP500 and NASDAQ in the past",
            "occur frequently and are indicative of a relatively stable state away from",
            "a crash. This is an automatically generated message.",sep = " ") 
    }
    else
    {
      paste("The system is showing reducing variability. Such trends in the major",
            "markets such as DJI, SP500 and NASDAQ in the past occur frequently and",
            "are indicative of a relatively stable state away from a crash. This is an automatically generated message.",sep = " ")
    }
  })
  
  output$summary_current_sensitivity <- renderText({ 
    
    if (input$sensitivity == TRUE)
    {
      paste("Plese refer to Guttal et al. (2016), for information on how to interpret sensitivity analysis.")
    }
    })
  output$ews_current <- renderPlot({
    
    market = Stock_Market[as.numeric(input$current_market)]
    Stock = read.csv(paste(c("data_files/",market,"_data.txt"),collapse = ""),stringsAsFactors=FALSE)
    Stock$Date = as.Date(Stock$Date)
    
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
    plot_timeseries(Stock$Date, Stock$Close, "black",axis_font, "",
                    "Stock Index", 3,T,x_dist,y_dist,"a")
    points(curr_stock_precrash$dates,curr_stock_precrash$smooth,type='l',lwd = 2,col='red')
    points(curr_stock_precrash$dates[1],curr_stock_precrash$smooth[1],lwd = 8,col = 'deepskyblue')
    points(curr_stock_precrash$dates[curr_stock_precrash$N],curr_stock_precrash$smooth[curr_stock_precrash$N],
           lwd = 8,col = 'deepskyblue')

    # Resisduals
    plot_timeseries(curr_stock_precrash$dates,curr_stock_precrash$residuals, "blue",axis_font, " ",
                    "Residuals", 2,F,x_dist,y_dist,"b")
    axis(2,at=pretty(curr_stock_precrash$residuals,n=2),
         labels=format(pretty(curr_stock_precrash$residuals,n=2), scientific=F),
         las=1,cex.axis=1.4,tck=0)
    draw_rw_arrow(curr_stock_precrash$dates,curr_stock_precrash$residuals,rw,1.5)

    
    # Variance
    plot_timeseries(curr_stock_precrash$dates,ews_trends$var_residuals, 'chartreuse4',
                    axis_font, "Date", "Variance", 2,T,x_dist,y_dist,"c")
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kw-kend + 1],
           ews_trends$var_residuals[curr_stock_precrash$N-kw-kend + 1],lwd = 8,col = 'darkviolet')
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kend],
           ews_trends$var_residuals[curr_stock_precrash$N-kend],lwd = 8,col = 'darkviolet')
    kendall_text(curr_stock_precrash$dates,ews_trends$var_residuals,rw,kendalls$var, 1.2)

    
    # Power Spectrum
    plot_timeseries(curr_stock_precrash$dates,ews_trends$spec_residuals, 'deeppink',
                    axis_font, "Date", "Power\nSpectrum", 3,T,x_dist,y_dist,"d")
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
      par(mfrow=c(2,2),mai=c(.65,1.5,0.1,0.25))
      # Variance kenall time series
      rolling_kt_plot(kt_series$Date[kt_dates_index],kt_series$var[kt_dates_index],"Date",
                      'Rolling \n Kendall-t',axis_font,x_dist,y_dist,"e")
      
      # Power spectrum kenall time series
      rolling_kt_plot(kt_series$Date[kt_dates_index],kt_series$spec[kt_dates_index],"Date",
                      'Rolling \n Kendall-t',axis_font,x_dist,y_dist,"f")

      # Variance histograms
      kendall_histogram_plot(kendall_histograms$var,axis_font,'Kendall-Tau',
                             "Normalized \n Frequency",x_dist,y_dist,"g")
      
      # Power spectrum histograms
      kendall_histogram_plot(kendall_histograms$spec,axis_font,'Kendall-Tau',
                             "Normalized \n Frequency",x_dist,y_dist,"h")
    }
    
  })
  
  ########                       ########
  ######## Analyse your self Tab ########
  ########                       ########
  output$summary_analyse <- renderText({ 
    market = Stock_Market[as.numeric(input$market)]
    kt_last = tail(read.csv(paste(c("data_files/",market,"_rolling_kt.txt"),collapse = ""),stringsAsFactors=FALSE),1)
    if (kt_last$var > 0.85 || kt_last$spec > 0.85 )
    {
      paste("The system is exhibiting strong trends of increasing variability. One must check",
            "if these trends die away soon or whether they persist. All major stock market crashes",
            "were preceded by strong trends of increasing variability in the past, but there could",
            "also be chances of false alarms. This is an automatically generated message.", sep = " ")
    }
    else if (kt_last$var > 0.5 || kt_last$spec > 0.5)
    {
      paste("The system is showing increasing trends in variability but the strength is",
            "moderate. If these trends become stronger (Kendall-tau values more than 0.85 or so), there could be a possibility of",
            "systemic risk. Therefore, one must check these trends again to see if the strength",
            "of rising variability, as denoted by Kendall-tau, going to increase further. This is an automatically generated message.",sep = " ") 
    }
    else if (kt_last$var > -0.5 & kt_last$spec > -0.5 )
    {
      paste("The current trends of variability for variance and power-spectrum are relatively",
            "weak. Such trends in the major markets such as DJI, SP500 and NASDAQ in the past",
            "occur frequently and are indicative of a relatively stable state away from",
            "a crash. This is an automatically generated message.",sep = " ") 
    }
    else
    {
      paste("The system is showing reducing variability. Such trends in the major",
            "markets such as DJI, SP500 and NASDAQ in the past occur frequently and",
            "are indicative of a relatively stable state away from a crash. This is an automatically generated message.",sep = " ")
    }
  })

  output$date_limit <- renderUI({
    
    market = Stock_Market[as.numeric(input$market)]
    Stock = read.csv(paste(c("data_files/",market,"_data.txt"),collapse = ""),stringsAsFactors=FALSE)
    Stock$Date = as.Date(Stock$Date)
    start_date = min(tail(Stock$Date,1),Sys.Date())
    end_date = head(Stock$Date,1) + years(4)
    "inputdate" = dateInput("date", "",value  = start_date,
                            min = end_date, max = start_date, format = "dd-mm-yyyy", width = 440)
  })  

  output$ews_finance <- renderPlot({

    market = Stock_Market[as.numeric(input$market)]
    Stock = read.csv(paste(c("data_files/",market,"_data.txt"),collapse = ""),stringsAsFactors=FALSE)
    Stock$Date = as.Date(Stock$Date)
   
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
    plot_timeseries(Stock$Date, Stock$Close, "black",axis_font, " ",
                    "Stock Index", 3,T,x_dist,y_dist,"a")
    points(stock_precrash$dates,stock_precrash$smooth,type='l',lwd = 2,col='red')
    points(stock_precrash$dates[1],stock_precrash$smooth[1],lwd = 8,col = 'deepskyblue')
    points(stock_precrash$dates[stock_precrash$N],stock_precrash$smooth[stock_precrash$N],
           lwd = 8,col = 'deepskyblue')
    mtext(side = 2, "a" ,cex = 1.5,line = -1.5, las = 2,
          at = min(Stock$Close)+0.95*(max(Stock$Close)-min(Stock$Close)))
    # Resisduals
    plot_timeseries(stock_precrash$dates,stock_precrash$residuals, "blue",axis_font, " ",
                    "Residuals", 2,F,x_dist,y_dist,"b")
    axis(2,at=pretty(stock_precrash$residuals,n=2),
         labels=format(pretty(stock_precrash$residuals,n=2), scientific=F),
         las=1,cex.axis=axis_font,tck=0)
    draw_rw_arrow(stock_precrash$dates,stock_precrash$residuals,input$rw,2)
    # Variance
    plot_timeseries(stock_precrash$dates,ews_trends$var_residuals, 'chartreuse4',
                    axis_font, "", "Variance", 2,T,x_dist,y_dist,"c")
    points(stock_precrash$dates[stock_precrash$N-input$kw + 1],
           ews_trends$var_residuals[stock_precrash$N-input$kw + 1],lwd = 8,col = 'darkviolet')
    points(stock_precrash$dates[stock_precrash$N],
           ews_trends$var_residuals[stock_precrash$N],lwd = 8,col = 'darkviolet')
    draw_kw_arrow(stock_precrash$dates,ews_trends$var_residuals,stock_precrash$N,input$kw,kend,2)
    kendall_text(stock_precrash$dates,ews_trends$var_residuals,input$rw,kendalls$var, 1.5)
    # Power Spectrum
    plot_timeseries(stock_precrash$dates,ews_trends$spec_residuals, 'deeppink',
                    axis_font, "Date", "Power\nSpectrum", 3,T,x_dist,y_dist,"d")
    points(stock_precrash$dates[stock_precrash$N-input$kw + 1],
           ews_trends$spec_residuals[stock_precrash$N-input$kw + 1],lwd = 8,col = 'darkviolet')
    points(stock_precrash$dates[stock_precrash$N],
           ews_trends$spec_residuals[stock_precrash$N],lwd = 8,col = 'darkviolet')
    kendall_text(stock_precrash$dates,ews_trends$spec_residuals,input$rw,kendalls$spec, 1.5)
    
    })
  
  output$Analyze_yourself <- downloadHandler(
    
    filename = function(){
      paste(Stock_Market[as.numeric(input$market)],"_date_",input$date,"_bw_",input$bw,"_rw_",input$rw,"_kw_",input$kw,".pdf",sep="")
    },
    content = function(file){
      market = Stock_Market[as.numeric(input$market)]
      Stock = read.csv(paste(c("data_files/",market,"_data.txt"),collapse = ""),stringsAsFactors=FALSE)
      Stock$Date = as.Date(Stock$Date)
      
      stock_precrash = precrashdata_Shiny(input$date-years(4),input$date,Stock,input$bw);
      ews_trends = ews(stock_precrash$residuals, input$rw)
      kendalls = kendall_coefficient(ews_trends,input$kw,0,stock_precrash$N)
      
      ############# Plotting ##########
      axis_font = 1.7
      x_dist = 2.8
      y_dist = 7
      
      pdf(file,5,10)
      par(mfrow=c(4,1),mai=c(.5,1.5,0.1,0.2))
      # Historical data
      plot_timeseries(Stock$Date, Stock$Close, "black",axis_font, " ",
                      "Stock Index", 3,T,x_dist,y_dist,"a")
      points(stock_precrash$dates,stock_precrash$smooth,type='l',lwd = 2,col='red')
      points(stock_precrash$dates[1],stock_precrash$smooth[1],lwd = 8,col = 'deepskyblue')
      points(stock_precrash$dates[stock_precrash$N],stock_precrash$smooth[stock_precrash$N],
             lwd = 8,col = 'deepskyblue')
      # Resisduals
      plot_timeseries(stock_precrash$dates,stock_precrash$residuals, "blue",axis_font, " ",
                      "Residuals", 2,F,x_dist,y_dist,"b")
      axis(2,at=pretty(stock_precrash$residuals,n=2),
           labels=format(pretty(stock_precrash$residuals,n=2), scientific=F),
           las=1,cex.axis=axis_font,tck=0)
      draw_rw_arrow(stock_precrash$dates,stock_precrash$residuals,input$rw,2)
      # Variance
      plot_timeseries(stock_precrash$dates,ews_trends$var_residuals, 'chartreuse4',
                      axis_font, "", "Variance", 2,T,x_dist,y_dist,"c")
      points(stock_precrash$dates[stock_precrash$N-input$kw + 1],
             ews_trends$var_residuals[stock_precrash$N-input$kw + 1],lwd = 8,col = 'darkviolet')
      points(stock_precrash$dates[stock_precrash$N],
             ews_trends$var_residuals[stock_precrash$N],lwd = 8,col = 'darkviolet')
      draw_kw_arrow(stock_precrash$dates,ews_trends$var_residuals,stock_precrash$N,input$kw,kend,2)
      kendall_text(stock_precrash$dates,ews_trends$var_residuals,input$rw,kendalls$var, 1.5)
      # Power Spectrum
      plot_timeseries(stock_precrash$dates,ews_trends$spec_residuals, 'deeppink',
                      axis_font, "Date", "Power\nSpectrum", 3,T,x_dist,y_dist,"d")
      points(stock_precrash$dates[stock_precrash$N-input$kw + 1],
             ews_trends$spec_residuals[stock_precrash$N-input$kw + 1],lwd = 8,col = 'darkviolet')
      points(stock_precrash$dates[stock_precrash$N],
             ews_trends$spec_residuals[stock_precrash$N],lwd = 8,col = 'darkviolet')
      kendall_text(stock_precrash$dates,ews_trends$spec_residuals,input$rw,kendalls$spec, 1.5)
      dev.off()
    }
  )
  
})