#Written by Nikunj Goel, Department of Ecology and Evolutionary Biology,Yale University, 06511, New Haven, USA

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
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
  
  output$Alert <- renderText({ 
    market = Stock_Market[as.numeric(input$current_market)]
    kt_last = tail(read.csv(paste(c(market,"_rolling_kt.txt"),collapse = ""),stringsAsFactors=FALSE),1)
    if (kt_last$var > 0.9 || kt_last$spec > 0.9 )
    {
      paste("ALERT: Strong trends of variability could indicate impending meltdown")
    }
    else if (kt_last$var < 0.7 && kt_last$spec < 0.7 )
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
    Stock = read.csv(paste(c(market,"_data.txt"),collapse = ""),stringsAsFactors=FALSE)
    Stock$Date = as.Date(Stock$Date)
    currentdate = tail(Stock$Date,1)
    
    kt_series = read.csv(paste(c(market,"_rolling_kt.txt"),collapse = ""),stringsAsFactors=FALSE)
    kt_series$Date = as.Date(kt_series$Date)
    kt_dates_index = which(kt_series$Date <= Sys.Date() & kt_series$Date >= Sys.Date()-years(20))
    
    kendall_histograms = read.csv(paste(c(market,"_histograms.txt"),collapse = ""),stringsAsFactors=FALSE)
    
    curr_stock_precrash = precrashdata_Shiny(Sys.Date()-years(4),Sys.Date(),Stock,bw);
    ews_trends = ews(curr_stock_precrash$residuals, rw)
    kendalls = kendall_coefficient(ews_trends,kw,kend,curr_stock_precrash$N)
    
    ############# Plotting ##########
    
    # http://www.r-bloggers.com/labeling-the-vertical-axis-in-r-plots/
    ### down left up right 
    par(mfrow=c(4,3),mai=c(.63,1.5,0.1,0.2))

    # Historical data
    plot(Stock,type='l',lwd = 4,xlab = '',ylab = NA,
         col='black',yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    points(curr_stock_precrash$dates,curr_stock_precrash$smooth,type='l',lwd = 2,col='red')
    points(curr_stock_precrash$dates[1],curr_stock_precrash$smooth[1],lwd = 8,col = 'deepskyblue')
    points(curr_stock_precrash$dates[curr_stock_precrash$N],curr_stock_precrash$smooth[curr_stock_precrash$N],
           lwd = 8,col = 'deepskyblue')
    box(lwd=5)
    axis(2,at=pretty(Stock$Close,n=3),
         labels=sciNotation(pretty(Stock$Close,n=3), 1),
         las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, "Historical\nStock Index", line = 7,cex = 1.7 )
    
    # Current data
    plot(curr_stock_precrash$dates,curr_stock_precrash$precrash,type='l',lwd = 5,xlab = '',
         ylab = NA,yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    box(lwd=5)
    axis(2,at=pretty(curr_stock_precrash$precrash,n=3),
         labels=sciNotation(pretty(curr_stock_precrash$precrash,n=3), 1),
         las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, "Stock Index", line = 7,cex = 1.7 )
    points(curr_stock_precrash$dates,curr_stock_precrash$smooth,type='l',lwd = 3,col='red')
    points(curr_stock_precrash$dates[1],curr_stock_precrash$smooth[1],lwd = 8,col = 'deepskyblue')
    points(curr_stock_precrash$dates[curr_stock_precrash$N],curr_stock_precrash$smooth[curr_stock_precrash$N],
           lwd = 8,col = 'deepskyblue')
    
    # Resisduals
    plot(curr_stock_precrash$dates,curr_stock_precrash$residuals,type='l',lwd = 5,xlab = '',ylab = NA,
         col='blue',yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    box(lwd=5)
    axis(2,at=pretty(curr_stock_precrash$residuals,n=2),
         labels=format(pretty(curr_stock_precrash$residuals,n=2), scientific=FALSE),
         las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, 'Residuals', line = 7,cex = 1.7 )
    Arrows(curr_stock_precrash$dates[1], 0.7*min(curr_stock_precrash$residuals),
           curr_stock_precrash$dates[rw], 0.7*min(curr_stock_precrash$residuals),
           code = 3,arr.length = 0.5, col = "black", lwd = 3, arr.type = "T")
    text(curr_stock_precrash$dates[floor(rw/2)],0.7*min(curr_stock_precrash$residuals)
         ,expression(l[rw]),cex = 2,pos = 1,offset = 0.4)
    
    # Variance
    plot(curr_stock_precrash$dates,ews_trends$var_residuals,type='l',lwd = 5,xlab= '',ylab = NA,
         col='chartreuse4',yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    box(lwd=5)
    axis(2,at=pretty(ews_trends$var_residuals,n=3),
         labels=sciNotation(pretty(ews_trends$var_residuals,n=3), 1),
         las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, "Variance", line = 7,cex = 1.7 )
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kw-kend + 1],
           ews_trends$var_residuals[curr_stock_precrash$N-kw-kend + 1],
           lwd = 8,col = 'darkviolet')
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kend],
           ews_trends$var_residuals[curr_stock_precrash$N-kend],
           lwd = 8,col = 'darkviolet')
    Arrows(curr_stock_precrash$dates[curr_stock_precrash$N - kend],
           (1-0.8)*max(ews_trends$var_residuals,na.rm = T) + 0.8* min(ews_trends$var_residuals,na.rm = T),
           curr_stock_precrash$dates[curr_stock_precrash$N - kend - kw +1],
           (1-0.8)*max(ews_trends$var_residuals,na.rm = T) + 0.8* min(ews_trends$var_residuals,na.rm = T),
           code = 3,arr.length = 0.5, col = "black", lwd = 3, arr.type = "T")
    text(curr_stock_precrash$dates[floor((curr_stock_precrash$N - kend)-kw/2)],
         (1-0.8)*max(ews_trends$var_residuals,na.rm = T) + 0.8* min(ews_trends$var_residuals,na.rm = T),
         expression(l[kw]),cex = 2,pos = 1,offset = 0.4)
    text(curr_stock_precrash$dates[floor(rw/2)],
         (0.8*max(ews_trends$var_residuals,na.rm = T)+0.2*min(ews_trends$var_residuals,na.rm = T)),
         paste(c("Kendall-tau = ",kendalls$var), collapse = ""),cex = 1.5)
  
    # Power Spectrum
    plot(curr_stock_precrash$dates,ews_trends$spec_residuals,type='l',lwd = 5,xlab = NA, ylab = NA,
         col='deeppink',yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    box(lwd=5)
    axis(2,at=pretty(ews_trends$spec_residuals,n=3),
         labels=sciNotation(pretty(ews_trends$spec_residuals,n=3), 1),
         las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, 'Power\nSpectrum ', line = 7,cex = 1.7 )
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kw-kend + 1],
           ews_trends$spec_residuals[curr_stock_precrash$N-kw-kend + 1],
           lwd = 8,col = 'darkviolet')
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kend],
           ews_trends$spec_residuals[curr_stock_precrash$N-kend],
           lwd = 8,col = 'darkviolet')
    text(curr_stock_precrash$dates[floor(rw/2)],
         (0.8*max(ews_trends$spec_residuals,na.rm = T)+0.2*min(ews_trends$spec_residuals,na.rm = T)),
         paste(c("Kendall-t = ",kendalls$spec), collapse = ""),cex = 1.5)
    
    # Autocorrelation at lag 1
    plot(curr_stock_precrash$dates,ews_trends$acf_residuals,type='l',lwd = 5,xlab = '',ylab = NA,
         col='green',yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    box(lwd=5)
    axis(2,at=pretty(ews_trends$acf_residuals,n=2),
         labels=format(pretty(ews_trends$acf_residuals,n=2), scientific=FALSE),
         las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, 'Autocorrelation \n at Lag 1', line = 7,cex = 1.7 )
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kw-kend + 1],
           ews_trends$acf_residuals[curr_stock_precrash$N-kw-kend + 1],
           lwd = 8,col = 'darkviolet')
    points(curr_stock_precrash$dates[curr_stock_precrash$N-kend],
           ews_trends$acf_residuals[curr_stock_precrash$N-kend],
           lwd = 8,col = 'darkviolet')
    text(curr_stock_precrash$dates[floor(rw/2)],
         (0.8*max(ews_trends$acf_residuals,na.rm = T)+0.2*min(ews_trends$acf_residuals,na.rm = T)),
         paste(c("Kendall-t = ",kendalls$acf), collapse = ""),cex = 1.5)
    
    # Variance kenall time series
    plot(kt_series$Date[kt_dates_index],kt_series$var[kt_dates_index],type='l', ylim = c(-1,1),
         lwd = 5,xlab = '',ylab = NA,yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    points(kt_series$Date[kt_dates_index],rep.int(0.9, times = length(kt_dates_index)), type = 'l',
           col ='red', lwd = 3, ylim = c(-1,1))
    box(lwd=5)
    axis(2,las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, '  Rolling \n Kendall-t', line = 7,cex = 1.7 )
    mtext(side = 1, 'Date', line = 3,cex = 1.7 )
    
    # Power spectrum kenall time series
    plot(kt_series$Date[kt_dates_index],kt_series$spec[kt_dates_index],type='l', ylim = c(-1,1),
         lwd = 5,xlab = '',ylab = NA,yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    points(kt_series$Date[kt_dates_index],rep.int(0.9, times = length(kt_dates_index)), type = 'l',
           col ='red', lwd = 3, ylim = c(-1,1))
    box(lwd=5)
    axis(2,las=1,cex.axis=1.7,tck=0)
    mtext(side = 1, 'Date', line = 3,cex = 1.7 )
    
    # Autocorrelation kenall time series
    plot(kt_series$Date[kt_dates_index],kt_series$acf[kt_dates_index],type='l', ylim = c(-1,1),
         lwd = 5,xlab = '',ylab = NA,yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    points(kt_series$Date[kt_dates_index],rep.int(0.9, times = length(kt_dates_index)), type = 'l',
           col ='red', lwd = 3, ylim = c(-1,1))
    box(lwd=5)
    axis(2,las=1,cex.axis=1.7,tck=0)
    mtext(side = 1, 'Date', line = 3,cex = 1.7 )
    
    # Variance histograms
    h = hist(kendall_histograms$var,breaks = seq(-1,1, by = 0.1),plot=F);
    h$counts = h$counts/sum(h$counts)
    plot(h, col="grey", ylim  = c(0,1), xlab = '',ylab = NA,yaxt = 'n', 
         cex.lab = 2, cex.axis=1.7,xlim = c(-1,1), main ="")
    box(lwd=5)
    axis(2,las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, "Normalized \n Frequency", line = 7,cex = 1.7 )
    mtext(side = 1, 'Kendall-Tau', line = 3,cex = 1.7 )

    # Power spectrum histograms
    h = hist(kendall_histograms$spec,breaks = seq(-1,1, by = 0.1),plot=F);
    h$counts = h$counts/sum(h$counts)
    plot(h, col="grey", ylim  = c(0,1), xlab = '',ylab = NA,yaxt = 'n', 
         cex.lab = 2, cex.axis=1.7,xlim = c(-1,1), main ="")
    box(lwd=5)
    axis(2,las=1,cex.axis=1.7,tck=0)
    mtext(side = 1, 'Kendall-Tau', line = 3,cex = 1.7 )
    
    # Autocorrelation histograms
    h = hist(kendall_histograms$acf,breaks = seq(-1,1, by = 0.1),plot=F);
    h$counts = h$counts/sum(h$counts)
    plot(h, col="grey", ylim  = c(0,1), xlab = '',ylab = NA,yaxt = 'n', 
         cex.lab = 2, cex.axis=1.7,xlim = c(-1,1), main ="")
    box(lwd=5)
    axis(2,las=1,cex.axis=1.7,tck=0)
    mtext(side = 1, 'Kendall-Tau', line = 3,cex = 1.7 )
    
  },width = 1300, height = 650
  
  )
  
  output$ews_finance <- renderPlot({

    market = Stock_Market[as.numeric(input$market)]
    Stock = read.csv(paste(c(market,"_data.txt"),collapse = ""),stringsAsFactors=FALSE)
    Stock$Date = as.Date(Stock$Date)
    currentdate = tail(Stock$Date,1)
   
    stock_precrash = precrashdata_Shiny(input$daterange[1],input$daterange[2],Stock,input$bw);
    ews_trends = ews(stock_precrash$residuals, input$rw)
    kendalls = kendall_coefficient(ews_trends,input$kw,0,stock_precrash$N)
    
    ############# Plotting ##########
    
    # http://www.r-bloggers.com/labeling-the-vertical-axis-in-r-plots/
    ### down left up right 
    par(mfrow=c(4,1),mai=c(.5,1.5,0.1,0.2))
    
    # Historical data
    plot(Stock,type='l',lwd = 4,xlab = '',ylab = NA,
         col='black',yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    points(stock_precrash$dates,stock_precrash$smooth,type='l',lwd = 2,col='red')
    points(stock_precrash$dates[1],stock_precrash$smooth[1],lwd = 8,col = 'deepskyblue')
    points(stock_precrash$dates[stock_precrash$N],stock_precrash$smooth[stock_precrash$N],
           lwd = 8,col = 'deepskyblue')
    box(lwd=5)
    axis(2,at=pretty(Stock$Close,n=3),
         labels=sciNotation(pretty(Stock$Close,n=3), 1),
         las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, "Historical\nStock Index", line = 7,cex = 1.7 )

    # Current data
#     plot(stock_precrash$dates,stock_precrash$precrash,type='l',lwd = 5,xlab = '',
#        ylab = NA,yaxt = 'n',cex.lab = 2,cex.axis=1.7)
#     box(lwd=5)
#     axis(2,at=pretty(stock_precrash$precrash,n=3),
#          labels=sciNotation(pretty(stock_precrash$precrash,n=3), 1),
#          las=1,cex.axis=1.7,tck=0)
#     mtext(side = 2, "Stock Index", line = 7,cex = 1.7 )
#     points(stock_precrash$dates,stock_precrash$smooth,type='l',lwd = 3,col='red')
#     points(stock_precrash$dates[1],stock_precrash$smooth[1],lwd = 8,col = 'deepskyblue')
#     points(stock_precrash$dates[stock_precrash$N],stock_precrash$smooth[stock_precrash$N],
#            lwd = 8,col = 'deepskyblue')
    
    # Resisduals
    plot(stock_precrash$dates,stock_precrash$residuals,type='l',lwd = 5,xlab = '',ylab = NA,
         col='blue',yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    box(lwd=5)
    axis(2,at=pretty(stock_precrash$residuals,n=2),
         labels=format(pretty(stock_precrash$residuals,n=2), scientific=FALSE),
         las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, 'Residuals', line = 7,cex = 1.7 )
    Arrows(stock_precrash$dates[1], 0.7*min(stock_precrash$residuals),
           stock_precrash$dates[input$rw], 0.7*min(stock_precrash$residuals),
           code = 3,arr.length = 0.5, col = "black", lwd = 3, arr.type = "T")
    text(stock_precrash$dates[floor(input$rw/2)],0.7*min(stock_precrash$residuals)
         ,expression(l[rw]),cex = 2,pos = 1,offset = 0.4)
    
    # Variance
    plot(stock_precrash$dates,ews_trends$var_residuals,type='l',lwd = 5,xlab= '',ylab = NA,
         col='chartreuse4',yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    box(lwd=5)
    axis(2,at=pretty(ews_trends$var_residuals,n=3),
         labels=sciNotation(pretty(ews_trends$var_residuals,n=3), 1),
         las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, "Variance", line = 7,cex = 1.7 )
    points(stock_precrash$dates[stock_precrash$N-input$kw + 1],
           ews_trends$var_residuals[stock_precrash$N-input$kw + 1],
           lwd = 8,col = 'darkviolet')
    points(stock_precrash$dates[stock_precrash$N],
           ews_trends$var_residuals[stock_precrash$N],
           lwd = 8,col = 'darkviolet')
    Arrows(stock_precrash$dates[stock_precrash$N ],
           (1-0.8)*max(ews_trends$var_residuals,na.rm = T) + 0.8* min(ews_trends$var_residuals,na.rm = T),
           stock_precrash$dates[stock_precrash$N  - input$kw +1],
           (1-0.8)*max(ews_trends$var_residuals,na.rm = T) + 0.8* min(ews_trends$var_residuals,na.rm = T),
           code = 3,arr.length = 0.5, col = "black", lwd = 3, arr.type = "T")
    text(stock_precrash$dates[floor((stock_precrash$N )-input$kw/2)],
         (1-0.8)*max(ews_trends$var_residuals,na.rm = T) + 0.8* min(ews_trends$var_residuals,na.rm = T),
         expression(l[kw]),cex = 2,pos = 1,offset = 0.4)
    text(stock_precrash$dates[floor(input$rw/2)],
         (0.8*max(ews_trends$var_residuals,na.rm = T)+0.2*min(ews_trends$var_residuals,na.rm = T)),
         paste(c("Kendall-t = ",kendalls$var), collapse = ""),cex = 1.5)
        
    # Power Spectrum
    plot(stock_precrash$dates,ews_trends$spec_residuals,type='l',lwd = 5,xlab = NA, ylab = NA,
         col='deeppink',yaxt = 'n',cex.lab = 2,cex.axis=1.7)
    box(lwd=5)
    axis(2,at=pretty(ews_trends$spec_residuals,n=3),
         labels=sciNotation(pretty(ews_trends$spec_residuals,n=3), 1),
         las=1,cex.axis=1.7,tck=0)
    mtext(side = 2, 'Power\nSpectrum ', line = 7,cex = 1.7 )
    points(stock_precrash$dates[stock_precrash$N-input$kw + 1],
           ews_trends$spec_residuals[stock_precrash$N-input$kw + 1],
           lwd = 8,col = 'darkviolet')
    points(stock_precrash$dates[stock_precrash$N],
           ews_trends$spec_residuals[stock_precrash$N],
           lwd = 8,col = 'darkviolet')
    text(stock_precrash$dates[floor(input$rw/2)],
         (0.8*max(ews_trends$spec_residuals,na.rm = T)+0.2*min(ews_trends$spec_residuals,na.rm = T)),
         paste(c("Kendall-t = ",kendalls$spec), collapse = ""),cex = 1.5)
    
    # Autocorrelation at lag 1
#     plot(stock_precrash$dates,ews_trends$acf_residuals,type='l',lwd = 5,xlab = '',ylab = NA,
#          col='green',yaxt = 'n',cex.lab = 2,cex.axis=1.7)
#     box(lwd=5)
#     axis(2,at=pretty(ews_trends$acf_residuals,n=2),
#          labels=format(pretty(ews_trends$acf_residuals,n=2), scientific=FALSE),
#          las=1,cex.axis=1.7,tck=0)
#     mtext(side = 2, 'Autocorrelation \n at Lag 1', line = 7,cex = 1.7 )
#     mtext(side = 1, 'Date', line = 2.5,cex = 1.7 )
#     points(stock_precrash$dates[stock_precrash$N-input$kw + 1],
#            ews_trends$acf_residuals[stock_precrash$N-input$kw + 1],
#            lwd = 8,col = 'darkviolet')
#     points(stock_precrash$dates[stock_precrash$N],
#            ews_trends$acf_residuals[stock_precrash$N],
#            lwd = 8,col = 'darkviolet')
#     text(stock_precrash$dates[floor(input$rw/2)],
#          (0.8*max(ews_trends$acf_residuals,na.rm = T)+0.2*min(ews_trends$acf_residuals,na.rm = T)),
#          paste(c("Kendall-t = ",kendalls$acf), collapse = ""),cex = 1.5)
    
    },width = 650, height = 700#900
    
    )
})