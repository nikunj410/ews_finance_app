#Code written by Nikunj Goel, Department of Ecology and Evolutionary Biology,Yale University, 06511, New Haven, USA

library('lubridate')
library('moments')
library('Kendall')
library('KernSmooth')
library('shiny')
library('sfsmisc')
library('shape')
library('doParallel')
library('foreach')

StockMarketList <- function() {
  list = c("S&P_500_USA",
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
  return(list)
}

StockIndexList <- function(){
  
  list = c("S&P500 Index (USA, North America)",
           "NASDAQ Composite Index (USA, North America)",
           "Dow Jones Index (USA, North America)",
           "NYSE Composite Index (USA, North America)",
           "Russell 1000 Index (USA, North America)",
           "Wilshire 5000 Index (USA, North America)",
           "S&P TSX Composite Index (Canada, North America)",
           "Mexbol IPC Index (Mexico, North America)",
           "Bovespa Index (Brazil, South America)",
           "MERVAL Index (Argentina, South America)",
           "DAX Index (Germany, Europe)",
           "CAC-40 Index (France, Europe)",
           "RTSI Index (Russia, Europe)",
           "IBEX 35 Index (Spain, Europe)",
           "ATX Index (Austria, Europe)",
           "Euronext BEL-20 Index (Belgium, Europe)",
           "CROBEX Index (Croatia, Europe)",
           "PS Index (Czech Republic, Europe)",
           "OMX Copenhagen 20 Index (Denmark, Europe)",
           "OMX Tallinn Index (Estonia, Europe)",
           "OMX Helsinki 25 Index (Finland, Europe)",
           "Athens Composite Index (Greece, Europe)",
           "BUX Blue Chip Index (Hungary, Europe)",
           "OMX Iceland All-Share Index (Iceland, Europe)",
           "OMX Riga Index (Latvia, Europe)",
           "OMX Vilnius Index (Lithuania, Europe)",
           "Lux General Index (Luxembourg, Europe)",
           "AEX Amsterdam Index (Netherlands, Europe)",
           "OMX Oslo 20 Index (Norway, Europe)",
           "BET Index (Romania, Europe)",
           "BELEX 15 Index (Serbia, Europe)",
           "SBITOP Index (Slovenia, Europe)",
           "OMX Stockholm 30 Index (Sweden, Europe)",
           "Swiss Market Index (Switzerland, Europe)",
           "UX Index (Ukraine, Europe)",
           "Shanghai Composite Index (China, Asia)",
           "Hang Seng Index (Hong Kong, Asia)",
           "Nikkei 225 Index (Japan, Asia)",
           "Bombay Stock Exchange (India, Asia)",
           "KOSPI Composite Index (South Korea, Asia)",
           "Jakarta Composite Index (Indonesia, Asia)",
           "Amman General Index (Jordan, Asia)",
           "Straits Times Index (Singapore, Asia)",
           "Colombo All Shares Index (Sri Lanka, Asia)",
           "Taiwan Weighted Index (Taiwan, Asia)",
           "All Ordinaries Index (Australia, Oceania)",
           "NZSE 50 Index (New Zealand, Oceania)",
           "Johannesburg Stock Exchange (South Africa, Africa)"
          )
  return(list)
}

sciNotation <- function(x, digits = 1) {
  if (length(x) > 1) {
    return(append(sciNotation(x[1]), sciNotation(x[-1])))
  }
  if (!x) return(0)
  exponent <- floor(log10(x))
  base <- round(x / 10^exponent, digits)
  as.expression(substitute(base %*% 10^exponent, 
                           list(base = base, exponent = exponent)))
}

copypastedata = function(market,Stock,keeps)
{
  Stock = Stock[keeps]
  colnames(Stock) <- c("Date", "Close")
  Stock$Date = rev(Stock$Date)
  Stock$Close = rev(Stock$Close)
  del_ind = which(Stock$Close == 0)
  if (length(del_ind != 0))
  {
    Stock = Stock[- del_ind ]
  }
  Stock = Stock[complete.cases(Stock),]
  filestr = c("data_files/",market,"_data.txt")
  write.table(Stock,paste(filestr,collapse=''), sep=',',col.names=T,row.names=F)
  
}

precrashdata <- function(currentdate,Stock,bw,N)
{
  
  stdindex = which(Stock$Date == currentdate)
  indices_precrash = seq(stdindex-N+1, stdindex);
  precrash = Stock$Close[indices_precrash]
  smoothdata=ksmooth(1:N,precrash,kernel=c("box","normal"),bandwidth=bw) # To calculate deterministic trends in time series of stock market
  smooth=smoothdata$y
  residuals = precrash - smooth # Residual data
  return(list(dates=Stock$Date[indices_precrash],precrash=precrash,smooth=smooth,residuals=residuals))
  
}

precrashdata_Shiny <- function(enddate,currentdate,Stock,bw)
{
  
  indices_precrash = which(Stock$Date <= currentdate & Stock$Date >= enddate)
  currentdate = tail(Stock$Date[indices_precrash],1)
  enddate = head(Stock$Date[indices_precrash],1)
  N = length(indices_precrash)
  precrash = Stock$Close[indices_precrash]
  smoothdata=ksmooth(1:N,precrash,kernel=c("box","normal"),bandwidth=bw) # To calculate deterministic trends in time series of stock market
  smooth=smoothdata$y
  residuals = precrash - smooth # Residual data
  return(list(dates=Stock$Date[indices_precrash],precrash=precrash,smooth=smooth,
              residuals=residuals, N = N, currentdate = currentdate, enddate = enddate))
  
}

ews <- function(residuals, l_rw) 
{
  
  #Declare arrays   
  N=length(residuals)
  var_residuals=numeric(N)
  avgspec_residuals=numeric(N)
  #Apply rolling window to residuals.
  #Residual data analysis.
  registerDoParallel(cores = 4)
  ### Parallelized for loop
  r = foreach(i = 1:(N-l_rw+1)) %dopar% 
  {
    c(var(residuals[i:(i+l_rw-1)]),mean(spectrum(residuals[i:(i+l_rw-1)],plot=FALSE)$spec[2:floor(l_rw/8)]))
  }
  var_residuals[1:l_rw]= NA
  avgspec_residuals[1:l_rw]= NA
  
  for (i in 1:(N-l_rw+1))
  {
    var_residuals[i+l_rw-1] = r[[i]][1]
    avgspec_residuals[i+l_rw-1] = r[[i]][2]
  }
  
  #Output results
  ews_trends = list(var_residuals=var_residuals, spec_residuals=avgspec_residuals)
  return(ews_trends)
  
}

kendall_coefficient <- function(ews_trends,l_kw,k_end,N)
{
  
  init_index= N - l_kw; 
  kendaltau_var = min(signif(Kendall(1:l_kw, ews_trends$var_residuals[(init_index-k_end):(N-1-k_end)])$tau[1],3),0.999);
  kendaltau_avgspec = min(signif(Kendall(1:l_kw, ews_trends$spec_residuals[(init_index-k_end):(N-1-k_end)])$tau[1],3),0.999);
  kendalls = list(var=kendaltau_var, spec=kendaltau_avgspec)
  return(kendalls)
  
}

sensitivity_histograms <- function(currentdate,Stock,rwrange,bwrange,N_sensitivity,l_kw,k_end)
{
  z = 0;
  allkendalls=array(0,dim=c(length(bwrange)*length(rwrange)*length(k_end)*length(l_kw),6))
  for (rw in rwrange)
  {
    for (bw in bwrange)
    {
      stock_precrash = precrashdata(currentdate,Stock,bw,N_sensitivity)
      ews_trends = ews(stock_precrash$residuals, rw)
      for (kw in l_kw)
      {
        for ( end  in k_end)
        { 
          z = z+1;
          kendalls = kendall_coefficient(ews_trends,kw,end,N_sensitivity)
          allkendalls[z,] = c(rw, bw,kw,end, kendalls$var, kendalls$spec);
        } 
      } 
    } 
  }
  
  return(list(var = allkendalls[,5],spec = allkendalls[,6]))
}

### Ploting
plot_timeseries <- function(x,y,colour,axis_font,x_label,y_label, y_points,flag,x_dist,y_dist)
{
  plot(x,y,type='l',lwd = 4,xlab = '',ylab = NA,
       col=colour,yaxt = 'n',cex.lab = 2,cex.axis = axis_font)
  box(lwd=5)
  if (flag == T)
  {
    axis(2,at=pretty(y,n=y_points),labels=sciNotation(pretty(y,n=y_points), 1),las=1,cex.axis=axis_font,tck=0)
  }
  
  mtext(side = 2, y_label, line = y_dist,cex = 1.4 )
  mtext(side = 1, x_label, line = x_dist,cex = 1.4 )
}

draw_rw_arrow <- function(x,y,rw,text_font)
{
  Arrows(x[1], 0.7*min(y),x[rw], 0.7*min(y),code = 3,arr.length = 0.5, col = "black", lwd = 3, arr.type = "T")
  text(x[floor(rw/2)],0.7*min(y),expression(l[rw]),cex = text_font,pos = 1,offset = 0.4)
}

draw_kw_arrow <- function(x,y,N,kw,kend,text_font)
{
  Arrows(x[N - kend],(1-0.8)*max(y,na.rm = T) + 0.8* min(y,na.rm = T),x[N - kend - kw +1],
         (1-0.8)*max(y,na.rm = T) + 0.8* min(y,na.rm = T),code = 3,arr.length = 0.5, col = "black",
         lwd = 3, arr.type = "T")
  text(x[floor((N - kend)-kw/2)],(1-0.8)*max(y,na.rm = T) + 0.8* min(y,na.rm = T),expression(l[kw]),
       cex = text_font,pos = 1,offset = 0.4)
}

kendall_text<- function(x,y,rw,kendall_value, kendall_font)
{
  text(x[floor(rw/2)],
      (0.8*max(y,na.rm = T)+0.2*min(y,na.rm = T)),
      paste(c("Kendall-t = ",kendall_value), collapse = ""),cex = kendall_font)  
}

rolling_kt_plot<- function(x,y,x_label,y_label,axis_font,x_dist,y_dist)
{
  plot(x,y,type='l', ylim = c(-1,1),lwd = 5,xlab = '',ylab = NA,yaxt = 'n',cex.lab = 2,cex.axis=axis_font)
  points(x,rep.int(0.9, times = length(x)), type = 'l',col ='red', lwd = 3, ylim = c(-1,1))
  box(lwd=5)
  axis(2,las=1, at = c(-1, 0, 1), cex.axis=axis_font,tck=0)
  mtext(side = 2,y_label, line = y_dist,cex = 1.4 )
  mtext(side = 1, x_label, line = x_dist,cex = 1.4 )
}

kendall_histogram_plot<-function(x,axis_font,x_label,y_label,x_dist,y_dist)
{
  h = hist(x,breaks = seq(-1,1, by = 0.1),plot=F);
  h$counts = h$counts/sum(h$counts)
  plot(h, col="grey", ylim  = c(0,1), xlab = '',ylab = NA,yaxt = 'n', 
       cex.lab = 2, cex.axis=axis_font,xlim = c(-1,1), main ="")
  box(lwd=5)
  axis(2,las=1, at = c(0,0.5,1), cex.axis=axis_font,tck=0)
  mtext(side = 2, y_label, line = y_dist,cex = 1.4 )
  mtext(side = 1, x_label, line = x_dist,cex = 1.4 )
}