#Contains all functions to analyze time series of stock markets. 
library('lubridate')
library('moments')
library('Kendall')
library('KernSmooth')
library('shiny')
library('sfsmisc')
library('shape')
library('doParallel')
library('foreach')
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
  filestr = c(market,"_data.txt")
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



