#Contains all functions to analyze time series of stock markets. 
library('lubridate')
library('moments')
library('Kendall')
library('KernSmooth')
library('shiny')
library('sfsmisc')
library('shape')

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
  acf_residuals=numeric(N)
  avgspec_residuals=numeric(N)
  #Apply rolling window to residuals.
  #Residual data analysis.
  for (i in 1:(N-l_rw+1))
  {
    rolldata = residuals[i:(i+l_rw-1)];
    var_residuals[i+l_rw-1] = var(rolldata);
    acf_residuals[i+l_rw-1] = acf(rolldata,plot=FALSE)$acf[2];
    
    spec_residuals = spectrum(rolldata,plot=FALSE)$spec;
    avgspec_residuals[i+l_rw-1] = mean(spec_residuals[2:floor(l_rw/8)]);
  }
  acf_residuals[1:l_rw]= NA
  var_residuals[1:l_rw]= NA
  avgspec_residuals[1:l_rw]= NA
  
  #Output results
  ews_trends = list(acf_residuals=acf_residuals, var_residuals=var_residuals, spec_residuals=avgspec_residuals)
  return(ews_trends)
  
}

kendall_coefficient <- function(ews_trends,l_kw,k_end,N)
{
  
  init_index= N - l_kw; 
  kendaltau_acf = min(signif(Kendall(1:l_kw, ews_trends$acf_residuals[(init_index-k_end):(N-1-k_end)])$tau[1],3),0.999);
  kendaltau_var = min(signif(Kendall(1:l_kw, ews_trends$var_residuals[(init_index-k_end):(N-1-k_end)])$tau[1],3),0.999);
  kendaltau_avgspec = min(signif(Kendall(1:l_kw, ews_trends$spec_residuals[(init_index-k_end):(N-1-k_end)])$tau[1],3),0.999);
  kendalls = list(acf=kendaltau_acf,  var=kendaltau_var, spec=kendaltau_avgspec)
  return(kendalls)

  
}

sensitivity_histograms <- function(currentdate,Stock,rwrange,bwrange,N_sensitivity,l_kw,k_end)
{
  z = 0;
  allkendalls=array(0,dim=c(length(bwrange)*length(rwrange)*length(k_end)*length(l_kw),7))
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
          allkendalls[z,] = c(rw, bw,kw,end, kendalls$acf, kendalls$var, kendalls$spec);
        } 
      } 
    } 
  }
  
  return(list(acf = allkendalls[,5],var = allkendalls[,6],spec = allkendalls[,7]))
}


rolling_kendall <- function (Stock, rw, kw, N, bw)
{
  
  startindex = 1;
  endindex = length(Stock$Close)
  
  Nts = endindex - startindex - N + 2
  istart = startindex - 1
  
  rolling = array(dim=c(Nts,4)) #store n number of 3 kendall's. 
  print(Nts)
  for (i in 1:Nts)
  {
    istart = istart + 1
    indices = seq(istart, istart+N-1);
    stocks = Stock$Close[indices];
    
    #Detrending the data
    smoothdata=ksmooth(indices,stocks,kernel=c("box","normal"),bandwidth=bw)
    smooth=smoothdata$y
    
    residuals=stocks-smooth
    #Find historical Kendalls
    Kendalls = kendall_coefficient(ews(residuals, rw),kw,0,N)
    rolling[i,] = c(as.character(Stock$Date[N - 1 + i]),Kendalls$acf,Kendalls$var,Kendalls$spec)
    
  }  
  
  return(list(Date = rolling[,1]), acf = rolling[,2], var = rolling[,3], spec = rolling[,4])
  
}

