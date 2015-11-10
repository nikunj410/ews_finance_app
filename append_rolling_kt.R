#Written by Nikunj Goel, Department of Ecology and Evolutionary Biology,Yale University, 06511, New Haven, USA

source("functions_stock_analysis.R")

N = 1000
bw = 25
rw = 500
kw = 250
Begin = Sys.time()
Stock_Markets = c("S&P_500_USA",
                  "NASDAQ_Composite_Index_USA",
                  "Dow_Jones_Industrial_Average_USA",
                  "Russell_1000_Index_USA",
                  "Wilshire_5000_Index_USA",
                  "Shanghai_Composite_Index_China",
                  "Hang_Seng_Index_China",
                  "Nikkei_225_Index_Japan",
                  "DAX_Index_Germany",
                  "CAC_40_Index_France",
                  "Bovespa_Index_Brazil",
                  "RTSI_Index_Russia",
                  "BSE_Sensex_India",
                  "S&P_TSX_Composite_Index_Canada",
                  "All_Ordinaries_Index_Australia",
                  "IBEX_35_Index_Spain",
                  "Mexbol_IPC_Index_Mexico",
                  "KOSPI_Composite_Index_South_Korea",
                  "Jakarta_Composite_Index_Indonesia",
                  "MERVAL_Index_Argentina",
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
                  "Amman_General_Index_Jordan",
                  "Straits_Times_Index_Singapore",
                  "Colombo_All_Shares_Index_Sri_Lanka",
                  "Taiwan_Weighted_Index_Taiwan",
                  "NZSE_50_Index_New_Zealand",
                  "New_York_Stock_Exchange_USA",
                  "Johannesburg_Stock_Exchange_South_Africa")

for(market in Stock_Markets)
{
  print(market)
  Stock = read.csv(paste(c("data_files/",market,"_data.txt"),collapse = ""),stringsAsFactors=FALSE)
  Stock$Date = as.Date(Stock$Date)
  
  if (file.exists(paste(c("data_files/",market,"_rolling_kt.txt"),collapse = "")) == T)
  {  
    rolling_kt = read.csv(paste(c("data_files/",market,"_rolling_kt.txt"),collapse = ""),stringsAsFactors=FALSE)
    rolling_kt$Date = as.Date(rolling_kt$Date)
    stock_tail_date = tail(Stock$Date,1)
    rolling_tail_date = tail(rolling_kt$Date,1)
    index = which(Stock$Date > rolling_tail_date & Stock$Date <= stock_tail_date)
    
    if (length(index) != 0)
    {
      Nts = length(index)
      Append = array(dim=c(Nts,3))
      
      istart = index[1] - 1
      for(k in 1:Nts)
      {
        istart = istart + 1
        indices = seq(istart-N+1, istart);
        stocks = Stock$Close[indices];
        smoothdata=ksmooth(indices,stocks,kernel=c("box","normal"),bandwidth=bw)
        smooth=smoothdata$y
        residuals=stocks-smooth
        Kendalls = kendall_coefficient(ews(residuals, rw),kw,0,N)
        Append[k,] = c(as.character(Stock$Date[istart]),Kendalls$var,Kendalls$spec)
        
      }
      
      filestr = c("data_files/",market,"_rolling_kt.txt")
      write.table(Append,paste(filestr,collapse=''), sep=',',append = T,col.names=F,row.names=F)
    }
  }
  
  if (file.exists(paste(c("data_files/",market,"_rolling_kt.txt"),collapse = "")) == F)
  {
    filestr = c("data_files/",market,"_rolling_kt.txt")
    write.table(rolling_kendall(Stock, rw, kw, N, bw),paste(filestr,collapse=''), sep=',',col.names=T,row.names=F)
  }
}

print(Sys.time()-Begin) 
print(Sys.Date())