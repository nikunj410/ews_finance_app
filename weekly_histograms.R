#Written by Nikunj Goel, Department of Ecology and Evolutionary Biology,Yale University, 06511, New Haven, USA

Begin = Sys.time()
source("functions_stock_analysis.R")

N = 1000
bw = 25
rw = 500
kw = 250

rwrange=c( seq (375,625,by=20));# Length of rolling window. Default: from 375 to 625 in steps of 25
bwrange=c(seq (5,100,by=5));# Bandwith. Default: from 2.5 to 100 in steps of 2.5
N_sensitivity=1150; #length of stock-index to be analyzed Default: 1500 (6 years)
l_kw= c(seq(175,325,by = 15)) #length over which kendall is estimated. Default: from 175 to 325 in steps of 5
k_end = c(seq(0,200,by = 10))#Stand/end point of the kendall window. Default: from 0 to 200 in steps of 5

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
  Stock = read.csv(paste(c(market,"_data.txt"),collapse = ""),stringsAsFactors=FALSE)
  histograms = sensitivity_histograms(as.Date(tail(Stock$Date,1)),Stock,rwrange,bwrange,N_sensitivity,l_kw,k_end)
  
  filestr = c(market,"_histograms.txt")
  write.table(histograms,paste(filestr,collapse=''), sep=',',append = F,col.names=T,row.names=F)
  
}
print(Sys.time()-Begin) 