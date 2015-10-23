#Written by Nikunj Goel, Department of Ecology and Evolutionary Biology,Yale University, 06511, New Haven, USA

Begin = Sys.time()
source("functions_stock_analysis.R")
library(devtools)
library(Quandl)
my_id = c("UE_H66ZRgM7n_4wMs-DQ")
print(Sys.Date())

## Missing data sets Ireland, Italy, UK, Ecuador, Philippines, India (NIFTY), Malaysia and Israel
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
                  "FTSE_100_Index_UK",
                  "FTSE_MIB_Index_Italy",
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
                  "Tel_Aviv_100_Index_Israel",
                  "Amman_General_Index_Jordan",
                  "Straits_Times_Index_Singapore",
                  "Colombo_All_Shares_Index_Sri_Lanka",
                  "Taiwan_Weighted_Index_Taiwan",
                  "NZSE_50_Index_New_Zealand",
                  "New_York_Stock_Exchange_USA",
                  "Johannesburg_Stock_Exchange_South_Africa")


###Kuala Lumpur Composite Index and S&P Nifty Index not found ####
market = c(Stock_Markets[1])
Stock=Quandl("YAHOO/INDEX_GSPC", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[2])
Stock=Quandl("NASDAQOMX/COMP", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Trade Date","Index Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[3])
Stock=Quandl("BCB/UDJIAD1", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[4])
Stock=Quandl("YAHOO/INDEX_RUI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[5])
Stock=Quandl("YAHOO/INDEX_W5000", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[6])
Stock=Quandl("YAHOO/INDEX_SSEC", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[7])
Stock=Quandl("YAHOO/INDEX_HSI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[8])
Stock=Quandl("NIKKEI/INDEX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close Price")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[9])
Stock=Quandl("YAHOO/INDEX_GDAXI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[10])
Stock=Quandl("YAHOO/INDEX_FCHI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[11])
Stock=Quandl("BCB/7", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Value")
copypastedata(market,Stock,keeps)
print(market)

# market = c(Stock_Markets[12])
# Stock=Quandl("YAHOO/INDEX_FTSE", authcode="UE_H66ZRgM7n_4wMs-DQ")
# keeps <- c("Date","Close")
# copypastedata(market,Stock,keeps)
# print(market)

# market = c(Stock_Markets[13])
# Stock=Quandl("", authcode="UE_H66ZRgM7n_4wMs-DQ")
# keeps <- c("Date","Close")
# copypastedata(market,Stock,keeps)
# print(market)

market = c(Stock_Markets[14])
Stock=Quandl("YAHOO/INDEX_RTS_RS", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[15])
Stock=Quandl("YAHOO/INDEX_BSESN", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[16])
Stock=Quandl("YAHOO/INDEX_GSPTSE", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[17])
Stock=Quandl("YAHOO/INDEX_AORD", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[18])
Stock=Quandl("YAHOO/INDEX_IBEX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[19])
Stock=Quandl("YAHOO/INDEX_MXX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[20])
Stock=Quandl("YAHOO/INDEX_KS11", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[21])
Stock=Quandl("YAHOO/INDEX_JKSE", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[22])
Stock=Quandl("YAHOO/INDEX_MERV", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[23])
Stock=Quandl("YAHOO/INDEX_ATX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[24])
Stock=Quandl("YAHOO/INDEX_BFX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[25])
Stock=Quandl("ZAGREBSE/CROBEX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Index")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[26])
Stock=Quandl("PRAGUESE/PX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Index")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[27])
Stock=Quandl("NASDAQOMX/OMXC20", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Trade Date","Index Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[28])
Stock=Quandl("NASDAQOMX/OMXTGI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Trade Date","Index Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[29])
Stock=Quandl("NASDAQOMX/OMXH25", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Trade Date","Index Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[30])
Stock=Quandl("YAHOO/INDEX_GD_AT", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[31])
Stock=Quandl("BUDAPESTSE/BUX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[32])
Stock=Quandl("NASDAQOMX/OMXIGI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Trade Date","Index Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[33])
Stock=Quandl("NASDAQOMX/OMXRGI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Trade Date","Index Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[34])
Stock=Quandl("NASDAQOMX/OMXVGI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Trade Date","Index Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[35])
Stock=Quandl("LUXSE/LUXGR", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[36])
Stock=Quandl("YAHOO/INDEX_AEX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[37])
Stock=Quandl("NASDAQOMX/OMXO20GI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Trade Date","Index Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[38])
Stock=Quandl("BUCHARESTSE/INDICES", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","BET")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[39])
Stock=Quandl("BELGRADESE/BELEX15", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[40])
Stock=Quandl("LJUBSE/INDEX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Index")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[41])
Stock=Quandl("NASDAQOMX/OMXS30", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Trade Date","Index Value")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[42])
Stock=Quandl("YAHOO/INDEX_SSMI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[43])
Stock=Quandl("UKRSE/UX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)
# Isreal
# market = c(Stock_Markets[44])
# Stock=Quandl("YAHOO/INDEX_TA100", authcode="UE_H66ZRgM7n_4wMs-DQ")
# keeps <- c("Date","Close")
# copypastedata(market,Stock,keeps)
# print(market)

market = c(Stock_Markets[45])
Stock=Quandl("AMMANSE/INDEX_GENERAL_INDEX", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Index")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[46])
Stock=Quandl("YAHOO/INDEX_STI", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[47])
Stock=Quandl("YAHOO/INDEX_CSE", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[48])
Stock=Quandl("YAHOO/INDEX_TWII", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[49])
Stock=Quandl("YAHOO/INDEX_NZ50", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[50])
Stock=Quandl("YAHOO/INDEX_NYA", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)

market = c(Stock_Markets[51])
Stock=Quandl("GOOG/JSE_JSE", authcode="UE_H66ZRgM7n_4wMs-DQ")
keeps <- c("Date","Close")
copypastedata(market,Stock,keeps)
print(market)


print(Sys.time()-Begin) 
print(Sys.Date())
