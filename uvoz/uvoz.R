##
library(XML)


uvoz_tabele<- function(ime){
  naslov <- paste('podatki/html/', ime, '.html', sep='')
  tabela <- readHTMLTable(naslov, which=2, stringsAsFactors = FALSE)
}

financial <- uvoz_tabele("financial")[,-7]
fundamentals <- uvoz_tabele("fundamentals")[,-7]
prices <- uvoz_tabele("prices")[,-8]
summ <- uvoz_tabele("summary")[,-8]
trade_data <- uvoz_tabele("trade_data")[,-8]

#urejanje podatkov!!!

write.csv(financial, 'podatki/csv/financial.csv')
write.csv(fundamentals, 'podatki/csv/fundamentals.csv')
write.csv(prices, 'podatki/csv/prices.csv')
write.csv(summ, 'podatki/csv/summ.csv')
write.csv(trade_data, 'podatki/csv/trade_data.csv')
