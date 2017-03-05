ustvari_tabelo <- function(ime) {
  naslov <- paste('podatki/csv/', ime, '.csv', sep='')
  return(read.table(naslov, sep = ",", skip = 1, as.is = TRUE,
                    fileEncoding = "Windows-1250"
  ))
}

fund <- ustvari_tabelo('fundamentals')[,-1]
price <-ustvari_tabelo('prices')[,-1]
trade <-ustvari_tabelo('trade_data')[,-1]
sum <-ustvari_tabelo('summ')[,-1]
fin<-ustvari_tabelo('financial')[,-1]

#imena stolpcev!!

