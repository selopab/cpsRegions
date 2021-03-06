#######################################################
#                                                     #
#   Script para hacer la reasignaci�n de la ciudad    #    
#                                                     #
#######################################################

rm(list=ls())


rm(list=ls())
require(stringr)
require(readxl)
require(xlsx)
require(tidyverse)

if (Sys.info()['user']=='joyce'){
   setwd('C:/Users/joyce/Dropbox/Documentos_/Joyce/regionesFebrero')
} else {
  setwd('D:/Dropbox/Dropbox/Documentos_/Joyce/regionesFebrero')
}


roster <-read.csv('./out/inventarioCPRegiones.csv', stringsAsFactors = F) %>%
  mutate(codigoPostal = as.character(zc),
         codigoPostal = ifelse(nchar(codigoPostal)==4, paste0('0', codigoPostal),codigoPostal))

catalogo <- read_excel('./inp/codigos-postales-de-mexico.xlsx') %>% rename(codigoPostal = C�digo)

delegaciones <- roster %>%
  left_join(catalogo)%>%
  mutate(Regi�n2 = ifelse(Regi�n == 4 & Municipio == 'Iztapalapa', 3,
                  ifelse(Regi�n == 4 & Municipio != 'Iztapalapa', 16, Regi�n)),
         Regi�n = ifelse(is.na(Regi�n2),16, Regi�n2)) %>%
  select(c("zc","Regi�n", "codigoPostal")) %>%
  distinct(codigoPostal, .keep_all = T)

write.csv(delegaciones, './out/inventarioCPRegiones_version14.csv', row.names = F)








