#######################################################
#                                                     #
#   Script para hacer la reasignación de la ciudad    #    
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

catalogo <- read_excel('./inp/codigos-postales-de-mexico.xlsx') %>% rename(codigoPostal = Código)

delegaciones <- roster %>%
  left_join(catalogo)%>%
  mutate(Región2 = ifelse(Región == 4 & Municipio == 'Iztapalapa', 3,
                  ifelse(Región == 4 & Municipio != 'Iztapalapa', 16, Región)),
         Región = ifelse(is.na(Región2),16, Región2)) %>%
  select(c("zc","Región", "codigoPostal")) %>%
  distinct(codigoPostal, .keep_all = T)

write.csv(delegaciones, './out/inventarioCPRegiones_version14.csv', row.names = F)








