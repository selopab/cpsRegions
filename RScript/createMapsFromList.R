# Este script toma un archivo ya con asignaciones de regiones y genera dos outputs: 

# 1) un mapa de cada región
# 2) una tabla con la carga de trabajo por cp que alimenta la tabla dinámica que se usó para la asignación manual

rm(list=ls())
require(tidyverse)
require(stringr)
require(sf)
require(readxl)

if (Sys.info()['user']=='joyce'){
  setwd('C:/Users/joyce/Dropbox/Documentos_/Joyce/regionesFebrero')
  inpMapas <- 'C:/Users/joyce/Dropbox/P15'  
} else {
  setwd('D:/Dropbox/Dropbox/Documentos_/Joyce/regionesFebrero')
  inpMapas <- 'D:/Dropbox/Dropbox/P15' 
}

registros <- read.csv('./inp/registros_desde_2019-08-01_al_2020-02-03_12T44.csv', stringsAsFactors = F) %>%
  mutate(zc = as.character(CP.GEO),
         zc = ifelse(str_length(zc)==4,paste0('0',zc),zc),
         numero = 1) 

regiones <- read_excel('./out/Regiones10022020.xlsx') 


#%>%
#  mutate(zc = as.character(CP),
#         zc = ifelse(str_length(zc)==4, paste0('0',zc),zc))


temp1 <- registros %>%
  distinct(zc, DOMICILIO, .keep_all=T)%>%
  group_by(DOMICILIO, zc) %>%
  summarize(totalDomicilios = sum(numero)) %>%
  ungroup() %>%
  group_by(zc) %>%
  summarize(totalDomicilios = sum(totalDomicilios))

cargaXCP <- registros %>%
  group_by(zc) %>%
  summarize(totalNotificaciones = sum(numero)) %>%
  inner_join(temp1, by = 'zc')


cargaTrabajo <- inner_join(regiones, cargaXCP, by = 'zc') %>%
  group_by(Región) %>%
  summarize(totalDomicilios = sum(totalDomicilios),
            totalNotificaciones = sum(totalNotificaciones))


tablaPJpyce <- inner_join(regiones, cargaXCP, by = 'zc') %>%
  select(c(`División original`, zc, totalDomicilios, totalNotificaciones, Colonia, Delegación)) %>%
  group_by(zc) %>%
  mutate(renglon = count()) %>%
  rename(Región = `División original`, CP = zc, domicilios = totalDomicilios, expedientes = totalNotificaciones)



write.csv(tablaPJpyce, './out/tablaPJoyce.csv',row.names = F)

poligonos <- st_read(paste(inpMapas, 'CP_CdMx/CP_09CdMx_v2.shp', sep='/'), stringsAsFactors = F) %>%
  rename(zc = d_cp)


listas_regiones <- list(c(13,15))
  
  #list(c(2, 6, 11, 13), c(10, 4, 5), c(1, 2, 8), c(10, 12, 14), 16, 7)

for (i in c(1:length(listas_regiones))){
  regionesMapa <- listas_regiones[[i]]
  total <- length(regionesMapa)
  min <- min(regionesMapa)
  temporal <- subset(regiones, Región %in% regionesMapa)
  
  
  pol <- poligonos %>%
    inner_join(temporal) %>%
    mutate(division = as.factor(Región))
  
  cps <- st_centroid(pol) 
  cps <- cbind(cps, st_coordinates(st_centroid(pol$geometry)))
  
 mapa <-  ggplot() +
    geom_sf(data=pol, aes(fill = division))  +
    geom_text(data=cps, aes(x=X, y=Y, label=zc), size=0.5)
 
 ggsave(paste0('./maps/Region',eval(total),'-',eval(min),'.pdf'), mapa, device='pdf')
  
  }








