rm(list=ls())
require(stringr)
require(sf)
require(readxl)
require(xlsx)
require(tidyverse)

if (Sys.info()['user']=='joyce'){
  setwd('C:/Users/joyce/Dropbox/Documentos_/Joyce/regionesFebrero')
  inpMapas <- 'C:/Users/joyce/Dropbox/P15'  
} else {
  setwd('D:/Dropbox/Dropbox/Documentos_/Joyce/regionesFebrero')
  inpMapas <- 'D:/Dropbox/Dropbox/P15' 
}

registros <- read.csv('./inp/registros_desde_2019-08-01_al_2020-03-25_12T40.csv', stringsAsFactors = F) %>%
  mutate(zc = as.character(CP.GEO),
         zc = ifelse(str_length(zc)==4,paste0('0',zc),zc),
         numero = 1,
         zc = ifelse(zc == '11570', '11560', zc)) 

regiones <- read_excel('./inp/RegionesCP_2503.xlsx', sheet = 'Datos') %>%
  mutate(zc = as.character(CP),
         zc = ifelse(str_length(zc)==4,paste0('0',zc),zc),
         Regi�n = ifelse(zc == '14749', 5, Regi�n))
#%>%
 # select(c("Regi�n", "zc", "Etapa", "domicilios", "domicilios_group", "expedientes", "colonia", "delegaci�n"))

cargasOriginales <- regiones %>%
  group_by(Regi�n)%>%
  summarize(totalDomicilios = sum(domicilios),
            totalNotificaciones = sum(notificaciones) )

faltanDeEdgar <- subset(registros, !(zc %in% regiones$zc))$zc %>% unique()

aver <- subset(registros, zc %in% faltanDeEdgar)

poligonos <- st_read(paste(inpMapas, 'CP_CdMx/CP_09CdMx_v2.shp', sep='/'), stringsAsFactors = F) %>%
  rename(zc = d_cp) %>%
  left_join(regiones) %>%
  mutate(Regi�n = ifelse(zc %in% faltanDeEdgar,100,Regi�n),
    etapaAsignacion = 1)

faltantes <- subset(poligonos, Regi�n==100)
vecinos <- st_intersects(faltantes, poligonos, sparse = T)

for (i in 1:length(vecinos)){
  regionesVecinas <- poligonos[vecinos[[i]],]$Regi�n
  nvaRegion <- names(sort(table(as.factor(regionesVecinas)), decreasing=T)[1])
  faltantes[i,]$Regi�n = nvaRegion
  faltantes[i,]$Ronda = 2
}

for(codigo in faltantes$zc){
  rnva <- subset(faltantes, zc==codigo)$Regi�n
  poligonos <- poligonos %>%
    mutate(Regi�n = ifelse(zc == codigo, rnva, Regi�n),
           etapaAsignacion = ifelse(zc == codigo, 2, etapaAsignacion))
}


faltantes <- subset(poligonos, is.na(Regi�n))
vecinos <- st_intersects(faltantes, poligonos, sparse = T)

for (i in 1:length(vecinos)){
  regionesVecinas <- poligonos[vecinos[[i]],]$Regi�n
  nvaRegion <- names(sort(table(as.factor(regionesVecinas)), decreasing=T)[1])
  if(!is.null(nvaRegion)){
    faltantes[i,]$Regi�n = nvaRegion
  }
}

for(codigo in faltantes$zc){
  rnva <- subset(faltantes, zc==codigo)$Regi�n[1]
  poligonos <- poligonos %>%
    mutate(Regi�n = ifelse(zc == codigo, rnva, Regi�n),
           etapaAsignacion = ifelse(zc == codigo, 3, etapaAsignacion))
}

faltantes <- subset(poligonos, is.na(Regi�n))
vecinos <- st_intersects(faltantes, poligonos, sparse = T)

for (i in 1:length(vecinos)){
  regionesVecinas <- poligonos[vecinos[[i]],]$Regi�n
  nvaRegion <- names(sort(table(as.factor(regionesVecinas)), decreasing=T)[1])
  if(!is.null(nvaRegion)){
    faltantes[i,]$Regi�n = nvaRegion
  }
}

for(codigo in faltantes$zc){
  rnva <- subset(faltantes, zc==codigo)$Regi�n[1]
  poligonos <- poligonos %>%
    mutate(Regi�n = ifelse(zc == codigo, rnva, Regi�n),
           etapaAsignacion = ifelse(zc == codigo, 3, etapaAsignacion))
}

poligonos <- poligonos %>%
  mutate(Regi�n = ifelse(is.na(Regi�n) | Regi�n==100, 16, Regi�n),
         Regi�n = ifelse(zc == '01310', '9', Regi�n),
         Regi�n = ifelse(zc == '06500', '11', Regi�n),
         Regi�n = ifelse(zc == '09480', '3', Regi�n),
         Regi�n = ifelse(zc == '01060', '10', Regi�n),
         Regi�n = ifelse(zc == '11310', '13', Regi�n),
    division = as.factor(Regi�n))

st_write(poligonos, './out/regionesMarzo25.shp', delete_dsn = T)

nostannunca <- registros %>%
                subset(zc %in% faltanDeEdgar & !(zc %in% poligonos$zc) & zc!=0)

cargaNostan <- nostannunca %>%
  distinct(zc, DOMICILIO, .keep_all=T)%>%
  group_by(DOMICILIO, zc) %>%
  summarize(totalDomicilios = sum(numero)) %>%
  ungroup() %>%
  group_by(zc) %>%
  summarize(totalDomicilios = sum(totalDomicilios))

estan <- subset(poligonos, zc %in% faltanDeEdgar)

mapa <- ggplot() +
  geom_sf(data=subset(poligonos), aes(fill = division)) +
  ggtitle(paste('CDMX por regiones')) +
  theme_void() +
  coord_sf(datum = NA)

ggsave(paste0('./maps/Regiones/CDMX.pdf'), mapa, device='pdf')

for (i in c(1:3, 5:16, 100)){
  subconjunto = subset(poligonos, Regi�n==eval(i))
  cpsub <- st_centroid(subconjunto) 
  cpsub <- cbind(cpsub, st_coordinates(st_centroid(subconjunto$geometry)))
  
  mapa <- ggplot() +
    geom_sf(data=subconjunto, aes(fill = Delegaci�n)) +
    ggtitle(paste('Regi�n', eval(i))) +
    geom_text(data=cpsub, aes(x=X, y=Y, label=zc), size=1) +
    theme_void() +
    coord_sf(datum = NA)
  
  ggsave(paste0('./maps/Regiones/Region',eval(i),'.pdf'), mapa, device='pdf')
}

cargaXCP <- as.data.frame(poligonos) %>%
  dplyr::select(-geometry) %>%
  mutate(domicilios = ifelse(is.na(domicilios),0,domicilios))%>%
  group_by(Regi�n) %>%
  summarize(totalDomicilios = sum(domicilios)) 

casiCompleto <-as.data.frame(poligonos) %>%
  dplyr::select(-c( domicilios, domicilios_group, notificaciones, geometry, etapaAsignacion))

write.xlsx(casiCompleto, './out/Regiones25032020.xlsx', row.names = F)

roster <- as.data.frame(poligonos) %>%
  dplyr::select(c(zc, Regi�n)) %>%
  unique()

write.csv(roster, './out/inventarioCPRegiones.csv')
