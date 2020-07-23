##############################################################3
# Este script toma los datos que nos dieron en la junta y los datos de Edr para hacer una primera versión de la división de la ciudad

#Su principal output es tablaPJoyce.csv que sirve para ver la carga de trabajo por CP


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

registros <- read.csv('./inp/registros_desde_2019-08-01_al_2020-03-25_12T40.csv', stringsAsFactors = F) %>%
  mutate(zc = as.character(CP.GEO),
         zc = ifelse(str_length(zc)==4,paste0('0',zc),zc),
         numero = 1) 

regiones <- read_excel('./inp/CATALOGO_CON_COLONIA_CODIGO_POSTAL.xlsx') %>%
  mutate(zc = as.character(CP),
         zc = ifelse(str_length(zc)==4,paste0('0',zc),zc)) 


#%>%
#  mutate(`División original` = ifelse(Delegación == 'Tlalpan',16,`División original`))


subconjunto <- regiones %>%
                subset(zc %in% unique(regiones$zc)[nostan])

temp1 <- registros %>%
  distinct(zc, DOMICILIO, .keep_all=T)%>%
  group_by(DOMICILIO, zc) %>%
  summarize(totalDomicilios = sum(numero)) %>%
  ungroup() %>%
  group_by(zc) %>%
  summarize(totalDomicilios = sum(totalDomicilios))

cargaXCP <- registros %>%
  group_by(zc) %>%
  #mutate(renglon = row_number(),
   #      numero = ifelse(renglon == 1,  numero, 0)) %>%
  summarize(totalNotificaciones = sum(numero)) %>%
  inner_join(temp1, by = 'zc')



nostan <- !(unique(regiones$zc) %in% registros$zc)
nostan2 <- !(registros$zc %in%  regiones$zc)

cargaTrabajo <- inner_join(regiones, cargaXCP, by = 'zc') %>%group_by(zc) %>%
                mutate(renglon = row_number(),
                numero = ifelse(renglon == 1,  numero, 0)) %>%
                ungroup() %>%
                group_by(`División original`) %>%
                summarize(totalDomicilios = sum(totalDomicilios),
                          totalNotificaciones = sum(totalNotificaciones))


tablaPJpyce <- inner_join(regiones, cargaXCP, by = 'zc') %>%
  select(c(`División original`, zc, totalDomicilios, totalNotificaciones, Colonia, Delegación)) %>%
  group_by(zc) %>%
  mutate(renglon = row_number(),
         totalDomicilios = ifelse(renglon == 1,  totalDomicilios, 0),
         totalNotificaciones = ifelse(renglon == 1,  totalNotificaciones, 0)) %>%
  rename(Región = `División original`, CP = zc, domicilios = totalDomicilios, expedientes = totalNotificaciones)
  


write.csv(tablaPJpyce, './out/tablaPJoyce.csv',row.names = F)

poligonos <- st_read(paste(inpMapas, 'CP_CdMx/CP_09CdMx_v2.shp', sep='/'), stringsAsFactors = F) %>%
  rename(zc = d_cp)



temporal <- subset(regiones, `División original`==13 | `División original`==11 | `División original`==6 | `División original`==2)


pol <- poligonos %>%
  inner_join(temporal) %>%
  mutate(division = as.factor(`División original`))

cps <- st_centroid(pol) 
cps <- cbind(cps, st_coordinates(st_centroid(pol$geometry)))

ggplot() +
  geom_sf(data=pol, aes(fill = division))  +
  geom_text(data=cps, aes(x=X, y=Y, label=CP), size=1)




