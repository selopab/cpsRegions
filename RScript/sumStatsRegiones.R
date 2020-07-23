##########################
# Sum stats regiones CDA #
##########################

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

regiones <- st_read('./out/regionesMarzo25.shp', stringsAsFactors = F) 

JLCA <- st_sfc(st_point(c(-99.145494, 19.424627))) %>% st_set_crs(4326) 

sumstats <- regiones %>%
  mutate(area = st_area(.)) %>%
  distinct(zc, .keep_all = T) %>%
  mutate(area = area/(1000*1000)) %>%
  group_by(Región) %>%
  summarise(areaRegion = sum(area),
            domicilios = sum(domicls, na.rm=T),
            expedientes = sum(expdnts, na.rm=T),
            geometry = st_union(geometry)) %>%
  st_transform(4326)

sumstats2 <- st_centroid(sumstats) %>%
  mutate(distanciaJLCA = st_distance(geometry, JLCA)/1000)

tabla <- as.data.frame(sumstats2) %>%
  dplyr::select(-geometry)%>%
mutate(areaRegion = as.numeric(areaRegion),
       distanciaJLCA = as.numeric(distanciaJLCA))


write.csv(tabla, './out/sumstatsRegiones.csv', row.names = F)

temp <- sumstats2 %>%
  dplyr::select(c(Región, geometry))

cpsub <- cbind(temp, st_coordinates(st_centroid(temp$geometry)))

mapa <- ggplot() +
  geom_sf(data=subset(sumstats), aes(fill = Región)) +
  geom_text(data=cpsub, aes(x=X, y=Y, label=Región), size=5) + 
  ggtitle(paste('CDMX por regiones')) +
  theme_void() +
  coord_sf(datum = NA)

ggsave(paste0('./maps/Regiones/CDMX_v2.pdf'), mapa, device='pdf')





