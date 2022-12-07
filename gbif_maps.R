### content: GBIF Übung


library(rgbif)
library(data.table)
library(sf)
library(mapview)



# Beispiel# Schlickgras-Gesellschaften
# Kennarten: "Spartina maritima", "Spartina anglica", "Spartina townsendii"

speclist <- c("Spartina maritima", "Spartina anglica", "Spartina townsendii")


keys <- vapply(speclist, function(x) name_suggest(x)$data$key[1], 
               numeric(1), USE.NAMES=FALSE)



schlickgras <- occ_data(taxonKey  = keys, country="DE", hasCoordinate = TRUE, limit = 20000, datasetKey="e6fab7b3-c733-40b9-8df3-2a03e49532c1")

schlickgras # kompliziertes Format

schlickgras_df<- rbindlist(lapply(schlickgras, function(x) x$data), fill = TRUE, use.names = TRUE)

schlickgras_df <- schlickgras_df[,c("scientificName","decimalLatitude","decimalLongitude")]

table(schlickgras_df$scientificName)

# Doppelungen löschen
schlickgras_df <- schlickgras_df[!duplicated(schlickgras_df),]



######################################################
# 2. Anschauen der einzelnen Ergebnisse auf der Karte 
######################################################


# Umwandeln in SimpleFeature

schlickgras_df_geo <- st_as_sf(schlickgras_df, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(4326))

# plotten der einzelnen Arten
unique(schlickgras_df_geo$scientificName)

plot(schlickgras_df_geo[schlickgras_df_geo$scientificName=="Spartina anglica C.E.Hubb.",])
plot(schlickgras_df_geo[schlickgras_df_geo$scientificName=="Spartina ×townsendii H.Groves & J.Groves",])

# auf Karte plotten
mapview(schlickgras_df_geo[schlickgras_df_geo$scientificName=="Spartina anglica C.E.Hubb.",], map.types="OpenStreetMap")
mapview(schlickgras_df_geo[schlickgras_df_geo$scientificName=="Spartina ×townsendii H.Groves & J.Groves",], map.types="OpenStreetMap")




#####################################################
#################### Zählen der Kombination #########

# schreiben der Koordinaten in einen Data Frame
df <- st_coordinates(schlickgras_df_geo)
df <- as.data.frame(df)

# zählen der Doppelungen
df2 <-  setDT(df)[,list(value=.N),names(df)]
df2 <- as.data.frame(df2)

# plotten der Anzahl
schlickgras_count <- st_as_sf(df2, coords = c("X", "Y"), crs = st_crs(4326))

mapview(schlickgras_count, map.types="OpenStreetMap")





