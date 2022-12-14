### content: GBIF Übung


library(rgbif)
library(data.table)
library(sf)
library(mapview)



# Beispiel# Schlickgras-Gesellschaften
# Kennarten: "Spartina maritima", "Spartina anglica", "Spartina townsendii"
speclist <- c("Spartina maritima", "Spartina anglica", "Spartina townsendii")

# Beispiel Borstgrasrasen /entfernt: "Holcus lanatus", "Avenella flexuosa", "Festuca ovina"
speclist <- c("Nardus stricta", "Danthonia decumbens", "Potentilla erecta", "Carex pilulifera", "Luzula campestris", "Antennaria dioica") 

#  diese Funktion soll dabei helfen die richtige Art zu finden
keys <- vapply(speclist, function(x) name_suggest(x)$data$key[1], numeric(1), USE.NAMES=FALSE)


# Beispiel Schwermetall
speclist <- c("Minuartia caespitosa", "Silene vulgaris subsp. vulgaris", "Armeria maritima") 
# können nicht mit der Funktion automatisiert ausgelesen werden, da es Armeria maritima von zwei Autoren gibt. Daher lieber gleich die Keys angeben
keys <- c("7509175", "5587240","8930531")



# auslesen der Daten aus GBIF. Für Borstgrasrasen dauert das sehr lange. 
gbif_species <- occ_data(taxonKey =keys, country="DE", hasCoordinate = TRUE, limit = 30000, datasetKey="e6fab7b3-c733-40b9-8df3-2a03e49532c1")

# Aus GBIF wird eine verschachtelte Liste exportiert, mit dieser Funktion macht man einen Data-Frame draus. 
gbif_species_df<- rbindlist(lapply(gbif_species, function(x) x$data), fill = TRUE, use.names = TRUE)

# auswählen von Koordinaten und Artname
gbif_species_df <- gbif_species_df[,c("species","decimalLatitude","decimalLongitude")]

# anzeigen, wie viele Vorkommen gefunden wurden
table(gbif_species_df$species)

# Doppelungen löschen
gbif_species_df <- gbif_species_df[!duplicated(gbif_species_df),]


###########################################################################
#################### Zählen der Kombination und plotten auf Karte #########

# Umwandeln in SimpleFeature Geodaten

gbif_species_df_geo <- st_as_sf(gbif_species_df, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(4326))

# schreiben der Koordinaten in einen Data Frame
df <- st_coordinates(gbif_species_df_geo)
df <- as.data.frame(df)

# zählen der Doppelungen
df2 <-  setDT(df)[,list(value=.N),names(df)]
df2 <- as.data.frame(df2)

table(df2$value)

# Anzeigen aller Arten der Pflanzengesellschaft, oder fast alle Arten (Anzahl-1)
df2 <- df2[df2$value>=(max(df2$value)-1),] 
df2$value <- as.factor(df2$value)

# plotten der Anzahl
df_count <- st_as_sf(df2, coords = c("X", "Y"), crs = st_crs(4326))

mapview(df_count, map.types="OpenStreetMap", cex = 2 )


####






