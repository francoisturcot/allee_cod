# ------------------------------------------------------------------
# Cod stock-unit map: dissolve statistical polygons into stock units
# ------------------------------------------------------------------
library(sf); library(dplyr); library(ggplot2); library(rnaturalearth)
library(ggrepel)

sf_use_s2(FALSE)

## 1. ICES Areas (live REST service, WGS84) ------------------------
ices <- st_read(list.files("data/spatial", "\\.shp$",
                           recursive = TRUE, full.names = TRUE)[1],
                quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid()

stopifnot("Area_27" %in% names(ices))

ices_key <- list(
  "BA2224"        = c("3.c.22","3.b.23","3.d.24"),
  "BA2532"        = paste0("3.d.", 25:32),
  "FAPL"          = c("5.b.1.a","5.b.1.b"),
  "ICE"           = c("5.a.1","5.a.2"),
  "IIIaW-IV-VIId" = c("3.a.20","4.a","4.b","4.c","7.d"),
  "IS"            = "7.a",
  "NEAR"          = c("1.a","1.b","2.a.1","2.a.2","2.b.1","2.b.2"),
  "VIa"           = "6.a",
  "VIIek"         = c("7.e","7.f","7.g","7.h","7.j.1","7.j.2","7.k.1","7.k.2"))

setdiff(unlist(ices_key), unique(ices$Area_27))
ices_key[["NEAR"]] <- c("1.a","1.b","2.a.1","2.a.2","2.b.1","2.b.2")
ices_key[["BA2532"]] <- c("3.d.25","3.d.26","3.d.27",
                          "3.d.28.1","3.d.28.2",
                          "3.d.29","3.d.30","3.d.31","3.d.32")

# sanity check that every code in the key actually matches a polygon

setdiff(unlist(ices_key), unique(ices$Area_27))   # expect character(0)

## 2. NAFO Divisions 2021 (DFO / NAFO Secretariat) -----------------
zip <- tempfile(fileext = ".zip"); dir <- tempfile(); dir.create(dir)
download.file(paste0("https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/",
                     "59af1c96-fc8f-4fa0-b398-d65e953eadaa/attachments/NAFO_Divisions_2021_SHP.zip"),
              zip, mode = "wb"); unzip(zip, exdir = dir)
nafo <- st_read(grep("poly_clipped\\.shp$", 
                     list.files(dir, "\\.shp$", recursive = TRUE, full.names = TRUE),
                     value = TRUE),
                quiet = TRUE) %>%
  st_transform(4326) %>% st_make_valid()

names(nafo)

ncol_id <- names(nafo)[which.max(sapply(nafo, function(x)
  sum(grepl("^3L$|^4Vn$|^2J$", as.character(x)))))]
nafo$code <- as.character(nafo[[ncol_id]])
ncol_id
grep("^5", sort(unique(nafo$code)), value = TRUE)
# auto-detect the division-code column (values like "3L", "4Vn")
ncol_id <- names(nafo)[which.max(sapply(nafo, function(x)
  sum(grepl("^3L$|^4Vn$|^2J$", as.character(x)))))]
nafo$code <- as.character(nafo[[ncol_id]])

nafo$Label

## 3. US stocks: GARFO statistical areas (download once, edit path)
zip2 <- tempfile(fileext = ".zip"); dir2 <- tempfile(); dir.create(dir2)
download.file("https://storage.googleapis.com/nmfs_odp_nefsc/GIS_data/NEFSC_GIS.zip",
              zip2, mode = "wb")
unzip(zip2, exdir = dir2)

shp <- list.files(dir2, "\\.shp$", recursive = TRUE, full.names = TRUE)
basename(shp)   # find the statistical-areas layer, then st_read() it
us <- st_read(grep("Statistical_Areas_2010_withNames\\.shp$", shp, value = TRUE),
              quiet = TRUE) %>%
  st_transform(4326) %>% st_make_valid()

names(us)
head(us[, sapply(us, is.character) | sapply(us, is.numeric)], 3)

us$code <- as.character(us$Id)          # <- check names(us); stat-area field
us_key <- list(   # 90 FR 11246, Table 1
  EGOM = c("511","512", "465", "467"),
  WGOM = c("513","514","515","521","526","541"),
  GB   = c("522","525","542", "543", "551","552","561","562", "464"),
  SNE  = c("533","534","537","538","539", as.character(611:616),
           as.character(621:629), as.character(631:639)))

## 4. Stock definitions --------------------------------------------
nafo_key <- list(
  "3M" = c("3M"),
  "2J3KL"  = c("2J","3K","3L"),        "3Pn4RS" = c("3Pn","4R","4S"),
  "3Ps"    = "3Ps",                    "4TVn"   = c("4T","4Vn"),
  "4VsW"   = c("4Vs","4W"),            "4X5Yb"   = c("4X"), #its actually 5Yb, its only a small portion of 5. just show 4X for the display
  "5Zjm"   = c("5Zc"), #5Zjm is a small subdivision. available are 5Ze , c, e, u or w. 
  "3NO"    = c("3N","3O"),
  "1IN"    = c("1A","1B","1C","1D","1E"))

ices_key <- list(
  "BA2224" = c("3.c.22","3.b.23","3.d.24"),
  "BA2532" = paste0("3.d.", 25:32),
  "FAPL"   = c("5.b.1.a","5.b.1.b"),   "ICE" = c("5.a.1","5.a.2"),
  "IIIaW-IV-VIId" = c("3.a.20","4.a","4.b","4.c","7.d"),
  "IS"     = "7.a",
  "NEAR"   = grep("^1\\.|^2\\.", unique(ices$Area_27), value = TRUE),
  "VIa"    = c("6.a"),
  "VIIek"  = c("7.e","7.f","7.g","7.h","7.j.1","7.j.2","7.k.1","7.k.2"))

dissolve <- function(layer, field, codes, id) {
  g <- layer[as.character(layer[[field]]) %in% codes, ]
  if (nrow(g) == 0) return(NULL)
  st_sf(stock_id = id, geometry = st_union(st_geometry(g)))
}

stocks <- bind_rows(
  do.call(rbind, Map(function(k, v) dissolve(nafo, "code", v, k),
                     names(nafo_key), nafo_key)),
  do.call(rbind, Map(function(k, v) dissolve(ices, "Area_27", v, k),
                     names(ices_key), ices_key)),
  do.call(rbind, Map(function(k, v) dissolve(us, "code", v, k),
                     names(us_key), us_key)),
  
  st_sf(stock_id = "1F-XIV",
        geometry = st_union(c(st_geometry(st_union(nafo[nafo$code == "1F", ])),
                              st_geometry(st_union(
                                ices[grepl("^14\\.", ices$Area_27), ])))))
) %>% st_make_valid()

table(st_geometry_type(stocks))   # expect POLYGON / MULTIPOLYGON now
nrow(stocks)                      # expect 24 
stocks$stock_id


## 5. Join your grading variable -----------------------------------
grades <- data.frame(stock_id = stocks$stock_id,
                     value = NA_real_)      # <- replace with your metric
names(grades)
grades$stock_id
stocks <- left_join(stocks, grades, by = "stock_id")

## 6. Map ----------------------------------------------------------
prj  <- "+proj=laea +lat_0=62 +lon_0=-15 +datum=WGS84 +units=m"
land <- ne_countries(scale = "medium", returnclass = "sf") %>% st_transform(prj)
sp   <- st_transform(stocks, prj)
bb   <- st_bbox(sp)
pad  <- 0.03 * c(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"])

ctr <- sp %>%
  mutate(lon = st_coordinates(st_point_on_surface(geometry))[, 1],
         lat = st_coordinates(st_point_on_surface(geometry))[, 2])

outline <- stocks %>% st_transform(prj) %>%
  group_by(stock_id) %>% summarise(.groups = "drop")

str(sp)
head(sp)
unique(sp$stock_id)


# stocks map


p = ggplot() +
  geom_sf(data = land, fill = "grey92", colour = "grey60", linewidth = 0.2) +
  geom_sf(data = sp, aes(fill = value), colour = NA, alpha = 0.85) +
  geom_sf(data = outline, fill = NA, colour = "grey25", linewidth = 0.35) +
  geom_label_repel(data = ctr, aes(lon, lat, label = stock_id),
                   size = 2.4, label.size = 0.15,
                   label.padding = unit(0.12, "lines"),
                   fill = "white", alpha = 0.85, box.padding = 0.35,
                   segment.size = 0.25, segment.colour = "grey40",
                   min.segment.length = 0, max.overlaps = Inf) +
  scale_fill_viridis_c(option = "C", na.value = "grey80", name = "Grade") +
  coord_sf(xlim = c(bb["xmin"] - pad[1], bb["xmax"] + pad[1]),
           ylim = c(bb["ymin"] - pad[2], bb["ymax"] + pad[2]), expand = FALSE) +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_line(colour = "grey90", linewidth = 0.2))+
  xlab("Longitude")+
  ylab("Latitude")



ggsave(
  "figures/map_stocks.tiff",
  plot = p,
  width = 170,
  height = 100,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

p

#ggsave("figures/map_stocks.png", width = 7, height = 4)


### proper names here are 

# 4X5Yb

# 5Zjm











## map Allee effects




library(ggrepel)

allee <- data.frame(
  stock_id = c("4TVn","4VSw","GB","2J3KL","WGOM","3Ps","1IN","3Pn4RS","4X5Yb",
               "3NO","5Zjm","EGOM","1F-XIV","3M","SNE",
               "IIIaW-IV-VIId","ICE","BA2224","NEAR","BA2532","FAPL","VIIek",
               "VIa","IS"),
  allee    = c(rep("Yes", 9), rep("No", 6), "Yes", rep("No", 8)),
  As       = c(1.22, 0.92, 0.92, 0.89, 0.80, 0.575, 0.53, NA, NA,
               NA, NA, NA, NA, NA, NA,
               0.57, NA, NA, NA, NA, NA, NA, NA, NA))

sp <- sp %>%
  dplyr::mutate(key = tolower(stock_id)) %>%
  dplyr::left_join(
    allee %>% dplyr::mutate(key = tolower(stock_id)) %>% dplyr::select(-stock_id),
    by = "key")

stopifnot(!anyNA(sp$allee))   # fails if a stock in the map has no table row

# lab <- sp %>% filter(!is.na(As)) %>%
#   mutate(lon = st_coordinates(st_point_on_surface(geometry))[, 1],
#          lat = st_coordinates(st_point_on_surface(geometry))[, 2])

lab <- sp %>%
  dplyr::filter(!is.na(As)) %>%
  dplyr::mutate(lon = sf::st_coordinates(sf::st_point_on_surface(geometry))[, 1],
                lat = sf::st_coordinates(sf::st_point_on_surface(geometry))[, 2])

sp <- sp %>%
  sf::st_collection_extract("POLYGON") %>%
  sf::st_cast("MULTIPOLYGON")

p2 = ggplot() +
  geom_sf(data = land, fill = "grey92", colour = "grey60", linewidth = 0.2) +
  geom_sf(data = sp, aes(fill = allee), colour = "grey25", linewidth = 0.35,
          alpha = 0.75) +
  geom_label_repel(data = lab, aes(lon, lat, label = formatC(As, format = "f",
                                                             digits = 2)),
                   size = 2.6, label.size = 0.2, fill = "white",
                   alpha = 0.9, box.padding = 0.3, min.segment.length = 0) +
  scale_fill_manual(values = c(Yes = "#C0392B",  No = "#2C7BB6"),
                    na.value = "grey85", name = "Allee effect") +
  coord_sf(xlim = c(bb["xmin"] - pad[1], bb["xmax"] + pad[1]),
           ylim = c(bb["ymin"] - pad[2], bb["ymax"] + pad[2]), expand = FALSE) +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_line(colour = "grey90", linewidth = 0.2))+
  xlab("Longitude")+
  ylab("Latitude")

#ggsave("figures/map_allee.png", width = 7, height = 4)
ggsave(
  "figures/map_allee.tiff",
  plot = p2,
  width = 170,
  height = 100,
  units = "mm",
  dpi = 600,
  compression = "lzw"
)

