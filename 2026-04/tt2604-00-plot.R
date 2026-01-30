# Packages
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(showtext)
library(sysfonts)

# Tidy Tuesday Data
tuesdata <- tidytuesdayR::tt_load(2026, week = 4)

nfirms_top50 <- 
  tuesdata %>% 
  "[["("companies") %>% 
  # filter(capital_stock<999999999999) %>% # optional: remove top coded data
  arrange(desc(capital_stock)) %>%
  mutate(rank=row_number(),
         topshare =cumsum(capital_stock)/sum(capital_stock),
         percentile = rank/max(rank)) %>% 
  mutate(dif=topshare-0.5) %>% # find observations closest to 50 percent
  filter(dif>0) %>% # difference must be positive (not the same as absolute difference)
  filter(dif==min(dif)) %>% # smallest difference
  pull(rank)

nfirms_rest <-
  tuesdata %>% 
  "[["("companies") %>% 
  # filter(capital_stock<999999999999) %>% # optional: remove top coded data
  arrange(desc(capital_stock)) %>%
  nrow() %>% 
  (function(y){y-nfirms_top50})

# Fonts
font_add_google("Londrina Solid", "Londrina")
showtext_auto()

# Map and Hexagons
brazil <- ne_countries(country = "Brazil", scale = "medium", returnclass = "sf") %>%
  st_transform(3857)

bbox <- st_bbox(brazil)

bbox_width  <- bbox$xmax - bbox$xmin

bbox_height <- bbox$ymax - bbox$ymin

hex_area <- (bbox_width * bbox_height / 77) * 1.05  # 1.05 ensures slight overlap, 77 deduced by trial and error to give 100 hexagons

hex_size <- sqrt(2 * hex_area / (3 * sqrt(3)))

hex_grid <- st_make_grid(brazil, cellsize = hex_size, square = FALSE)

hex_sf <- st_sf(geometry = hex_grid)

hex_centroids <- st_centroid(hex_sf)
inside <- st_within(hex_centroids, brazil, sparse = FALSE)[,1] # centroids must be in brazil
hex_inside <- hex_sf[inside, ]

centroids <- st_centroid(hex_inside) 
coords <- st_coordinates(centroids)

hex_ranked <- hex_inside %>%
  mutate(y = coords[, "Y"], x = coords[, "X"]) %>%
  arrange(desc(y), x) %>%
  mutate(rank = row_number()) # rank from north to south, west to east as secondary ranking

hex_tt <- 
  hex_ranked %>%
  mutate(share = case_when(
    rank <= 50 ~ "Top 25 Firms", # we know that only 25 firms own 50 of the capital stock
    TRUE        ~ "Rest"
  ))

# coordinates for text labels
x_center <- (bbox$xmin + bbox$xmax) / 2 # center of the map
y_top    <- bbox$ymin + 0.77 * (bbox$ymax - bbox$ymin)   # 77th percentile
y_bottom <- bbox$ymin + 0.45 * (bbox$ymax - bbox$ymin)   # 45th percentile

(ggplot() +
  geom_sf(data = hex_tt, aes(fill = share), color = "black", linewidth = .4) +
  # geom_sf(data = brazil, fill = NA, color = "black", linewidth = 0.5) + # optional outline
  annotate("label", x = x_center - 4e+05, y = y_top,
           label = paste0(" ", nfirms_top50, " \n Firms "), size = 6, lineheight = .3,
           fill = "white", color = "#009739", fontface = "bold", family="Londrina") +
  annotate("label", x = x_center + 7e+05, y = y_bottom,
           label = paste0(" ", nfirms_rest, " \n Firms "), size = 6, lineheight = .3,
           fill = "white", color = "#009739", fontface = "bold", family="Londrina") +
  scale_fill_manual(values = c("Top 25 Firms" = "#fedd00", "Rest" = "#012169")) +
  labs(
    title = paste0(" ", nfirms_top50, " Firms own half the \n capital stock in Brazil ") %>% toupper(),
    caption = "Data: Tidy Tuesday 2026, Week 4"
  ) +
  theme_void(base_family = "Londrina") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 32, hjust = 0.5, lineheight = .3, color="white"),
    plot.caption = element_text(size=20, hjust = 0.5, color="white"),
    panel.background = element_rect(fill="#009739"),
    plot.background = element_rect(fill="#009739")
  )
  ) %>% 
  ggsave("tt2604-d1.png", plot=., width=5, height=5, units="cm")

