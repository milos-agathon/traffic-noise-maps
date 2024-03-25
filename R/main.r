# 1. PACKAGES
#------------

install.packages("remotes")
remotes::install_github(
    "r-arcgis/arcgis",
    dependencies = TRUE
)

install.packages("pacman")

pacman::p_load(
    geodata,
    arcgis,
    tidyverse,
    sf,
    terra,
    maptiles,
    tidyterra,
    exactextractr,
    osmdata
)

# 2. BOUNDARIES
#--------------

country_sf <- geodata::gadm(
    country = "BEL",
    level = 1,
    path = getwd()
) |>
    sf::st_as_sf()

region_sf <- subset(country_sf, NAME_1 = "Bruxelles")

region_bbox <- sf::st_bbox(region_sf)

# 3. NOISE IMAGE
#---------------

url <- "https://noise.discomap.eea.europa.eu/arcgis/rest/services/noiseStoryMap/NoiseContours_road_lden/ImageServer"

noise_data <- arcgislayers::arc_open(
    url
)

noise_raster <- arcgislayers::arc_raster(
    x = noise_data,
    xmin = region_bbox[["xmin"]],
    xmax = region_bbox[["xmax"]],
    ymin = region_bbox[["ymin"]],
    ymax = region_bbox[["ymax"]],
    crs = sf::st_crs(region_sf),
    width = 4000,
    height = 4000
)

unique(terra::values(noise_raster))

noise_raster_clean <- terra::ifel(
    noise_raster == 0 | noise_raster == 15,
    NA,
    noise_raster
)

# 4. OSM MAJOR ROADS
#-------------------

major_roads <- c(
    "motorway", "motorway_link",
    "trunk", "trunk_link",
    "primary", "primary_link",
    "secondary", "secondary_link",
    "tertiary", "tertiary_link"
)

osm_roads <- osmdata::opq(
    bbox = region_bbox,
    timeout = 180,
    memsize = 104857600
) |>
    osmdata::add_osm_feature(
        key = "highway",
        value = major_roads
    ) |>
    osmdata::osmdata_sf()

osm_major_roads <- osm_roads |>
    sf::st_intersection(
        sf::st_as_sfc(region_bbox)
    ) |>
    sf::st_transform(
        crs = sf::st_crs(noise_raster_clean)
    )

# 5. ROADS AND NOISE MAP
#-----------------------

colors <- hcl.colors(
    n = 5,
    palette = "Plasma",
    rev = TRUE
)

p <- ggplot() +
    tidyterra::geom_spatraster(
        data = as.factor(noise_raster_clean)
    ) +
    # geom_sf(
    #     data = osm_major_roads,
    #     color = "grey80",
    #     size = .5
    # ) +
    scale_fill_manual(
        name = "",
        values = colors,
        na.value = "black"
    ) +
    theme_void()

ggsave(
    filename = "noise.png",
    plot = p,
    width = 7,
    height = 7,
    units = "in"
)

# 6. ROAD BUFFER
#---------------
major_roads_buffer <- sf::st_buffer(
    osm_major_roads,
    dist = units::set_units(
        50, m
    )
)

major_noise_extract <- exactextractr::exact_extract(
    x = noise_raster_clean,
    y = major_roads_buffer,
    fun = "mode"
)

major_roads_noise_sf <- cbind(
    osm_major_roads,
    major_noise_extract
)

# 7. FINAL MAP
#-------------

names(major_roads_noise_sf)

streets <- maptiles::get_tiles(
    region_bbox,
    provider = "CartoDB.Positron",
    zoom = 12,
    crop = TRUE,
    project = FALSE
)

map <- ggplot() +
    tidyterra::geom_spatraster_rgb(
        data = streets
    ) +
    geom_sf(
        data = subset(
            major_roads_noise_sf,
            !is.na(major_noise_extract)
        ),
        aes(
            color = as.factor(
                major_noise_extract
            )
        ),
        size = .25
    ) +
    scale_color_manual(
        name = "",
        values = colors,
        na.value = "white"
    ) +
    guides(
        color = guide_legend(
            override.aes = list(
                size = 3
            )
        )
    ) +
    theme_void()
