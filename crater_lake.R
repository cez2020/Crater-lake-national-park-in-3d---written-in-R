library("rayshader")
library("osmdata")
library("rayimage")
library("rgl")
library("rgdal")
library("magrittr")
library("rgl")
library("sf")
library("magick")
library("av")
library("sp")
library("raster")
library("scales")

crater = raster("./dem/Crater_Lake.tif")
crater_mat = raster_to_matrix(crater)

# resize matrix
crater_small = resize_matrix(crater_mat,0.25)

# map and add elevation colors
crater_small %>% 
  height_shade() %>% 
  plot_map()

# 
crater_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(crater_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  plot_map()

# add shadow
crater_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(crater_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(crater_small,zscale = 6),0) %>%
  plot_map()

# add some snow effect
snow_palette = "white"
snow_hs = height_shade(crater_small, texture = snow_palette)

crater_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(crater_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_overlay(generate_altitude_overlay(snow_hs, crater_small, 1000, 2800, lower = FALSE), alphalayer = 0.1) %>% 
  add_shadow(lamb_shade(crater_small,zscale = 6),0) %>%
  plot_map()


# add texture shade function and ambient occlusion
crater_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(crater_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_overlay(generate_altitude_overlay(snow_hs, crater_small, 1000, 2800, lower = FALSE), alphalayer = 0.1) %>% 
  add_shadow(lamb_shade(crater_small,zscale = 6),0) %>%
  add_shadow(ambient_shade(crater_small), 0) %>%
  add_shadow(texture_shade(crater_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
  plot_map()

extent(crater)
extent_utm = extent(crater)

crs(crater)

# to shorten code
base_map = crater_small %>% 
  height_shade() %>% 
  add_water(detect_water(crater_small), color = "imhof4") %>%
  add_overlay(sphere_shade(crater_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_overlay(generate_altitude_overlay(snow_hs, crater_small, 1000, 2800, lower = FALSE), alphalayer = 0.1) %>% 
  add_shadow(lamb_shade(crater_small,zscale = 6),0) %>%
  add_shadow(ambient_shade(crater_small), 0) %>%
  add_shadow(texture_shade(crater_small,detail=8/10,contrast=9,brightness = 11), 0.1)

plot_map(base_map)

# add Open Street Map features
lat_range   = c(42.8604808, 43.0148828)
long_range = c(-122.2142962, -121.999645)


osm_bbox = c(long_range[1],lat_range[1], long_range[2],lat_range[2])

# type available_features() in console to see list of osm features
crater_highway = opq(osm_bbox) %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() 
crater_highway

crater_highway$osm_lines

# convert data to crater's coordinates
crater_lines = st_transform(crater_highway$osm_lines, crs=crs(crater))
crater_lines

# plot osm data
ggplot(crater_lines,aes(color=osm_id)) + 
  geom_sf() +
  theme(legend.position = "none") +
  labs(title = "Open Street Map 'highway' attribute in Crater Lake National Park")

# add osm data to base map
base_map %>% 
  add_overlay(generate_line_overlay(crater_lines,extent = extent_utm,
                                    heightmap = crater_small)) %>% 
  plot_map()

# make lines white and thicker
base_map %>% 
    add_overlay(generate_line_overlay(crater_lines,extent = extent_utm,
                                    linewidth = 3, color="white",
                                    heightmap = crater_small)) %>% 
  plot_map()

# filter and separate them into several different categories
crater_trails = crater_lines %>% 
  filter(highway %in% c("path"))

crater_footpaths = crater_lines %>% 
  filter(highway %in% c("footway"))

crater_roads = crater_lines %>% 
  filter(highway %in% c("unclassified", "secondary", "tertiary", "residential", "service"))

# plot them all together
base_map %>% 
  # add_overlay(generate_line_overlay(crater_footpaths,extent = extent_utm,
  #                                   linewidth = 6, color="white",
  #                                   heightmap = crater_small)) %>%
  add_overlay(generate_line_overlay(crater_trails,extent = extent_utm,
                                    linewidth = 3, color="white", lty=3,
                                    heightmap = crater_small)) %>%
  add_overlay(generate_line_overlay(crater_roads,extent = extent_utm,
                                    linewidth = 8, color="white",
                                    heightmap = crater_small)) %>%
  plot_map()


# Create an outline by plotting the same data twice
base_map %>% 
  add_overlay(generate_line_overlay(crater_trails,extent = extent_utm,
                                    linewidth = 3, color="#E8E8E8", lty=3,
                                    heightmap = crater_small)) %>% 
  add_overlay(generate_line_overlay(crater_roads,extent = extent_utm,
                                    linewidth = 8, color="#47473F",
                                    heightmap = crater_small)) %>%
  add_overlay(generate_line_overlay(crater_roads,extent = extent_utm,
                                    linewidth = 5, color="#D4D4BA",
                                    heightmap = crater_small)) %>% 
  
  plot_map()


# Add add a slight shadow effect by plotting the same data twice
base_map %>% 
  add_overlay(generate_line_overlay(crater_trails,extent = extent_utm,
                                    linewidth = 3, color="#47473F", lty=3, offset = c(2,-2),
                                    heightmap = crater_small)) %>% 
  add_overlay(generate_line_overlay(crater_trails,extent = extent_utm,
                                    linewidth = 3, color="#E8E8E8", lty=3,
                                    heightmap = crater_small)) %>%
  add_overlay(generate_line_overlay(crater_roads,extent = extent_utm,
                                    linewidth = 8, color="#47473F",
                                    heightmap = crater_small)) %>%
  add_overlay(generate_line_overlay(crater_roads,extent = extent_utm,
                                    linewidth = 5, color="#D4D4BA",
                                    heightmap = crater_small)) %>% 
  plot_map()


# Save overlays into their own layer
trails_layer = generate_line_overlay(crater_trails,extent = extent_utm,
                                     linewidth = 3, color="#47473F", lty=3, offset = c(2,-2),
                                     heightmap = crater_small) %>% 
  add_overlay(generate_line_overlay(crater_trails,extent = extent_utm,
                                    linewidth = 3, color="#E8E8E8", lty=3,
                                    heightmap = crater_small)) %>%
  add_overlay(generate_line_overlay(crater_roads,extent = extent_utm,
                                    linewidth = 8, color="#47473F",
                                    heightmap = crater_small)) %>%
  add_overlay(generate_line_overlay(crater_roads,extent = extent_utm,
                                    linewidth = 5, color="#D4D4BA",
                                    heightmap = crater_small)) 

# add waterways
crater_water_lines = opq(osm_bbox) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 
crater_water_lines

crater_water_lines$osm_lines

# convert coordinates
crater_streams = st_transform(crater_water_lines$osm_lines,crs=crs(crater)) 
crater_streams

# stream layer
stream_layer = generate_line_overlay(crater_streams,extent = extent_utm,
                                     linewidth = 4, color="skyblue2", 
                                     heightmap = crater_small)

# plot with streams
base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_map()


# add a title
base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_map(title_text = "Crater Lake National Park, Oregon", title_offset = c(15,15),
           title_bar_color = "grey5", title_color = "white", title_bar_alpha = 1)


# render it in 3D
base_map %>% 
  add_overlay(stream_layer, alphalayer = 0.8) %>% 
  add_overlay(trails_layer) %>%
  plot_3d(crater_small, zscale = 15, windowsize=c(1200,800))
render_camera(theta=45,  phi=65, zoom=0.33,  fov=60)
render_snapshot()

# iterate 360 times
# generate 360 images
# to later make a movie
thetavec = seq(0,359)
for(i in 1:360) {
  render_camera(theta =  thetavec[i], phi = 65,zoom = 0.33, fov = 60)
  #uncomment the next line to save each frame to the working directory
  render_snapshot(paste0("frame", i, ".png"))
}