library(aqp)
library(soilDB)
library(sharpshootR)
library(sf)


# soil texture colors
texture.rat <- read.csv('http://soilmap2-1.lawr.ucdavis.edu/800m_grids/RAT/texture_2550.csv')
cols <- texture.rat$hex[match(levels(d$fine_earth_texture_cl), texture.rat$names)]


## define points, in order


# example 1
p <- data.frame(
  position = 1:3,
  x = c(-120.83914, -120.83656, -120.83322),
  y = c(38.19944, 38.19897, 38.19779)
)

# # example 2
# p <- data.frame(
#   position = 1:4,
#   x = c(-120.78240, -120.78077, -120.77893, -120.77610),
#   y = c(38.23393, 38.23267, 38.23137, 38.22952)
# )


p <- st_as_sf(p, coords = c('x', 'y'))
st_crs(p) <- 4326

## look-up MU keys
mu <- SDA_spatialQuery(p, what = 'mukey')

# abbreviated names
mu$.shortname <- gsub(', ', '\n', mu$muname)


## get mu/component data
.in <- format_SQL_in_statement(unique(mu$mukey))
d <- fetchSDA(WHERE = sprintf("mukey IN %s AND compkind != 'Miscellaneous area'", .in), duplicates = TRUE)

d <- trunc(d, 0, 110)


d$.profilelabel <- sprintf("%s\n(%s%%)", d$compname, d$comppct_r)

## fine earth texture classes
d$fine_earth_texture_cl <- ssc_to_texcl(sand = d$sandtotal_r, clay = d$claytotal_r)

# convert texture class to names
d$fine_earth_texture_cl <- factor(
  factor(d$fine_earth_texture_cl, 
         levels = SoilTextureLevels(which = 'codes'), 
         labels = SoilTextureLevels(which = 'names'))
)




## explicit ordering
d$mukey <- factor(d$mukey, levels = mu$mukey)
d$.shortname <- gsub(', ', '\n', d$muname)
d$.shortname <- factor(d$.shortname, levels = mu$.shortname)

d$.longname <- sprintf("%s\n%s (%s%%)", d$musym, d$compname, d$comppct_r)


par(mar = c(0, 0, 3, 0))

groupedProfilePlot(d, groups = '.shortname', group.name.offset = -10, label = '.profilelabel', color = 'fine_earth_texture_cl', cex.id = 0.5, cex.names = 0.66, name.style = 'center-center', width = 0.3, hz.depths = TRUE, id.style='top', fixLabelCollisions = TRUE, hz.depths.offset = 0.05, plot.depth.axis = FALSE, col.label = 'Soil Texture of Fine Earth Fraction (<2mm)', col.palette = cols)




d$mukey <- factor(d$mukey, levels = rev(mu$mukey))
o <- order(as.numeric(d$mukey), d$comppct_r, decreasing = TRUE)

# consider ordering by hillslope / or geomorphic component (3D hills)


plotSPC(d, plot.order = o, label = '.profilelabel', color = 'fine_earth_texture_cl', cex.id = 0.66, cex.names = 0.66, name.style = 'center-center', width = 0.3, hz.depths = TRUE, id.style='top', fixLabelCollisions = TRUE, hz.depths.offset = 0.05, plot.depth.axis = FALSE, col.label = 'Soil Texture of Fine Earth Fraction (<2mm)', col.palette = cols)

addVolumeFraction(d, colname = 'fragvol_r')

text(x = 1:length(d), y = -15, d$musym[o], cex = 0.66, font = 3)
text(x = 1:length(d), y = -10, d$hill[o], cex = 0.66)

# text(x = 1:length(d), y = -10, d$hillslopeprof[o], cex = 0.66)



