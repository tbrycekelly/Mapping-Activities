library(SimpleMapper)
library(SimpleBathy)
library(raster)

files = list.files(path = 'data/1979-2006/', pattern = '.tif', full.names = T)
#files = list.files(path = 'data/2007-2023/', pattern = '.tif', full.names = T)


tmp = raster::raster(files[1])
coords = raster::coordinates(tmp)
z = raster::as.array(tmp)
z = z
coords = projectionStereographic(lon = coords[,1]/1e3, lat = coords[,2]/1e3, lon0 = -45, lat0 = 90, inv = T)
coords = list(longitude = t(array(coords$longitude, dim = rev(dim(z))[-1])),
              latitude = t(array(coords$latitude, dim = rev(dim(z))[-1])))

z = array(z, dim = c(dim(z)[1:2], length(files)))
for (i in 2:length(files)) {
  tmp = raster::raster(files[i])
  ztmp = raster::as.array(tmp)
  z[,,i] = ztmp[,,1]
}


zmedian1 = apply(z, MARGIN = c(1,2), function(x) {median(x) == 1})

files = list.files(path = 'data/2007-2023/', pattern = '.tif', full.names = T)


tmp = raster::raster(files[1])
coords = raster::coordinates(tmp)
z = raster::as.array(tmp)
z = z
coords = projectionStereographic(lon = coords[,1]/1e3, lat = coords[,2]/1e3, lon0 = -45, lat0 = 90, inv = T)
coords = list(longitude = t(array(coords$longitude, dim = rev(dim(z))[-1])),
              latitude = t(array(coords$latitude, dim = rev(dim(z))[-1])))

z = array(z, dim = c(dim(z)[1:2], length(files)))
for (i in 2:length(files)) {
  tmp = raster::raster(files[i])
  ztmp = raster::as.array(tmp)
  z[,,i] = ztmp[,,1]
}


zmedian2 = apply(z, MARGIN = c(1,2), function(x) {median(x) == 1})



map = plotBasemap(lat = 90, scale = 5e3, land.col = 'white')
map = addBathy(map, pal = rev(pals::brewer.blues(128)), zlim = c(-9e3, 0), ztrim = c(-4e3, NA))
map = SimpleMapper::addLayer(map, lon = coords$longitude, lat = coords$latitude, z = zmedian1, zlim = c(0.5,1), pal = '#00000050', ztrim = c(NA,NA))
map = SimpleMapper::addLayer(map, lon = coords$longitude, lat = coords$latitude, z = zmedian2, zlim = c(0.5,1), pal = '#bb22bb60', ztrim = c(NA,NA))
map = addRelief(map)
map = addCoastline(map)


## Export to geotiffs
tmp = raster::raster(files[1])
tmp$N_200709_extent_v3.0 = zmedian2
raster::writeRaster(x = tmp, filename = 'Median Sea Ice Extent (2007-2023).tif')

tmp$N_200709_extent_v3.0 = zmedian1
raster::writeRaster(x = tmp, filename = 'Median Sea Ice Extent (1979-2006).tif')

