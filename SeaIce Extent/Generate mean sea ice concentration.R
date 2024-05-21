library(SimpleMapper)
library(SimpleBathy)
library(raster)

files = list.files(path = 'data/1979-2006 Concentration/', pattern = '.tif', full.names = T)
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


zmean1 = apply(z, MARGIN = c(1,2), function(x) {mean(x)})
zmean1[zmean1 == 2540] = NA

files = list.files(path = 'data/2007-2023 Concentration/', pattern = '.tif', full.names = T)


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


zmean2 = apply(z, MARGIN = c(1,2), function(x) {mean(x)})
zmean2[zmean2 == 2540] = NA

par(mfrow = c(2,2))

map = plotBasemap(lat = 90, scale = 5e3, land.col = 'white')
map = SimpleMapper::addLayer(map, lon = coords$longitude, lat = coords$latitude, z = zmean1, pal = rev(pals::brewer.blues(8)),
                             zlim = c(100, 500), ztrim = c(NA, 500))
map = addRelief(map)
map = addCoastline(map)


map = plotBasemap(lat = 90, scale = 5e3, land.col = 'white')
map = SimpleMapper::addLayer(map, lon = coords$longitude, lat = coords$latitude, z = zmean2, pal = rev(pals::brewer.blues(8)),
                             zlim = c(100, 500), ztrim = c(NA, 500))
map = addRelief(map)
map = addCoastline(map)


map = plotBasemap(lat = 90, scale = 5e3, land.col = 'white')
map = SimpleMapper::addLayer(map, lon = coords$longitude, lat = coords$latitude, z = (zmean1 - zmean2), pal = c("#181C43", "#0E5EBC", "#73A8BD", "white", "#CF8971", "#A52224", "#3C0912"),
                             zlim = c(-1000, 1000))
map = addRelief(map)
map = addCoastline(map)
polygon(x = seq(-610, 610, by = 10), y = sqrt(610^2 - seq(-610, 610, by = 10)^2), col = 'white', border = NA)
polygon(x = seq(-610, 610, by = 10), y = -sqrt(610^2 - seq(-610, 610, by = 10)^2), col = 'white', border = NA)


## Export to geotiffs
tmp = raster::raster(files[1])
tmp$N_200709_extent_v3.0 = zmedian2
raster::writeRaster(x = tmp, filename = 'Median Sea Ice Extent (2007-2023).tif')

tmp$N_200709_extent_v3.0 = zmedian1
raster::writeRaster(x = tmp, filename = 'Median Sea Ice Extent (1979-2006).tif')

