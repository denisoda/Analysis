doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("jpeg", "reshape", "ggplot2")
if (doInstall) {
  install.packages(toInstall, repos = "http://cran.r-project.org")
}
lapply(toInstall, library, character.only = TRUE)



library(jpeg)
library(reshape)
library(ggplot2)

# Image URL:
# allImageURLs <- c("http://nerdist.com/wp-content/uploads/2016/06/Akira-Movie-featured.jpg",
#                   "http://upload.wikimedia.org/wikipedia/commons/thumb/e/ec/Mona_Lisa%2C_by_Leonardo_da_Vinci%2C_from_C2RMF_retouched.jpg/402px-Mona_Lisa%2C_by_Leonardo_da_Vinci%2C_from_C2RMF_retouched.jpg",
#                   "http://cache.boston.com/universal/site_graphics/blogs/bigpicture/obama_11_05/obama22_16604051.jpg",
#                   "http://upload.wikimedia.org/wikipedia/commons/thumb/e/ea/Van_Gogh_-_Starry_Night_-_Google_Art_Project.jpg/758px-Van_Gogh_-_Starry_Night_-_Google_Art_Project.jpg",
#                   "http://www.10mfh.com/wp-content/uploads/2011/09/dino_riders.jpg",
#                   "http://images3.alphacoders.com/855/8557.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/ngm_101912/bp19.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/ngm_101912/bp26.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/ngm_101912/bp35.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/balloon/bp6.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/smithsonian_030512/bp14.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/smithsonian_030512/bp15.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/earth_day_2012/bp6.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/2011part2/bp1.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/2011part2/bp4.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/2011part2/bp15.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/2011part2/bp27.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/natural_world_2011/bp40.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/ngmphotocontest_111811/bp10.jpg",
#                   "http://inapcache.boston.com/universal/site_graphics/blogs/bigpicture/ngmphotocontest_111811/bp54.jpg")
# 
# imageLoader <- function(url){
#   # This function takes an URL, and generates a 
#   # data.frame with pixel locations and colors
#   # Download to disk, load
#   download.file(url, "tempPicture.jpg", mode = "wb") # Stash image locally
#   readImage <- readJPEG("tempPicture.jpg")  
#   
#   longImage <- melt(readImage)
#   rgbImage <- reshape(longImage, timevar = "X3",
#                       idvar = c("X1", "X2"), direction = "wide")
#   rgbImage$X1 <- -rgbImage$X1
#   return(rgbImage)
# }

imageLoaderLocal <- function(nm){
  readImage <- readJPEG(sprintf("img_R/%s.jpg", nm))  
  
  longImage <- melt(readImage)
  rgbImage <- reshape(longImage, timevar = "X3",
                      idvar = c("X1", "X2"), direction = "wide")
  rgbImage$X1 <- -rgbImage$X1
  return(rgbImage)
}

# imageLoad <- function(url, nm) {
#   download.file(url, mode = "wb", destfile = sprintf("img_R/%s.jpg", nm))
# }

# for (i in 1:length(allImageURLs)) {
#   imageLoad(allImageURLs[i], i)
# }


##########
# Part 2 # Identifying "dominant" colors with k-means
##########

# rgbImage <- imageLoader(allImageURLs[2]) # Pick one, or use your own URL.
rgbImage <- imageLoaderLocal(1)
with(rgbImage, plot(X2, X1, col = rgb(rgbImage[, 3:5]), asp = 1, pch = "."))

# Cluster in color space:
kColors <- 3 # Number of palette colors
kMeans <- kmeans(rgbImage[, 3:5], centers = kColors)

kMeans
factor(kMeans$cluster)

zp1 <- qplot(factor(kMeans$cluster), geom = "bar",
             fill = factor(kMeans$cluster))
zp1 <- zp1 + scale_fill_manual(values = rgb(kMeans$centers))
zp1

approximateColor <- kMeans$centers[kMeans$cluster, ]
with(rgbImage, plot(X2, X1, col = rgb(approximateColor), asp = 1, pch = "."))




########
# Task #
########

# Как сделать, чтобы пастеризация не изменялась?
