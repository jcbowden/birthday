# Happy birthday by John Lennon and Paul McCartney

require(rgl)
require(tiff)

#require(graphics)
#require(Matrix)

PlotSurface <- function(matrixIN,  scalex, scaley, scaleZHeight) {
  z <-  ( matrixIN ) * scaleZHeight 
  zlim <- range(z)
  zlen <- zlim[2] - zlim[1] + 1
  
  x <-  (1:nrow(matrixIN))  * scalex  # x spacing (S to N)
  y <-  (1:ncol(matrixIN))  * scaley  # y spacing (E to W)
  
  colorlut <- terrain.colors(zlen) # height color lookup table
  col <- colorlut[ z-zlim[1]+1 ] # assign colors to heights for each point
  open3d()
  surface3d(x,y,z,  color=col, back="lines")
}

#resnames <- sprintf('bds%02d', seq(1,18,1))
#bds <- ""
#for (i in 1:18) {
#  bds_temp <- get(resnames[i])  # get the dataset by text string
#  message("i = ", i)
#  bds <- paste(bds,bds_temp,sep="")
#}
#nchar(bds)

# setwd("C:/Users/bow355/Desktop/ATA_magazines")
setwd("H:/All data/Statistics")
bds <- readLines("birthday_beatles.txt",encoding="UTF-8")
byte_c <- nchar(bds)  # 660 characters including 22 EOL - 638 without EOL

beatle  <- rep(NA, byte_c/2)
seq_by2 <- seq(1,byte_c, 2)
#J == 00
#P == 01
#R == 10
#G == 11

i<-2
s2 <- substr(bds, i, i)

# convert 2 bit values into JPRG
j <- 1
for (i in seq_by2) {
 s1 <- substr(bds, i, i) # bds[i]
 s2 <- substr(bds, i+1, i+1)
 if (s1 == "0") {
   if (s2 == "0") {
     beatle[j] <- "J"
   } else {
     beatle[j] <- "P"
   }
 } else {
   if (s2 == "0") {
     beatle[j] <- "R"
   } else {
     beatle[j] <- "G"
   }
 } 
 j <- j + 1 ;  
}

str_base <- "C:/Users/bow355/Desktop/ATA_magazines/beatles_print/"
str_base <- "H:/All data/Statistics/beatles_print/"
paste0(str_base,"john.tif")

# load main images - 100x100 pixels
john <- readTIFF(paste0(str_base,"john.tif"),
                 native = FALSE, all = FALSE, convert = FALSE,
                 info = TRUE, indexed = FALSE, as.is = FALSE)
paul <- readTIFF(paste0(str_base,"paul.tif"),
                 native = FALSE, all = FALSE, convert = FALSE,
                 info = TRUE, indexed = FALSE, as.is = FALSE)
ringo <- readTIFF(paste0(str_base,"ringo.tif"),
                 native = FALSE, all = FALSE, convert = FALSE,
                 info = TRUE, indexed = FALSE, as.is = FALSE)
george <- readTIFF(paste0(str_base,"george.tif"),
                 native = FALSE, all = FALSE, convert = FALSE,
                 info = TRUE, indexed = FALSE, as.is = FALSE)





nfaces <- nchar(bds) /8  * 4
nfaces_side <- sqrt(nfaces)
nfaces_side_rnd <- trunc(nfaces_side)
image_size <- nrow(john)  # assuming all beatles images are the same width and height
image_size_m1 <- image_size -1 ;
width <- nfaces_side
height <- nfaces_side

big_size <- image_size * nfaces_side_rnd
beatle_mat <- matrix( rep(0.0,(big_size*big_size)), nrow = big_size, ncol = big_size)


zeortobigsize <- seq(1, nfaces_side_rnd*image_size, image_size)
zeorto25 <- seq(1, nfaces_side_rnd, 1)

i <- 1
j <- 1
k <- 1
for (j in zeortobigsize) {
  for (i in zeortobigsize) {
    if (k <= nfaces) {
      if (beatle[k] == "J") {
        beatle_mat[i:(i+image_size_m1),j:(j+image_size_m1)] <- john
      } else if (beatle[k] == "P") {
        beatle_mat[i:(i+image_size_m1),j:(j+image_size_m1)] <- paul
      } else if (beatle[k] == "R") {
        beatle_mat[i:(i+image_size_m1),j:(j+image_size_m1)] <- ringo
      } else if (beatle[k] == "G") {
        beatle_mat[i:(i+image_size_m1),j:(j+image_size_m1)] <- george
      }
    }
    k <- k + 1
  }  
}

beatle[1]

PlotSurface(beatle_mat,  1.0, 1.0, 10.0)
?rgl.ids()
