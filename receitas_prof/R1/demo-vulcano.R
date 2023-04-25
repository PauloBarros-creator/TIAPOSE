library(lattice)
## volcano  ## 87 x 61 matrix
#pdf("vulcano.pdf") # create a new graphical device (pdf file)
wireframe(volcano,shade = TRUE,aspect = c(61/87, 0.4),light.source = c(10,0,10))
#dev.off() # close the device
