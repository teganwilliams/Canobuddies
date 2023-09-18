library(NISTunits)

fov.px <- function ( deg_theta, focal_length_mm , pixel_pitch_um) {
  # Convert degrees of theta to radians
  rads_theta <- NISTunits :: NISTdegTOradian ( deg_theta )
  
  # Calculate radius of circle drawn by angle of view ( rads _ theta and max_ rads _theta ) in mm projected onto the sensor plane
  R <- 2 * focal_length_mm * sin( rads_theta / 2)
  
  # Calculate the px per mm on the sensor , i.e. the pixel pitch
  sensor_px_per_mm_flat <- 1/ pixel_pitch_um * 1000
  
  # Multiply the mm radius of the desired circle by the number of pixels per mm on the sensor , to get the number of pixels radius of the desired circle
  pixels_for_theta <- round (R * sensor_px_per_mm_flat , 0)
  return ( pixels_for_theta )
}

fov.px(deg_theta = 60, focal_length_mm = 8, pixel_pitch_um = 5.95)
# pixels for radius? 1345

#install.packages("jpeg")
library(jpeg)
im = readJPEG("fieldcourse/pictures/EE5_5266.jpg", native = F)     #if native = T creates a raster, else an array


## convert to class HemiphotImage - adding a circle
im.hemi = Image2Hemiphot(im)
#PlotHemiImage(image = im.hemi, draw.circle = T) 

im.hemi = SetCircle(im.hemi, cx = 3000, cy = 2000, cr = 1345)
 # PlotHemiImage(im.hemi)

#PlotHemiImage(im.hemi, draw.circle = T, channel = "R")
#PlotHemiImage(im.hemi, draw.circle = T, channel = "G")
#PlotHemiImage(im.hemi, draw.circle = T, channel = "B")

im.blue = SelectRGB(im.hemi, "B")
#PlotHemiImage(im.blue, draw.circle = T)

threshold = 0.55
image.th = ThresholdImage(image = im.blue, th = threshold, draw.image = T)

#PlotHemiImage(image.th, draw.circle = T) 
DrawCircles(image.th[[2]], image.th[[3]], image.th[[4]])

#PlotHemiImage(image.th, draw.circle = T) 

## calculate canopy cover based on canopy openess of the 89 circles
## the openess by circle is stored in gap.fractions
gap.fractions = CalcGapFractions(image.th)
canopy.openness = CalcOpenness(fractions = gap.fractions); canopy.openness

## calculate LAI according to Licor's LAI Analyzer 
canopy.LAI = CalcLAI(fractions = gap.fractions, width = 6); canopy.LAI
