# stats and plots for field course 2023

# packages ----
library(tidyverse)  
library(gridExtra)
library(readxl)
library(ggeffects)
library(MuMIn)
library(sjPlot)   
library(insight)
library(httr)
library (geosphere)
library(ape)
library(geostats)
library(nlme)

# data - change type to csv? ----

data_final <- read_excel("data_final.xlsx")
View(data_final)

data_final$Site <- as.character(data_final$Site)

data_final2 <- data_final %>%
  mutate(forest_type = if_else(.$pine_percentage == "100", 'pine', 'mixed'))

# variogram and spatial autocorrelation ----

spatial <- distm(fun = distGeo, x = data_final [,c ("lat", "long")] ) 
Moran.I(x = data_final$exp_shannon, spatial  )

# p value below 0.05 - we have spatial autocorrelation

# could also see spa when points were colored according to sites.

semivariogram(x = data_final$lat, y = data_final$long, z = data_final$exp_shannon)

# lag: how much the signal is lagging according to distance (?)
# x not m bc coordinates are in decimal degrees

# change x to meters?

# models ----

# poly - not using this was only working with the wrong data
mod_poly <- lm(exp_shannon~poly(LAI, 3), data = data_final2)
summary(mod_poly)

# other models - taking into account sp. ac

mod_null <- gls( exp_shannon ~ 1, data = data_final2, correlation = corSpatial(form = ~ lat + long, 
                                                                                 nugget = T ) )
mod_lai1 <-  gls( exp_shannon ~ LAI, data = data_final2, correlation = corSpatial(form = ~ lat + long, 
                                                                                 nugget = T ) )
mod_forest <- gls( exp_shannon ~ forest_type, data = data_final2, correlation = corSpatial(form = ~ lat + long, 
                                                                                   nugget = T ) )
mod_lai_forest <- gls( exp_shannon ~ LAI + forest_type, data = data_final2, correlation = corSpatial(form = ~ lat + long, 
                                                                                 nugget = T ) )
mod_lai_forest_interaction <- gls( exp_shannon ~ LAI * forest_type, data = data_final2, correlation = corSpatial(form = ~ lat + long, nugget = T ) )

mod_soil_forest <- gls( exp_shannon ~ LAI + forest_type+ soil_texture, data = data_final2, correlation = corSpatial(form = ~ lat + long, nugget = T ) )

mod_texture_lai <- gls (exp_shannon ~ LAI + soil_texture,data = data_final2, correlation = corSpatial(form = ~ lat + long, nugget = T ) )

mod_pH <- gls( exp_shannon ~ soil_ph, data = data_final2, correlation = corSpatial(form = ~ lat + long, nugget = T ) )

mod_texture <- gls (exp_shannon ~ soil_texture, data = data_final2, correlation = corSpatial(form = ~ lat + long, nugget = T ) )                                                                                             


 # use likelihood ratio test to see which model is better bc aics are so similar                                                                                                                                  
 # change : to star and back to see aic for that

AICc(mod_null, mod_lai1, mod_forest, mod_lai_forest, mod_lai_forest_interaction,
     mod_texture_lai,  mod_soil_forest, mod_texture, mod_pH)

summary(mod_lai_forest_interaction)
summary(mod_forest)
summary(mod_lai_forest)
summary(mod_lai1)

tab_model(mod_lai1)
# tab_model(mod_lai1, file = "mod_lai1.doc")

anova(mod_lai1) # to get the F statistic

(boxplot <- ggplot(data_final2, aes(x = forest_type, y = LAI))+ geom_boxplot())

# model with only lai is best

# residuals and how much does forest type take away - not using this

mod_for_res <- lm(exp_shannon ~ forest_type, data = data_final2)
resids <- residuals(mod_for_res)
mod_residsonly <- lm(resids ~ LAI, data = data_final2)
summary(mod_residsonly)

# but they explain the same thing in the same way still so makes sense


# Extract the prediction data frame from best model

pred.mm <- ggpredict(mod_lai1, terms = c("LAI"))  # this gives overall predictions for the model

colours = c("#BF3EFF", "#4ecb50") # new dataframe for colors



# final plot ----

(final_plot <- ggplot(pred.mm) +
  geom_line(aes(x = x, y = predicted, shape = "model predictions", linetype = "model predictions"),
            colour = "#FF8C00", size = 1) + 
  scale_linetype_manual('', values =c("model predictions" = 1))+
  geom_point(data = data_final2,                      # adding the raw data (scaled values)
             aes(x = LAI, y = exp_shannon,  colour = forest_type), size =2) +  
  scale_colour_manual(values = colours)+
 #  geom_smooth(data=data_final2, method = "lm", aes(x = LAI, y = exp_shannon), se=FALSE, colour = "red") +
  theme_classic() +
  ylab("Exponential Shannon's Diversity Index\n") +                             
  xlab("\nLeaf Area Index")  +
  labs(color = "Forest type\n")+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "plain"),                      
        panel.grid = element_blank(), 
        legend.text=element_text(size = 15),
        legend.title = element_text(size = 16),
        plot.margin = unit(c(1,1,1,1), units = , "cm")))

ggsave("final_plot.png", width = 9, height = 6, dpi = 300)


# other plots ----

# plot with line (lm) plotted instead of predictions

(light_plot <- ggplot(data_final2, aes(x = LAI, y = exp_shannon)) +
    geom_point(aes(colour=forest_type), size =2) +         # slope
    scale_color_manual(values = colours) +
    geom_smooth(method= "lm", se=FALSE, size=1, colour = 'darkgreen') +
    theme_classic() +
    ylab("Shannons Diversity Index\n") +                             
    xlab("\nLeaf Area Index")  +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 10, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")))


# was the plot for the poly model - not using
ggplot(pred.mm) +
  geom_line(aes(x = x, y = predicted), colour = "#099430", size = 1) +          # slope
  geom_point(data = data_final2,                      # adding the raw data (scaled values)
             aes(x = LAI, y = exp_shannon,  colour = forest_type), size =2) +   
  geom_smooth(data =data_final, aes(x = LAI, y = exp_shannon), se=FALSE, size=0.5, colour = "black", linetype = "dashed") +
  geom_smooth(data=data_final2, method ="lm", aes(x = LAI, y = exp_shannon), se=FALSE, colour = "red") +
  theme_classic() +
  ylab("Shannons Diversity Index\n") +                             
  xlab("\nLAI")  +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"))
