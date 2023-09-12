# Stats fieldcourse
# Project canobuddies
# How does light availability affect understorey vegetation?
# Else, Rebecca, Kinga, Tegan, Luisa, Thomas, Hannah

#Library ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(skimr)
library(ggeffects) # to visualise predictions

# Data wrangling ----
data_final$Site <- as.character(data_final$Site)

data_final2 <- data_final %>%
  mutate(forest_type = if_else(.$pine_percentage == "100", 'pine', 'mixed')) 
# adding column to seperate by forest type


mod_null <- lm(exp_shannon ~1, data= data_final2)
mod_lai1 <- lm(exp_shannon ~ LAI, data = data_final2) # explanatory variable LAI
mod_forest <- lm(exp_shannon ~ forest_type, data = data_final2) # explanatory variable forest
mod_pH <- lm(exp_shannon ~ soil_ph, data=data_final2)
mod_text <- lm(exp_shannon ~ soil_texture, data = data_final2)
mod_lai2 <- lm(exp_shannon ~ LAI + forest_type, data = data_final2)
mod_lai4 <- lm(exp_shannon ~ LAI * forest_type, data = data_final2)


summary(mod_lai1)
summary(mod_forest)
summary(mod_pH)
summary(mod_text)
summary(mod_lai2)
summary(mod_lai4)

AIC(mod_null, mod_lai1, mod_forest,mod_pH, mod_text, mod_lai2, mod_lai4) # comparing models
# mod_lai1 has the lowest AIC
# both pH and texture have a higher AIC than the null model - they have very low explanatory power

# polynomial models that we are no longer using
# mod_lai <- lm(exp_shannon~poly(LAI, 3), data = data_final2)
# summary(mod_lai)
# mod_res <- lm(exp_shannon ~ forest_type, data = data_final2)
# resids <- residuals(mod_res)
# mod_lai3 <- lm(resids ~ poly(LAI,3), data = data_final2)
# summary(mod_lai3)
# mod_lm <- lm(resids ~LAI, data=data_final2)
# summary(mod_lm)

# Data visualisation ----
# Extract the prediction data frame
pred.mm <- ggpredict(mod_lai1, terms = c("LAI"))  # this gives overall predictions for the model

# colours for plots
colours <- c("#cd96e6", "#4ecb50", "#FF75E6", "#35E34F", "#FFCF4C", "#FF6B6B")

# plot made for original polynomial model
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

# plot without model predictions
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

# Final Plot
# includes line with the model predictions from mod_lai1 (model with just LAI as an explanatory variable)
ggplot(pred.mm) +
  geom_line(aes(x = x, y = predicted, shape="model predictions", linetype="model predictions"), colour = "#099430", size = 1) +  
  scale_linetype_manual('',values=c("model predictions"=1)) +
  geom_point(data = data_final2,              
             aes(x = LAI, y = exp_shannon,  colour=forest_type), size =2) + 
  theme_classic() +
  scale_color_manual(values = colours) +
  ylab("Shannons Diversity Index\n") +                             
  xlab("\nLeaf Area Index")  +
  labs(color = "forest type") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 10, face = "plain"),                      
        panel.grid = element_blank(),                                          
        plot.margin = unit(c(1,1,1,1), units = , "cm"))
