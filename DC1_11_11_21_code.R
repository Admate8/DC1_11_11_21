######################################                                  
#                                    #
#           Data Cleaning            #
#                                    #
######################################

### LOAD THE DATA ###
load("/Users/adrianwisnios/Desktop/Masters/Part 1/ST952 An Introduction to Statistical Practice /Assignment 1/AutoUSA85.Rdata")
#load(".../AutoUSA85.Rdata")
# Quick look at the structure
knitr::kable(skimr::skim(AutoUSA85))

# Load libraries
library(dplyr)      # Data manipulations
library(ggplot2)    # Plots
library(gridExtra)  # Multiple plots

# Impute missing values
AutoUSA85[is.na(AutoUSA85$Doors), ]$Doors <- "four" 
AutoUSA85[is.na(AutoUSA85$Bore), ]$Bore <- 
  median(AutoUSA85$Bore, na.rm = T)
AutoUSA85[is.na(AutoUSA85$Stroke), ]$Stroke <- 
  median(AutoUSA85$Stroke, na.rm = T)
AutoUSA85[is.na(AutoUSA85$Horsepower), ]$Horsepower <-
  median(AutoUSA85$Horsepower, na.rm = T)
AutoUSA85[is.na(AutoUSA85$PeakRPM), ]$PeakRPM <- 
  median(AutoUSA85$PeakRPM, na.rm = T)
AutoUSA85[is.na(AutoUSA85$Price), ]$Price <- 
  median(AutoUSA85$Price, na.rm = T)

# Data cleaning
Auto_clean <- AutoUSA85 %>%
  # Combine "convertible" and "hardtop"
  mutate(Body = as.factor(
    ifelse(Body %in% c("convertible", "hardtop"), "other", 
           as.character(Body))),
    # Collapse cylinders
    Cylinders = case_when(
      Cyl %in% c("two", "three", "four") ~ "<=four",
      Cyl %in% c("eight", "twelve") ~ ">=eight",
      TRUE ~ as.character(Cyl)), 
    Cylinders = factor(Cylinders, 
                       c("<=four", "five", "six", ">=eight"))) %>%
  select(-EngLoc, -Cyl) %>%
  # Remove rare categories
  filter(FuelSys != "mfi" & FuelSys != "spfi" & FuelSys != "4bbl" ) %>%
  # Change level names 
  mutate(Drive = case_when(
    Drive == "fwd" ~ "Front",
    Drive == "rwd" ~ "Rear",
    TRUE ~ "Four"), 
    Drive = as.factor(Drive))

######################################                                  
#                                    #
#                 EDA                #
#                                    #
######################################

# Separate predictors - for future convenience
num_var   <- select_if(Auto_clean, is.numeric)
cat_var   <- select_if(Auto_clean, is.factor) %>% cbind(MPG = Auto_clean$MPG)

# Helper function to display categories counts
give.n <- function(x){
  return(c(y = median(x)*0.97, label = length(x)))} 
  
# ## Fig: Categorical variables ###
# Leading plot with y-axis
c_plot1 <- ggplot(cat_var, aes(Doors, MPG)) +
  geom_boxplot(fill = "#78544c") + theme_bw() + 
  stat_summary(fun.data = give.n, geom = "text", fun = median,
               position = position_dodge(width = 0.75), 
               colour = "white", size = 2.5) +
  theme(axis.text.x=element_text(size=6),
        plot.background = element_rect(fill = "#e2d4d0", color = "#e2d4d0"))

# Function for other variables
cat_plots <- function(x, y){
  cat_var %>%
    ggplot(aes(get(x), get(y))) + 
    geom_boxplot(fill = "#78544c") + theme_bw() +
    stat_summary(fun.data = give.n, geom = "text", fun = median,
                 position = position_dodge(width = 0.75), 
                 colour = "white", size = 2.5) + 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
          axis.ticks.y = element_blank(), axis.text.x=element_text(size=6),
          plot.background = element_rect(fill = "#e2d4d0", color="#e2d4d0")) +
    xlab(x)
}

# Combine plots
grid.arrange(c_plot1, cat_plots("Body", "MPG"), cat_plots("Drive", "MPG"), 
             cat_plots("Cylinders", "MPG"), cat_plots("FuelSys", "MPG"),
             nrow = 1, ncol = 5)

### Fig: Clusters ###
Auto_clean %>%
  # Split by petrol and diesel
  mutate(EngType = ifelse(CompRatio < 12, "Petrol", "Diesel")) %>%
  ggplot(aes(CompRatio, MPG, col = FuelSys)) + 
  geom_point(alpha = 0.5) + 
  # Fir regression lines
  geom_smooth(aes(CompRatio, MPG), method = "lm", 
              se = F, size = 0.25, col = "black") + 
  geom_smooth(aes(CompRatio, MPG, col = EngType), method = "lm", 
              se = F, size = 0.25) + theme_bw() +
  xlab("Compression Ratio") + scale_color_discrete(name = "Fuel System") + 
  theme(legend.position = "top",
        plot.background = element_rect(fill = "#e2d4d0", color="#e2d4d0"),
        legend.background = element_rect(fill = "#e2d4d0"))

# Correlation matrix
corr = round(cor(num_var), 1)

### Fig: Correlations ###
ggcorrplot::ggcorrplot(corr, type = "lower", hc.order = TRUE, lab = TRUE,
                       show.legend = FALSE, lab_size = 3, tl.cex = 12) +
  theme(plot.background = element_rect(fill = "#e2d4d0", color="#e2d4d0"))

### Fig: Horsepower ###
Auto_clean %>%
  mutate(Price_T = round(Price / 1000, 4)) %>%
  ggplot() + 
  geom_point(aes(Horsepower, MPG, col = Price_T, size = Cylinders), alpha = 0.6) + 
  theme_bw() + facet_grid(vars(Drive)) + 
  theme(legend.position = "top",
        plot.background = element_rect(fill = "#e2d4d0", color="#e2d4d0"),
        legend.background = element_rect(fill = "#e2d4d0"),
        strip.background = element_rect(fill="#78544c"),
        strip.text = element_text(colour = 'white')) + 
  viridis::scale_colour_viridis(name = "Price (K$)", direction = -1)

### Fig: Cars size ###
ggplot(Auto_clean, aes(Len, Wid, col = EngSiz)) + 
  geom_point(alpha = 0.5, shape = 18, size = 3)  + 
  theme_bw() + xlab("Length of Car (inches)") + ylab("Width of Car (inches)") + 
  viridis::scale_colour_viridis(name = "Engine Size (cubic inches)", direction = -1) + 
  theme(legend.position = "top", 
        plot.background = element_rect(fill = "#e2d4d0", color="#e2d4d0"),
        legend.background = element_rect(fill = "#e2d4d0"))

### Fig: Cars engine ###
ggpubr::ggscatterhist(
  Auto_clean, x = "CurbWt", y = "EngSiz",
  color = "Cylinders", size = 2, alpha = 0.4,
  margin.params = list(fill = "Cylinders", colour = "black", size = 0.1),
  
  xlab = "Curb Weight (lbs)",
  ylab = "Engine Size (cubic inches)",
  ggtheme = theme(plot.background = element_rect(fill = "#e2d4d0", color="#e2d4d0"),
                  legend.background = element_rect(fill = "#e2d4d0"),
                  panel.background = element_rect(fill = "white"))) 

#### Table: Outlier ###
knitr::kable(Auto_clean %>% filter(between(Price, 10000, 12000) & Horsepower >220), caption = "Outlier", booktabs = T) %>%
  kableExtra::kable_styling(latex_options = c("scale", "HOLD_position"))

### Table: Identical Values ###
knitr::kable((Auto_clean %>% filter(Body == "other"))[7:8,], caption = "Identical Values", booktabs = T) %>%
  kableExtra::kable_styling(latex_options = c("scale", "HOLD_position"))

######################################                                  
#                                    #
#              Modelling             #
#                                    #
######################################

# Some further data manipulations
levels(Auto_clean$FuelSys) <- c(levels(Auto_clean$FuelSys), "Other", "idi")
Auto_clean$FuelSys[Auto_clean$FuelSys != "idi"] <- "Other"
Auto_clean$FuelSys[Auto_clean$FuelSys == "idi"] <- "idi"
Auto_clean$FuelSys <- droplevels(Auto_clean$FuelSys)
Auto_clean$FuelSys <- relevel(Auto_clean$FuelSys, "Other")
data <- Auto_clean

# Select numerical cols
num_cols <- colnames(data[sapply(data,class) != "factor"]) 
# Drop MPG
num_cols <- num_cols[-1] 
# Untransformed data correlation
utr_corr <- cor(data[num_cols], data$MPG, use = "complete.obs") 
# Transformed data correlation
tr_corr <- cor(data[num_cols]^-1, data$MPG, use = "complete.obs") 
tab <- t(cbind(utr_corr,tr_corr))
rownames(tab) <- c("Untransformed", "Transformed")
tab <- round(tab,2)

### Table: Correlations between MPG and untransformed/transformed variables ###
knitr::kable(tab, caption="Correlations between MPG and untransformed/transformed variables", booktabs = T) %>%
  kableExtra::kable_styling(latex_options = c("scale", "HOLD_position"))

# Simple first model
lm1 <- lm(MPG ~ . + FuelSys:CompRatio, data = data)
inv_cols <- c("EngSiz","CompRatio","Horsepower","Price")
datatr <- data
datatr[inv_cols] <- datatr[inv_cols]^-1
# Model with transformed independent variables
lm2 <- lm(MPG ~ . + FuelSys:CompRatio, data = datatr)

### Fig: Diagnostic plots for model with no transformed explanatory variables ###
par(mfrow=c(1,2), bg = "#e2d4d0")
plot(lm1, which=c(1,2), pch=20 )

### Fig: Diagnostic plots for model with transformed explanatory variables ###
par(mfrow=c(1,2), bg = "#e2d4d0")
plot(lm2, which=c(1,2), pch=20)

### Table: Comparison of $R^2$ of models ###
tab <- c(summary(lm1)$r.squared, summary(lm2)$r.squared)
tab <- cbind(tab, c(summary(lm1)$adj.r.squared, summary(lm2)$adj.r.squared))
rownames(tab) <- c("Model 1", "Model 2")
colnames(tab) <- c("R2", "Adjusted R2")
knitr::kable(round(tab,3), 
             caption="Comparison of $R^2$ of models", booktabs = T) %>%
  kableExtra::kable_styling(latex_options = "HOLD_position")

### Fig: Profile likelihood for boxcox transformation ###
par(mfrow=c(1,1), bg = "#e2d4d0")
MASS::boxcox(lm1)

# Model with transformed dependent variable
datatrdep <- data
datatrdep$MPG <- datatrdep$MPG^-1
colnames(datatrdep)[1] <- "GPM"
lm3 <- lm(GPM ~ . + FuelSys:CompRatio, data=datatrdep)

### Fig: Diagnostic plots for Gallons per Mile model ###
par(mfrow=c(1,3), bg = "#e2d4d0")
plot(lm3, which=c(1,2,5), pch=20)

### Fig: Dffits plot showing observations 54 and 125 with large values ###
par(mfrow=c(1,1), bg = "#e2d4d0")
plot(dffits(lm3), ylab = "Difference in predicted value")

### Fig: Added variables plots ###
par(mfrow=c(1,2), bg = "#e2d4d0")
car::avPlots(lm3, "Price")
datatrdep <- datatrdep[-c(54, 125),]
lm4 <- lm(GPM ~ . + FuelSys:CompRatio, data=datatrdep)
car::avPlots(lm4, "Price")

### Table: ANOVA test
lm5 <- lm(GPM ~ . - Price - Ht - Wid - Len - WhlBase - 
            Body - Doors - EngSiz - Bore + FuelSys:CompRatio,
          data=datatrdep)
knitr::kable(as.data.frame(anova(lm5, lm4)), caption = "ANOVA test",
             booktabs = T) %>%
  kableExtra::kable_styling(latex_options = "HOLD_position")

# Discard variables
datatrdep[,c("Price", "Ht", "Wid", "Len", "WhlBase", "Body", "Doors",
             "EngSiz", "Bore")] <- NULL

# Final model
lmfin <- lm(1000*GPM ~ . + FuelSys:CompRatio, data = datatrdep)

### Table: Final coefficients ###
knitr::kable(t(round(summary(lmfin)$coefficients[,c(1,4)],3)), 
             booktabs = T, caption = "Final coefficients") %>%
  kableExtra::kable_styling(latex_options = c("scale", "HOLD_position"))

### Table: $R^2$ of final model ###
tab <- t(c(summary(lmfin)$r.squared, summary(lmfin)$r.squared))
rownames(tab) <- "Final Model"
colnames(tab) <- c("R2", "Adjusted R2")
knitr::kable(round(tab, 3), caption="$R^2$ of final model", 
             booktabs = T) %>%
  kableExtra::kable_styling(latex_options = "HOLD_position")

### Fig: Diagnostic plots for the final model ###
par(mfrow=c(1,3), bg = "#e2d4d0")
plot(lmfin, which=c(1,2,5), pch=20)
