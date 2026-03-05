library(readxl); library(ggplot2); library(tidyr); library(dplyr)

RimData <- read_excel("RimData.xlsx")

RimData <- RimData %>% 
  rename(
    RimStyle = 'Rim Style', 
    RimHeight = 'Rim Height'
  )

ggplot(data = RimData, aes(x = RimDiameter, y = RimWidth)) + 
  geom_point(aes(   # color = RimStyle, 
    shape = RimStyle), size = 3) + 
  #scale_color_manual(values = c("darkorange", "midnightblue")) + 
  geom_smooth(method ='lm', formula = y~x) +
  theme_classic() + 
  facet_wrap(~RimStyle)

# GeomPointrange# ugly but it works. Do folks know about stacking in ggplot?
# also,   facet_wrap(~RimStyle)- good to break down by rim style?

corr_DiamWidth <- cor.test(RimData$RimDiameter, RimData$RimWidth)  

ggplot(data = RimData, aes(x = RimDiameter, y = RimHeight)) + 
  geom_jitter(aes(color = RimStyle, shape = RimStyle), size = 3) + 
  scale_color_manual(values = c("darkorange", "midnightblue")) + 
  geom_smooth(method ='lm', formula = y~x) +
  theme_classic()

cor.test(RimData$RimDiameter, RimData$RimHeight)  

plot_order1 <- ggplot(data = RimData, aes(x = RimWidth, y = RimHeight)) + 
  geom_smooth(method ='lm', formula = y~x) +
  geom_jitter(aes(color = RimStyle, shape = RimStyle), size = 3) + # note that different geoms can be shifted in order to stack differently on the plot
  scale_color_manual(values = c("darkorange", "midnightblue")) +
  theme_classic()

plot_order2 <- ggplot(data = RimData, aes(x = RimWidth, y = RimHeight)) + 
  geom_jitter(aes(color = RimStyle, shape = RimStyle), size = 3) + # note that different geoms can be shifted in order to stack differently on the plot
  geom_smooth(method ='lm', formula = y~x) +
  scale_color_manual(values = c("darkorange", "midnightblue")) +
  theme_classic()

cor.test(RimData$RimWidth, RimData$RimHeight)  
