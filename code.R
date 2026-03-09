#libraries
library(tidyverse)
library(readr)

#load dataset
df <- read_csv("artBoneRicePatterson.csv")


#run pearson
cor.test(df$bone_abundance, df$art_count, method = "pearson")

# bone and ar count relationship graphic

ggplot(df, aes(x = bone_abundance, y = art_count)) +
  geom_point(color = "lightsalmon", size = 3, alpha = 0.95) +
  geom_smooth(method = "lm", se = FALSE, color = "gray35", linewidth = 1) +
  labs(
    x = "Bone Abundance",
    y = "Art Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3)
  )
  

#create representative table with all regions combined
taxa_summary <- df %>%
  group_by(taxa) %>%
  summarise(
    total_art = sum(art_count),
    total_bone = sum(bone_abundance)
  )
View(taxa_summary)