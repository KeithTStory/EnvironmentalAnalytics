#The purpose of this script is to analyze the petroleum profiles of the bonny soil core samples compated to bonny crude.

library(readr)
library(tidyverse)
library(multcompView)
library(plotrix)


#Directory for master csv file

raw <- read_csv("MasterDoc-graphing_NoSymbol.csv", na = c("", "NA", "None Supplied"), col_types = cols("Depth (m)" = col_number()))
BonnyCrude <- read_csv("BonnyCrude.csv", col_types = cols("Depth_Bins" = col_factor()))
attach(raw)

bins <- seq(0, 3.5, 0.5)


df1 <- raw %>%
  select(1, 3, 5, 7, 9, 38:45, 48:55) %>%
  filter(!is.na(`Depth (m)`)) %>%
  filter(`Sample Site`!= "BOD") %>%
  mutate(Unique_Site = paste(`Sample Site`, `Report Number`, sep = " ")) %>%
  dplyr::mutate(Depth_Bins = cut(x = `Depth (m)`,
                                 breaks = c(-Inf, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, Inf),
                                 labels = c("0.0 - 0.5 m", "0.5 - 1.0 m", "1.0 - 1.5 m", "1.5 - 2.0 m", "2.0 - 2.5 m", "2.5 - 3.0 m", "3.0 - 3.5 m", "> 3.5 m"))) %>%
  group_by(Depth_Bins) %>%
  select(-c(`Depth (m)`, `Lab Sample Number`, Cluster, `Sample Site`, `Report Number`)) %>%
  gather(EC, Composition, `Aliphatic >EC5 - EC6`:`Aromatic > EC35 - EC44`) %>%
  group_by(Unique_Site, EC, Depth_Bins) %>%
  summarize("Relative Composition" = mean(Composition)) %>%
  filter(!is.na(`Relative Composition`)) %>%
  tibble::rowid_to_column("ID")

# is.na(df1$Depth_Bins) <- df1$Depth_Bins == "1.0 - 1.5 m"

df2 <- df1 %>%
  group_by(Depth_Bins, EC) %>%
  summarise(meanTPH = mean(`Relative Composition`, na.rm = TRUE), varTPH = var(`Relative Composition`, na.rm = TRUE)) %>%
  na.omit() %>%
  mutate(FractionalTPH = meanTPH/sum(meanTPH)) %>% 
  mutate(seTPH = sqrt(varTPH/sum(varTPH))/sqrt(n())) %>%
  mutate("Group" = "CoreSample") %>%
  # ungroup() %>%
  select(-c(meanTPH, varTPH)) %>%
  # filter(Depth_Bins != "1.0 - 1.5 m") %>%
  as.data.frame()

# test <- subset(df2, Depth_Bins == "1.0 - 1.5 m")
# sum(test$FractionalTPH)
# # 
# write_csv(df2, "df2_Test.csv")


df3 <- as_tibble(rbind(as.data.frame(df2), as.data.frame(BonnyCrude)))

# df3$Depth_Bins <- factor(df3$Depth_Bins)
df3$EC <- factor(df3$EC, levels = c("Aliphatic >EC5 - EC6",
                                               "Aliphatic >EC6 - EC8",
                                               "Aliphatic >EC8 - EC10",
                                               "Aliphatic >EC10 - EC12",
                                               "Aliphatic >EC12 - EC16",
                                               "Aliphatic >EC16 - EC21",
                                               "Aliphatic >EC21 - EC35",
                                               "Aliphatic > EC35 - EC44",
                                               "Aromatic >EC5 - EC7",
                                               "Aromatic >EC7 - EC8",
                                               "Aromatic >EC8 - EC10",
                                               "Aromatic >EC10 - EC12",
                                               "Aromatic >EC12 - EC16",
                                               "Aromatic >EC16 - EC21",
                                               "Aromatic >EC21 - EC35",
                                               "Aromatic > EC35 - EC44"
            ))


# df3$Depth_Bins[df3$Depth_Bins != "1.0 - 1.5 m"]
# df3$Depth_Bins <- factor(df3$Depth_Bins)
# levels(df3)

# as.factor(df3$Depth_Bins)

#Plot with averaged data
(g <- ggplot(df3, aes(x = EC, y = `FractionalTPH`*100, fill = Depth_Bins)) +
    geom_bar(data = subset(df3, Group == "CoreSample"), stat = "identity") +
    geom_bar(data = subset(df3, Group == "Reference"), stat = "identity", fill = "black", color = "black", alpha = 0.3) +
    #geom_errorbar(data = subset(df3, Group == "CoreSample"),aes(ymin=(FractionalTPH-seTPH)*100, ymax=(FractionalTPH+seTPH)*100, width=.21))+
    theme_bw() +
    theme(legend.position= "none",
          axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_blank(),
          strip.text.x = element_text(size = 12, colour = "black"),
          plot.title = element_blank()
    ) +
    
    #scale_y_continuous(limits = c(0, .7), labels = scales::percent) +
    labs(title = "Relative Composition", y = "Fractional TPH Concentration (as % of Total)") + 
    facet_wrap(Depth_Bins ~ .)
)

############
df4 <- df3 %>%
  separate(EC, into= c("Parent", "Size"), sep = " >") %>%
  mutate(Subgroup = paste(Parent, Group, Depth_Bins, sep = " "))
  
(g2 <- ggplot(df4, aes(x = Subgroup, y = FractionalTPH, fill = Group)) +
  geom_boxplot() +
    theme(legend.position= "none",
          axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_blank(),
          strip.text.x = element_text(size = 12, colour = "black"),
          plot.title = element_blank()
    )
)

#######
Ali_0.5 <- filter(df4, Parent == "Aliphatic", Depth_Bins == "0.0 - 0.5 m")
Ali_1.0 <- filter(df4, Parent == "Aliphatic", Depth_Bins == "0.5 - 1.0 m")
Ali_2.0 <- filter(df4, Parent == "Aliphatic", Depth_Bins == "1.5 - 2.0 m")
Ali_2.5 <- filter(df4, Parent == "Aliphatic", Depth_Bins == "2.0 - 2.5 m")
Ali_3.0 <- filter(df4, Parent == "Aliphatic", Depth_Bins == "2.5 - 3.0 m")
Ali_3.5 <- filter(df4, Parent == "Aliphatic", Depth_Bins == "3.0 - 3.5 m")
Ali_deep <- filter(df4, Parent == "Aliphatic", Depth_Bins == "1.0 - 1.5 m")

Aro_0.5 <- filter(df4, Parent == "Aromatic", Depth_Bins == "0.0 - 0.5 m")
Aro_1.0 <- filter(df4, Parent == "Aromatic", Depth_Bins == "0.5 - 1.0 m")
Aro_2.0 <- filter(df4, Parent == "Aromatic", Depth_Bins == "1.5 - 2.0 m")
Aro_2.5 <- filter(df4, Parent == "Aromatic", Depth_Bins == "2.0 - 2.5 m")
Aro_3.0 <- filter(df4, Parent == "Aromatic", Depth_Bins == "2.5 - 3.0 m")
Aro_3.5 <- filter(df4, Parent == "Aromatic", Depth_Bins == "3.0 - 3.5 m")
Aro_deep <- filter(df4, Parent == "Aromatic", Depth_Bins == "1.0 - 1.5 m")

anova_one_way <- aov(FractionalTPH~Subgroup, data = Ali_0.5)
summary(anova_one_way)
TukeyHSD(anova_one_way)



# model=lm(df4$FractionalTPH ~ df4$Subgroup)
# ANOVA=aov(model)

# # Tukey test to study each pair of treatment :
# TUKEY <- TukeyHSD(x=ANOVA, 'df4$Subgroup', conf.level=0.95)
# 
# # Tuckey test representation :
# plot(TUKEY , las=.5 , col="brown")

