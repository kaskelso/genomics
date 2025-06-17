############ R code for Askelson et al. 2025, Western Screech-owl conservation genomics ############

### Code for Fig 1. Askelson et al. Western Screech Owls #####
# This approach was inspired from https://github.com/milos-agathon/crisp-topographical-map-with-r #
# There are a few lines of code from this github but it is heavily modified #

# This uses 6 packages, here are the versions I used
# rnaturalearth 1.0.1
# rmapshaper 0.5.0
# terra 1.8.29
# tidyverse 2.0.0
# whatarelief 0.0.1.9012
# sf 1.0.19

libs <- c( "rnaturalearth", "rmapshaper", "terra", "tidyverse", "whatarelief", "sf")

invisible(lapply(libs, library, character.only = T))

weso_locations <- read.table("/Users/kennyaskelson/Desktop/PhD_UBC/WESO/weso_data/map/mapping_WESO.txt", header = TRUE) #just a two column file with lat/longs

world <- read_sf("/Users/kennyaskelson/Desktop/PhD_UBC/WESO/weso_data/map/world-administrative-boundaries/world-administrative-boundaries.shp") #this is just used to draw country and state/province lines

# Filter for North America countries
wio <- world %>%
  filter(name %in% c("United States of America", "Canada", "Mexico"))

# Load US state boundaries (from TIGER/Line shapefiles or Natural Earth)
us_states <- ne_states(country = "United States of America", returnclass = "sf")

canada_provinces_alt <- ne_states(country = "Canada", returnclass = "sf")

# Simplify shapefiles
us_states_simplified <- ms_simplify(us_states, keep = 0.05)  # Adjust 'keep' for more simplification


template <- rast(xmin=-175, xmax=-65, ymin=15, ymax=65, res=.05)

r <- elevation(template)

country_elevation_df <- na.omit(as.data.frame(r, xy = TRUE))

names(country_elevation_df)[3] <- "elevation"


USA_Can_MX <- ggplot(data = country_elevation_df) +
  geom_raster(
    aes(x = x, y = y, fill = elevation),
    alpha = 1, na.rm = TRUE
  ) + geom_sf(data = wio, col = 1, fill = NA) +  # Plot country boundaries
  geom_sf(data = us_states, fill = NA, col = "black", size = 0.05, linewidth = 0.1) +  # Add US states
  geom_sf(data = canada_provinces_alt, fill = NA, col = "black", size = 0.05, linewidth = 0.1) +  # Add Canada provinces
  coord_sf(xlim = c(-153, -69), ylim = c(26, 63)) +
  scale_fill_gradient(
    low = "white",
    high = "saddlebrown",
    limits = c(0,5000),
    na.value = "white",
    breaks = c(-5000, 0, 5000),
    labels = c("-5000", "0", "5000"),
    name = "Elevation (m)"
  ) + labs(x = NULL, y = NULL) + 
  theme_void() +
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = unit(c(0,8,0,3), "cm"),
        legend.text = element_text(size = 14),           
        legend.title = element_text(size = 16),          
        legend.position = c(1, 0.5),                 
        legend.justification = c(0, 0.5),                
        legend.background = element_rect(fill = "white", color = NA),  
        legend.box.margin = margin(0, 0, 0, 0),          
        legend.spacing.y = unit(0.4, "cm")) +
  geom_point(data = weso_locations, 
             aes(x = long, y = lat, color = location1, shape = location1), size = 4, stroke = 2) +
  scale_shape_manual(name = "", values = c("AK" = 15, 
                                           "BC_coast" = 16, 
                                           "OR_interior" = 21, 
                                           "SW" = 22, 
                                           "EASO" = 19, 
                                           "OR_coast" = 23, 
                                           "BC_interior" = 21, 
                                           "CA" = 22, 
                                           "VI" = 17,
                                           "WA_coast" = 18, 
                                           "WA_interior" = 25),
                     labels = c("AK" = "Alaska (AK)", 
                                "BC_coast" = "British Columbia (BC) Coast", 
                                "OR_interior" = "Oregon (OR) Interior", 
                                "SW" = "Southwest (SW)", 
                                "EASO" = "Eastern Screech-Owl (EASO)", 
                                "OR_coast" = "OR Coast", 
                                "BC_interior" = "BC Interior", 
                                "CA" = "California (CA)", 
                                "VI" = "Vancouver Island (VI)",
                                "WA_coast" = "Washington (WA) Coast", 
                                "WA_interior" = "WA Interior"),
                     guide = guide_legend(override.aes = list(size = 5))) +
  scale_color_manual(name = "", values = c("AK" = "#E69F00", 
                                           "BC_coast" = "#009E73", 
                                           "OR_interior" = "#D55E00", 
                                           "SW" = "#56B4E9", 
                                           "EASO" = "#0072B2", 
                                           "OR_coast" = "#D55E00", 
                                           "BC_interior" = "#009E73", 
                                           "CA" = "#F0E442", 
                                           "VI" = "#999999",
                                           "WA_coast" = "#B2DF8A", 
                                           "WA_interior" = "#B2DF8A"),
                     labels = c("AK" = "Alaska (AK)", 
                                "BC_coast" = "British Columbia (BC) Coast", 
                                "OR_interior" = "Oregon (OR) Interior", 
                                "SW" = "Southwest (SW)", 
                                "EASO" = "Eastern Screech-Owl (EASO)", 
                                "OR_coast" = "OR Coast", 
                                "BC_interior" = "BC Interior", 
                                "CA" = "California (CA)", 
                                "VI" = "Vancouver Island (VI)",
                                "WA_coast" = "Washington (WA) Coast", 
                                "WA_interior" = "WA Interior"),
                     guide = guide_legend(override.aes = list(size = 5))) 


PNW <- ggplot(data = country_elevation_df) +
  geom_raster(
    aes(x = x, y = y, fill = elevation),
    alpha = 1, na.rm = TRUE
  )  + geom_sf(data = wio, col = 1, fill = NA) +  # Plot country boundaries
  geom_sf(data = us_states, fill = NA, col = "black", size = 0.05, linewidth = 0.1) +  # Add US states
  geom_sf(data = canada_provinces_alt, fill = NA, col = "black", size = 0.05, linewidth = 0.1) +  # Add Canada provinces
  coord_sf(xlim = c(-147, -117), ylim = c(45, 61)) +
  # Add Canada provinces
  scale_fill_gradient(
    low = "white",
    high = "saddlebrown",
    na.value = "white",
    limits = c(0,5000),
    breaks = c(-5000, 0, 5000),
    labels = c("-5000", "0", "5000"),
    name = "Elevation (m)"
  ) +
  geom_point(data = weso_locations, 
             aes(x = long, y = lat, color = location1, shape = location1), size = 3, stroke = 2) +
  theme_void() +
  theme(axis.text = element_text(size = 11, colour = 1),
        panel.background = element_rect(fill = NA, color = NA), axis.title = element_blank(),
        panel.grid = element_line(colour = NA), legend.position = "none", axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) +
  scale_shape_manual(name = "", values = c("AK" = 15, 
                                           "BC_coast" = 16, 
                                           "OR_interior" = 21, 
                                           "SW" = 22, 
                                           "EASO" = 19, 
                                           "OR_coast" = 23, 
                                           "BC_interior" = 21, 
                                           "CA" = 22, 
                                           "VI" = 17,
                                           "WA_coast" = 18, 
                                           "WA_interior" = 25)) +
  scale_color_manual(name = "", values = c("AK" = "#E69F00", 
                                           "BC_coast" = "#009E73", 
                                           "OR_interior" = "#D55E00", 
                                           "SW" = "#56B4E9", 
                                           "EASO" = "#0072B2", 
                                           "OR_coast" = "#D55E00", 
                                           "BC_interior" = "#009E73", 
                                           "CA" = "#F0E442", 
                                           "VI" = "#999999",
                                           "WA_coast" = "#B2DF8A", 
                                           "WA_interior" = "#B2DF8A")) 



weso_map <- cowplot::ggdraw() + 
  cowplot::draw_plot(USA_Can_MX, x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_plot(PNW + theme(axis.text = element_blank(), axis.ticks = element_blank()), 
                     x = 0.02, y = 0.07, width = 0.3, height = 0.45, scale = 1.25)


#####################################################################################################

### Code for Fig 2 Askelson et al. Western Screech Owls #####

library(ggplot2)
library(ggpubr)

# package versions:
# ggplot2 3.5.1
# ggpubr 0.6.0

# This code uses .csv files generated from WESO_genomic_analysis.R that uses Irwin et al. 2016 & 2018 scripts. #
# See that file for how these were made and if you use this approach and these scripts please cite Irwin et al. 2016 & 2018 #

weso_131 <- read.csv("/Users/kennyaskelson/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/GBS_WESO_131_balanced_PC_scores.csv", header = TRUE)

weso_121 <- read.csv("/Users/kennyaskelson/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/GBS_WESO_121_balanced_PC_scores.csv", header = TRUE)

weso_98 <- read.csv("/Users/kennyaskelson/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/GBS_WESO_98_balanced_PC_scores.csv", header = TRUE)

weso_131_plot <- ggplot() + 
  geom_point(data = weso_131, 
             aes(x = (-1*PC1), y = PC2, color = location1, shape = location1), size = 2) + xlab("PC 1 (37%)") + ylab("PC 2 (7.2%)") + #Note: PC axes being negative or positive is arbitrary, in this case PC1 values are flipped so the Eastern screech owls cluster appear on the right on the PCA
  theme_classic() +
  theme(text = element_text(size = 20)) + 
  scale_shape_manual(values = c("AK" = 15, 
                                "BC_coast" = 16, 
                                "OR_interior" = 21, 
                                "SW" = 22, 
                                "EASO" = 19, 
                                "OR_coast" = 23, 
                                "BC_interior" = 21, 
                                "CA" = 22, 
                                "VI" = 17,
                                "WA_coast" = 18, 
                                "WA_interior" = 25)) +
  scale_color_manual(values = c("AK" = "#E69F00", 
                                "BC_coast" = "#009E73", 
                                "OR_interior" = "#D55E00", 
                                "SW" = "#56B4E9", 
                                "EASO" = "#0072B2", 
                                "OR_coast" = "#D55E00", 
                                "BC_interior" = "#009E73", 
                                "CA" = "#F0E442", 
                                "VI" = "#999999",
                                "WA_coast" = "#B2DF8A", 
                                "WA_interior" = "#B2DF8A")) + theme(legend.title=element_blank()) 

weso_121_plot <- ggplot() + 
  geom_point(data = weso_121, 
             aes(x = PC1, y = PC2, color = location1, shape = location1), size = 2) + xlab("PC 1 (14.7%)") + ylab("PC 2 (5.5%)") +
  theme_classic() +
  theme(text = element_text(size = 20)) + 
  scale_shape_manual(values = c("AK" = 15, 
                                "BC_coast" = 16, 
                                "OR_interior" = 21, 
                                "SW" = 22, 
                                "EASO" = 19, 
                                "OR_coast" = 23, 
                                "BC_interior" = 21, 
                                "CA" = 22, 
                                "VI" = 17,
                                "WA_coast" = 18, 
                                "WA_interior" = 25)) +
  scale_color_manual(values = c("AK" = "#E69F00", 
                                "BC_coast" = "#009E73", 
                                "OR_interior" = "#D55E00", 
                                "SW" = "#56B4E9", 
                                "EASO" = "#0072B2", 
                                "OR_coast" = "#D55E00", 
                                "BC_interior" = "#009E73", 
                                "CA" = "#F0E442", 
                                "VI" = "#999999",
                                "WA_coast" = "#B2DF8A", 
                                "WA_interior" = "#B2DF8A")) + theme(legend.title=element_blank()) 

weso_98_plot <- ggplot() + 
  geom_point(data = weso_98, 
             aes(x = PC1, y = PC2, color = location1, shape = location1), size = 2) + xlab("PC 1 (9%)") + ylab("PC 2 (5.1%)") +
  theme_classic() +
  theme(text = element_text(size = 20)) + 
  scale_shape_manual(values = c("AK" = 15, 
                                "BC_coast" = 16, 
                                "OR_interior" = 21, 
                                "SW" = 22, 
                                "EASO" = 19, 
                                "OR_coast" = 23, 
                                "BC_interior" = 21, 
                                "CA" = 22, 
                                "VI" = 17,
                                "WA_coast" = 18, 
                                "WA_interior" = 25)) +
  scale_color_manual(values = c("AK" = "#E69F00", 
                                "BC_coast" = "#009E73", 
                                "OR_interior" = "#D55E00", 
                                "SW" = "#56B4E9", 
                                "EASO" = "#0072B2", 
                                "OR_coast" = "#D55E00", 
                                "BC_interior" = "#009E73", 
                                "CA" = "#F0E442", 
                                "VI" = "#999999",
                                "WA_coast" = "#B2DF8A", 
                                "WA_interior" = "#B2DF8A")) + theme(legend.title=element_blank()) 



ggarrange(weso_131_plot, weso_121_plot, weso_98_plot, 
          labels = c("A", "B", "C"), font.label = list(size = 20), common.legend = TRUE, legend = "right",
          ncol = 2, nrow = 2)


#####################################################################################################


### Code for Fig 3 Askelson et al. Western Screech Owls #####

### This uses output files from Admixture, see bioinformatic script for how these were generated
### I followed this blogs R code with modifications to plot in base R https://plantarum.ca/2021/06/01/admixture/

# dplyr 1.1.4

setwd("~/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/admixture_runs")

weso_121_K3 <- read.table("weso_plate12_121_SNP_biallelic_whole_genome_filtered_redo_missing_balanced_miss095_minGQ10_mac3_plink.3.Q")

popmap <- read.table("~/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/admixture_runs/WESO_forAdmixture_latlong.txt", header = TRUE)

weso_121_K3 <- cbind(popmap, weso_121_K3) # I bind together sample info and admixture results (rows in same order!)

weso_121_K3 <- weso_121_K3 %>%
  mutate(plot_order = case_when(
    location == "SW" ~ 1,
    location == "CA" ~ 2,
    location == "OR_coast" ~ 3,
    location == "WA_coast" ~ 4,
    location == "BC_coast" ~ 5,
    location == "OR_interior" ~ 6,
    location == "WA_interior" ~ 7,
    location == "BC_interior" ~ 8,
    location == "VI" ~ 9,
    location == "AK" ~ 10,
    TRUE ~ NA_real_  # Set to NA if location doesn't match
  )) %>%
  arrange(plot_order, Long)             # change plot order by longitude 

weso_121_K3 <- weso_121_K3[order(weso_121_K3$plot_order), ]

xlabels_121 <- aggregate(1:nrow(weso_121_K3),
                         by = list(weso_121_K3[, "location"]),
                         FUN = mean)

sampleEdges_121 <- aggregate(1:nrow(weso_121_K3),
                             by = list(weso_121_K3[, "location"]), 
                             FUN = max)

barplot(t(as.matrix(weso_121_K3[, -1:-7])), col=c("#999999","#56B4E9", "#009E73"), 
        space = 0, xlab="", ylab = "Ancestry", 
        border=NA, axisnames = FALSE)
abline(v = sampleEdges_121$x, lwd = 2)
axis(1, at = xlabels_121$x - 0.5, labels = FALSE)
text(x = xlabels_121$x - 0.5, y = par("usr")[3] - 0.05, labels = xlabels_121$Group.1, srt = 45, adj = 1, xpd = TRUE)

#### K = 4 #####

weso_121_K4 <- read.table("weso_plate12_121_SNP_biallelic_whole_genome_filtered_redo_missing_balanced_miss095_minGQ10_mac3_plink.4.Q")

popmap <- read.table("~/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/admixture_runs/WESO_forAdmixture_latlong.txt", header = TRUE)

weso_121_K4 <- cbind(popmap, weso_121_K4)

weso_121_K4 <- weso_121_K4 %>%
  mutate(plot_order = case_when(
    location == "SW" ~ 1,
    location == "CA" ~ 2,
    location == "OR_coast" ~ 3,
    location == "WA_coast" ~ 4,
    location == "BC_coast" ~ 5,
    location == "OR_interior" ~ 6,
    location == "WA_interior" ~ 7,
    location == "BC_interior" ~ 8,
    location == "VI" ~ 9,
    location == "AK" ~ 10,
    TRUE ~ NA_real_  # Set to NA if location doesn't match
  )) %>%
  arrange(plot_order, Long)

weso_121_K4 <- weso_121_K4[order(weso_121_K4$plot_order), ]

xlabels_121 <- aggregate(1:nrow(weso_121_K4),
                         by = list(weso_121_K4[, "location"]),
                         FUN = mean)

sampleEdges_121 <- aggregate(1:nrow(weso_121_K4),
                             by = list(weso_121_K4[, "location"]), 
                             FUN = max)

barplot(t(as.matrix(weso_121_K4[, -1:-7])), col=c("#009E73","#F0E442","#56B4E9","#999999"), 
        space = 0, xlab="", ylab = "Ancestry", 
        border=NA, axisnames = FALSE)
abline(v = sampleEdges_121$x, lwd = 2)
axis(1, at = xlabels_121$x - 0.5, labels = FALSE)
text(x = xlabels_121$x - 0.5, y = par("usr")[3] - 0.05, labels = xlabels_121$Group.1, srt = 45, adj = 1, xpd = TRUE)

####################################
########### SW REMOVED #############
####################################

weso_98_K2 <- read.table("weso_plate12_98_SNP_biallelic_whole_genome_filtered_redo_missing_balanced_miss095_minGQ10_mac3.2.Q")

popmap <- read.table("~/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/admixture_runs/WESO_98_forAdmixture_latlong.txt", header = TRUE)

weso_98_K2 <- cbind(popmap, weso_98_K2)

weso_98_K2 <- weso_98_K2 %>%
  mutate(plot_order = case_when(
    location == "CA" ~ 1,
    location == "OR_coast" ~ 2,
    location == "WA_coast" ~ 3,
    location == "BC_coast" ~ 4,
    location == "OR_interior" ~ 5,
    location == "WA_interior" ~ 6,
    location == "BC_interior" ~ 7,
    location == "VI" ~ 8,
    location == "AK" ~ 9,
    TRUE ~ NA_real_  # Set to NA if location doesn't match
  )) %>%
  arrange(plot_order, Long)

weso_98_K2 <- weso_98_K2[order(weso_98_K2$plot_order), ]

xlabels_98 <- aggregate(1:nrow(weso_98_K2),
                        by = list(weso_98_K2[, "location"]),
                        FUN = mean)

sampleEdges_98 <- aggregate(1:nrow(weso_98_K2),
                            by = list(weso_98_K2[, "location"]), 
                            FUN = max)

barplot(t(as.matrix(weso_98_K2[, -1:-7])), col=c("#009E73", "#999999"), 
        space = 0, xlab="", ylab = "Ancestry", 
        border=NA, axisnames = FALSE)
abline(v = sampleEdges_98$x, lwd = 2)
axis(1, at = xlabels_98$x - 0.5, labels = FALSE)
text(x = xlabels_98$x - 0.5, y = par("usr")[3] - 0.05, labels = xlabels_98$Group.1, srt = 45, adj = 1, xpd = TRUE)


#### K3 #####


weso_98_K3 <- read.table("weso_plate12_98_SNP_biallelic_whole_genome_filtered_redo_missing_balanced_miss095_minGQ10_mac3.3.Q")

popmap <- read.table("~/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/admixture_runs/WESO_98_forAdmixture_latlong.txt", header = TRUE)

weso_98_K3 <- cbind(popmap, weso_98_K3)

weso_98_K3 <- weso_98_K3 %>%
  mutate(plot_order = case_when(
    location == "CA" ~ 1,
    location == "OR_coast" ~ 2,
    location == "WA_coast" ~ 3,
    location == "BC_coast" ~ 4,
    location == "OR_interior" ~ 5,
    location == "WA_interior" ~ 6,
    location == "BC_interior" ~ 7,
    location == "VI" ~ 8,
    location == "AK" ~ 9,
    TRUE ~ NA_real_  # Set to NA if location doesn't match
  )) %>%
  arrange(plot_order, Long)

weso_98_K3 <- weso_98_K3[order(weso_98_K3$plot_order), ]

xlabels_98 <- aggregate(1:nrow(weso_98_K3),
                        by = list(weso_98_K3[, "location"]),
                        FUN = mean)

sampleEdges_98 <- aggregate(1:nrow(weso_98_K3),
                            by = list(weso_98_K3[, "location"]), 
                            FUN = max)

barplot(t(as.matrix(weso_98_K3[, -1:-7])), col=c("#009E73", "#F0E442", "#999999"), 
        space = 0, xlab="", ylab = "Ancestry", 
        border=NA, axisnames = FALSE)
abline(v = sampleEdges_98$x, lwd = 2)
axis(1, at = xlabels_98$x - 0.5, labels = FALSE)
text(x = xlabels_98$x - 0.5, y = par("usr")[3] - 0.05, labels = xlabels_98$Group.1, srt = 45, adj = 1, xpd = TRUE)

####### Plot them altogether! #####

par(mfrow=c(2,2))

barplot(t(as.matrix(weso_121_K3[, -1:-7])), col=c("#999999", "#56B4E9", "#009E73"), 
        space = 0, xlab="", ylab = "Ancestry", 
        border=NA, axisnames = FALSE)
abline(v = sampleEdges_121$x, lwd = 2)
axis(1, at = xlabels_121$x - 0.5, labels = FALSE)
text(x = xlabels_121$x - 0.5, y = par("usr")[3] - 0.05, labels = xlabels_121$Group.1, srt = 45, adj = 1, xpd = TRUE)
mtext("A", side = 3, line = 1, adj = -0.05, font = 2, cex = 2)  
text(x = par("usr")[2]-1.5, y = mean(par("usr")[3:4]), "K=3", srt = 270, adj = 0.5, font = 2, cex = 2, xpd = TRUE)  


barplot(t(as.matrix(weso_98_K2[, -1:-7])), col=c("#009E73", "#999999"), 
        space = 0, xlab="", ylab = "Ancestry", 
        border=NA, axisnames = FALSE)
abline(v = sampleEdges_98$x, lwd = 2)
axis(1, at = xlabels_98$x - 0.5, labels = FALSE)
text(x = xlabels_98$x - 0.5, y = par("usr")[3] - 0.05, labels = xlabels_98$Group.1, srt = 45, adj = 1, xpd = TRUE)
mtext("C", side = 3, line = 1, adj = -0.05, font = 2, cex = 2)  
text(x = par("usr")[2]-1.5, y = mean(par("usr")[3:4]), "K=2", srt = 270, adj = 0.5, font = 2, cex = 2, xpd = TRUE)  

barplot(t(as.matrix(weso_121_K4[, -1:-7])), col=c("#009E73","#F0E442","#56B4E9","#999999"), 
        space = 0, xlab="", ylab = "Ancestry", 
        border=NA, axisnames = FALSE)
abline(v = sampleEdges_121$x, lwd = 2)
axis(1, at = xlabels_121$x - 0.5, labels = FALSE)
text(x = xlabels_121$x - 0.5, y = par("usr")[3] - 0.05, labels = xlabels_121$Group.1, srt = 45, adj = 1, xpd = TRUE)
mtext("B", side = 3, line = 1, adj = -0.05, font = 2, cex = 2)  
text(x = par("usr")[2]-1.5, y = mean(par("usr")[3:4]), "K=4", srt = 270, adj = 0.5, font = 2, cex = 2, xpd = TRUE)  

barplot(t(as.matrix(weso_98_K3[, -1:-7])), col=c("#009E73", "#F0E442", "#999999"), 
        space = 0, xlab="", ylab = "Ancestry", 
        border=NA, axisnames = FALSE)
abline(v = sampleEdges_98$x, lwd = 2)
axis(1, at = xlabels_98$x - 0.5, labels = FALSE)
text(x = xlabels_98$x - 0.5, y = par("usr")[3] - 0.05, labels = xlabels_98$Group.1, srt = 45, adj = 1, xpd = TRUE)
mtext("D", side = 3, line = 1, adj = -0.05, font = 2, cex = 2)  
text(x = par("usr")[2]-1.5, y = mean(par("usr")[3:4]), "K=3", srt = 270, adj = 0.5, font = 2, cex = 2, xpd = TRUE)  


#####################################################################################################

### Code for Fig 4 Askelson et al. Western Screech Owls #####

# This code uses .csv files generated from WESO_genomic_analysis.R that uses Irwin et al. 2016 & 2018 scripts. #
# See that file for how these were made and if you use this approach and these scripts please cite Irwin et al. 2016 & 2018 #
# This uses output files from VCF filtering script, see bioinformatic script for how these were generated #
# The column clust_group was added manually based on sample cluster grouping from the PCA #

# ggplot2 3.5.1
# ggpubr 0.6.0
# dunn.test 1.3.6

weso_121 <- read.csv("~/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/chr8_inv/GBS_WESO_121_chr8_PC_scores.csv", header = TRUE)

weso_98 <- read.csv("~/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/chr8_inv/GBS_WESO_98_chr8_PC_scores.csv", header = TRUE)


weso_121_plot <- ggplot() + 
  geom_point(data = weso_121, 
             aes(x = PC1, y = PC2, color = location1, shape = location1), size = 2) + xlab("PC 1 (42.8%)") + ylab("PC 2 (11.1%)") +
  theme_classic() +
  theme(text = element_text(size = 20)) + 
  scale_shape_manual(values = c("AK" = 15, 
                                "BC_coast" = 16, 
                                "OR_interior" = 21, 
                                "SW" = 22, 
                                "EASO" = 19, 
                                "OR_coast" = 23, 
                                "BC_interior" = 21, 
                                "CA" = 22, 
                                "VI" = 17,
                                "WA_coast" = 18, 
                                "WA_interior" = 25)) +
  scale_color_manual(values = c("AK" = "#E69F00", 
                                "BC_coast" = "#009E73", 
                                "OR_interior" = "#D55E00", 
                                "SW" = "#56B4E9", 
                                "EASO" = "#0072B2", 
                                "OR_coast" = "#D55E00", 
                                "BC_interior" = "#009E73", 
                                "CA" = "#F0E442", 
                                "VI" = "#999999",
                                "WA_coast" = "#B2DF8A", 
                                "WA_interior" = "#B2DF8A")) + theme(legend.title=element_blank()) 


weso_98_plot <- ggplot() + 
  geom_point(data = weso_98, 
             aes(x = PC1, y = PC2, color = location1, shape = location1), size = 2) + xlab("PC 1 (53.2%)") + ylab("PC 2 (5%)") +
  theme_classic() +
  theme(text = element_text(size = 20)) + 
  scale_shape_manual(values = c("AK" = 15, 
                                "BC_coast" = 16, 
                                "OR_interior" = 21, 
                                "SW" = 22, 
                                "EASO" = 19, 
                                "OR_coast" = 23, 
                                "BC_interior" = 21, 
                                "CA" = 22, 
                                "VI" = 17,
                                "WA_coast" = 18, 
                                "WA_interior" = 25)) +
  scale_color_manual(values = c("AK" = "#E69F00", 
                                "BC_coast" = "#009E73", 
                                "OR_interior" = "#D55E00", 
                                "SW" = "#56B4E9", 
                                "EASO" = "#0072B2", 
                                "OR_coast" = "#D55E00", 
                                "BC_interior" = "#009E73", 
                                "CA" = "#F0E442", 
                                "VI" = "#999999",
                                "WA_coast" = "#B2DF8A", 
                                "WA_interior" = "#B2DF8A")) + theme(legend.title=element_blank()) 

# This next step uses a .het file from VCF filtering script, see bioinformatic script for how these were generated #

# The column clust_group was added manually based on sample cluster grouping from the PCA #


inv_het <- read.table("~/Desktop/PhD_UBC/WESO/weso_data/fall_2024/gatk_hard_filtered/chr8_inv/weso_plate12_121_SNP_biallelic_whole_genome_filtered_redo_missing_balanced_miss095_minGQ10_mac3_het.het", header = TRUE) 

inv_het$heterozygosity <- ((inv_het$N_SITES-inv_het$O.HOM.)/inv_het$N_SITES) # vcftools outputs observed homozygosity, so we minus homozygous sites from N.sites and divide by N.sites to get the proportion of het sites

het_plot <- ggplot(inv_het, aes(x=clust_group, y=heterozygosity)) + xlab("cluster group") + 
  geom_boxplot() + theme_classic() + theme(text = element_text(size = 20)) + 
  scale_x_discrete(labels = c("clust_1" = "cluster 1", "clust_2" = "cluster 2", "clust_3" = "cluster 3"))

weso_legend <- get_legend(weso_121_plot)

ggarrange(weso_98_plot, weso_121_plot, het_plot,
          labels = c("A", "B", "C"), font.label = list(size = 20),legend.grob = weso_legend, common.legend = TRUE, legend = "right",
          ncol = 2, nrow = 2)

# Now let's do the stats tests

kruskal.test(inv_het$heterozygosity~inv_het$clust_group)

#Kruskal-Wallis rank sum test

#data:  inv_het$heterozygosity by inv_het$clust_group
#Kruskal-Wallis chi-squared = 60.9, df = 2, p-value = 5.965e-14

library(dunn.test)

out <- dunn.test(inv_het$heterozygosity, inv_het$clust_group, kw=FALSE, method="bonferroni")

out$comparisons

#[1] "clust_1 - clust_2" "clust_1 - clust_3" "clust_2 - clust_3"


out$P.adjusted

# 3.496803e-13 8.390568e-02 2.950863e-07
