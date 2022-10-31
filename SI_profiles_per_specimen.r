# Plot Sr spiking LAICPMS profiles Batch 2

require(tidyverse)
require(ggpubr)
require(RColorBrewer)
require(readxl)

# Load data - Batch 1
Cedule_G003_1 <- read.csv("Batch1/HR/Cedule_003_1.csv", header = TRUE)[-c(1, 2), ]
Cedule_G003_2 <- read.csv("Batch1/HR/Cedule_003_2.csv", header = TRUE)[-c(1, 2), ]
Cedule_G003_3 <- read.csv("Batch1/HR/Cedule_003_3.csv", header = TRUE)[-c(1, 2), ]
Cedule_G511_1 <- read.csv("Batch1/HR/Cedule_511_1.csv", header = TRUE)[-c(1, 2), ]
Cedule_G511_2 <- read.csv("Batch1/HR/Cedule_511_2.csv", header = TRUE)[-c(1, 2), ]
Cedule_G511_3 <- read.csv("Batch1/HR/Cedule_511_3.csv", header = TRUE)[-c(1, 2), ]
Cedule_G600_1 <- read.csv("Batch1/HR/Cedule_600_1.csv", header = TRUE)[-c(1, 2), ]
Cedule_G600_2 <- read.csv("Batch1/HR/Cedule_600_1.csv", header = TRUE)[-c(1, 2), ]
Cedule_G600_3 <- read.csv("Batch1/HR/Cedule_600_3.csv", header = TRUE)[-c(1, 2), ]

# Vectorize dataframe names
dfnames_batch1 <- c("Cedule_G003_1",
    "Cedule_G003_2",
    "Cedule_G003_3",
    "Cedule_G511_1",
    "Cedule_G511_2",
    "Cedule_G511_3",
    "Cedule_G600_1",
    "Cedule_G600_2",
    "Cedule_G600_3"
)

# Rename columns and remove CaCa column
for(i in dfnames_batch1){
    df <- get(i)
    df <- df[-c(1, 2), ]
    df <- as.data.frame(apply(df, 2, as.numeric))
    colnames(df) <- c("Time", "Depth", "MgCa24", "MgCa", "CaCa", "CaCa44", "CaCa48", "MnCa", "SrCa87", "SrCa", "BaCa")
    df$MgCa24 <- df$CaCa <- df$CaCa44 <- df$CaCa48 <- df$SrCa87 <- NULL
    df$Specimen <- strsplit(i, "_")[[1]][2]
    df$Species <- strsplit(i, "_")[[1]][1]
    df$Profile <- strsplit(i, "_")[[1]][3]
    assign(i, df)
}

LA_combined_batch1 <- bind_rows(Cedule_G003_1,
    Cedule_G003_2,
    Cedule_G003_3,
    Cedule_G511_1,
    Cedule_G511_2,
    Cedule_G511_3,
    Cedule_G600_1,
    Cedule_G600_2,
    Cedule_G600_3,
    .id = "Specimen_id"
)

# Create profile plots of batch1 data
Profile_plot_Sr_offset_batch1 <- ggplot(LA_combined_batch1) +
    geom_point(aes(Depth,
            SrCa + as.numeric(Specimen_id) * 2 - 2,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca] (mmol/mol)",
        breaks = seq(0, 25, 1),
        labels = seq(0, 25, 1),
        limits = c(0, 25)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    ggtitle("Offset (+ 2) Sr/Ca curves") +
    theme_bw()

# --------------------------------------------------------------------------------------------
# Create profile per specimen

# G003_1 -----------------------------------------------------------------------
Profile_plot_Mg_G003_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 1),
        aes(Depth,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G003_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 1),
        aes(Depth,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G003_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 1),
        aes(Depth,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G003_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 1),
        aes(Depth,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G003_1 <- ggarrange(Profile_plot_Mg_G003_1,
    Profile_plot_Sr_G003_1,
    Profile_plot_Mn_G003_1,
    Profile_plot_Ba_G003_1,
    ncol = 1)

# G003_2 -----------------------------------------------------------------------
Profile_plot_Mg_G003_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 2),
        aes(Depth,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G003_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 2),
        aes(Depth,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G003_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 2),
        aes(Depth,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G003_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 2),
        aes(Depth,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G003_2 <- ggarrange(Profile_plot_Mg_G003_2,
    Profile_plot_Sr_G003_2,
    Profile_plot_Mn_G003_2,
    Profile_plot_Ba_G003_2,
    ncol = 1)

# G003_3 -----------------------------------------------------------------------
Profile_plot_Mg_G003_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 3),
        aes(Depth,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G003_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 3),
        aes(Depth,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G003_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 3),
        aes(Depth,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G003_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 3),
        aes(Depth,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G003_3 <- ggarrange(Profile_plot_Mg_G003_3,
    Profile_plot_Sr_G003_3,
    Profile_plot_Mn_G003_3,
    Profile_plot_Ba_G003_3,
    ncol = 1)

# G511_1 -----------------------------------------------------------------------
Profile_plot_Mg_G511_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 4),
        aes(Depth,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G511_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 4),
        aes(Depth,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G511_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 4),
        aes(Depth,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G511_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 4),
        aes(Depth,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G511_1 <- ggarrange(Profile_plot_Mg_G511_1,
    Profile_plot_Sr_G511_1,
    Profile_plot_Mn_G511_1,
    Profile_plot_Ba_G511_1,
    ncol = 1)

# G511_2 -----------------------------------------------------------------------
Profile_plot_Mg_G511_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 5),
        aes(Depth,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G511_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 5),
        aes(Depth,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G511_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 5),
        aes(Depth,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G511_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 5),
        aes(Depth,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G511_2 <- ggarrange(Profile_plot_Mg_G511_2,
    Profile_plot_Sr_G511_2,
    Profile_plot_Mn_G511_2,
    Profile_plot_Ba_G511_2,
    ncol = 1)

# G511_3 -----------------------------------------------------------------------
Profile_plot_Mg_G511_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 6),
        aes(Depth,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G511_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 6),
        aes(Depth,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G511_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 6),
        aes(Depth,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G511_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 6),
        aes(Depth,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G511_3 <- ggarrange(Profile_plot_Mg_G511_3,
    Profile_plot_Sr_G511_3,
    Profile_plot_Mn_G511_3,
    Profile_plot_Ba_G511_3,
    ncol = 1)

# G600_1 -----------------------------------------------------------------------
Profile_plot_Mg_G600_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 7),
        aes(Depth,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G600_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 7),
        aes(Depth,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G600_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 7),
        aes(Depth,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G600_1 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 7),
        aes(Depth,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G600_1 <- ggarrange(Profile_plot_Mg_G600_1,
    Profile_plot_Sr_G600_1,
    Profile_plot_Mn_G600_1,
    Profile_plot_Ba_G600_1,
    ncol = 1)

# G600_2 -----------------------------------------------------------------------
Profile_plot_Mg_G600_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 8),
        aes(Depth,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G600_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 8),
        aes(Depth,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G600_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 8),
        aes(Depth,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G600_2 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 8),
        aes(Depth,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G600_2 <- ggarrange(Profile_plot_Mg_G600_2,
    Profile_plot_Sr_G600_2,
    Profile_plot_Mn_G600_2,
    Profile_plot_Ba_G600_2,
    ncol = 1)

# G600_3 -----------------------------------------------------------------------
Profile_plot_Mg_G600_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 9),
        aes(Depth,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G600_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 9),
        aes(Depth,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G600_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 9),
        aes(Depth,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G600_3 <- ggplot(LA_combined_batch1) +
    geom_point(data = subset(LA_combined_batch1, Specimen_id == 9),
        aes(Depth,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G600_3 <- ggarrange(Profile_plot_Mg_G600_3,
    Profile_plot_Sr_G600_3,
    Profile_plot_Mn_G600_3,
    Profile_plot_Ba_G600_3,
    ncol = 1)

# --------------------------------------------------------------------------------------------

# Load data - Batch 2
# Medulis_G177 <- read.csv("Batch2/LA data/HR2/G177.csv", header = TRUE)
# Medulis_G191 <- read.csv("Batch2/LA data/HR2/G191.csv", header = TRUE)
# Medulis_G259 <- read.csv("Batch2/LA data/HR2/G259.csv", header = TRUE)
# Oedulis_G271 <- read.csv("Batch2/LA data/HR2/G271.csv", header = TRUE)
# Oedulis_G271_chalky <- read.csv("Batch2/LA data/HR2/G271_chalky.csv", header = TRUE)
# Oedulis_G282 <- read.csv("Batch2/LA data/HR2/G282.csv", header = TRUE)
# Oedulis_G282_chalky <- read.csv("Batch2/LA data/HR2/G282_chalky.csv", header = TRUE)
# Oedulis_G372 <- read.csv("Batch2/LA data/HR2/G372.csv", header = TRUE)
# Oedulis_G372_chalky <- read.csv("Batch2/LA data/HR2/G372_chalky.csv", header = TRUE)
# Cedule_G457 <- read.csv("Batch2/LA data/HR2/G457.csv", header = TRUE)
# Cedule_G472 <- read.csv("Batch2/LA data/HR2/G472.csv", header = TRUE)
# Cedule_G555 <- read.csv("Batch2/LA data/HR2/G555.csv", header = TRUE)

# Load from xlsx
filename <- "Batch2/LA data/01_20211025_SEQ326_NdW_LR__profiles.xlsx"
sheets <- excel_sheets(filename)
datalist <- lapply(sheets, function(X) read_excel(filename, sheet = X))
names(datalist) <- sheets

Medulis_G177 <- as.data.frame(datalist["G177"])
Medulis_G191 <- as.data.frame(datalist["G191"])
Medulis_G259 <- as.data.frame(datalist["G259"])
Oedulis_G271 <- as.data.frame(datalist["G271"])
Oedulis_G271_chalky <- as.data.frame(datalist["G271_chalky"])
Oedulis_G282 <- as.data.frame(datalist["G282"])
Oedulis_G282_chalky <- as.data.frame(datalist["G282_chalky"])
Oedulis_G372 <- as.data.frame(datalist["G372"])
Oedulis_G372_chalky <- as.data.frame(datalist["G372_chalky"])
Cedule_G457 <- as.data.frame(datalist["G457"])
Cedule_G472 <- as.data.frame(datalist["G472"])
Cedule_G555 <- as.data.frame(datalist["G555"])

# Vectorize dataframe names
dfnames <- c("Medulis_G177",
    "Medulis_G191",
    "Medulis_G259",
    "Oedulis_G271",
    "Oedulis_G271_chalky",
    "Oedulis_G282",
    "Oedulis_G282_chalky",
    "Oedulis_G372",
    "Oedulis_G372_chalky",
    "Cedule_G457",
    "Cedule_G472",
    "Cedule_G555"
)

# Rename columns and remove CaCa column
for(i in dfnames){
    df <- get(i)
    colnames(df) <- c("Time", "Depth", "Xpos", "Ypos", "NaCa", "MgCa", "CaCa", "MnCa", "SrCa", "BaCa")
    df$CaCa <- NULL
    df$Specimen <- strsplit(i, "_")[[1]][2]
    df$Species <- strsplit(i, "_")[[1]][1]

    # calculate distance along profile
    df$Distance <- 0
    for(j in 2:length(df$Xpos)){
        df$Distance[j] <- df$Distance[j - 1] + sqrt((df$Xpos[j] - df$Xpos[j - 1]) ^ 2 + (df$Ypos[j] - df$Ypos[j - 1]) ^ 2)
    }
    assign(i, df)
}

# Load relative start and end positions of chalky lines in O. edulis
chalkypos <- read.csv("Batch2/Oedulis_chalky_positions.csv", header = TRUE)

# Update position of chalky data
Oedulis_G271_chalky$Distance <- Oedulis_G271_chalky$Distance + chalkypos$start[1] * 1000
Oedulis_G282_chalky$Distance <- Oedulis_G282_chalky$Distance + chalkypos$start[2] * 1000
Oedulis_G372_chalky$Distance <- Oedulis_G372_chalky$Distance + chalkypos$start[3] * 1000

# Combine data
LA_combined <- bind_rows(Medulis_G177,
    Medulis_G191,
    Medulis_G259,
    Oedulis_G271,
    Oedulis_G271_chalky,
    Oedulis_G282,
    Oedulis_G282_chalky,
    Oedulis_G372,
    Oedulis_G372_chalky,
    Cedule_G457,
    Cedule_G472,
    Cedule_G555,
    .id = "Specimen_id"
)

#save(LA_combined, file = "Batch2/LA data/LA_combined_batch2.Rdata")

# Recalculate to concentrations (in mass fraction) using fixed Ca concentration of 40.04 wt%
LA_combined_mass <- data.frame(
    Time = LA_combined$Time,
    Depth = LA_combined$Distance,
    Na = LA_combined$NaCa / 1000 * 22.9 / 100.09,
    Mg = LA_combined$MgCa / 1000 * 24.31 / 100.09,
    Mn = LA_combined$MnCa / 1000 * 54.94 / 100.09,
    Sr = LA_combined$SrCa / 1000 * 87.62 / 100.09,
    Ba = LA_combined$BaCa / 1000 * 137.33 / 100.09,
    Specimen = LA_combined$Specimen,
    Species = LA_combined$Species
)

# Recalculate to concentrations (in mol fraction) using fixed Ca fraction of 0.2
LA_combined_mol <- data.frame(
    Time = LA_combined$Time,
    Depth = LA_combined$Distance,
    Na = LA_combined$NaCa / 1000 * 0.2,
    Mg = LA_combined$MgCa / 1000 * 0.2,
    Mn = LA_combined$MnCa / 1000 * 0.2,
    Sr = LA_combined$SrCa / 1000 * 0.2,
    Ba = LA_combined$BaCa / 1000 * 0.2,
    Specimen = LA_combined$Specimen,
    Species = LA_combined$Species
)

# Create box-violin plots
Boxplot_Na <- ggplot(LA_combined_mol) +
    geom_violin(aes(x = Specimen,
            y = Na,
            fill = Species),
        kernel = "rectangular",
        scale = "width",
        cex = 0,
        alpha = 0.5,
        color = NA,
        trim = FALSE,
        show.legend = FALSE) +
    geom_boxplot(aes(x = Specimen,
            y = Na,
            color = Species),
        coef = 2,
        width = 0.3,
        outlier.shape = NA) +
    scale_y_continuous("[Na] (umol/mol)",
        breaks = seq(0, 0.02, 0.002),
        labels = seq(0, 20000, 2000),
        limits = c(0, 0.02)) +
    theme_bw()

Boxplot_Mg <- ggplot(LA_combined_mol) +
    geom_violin(aes(x = Specimen,
            y = Mg,
            fill = Species),
        kernel = "rectangular",
        scale = "width",
        cex = 0,
        alpha = 0.5,
        color = NA,
        trim = FALSE,
        show.legend = FALSE) +
    geom_boxplot(aes(x = Specimen,
            y = Mg,
            color = Species),
        coef = 2,
        width = 0.3,
        outlier.shape = NA) +
    scale_y_continuous("[Mg] (umol/mol)",
        breaks = seq(0, 0.005, 0.001),
        labels = seq(0, 5000, 1000),
        limits = c(0, 0.005)) +
    theme_bw()

Boxplot_Mn <- ggplot(LA_combined_mol) +
    geom_violin(aes(x = Specimen,
            y = Mn,
            fill = Species),
        kernel = "rectangular",
        scale = "width",
        cex = 0,
        alpha = 0.5,
        color = NA,
        trim = FALSE,
        show.legend = FALSE) +
    geom_boxplot(aes(x = Specimen,
            y = Mn,
            color = Species),
        coef = 2,
        width = 0.3,
        outlier.shape = NA) +
    scale_y_continuous("[Mn] (umol/mol)",
        breaks = seq(0, 5E-5, 1E-5),
        labels = seq(0, 50, 10),
        limits = c(0, 5E-5)) +
    theme_bw()

Boxplot_Sr <- ggplot(LA_combined_mol) +
    geom_violin(aes(x = Specimen,
            y = Sr,
            fill = Species),
        kernel = "rectangular",
        scale = "width",
        cex = 0,
        alpha = 0.5,
        color = NA,
        trim = FALSE,
        show.legend = FALSE) +
    geom_boxplot(aes(x = Specimen,
            y = Sr,
            color = Species),
        coef = 2,
        width = 0.3,
        outlier.shape = NA) +
    scale_y_continuous("[Sr] (umol/mol)",
        breaks = seq(0, 0.001, 0.0001),
        labels = seq(0, 1000, 100),
        limits = c(0, 0.001)) +
    theme_bw()

Boxplot_Ba <- ggplot(LA_combined_mol) +
    geom_violin(aes(x = Specimen,
            y = Ba,
            fill = Species),
        kernel = "rectangular",
        scale = "width",
        cex = 0,
        alpha = 0.5,
        color = NA,
        trim = FALSE,
        show.legend = FALSE) +
    geom_boxplot(aes(x = Specimen,
            y = Ba,
            color = Species),
        coef = 2,
        width = 0.3,
        outlier.shape = NA) +
    scale_y_continuous("[Ba] (umol/mol)",
        breaks = seq(0, 2E-6, 5E-7),
        labels = seq(0, 2, 0.5),
        limits = c(0, 2E-6)) +
    theme_bw()

Combined_boxplots <- grid.arrange(Boxplot_Na + theme(legend.position = "none"),
    Boxplot_Mg + theme(legend.position = "none"),
    Boxplot_Sr + theme(legend.position = "none"),
    Boxplot_Mn + theme(legend.position = "none"),
    Boxplot_Ba + theme(legend.position = "none"),
    ncol = 2)

# --------------------------------------------------------------------------------------------

# Load smoothed data
filename_smooth <- "Batch2/LA data/01_20211025_SEQ326_NdW_LR__profiles_smooth_0.05.xlsx"
sheets_smooth <- excel_sheets(filename_smooth)
datalist <- lapply(sheets_smooth, function(X) read_excel(filename_smooth, sheet = X))
names(datalist) <- sheets_smoothLR

Medulis_G177_smooth <- as.data.frame(datalist["G177"])
Medulis_G191_smooth <- as.data.frame(datalist["G191"])
Medulis_G259_smooth <- as.data.frame(datalist["G259"])
Oedulis_G271_smooth <- as.data.frame(datalist["G271"])
Oedulis_G271_chalky_smooth <- as.data.frame(datalist["G271_chalky"])
Oedulis_G282_smooth <- as.data.frame(datalist["G282"])
Oedulis_G282_chalky_smooth <- as.data.frame(datalist["G282_chalky"])
Oedulis_G372_smooth <- as.data.frame(datalist["G372"])
Oedulis_G372_chalky_smooth <- as.data.frame(datalist["G372_chalky"])
Cedule_G457_smooth <- as.data.frame(datalist["G457"])
Cedule_G472_smooth <- as.data.frame(datalist["G472"])
Cedule_G555_smooth <- as.data.frame(datalist["G555"])

# Vectorize dataframe names
dfnames_smooth <- c("Medulis_G177_smooth",
    "Medulis_G191_smooth",
    "Medulis_G259_smooth",
    "Oedulis_G271_smooth",
    "Oedulis_G271_chalky_smooth",
    "Oedulis_G282_smooth",
    "Oedulis_G282_chalky_smooth",
    "Oedulis_G372_smooth",
    "Oedulis_G372_chalky_smooth",
    "Cedule_G457_smooth",
    "Cedule_G472_smooth",
    "Cedule_G555_smooth"
)

# Rename columns and remove CaCa column
for(i in dfnames_smooth){
    df <- get(i)
    colnames(df) <- c("Time", "Depth", "Xpos", "Ypos", "NaCa", "MgCa", "CaCa", "MnCa", "SrCa", "BaCa")
    df$CaCa <- NULL
    df$Specimen <- strsplit(i, "_")[[1]][2]
    df$Species <- strsplit(i, "_")[[1]][1]

    # calculate distance along profile
    df$Distance <- 0
    for(j in 2:length(df$Xpos)){
        df$Distance[j] <- df$Distance[j - 1] + sqrt((df$Xpos[j] - df$Xpos[j - 1]) ^ 2 + (df$Ypos[j] - df$Ypos[j - 1]) ^ 2)
    }
    assign(i, df)
}

# Update position of chalky data
Oedulis_G271_chalky_smooth$Distance <- Oedulis_G271_chalky_smooth$Distance + chalkypos$start[1] * 1000
Oedulis_G282_chalky_smooth$Distance <- Oedulis_G282_chalky_smooth$Distance + chalkypos$start[2] * 1000
Oedulis_G372_chalky_smooth$Distance <- Oedulis_G372_chalky_smooth$Distance + chalkypos$start[3] * 1000

# Combine data
LA_combined_smooth <- bind_rows(Medulis_G177_smooth,
    Medulis_G191_smooth,
    Medulis_G259_smooth,
    Oedulis_G271_smooth,
    Oedulis_G271_chalky_smooth,
    Oedulis_G282_smooth,
    Oedulis_G282_chalky_smooth,
    Oedulis_G372_smooth,
    Oedulis_G372_chalky_smooth,
    Cedule_G457_smooth,
    Cedule_G472_smooth,
    Cedule_G555_smooth,
    .id = "Specimen_id"
)

#save(LA_combined_smooth, file = "Batch2/LA data/LA_combined_batch2_smooth.Rdata")

# Create profile plots
Profile_plot_Sr_offset_all <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, !(Specimen_id %in% c(5, 7, 9))),
        aes(Distance,
            SrCa + as.numeric(Specimen_id) - 1,
            col = Species),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca] (mmol/mol)",
        breaks = seq(0, 20, 1),
        labels = seq(0, 20, 1),
        limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    ggtitle("Offset (+ 1) Sr/Ca curves") +
    theme_bw()

Profile_plot_Sr_offset_all2 <- ggplot(LA_combined_smooth) +
    geom_point(data = LA_combined_smooth,
        aes(Distance,
            SrCa + as.numeric(Specimen_id) - 1,
            col = Species),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca] (mmol/mol)",
        breaks = seq(0, 20, 1),
        labels = seq(0, 20, 1),
        limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    ggtitle("Offset (+ 1) Sr/Ca curves") +
    theme_bw()

# Isolate C. edule data
Profile_plot_Sr_Cedule <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Species == "Cedule"),
        aes(Distance,
            SrCa + as.numeric(Specimen_id) * 2 - 20,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca] (mmol/mol)",
        breaks = seq(0, 20, 1),
        labels = seq(0, 20, 1),
        limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    ggtitle("Offset (+ 2) Sr/Ca curves Cerastoderma edule") +
    theme_bw()

# Isolate O. edulis data
Profile_plot_Sr_Oedulis <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Species == "Oedulis"),
        aes(Distance,
            SrCa + as.numeric(Specimen_id) - 4,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca] (mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    ggtitle("Offset (+ 2) Sr/Ca curves Ostrea edulis") +
    theme_bw()

# Isolate O. edulis data, excluding chalky records
Profile_plot_Sr_Oedulis_nochalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Species == "Oedulis" & !(Specimen_id %in% c(5, 7, 9))),
        aes(Distance,
            SrCa + as.numeric(Specimen_id) - 4,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca] (mmol/mol)",
        breaks = seq(0, 10, 1),
        labels = seq(0, 10, 1),
        limits = c(0, 10)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    ggtitle("Offset (+ 2) Sr/Ca curves Ostrea edulis") +
    theme_bw()

# Isolate M. edulis data
Profile_plot_Sr_Medulis <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Species == "Medulis"),
        aes(Distance,
            SrCa + as.numeric(Specimen_id) * 2 - 2,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca] (mmol/mol)",
        breaks = seq(0, 10, 1),
        labels = seq(0, 10, 1),
        limits = c(0, 10)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    ggtitle("Offset (+ 2) Sr/Ca curves Mytilus edulis") +
    theme_bw()

# --------------------------------------------------------------------------------------------
# Create profile per specimen

Profile_plot_Na_G177 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 1),
        aes(Distance,
            NaCa),
        col = brewer.pal(5, "Dark2")[1],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 25, 10),
        labels = seq(0, 25, 10),
        limits = c(0, 25)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G177 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 1),
        aes(Distance,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 20, 5),
        labels = seq(0, 20, 5),
        limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G177 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 1),
        aes(Distance,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G177 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 1),
        aes(Distance,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.1, 0.01),
        labels = seq(0, 100, 10),
        limits = c(0, 0.1)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G177 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 1),
        aes(Distance,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.003, 0.0005),
        labels = seq(0, 3, 0.5),
        limits = c(0, 0.003)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G177 <- grid.arrange(Profile_plot_Na_G177,
    Profile_plot_Mg_G177,
    Profile_plot_Sr_G177,
    Profile_plot_Mn_G177,
    Profile_plot_Ba_G177,
    ncol = 1)

# --------------------------------------------------------------------------------------------
Profile_plot_Na_G191 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 2),
        aes(Distance,
            NaCa),
        col = brewer.pal(5, "Dark2")[1],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 35, 5),
        labels = seq(0, 35, 5),
        limits = c(0, 35)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G191 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 2),
        aes(Distance,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 20, 5),
        labels = seq(0, 20, 5),
        limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G191 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 2),
        aes(Distance,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G191 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 2),
        aes(Distance,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.1, 0.01),
        labels = seq(0, 100, 10),
        limits = c(0, 0.1)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G191 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 2),
        aes(Distance,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.003, 0.0005),
        labels = seq(0, 3, 0.5),
        limits = c(0, 0.003)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G191 <- grid.arrange(Profile_plot_Na_G191,
    Profile_plot_Mg_G191,
    Profile_plot_Sr_G191,
    Profile_plot_Mn_G191,
    Profile_plot_Ba_G191,
    ncol = 1)

# --------------------------------------------------------------------------------------------
Profile_plot_Na_G259 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 3),
        aes(Distance,
            NaCa),
        col = brewer.pal(5, "Dark2")[1],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 35, 5),
        labels = seq(0, 35, 5),
        limits = c(0, 35)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G259 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 3),
        aes(Distance,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 20, 5),
        labels = seq(0, 20, 5),
        limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G259 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 3),
        aes(Distance,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G259 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 3),
        aes(Distance,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.1, 0.01),
        labels = seq(0, 100, 10),
        limits = c(0, 0.1)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G259 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 3),
        aes(Distance,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.003, 0.0005),
        labels = seq(0, 3, 0.5),
        limits = c(0, 0.003)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G259 <- grid.arrange(Profile_plot_Na_G259,
    Profile_plot_Mg_G259,
    Profile_plot_Sr_G259,
    Profile_plot_Mn_G259,
    Profile_plot_Ba_G259,
    ncol = 1)

# --------------------------------------------------------------------------------------------
Profile_plot_Na_G271 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 4),
        aes(Distance,
            NaCa),
        col = brewer.pal(5, "Dark2")[1],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 35, 5),
        labels = seq(0, 35, 5),
        limits = c(0, 35)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G271 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 4),
        aes(Distance,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 20, 5),
        labels = seq(0, 20, 5),
        limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G271 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 4),
        aes(Distance,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 1.5, .5),
        labels = seq(0, 1.5, .5),
        limits = c(0, 1.5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G271 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 4),
        aes(Distance,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.5, 0.1),
        labels = seq(0, 500, 100),
        limits = c(0, 0.5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G271 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 4),
        aes(Distance,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G271 <- grid.arrange(Profile_plot_Na_G271,
    Profile_plot_Mg_G271,
    Profile_plot_Sr_G271,
    Profile_plot_Mn_G271,
    Profile_plot_Ba_G271,
    ncol = 1)

# --------------------------------------------------------------------------------------------
Profile_plot_Na_G271_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 5),
               aes(Distance,
                   NaCa),
               col = brewer.pal(5, "Dark2")[1],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 50, 5),
                       labels = seq(0, 50, 5),
                       limits = c(0, 50)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G271_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 5),
               aes(Distance,
                   MgCa),
               col = brewer.pal(5, "Dark2")[2],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 10, 2),
                       labels = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G271_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 5),
               aes(Distance,
                   SrCa),
               col = brewer.pal(5, "Dark2")[3],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 1, .2),
                       labels = seq(0, 1, .2),
                       limits = c(0, 1)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G271_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 5),
               aes(Distance,
                   MnCa),
               col = brewer.pal(5, "Dark2")[4],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
                       breaks = seq(0, 0.05, 0.01),
                       labels = seq(0, 50, 10),
                       limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G271_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 5),
               aes(Distance,
                   BaCa),
               col = brewer.pal(5, "Dark2")[5],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
                       breaks = seq(0, 0.002, 0.0005),
                       labels = seq(0, 2, .5),
                       limits = c(0, 0.002)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G271_chalky <- grid.arrange(Profile_plot_Na_G271_chalky,
                                       Profile_plot_Mg_G271_chalky,
                                       Profile_plot_Sr_G271_chalky,
                                       Profile_plot_Mn_G271_chalky,
                                       Profile_plot_Ba_G271_chalky,
                                       ncol = 1)

# --------------------------------------------------------------------------------------------
Profile_plot_Na_G282 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 6),
               aes(Distance,
                   NaCa),
               col = brewer.pal(5, "Dark2")[1],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 35, 5),
                       labels = seq(0, 35, 5),
                       limits = c(0, 35)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G282 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 6),
               aes(Distance,
                   MgCa),
               col = brewer.pal(5, "Dark2")[2],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 20, 5),
                       labels = seq(0, 20, 5),
                       limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G282 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 6),
               aes(Distance,
                   SrCa),
               col = brewer.pal(5, "Dark2")[3],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 1.5, .5),
                       labels = seq(0, 1.5, .5),
                       limits = c(0, 1.5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G282 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 6),
               aes(Distance,
                   MnCa),
               col = brewer.pal(5, "Dark2")[4],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
                       breaks = seq(0, 0.5, 0.1),
                       labels = seq(0, 500, 100),
                       limits = c(0, 0.5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G282 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 6),
               aes(Distance,
                   BaCa),
               col = brewer.pal(5, "Dark2")[5],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
                       breaks = seq(0, 0.01, 0.002),
                       labels = seq(0, 10, 2),
                       limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G282 <- grid.arrange(Profile_plot_Na_G282,
                                       Profile_plot_Mg_G282,
                                       Profile_plot_Sr_G282,
                                       Profile_plot_Mn_G282,
                                       Profile_plot_Ba_G282,
                                       ncol = 1)

# --------------------------------------------------------------------------------------------
Profile_plot_Na_G282_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 7),
               aes(Distance,
                   NaCa),
               col = brewer.pal(5, "Dark2")[1],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 250, 50),
                       labels = seq(0, 250, 50),
                       limits = c(0, 250)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G282_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 7),
               aes(Distance,
                   MgCa),
               col = brewer.pal(5, "Dark2")[2],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 35, 5),
                       labels = seq(0, 35, 5),
                       limits = c(0, 35)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G282_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 7),
               aes(Distance,
                   SrCa),
               col = brewer.pal(5, "Dark2")[3],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 1.5, .5),
                       labels = seq(0, 1.5, .5),
                       limits = c(0, 1.5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G282_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 7),
               aes(Distance,
                   MnCa),
               col = brewer.pal(5, "Dark2")[4],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
                       breaks = seq(0, 0.05, 0.01),
                       labels = seq(0, 50, 10),
                       limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G282_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 7),
               aes(Distance,
                   BaCa),
               col = brewer.pal(5, "Dark2")[5],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
                       breaks = seq(0, 0.002, 0.0005),
                       labels = seq(0, 2, .5),
                       limits = c(0, 0.002)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G282_chalky <- grid.arrange(Profile_plot_Na_G282_chalky,
                                       Profile_plot_Mg_G282_chalky,
                                       Profile_plot_Sr_G282_chalky,
                                       Profile_plot_Mn_G282_chalky,
                                       Profile_plot_Ba_G282_chalky,
                                       ncol = 1)
                                       
# --------------------------------------------------------------------------------------------
Profile_plot_Na_G372 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 8),
        aes(Distance,
            NaCa),
        col = brewer.pal(5, "Dark2")[1],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 100, 10),
        labels = seq(0, 100, 10),
        limits = c(0, 100)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G372 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 8),
        aes(Distance,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 30, 5),
        labels = seq(0, 30, 5),
        limits = c(0, 30)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G372 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 8),
        aes(Distance,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 2.5, .5),
        labels = seq(0, 2.5, .5),
        limits = c(0, 2)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G372 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 8),
        aes(Distance,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.5, 0.1),
        labels = seq(0, 500, 100),
        limits = c(0, 0.5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G372 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 8),
        aes(Distance,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G372 <- grid.arrange(Profile_plot_Na_G372,
    Profile_plot_Mg_G372,
    Profile_plot_Sr_G372,
    Profile_plot_Mn_G372,
    Profile_plot_Ba_G372,
    ncol = 1)

# --------------------------------------------------------------------------------------------
Profile_plot_Na_G372_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 9),
               aes(Distance,
                   NaCa),
               col = brewer.pal(5, "Dark2")[1],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 100, 20),
                       labels = seq(0, 100, 20),
                       limits = c(0, 100)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G372_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 9),
               aes(Distance,
                   MgCa),
               col = brewer.pal(5, "Dark2")[2],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 25, 5),
                       labels = seq(0, 25, 5),
                       limits = c(0, 25)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G372_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 9),
               aes(Distance,
                   SrCa),
               col = brewer.pal(5, "Dark2")[3],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
                       breaks = seq(0, 1.5, .5),
                       labels = seq(0, 1.5, .5),
                       limits = c(0, 1.5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G372_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 9),
               aes(Distance,
                   MnCa),
               col = brewer.pal(5, "Dark2")[4],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
                       breaks = seq(0, 0.06, 0.01),
                       labels = seq(0, 60, 10),
                       limits = c(0, 0.06)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G372_chalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 9),
               aes(Distance,
                   BaCa),
               col = brewer.pal(5, "Dark2")[5],
               alpha = 0.1,
               size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
                       breaks = seq(0, 0.002, 0.0005),
                       labels = seq(0, 2, .5),
                       limits = c(0, 0.002)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G372_chalky <- grid.arrange(Profile_plot_Na_G372_chalky,
                                       Profile_plot_Mg_G372_chalky,
                                       Profile_plot_Sr_G372_chalky,
                                       Profile_plot_Mn_G372_chalky,
                                       Profile_plot_Ba_G372_chalky,
                                       ncol = 1)

# --------------------------------------------------------------------------------------------
Profile_plot_Na_G457 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 10),
        aes(Distance,
            NaCa),
        col = brewer.pal(5, "Dark2")[1],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 40, 10),
        labels = seq(0, 40, 10),
        limits = c(0, 40)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G457 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 10),
        aes(Distance,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G457 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 10),
        aes(Distance,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G457 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 10),
        aes(Distance,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.1, 0.01),
        labels = seq(0, 100, 10),
        limits = c(0, 0.1)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G457 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 10),
        aes(Distance,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G457 <- grid.arrange(Profile_plot_Na_G457,
    Profile_plot_Mg_G457,
    Profile_plot_Sr_G457,
    Profile_plot_Mn_G457,
    Profile_plot_Ba_G457,
    ncol = 1)

# --------------------------------------------------------------------------------------------
Profile_plot_Na_G472 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 11),
        aes(Distance,
            NaCa),
        col = brewer.pal(5, "Dark2")[1],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 70, 10),
        labels = seq(0, 70, 10),
        limits = c(0, 70)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G472 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 11),
        aes(Distance,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G472 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 11),
        aes(Distance,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G472 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 11),
        aes(Distance,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G472 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 11),
        aes(Distance,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G472 <- grid.arrange(Profile_plot_Na_G472,
    Profile_plot_Mg_G472,
    Profile_plot_Sr_G472,
    Profile_plot_Mn_G472,
    Profile_plot_Ba_G472,
    ncol = 1)

# --------------------------------------------------------------------------------------------
Profile_plot_Na_G555 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 12),
        aes(Distance,
            NaCa),
        col = brewer.pal(5, "Dark2")[1],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Na]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 70, 10),
        labels = seq(0, 70, 10),
        limits = c(0, 70)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mg_G555 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 12),
        aes(Distance,
            MgCa),
        col = brewer.pal(5, "Dark2")[2],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mg]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 5, 1),
        labels = seq(0, 5, 1),
        limits = c(0, 5)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Sr_G555 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 12),
        aes(Distance,
            SrCa),
        col = brewer.pal(5, "Dark2")[3],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)",
        breaks = seq(0, 8, 1),
        labels = seq(0, 8, 1),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Mn_G555 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 12),
        aes(Distance,
            MnCa),
        col = brewer.pal(5, "Dark2")[4],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.05, 0.01),
        labels = seq(0, 50, 10),
        limits = c(0, 0.05)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Profile_plot_Ba_G555 <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Specimen_id == 12),
        aes(Distance,
            BaCa),
        col = brewer.pal(5, "Dark2")[5],
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("[Ba]/[Ca]\n(umol/mol)",
        breaks = seq(0, 0.01, 0.002),
        labels = seq(0, 10, 2),
        limits = c(0, 0.01)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    #ggtitle(paste("TE Profiles", dfnames[1])) +
    theme_bw()

Combined_profiles_G555 <- grid.arrange(Profile_plot_Na_G555,
    Profile_plot_Mg_G555,
    Profile_plot_Sr_G555,
    Profile_plot_Mn_G555,
    Profile_plot_Ba_G555,
    ncol = 1)