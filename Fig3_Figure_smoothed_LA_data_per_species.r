# Plot smothed Sr profiles for Figure 1
# Project "LAICPMS_Sr_spiking"

require(tidyverse)
require(RColorBrewer)
require(readxl)
require(ggpubr)

# Load data - Batch 1
Cedule_G003_1 <- read.csv("Batch1/HR/Cedule_003_1.csv", header = TRUE)[-c(1, 2), ]
Cedule_G003_2 <- read.csv("Batch1/HR/Cedule_003_2.csv", header = TRUE)
Cedule_G003_3 <- read.csv("Batch1/HR/Cedule_003_3.csv", header = TRUE)
Cedule_G511_1 <- read.csv("Batch1/HR/Cedule_511_1.csv", header = TRUE)
Cedule_G511_2 <- read.csv("Batch1/HR/Cedule_511_2.csv", header = TRUE)
Cedule_G511_3 <- read.csv("Batch1/HR/Cedule_511_3.csv", header = TRUE)
Cedule_G600_1 <- read.csv("Batch1/HR/Cedule_600_1.csv", header = TRUE)
Cedule_G600_2 <- read.csv("Batch1/HR/Cedule_600_1.csv", header = TRUE)
Cedule_G600_3 <- read.csv("Batch1/HR/Cedule_600_3.csv", header = TRUE)

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
    df$Depth2 <- max(df$Depth) - df$Depth # Invert growth direction
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
    geom_point(aes(Depth2,
            SrCa + as.numeric(Specimen_id) * 2 - 2,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("Sr/Ca (mmol/mol)",
        breaks = seq(0, 23, 5),
        labels = seq(0, 23, 5)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    coord_cartesian(ylim = c(0, 23)) +
    ggtitle("Offset (+ 2 mmol/mol) Sr/Ca curves \n Cerastoderma edule batch 1") +
    theme_bw()

# ------------------------------------------------------------------------------

# Load smoothed data
filename_smooth <- "Batch2/LA data/01_20211025_SEQ326_NdW_LR__profiles_smooth_0.05.xlsx"
sheets_smooth <- excel_sheets(filename_smooth)
datalist <- lapply(sheets_smooth, function(X) read_excel(filename_smooth, sheet = X))
names(datalist) <- sheets_smooth

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

# Load relative start and end positions of chalky lines in O. edulis
chalkypos <- read.csv("Batch2/Oedulis_chalky_positions.csv", header = TRUE)

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
    scale_y_continuous("Sr/Ca (mmol/mol)",
        breaks = seq(0, 20, 2),
        labels = seq(0, 20, 2),
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
    scale_y_continuous("Sr/Ca (mmol/mol)",
        breaks = seq(0, 20, 2),
        labels = seq(0, 20, 2),
        limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [um]") +
    ggtitle("Offset (+ 1) Sr/Ca curves") +
    theme_bw()

# Isolate C. edule data
Profile_plot_Sr_Cedule <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Species == "Cedule"),
        aes(Distance / 1000,
            SrCa + as.numeric(Specimen_id) * 2 - 20,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("Sr/Ca (mmol/mol)",
        breaks = seq(0, 20, 5),
        labels = seq(0, 20, 5),
        limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    ggtitle("Offset (+ 2 mmol/mol) Sr/Ca curves \n Cerastoderma edule") +
    theme_bw()

# Isolate O. edulis data
Profile_plot_Sr_Oedulis <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Species == "Oedulis" & Specimen_id %in% c(4, 6, 8)),
        aes(Distance / 1000,
            SrCa + as.numeric(Specimen_id) - 4,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("Sr/Ca (mmol/mol)",
        breaks = seq(0, 10, 2),
        labels = seq(0, 10, 2),
        limits = c(0, 8)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    ggtitle("Offset (+ 2 mmol/mol) Sr/Ca curves \n Ostrea edulis") +
    theme_bw()

# Isolate O. edulis data, excluding chalky records
Profile_plot_Sr_Oedulis_nochalky <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Species == "Oedulis" & !(Specimen_id %in% c(5, 7, 9))),
        aes(Distance / 1000,
            SrCa + as.numeric(Specimen_id) - 4,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("Sr/Ca (mmol/mol)",
        breaks = seq(0, 10, 2),
        labels = seq(0, 10, 2),
        limits = c(0, 10)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    ggtitle("Offset (+ 2 mmol/mol) Sr/Ca curves \n Ostrea edulis") +
    theme_bw()

# Isolate M. edulis data
Profile_plot_Sr_Medulis <- ggplot(LA_combined_smooth) +
    geom_point(data = subset(LA_combined_smooth, Species == "Medulis"),
        aes(Distance / 1000,
            SrCa + as.numeric(Specimen_id) * 2 - 2,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    scale_y_continuous("Sr/Ca (mmol/mol)",
        breaks = seq(0, 10, 2),
        labels = seq(0, 10, 2),
        limits = c(0, 10)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    ggtitle("Offset (+ 2 mmol/mol) Sr/Ca curves \n Mytilus edulis") +
    theme_bw()


# Combine species plots into one multi-panel plot
Species_combined <- ggarrange(
    Profile_plot_Sr_offset_batch1 +
        theme(legend.position = "none") +
        coord_cartesian(ylim = c(0, 23)),
    Profile_plot_Sr_Cedule +
        theme(legend.position = "none") +
        coord_cartesian(ylim = c(0, 23)),
    Profile_plot_Sr_Medulis +
        theme(legend.position = "none") +
        coord_cartesian(ylim = c(0, 10),
            xlim = c(0, 18)),
    Profile_plot_Sr_Oedulis +
        theme(legend.position = "none") +
        coord_cartesian(ylim = c(0, 10),
            xlim = c(0, 36)),
    ncol = 2,
    nrow = 2,
    labels = c("A", "B", "C", "D")
)
