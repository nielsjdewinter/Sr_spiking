# Plot XRF profiles of Sr spiking

require(tidyverse)
require(RColorBrewer)
require(ggpubr)

# Load data
Cedule003 <- read.csv("XRF_Sr_spiking/Cedule003_cal.csv", header = TRUE)
Cedule511 <- read.csv("XRF_Sr_spiking/Cedule511_cal.csv", header = TRUE)
Cedule600 <- read.csv("XRF_Sr_spiking/Cedule600_cal.csv", header = TRUE)

# Remove bad (repeated) values in Cedule003
Cedule003[0:54, -c(1, 2)] <- NA

# Combine data
dat <- rbind(select(Cedule003, "X", "Na", "Mg", "Al", "Si", "P", "S", "Ca", "Ti", "Cr", "Mn", "Fe", "Sr", "Na_SD", "Mg_SD", "Al_SD", "Si_SD", "P_SD", "S_SD", "Ca_SD", "Ti_SD", "Cr_SD", "Mn_SD", "Fe_SD", "Sr_SD"),
    select(Cedule511, "X", "Na", "Mg", "Al", "Si", "P", "S", "Ca", "Ti", "Cr", "Mn", "Fe", "Sr", "Na_SD", "Mg_SD", "Al_SD", "Si_SD", "P_SD", "S_SD", "Ca_SD", "Ti_SD", "Cr_SD", "Mn_SD", "Fe_SD", "Sr_SD"),
    select(Cedule600, "X", "Na", "Mg", "Al", "Si", "P", "S", "Ca", "Ti", "Cr", "Mn", "Fe", "Sr", "Na_SD", "Mg_SD", "Al_SD", "Si_SD", "P_SD", "S_SD", "Ca_SD", "Ti_SD", "Cr_SD", "Mn_SD", "Fe_SD", "Sr_SD")
)
dat$Specimen <- c(rep("G003", nrow(Cedule003)), rep("G511", nrow(Cedule511)), rep("G600", nrow(Cedule600)))
dat$Specimen_id <- c(rep("0", nrow(Cedule003)), rep("1", nrow(Cedule511)), rep("2", nrow(Cedule600)))
colnames(dat)[1] <- "point" # spectrum number
dat$Distance <- dat$point * 0.025 # distance in mm

# Calculate element ratios
dat$SrCa <- (dat$Sr / 87.62) / (dat$Ca / 40.08) # Sr/Ca in mol/mol
dat$MgCa <- (dat$Mg / 24.30) / (dat$Ca / 40.08) # Mg/Ca in mol/mol
dat$SCa <- (dat$S / 32.06) / (dat$Ca / 40.08) # S/Ca in mol/mol
dat$MnCa <- (dat$Mn / 54.94) / (dat$Ca / 40.08) # Mn/Ca in mol/mol
dat$PCa <- (dat$P / 30.97) / (dat$Ca / 40.08) # P/Ca in mol/mol

# Plot Sr lines with offset
Profile_plot_Sr_XRF_offset <- ggplot(dat) +
    geom_point(aes(Distance,
            SrCa * 1000 + as.numeric(Specimen_id) * 2,
            col = Specimen),
        alpha = 0.1,
        size = 0.1) +
    geom_point(aes(Distance,
            SrCa * 1000 + as.numeric(Specimen_id) * 2,
            col = Specimen),
        size = 1) +
    geom_line(aes(Distance,
            SrCa * 1000 + as.numeric(Specimen_id) * 2,
            col = Specimen)) +
    geom_ribbon(aes(x = Distance,
            ymax = SrCa * 1000 + as.numeric(Specimen_id) * 2,
            ymin = 1.5 + as.numeric(Specimen_id) * 2,
            fill = Specimen),
        alpha = 0.3) +
    scale_y_continuous("[Sr]/[Ca] (mmol/mol)",
        breaks = seq(0, 20, 2),
        labels = seq(0, 20, 2),
        limits = c(0, 20)) +
    scale_x_continuous("Distance from ventral margin [mm]") +
    ggtitle("Offset (+ 2 mmol/mol) Sr/Ca curves") +
    coord_cartesian(xlim = c(0, 8), ylim = c(0, 15)) +
    theme_bw()

# Plot combined plot with multiple elements
# G003
Profile_plot_Sr_G003 <- ggplot(dat) +
    geom_line(data = subset(dat, Specimen_id == 0),
        aes(Distance,
            SrCa * 1000,
            col = Specimen),
        col = brewer.pal(5, "Dark2")[1]) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)") +
    scale_x_continuous("Distance from ventral margin [mm]") +
    coord_cartesian(xlim = c(0, 8)) +
    theme_bw()

Profile_plot_S_G003 <- ggplot(dat) +
    geom_line(data = subset(dat, Specimen_id == 0),
        aes(Distance,
            SCa * 1000,
            col = Specimen),
        col = brewer.pal(5, "Dark2")[2]) +
    scale_y_continuous("[S]/[Ca]\n(mmol/mol)") +
    scale_x_continuous("Distance from ventral margin [mm]") +
    coord_cartesian(xlim = c(0, 8)) +
    theme_bw()

Profile_plot_Mn_G003 <- ggplot(dat) +
    geom_line(data = subset(dat, Specimen_id == 0),
        aes(Distance,
            MnCa * 1E6,
            col = Specimen),
        col = brewer.pal(5, "Dark2")[4]) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)") +
    scale_x_continuous("Distance from ventral margin [mm]") +
    coord_cartesian(xlim = c(0, 8)) +
    theme_bw()

Combined_profiles_G003 <- ggarrange(Profile_plot_Sr_G003,
    Profile_plot_S_G003,
    Profile_plot_Mn_G003,
    ncol = 1)

# G511
Profile_plot_Sr_G511 <- ggplot(dat) +
    geom_line(data = subset(dat, Specimen_id == 1),
        aes(Distance,
            SrCa * 1000,
            col = Specimen),
        col = brewer.pal(5, "Dark2")[1]) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)") +
    scale_x_continuous("Distance from ventral margin [mm]") +
    coord_cartesian(xlim = c(0, 8)) +
    theme_bw()

Profile_plot_S_G511 <- ggplot(dat) +
    geom_line(data = subset(dat, Specimen_id == 1),
        aes(Distance,
            SCa * 1000,
            col = Specimen),
        col = brewer.pal(5, "Dark2")[2]) +
    scale_y_continuous("[S]/[Ca]\n(mmol/mol)") +
    scale_x_continuous("Distance from ventral margin [mm]") +
    coord_cartesian(xlim = c(0, 8)) +
    theme_bw()

Profile_plot_Mn_G511 <- ggplot(dat) +
    geom_line(data = subset(dat, Specimen_id == 1),
        aes(Distance,
            MnCa * 1E6,
            col = Specimen),
        col = brewer.pal(5, "Dark2")[4]) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)") +
    scale_x_continuous("Distance from ventral margin [mm]") +
    coord_cartesian(xlim = c(0, 8)) +
    theme_bw()

Combined_profiles_G511 <- ggarrange(Profile_plot_Sr_G511,
    Profile_plot_S_G511,
    Profile_plot_Mn_G511,
    ncol = 1)

# G600
Profile_plot_Sr_G600 <- ggplot(dat) +
    geom_line(data = subset(dat, Specimen_id == 2),
        aes(Distance,
            SrCa * 1000,
            col = Specimen),
        col = brewer.pal(5, "Dark2")[1]) +
    scale_y_continuous("[Sr]/[Ca]\n(mmol/mol)") +
    scale_x_continuous("Distance from ventral margin [mm]") +
    coord_cartesian(xlim = c(0, 8)) +
    theme_bw()

Profile_plot_S_G600 <- ggplot(dat) +
    geom_line(data = subset(dat, Specimen_id == 2),
        aes(Distance,
            SCa * 1000,
            col = Specimen),
        col = brewer.pal(5, "Dark2")[2]) +
    scale_y_continuous("[S]/[Ca]\n(mmol/mol)") +
    scale_x_continuous("Distance from ventral margin [mm]") +
    coord_cartesian(xlim = c(0, 8)) +
    theme_bw()

Profile_plot_Mn_G600 <- ggplot(dat) +
    geom_line(data = subset(dat, Specimen_id == 2),
        aes(Distance,
            MnCa * 1E6,
            col = Specimen),
        col = brewer.pal(5, "Dark2")[4]) +
    scale_y_continuous("[Mn]/[Ca]\n(umol/mol)") +
    scale_x_continuous("Distance from ventral margin [mm]") +
    coord_cartesian(xlim = c(0, 8)) +
    theme_bw()

Combined_profiles_G600 <- ggarrange(Profile_plot_Sr_G600,
    Profile_plot_S_G600,
    Profile_plot_Mn_G600,
    ncol = 1)
