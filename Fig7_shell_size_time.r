# ------------------------------------------------------------------------------
# Plot shell size vs time

require(tidyverse)
require(readxl)
require(ggrepel)
require(ggpubr)
require(lubridate)
require(RColorBrewer)

# Load result matrix
Resultmat <- read.csv("Extract scan positions/peak_stats.csv", header = TRUE)

Resultmat$Date <- as_date(Resultmat$Timing, format = "%e/%m/%Y") # Convert timing to date format

# Trim Results to remove NA rows
Resultmat_clean <- Resultmat[!is.na(Resultmat$Mg_peak), ]
Resultmat_clean$Specimen2 <- substr(Resultmat_clean$Specimen, 1, 4) # merge data from parallel lines in the same specimen
Resultmat_clean[, grep("SL", colnames(Resultmat_clean))] <- Resultmat_clean[, grep("SL", colnames(Resultmat_clean))] / 1000 # Convert shell size measurements to mm
Resultmat_clean$SL_end[which(is.na(Resultmat_clean$SL_end))] <- Resultmat_clean$SL_peak[which(is.na(Resultmat_clean$SL_end))] # For missing ends of spikes set SL_end equal to the peak of the spike

Shell_measurements <- read.csv("Shell_measurements2.csv", header = TRUE) # Load shell size measurements
Shell_measurements$Name <- paste("G", Shell_measurements$Name, sep = "") # Add G for correct specimen code
Shell_measurements$Name[which(Shell_measurements$Name == "G3")] <- "G003"
Shell_measurements$Specimen <- Shell_measurements$Name
Shell_measurements$Date <- as_date(Shell_measurements$Date, format = "%e/%m/%Y") # Convert timing to date format

# Create individual growth plots

# Build custom color scale
colorscale <- unlist(brewer.pal(2, "Set1"))
names(colorscale) <- c("Sr_spike", "Caliper Measurement")

# C. edule - Batch1

Shell_size_plot_G003 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G003"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G003"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G003"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        limits = c(10, 25)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("C. edule G003") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

Shell_size_plot_G511 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G511"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G511"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G511"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        limits = c(10, 25)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("C. edule G511") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

Shell_size_plot_G600 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G600"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G600"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G600"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        limits = c(10, 25)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("C. edule G600") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

# C. edule - Batch2

Shell_size_plot_G457 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G457"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G457"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G457"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        limits = c(10, 25)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("C. edule G457") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

Shell_size_plot_G472 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G472"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G472"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G472"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        limits = c(10, 25)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("C. edule G472") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

Shell_size_plot_G555 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G555"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G555"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G555"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        limits = c(10, 25)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("C. edule G555") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

# M. edulis

Shell_size_plot_G177 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G177"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G177"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G177"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        limits = c(15, 35)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("M. edulis G177") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

Shell_size_plot_G191 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G191"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G191"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G191"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        limits = c(15, 35)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("M. edulis G191") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

Shell_size_plot_G259 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G259"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G259"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G259"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        limits = c(15, 35)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("M. edulis G259") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

# O. edulis

Shell_size_plot_G271 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G271"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G271"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G271"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        breaks = seq(20, 55, 5),
        limits = c(20, 55)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("O. edulis G271") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

Shell_size_plot_G282 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G282"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G282"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G282"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        breaks = seq(20, 55, 5),
        limits = c(20, 55)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("O. edulis G282") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

Shell_size_plot_G372 <- ggplot(data = subset(Resultmat_clean, subset = Specimen2 == "G372"),
        aes(x = Date,
            y = SL_mid)) +
    geom_pointrange(aes(y = SL_peak,
            ymin = SL_start,
            ymax = SL_end,
            color = "Sr_spike")) +
    geom_line(aes(color = "Sr spike")) +
    geom_line(data = subset(Shell_measurements, subset = Specimen == "G372"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    geom_point(data = subset(Shell_measurements, subset = Specimen == "G372"),
        aes(x = Date,
            y = height,
            color = "Caliper Measurement")) +
    scale_y_continuous("Shell size [mm]",
        breaks = seq(20, 55, 5),
        limits = c(20, 55)) +
    scale_x_date("Date",
        limits = c(as_date("20/02/2020", format = "%e/%m/%Y"), as_date("01/10/2020",format = "%e/%m/%Y")),
        date_breaks = "1 month",
        date_labels = "%b") +
    scale_colour_manual(values = colorscale) +
    ggtitle("O. edulis G372") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

Combined_shell_size_plots <- ggarrange(
    # C. edule
    Shell_size_plot_G003,
    Shell_size_plot_G511,
    Shell_size_plot_G600,
    Shell_size_plot_G457,
    Shell_size_plot_G472,
    Shell_size_plot_G555,
    # M. edulis
    Shell_size_plot_G177,
    Shell_size_plot_G191,
    Shell_size_plot_G259,
    # O. edulis
    Shell_size_plot_G271,
    Shell_size_plot_G282,
    Shell_size_plot_G372,
    ncol = 6,
    nrow = 2,
    common.legend = TRUE
)