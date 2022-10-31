# Script to create plots comparing shell height and growth rate calculations between methods.
require(tidyverse)
require(ggpubr)
require(lubridate)
require(RColorBrewer)

# Load result matrix
Resultmat <- read.csv("Extract scan positions/peak_stats.csv", header = TRUE)

Resultmat$Date <- as_date(Resultmat$Timing, format = "%e/%m/%Y") # Convert timing to date format

# Trim Results to remove NA rows
Resultmat_clean <- Resultmat[!is.na(Resultmat$Mg_peak), ]
Resultmat_clean$Specimen2 <- substr(Resultmat_clean$Specimen, 1, 4) # merge data from parallel lines in the same specimen
Resultmat_clean[, grep("SL", colnames(Resultmat_clean))] <- Resultmat_clean[, grep("SL", colnames(Resultmat_clean))] / 1000 # Convert shell height measurements to mm
Resultmat_clean$SL_end[which(is.na(Resultmat_clean$SL_end))] <- Resultmat_clean$SL_peak[which(is.na(Resultmat_clean$SL_end))] # For missing ends of spikes set SL_end equal to the peak of the spike

Shell_measurements <- read.csv("Shell_measurements2.csv", header = TRUE) # Load shell height measurements
Shell_measurements$Name <- paste("G", Shell_measurements$Name, sep = "") # Add G for correct specimen code
Shell_measurements$Name[which(Shell_measurements$Name == "G3")] <- "G003"
Shell_measurements$Specimen <- Shell_measurements$Name
Shell_measurements$Date <- as_date(Shell_measurements$Date, format = "%e/%m/%Y") # Convert timing to date format

# Isolate height vs time data
Srspike_heights <- select(Resultmat_clean, "Date", "Specimen2", "SL_mid", "SL_start", "SL_end", "Species")

# Interpolate caliper heights beloning to dates of Sr spiking
Srspike_heights$measured_height <- NA

for(i in 1:nrow(Srspike_heights)){
    date <- Srspike_heights$Date[i]
    spec <- Srspike_heights$Specimen2[i]
    measure_dates <- Shell_measurements$Date[which(Shell_measurements$Specimen == spec)]
    measure_heights <- Shell_measurements$height[which(Shell_measurements$Specimen == spec)]
    Srspike_heights$measured_height[i] <- approx(measure_dates, measure_heights, xout = date)$y
}

# Function to extract linear regression equation (source: https://groups.google.com/g/ggplot2/c/1TgH-kG5XMA)
lm_eqn <- function(df, x, y){
    m <- lm(y ~ x, df);
    eq <- substitute(italic(y) == a + b~"*"~italic(x)*","~~italic(r)^2~"="~r2, 
         list(a = format(unname(coef(m)[1]), digits = 2),
              b = format(unname(coef(m)[2]), digits = 2),
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
}

heights_Cedule <- lm_eqn(subset(Srspike_heights, Species == "Cedule"),
    x = Srspike_heights$measured_height[which(Srspike_heights$Species == "Cedule")],
    y = Srspike_heights$SL_mid[which(Srspike_heights$Species == "Cedule")])
heights_Medulis <- lm_eqn(subset(Srspike_heights, Species == "Medulis"),
    x = Srspike_heights$measured_height[which(Srspike_heights$Species == "Medulis")],
    y = Srspike_heights$SL_mid[which(Srspike_heights$Species == "Medulis")])
heights_Oedulis <- lm_eqn(subset(Srspike_heights, Species == "Oedulis"),
    x = Srspike_heights$measured_height[which(Srspike_heights$Species == "Oedulis")],
    y = Srspike_heights$SL_mid[which(Srspike_heights$Species == "Oedulis")])

# Create crossplot of Sr spike vs measured height
heights_cross <- ggplot(Srspike_heights) +
    geom_smooth(aes(x = measured_height,
            y = SL_mid,
            color = Species,
            fill = Species
        ),
        alpha = 0.1,
        method = "lm",
        se = TRUE,
        fullrange = TRUE) +
    # Add linear functions
    geom_text(aes(color = "Cedule"),
        x = 30,
        y = mean(Srspike_heights$SL_mid[which(Srspike_heights$Species == "Cedule")]),
        label = heights_Cedule,
        parse = TRUE) +
    geom_text(aes(color = "Medulis"),
        x = 40,
        y = mean(Srspike_heights$SL_mid[which(Srspike_heights$Species == "Medulis")]),
        label = heights_Medulis,
        parse = TRUE) +
    geom_text(aes(color = "Oedulis"),
        x = 25,
        y = mean(Srspike_heights$SL_mid[which(Srspike_heights$Species == "Oedulis")]) + 5,
        label = heights_Oedulis,
        parse = TRUE) +
    geom_pointrange(aes(x = measured_height,
            y = SL_mid,
            ymin = SL_start,
            ymax = SL_end,
            color = Species
        )
    ) +
    geom_abline(slope = 1,
        intercept = 0,
        linetype = "dashed",
        size = 1.5) +
    scale_x_continuous("Measured shell height [mm]") +
    scale_y_continuous("Shell height inferred from Sr spikes [mm]") +
    ggtitle("Measured shell height vs\nShell height based on Sr spiking") +
    theme_bw() +
    theme(legend.position = "none")

# ------------------------------------------------------------------------------
# Calculate monthly growth rates using both methods
# Create matrix of days, species and methods
growthrates <- data.frame(days = rep(seq(ymd(200201), ymd(200930), 1), 3))
growthrates$Species <- rep(c("Cedule", "Medulis", "Oedulis"), each = length(growthrates$days) / 3)

# Rephrase Species column in Shell_measurements to align with Srspike_heights
Shell_measurements$Species[which(Shell_measurements$Species == "C.edule")] <- "Cedule"
Shell_measurements$Species[which(Shell_measurements$Species == "M.edulis")] <- "Medulis"
Shell_measurements$Species[which(Shell_measurements$Species == "O.edulis")] <- "Oedulis"

# Add columns for both methods
growthrates$Sr_spike <- NA
growthrates$Sr_spike_SD <- NA
growthrates$measured <- NA
growthrates$measured_SD <- NA

# Calculate growth rate for each day
for(i in 1:nrow(growthrates)){
    date <- growthrates$days[i]
    spec <- growthrates$Species[i]
    GR_Sr <- vector()
    GR_meas <- vector()
    # Loop through specimens
    for(specimen in unique(Shell_measurements$Specimen[which(Shell_measurements$Species == spec)])){
        # Find date and height vectors for inter- and extrapolation
        Sr_dates <- Srspike_heights$Date[which(Srspike_heights$Specimen2 == specimen)]
        Sr_heights <- Srspike_heights$SL_mid[which(Srspike_heights$Specimen2 == specimen)]
        measure_dates <- Shell_measurements$Date[which(Shell_measurements$Specimen == specimen)]
        measure_heights <- Shell_measurements$height[which(Shell_measurements$Specimen == specimen)]
        # Find growth rate belonging to date for Sr spike data
        if(date < min(Sr_dates)){
            GR_Sr <- append(GR_Sr, diff(Sr_heights[1:2]) / as.numeric(diff(Sr_dates[1:2])))
        }else if(date > max(Sr_dates)){
            GR_Sr <- append(GR_Sr, diff(tail(Sr_heights, 2)) / as.numeric(diff(tail(Sr_dates, 2))))
        }else{
            # Find values before and after and calculate growth rate
            GR_Sr <- append(GR_Sr, (Sr_heights[min(which(Sr_dates >= date))] - Sr_heights[max(which(Sr_dates <= date))]) /
                as.numeric(Sr_dates[min(which(Sr_dates >= date))] - Sr_dates[max(which(Sr_dates <= date))]))
        }

        # Repeat for measured data
        if(date < min(measure_dates)){
            GR_meas <- append(GR_meas, diff(measure_heights[1:2]) / as.numeric(diff(measure_dates[1:2])))
        }else if(date > max(measure_dates)){
            GR_meas <- append(GR_meas, diff(tail(measure_heights, 2)) / as.numeric(diff(tail(measure_dates, 2))))
        }else{
            # Find values before and after and calculate growth rate
            GR_meas <- append(GR_meas, (measure_heights[min(which(measure_dates >= date))] - measure_heights[max(which(measure_dates <= date))]) /
                as.numeric(measure_dates[min(which(measure_dates >= date))] - measure_dates[max(which(measure_dates <= date))]))
        }
    }
    growthrates$Sr_spike[i] <- mean(GR_Sr)
    growthrates$Sr_spike_SD[i] <- sd(GR_Sr)
    growthrates$measured[i] <- mean(GR_meas)
    growthrates$measured_SD[i] <- sd(GR_meas)
}

# Calculate monthly growth rates
growthrates$month <- month(growthrates$days)
growthrates$monthname <- month.abb[growthrates$month]
monthly_GR <- growthrates |>
    group_by(Species, month) |>
    summarize(
        monthname = first(monthname),
        N = n(),
        Sr_spike_mean = mean(Sr_spike, na.rm = TRUE),
        Sr_spike_SE = sqrt(sum(Sr_spike_SD ^ 2, na.rm = TRUE) / N),
        measured_mean = mean(measured, na.rm = TRUE),
        measured_SE = sqrt(sum(measured_SD ^ 2, na.rm = TRUE) / N)
    )

# Plot monthly growth rates per species and method
GRplot_Srspike <- ggplot(monthly_GR) +
    geom_col(aes(x = month,
            y = Sr_spike_mean,
            fill = Species),
        alpha = 0.5,
        color = "black",
        position = "dodge") +
    geom_errorbar(aes(x = month,
            ymin = Sr_spike_mean - Sr_spike_SE,
            ymax = Sr_spike_mean + Sr_spike_SE,
            color = Species),
        position = position_dodge(width = 0.9),
        width = 0.5,
        size = 1.2) +
    scale_x_continuous("Month",
        breaks = unique(monthly_GR$month),
        labels = unique(monthly_GR$monthname)) +
    scale_y_continuous("Growth rate according to Sr spiking\n[mm/d]") +
    theme_bw()

GRplot_measured <- ggplot(monthly_GR) +
    geom_col(aes(x = month,
            y = measured_mean,
            fill = Species),
        alpha = 0.5,
        color = "black",
        position = "dodge") +
    geom_errorbar(aes(x = month,
            ymin = measured_mean - measured_SE,
            ymax = measured_mean + measured_SE,
            color = Species),
        position = position_dodge(width = 0.9),
        width = 0.5,
        size = 1.2) +
    scale_x_continuous("Month",
        breaks = unique(monthly_GR$month),
        labels = unique(monthly_GR$monthname)) +
    scale_y_continuous("Growth rate according to measurements\n[mm/d]") +
    theme_bw()

GRplot_cross <- ggplot(monthly_GR) +
    geom_smooth(aes(x = measured_mean,
            y = Sr_spike_mean,
            color = Species,
            fill = Species
        ),
        alpha = 0.1,
        method = "lm",
        se = TRUE,
        fullrange = TRUE) +
    geom_pointrange(aes(x = measured_mean,
            y = Sr_spike_mean,
            ymin = Sr_spike_mean - Sr_spike_SE,
            ymax = Sr_spike_mean + Sr_spike_SE,
            color = Species
        )
    ) +
    geom_errorbarh(aes(y = Sr_spike_mean,
            xmin = measured_mean - measured_SE,
            xmax = measured_mean + measured_SE,
            color = Species
        )
    ) +
    geom_abline(slope = 1,
        intercept = 0,
        linetype = "dashed",
        size = 1.5) +
    scale_x_continuous("Growth rate according to measurements\n[mm/d]",
        limits = c(0, 0.4)) +
    scale_y_continuous("Growth rate according to Sr spiking\n[mm/d]",
        limits = c(0, 0.4)) +
    ggtitle("Measured growth rate vs\nGrowth rate based on Sr spiking") +
    theme_bw() +
    theme(legend.position = "none")

# Create histograms of height measurements for both methods
Srspike_heights$month <- month(Srspike_heights$Date)
Srspike_heights$monthname <- month.abb[Srspike_heights$month]
Shell_measurements$month <- month(Shell_measurements$Date)
Shell_measurements$monthname <- month.abb[Shell_measurements$month]

Hist_Srspike <- ggplot(Srspike_heights) +
    geom_bar(aes(x = month,
            fill = Species),
        position = "dodge") +
    scale_x_continuous("Month",
        breaks = unique(monthly_GR$month),
        labels = unique(monthly_GR$monthname),
        limits = c(2, 9.5)) +
    scale_y_continuous("Number of\nSr spikes") +
    theme_bw()

Hist_measured <- ggplot(Shell_measurements) +
    geom_bar(aes(x = month,
            fill = Species),
        position = "dodge") +
    scale_x_continuous("Month",
        breaks = unique(monthly_GR$month),
        labels = unique(monthly_GR$monthname),
        limits = c(2, 9.5)) +
    scale_y_continuous("Number of\nmeasurements") +
    theme_bw()
    
Combined_GR_heights <- ggarrange(
    Hist_measured,
    Hist_Srspike,
    GRplot_measured,
    GRplot_Srspike,
    GRplot_cross,
    heights_cross,
    ncol = 2,
    nrow = 3,
    heights = c(0.5, 1, 1),
    common.legend = TRUE,
    labels = "AUTO"
)