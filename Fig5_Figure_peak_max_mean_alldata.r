# Plot overview of peak heights for all specimens and all elements

require(tidyverse)
require(readxl)
require(ggrepel)
require(ggpubr)
require(lubridate)

# Load LAICPMS data

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
    df$Specimen <- paste(strsplit(i, "_")[[1]][2], strsplit(i, "_")[[1]][3], sep = "_")
    df$Species <- strsplit(i, "_")[[1]][1]
    df$Distance <- df$Depth * 1000 # Due to lack of X and Y export, Distance for batch1 is set equal to the "Depth" value calculated from the scan time (in micrometer)
    assign(i, df)
}

# Load data - Batch 2

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
LA_combined <- bind_rows(Cedule_G003_1,
    Cedule_G003_2,
    Cedule_G003_3,
    Cedule_G511_1,
    Cedule_G511_2,
    Cedule_G511_3,
    Cedule_G600_1,
    Cedule_G600_2,
    Cedule_G600_3,
    Medulis_G177,
    Medulis_G191,
    Medulis_G259,
    Oedulis_G271,
    Oedulis_G282,
    Oedulis_G372,
    Cedule_G457,
    Cedule_G472,
    Cedule_G555,
    .id = "Specimen_id"
)

# ------------------------------------------------------------------------------
# Load lengths of scans in pts and mm for conversion to mm scale
Scandist <- read.csv("Extract scan positions/Scan_dist_conversion.csv")

# Load coordinates of scan lines
Batch1_pos <- read.csv("Extract scan positions/Batch1_coordinates.csv")
Batch1_pos$Specimen <- paste("G", Batch1_pos$Specimen, sep = "")
Batch1_pos$X <- Batch1_pos$X * 1000 # Convert to um
Batch1_pos$Y <- Batch1_pos$Y * 1000 # Convert to um

Batch1_pos$L <- 0 # Add length column in mm
for(i in 2:length(Batch1_pos$X)){ # Loop through XY data and tally up cumulative length of the linescan
    if(Batch1_pos$Specimen[i - 1] == Batch1_pos$Specimen[i]){
        Batch1_pos$L[i] <- Batch1_pos$L[i - 1] + sqrt((Batch1_pos$X[i - 1] - Batch1_pos$X[i]) ^ 2 + (Batch1_pos$Y[i - 1] - Batch1_pos$Y[i]) ^ 2)
    }
}

# Reverse Batch1 length!!! (measured in opposite direction)
for(specimen in unique(Batch1_pos$Specimen)){
    Batch1_pos$L[which(Batch1_pos$Specimen == specimen)] <- max(Batch1_pos$L[which(Batch1_pos$Specimen == specimen)]) - Batch1_pos$L[which(Batch1_pos$Specimen == specimen)]
}

Batch2_pos <- read.csv("Extract scan positions/Batch2_coordinates.csv")
Batch2_pos$L <- 0 # Add length column in mm
for(i in 2:length(Batch2_pos$X)){ # Loop through XY data and tally up cumulative length of the linescan
    if(Batch2_pos$Specimen[i - 1] == Batch2_pos$Specimen[i]){
        Batch2_pos$L[i] <- Batch2_pos$L[i - 1] + sqrt((Batch2_pos$X[i - 1] - Batch2_pos$X[i]) ^ 2 + (Batch2_pos$Y[i - 1] - Batch2_pos$Y[i]) ^ 2)
    }
}

combined_pos <- bind_rows(Batch1_pos, Batch2_pos[which(!grepl("chalky", Batch2_pos$Specimen)), ]) # Combine positions, ignoring the chalky lines in oysters

# ------------------------------------------------------------------------------
# Load coordinates of reference points at umbos of shells
Refpts <- read.csv("Extract scan positions/Refpoints.csv")
# Refpts[Refpts$Specimen %in% Batch2_pos$Specimen, c(2, 3)] <- Refpts[Refpts$Specimen %in% Batch2_pos$Specimen, c(2, 3)] / 1000 # Convert to mm

# Load positions of starts, ends and maxima of Sr spikes relative to spike length
Spike_pos <- read.csv("Extract scan positions/Scan_spike_positions_mm.csv")

# Pivot table to separate out individual spikes
Spike_pos_long <- pivot_longer(Spike_pos,
    2:16,
    names_to = "Spike",
    values_to = "L_mm"
)
# Split out spike positions
Spike_pos_long <- separate(Spike_pos_long, Spike, into = c("position", "Spike_nr"), sep = "_")
Spike_pos_long$L_um <- Spike_pos_long$L_mm * 1000 # Convert to micrometer

# ------------------------------------------------------------------------------
# Add column to indicate which values fall within peaks
LA_combined$peak <- FALSE
LA_combined$peakID <- NA

# Loop through Sr spikes in all specimens and return statistics on shell length and Sr spikes and locations of Sr peaks
Resultmat <- unique(Spike_pos_long[, c(1, 3)]) # Aggregate all peaks
Resultmat$Na_peak <- NA # Na/Ca ratio at peak
Resultmat$Na_mean <- NA # Mean Na/Ca ratio within peak
Resultmat$Na_bkg <- NA # Na/Ca ratio background

Resultmat$Mg_peak <- NA # Mg/Ca ratio at peak
Resultmat$Mg_mean <- NA # Mean Mg/Ca ratio within peak
Resultmat$Mg_bkg <- NA # Mg/Ca ratio background

Resultmat$Mn_peak <- NA # Mn/Ca ratio at peak
Resultmat$Mn_mean <- NA # Mean Mn/Ca ratio within peak
Resultmat$Mn_bkg <- NA # Mn/Ca ratio background

Resultmat$Sr_peak <- NA # Sr/Ca ratio at peak
Resultmat$Sr_mean <- NA # Mean Sr/Ca ratio within peak
Resultmat$Sr_bkg <- NA # Sr/Ca ratio background

Resultmat$Ba_peak <- NA # Ba/Ca ratio at peak
Resultmat$Ba_mean <- NA # Mean Ba/Ca ratio within peak
Resultmat$Ba_bkg <- NA # Ba/Ca ratio background

Resultmat$SL_peak <- NA # Shell length relative to ref point at peak
Resultmat$SL_start <- NA # Shell length relative to ref point at start of peak
Resultmat$SL_end <- NA # Shell length relative to ref point at end of peak
Resultmat$SL_mid <- NA # Shell length relative to ref point at handpicked max of peak
Resultmat$dL <- NA # Growth along shell during peak
Resultmat$peaksym <- NA # Position of middle of peak in growth direction (peak symmetry)
Resultmat$X_peak <- NA # X coordinate of the Sr peak
Resultmat$Y_peak <- NA # Y coordinate of the Sr peak

# Keep track of specimen and peak ID
Specimen_id <- 1
peak_id <- 0
current_specimen <- Resultmat$Specimen[1]

for(i in 1:nrow(Resultmat)){ # Loop through all Sr peaks
    L_start <- Spike_pos_long$L_um[which(Spike_pos_long$Specimen == Resultmat$Specimen[i] &
        Spike_pos_long$Spike_nr == Resultmat$Spike_nr[i] &
        Spike_pos_long$position == "start")] # Retrieve start position of spike
    pos_start <- which.min(abs(LA_combined$Distance[which(LA_combined$Specimen == Resultmat$Specimen[i])] - L_start)) + which(LA_combined$Specimen == Resultmat$Specimen[i])[1] - 1 # Find start position in LA_combined
    # Interpolate coordinates
    X_start <- approx(x = combined_pos$L[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        y = combined_pos$X[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        xout = L_start)$y
    Y_start <- approx(x = combined_pos$L[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        y = combined_pos$Y[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        xout = L_start)$y
    if(!is.na(L_start)){ # If start of peak is found, calculate shell length at this location
        Resultmat$SL_start[i] <- sqrt((X_start - Refpts$X[which(Refpts$Specimen == Resultmat$Specimen[i])]) ^ 2 + (Y_start - Refpts$Y[which(Refpts$Specimen == Resultmat$Specimen[i])]) ^ 2)
    }

    L_end <- Spike_pos_long$L_um[which(Spike_pos_long$Specimen == Resultmat$Specimen[i] &
        Spike_pos_long$Spike_nr == Resultmat$Spike_nr[i] &
        Spike_pos_long$position == "end")] # Retrieve end position of spike
    pos_end <- which.min(abs(LA_combined$Distance[which(LA_combined$Specimen == Resultmat$Specimen[i])] - L_end)) + which(LA_combined$Specimen == Resultmat$Specimen[i])[1] - 1 # Find end position in LA_combined
    # Interpolate coordinates   
    X_end <- approx(x = combined_pos$L[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        y = combined_pos$X[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        xout = L_end)$y
    Y_end <- approx(x = combined_pos$L[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        y = combined_pos$Y[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        xout = L_end)$y
    if(!is.na(L_end)){ # If end of peak is found, calculate shell length at this location
        Resultmat$SL_end[i] <- sqrt((X_end - Refpts$X[which(Refpts$Specimen == Resultmat$Specimen[i])]) ^ 2 + (Y_end - Refpts$Y[which(Refpts$Specimen == Resultmat$Specimen[i])]) ^ 2)
    }

    L_mid <- Spike_pos_long$L_um[which(Spike_pos_long$Specimen == Resultmat$Specimen[i] &
        Spike_pos_long$Spike_nr == Resultmat$Spike_nr[i] &
        Spike_pos_long$position == "mid")] # Retrieve mid position of spike
    pos_mid <- which.min(abs(LA_combined$Distance[which(LA_combined$Specimen == Resultmat$Specimen[i])] - L_mid)) + which(LA_combined$Specimen == Resultmat$Specimen[i])[1] - 1 # Find mid position in LA_combined
    # Interpolate coordinates  
    X_mid <- approx(x = combined_pos$L[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        y = combined_pos$X[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        xout = L_mid)$y
    Y_mid <- approx(x = combined_pos$L[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        y = combined_pos$Y[which(combined_pos$Specimen == Resultmat$Specimen[i])],
        xout = L_mid)$y
    if(!is.na(L_mid)){ # If handpicked max of peak is found, calculate shell length at this location
        Resultmat$SL_mid[i] <- sqrt((X_mid - Refpts$X[which(Refpts$Specimen == Resultmat$Specimen[i])]) ^ 2 + (Y_mid - Refpts$Y[which(Refpts$Specimen == Resultmat$Specimen[i])]) ^ 2)
    }

    # Find width of peak in growth direction
    Resultmat$dL[i] <- sqrt((X_end - X_start) ^ 2 + (Y_end - Y_start) ^ 2)

    # Find (a)symmetry of Sr peak as the relative distance of peak middle from mid between start and end of peak
    Resultmat$peaksym[i] <- sqrt((X_mid - X_start) ^ 2 + (Y_mid - Y_start) ^ 2) / sqrt((X_end - X_start) ^ 2 + (Y_end - Y_start) ^ 2)
    
    if(!is.na(L_start) & !is.na(L_end)){ # Check if the peak exists
        # Mark data in LA_combined that fall within peaks
        if(Resultmat$Specimen[i] != current_specimen){ # Check if peak belongs to new specimen
            Specimen_id <- Specimen_id + 1 # Increment specimen number
            peak_id <- 1 # Reset peak ID to 0
            current_specimen <- Resultmat$Specimen[i] # Set new specimen
        }else{ # If next peak in same specimen
            peak_id <- peak_id + 1 # Increment peak ID
        }

        LA_combined$peak[seq(pos_start, pos_end)] <- TRUE
        #LA_combined$peakID[seq(pos_start, pos_end)] <- i
        LA_combined$peakID[seq(pos_start, pos_end)] <- Specimen_id + peak_id / 10

        # Na concentrations
        Resultmat$Na_peak[i] <- mean(sort(LA_combined$NaCa[seq(pos_start, pos_end)], decreasing = TRUE)[1:10], na.rm = TRUE) / 1000 * 0.2 # Find maximum Na concentration [mol/mol] in peak, averaging the highest 10 values to evade wild spikes
        Resultmat$Na_mean[i] <- mean(LA_combined$NaCa[seq(pos_start, pos_end)], na.rm = TRUE) / 1000 * 0.2 # Find mean Na concentration [mol/mol] in peak
        Resultmat$Na_bkg[i] <- mean(LA_combined$NaCa[c((min(pos_start, pos_end) - 25):min(pos_start, pos_end), (max(pos_start, pos_end) + 25):max(pos_start, pos_end))], na.rm = TRUE) / 1000 * 0.2 # Find background Na concentration [mol/mol] by averaging the 50 points before and after the peak
        
        # Mg concentrations
        Resultmat$Mg_peak[i] <- mean(sort(LA_combined$MgCa[seq(pos_start, pos_end)], decreasing = TRUE)[1:10], na.rm = TRUE) / 1000 * 0.2 # Find maximum Mg concentration [mol/mol] in peak, averaging the highest 10 values to evade wild spikes
        Resultmat$Mg_mean[i] <- mean(LA_combined$MgCa[seq(pos_start, pos_end)], na.rm = TRUE) / 1000 * 0.2 # Find mean Mg concentration [mol/mol] in peak
        Resultmat$Mg_bkg[i] <- mean(LA_combined$MgCa[c((min(pos_start, pos_end) - 25):min(pos_start, pos_end), (max(pos_start, pos_end) + 25):max(pos_start, pos_end))], na.rm = TRUE) / 1000 * 0.2 # Find background Mg concentration [mol/mol] by averaging the 50 points before and after the peak

        # Mn concentrations
        Resultmat$Mn_peak[i] <- mean(sort(LA_combined$MnCa[seq(pos_start, pos_end)], decreasing = TRUE)[1:10], na.rm = TRUE) / 1000 * 0.2 # Find maximum Mn concentration [mol/mol] in peak, averaging the highest 10 values to evade wild spikes
        Resultmat$Mn_mean[i] <- mean(LA_combined$MnCa[seq(pos_start, pos_end)], na.rm = TRUE) / 1000 * 0.2 # Find mean Mn concentration [mol/mol] in peak
        Resultmat$Mn_bkg[i] <- mean(LA_combined$MnCa[c((min(pos_start, pos_end) - 25):min(pos_start, pos_end), (max(pos_start, pos_end) + 25):max(pos_start, pos_end))], na.rm = TRUE) / 1000 * 0.2 # Find background Mn concentration [mol/mol] by averaging the 50 points before and after the peak

        # Sr concentrations
        Resultmat$Sr_peak[i] <- mean(sort(LA_combined$SrCa[seq(pos_start, pos_end)], decreasing = TRUE)[1:10], na.rm = TRUE) / 1000 * 0.2 # Find maximum Sr concentration [mol/mol] in peak, averaging the highest 10 values to evade wild spikes
        Resultmat$Sr_mean[i] <- mean(LA_combined$SrCa[seq(pos_start, pos_end)], na.rm = TRUE) / 1000 * 0.2 # Find mean Sr concentration [mol/mol] in peak
        Resultmat$Sr_bkg[i] <- mean(LA_combined$SrCa[c((min(pos_start, pos_end) - 25):min(pos_start, pos_end), (max(pos_start, pos_end) + 25):max(pos_start, pos_end))], na.rm = TRUE) / 1000 * 0.2 # Find background Sr concentration [mol/mol] by averaging the 50 points before and after the peak

        # Ba concentrations
        Resultmat$Ba_peak[i] <- mean(sort(LA_combined$BaCa[seq(pos_start, pos_end)], decreasing = TRUE)[1:10], na.rm = TRUE) / 1000 * 0.2 # Find maximum Ba concentration [mol/mol] in peak, averaging the highest 10 values to evade wild spikes
        Resultmat$Ba_mean[i] <- mean(LA_combined$BaCa[seq(pos_start, pos_end)], na.rm = TRUE) / 1000 * 0.2 # Find mean Ba concentration [mol/mol] in peak
        Resultmat$Ba_bkg[i] <- mean(LA_combined$BaCa[c((min(pos_start, pos_end) - 25):min(pos_start, pos_end), (max(pos_start, pos_end) + 25):max(pos_start, pos_end))], na.rm = TRUE) / 1000 * 0.2 # Find background Ba concentration [mol/mol] by averaging the 50 points before and after the peak

        pos_peak <- which.max(LA_combined$SrCa[min(pos_start, pos_end):max(pos_start, pos_end)]) + min(pos_start, pos_end) # Find position of the highest Sr/Ca value
        L_peak <- LA_combined$Distance[pos_peak] # Find position of peak along scan line
        X_peak <- approx(x = combined_pos$L[which(combined_pos$Specimen == Resultmat$Specimen[i])],
            y = combined_pos$X[which(combined_pos$Specimen == Resultmat$Specimen[i])],
            xout = L_peak)$y
        Resultmat$X_peak[i] <- X_peak # Store peak location in matrix
        Y_peak <- approx(x = combined_pos$L[which(combined_pos$Specimen == Resultmat$Specimen[i])],
            y = combined_pos$Y[which(combined_pos$Specimen == Resultmat$Specimen[i])],
            xout = L_peak)$y
        Resultmat$Y_peak[i] <- Y_peak # Store peak location in matrix

        # Calculate shell length at Sr peak
        Resultmat$SL_peak[i] <- sqrt((X_peak - Refpts$X[which(Refpts$Specimen == Resultmat$Specimen[i])]) ^ 2 + (Y_peak - Refpts$Y[which(Refpts$Specimen == Resultmat$Specimen[i])]) ^ 2)
    }
}

# Add Species names
Resultmat$Species <- as.character(lapply(Resultmat$Specimen, function(x) LA_combined$Species[match(x, LA_combined$Specimen)]))

# Trim Results to remove NA rows
Resultmat_clean <- Resultmat[!is.na(Resultmat$Mg_peak), ]
# Add "jittered" horizontal axis for plotting
Resultmat_clean$Plot_X <- match(Resultmat_clean$Specimen, unique(Resultmat_clean$Specimen)) + as.numeric(Resultmat_clean$Spike_nr) / 10

# ------------------------------------------------------------------------------
# Plot means and high values within the peaks
TEpeaks_plot_Na <- ggplot(Resultmat_clean) +
    geom_point(aes(Plot_X,
            Na_mean - Na_bkg,
            alpha = Na_mean > Na_bkg,
            color = Species)
    ) +
    geom_point(aes(Plot_X,
            Na_peak - Na_bkg,
            alpha = Na_peak > Na_bkg,
            color = Species)
    ) +
    geom_linerange(aes(x = Plot_X,
            ymin = Na_mean - Na_bkg,
            ymax = Na_peak - Na_bkg,
            color = Species,
            alpha = Na_peak > Na_bkg)
    ) +
    geom_text_repel(data = subset(Resultmat_clean, (Na_peak - Na_bkg) > 6E-3), # Add labels for points outside plot area (top)
        aes(x = Plot_X,
            y = rep(6E-3, length(Plot_X)),
            label = round((Na_peak - Na_bkg) * 1000, 1),
            color = Species,
            alpha = Na_peak > Na_bkg)
    ) +
    geom_text_repel(data = subset(Resultmat_clean, (Na_mean - Na_bkg) < -1E-3), # Add labels for points outside plot area (bottom)
        aes(x = Plot_X,
            y = rep(-1E-3, length(Plot_X)),
            label = round((Na_mean - Na_bkg) * 1000, 1),
            color = Species,
            alpha = Na_mean > Na_bkg)
    ) +
    geom_hline(yintercept = 0,
        linetype = "dashed") +
    scale_x_continuous("Specimen",
        breaks = unique(floor(Resultmat_clean$Plot_X)),
        labels = unique(Resultmat_clean$Specimen)) +
    scale_y_continuous("Na/Ca - Na/Ca_bkg\n[mmol/mol]",
        breaks = seq(-1E-3, 6E-3, 1E-3),
        labels = seq(-1, 6, 1)) +
    coord_cartesian(ylim = c(-1E-3, 6E-3)) +
    theme_bw() +
    theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90))

TEpeaks_plot_Mg <- ggplot(Resultmat_clean) +
    geom_point(aes(Plot_X,
            Mg_mean - Mg_bkg,
            alpha = Mg_mean > Mg_bkg,
            color = Species)
    ) +
    geom_point(aes(Plot_X,
            Mg_peak - Mg_bkg,
            alpha = Mg_peak > Mg_bkg,
            color = Species)
    ) +
    geom_linerange(aes(x = Plot_X,
            ymin = Mg_mean - Mg_bkg,
            ymax = Mg_peak - Mg_bkg,
            color = Species,
            alpha = Mg_peak > Mg_bkg)
    ) +
    geom_text_repel(data = subset(Resultmat_clean, (Mg_peak - Mg_bkg) > 4E-3), # Add labels for points outside plot area (top)
        aes(x = Plot_X,
            y = rep(4E-3, length(Plot_X)),
            label = round((Mg_peak - Mg_bkg) * 1000, 1),
            color = Species,
            alpha = Mg_peak > Mg_bkg)
    ) +
    geom_text_repel(data = subset(Resultmat_clean, (Mg_mean - Mg_bkg) < -5E-4), # Add labels for points outside plot area (bottom)
        aes(x = Plot_X,
            y = rep(-5E-4, length(Plot_X)),
            label = round((Mg_mean - Mg_bkg) * 1000, 1),
            color = Species,
            alpha = Mg_mean > Mg_bkg)
    ) +
    geom_hline(yintercept = 0,
        linetype = "dashed") +
    scale_x_continuous("Specimen",
        breaks = unique(floor(Resultmat_clean$Plot_X)),
        labels = unique(Resultmat_clean$Specimen)) +
    scale_y_continuous("Mg/Ca - Mg/Ca_bkg\n[mmol/mol]",
        breaks = seq(-5E-4, 4E-3, 5E-4),
        labels = seq(-0.5, 4, 0.5)) +
    coord_cartesian(ylim = c(-5E-4, 4E-3)) +
    theme_bw() +
    theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90))

TEpeaks_plot_Mn <- ggplot(Resultmat_clean) +
    geom_point(aes(Plot_X,
            Mn_mean - Mn_bkg,
            alpha = Mn_mean > Mn_bkg,
            color = Species)
    ) +
    geom_point(aes(Plot_X,
            Mn_peak - Mn_bkg,
            alpha = Mn_peak > Mn_bkg,
            color = Species)
    ) +
    geom_linerange(aes(x = Plot_X,
            ymin = Mn_mean - Mn_bkg,
            ymax = Mn_peak - Mn_bkg,
            color = Species,
            alpha = Mn_peak > Mn_bkg)
    ) +
    geom_text_repel(data = subset(Resultmat_clean, (Mn_peak - Mn_bkg) > 1E-4), # Add labels for points outside plot area (top)
        aes(x = Plot_X,
            y = rep(1E-4, length(Plot_X)),
            label = round((Mn_peak - Mn_bkg) * 1000, 2),
            color = Species,
            alpha = Mn_peak > Mn_bkg)
    ) +
    geom_text_repel(data = subset(Resultmat_clean, (Mn_mean - Mn_bkg) < -2E-5), # Add labels for points outside plot area (bottom)
        aes(x = Plot_X,
            y = rep(-2E-5, length(Plot_X)),
            label = round((Mn_mean - Mn_bkg) * 1000, 2),
            color = Species,
            alpha = Mn_mean > Mn_bkg)
    ) +
    geom_hline(yintercept = 0,
        linetype = "dashed") +
    scale_x_continuous("Specimen",
        breaks = unique(floor(Resultmat_clean$Plot_X)),
        labels = unique(Resultmat_clean$Specimen)) +
    scale_y_continuous("Mn/Ca - Mn/Ca_bkg\n[umol/mol]",
        breaks = seq(-2E-5, 1E-4, 2E-5),
        labels = seq(-20, 100, 20)) +
    coord_cartesian(ylim = c(-2E-5, 1E-4)) +
    theme_bw() +
    theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90))

TEpeaks_plot_Sr <- ggplot(Resultmat_clean) +
    geom_point(aes(Plot_X,
            Sr_mean - Sr_bkg,
            alpha = Sr_mean > Sr_bkg,
            color = Species)
    ) +
    geom_point(aes(Plot_X,
            Sr_peak - Sr_bkg,
            alpha = Sr_peak > Sr_bkg,
            color = Species)
    ) +
    geom_linerange(aes(x = Plot_X,
            ymin = Sr_mean - Sr_bkg,
            ymax = Sr_peak - Sr_bkg,
            color = Species,
            alpha = Sr_peak > Sr_bkg)
    ) +
    geom_text_repel(data = subset(Resultmat_clean, (Sr_peak - Sr_bkg) > 3E-3), # Add labels for points outside plot area (top)
        aes(x = Plot_X,
            y = rep(3E-3, length(Plot_X)),
            label = round((Sr_peak - Sr_bkg) * 1000, 1),
            color = Species,
            alpha = Sr_peak > Sr_bkg)
    ) +
    geom_text_repel(data = subset(Resultmat_clean, (Sr_mean - Sr_bkg) < -5E-4), # Add labels for points outside plot area (bottom)
        aes(x = Plot_X,
            y = rep(-5E-4, length(Plot_X)),
            label = round((Sr_mean - Sr_bkg) * 1000, 1),
            color = Species,
            alpha = Sr_mean > Sr_bkg)
    ) +
    geom_hline(yintercept = 0,
        linetype = "dashed") +
    scale_x_continuous("Specimen",
        breaks = unique(floor(Resultmat_clean$Plot_X)),
        labels = unique(Resultmat_clean$Specimen)) +
    scale_y_continuous("Sr/Ca - Sr/Ca_bkg\n[mmol/mol]",
        breaks = seq(-5E-4, 3E-3, 5E-4),
        labels = seq(-0.5, 3, 0.5)) +
    coord_cartesian(ylim = c(-5E-4, 3E-3)) +
    theme_bw() +
    theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90))

TEpeaks_plot_Ba <- ggplot(Resultmat_clean) +
    geom_point(aes(Plot_X,
            Ba_mean - Ba_bkg,
            alpha = Ba_mean > Ba_bkg,
            color = Species)
    ) +
    geom_point(aes(Plot_X,
            Ba_peak - Ba_bkg,
            alpha = Ba_peak > Ba_bkg,
            color = Species)
    ) +
    geom_linerange(aes(x = Plot_X,
            ymin = Ba_mean - Ba_bkg,
            ymax = Ba_peak - Ba_bkg,
            alpha = Ba_peak > Ba_bkg,
            color = Species)
    ) +
    geom_text_repel(data = subset(Resultmat_clean, (Ba_peak - Ba_bkg) > 3E-6), # Add labels for points outside plot area (top)
        aes(x = Plot_X,
            y = rep(3E-6, length(Plot_X)),
            label = round((Ba_peak - Ba_bkg) * 10 ^ 6, 0),
            alpha = Ba_peak > Ba_bkg,
            color = Species)
    ) +
    geom_text_repel(data = subset(Resultmat_clean, (Ba_mean - Ba_bkg) < -5E-7), # Add labels for points outside plot area (bottom)
        aes(x = Plot_X,
            y = rep(-5E-7, length(Plot_X)),
            label = round((Ba_mean - Ba_bkg) * 10 ^ 6, 0),
            alpha = Ba_mean > Ba_bkg,
            color = Species)
    ) +
    geom_hline(yintercept = 0,
        linetype = "dashed") +
    scale_x_continuous("Specimen",
        breaks = unique(floor(Resultmat_clean$Plot_X)),
        labels = unique(Resultmat_clean$Specimen)) +
    scale_y_continuous("Ba/Ca - Ba/Ca_bkg\n[umol/mol]",
        breaks = seq(-5E-7, 3E-6, 5E-7),
        labels = seq(-0.5, 3, 0.5)) +
    coord_cartesian(ylim = c(-5E-7, 3E-6)) +
    theme_bw() +
    theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90))

TEpeaks_plot_combined <- ggarrange(TEpeaks_plot_Na + theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()),
    TEpeaks_plot_Mg + theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()),
    TEpeaks_plot_Mn + theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()),
    TEpeaks_plot_Sr + theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()),
    TEpeaks_plot_Ba,
    ncol = 1,
    heights = c(1, 1, 1, 1, 1.4),
    common.legend = TRUE,
    legend="right"
)
