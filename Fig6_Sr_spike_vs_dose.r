# Digitize shell height in cross section

require(tidyverse)
require(readxl)
require(ggrepel)
require(ggpubr)
require(lubridate)

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

# Open quantified shell widths at positions of spikes
Thickness_mm <- read.csv("Extract scan positions/Scan_shell_thickness_mm.csv")

# Pivot table to separate out individual spikes
Thickness_long <- pivot_longer(Thickness_mm,
    2:6,
    names_to = "Spike",
    values_to = "Thickness_mm"
)
Thickness_long$Thickness_um <- Thickness_long$Thickness_mm * 1000 # Convert to um

Resultmat$Shell_Thickness <- Thickness_long$Thickness_mm # Add thickness values to result matrix
Resultmat$Segment_Volume <- Resultmat$Shell_Thickness * Resultmat$dL * pi * Resultmat$SL_peak # Calculate the proxy volume of the shell section containing the Sr peak [um^3]
rho_CaCO3 <- 2.93 # Density of CaCO3 (calcite = 2.91, aragonite = 2.93 - 2.95)
Resultmat$Segment_Mass <- (Resultmat$Segment_Volume * 10 ^ -12) * rho_CaCO3 # Mass of shell section containing Sr peak [mg]
M_CaCO3 <- 100.09 # Molar mass of CaCO3
Resultmat$Segment_mol <- Resultmat$Segment_Mass / M_CaCO3 # Molar amount of CaCO3 in shell segment containing Sr peak [mol]

Resultmat$Na_tot <- (Resultmat$Na_mean - Resultmat$Na_bkg) * Resultmat$Segment_mol # Amount [mol] of Na taken up during the spike calculated from the excess Na relative to the background value
Resultmat$Mg_tot <- (Resultmat$Mg_mean - Resultmat$Mg_bkg) * Resultmat$Segment_mol # Amount [mol] of Mg taken up during the spike calculated from the excess Mg relative to the background value
Resultmat$Mn_tot <- (Resultmat$Mn_mean - Resultmat$Mn_bkg) * Resultmat$Segment_mol # Amount [mol] of Mn taken up during the spike calculated from the excess Mn relative to the background value
Resultmat$Sr_tot <- (Resultmat$Sr_mean - Resultmat$Sr_bkg) * Resultmat$Segment_mol # Amount [mol] of Sr taken up during the spike calculated from the excess Sr relative to the background value
Resultmat$Ba_tot <- (Resultmat$Ba_mean - Resultmat$Ba_bkg) * Resultmat$Segment_mol # Amount [mol] of Ba taken up during the spike calculated from the excess Ba relative to the background value

# Add height of the Sr dose (in mol/L in water)
Sr_Dose <- read.csv("Extract scan positions/Sr_dose.csv")

# Pivot table to separate out individual spikes
Dose_long <- pivot_longer(Sr_Dose,
    2:6,
    names_to = "Spike",
    values_to = "Sr_dose"
)
Resultmat$Dose <- Dose_long$Sr_dose # Add dose values to result matrix

# Add timing of the Sr dose (date)
Spike_timing <- read.csv("Extract scan positions/spike_timing.csv")

# Pivot table to separate out individual spikes
Timing_long <- pivot_longer(Spike_timing,
    2:6,
    names_to = "Spike",
    values_to = "date"
)
Resultmat$Timing <- Timing_long$date # Add timing values to result matrix

# Add Speciesnames
Resultmat$Species <- as.character(lapply(Resultmat$Specimen, function(x) LA_combined$Species[match(x, LA_combined$Specimen)]))

# Export stats of peaks
write.csv(Resultmat, "Extract scan positions/peak_stats.csv")

# Trim Results to remove NA rows
Resultmat_clean <- Resultmat[!is.na(Resultmat$Mg_peak), ]
# Add "jittered" horizontal axis for plotting
Resultmat_clean$Plot_X <- match(Resultmat_clean$Specimen, unique(Resultmat_clean$Specimen)) + as.numeric(Resultmat_clean$Spike_nr) / 10

# Group and summarize spike peak data
Spike_peak_groups <- Resultmat_clean %>%
    group_by(Dose, Species) %>%
    summarize(
        Na_relpeak_mean = mean(Na_peak - Na_bkg, na.rm = TRUE),
        Mg_relpeak_mean = mean(Mg_peak - Mg_bkg, na.rm = TRUE),
        Mn_relpeak_mean = mean(Mn_peak - Mn_bkg, na.rm = TRUE),
        Sr_relpeak_mean = mean(Sr_peak - Sr_bkg, na.rm = TRUE),
        Ba_relpeak_mean = mean(Ba_peak - Ba_bkg, na.rm = TRUE),
        Na_relpeak_sd = sd(Na_peak - Na_bkg, na.rm = TRUE),
        Mg_relpeak_sd = sd(Mg_peak - Mg_bkg, na.rm = TRUE),
        Mn_relpeak_sd = sd(Mn_peak - Mn_bkg, na.rm = TRUE),
        Sr_relpeak_sd = sd(Sr_peak - Sr_bkg, na.rm = TRUE),
        Ba_relpeak_sd = sd(Ba_peak - Ba_bkg, na.rm = TRUE)
    )
    
# Plot peak height relative to dose
pd = position_dodge(width = 5E-5) # Fix dodging to draw line through same points

Dose_height_Sr <- ggplot(data = Resultmat_clean) +
    geom_jitter(aes(x = Dose,
            y = Sr_peak - Sr_bkg,
            color = Species),
        width = 3E-5,
        alpha = 0.5) +
    geom_errorbar(data = Spike_peak_groups,
        aes(x = Dose,
            y = Sr_relpeak_mean,
            ymin = Sr_relpeak_mean - Sr_relpeak_sd,
            ymax = Sr_relpeak_mean + Sr_relpeak_sd,
            color = Species),
        width = 5E-5,
        size = 1,
        position = pd) +
    geom_line(data = Spike_peak_groups,
        aes(x = Dose,
            y = Sr_relpeak_mean,
            color = Species),
        size = 1,
        position = pd) +
    geom_hline(yintercept = 0,
        linetype = "dashed") +
    geom_vline(xintercept = 0.5E-4,
        linetype = "dotted",
        size = 1) +
    geom_text(aes(label = "[Sr]sw",
            x = 0.5E-4,
            y = 0.0025)) +
    scale_x_continuous("Dose of Sr spike in water [mmol/L]",
        breaks = seq(0, 0.0006, 0.0001),
        labels = seq(0, .6, 0.1)) +
    scale_y_continuous("Sr/Ca peak relative to\nbackground [mmol/mol]",
        breaks = seq(-0.5E-3, 3E-3, 5E-4),
        labels = seq(-0.5, 3, 0.5)) +
    coord_cartesian(xlim = c(0, 0.0006),
        ylim = c(-0.5E-3, 3E-3)) +
    theme_bw()