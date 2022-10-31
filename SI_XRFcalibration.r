# Apply XRF calibration on raw data

# Load raw data
raw <- read.csv("XRF_Sr_spiking/Cedule003_XRFraw.csv")
# Load calibration data
cal <- read.csv("XRF_Sr_spiking/202103_XRFcal.csv")

raw[raw == 0] <- NA # Remove zeroes
raw[, -1] <- raw[, -1] * 10 ^ 4 # Set unit to ppm

# Create dataframe to store corrected results
corr <- as.data.frame(matrix(NA, ncol = ncol(raw), nrow = nrow(raw)))
colnames(corr) <- colnames(raw)
corr$Spectrum <- raw$Spectrum

# Create vector of slope and SE of calibration
slopevec <- !(colnames(raw) %in% cal$element) # Check which elements have no calibration
SEvec <- colnames(raw) %in% cal$element # Check which elements have a calibration
slopevec[slopevec] <- 1 # Set slope of elements without calibration to 1
SEvec[!SEvec] <- 0 # Set SE of elements without calibration to 0
for(i in which(slopevec == 0)){ # Loop through elements with a slope
    slopevec[i] <- cal$Slope[which(cal$element == colnames(raw)[i])] # Add correct slope to slope vector
    SEvec[i] <- cal$SE_reg[which(cal$element == colnames(raw)[i])] # Add correct slope to slope vector
}

# Apply linear calibration
corr[, -1] <- t(t(raw[, -1]) * slopevec[-1])

# Remove data below 2x the SD on the calibration
corr_trim <- corr
corr_trim[cbind(rep(FALSE, nrow(corr)), t(t(corr[, -1]) - 2 * SEvec[-1]) < 1)] <- NA

# Add SD on calibration to dataframes
SEmat <- t(matrix(SEvec[-1], ncol = nrow(corr), nrow = length(SEvec[-1])))
colnames(SEmat) <- paste(colnames(corr)[-1], "_SD", sep = "")
corr <- cbind(corr, SEmat)
corr_trim <- cbind(corr_trim, SEmat)

# Export result
write.csv(corr, "XRF_Sr_spiking/Cedule003_cal.csv")
write.csv(corr_trim, "XRF_Sr_spiking/Cedule003_cal_trim.csv")
save(corr, file = "XRF_Sr_spiking/Cedule003_cal.rda")
save(corr_trim, file = "XRF_Sr_spiking/Cedule003_cal_trim.rda")
