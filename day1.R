#Day 1: Nov 10

# Practice:
# Import multispectral and hyperspectral rasters and lidar point clouds
# Inspect (bands, resolution, extent, point density), visualize (band images and composites), and summarize (data attributes and structure)
# Quality assessment
# Export subsets and intermediate products

# Today's goals
# download hyperspectral, multispectral and lidar data
# and visualize, inspect and crop the data

# We are working with open source NEON data
# And that means that you need to download it

# https://www.neonscience.org/
# You first need to create an account or sign in (top-right)
# If you are creating an account, you need to validate your email and verify your account

# Click "my account" (top-left) and fill in required information
# Click "save changes" button at the top or left
# Scroll back down to API Tokens section and click on "get API token"
# If you do not see this button, then you need to either/both validate your email and verify your account
# Copy this immediately!

# And then paste your token between the quotes below
# Sys.setenv(NEON_TOKEN = "your_token_goes_here")
Sys.setenv(NEON_TOKEN = "eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImF1ZCI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvYXBpL3YwLyIsImlhdCI6MTc5MzY5NjgxMywiZXhwIjoxODI1MjMyODEzLCJzdWIiOiJyYWt1ejY5NjZAbm1idS5ubyIsImVtYWlsIjoicmFrdXo2OTY2QG5tYnUubm8iLCJzY29wZSI6InJhdGU6cHVibGljIn0.g6aosuhiuG0WJa3Ky5iTbSRNIVBQnABuXfjMeJ8HeTL9hVZnHaN4Yyxtled9IwWscJeiRLn6oWFNMoAhVTSeiw")

token <- Sys.getenv("NEON_TOKEN") #retrieve
print(token) #print

# Let's load the necessary libraries for today
library(neonUtilities)
library(httr)
library(jsonlite)
library(BiocManager)
library(terra)
library(lidR)
library(ggplot2)
library(rhdf5) #note that this pacakge needs to be installed like this: BiocManager::install("rhdf5")

# Exercise 0 - get data ####

# Define site
target_site <- "HARV" 
# Q: What is this target site?
# see https://www.neonscience.org/field-site-map-and-info

# Define the NEON products we are interested in
product_vector <- c(
  "Hyperspectral_Era1" = "DP3.30006.001",
  "Hyperspectral_Era2" = "DP3.30006.002",
  "Multispectral_RGB"  = "DP3.30010.001",
  "LiDAR_PointCloud"   = "DP1.30003.001"
)
# Q: What data products are we downloading?
# see https://data.neonscience.org/data-products/explore

# Create an empty environment list
all_months_list <- list()

# Initialize loop through each NEON product to find availability
cat("Querying NEON API for site:", target_site, "...\n\n")
for (product_name in names(product_vector)) {
  prod_id <- product_vector[product_name]
  
  # API setup
  clean_url <- paste("https://data.neonscience.org/api/v0/products", prod_id, sep = "/")
  cat("Connecting to:", clean_url, "\n")
  res <- GET(clean_url) # Executes the API request
  
  # Parse successful responses to look for target site metadata
  if (status_code(res) == 200) {
    parsed_json <- fromJSON(content(res, "text", encoding = "UTF-8"))
    site_codes  <- parsed_json$data$siteCodes
    
    # And then find data for the target site
    site_match <- site_codes[site_codes$siteCode == target_site, ]
    
    # Extract the available data months 
    if (nrow(site_match) > 0 && !is.null(site_match$availableMonths)) {
      all_months_list[[product_name]] <- unlist(site_match$availableMonths)
    } else {
      all_months_list[[product_name]] <- character(0)
    }
  } else {
    all_months_list[[product_name]] <- character(0)
  }
}

# Process and align the months
hsi_months   <- unique(c(all_months_list$Hyperspectral_Era1, all_months_list$Hyperspectral_Era2))
rgb_months   <- all_months_list$Multispectral_RGB
lidar_months <- all_months_list$LiDAR_PointCloud
master_timeline <- sort(unique(c(hsi_months, rgb_months, lidar_months)))

# Build a grid checking which instruments collected data when
overlap_matrix <- data.frame(
  Month          = master_timeline,
  Hyperspectral  = master_timeline %in% hsi_months,
  Multispectral  = master_timeline %in% rgb_months,
  LiDAR_PointCloud = master_timeline %in% lidar_months
)

# Print the complete breakdown matrix
print(overlap_matrix, row.names = FALSE)

# Find dates where all elements are TRUE
perfect_overlaps <- overlap_matrix[overlap_matrix$Hyperspectral & 
                                     overlap_matrix$Multispectral & 
                                     overlap_matrix$LiDAR_PointCloud, ]

# Find cverlaps across all three sensors
if (nrow(perfect_overlaps) >= 2) {
  cat("You have", nrow(perfect_overlaps), "matching dates.\n")
  print(perfect_overlaps$Month)
} else if (nrow(perfect_overlaps) == 1) {
  cat("Only one overlapping month found (", perfect_overlaps$Month, "). Not enough for a time series.\n")
} else {
  cat("No overlapping months found.\n")
}
# Now that we have identified data, we need to download it

# We are going to select a smaller square located here
easting    <- 731500   
northing   <- 4713500  

# Define your final project directory
project_dir <- "C:/Users/rakuz6966/OneDrive - Norwegian University of Life Sciences/Documents/rforrs2026"

# Define a temporary staging folder (important if you are using OneDrive or similar for your final project)
temp_stage_dir <- "C:/neon_staging_temp"

# Download 2016 and 2024 to local staging
# 2016
byTileAOP(dpID = "DP3.30006.001", site = target_site, year = "2016", easting = easting, northing = northing, check.size = FALSE, savepath = temp_stage_dir, token = token)
byTileAOP(dpID = "DP3.30010.001", site = target_site, year = "2016", easting = easting, northing = northing, check.size = FALSE, savepath = temp_stage_dir, token = token)
byTileAOP(dpID = "DP1.30003.001", site = target_site, year = "2016", easting = easting, northing = northing, check.size = FALSE, savepath = temp_stage_dir, token = token)
# 2024 
byTileAOP(dpID = "DP3.30006.002", site = target_site, year = "2024", easting = easting, northing = northing, check.size = FALSE, savepath = temp_stage_dir, token = token, include.provisional = TRUE)
byTileAOP(dpID = "DP3.30010.001", site = target_site, year = "2024", easting = easting, northing = northing, check.size = FALSE, savepath = temp_stage_dir, token = token, include.provisional = TRUE)
byTileAOP(dpID = "DP1.30003.001", site = target_site, year = "2024", easting = easting, northing = northing, check.size = FALSE, savepath = temp_stage_dir, token = token, include.provisional = TRUE)
# This might take a couple of minutes

# Transfer datasets to the final project folder
product_directories <- list.dirs(temp_stage_dir, full.names = TRUE, recursive = FALSE)

for (dir_path in product_directories) {
  folder_name <- basename(dir_path)
  destination <- file.path(project_dir, folder_name)
  
  # Ensure target directory structure exists
  if (!dir.exists(destination)) dir.create(destination, recursive = TRUE)
  
  # Copy files over recursively
  file.copy(from = dir_path, to = project_dir, recursive = TRUE, overwrite = TRUE)
}

# Clean up the local staging area to save hard drive space
unlink(temp_stage_dir, recursive = TRUE)

# Check your computer to make sure that the data are properly downloaded
# You will likely notice that the files are very nested

# We are going to flatten and rename datasets 

# First, define and create the clean flat directory
clean_rs_dir <- file.path(project_dir, "rsdata")
if (!dir.exists(clean_rs_dir)) dir.create(clean_rs_dir, recursive = TRUE)

# Next, gather all the nested files
nested_h5   <- list.files(path = project_dir, pattern = "\\.h5$", recursive = TRUE, full.names = TRUE)
nested_tif  <- list.files(path = project_dir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
nested_laz  <- list.files(path = project_dir, pattern = "\\.laz$", recursive = TRUE, full.names = TRUE)

# Then combine them into a single processing list
all_nested_files <- c(nested_h5, nested_tif, nested_laz)

# And loop through to detect parameters, rename, and transfer safely
for (old_path in all_nested_files) {
  
  file_year <- "UNKNOWN"
  if (grepl("2016", old_path)) file_year <- "2016"
  if (grepl("2024", old_path)) file_year <- "2024"
  
  # Determine the intuitive sensor name (versus data product name)
  file_ext   <- tools::file_ext(old_path)
  sensor_tag <- "unknown"
  
  if (file_ext == "h5")  sensor_tag <- "hyperspectral"
  if (file_ext == "tif") sensor_tag <- "multispectral"
  if (file_ext == "laz") sensor_tag <- "lidar"
  
  # Make a short intuitive filename
  new_filename <- paste0("NEON_HARV_", sensor_tag, "_", file_year, ".", file_ext)
  new_dest_path <- file.path(clean_rs_dir, new_filename)
  
  # Copy the file to its clean, flat home with its shiny new name!
  file.copy(from = old_path, to = new_dest_path, overwrite = TRUE)
}

# # 4. Clean up the messy nested folders left behind in the project directory root
# folders_to_remove <- file.path(project_dir, c("DP3.30006.001", "DP3.30006.002", "DP3.30010.001", "DP1.30003.001"))
# for (f in folders_to_remove) {
#   if (dir.exists(f)) unlink(f, recursive = TRUE)
# }

# Check the new folders

# Clean up all messy nested folders starting with "DP" in the project directory root
folders_to_remove <- list.dirs(path = project_dir, full.names = TRUE, recursive = FALSE)
dp_folders <- folders_to_remove[grepl("^DP", basename(folders_to_remove))]

for (f in dp_folders) {
  if (dir.exists(f)) {
    # force = TRUE helps bypass picky OneDrive/Windows file-system state locks
    unlink(f, recursive = TRUE, force = TRUE) 
  }
}
# Check your folders again, make sure the original nested folders are gone

# Print the files in your new clean directory
print(list.files(clean_rs_dir))


# Exercise 1 - visual checks ####

# Q: why visualize data at this stage?
# A: To confirm spatial alignment and diagnose cross-track or illumination anomalies.

# Locate the fiels in your flat data directory
rs_data_dir <- file.path(project_dir, "rsdata")

h5_files   <- list.files(path = rs_data_dir, pattern = "\\.h5$", full.names = TRUE)
tiff_files <- list.files(path = rs_data_dir, pattern = "\\.tif$", full.names = TRUE)
laz_files  <- list.files(path = rs_data_dir, pattern = "\\.laz$", full.names = TRUE)

#Let's start with the lidar
laz_2016 <- laz_files[grepl("2016", laz_files)]
laz_2024 <- laz_files[grepl("2024", laz_files)]

# Check, print and plot
# 2016 lidar
if (length(laz_2016) > 0) {
  message("Reading 2016 Lidar Point Cloud...")
  las_2016 <- readLAS(laz_2016[1])
  print(las_2016)
  plot(las_2016, main = "3D Point Cloud Forest (2016)")
} else {
  warning("No 2016 .laz point cloud files found.")
}
# 2024 lidar
if (length(laz_2024) > 0) {
  message("Reading 2024 Lidar Point Cloud...")
  las_2024 <- readLAS(laz_2024[1])
  print(las_2024)
  plot(las_2024, main = "3D Point Cloud Forest (2024)")
} else {
  warning("No 2024 .laz point cloud files found.")
}
# Q: How do the lidar data compare?
# Use the print output in the console and the plots

# Now let's do the multispectral
rgb_2016 <- tiff_files[grepl("2016", tiff_files)]
rgb_2024 <- tiff_files[grepl("2024", tiff_files)]

#dev.new(noRStudioGD = TRUE)
par(mfrow = c(1, 2))
if (length(rgb_2016) > 0) plotRGB(rast(rgb_2016[1]), r = 1, g = 2, b = 3, stretch = "lin", main = "Multispectral RGB (2016)")
if (length(rgb_2024) > 0) plotRGB(rast(rgb_2024[1]), r = 1, g = 2, b = 3, stretch = "lin", main = "Multispectral RGB (2024)")
par(mfrow = c(1, 1)) # reset plots
# Q: How do the multispectral data compare?
# Again, use the print output in the console and the plots

# Q: how does plotRGB work?
?plotRGB
# Replot above, but change the parameters based on the help file

# And, finally, we will do the hyperspectral
# Define target wavelengths for RGB visualization
# Q: Why?
target_red   <- 660
target_green <- 540
target_blue  <- 465

# Path-mapping function
make_hsi_rgb_by_wavelength <- function(h5_path, target_site = "HARV") {
  file_structure <- h5ls(h5_path)
  
  possible_paths <- c(
    paste0("/", target_site, "/Reflectance/Reflectance_Metadata/Spectral_Data/Wavelength"), 
    paste0("/", target_site, "/Reflectance/Metadata/Spectral_Data/Wavelength")             
  )
  
  metadata_path <- NULL
  for (p in possible_paths) {
    parent_dir <- dirname(p)
    base_obj   <- basename(p)
    if (any(file_structure$group == parent_dir & file_structure$name == base_obj)) {
      metadata_path <- p
      break
    }
  }
  
  if (is.null(metadata_path)) {
    stop("Could not find the Wavelength metadata array in the HDF5 file structure.")
  }
  
  data_path <- paste0("/", target_site, "/Reflectance/Reflectance_Data")
  wavelengths <- h5read(h5_path, metadata_path)
  
  red_idx   <- which.min(abs(wavelengths - target_red))
  green_idx <- which.min(abs(wavelengths - target_green))
  blue_idx  <- which.min(abs(wavelengths - target_blue))
  
  # message(paste("\nParsing file:", basename(h5_path)))
  message(sprintf("Red Target %snm matched to Band %s (%snm)", target_red, red_idx, round(wavelengths[red_idx], 1)))
  message(sprintf("Green Target %snm matched to Band %s (%snm)", target_green, green_idx, round(wavelengths[green_idx], 1)))
  message(sprintf("Blue Target %snm matched to Band %s (%snm)", target_blue, blue_idx, round(wavelengths[blue_idx], 1)))
  
  r_band <- h5read(h5_path, data_path, index = list(red_idx, NULL, NULL))[1, , ]
  g_band <- h5read(h5_path, data_path, index = list(green_idx, NULL, NULL))[1, , ]
  b_band <- h5read(h5_path, data_path, index = list(blue_idx, NULL, NULL))[1, , ]
  
  r_mat <- t(r_band); g_mat <- t(g_band); b_mat <- t(b_band)
  
  r_mat[r_mat == -9999] <- NA
  g_mat[g_mat == -9999] <- NA
  b_mat[b_mat == -9999] <- NA
  
  rgb_stack <- c(rast(r_mat), rast(g_mat), rast(b_mat))
  names(rgb_stack) <- c("Red", "Green", "Blue")
  
  return(rgb_stack)
}

# Query our clean file list 
h5_2016 <- h5_files[grepl("2016", h5_files)]
h5_2024 <- h5_files[grepl("2024", h5_files)]

#dev.new(noRStudioGD = TRUE)
par(mfrow = c(1, 2))

# 2016 hyperspectral as RGB
if (length(h5_2016) > 0) {
  rgb_16 <- make_hsi_rgb_by_wavelength(h5_2016[1], "HARV")
  plotRGB(rgb_16, r = 1, g = 2, b = 3, stretch = "lin", main = "Hyperspectral RGB (2016)")
} else {
  warning("No 2016 hyperspectral file found.")
}

# 2024 hyperspectral as RGB 
if (length(h5_2024) > 0) {
  rgb_24 <- make_hsi_rgb_by_wavelength(h5_2024[1], "HARV")
  plotRGB(rgb_24, r = 1, g = 2, b = 3, stretch = "lin", main = "Hyperspectral RGB (2024)")
} else {
  warning("No 2024 hyperspectral file found.")
}

par(mfrow = c(1, 1)) # reset plots
# Q: How do the hyperspectral data compare?
# Again, use the print output in the console and the plots

# Q: If you wanted to visualize alternate bands, how would you modify the code above?

# Release internal file locks (NEON file structure specific)
H5close()

# Q: How else might we want to visualize the spectral data?

# Open a clean window for our 2x2 grid
#dev.new(noRStudioGD = TRUE)
layout(matrix(1:4, nrow = 2, byrow = TRUE))
par(mar = c(4, 4, 3, 2))

# 2016 multispectral profile
if (length(rgb_2016) > 0) {
  mst_16 <- rast(rgb_2016)
  # Randomly extract cell coordinates (excluding NA borders)
  valid_cells <- which(!is.na(values(mst_16[[1]])))
  sampled_cells <- sample(valid_cells, 10)
  mst_vals <- mst_16[sampled_cells]
  
  # Set up empty plot canvas (3 bands: Red=1, Green=2, Blue=3)
  plot(1:3, colMeans(mst_vals), type="n", xaxt="n", xlab="Bands", ylab="Digital Number", 
       ylim=c(0, max(mst_vals, na.rm=TRUE)), main="Multispectral Profiles (2016)")
  axis(1, at=1:3, labels=c("Red (1)", "Green (2)", "Blue (3)"))
  
  # Draw a line for each of the 10 random pixels
  for(i in 1:10) lines(1:3, as.numeric(mst_vals[i, ]), col=rainbow(10)[i], lwd=1.5)
}

# 2024 multispectral profile
if (length(rgb_2024) > 0) {
  mst_24 <- rast(rgb_2024)
  valid_cells <- which(!is.na(values(mst_24[[1]])))
  sampled_cells <- sample(valid_cells, 10)
  mst_vals <- mst_24[sampled_cells]
  
  plot(1:3, colMeans(mst_vals), type="n", xaxt="n", xlab="Bands", ylab="Digital Number", 
       ylim=c(0, max(mst_vals, na.rm=TRUE)), main="Multispectral Profiles (2024)")
  axis(1, at=1:3, labels=c("Red (1)", "Green (2)", "Blue (3)"))
  
  for(i in 1:10) lines(1:3, as.numeric(mst_vals[i, ]), col=rainbow(10)[i], lwd=1.5)
}

# 2016 hyperspectral profile
if (length(h5_2016) > 0) {
  # Dynamically pull the full wavelength profile array out using the H5 setup
  h5_layout <- h5ls(h5_2016)
  wave_path <- h5_layout$group[grep("Spectral_Data", h5_layout$group)][1]
  wavelengths_16 <- h5read(h5_2016, paste0(wave_path, "/Wavelength"))
  
  # Read full spectral data cube path
  data_path_16 <- paste0("/HARV/Reflectance/Reflectance_Data")
  
  # To sample safely from a large H5 matrix, we read a random spatial patch
  # and pull out 10 valid non-boundary reflectance columns
  cube_sample <- h5read(h5_2016, data_path_16, index=list(NULL, 1:50, 1:50))
  
  plot(wavelengths_16, cube_sample[,1,1], type="n", xlab="Wavelength (nm)", ylab="Reflectance",
       ylim=c(0, 5000), main="Hyperspectral Signatures (2016)")
  
  count <- 0
  for(x in 1:50) {
    for(y in 1:50) {
      sig <- cube_sample[, x, y]
      if(all(sig != -9999) && count < 10) {
        count <- count + 1
        lines(wavelengths_16, sig, col=rainbow(10)[count], lwd=1.5)
      }
    }
  }
}

# 2024 hyperspectral profile
if (length(h5_2024) > 0) {
  h5_layout_24 <- h5ls(h5_2024)
  wave_path_24 <- h5_layout_24$group[grep("Spectral_Data", h5_layout_24$group)][1]
  wavelengths_24 <- h5read(h5_2024, paste0(wave_path_24, "/Wavelength"))
  
  data_path_24 <- paste0("/HARV/Reflectance/Reflectance_Data")
  cube_sample_24 <- h5read(h5_2024, data_path_24, index=list(NULL, 1:50, 1:50))
  
  plot(wavelengths_24, cube_sample_24[,1,1], type="n", xlab="Wavelength (nm)", ylab="Reflectance",
       ylim=c(0, 5000), main="Hyperspectral Signatures (2024)")
  
  count <- 0
  for(x in 1:50) {
    for(y in 1:50) {
      sig <- cube_sample_24[, x, y]
      if(all(sig != -9999) && count < 10) {
        count <- count + 1
        lines(wavelengths_24, sig, col=rainbow(10)[count], lwd=1.5)
      }
    }
  }
}

# Release H5 connection
H5close()

# Q: How do these plots compare?


# Exercise 2 - inspect metadata ####

# Point to your flat data directory
rs_data_dir <- file.path(project_dir, "rsdata")

h5_files   <- list.files(path = rs_data_dir, pattern = "\\.h5$", full.names = TRUE)
tiff_files <- list.files(path = rs_data_dir, pattern = "\\.tif$", full.names = TRUE)
laz_files  <- list.files(path = rs_data_dir, pattern = "\\.laz$", full.names = TRUE)

years <- c("2016", "2024")

# Version 1. Check file architecture (Structural Schemas & Attributes)
for (yr in years) {
  cat("\n📂 FILE SCHEMAS FOR YEAR:", yr)

  # Lidar
  laz_match <- laz_files[grepl(yr, laz_files)]
  if (length(laz_match) > 0) {
    las <- readLAS(laz_match[1], filter = "-drop_z_below 0")
    cat("\n[1. Lidar Point Cloud Format Attributes]\n")
    cat("  • LAS Specification Format version:", las@header@PHB$`Version Major`, ".", las@header@PHB$`Version Minor`, "\n")
    cat("  • Point Data Record Format ID     :", las@header@PHB$`Point Data Record Format`, "\n")
    cat("  • Available Data Attribute Slots  : ", paste(names(las@data), collapse = ", "), "\n")
  }
  
  # Multispectral
  rgb_match <- tiff_files[grepl(yr, tiff_files)]
  if (length(rgb_match) > 0) {
    img_rast <- rast(rgb_match[1])
    cat("\n[2. Multispectral GeoTIFF Band Layout]\n")
    cat("  • Total Image Band Count :", nlyr(img_rast), "\n")
    cat("  • Data Numeric Type Class:", datatype(img_rast), "\n")
    cat("  • Native Band Name Vector:", paste(names(img_rast), collapse = ", "), "\n")
    cat("  • Coordinate System (WKT Format Extract):\n   ", substring(crs(img_rast), 1, 120), "... [truncated]\n")
  }
  
  # Hyperspectral
  h5_match <- h5_files[grepl(yr, h5_files)]
  if (length(h5_match) > 0) {
    cat("\n[3. Hyperspectral HDF5 Layout Hierarchy]\n")
    h5_layout <- h5ls(h5_match[1])
    unique_groups <- head(unique(h5_layout$group), 4) # Limit preview size for workshop scannability
    for (g in unique_groups) {
      sub_items <- h5_layout$name[h5_layout$group == g]
      cat("  📁 ", g, "\n     └── Fields: ", paste(head(sub_items, 6), collapse = ", "), "\n")
    }
  }
}
# Q: What does this tell you?

# Version 2. Detailed view of actual parameters
for (yr in years) {
  cat("\n SPATIAL METADATA FOR YEAR:", yr)
  
  # Lidar detailed summary
  laz_match <- laz_files[grepl(yr, laz_files)]
  if (length(laz_match) > 0) {
    las <- readLAS(laz_match[1], filter = "-drop_z_below 0")
    hdr <- las@header
    cat("\n[1. Lidar Metadata Summary]\n")
    cat("  • File Timeline Tracking  : Day", hdr@PHB$`File Creation Day of Year`, "of Year", hdr@PHB$`File Creation Year`, "\n")
    cat("  • Maximum X, Y, Z Extents : X =", hdr@PHB$`Max X`, "Y =", hdr@PHB$`Max Y`, "Z =", hdr@PHB$`Max Z`, "\n")
    cat("  • Minimum X, Y, Z Extents : X =", hdr@PHB$`Min X`, "Y =", hdr@PHB$`Min Y`, "Z =", hdr@PHB$`Min Z`, "\n")
    cat("  • Echo Return Multipliers : \n")
    print(table(las$ReturnNumber))
    cat("  • Feature Classes (2 = Ground, 5 = High Veg):\n")
    print(table(las$Classification))
  }
  
  # Multispectral detailed summary
  rgb_match <- tiff_files[grepl(yr, tiff_files)]
  if (length(rgb_match) > 0) {
    cat("\n[2. Multispectral Metadata Summary]\n")
    cat("--------------------------------------------------\n")
    cat(substring(describe(rgb_match[1]), 1, 350), "... [truncated driver log]\n")
  }
  
  # Hyperspectral detailed summary
  h5_match <- h5_files[grepl(yr, h5_files)]
  if (length(h5_match) > 0) {
    cat("\n[3. Hyperspectral Metadata Summary]\n")
    cat("--------------------------------------------------\n")
    h5_layout <- h5ls(h5_match[1])
    
    # Target Coordinate Projection System
    coord_group <- h5_layout$group[grep("Coordinate_System", h5_layout$group)][1]
    coord_name  <- h5_layout$name[h5_layout$group == coord_group & grep("string", h5_layout$name, ignore.case=TRUE)][1]
    proj_string <- h5read(h5_match[1], paste0(coord_group, "/", coord_name))
    cat("  • Extracted Proj String  :", substring(proj_string, 1, 90), "... \n")
    
    # Target Instrument Wavelength Ranges
    wave_group <- h5_layout$group[grep("Spectral_Data", h5_layout$group)][1]
    waves      <- h5read(h5_match[1], paste0(wave_group, "/Wavelength"))
    cat("  • Spectral Sensor Span    :", length(waves), "total bands\n")
    cat("     - First 3 Bands (Visible) :", paste(round(head(waves, 3), 1), collapse = "nm, "), "nm...\n")
    cat("     - Last 3 Bands (Infrared) :", paste(round(tail(waves, 3), 1), collapse = "nm, "), "nm\n")
    
    # Target Solar Flight Geometry
    log_rows <- h5_layout[grep("Logs/", h5_layout$group), ]
    if (nrow(log_rows) > 0) {
      target_log_group <- log_rows$group[1]
      solar_az <- h5read(h5_match[1], paste0(target_log_group, "/Solar_Azimuth_Angle"))
      solar_zn <- h5read(h5_match[1], paste0(target_log_group, "/Solar_Zenith_Angle"))
      cat("  • Flight Strip Geometry  :", basename(target_log_group), "\n")
      cat("     - Solar Azimuth Angle  :", round(solar_az[1], 1), "degrees\n")
      cat("     - Solar Zenith Angle   :", round(solar_zn[1], 1), "degrees\n")
    }
  }
}

H5close() # Release connection
# Q: What does this tell you?

# Version 3. Extract metadata and save as text files

# First, create a directory within your project
export_dir <- file.path(project_dir, "rsmetadata")
if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)

# Lidar metadata
if (length(laz_files) > 0) {
  for (i in seq_along(laz_files)) {
    base_name  <- tools::file_path_sans_ext(basename(laz_files[i]))
    output_txt <- file.path(export_dir, paste0("METADATA_LIDAR_", base_name, ".txt"))
    
    las <- readLAS(laz_files[i], filter = "-drop_all") 
    
    sink(output_txt)
    cat("COMPLETE METADATA FOR LIDAR\n")
    cat("Source File:", basename(laz_files[i]), "\n")
    print(summary(las))
    sink()
  }
}
# Q: What do you see when you check folder and open text file?

# Multispectral metadata
if (length(tiff_files) > 0) {
  for (i in seq_along(tiff_files)) {
    base_name  <- tools::file_path_sans_ext(basename(tiff_files[i]))
    output_txt <- file.path(export_dir, paste0("METADATA_IMAGERY_", base_name, ".txt"))
    
    full_gdal_log <- describe(tiff_files[i], print = FALSE)
    
    sink(output_txt)
    cat("COMPLETE METADATA FOR MULTISPECTRAL\n")
    cat("Source File:", basename(tiff_files[i]), "\n")
    cat(full_gdal_log)
    sink()
  }
}
# Q: What do you see when you check folder and open text file?

# Hyperspectral metadata
if (length(h5_files) > 0) {
  for (i in seq_along(h5_files)) {
    base_name  <- tools::file_path_sans_ext(basename(h5_files[i]))
    output_txt <- file.path(export_dir, paste0("METADATA_SPECTRAL_", base_name, ".txt"))
    
    # Grab the directory structure tree first
    h5_layout <- h5ls(h5_files[i], recursive = TRUE)
    
    # Filter out the dataset path 
    meta_rows <- h5_layout[h5_layout$otype == "H5I_DATASET" & h5_layout$name != "Reflectance_Data", ]
    
    # Open text capture sink connection
    sink(output_txt)
    
    # Never truncated
    old_max <- getOption("max.print")
    options(max.print = 999999)
    
    cat("COMPLETE METADATA FOR HYPERSPECTRAL\n")
    cat("Source File:", basename(h5_files[i]), "\n")
    
    # Loop through each field path and print its contents
    for (r in 1:nrow(meta_rows)) {
      full_field_path <- paste0(meta_rows$group[r], "/", meta_rows$name[r])
      
      cat("📁 PATH: ", full_field_path, "\n")
      cat("----------------------------------------------------------------------\n")
      
      # Read the actual internal value array or text attribute string
      field_values <- h5read(h5_files[i], full_field_path)
      
      # If the value is a large 2D matrix (often like aspect or slope), summarize it to keep logs readable
      if (is.matrix(field_values) && length(field_values) > 100) {
        cat("  [Spatial Array Layer] Dimensions:", paste(dim(field_values), collapse="x"), "\n")
        cat("  Summary Stats:\n")
        print(summary(as.vector(field_values)))
      } else {
        # Otherwise print the direct string data or parameter values
        print(field_values)
      }
      cat("\n\n")
    }
    
    # Restore original console print preferences and close connection
    options(max.print = old_max)
    sink()
  }
  H5close()
}
# Q: What do you see when you check folder and open text file?

# Exercise 3 - cropping & exporting ####

# Define and create the clean crop output directory
crop_out_dir <- file.path(project_dir, "rsdata", "crop")
if (!dir.exists(crop_out_dir)) dir.create(crop_out_dir, recursive = TRUE)

# Define the exact bounding box using the target anchor point
# NEON coordinates represent the lower-left corner or tile anchors.
# We are going to build a 100m x 100m bounding box extending East and North.
crop_extent <- ext(easting, easting + 100, northing, northing + 100)

# Re-verify our flat source files
rs_data_dir <- file.path(project_dir, "rsdata")
h5_files    <- list.files(path = rs_data_dir, pattern = "\\.h5$", full.names = TRUE)
tiff_files  <- list.files(path = rs_data_dir, pattern = "\\.tif$", full.names = TRUE)
laz_files   <- list.files(path = rs_data_dir, pattern = "\\.laz$", full.names = TRUE)
years       <- c("2016", "2024")

# Crop and export multispectral data
for (yr in years) {
  rgb_match <- tiff_files[grepl(yr, tiff_files)]
  
  if (length(rgb_match) > 0) {
    message(paste("Processing Multispectral Crop for Year:", yr))
    
    # Load pointer, crop immediately, and write to disk
    img_full <- rast(rgb_match[1])
    img_crop <- crop(img_full, crop_extent)
    
    out_name <- paste0("CROP_NEON_HARV_multispectral_", yr, ".tif")
    writeRaster(img_crop, file.path(crop_out_dir, out_name), overwrite = TRUE)
    
    # Explicitly purge variables from RAM
    rm(img_full, img_crop)
    gc() # Trigger garbage collection
  }
}
# Q: Check your folders; what do you see?

# Crop and export lidar data
for (yr in years) {
  laz_match <- laz_files[grepl(yr, laz_files)]
  
  if (length(laz_match) > 0) {
    message(paste("Processing Lidar Crop for Year:", yr))
    
    # RAM PRESERVATION: Use readLAS stream filters to drop all points outside 
    # the target grid box BEFORE pulling the point matrix into active memory.
    filter_string <- sprintf("-keep_xy %f %f %f %f", 
                             crop_extent$xmin, crop_extent$ymin, 
                             crop_extent$xmax, crop_extent$ymax)
    
    las_crop <- readLAS(laz_match[1], filter = filter_string)
    
    out_name <- paste0("CROP_NEON_HARV_lidar_", yr, ".laz")
    writeLAS(las_crop, file.path(crop_out_dir, out_name))
    
    rm(las_crop)
    gc()
  }
}
# Q: Check your folders; what do you see?

# Crop and export hyperspectral data

# I don't get code right the first time!
# Coding is an iterative process
# This is how I (eventually) got to the full crop

# Start with 2016 file for a test
h5_test <- list.files(path = file.path(project_dir, "rsdata"), pattern = "\\.h5$", full.names = TRUE)[1]
rgb_test <- list.files(path = file.path(project_dir, "rsdata"), pattern = "\\.tif$", full.names = TRUE)[1]

# Check and print dataset dimensions
layout <- h5ls(h5_test)
ref_data_meta <- layout[layout$name == "Reflectance_Data", ]
print(ref_data_meta)

# Check and print reference raster dimensions
ref_rast <- rast(rgb_test)
print(ref_rast)

# Test a 1-pixel slice across 3 dimensions
tryCatch({
  test_slice <- h5read(h5_test, "/HARV/Reflectance/Reflectance_Data", index = list(1, 1:5, 1:5))
  cat("Success! Slice dimensions are:", paste(dim(test_slice), collapse = " x "), "\n")
}, error = function(e) {
  cat("Slicing failed! Error message:", e$message, "\n")
})

# Crop and export all hyperspectral bands
for (yr in years) {
  h5_match  <- h5_files[grepl(yr, h5_files)][1]
  rgb_match <- tiff_files[grepl(yr, tiff_files)][1]
  
  if (!is.na(h5_match) && !is.na(rgb_match)) {
    message(paste("Processing Hyperspectral Crop for Year:", yr))
    
    h5_path <- h5_match
    
    # Grab full tile coordinates from the multispectral raster
    ref_raster  <- terra::rast(rgb_match)
    tile_extent <- terra::ext(ref_raster)
    
    # Explicitly set HSI resolution to 1 meter (matching the 1000x1000 diagnostic dim)
    hsi_pixel_size <- 1.0 
    
    # Extract Wavelength metadata from the H5 file attributes
    h5_layout  <- h5ls(h5_path)
    wave_group <- h5_layout$group[grep("Spectral_Data", h5_layout$group)][1] 
    waves      <- h5read(h5_path, paste0(wave_group, "/Wavelength"))
    num_bands  <- length(waves)
    
    # Calculate spatial index boundaries based on the 1-meter HSI grid
    col_start <- as.integer(round((crop_extent$xmin - tile_extent$xmin) / hsi_pixel_size)) + 1
    col_end   <- as.integer(round((crop_extent$xmax - tile_extent$xmin) / hsi_pixel_size))
    
    row_start <- as.integer(round((tile_extent$ymax - crop_extent$ymax) / hsi_pixel_size)) + 1
    row_end   <- as.integer(round((tile_extent$ymax - crop_extent$ymin) / hsi_pixel_size))
    
    # Enforce structural safety boundary constraints
    col_start <- max(1, min(col_start, 1000)); col_end <- max(1, min(col_end, 1000))
    row_start <- max(1, min(row_start, 1000)); row_end <- max(1, min(row_end, 1000))
    
    data_path <- "/HARV/Reflectance/Reflectance_Data"
    
    # Extract each band layer sequentially from disk
    message("   • Slicing spatial footprint channel-by-channel across 426 layers...")
    band_list <- list()
    ref_crs   <- terra::crs(ref_raster)
    
    for (b in 1:num_bands) {
      # Slice ONLY the target 100x100 index window from the file connection
      b_slice <- h5read(h5_path, data_path, index = list(b, col_start:col_end, row_start:row_end))
      
      # Strip down to a clean 2D matrix
      b_matrix <- b_slice[1, , ]
      
      # Handle background NoData values safely
      b_matrix[b_matrix == -9999] <- NA
      
      # Invert row values to match standard north-up geographic coordinates
      b_matrix_fixed <- t(b_matrix[, ncol(b_matrix):1])
      
      # Assign the sub-raster properties directly inline
      band_list[[b]] <- terra::rast(b_matrix_fixed, ext = crop_extent, crs = ref_crs)
    }
    
    # Bind the individual layers together into a single multi-band raster stack
    hsi_stack <- terra::rast(band_list)
    
    # Apply wavelength tags to all 426 bands
    names(hsi_stack) <- paste0("Band_", 1:num_bands, "_", round(waves, 1), "nm")
    
    # Export crop
    out_name <- paste0("CROP_NEON_HARV_hyperspectral_", yr, ".tif")
    terra::writeRaster(hsi_stack, file.path(crop_out_dir, out_name), overwrite = TRUE, gdal = "COMPRESS=LZW")
    
    # Clear memory allocations and drop active hooks before next year loop
    rm(band_list, hsi_stack, ref_raster)
    H5close()
    gc()
    message("   • Success! Complete 426-band spatial crop successfully written to disk.")
  } else {
    warning(paste("Skipping HSI crop for year", yr, "- file structures missing."))
  }
}
# Q: Check your folders; what do you see?


# Exercise 4 - QC/QA checks ####

crop_out_dir <- file.path(project_dir, "rsdata", "crop")
crop_tifs    <- list.files(crop_out_dir, pattern = "\\.tif$", full.names = TRUE)
crop_lazs    <- list.files(crop_out_dir, pattern = "\\.laz$", full.names = TRUE)

for (yr in years) {
  cat("\n Auditing spatial parameters for Year:", yr, "\n")
  
  ms_file  <- crop_tifs[grepl("multispectral", crop_tifs) & grepl(yr, crop_tifs)]
  hsi_file <- crop_tifs[grepl("hyperspectral", crop_tifs) & grepl(yr, crop_tifs)]
  laz_file <- crop_lazs[grepl(yr, crop_lazs)]
  
  # Check if all files exist before initializing parameters
  if (length(ms_file) == 0 || length(hsi_file) == 0 || length(laz_file) == 0) {
    cat("  ❌ Error: Missing one or more cropped layers for this year cycle.\n")
    next
  }
  
  # Clean, independent lines to ensure the R console never misinterprets spacing
  ms_rast  <- terra::rast(ms_file[1])
  hsi_rast <- terra::rast(hsi_file[1])
  las_sub  <- lidR::readLAS(laz_file[1], filter = "-drop_all")
  
  ms_ext   <- terra::ext(ms_rast)
  hsi_ext  <- terra::ext(hsi_rast)
  las_ext  <- lidR::ext(las_sub)
  
  # Assert Spatial Bounding Matches
  extent_check <- all.equal(c(ms_ext$xmin, ms_ext$xmax, ms_ext$ymin, ms_ext$ymax), c(hsi_ext$xmin, hsi_ext$xmax, hsi_ext$ymin, hsi_ext$ymax), tolerance = 1e-3)
  
  if (isTRUE(extent_check)) {
    cat("  Extent Alignment Match: Boundaries are perfectly identical.\n")
    cat(sprintf("     [X Range: %s to %s | Y Range: %s to %s]\n", ms_ext$xmin, ms_ext$xmax, ms_ext$ymin, ms_ext$ymax))
  } else {
    cat("  WARNING: Spatial boundaries vary slightly between raster sets!\n")
  }
  
  # Assert Coordinate System Synchronization
  ms_wkt  <- substring(terra::crs(ms_rast), 1, 100)
  hsi_wkt <- substring(terra::crs(hsi_rast), 1, 100)
  
  if (ms_wkt == hsi_wkt) {
    cat("  CRS Synchronization: Projection strings match perfectly.\n")
  } else {
    cat("  WARNING: Coordinate systems between MS and HSI are mismatched!\n")
  }
  
  # Clear and purge variables cleanly
  rm(ms_rast, hsi_rast, las_sub)
  invisible(gc())
}

# And let's do a visual check too
# Point to your cropped data directory
crop_data_dir <- file.path(project_dir, "rsdata", "crop")

crop_h5_files   <- list.files(path = crop_data_dir, pattern = "hyperspectral.*\\.tif$", full.names = TRUE)
crop_tiff_files <- list.files(path = crop_data_dir, pattern = "multispectral.*\\.tif$", full.names = TRUE)
crop_laz_files  <- list.files(path = crop_data_dir, pattern = "\\.laz$", full.names = TRUE)

# Lidar first
crop_laz_2016 <- crop_laz_files[grepl("2016", crop_laz_files)]
crop_laz_2024 <- crop_laz_files[grepl("2024", crop_laz_files)]

# Plot and print 2016
if (length(crop_laz_2016) > 0) {
  message("Reading 2016 Cropped Lidar Point Cloud...")
  las_crop_2016 <- readLAS(crop_laz_2016[1])
  
  # # Clean outlier noise using the dynamic 99.9th percentile filter
  # z_thresh_16 <- quantile(las_crop_2016$Z, 0.999)
  # las_crop_2016 <- filter_poi(las_crop_2016, Z <= z_thresh_16)
  
  print(las_crop_2016)
  plot(las_crop_2016)
} else {
  warning("No 2016 cropped .laz point cloud files found.")
}
# Run the code with the outlier section that is hashed out
# Q: What difference can you see?


# Plot and print 2024
if (length(crop_laz_2024) > 0) {
  message("Reading 2024 Cropped Lidar Point Cloud...")
  las_crop_2024 <- readLAS(crop_laz_2024[1])
  
  # # Clean outlier noise
  # z_thresh_24 <- quantile(las_crop_2024$Z, 0.999)
  # las_crop_2024 <- filter_poi(las_crop_2024, Z <= z_thresh_24)

  print(las_crop_2024)
  plot(las_crop_2024)
} else {
  warning("No 2024 cropped .laz point cloud files found.")
}
# Same thing here, re-run the code with the outlier section that is hashed out
# Q: What difference can you see?


# Multispectral
crop_rgb_2016 <- crop_tiff_files[grepl("2016", crop_tiff_files)]
crop_rgb_2024 <- crop_tiff_files[grepl("2024", crop_tiff_files)]

#dev.new(noRStudioGD = TRUE)
par(mfrow = c(1, 2))
if (length(crop_rgb_2016) > 0) plotRGB(rast(crop_rgb_2016[1]), r = 1, g = 2, b = 3, stretch = "lin", main = "Cropped Multispectral RGB (2016)")
if (length(crop_rgb_2024) > 0) plotRGB(rast(crop_rgb_2024[1]), r = 1, g = 2, b = 3, stretch = "lin", main = "Cropped Multispectral RGB (2024)")
par(mfrow = c(1, 1)) # reset plots
# Q: What do you see and how do the years compare?
# Q: Modify the parameters in plotRGB again

# Hyperspectral
# To visualize as RGB, again, define target wavelengths
target_red   <- 660
target_green <- 540
target_blue  <- 465
# Q: Do you really need to do this? Pros/cons?

# Modified mapping function (from earlier)
make_cropped_hsi_rgb <- function(tif_path) {
  hsi_stack <- rast(tif_path)
  
  # Parse wavelengths directly out of the GeoTIFF layer name properties
  layer_names <- names(hsi_stack)
  wavelengths <- as.numeric(gsub(".*_([0-9.]+)nm$", "\\1", layer_names))
  
  # Dynamically determine nearest band layers
  red_idx   <- which.min(abs(wavelengths - target_red))
  green_idx <- which.min(abs(wavelengths - target_green))
  blue_idx  <- which.min(abs(wavelengths - target_blue))
  
  message(paste("\nParsing Cropped Raster Stack:", basename(tif_path)))
  message(sprintf("Red Target %snm matched to Layer %s (%snm)", target_red, red_idx, round(wavelengths[red_idx], 1)))
  message(sprintf("Green Target %snm matched to Layer %s (%snm)", target_green, green_idx, round(wavelengths[green_idx], 1)))
  message(sprintf("Blue Target %snm matched to Layer %s (%snm)", target_blue, blue_idx, round(wavelengths[blue_idx], 1)))
  
  # Extract channels
  rgb_channels <- hsi_stack[[c(red_idx, green_idx, blue_idx)]]
  
  # Convert NEON 10,000 scale to standard fractional reflectance (0-1)
  rgb_channels <- rgb_channels / 10000
  
  # Diagnostic audit: Print true cell ranges to console to verify data presence
  cat("Diagnostic Real Data Range (Should be between 0 and 1):\n")
  print(global(rgb_channels, "range", na.rm = TRUE))
  
  names(rgb_channels) <- c("Red", "Green", "Blue")
  return(rgb_channels)
}

#dev.new(noRStudioGD = TRUE)
par(mfrow = c(1, 2))

# 2016 Cropped Hyperspectral Plot
if (length(crop_h5_files[grepl("2016", crop_h5_files)]) > 0) {
  hsi_crop_16 <- make_cropped_hsi_rgb(crop_h5_files[grepl("2016", crop_h5_files)][1])
  
  # Add axes = TRUE to force R to render titles on the plot window
  plotRGB(hsi_crop_16, r = 1, g = 2, b = 3, stretch = "lin", 
          axes = TRUE, mar = c(2, 2, 3, 2), main = "Cropped Hyperspectral RGB (2016)")
} else {
  warning("No 2016 cropped hyperspectral file found.")
}

# 2024 Cropped Hyperspectral Plot
if (length(crop_h5_files[grepl("2024", crop_h5_files)]) > 0) {
  hsi_crop_24 <- make_cropped_hsi_rgb(crop_h5_files[grepl("2024", crop_h5_files)][1])
  
  # Add axes = TRUE to force R to render titles on the plot window
  plotRGB(hsi_crop_24, r = 1, g = 2, b = 3, stretch = "lin", 
          axes = TRUE, mar = c(2, 2, 3, 2), main = "Cropped Hyperspectral RGB (2024)")
} else {
  warning("No 2024 cropped hyperspectral file found.")
}
par(mfrow = c(1, 1)) # reset plots
# Q: What do you see and how do the years compare?