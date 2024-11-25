##### Intermediate R, Fall 2024 #####
##### Karolinska Institutet #####
##### Gerda Stutaite #####

### Exam 1 - Task 1 (Image Filter) ###

# Install and load the package “magick”
install.packages("magick")
library(magick)

# Load the image
my_object <- image_read("C:/Users/gerstu/OneDrive - Karolinska Institutet/Skrivbordet/Exam_1_Before_Review/Image.jpg")

# Convert the object to a bitmap image
my_bitmap <- my_object[[1]]

# Display the image
image_read(my_bitmap)

# Retrieve more information about pixels
pixel_matrix <- image_data(my_object)
print(pixel_matrix) # The image is 528 pixels wide (width) and 393 pixels tall (height) + 3 channels

# GREY SCALE FILTER

# Create a function for the grey scale filter (takes a vector and returns a grey scale value)
function_greyscale <- function(color_rgb) {
  
  R <- color_rgb[1]
  G <- color_rgb[2]
  B <- color_rgb[3]
  
  greyscale_calculation <- round((R * 0.299) + (G * 0.587) + (B * 0.114))
  
  return(greyscale_calculation)
}

# Create a function to apply the grey scale filter to each pixel in the image
# Dimensions of the bitmap image
dim(my_bitmap) # Coded as 1) color channels, 2) width, and 3) height

apply_greyscale <- function(image, function_apply_grey) {
  
  width <- dim(my_bitmap)[2]
  height <- dim(my_bitmap)[3]
  
  for (i in 1:height) {  # Loop through each row (height)
    for (j in 1:width) {  # Loop through each column (width)
      
      pixel <- as.numeric(image[, j, i]) # Access each pixel 
      
      greyscale_value <- function_apply_grey(pixel) # Pass each pixel to the grey scale function
      
      greyscale_value <- as.integer(round(greyscale_value)) 
      
      image[, j, i] <- as.raw(c(greyscale_value, greyscale_value, greyscale_value)) # Every pixel now gets a grey scale value
    }
  }
  return(image)
}

# Call the "application" function for the grey scale filter
my_bitmap_grey <- apply_greyscale(my_bitmap, function_greyscale)

# See if the function worked successfully 
final_grey_image <- image_read(my_bitmap_grey)
print(final_grey_image) # It did

# CUT-OFF FILTER

# Create the cut-off function that takes the grey scale value
function_cutoff <- function(greyscale_calculation) {
  
  cut_off <- 127 # The default cut-off
  
  if (greyscale_calculation >= cut_off) { 
    
    return(c(255, 255, 255))  # White color (equal to or above the cut-off)
    
  } else {
    
    return(c(0, 0, 0))  # Black color (below the cut-off)
  }
}

# Create a function to apply the cut-off filter to each pixel in the image
apply_cutoff <- function(image, greyscale_function, function_apply_cutoff) {
  
  width <- dim(image)[2]
  height <- dim(image)[3]
  
  for (i in 1:height) {  # Loop through each row (height)
    for (j in 1:width) {  # Loop through each column (width)
      
      pixel <- as.numeric(image[, j, i]) # Access each pixel
      
      greyscale_value <- greyscale_function(pixel) # Get the grey scale value
      
      cut_off_value <- function_apply_cutoff(greyscale_value) # Apply the cut-off function based on the grey scale value
      
      image[, j, i] <- as.raw(cut_off_value)
    }
  }
  return(image)  
}

# Call the "application" function for the cut-off filter
my_bitmap_with_cutoff <- apply_cutoff(my_bitmap, function_greyscale, function_cutoff)

# See if the function worked successfully 
final_cutoff_image <- image_read(my_bitmap_with_cutoff )
print(final_cutoff_image) # It did

