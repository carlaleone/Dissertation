# Splitting .wav files before calculating the acoustic indices
# modification of code from Issy Key edited by Carla Leone
# Output is 1-minute long recordings every 10 minutes

## Load and install packages ---- 
install.packages("soundecology")
install.packages("seewave")
install.packages("tuneR")

library(soundecology)
library(tuneR)
library(seewave)

## Set the working directory ----
getwd()
audiofiles = "C:/Users/frleo/OneDrive/Desktop/Index/Recordings" #this is the local working directory where all my files are stored
setwd(audiofiles)

## Create a list of the sound files
wav_list <- list.files( pattern = ".WAV") #"." means the path defaults to the current working directory

## Create a folder for the output of the split files
output_folder <- file.path(audiofiles, "Split")
dir.create(output_folder, showWarnings = TRUE) #if can't see the folder created already, repeat the action and it will say it already exists, if it worked the first time

#### CUTTING FILES ####

start <- Sys.time() #Time how long this operation takes

for(i in seq_along(wav_list)) {
  
  audio <- readWave(wav_list[i])
  
  #Downsample the audio if desired
  # # ***CHECK WITH EACH RUN***
  audio <- downsample(audio,samp.rate=18000) #Experiment with different frequencies to downsample to - this decreases the time for the code to run
  
  # Define the duration of each section in seconds
  section_duration <- 60
  
  # Calculate the total duration of the input audio
  total_duration <- length(audio) / audio@samp.rate  # Duration in seconds
  
  # Calculate the number of 60-second sections
  num_sections <- ceiling(total_duration / section_duration)
  cat("File", i, "cut into", num_sections, "1 minute sections\n")
  
  # Extract the name of the folder above the subfolder (two levels up) - I am doing this because the subfolder name is included in the filename already
  parent_folder_name <- basename(dirname(dirname(wav_list[i])))
  
  # Loop to cut the audio into 60-second sections
  for (j in 1:num_sections) {
    
    #Only save every 5th cut audio file
    if (j %% 20 == 0) { 
      
      # Calculate the start and end times for the current section
      start_time <- (j - 1) * section_duration
      end_time <- min(j * section_duration, total_duration)
      
      # Cut the audio for the current section
      section_audio <- cutw(audio, from = start_time, to = end_time, output = "Wave")
      
      # Create an output file name (caused problems before because there was a double .wav extension in the file name)
      output_file_name <- file.path(output_folder, paste0(parent_folder_name, "_", basename(wav_list[i]), "_", j, ".WAV"))
      
      # Write the current section to a new WAV file
      writeWave(section_audio, output_file_name)
    }
  }
}
end <- Sys.time()
duration <- end - start
duration

#in WINDOWS save the output into the outpur folder ----
shell.exec(output_folder)

#This worked and below is the file path

#"C:\Users\frleo\OneDrive\Desktop\Index\Recordings\Split\._20220626_000004.WAV_20.WAV"
