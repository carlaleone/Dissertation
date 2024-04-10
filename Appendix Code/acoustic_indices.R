# Calculating Acoustic Indices for 1-minute long recordings
# Code from Ben Williams, modified by Carla Leone
# 28/12/2023

## Install and download packages ----
install.packages("seewave")
install.packages("soundecology")
install.packages("tuneR")

library(seewave)
library(soundecology)
library(tuneR)

install.packages("tidyverse")
library(tidyverse)

## Put all your .wav files you wish to analyse into one folder and prepare another folder for the results ----
#new working directory
#"C:\Users\frleo\OneDrive - University of Edinburgh\Index\Recordings"

folder_with_recordings = "C:/Users/frleo/OneDrive - University of Edinburgh/Dissertation/Index/20220626_061255/Split"
#folder where data is stored. Rememer to use forward slashes for file path.

folder_for_results = "C:/Users/frleo/OneDrive - University of Edinburgh/Dissertation/Index/20220626_061255/Split/Results" #output folder
dir.create(folder_for_results, showWarnings = TRUE)
outputfile_name = "results_jan6" #name to give your output file (the time and date will also be added)
sample_rate = 18000 #all sound measured in phonic richness was at or under 18 kHz, as anything above this was likely masked by self noise of the Hydromoth.

#Set the frequency bands you wish to work with. Splitting into three groups: low, high, full
#additional bands can be added here and in the loop below if desired
lowband1 = 50   # Based on observation, lowest fish sounds recorded
lowband2 = 1000   # Based on observation and literature for fish sounds

highband1 = 1000   
highband2 = 18000   

fullband1 = 50   
fullband2 = 18000   


#do not change these two lines
getwd()
setwd(folder_with_recordings)
path = folder_with_recordings



### Are you running on all audio files within a folder? The use this:

wav_files = dir(path, pattern =".WAV") #may need to change '.wav' to upper or lowercase


### Are you running on a subset of files in the folder? Then use these lines if executing over files specified by suffix:

#vector_of_files = file_name_ends[,1] #Reads the list of applicable suffixes
#wav_files = vector() #Creates empty vector
#for(i in 1:length(vector_of_files)){
#  wav_files = c(wav_files, dir(path, pattern = vector_of_files[i])) #Adds files to the empty vector
#}
#print(length(wav_files)) #tells you how many eligible files in the folder, check this appears correct

  
  


########################################################################

#create vectors to store results
filenames = vector() #always need this
ACI_low = vector()
H_low = vector()
ACI_high = vector()
H_high = vector()
BI_low = vector()
BI_high = vector()
TE_low = vector()
TE_high = vector()
SE_low = vector()
SE_high = vector()
M_low = vector()
M_high = vector()
AEI_low = vector()
AEI_high = vector()

ACI_full = vector()
H_full = vector()
BI_full = vector()
TE_full = vector()
SE_full = vector()
M_full = vector()
AEI_full = vector()

NDSI = vector()

#these lines track progress of the analysis
counter = 0
finish = length(wav_files)
started = format(Sys.time(), "%H.%M")

#Main loop to bandpass filter tracks and calculate indices ---- 
#This may error if iterating over several hundred files. Use:'for( i in 1:200){', then: 'for( i in 200:400){' and so on to split into smaller chunks
for( i in 1:finish){
  filenames = c(filenames,(wav_files[i])) #add the file name to empty list
  load_file = readWave(wav_files[i]) #select the wave file being iterated upon
  
  #This filters the track for seewave indices. Soundecology indices do it within the function. Default settings used.
  #Low freq filter
  low_file = ffilter(load_file, f = sample_rate, channel = 1, from = lowband1, to = lowband2, bandpass = TRUE,
                     custom = NULL, wl = 1024, ovlp = 75, wn = "hanning", fftw = FALSE,
                     rescale = FALSE, listen = FALSE, output="Wave")
  
  #High freq filter
  high_file = ffilter(load_file, f = sample_rate, channel = 1, from = highband1, to = highband2, bandpass = TRUE,
                      custom = NULL, wl = 1024, ovlp = 75, wn = "hanning", fftw = FALSE,
                      rescale = FALSE, listen = FALSE, output="Wave")
  
  #full freq filter
  full_file = ffilter(load_file, f = sample_rate, channel = 1, from = fullband1, to = fullband2, bandpass = TRUE,
                      custom = NULL, wl = 1024, ovlp = 75, wn = "hanning", fftw = FALSE,
                      rescale = FALSE, listen = FALSE, output="Wave")
  
  
  #Calculate the indices using the filtered tracks
  #Acoustic complexity
  ACIvalues1 = ACI(wave = low_file, f = sample_rate)
  ACI_low = c(ACI_low, ACIvalues1) #add to the list created earlier
  
  ACIvalues2 = ACI(wave = high_file, f = sample_rate)
  ACI_high = c(ACI_high, ACIvalues2) #add to the list created earlier
  
  ACIvalues2 = ACI(wave = full_file, f = sample_rate)
  ACI_full = c(ACI_full, ACIvalues2) #add to the list created earlier
  
  #Acoustic entropy
  Hvalues1 = H(wave = low_file, f = sample_rate)
  H_low = c(H_low, Hvalues1) #add to the list created earlier
  
  Hvalues2 = H(wave = high_file, f = sample_rate)
  H_high = c(H_high, Hvalues2) #add to the list created earlier
  
  Hvalues2 = H(wave = full_file, f = sample_rate)
  H_full = c(H_full, Hvalues2) #add to the list created earlier
  
  #Bioacoustic index
  BIvalues1 = bioacoustic_index(load_file, min_freq = lowband1, max_freq = lowband2)
  BI_low = c(BI_low, BIvalues1$left_area) #add to the list created earlier
  
  BIvalues2 = bioacoustic_index(load_file, min_freq = highband1, max_freq = highband2)
  BI_high = c(BI_high, BIvalues2$left_area) #add to the list created earlier
  
  BIvalues2 = bioacoustic_index(load_file, min_freq = fullband1, max_freq = fullband2)
  BI_full = c(BI_full, BIvalues2$left_area) #add to the list created earlier
  
  #Temporal entropy
  envelope = env(wav = low_file, f = sample_rate, plot = FALSE)
  TEvalues = th(envelope)
  TE_low = c(TE_low, TEvalues) #add to the list created earlier
  
  #Temporal entropy
  envelope = env(wav = high_file, f = sample_rate, plot = FALSE)
  TEvalues = th(envelope)
  TE_high = c(TE_high, TEvalues) #add to the list created earlier
  
  #Temporal entropy
  envelope = env(wav = full_file, f = sample_rate, plot = FALSE)
  TEvalues = th(envelope)
  TE_full = c(TE_full, TEvalues) #add to the list created earlier
  
  #Spectral entropy
  freqspec = meanspec(wave = low_file, f = sample_rate, plot = FALSE)
  SEvalues = sh(freqspec)
  SE_low = c(SE_low, SEvalues)
  
  #Spectral entropy
  freqspec = meanspec(wave = high_file, f = sample_rate, plot = FALSE)
  SEvalues = sh(freqspec)
  SE_high = c(SE_high, SEvalues)
  
  #Spectral entropy
  freqspec = meanspec(wave = full_file, f = sample_rate, plot = FALSE)
  SEvalues = sh(freqspec)
  SE_full = c(SE_full, SEvalues)
  
  #Amplitude (use SPL for soundtraps, reserve this for moths)
  Mvalues = M(wave = low_file, f = sample_rate)
  M_low = c(M_low, Mvalues)
  
  #Amplitude (use SPL for soundtraps, reserve this for moths)
  Mvalues = M(wave = high_file, f = sample_rate)
  M_high = c(M_high, Mvalues)
  
  #Amplitude (use SPL for soundtraps, reserve this for moths)
  Mvalues = M(wave = full_file, f = sample_rate)
  M_full = c(M_full, Mvalues)
  
  #AEI
  band_size = (lowband2 - lowband1)/10 #uses 10 freq bands, adjust accordingly
  AEIvalues = acoustic_evenness(low_file, max_freq = lowband2, freq_step = band_size) #run on default settings as used in Villanueva-Rivera et al. 2011
  AEI_low = c(AEI_low , AEIvalues$aei_left)
  
  #AEI
  band_size = (highband2 - highband1)/10 #uses 10 freq bands, adjust accordingly
  AEIvalues = acoustic_evenness(high_file, max_freq = highband2, freq_step = band_size) #run on default settings as used in Villanueva-Rivera et al. 2011
  AEI_high = c(AEI_high , AEIvalues$aei_left)
  
  #AEI
  band_size = (fullband2 - fullband1)/10 #uses 10 freq bands, adjust accordingly
  AEIvalues = acoustic_evenness(full_file, max_freq = fullband2, freq_step = band_size) #run on default settings as used in Villanueva-Rivera et al. 2011
  AEI_full = c(AEI_full , AEIvalues$aei_left)
  
  #NDSI
  soundspec = soundscapespec(wave = load_file, f = sample_rate, wl = 1024, wn = "hamming", ovlp = 50, plot = TRUE, xlab = "Frequency (kHz)", ylim = c(0, 1), main = wav_files[i])
  NDSIvalues = NDSI(soundspec, anthropophony = 1, biophony = 2:5) #here anthropophony = fishband, biophony = shrimp band
  NDSI = c(NDSI, NDSIvalues) 
  
  counter = counter + 1
  print(paste((counter/finish)*100, "% complete"))
  print(paste("Script started at: ", started))
  
}

#collate and save results to csv in the results folder
results_table = data.frame( wav_files, ACI_low, ACI_high, ACI_full, AEI_low, AEI_high, AEI_full, BI_low, BI_high, BI_full, H_low, H_high, 
                           H_full, TE_low, TE_high, TE_full, SE_low, SE_high, SE_full, M_low, M_high, M_full, NDSI)
csvname = paste0(outputfile_name, format(Sys.time(), " %d-%b-%Y %H.%M.%S"), ".csv")
setwd(folder_for_results)
path = folder_for_results
write.csv(results_table, csvname)

#Does a 'non-conformabl arguments' error occur 'arguments imply differing number of rows here: x n'?
#This is likely due to a corrupted file. Remove the nth and nth +/- 1 file from the folder and try again.
#You can retrieve the name of the nth file using: print(wav_files[n]) then navigate to this in your folder


#troubleshooting, sometimes the column length for .wav files and number of indices calculated does not match. Needed to re-run the code.
length(wav_files)
sum(is.na(wav_files))

wav.files.data = data.frame(wav_files)
wav.files.data$wav_files <- sub("\\.WAV$", "", wav.files.data$wav_files)

View(wav.files.data)
write.csv(wav.files.data, file = "C:/Users/frleo/OneDrive - University of Edinburgh/Dissertation/Index/Split_all/Results_jan3", ifelse(append, "a", "w"))
