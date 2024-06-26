% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_nirs.R
\name{import_nirs}
\alias{import_nirs}
\title{Import NIRS data}
\usage{
import_nirs(nirFile, folder = NULL)
}
\arguments{
\item{nirFile}{(STRING) Path to the .nir file.}

\item{folder}{(STRING: optional) Path to the folder containing the .nir file (ending with /). If you specify the folder, you do not need to include the folder path in the nirFile parameter. This can be useful if you have multiple files in the same folder, and can be combined with lapply to import multiple files at once (see example).}
}
\value{
A dataframe with the data in a long format with the following columns:
\itemize{
\item t: time in seconds from the start of the recording. You will see that this matches the values of the first column in COBI Studio .nir file.
\item startTime: the start time of the recording. This is taken from the .nir file.
\item fileName: the name of the file (full path)
\item dataRow: the row number of the data as it is in the original .nir file. The first row of the data is the first row after the baseline data ends in the original .nir file.
\item optode: the optode number (e.g., 1-16)
\item freq: the frequency (e.g., 730, 850)
\item nirValue: the value of the nir data (light intensity) for that optode and frequency at that time point. This matches the values in the original .nir file.
\item baselineValue: the baseline value for that optode and frequency. This is taken from the 10 second baseline period at the start of the recording, which was stored in the original .nir file.
}
}
\description{
This function will import a .nir file, identify the baseline values and return a dataframe with the NIRS data in a long format.
}
\details{
This function will import a .nir file, identify the baseline values and return a dataframe with the data in a long format.
}
\examples{
\dontrun{

# Import a single .nir file

nirsData <- import_nirs("path/to/nir/file.nir")

# Import multiple .nir files

# path should be the folder where the file is, ending in /

 myPath <- "C:.../fnirs_data/"

#list all of the nirs files in a directory

nirsFiles <- list.files(pattern = ".nir$", recursive = TRUE, path = myPath)

# import all of the nirs files

nirsData <- lapply(nirsFiles, import_nirs, folder = myPath)

# base the number of participants on the number of files in the directory
# this is useful for later analysis

nparticipants <- length(nirsFiles)


# add a participant id as the name of each dataframe in the data list

names(nirsData) <- c(paste("p",pids, sep = ""))

# add the participant id column to each of the dataframes in the data list,
# so we can identify each dataset if they are combined later

nirsData <- mapply(`[<-`, nirsData, 'npid', value = pids, SIMPLIFY = FALSE)

}
}
