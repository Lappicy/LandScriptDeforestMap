# Close conections that might be using too much RAM temporary memory in R ####
close.geo.connections <- function(OS = "Windows"){

  # Dependencies ####
  # This function runs on base R only


  # Function itself ####
  # If the OS is not windows, it might not run.. so don't run it
  if(!(toupper(OS) %in% c("WINDOWS", "WINDOW"))) stop("Only runs on Windows for now")

  # Close all the connections
  closeAllConnections()

  # Erase the temp files
  # Windows Environmental variable %USERPROFILE%\AppData\Local\Temp
  PCTempDir <- Sys.getenv("TEMP")

  # Detect and delete folders with pattern "Rtmp"
  folders <- dir(PCTempDir, pattern = "Rtmp", full.names = TRUE)
  unlink(folders, recursive = TRUE, force = TRUE, expand = TRUE)


  # Return (NULL) ####
  return(NULL)
}
