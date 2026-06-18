# Join all files created on calc.raster into one big file ####
join.files <- function(folder.calc.raster,
                       new.folder,
                       output.name,
                       column.order = NULL){

  # Dependencies ####
  require(dplyr)


  # Function itself ####
  # Pull and organize files
  # Which files in the folder has the format defined by the function
  # Quais arquivos existem no diretorio do formato definido pela função
  files.names <- list.files(folder.calc.raster,
                            pattern = paste0("\\", ".txt", "$"),
                            full.names = T)

  # Use lapply to get all these files
  # Fazer um lapply pra pegar todos esses arquivos
  files.list <- lapply(files.names, read.table,
                       header = TRUE, sep = "\t", dec = ".",
                       fileEncoding = "UTF-8", check.names = FALSE)

  # Change the list to a single data.frame with all the columns
  files.df <- dplyr::bind_rows(files.list)

  # Order the columns
  files.df <- files.df[,order(as.numeric(colnames(files.df)))]

  # Take away the rownames
  rownames(files.df) <- NULL

  # Save file
  write.table(x = files.df,
              file = paste0(new.folder, output.name),
              quote = FALSE, sep = "\t", dec = ".", row.names = FALSE,
              fileEncoding = "UTF-8")


  # Return ####
  # Return the data.frame
  return(arq_final)
}
