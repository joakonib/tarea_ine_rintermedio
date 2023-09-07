
### Ejercicio 1:descargar archivos-------
##extract_name para extraer el nombre
extract_name <- function(url){
  str_extract(url, pattern = "esi-\\d{4}---personas.csv")
}


###download esi data
download_esi_data <- function(url,directory,file_name){
  
  ruta <- file.path(directory,file_name)
  
  download.file(url, destfile = ruta)

}

### Ejercicio 2: leer archivos-------
read_esi_data <- function(path){
  
  separadores <-  c(",",";",":","\t")

  
###busco dentro de la base que archivos
  for (sep in separadores) {
    datos <- tryCatch(
      read.csv(path, sep = separadores),
      error = function(e) NULL
    )
    
##mensaje para decir que fueron cargados
    if (!is.null(datos)) {
      cat(paste0("Datos cargados correctamente de ", str_extract(path, "/(.*)"), "\n"))
      break
    }
  }
  
  ##mensaje de error
  if (!is.null(datos)) {
    return(datos)
    
  } else {
    stop(paste0("No se pudo leer el archivo ", str_extract(path, "/(.*)")))
  }
}


###mean con data.table
fun_media <- function(data, columna, group_var) {
  columna <- as.character(substitute(columna))
  # group_var <- as.character(substitute(group_var))
  data <- as.data.table(data)
  media <- data[, mean(eval(parse(text = columna)), na.rm = TRUE)] 
                # by = eval(parse(text = group_var))
  return(media)
}




