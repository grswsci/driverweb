getstrsplit <- function(data,whatstr = "_",whatcount = 1){
  varible = sapply(strsplit(data,whatstr,fixed = TRUE) ,'[',whatcount)
  return(varible)
}
