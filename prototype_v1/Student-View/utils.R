

grade_max <- function(x){
  max <- "NA"
  for (grade in x){
    if (max != "M" & grade == "M"){
      max <- "M"
    } else if (max != "J" & max != "M" & grade == "J"){
      max <- "J"
    } else if (max != "A" & max != "J" & max != "M" & grade == "A"){
      max <- "A"
    } 
  }
  return(max)
}

