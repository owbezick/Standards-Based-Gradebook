#Color Scheme
theme <- tibble("NC" = "#dbdbad"
                  , "Needs Work" = "#a1dab4" 
                  , "Progressing" = "#41b6c4"
                  , "Fluent" = "#225ea8")

#returns integer representation of fluency grade
grade_to_int <- function(grade){
  if (grade == "Fluent"){
    return(3)
  }
  else if (grade == "Progressing"){
    return(2)
  }
  else if (grade == "Needs Work"){
    return(1)
  }
  else {
    return(0)
  }
}

#returns string representation of fluency grade integer
int_to_grade <- function(grade){
  if (grade == 3){
    return("Fluent")
  }
  else if (grade == 2){
    return("Progressing")
  }
  else if (grade == 1){
    return("Needs Work")
  }
  else {
    return("NC")
  }
}

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

