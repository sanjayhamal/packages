#'Generate dataframe ia a package that will help to create predecessors
#'files along with empty assigned variables arranged in an order.

#' Libraries 'tidyverse', 'haven', 'readxl' are required for this package to work correctly.
#'The only thing required is the path of folder containing the metadataviewer file.
#'
#'The first step is using: metadata() function.
#'syntax: metadata(path_metadata_file, create)

#'path_metadata_file: It is the path or directory which contains the metadataviewer file. The extension is .xlsm
#'create       :      The dataset you want to create(Example: 'adae' or 'adsl' and so on)



#' Using function metadata(), you will be asked to import certain files from sdtm or adam.
#' Then, another function is used to create the dataset, The newly imported dataset from adam or sdtm should be
#' inserted in the function in a sequence stated by the program as a list.
#' 
#' The function to be used is: generate_dataframe(required_files = list(), previous_function_variable)


#' required_files            : After using the previous function, the program will ask to insert the files in the given sequence
#'                             and it should be inserted as a list: example: generate_dataframe(required_files = list(a, b, c, ..), )


#' previous_function         : It is the variable that you used for storing the metadata() function.

#' @export
generate_dataframe<- function(required_files, previous_function){
  predecessor = previous_function[[1]]
  assigned    = previous_function[[2]]
  main_file   = previous_function[[3]]
  guide1      = previous_function[[4]]
  
  Completed = predecessor
  To_complete = assigned
  View(Completed)
  View(To_complete)
  
  first_step<- predecessor %>% filter(source==main_file[1])
  first_step<- first_step %>% select(VARIABLE, LABEL, variables)
  
  
  final_file<- required_files[[1]]
  final_file
  
  
  for(i in seq(nrow(first_step))){
    if (first_step[i, "VARIABLE"]!= first_step[i, "variables"]){
      
      old1<- first_step[i, "variables"]
      new1<- first_step[i, "VARIABLE"]
      final_file[, new1] = final_file[, old1]
      
    }
  }
  
  req_vars<- c(first_step$VARIABLE)
  
  ultimate_file<-final_file %>% select(as.character(req_vars))
  #View(ultimate_file)
  if (length(required_files)>1){
    
    
    for(i in seq(2, length(required_files))){
      
      use_file<- required_files[[i]]
      source_file<- main_file[i]
      next_steps<- predecessor %>% filter(source==source_file)
      next_steps<- next_steps %>% select(VARIABLE, LABEL, variables)
      
      
      for(i in seq(nrow(next_steps))){
        
        if (next_steps[i, "VARIABLE"]!= next_steps[i, "variables"]){
          
          old1<- next_steps[i, "variables"]
          new1<- next_steps[i, "VARIABLE"]
          use_file[, new1] = use_file[, old1]
          
        }
        
      }  
      
      req_vars<- c(next_steps$VARIABLE)
      use_file<- use_file %>% select(USUBJID, as.character(req_vars))
      
      ultimate_file<- ultimate_file %>% left_join(use_file, by = 'USUBJID')
      
      
    }
    
  }
  #finally working on assigned variables:
  to_add_var<- c(assigned$VARIABLE)
  for ( i in seq(length(to_add_var))){
    ultimate_file[to_add_var[i]] = ""
  }
  
  #maintaing order of the variables:
  ordr<- c(guide1$VARIABLE)
  ultimate_file<- ultimate_file %>% select(as.character(ordr))
  
  return(ultimate_file)
  
}
