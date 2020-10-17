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
metadata<- function(path_metadata_file, create){
  guide<- read_excel(path = path_metadata_file, sheet = 'Variables')
  guide<- as.data.frame(guide)
  to_create = create
  guide1<- guide %>% filter(DATASET==to_create)
  predecessor<- guide1 %>% filter(SOURCE_VAR!="" & is.na(SOURCE_VAR)==F & lengths(strsplit(SOURCE_VAR, "\\W+"))<3)
  
  assigned<- guide1 %>% filter(SOURCE_VAR=="" | is.na(SOURCE_VAR)==T | lengths(strsplit(SOURCE_VAR, "\\W+"))>2)
  predecessor<- predecessor %>% separate(SOURCE_VAR, c('source', 'variables'), sep = "\\.")
  #View(predecessor)
  to_import<- unique(c(predecessor$source))
  print("Import these files from sdtm or adam folder: " )
  print(to_import)
  

  main_file<- predecessor %>% filter(variables=='USUBJID')
  main_file<- unique(c(main_file$source))

  ### use apply functions instead
  for(i in seq(length(to_import))){
    if(main_file[1]!= to_import[i]){
      main_file[length(main_file)+i]= to_import[i]
    }
    
  }
  
  main_file<- main_file[!is.na(main_file)]
  print("After importing, Put the files as a list in this sequence in next function: generate_dataframe(required_files = list(), previous_function= __) ")
  print(main_file)
  list1<- list(predecessor, assigned, main_file, guide1)
  return(list1)
}







