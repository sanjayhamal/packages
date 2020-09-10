#'Generate dataframe ia a package that will help to create predecessors
#'files along with empty assigned variables arranged in an order.
#'
#'A csv file containing information about variables from metadataviewer 
#'file is required.

#'First, load the csv file containing only variables from the metadaviewer file.

#'The first step is using: metadata() function.
#'syntax: metadata(metadata_file, create)

#'metadata_file: The csv file containing the information about varibles form
#'               metadataviewer file.
#'create       : The dataset you want to create(Example: 'adae')



#' Using function metadata(), you will be asked to import certain files from sdtm or adam.
#' Then, another function is used to create the dataset, The newly imported dataset from adam or sdtm should be
#' inserted in the function in a sequence stated by the program as a list.
#' 
#' The function to be used is: generate_dataframe(required_files = list(), previous_function_variable)


#' required_files            : After using the previous function, the program will ask to insert the files in the given sequence
#'                             and it should be inserted as a list: example: generate_dataframe(required_files = list(a, b, c, ..), )


#' previous_function_variable: It is the variable that you used for storing the metadata() function.

#' @export
metadata<- function(metadata_file, create){
  guide<- metadata_file
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
  for(i in seq(length(to_import))){
    if(main_file[1]!= to_import[i]){
      main_file[length(main_file)+i]= to_import[i]
    }
    
  }
  
  main_file<- main_file[!is.na(main_file)]
  print("After importing, Put the files as a list in this sequence in next function: generate_dataframe(required_files = list()) ")
  print(main_file)
  list1<- list(predecessor, assigned, main_file, guide1)
  return(list1)
}









