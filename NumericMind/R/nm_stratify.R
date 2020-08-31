#' stratifying function:
#' for stratify
#' remember that while stratifying the summarized table, the values in the columns of summarized table that we are stratifying should occur in equal frequencies. i.e values in column a must occur same as the adjacent values in column b.

#' syntax:
#' nm_stratify<- function(unsummarized_table = , summarized_table = , stratify_by = " ", stratify= c(" ", " ",))
#'
#'
#' unsummarized_table: It is the table that is used for creating a normal summary table.
#' summarized_table:   It is the summary table created by help of unsummarized_table using nm_table() function.
#' stratify_by:        It is the varibale by which you want to categorize.
#' stratify:           It is the collection of variables that you want to stratify: c(" ", " ")
#' @export


nm_stratify<- function(unsummarized_table, summarized_table, stratify_by, stratify){


  df_new<- data.frame(xyz = "")

  names(df_new)[1]<- "stf_by"
  for(i in seq(length(stratify))){
    df_new[stratify[i]] = ""

  }


  df_new
  summarized_table
  #unsummarized_table<- na.omit(unsummarized_table)
  unsummarized_table

  whole_column<- vector()
  whole_column[1]<- stratify_by
  for(i in seq(length(stratify))){
    whole_column[i+1]<- stratify[i]
  }
  whole_column

  unsummarized_table<- unsummarized_table %>% select(as.character(whole_column))
  unsummarized_table

  unsummarized_table<-unsummarized_table %>% rename('stf_by'= stratify_by)

  unsummarized_table
  stf_var<- unique(c(unsummarized_table$stf_by))
  stf_var<- sort(stf_var)
  stf_var

  for(i in seq(length(stf_var))){
    stf1<- unsummarized_table %>% filter(stf_by==stf_var[i]) %>%group_by_all() %>% distinct() %>% ungroup()
    stf1$stf_by= ""
    stf1<- stf1 %>% add_row('stf_by' = stf_var[i], .before = 1)
    stf1<- stf1 %>% add_row('stf_by' = "", .after = nrow(stf1))
    df_new<- bind_rows(df_new, stf1)
  }

  df_new$join_id= ""
  df_new[is.na(df_new)]<- ""
  for( i in seq(nrow(df_new))){
    if (df_new[i,1]!= ""){
      df_new[i, "join_id"] = df_new[i,1]
      df_new[i,2] = 'Subtotal'
    }
    else{
      df_new[i, "join_id"] = df_new[i,2]
    }
  }


  names(summarized_table)[1]<- 'join_id'
  summarized_table$join_id = trimws(summarized_table$join_id)

  df_new2<-df_new %>% left_join(summarized_table, by = 'join_id')
  names(df_new2)[1]<- stratify_by
  df_new2<-df_new2[-c(1,2, nrow(df_new2), nrow(df_new2)-1), ]
  df_new2<-df_new2 %>% select(-join_id)

  return(df_new2)




}
