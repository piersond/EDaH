

###################################
### Prep variables for server use

# Work from a copy of the database
app_data <- app_db

# Columns to remove from the database (not needed in the app)
drop_cols <- c('google_id', 'addit_contact_email', 'addit_contact_person', 'author_email', 'author_orcid_id',
               'author_PersonName', 'coarse_tot', 'curator_email', 'curator_organization', 'curator_PersonName', 'data_file',
               'experiments', 'gradient', 'header_row', 'key_version', 'location_name', 'merge_align', 'modification_date',
               'NA_1', 'NA_2', 'network', 'site_code', 'time_series', 'sample_collector') 

######################################
### SERVER STARTS HERE ###
function(input, output, session) {

  ### Data Explorer Table Starts Here ###  
  output$databaseTBL <- DT::renderDataTable({
    df <- app_data %>% 
      .[,setdiff(names(.),drop_cols)] %>%
      
      mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '" data-zip="', uniqueID, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    DT::datatable(df, 
                  options = list(ajax = list(url = action), 
                                 lengthMenu = c(10, 50, 100), 
                                 pageLength = 50,
                                 columnDefs = list(list(
                                   targets = 0:1, visible= TRUE)), 
                                 buttons = c('colvis'), dom = 'Bfrtip'),
                  extensions = 'Buttons', 
                  escape = FALSE, 
                  class = "display nowrap")
  })
  
}

