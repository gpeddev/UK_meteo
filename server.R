library(shiny)
library(maps)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(rmarkdown)
library(rlang)

source("helper.R", local = knitr::knit_global())
# Global variables

# sites is loaded once at the beginning of shiny app. 
# sites contains the coordinates for the site station, their name(Site_Name) 
# and their id (Site_ID)

sites<-read_csv("./Data/Sites.csv") 

shinyServer(function(input, output) {
    
    # Variables from ui.R
    #   -input$stations contains the selected stations
    #   -input$weather contains the weather variable
    #   -input$aggregation contains the aggregation variable
    #   -input$time contains the x-axis
    #   -input$hutton_station contains the station to check for hutton effect
    #   -input$month_hutton selects the month
    
    # Reactive
    #   -selected_stations()
    #   -loaded_files_for_graph()
    #   -loaded_file2()
    
    # Output
    #   -output$downloadTable<-downloadHandler
    #   -output$summary_table<-renderDataTable
    #   -output$timeaxis<-renderUI
    #   -output$map<-renderPlot
    #   -output$Graph<-renderPlot
    
    ################################################################################
    
    
    #return the selected station rows
    selected_stations<-reactive({
        if(!is_empty(input$stations))
            return(filter(sites,Site_Name%in%input$stations))
    })
    
    # load required data files that contains only the necessary columns
    loaded_files_for_graph<-reactive({
        loaded_files_for_summary()%>%
            select(c("ob_time","hour","day","month",input$weather,"Site_Name"))
    })
    
    # load required data files that contains all weather variables columns
    loaded_files_for_summary<-reactive({
        temp<-selected_stations()[,1]
        site_name_first<-paste("./Data/Site_",temp[[1,1]],".csv",sep="")
        first_tibble<-homogenize_tibble(site_name_first)
        if (nrow(temp)>1){
            for (i in 2:nrow(temp)){
                site_name_next<-paste("./Data/Site_",temp[[i,1]],".csv",sep="")
                next_tibble<-homogenize_tibble(site_name_next)
                first_tibble<-rbind(first_tibble,next_tibble)
            }
        }
        first_tibble<-first_tibble%>% 
            inner_join(sites,by=c("Site"="Site_ID"))
        first_tibble
    })
    
    load_files_for_hutton<-reactive({
        if(!is_empty(input$hutton_station))
            site_id<-filter(sites,Site_Name==input$hutton_station)
        filename<-paste("./Data/Site_",site_id[[1,1]],".csv",sep="")
        site_tibble<-homogenize_tibble(filename)
        site_tibble%>%select(c(ob_time,air_temperature,rltv_hum))
        
    })
    
    ################################################################################

    #Download summary table
    output$downloadTable<-downloadHandler(
        filename = function(){
            "dataTable.csv"
        },
        content = function(file){
            write.csv(give_table(loaded_files_for_summary()),file,row.names = FALSE)
        }
    )
    #Download report
    output$downloadReport <- downloadHandler(
        filename = "Report.docx",
        content = function(file) {
            render("Report.Rmd", output_format="word_document",
                   output_file=file, params=list(
                                       ldf_summary = loaded_files_for_summary(),
                                       ldf_graph = loaded_files_for_graph(),
                                       time = input$time,
                                       weather = input$weather
                                   ))

        }
    )
    
    #Show summary table
    output$summary_table<-renderDataTable({
        give_table(loaded_files_for_summary())
    }
    )
    
    #Select based on aggregation x-axis options
    output$timeaxis<-renderUI({ 
        if (input$aggregation=="Raw hourly data")
            opt<-c("Week hours"=1,"24 hours"=2)
        else 
            if (input$aggregation=="Daily averages")
                opt<-c("Calendar time"=3,"Week days"=4)
            else 
                if (input$aggregation=="Monthly averages")
                    opt<-c("Calendar time"=5)
                else 
                    if (input$aggregation=="Daily maxima")
                        opt<-c("Calendar time"=6,"Week days"=7)
                    else
                        if (input$aggregation=="Daily minima")
                            opt<-c("Calendar time"=8,"Week days"=9)
                        
                        radioButtons("time","Choose x axis",opt)
    })
    
    output$pickMonth<-renderUI({
        if(!is_empty(input$stations))
            temp<-filter(sites,Site_Name%in%input$hutton_station)
        temp<-temp[,1]
        site_name<-paste("./Data/Site_",temp[[1,1]],".csv",sep="")
        first_tibble<-homogenize_tibble(site_name)
        first_tibble<-first_tibble%>% 
            inner_join(sites,by=c("Site"="Site_ID"))
        first_tibble<-first_tibble[,1]
        first_tibble<-mutate(first_tibble,ob_time=month(ob_time))
        opt<-unlist(unique(first_tibble),use.names = FALSE)
        #radioButtons("month_hutton","Choose the month you want to view",opt)
        selectizeInput("month_hutton",
                       "Choose the month you want to view", 
                       opt)
    })
    
    # The basic ideas behind hutton criteria checks
    # 
    # We make a column for each criteria with true or false
    # We check previous days if the criteria are valid by
    # adding 1 or 2 days to the date. (That's like moving the column with
    # true false 1 or 2 days lower)
    # Afterwards checking the same raw give us the result
    #
    output$hutton_table<-renderDataTable({
        base<-load_files_for_hutton()
        
        # Relative humidity check
        base_hum<-base%>%
            mutate(humidity_over_90=(rltv_hum>=90))%>%      #give true or false to every hour depending if it has rltv_hum >=90
            select(c(ob_time,humidity_over_90))%>%          #select columns with datetime and humidity_over_90
            mutate(ob_time=as.Date(ob_time))%>%             #change datetime to date
            group_by(ob_time)%>%                            #group by date
            summarise(humidity_over_90=sum(humidity_over_90))%>%    #for each date summarize TRUE or FALSE (1 or 0)
            mutate(humidity_over_90=humidity_over_90>=6)            #put TRUE or FALSE if each date has over 90% humidity
        
        base_hum_prev1<-base_hum%>%mutate(ob_time=ob_time+1)                   #move dates one day forward
        base_hum_prev2<-base_hum%>%mutate(ob_time=ob_time+2)                   #move dates two days forward
        
        base_hum_prev1<-rename(base_hum_prev1,hut_hum_1=humidity_over_90)        #rename column
        base_hum_prev2<-rename(base_hum_prev2,hut_hum_2=humidity_over_90)        #rename column
        
        final_hum<-inner_join(base_hum_prev1,base_hum_prev2)%>%     #we align rows by date so we align previous 1 and 2 days
            mutate(hutton_criteria_hum=hut_hum_1 %and% hut_hum_2)%>%    #if true then the previous 2 satisfied the hutton humidity criteria
            select(c(ob_time,hutton_criteria_hum))                  #select necessary columns
        
        #Minimum time criteria
        base_temp<-base%>%
            select(c(ob_time,air_temperature))%>%       #select only datetime and air temperature variable
            mutate(ob_time=as.Date(ob_time))%>%         #transform date time to date
            group_by(ob_time)%>%                        #group by date
            summarise(min_air_temperature=min(air_temperature)>=10)      #creates a column that has TRUE or FALSE if minimum temp>=10
        
        
        base_temp_prev1<-base_temp%>%mutate(ob_time=ob_time+1)   #move date one day forward
        base_temp_prev2<-base_temp%>%mutate(ob_time=ob_time+2)   #move date two days forward
        
        base_temp_prev1<-rename(base_temp_prev1,min_temp_1=min_air_temperature)     #rename column
        base_temp_prev2<-rename(base_temp_prev2,min_temp_2=min_air_temperature)     #rename column
        
        final_temp<-inner_join(base_temp_prev1,base_temp_prev2)%>%      #align rows by date
            mutate(hutton_criteria_temp=min_temp_1 %and% min_temp_2)%>%        #if true then the previous 1 and 2 days satisfy hutton temp criteria
            select(c(ob_time,hutton_criteria_temp))
        
        #final combination and result
        result<-inner_join(final_hum,final_temp)%>%                             #combine to get result
            mutate(result=hutton_criteria_temp %and% hutton_criteria_hum)%>%
            select(c(ob_time,result))
        result<-rename(result,"Hutton"=result)
        #filter date
        if(!is_empty(input$month_hutton))
            result<-result%>%filter(month(ob_time)==input$month_hutton)
        result
        
    }
    
    )
    

    
    #show map
    output$map<-renderPlot({
        maps::map("world","UK")
        points(unlist(selected_stations()[,4]),unlist(selected_stations()[,3]),pch=16,col="red")
    })
    
    #graph
    output$Graph<-renderPlot({
        temp<-loaded_files_for_graph()
        time<-input$time
        weather<-input$weather
        draw_plots(temp,time,weather)
        
    })
    
    
})