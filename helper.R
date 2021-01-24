library(shiny)
library(maps)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(rmarkdown)
library(rlang)
################################################################################
# Helper functions
#   -homogenize_tibble(file_name)
#   -give_title(parenthesis)
#   -give_ylabel(ylabel)
#   -give_table()
#   -draw_plots()


`%and%` <- function(x, y){
  as.logical(x*y)
}

# this function homogenize tibble
# reads csv assigning value types to columns
# and returns a tibble.
homogenize_tibble<-function(file_name){
  #forces same column type
  temp<-read_csv(file_name,col_types=cols(ob_time=col_character(),
                                          hour=col_integer(),
                                          day=col_integer(),
                                          month=col_integer(),
                                          wind_speed=col_integer(),
                                          air_temperature=col_double(),
                                          rltv_hum=col_double(),
                                          visibility=col_integer(),
                                          Site=col_integer()))
  #change date time (cause of site_4)
  temp<-temp %>%
      mutate(ob_time=parse_date_time(ob_time,orders=c("YmdHMS","dmYHM")))
  as_tibble(temp)
}

give_hutton_tibble<-function(name){
  sites<-read_csv("./Data/Sites.csv") 
  first_tibble<-homogenize_tibble(name)
  first_tibble<-first_tibble%>% 
    inner_join(sites,by=c("Site"="Site_ID"))
  first_tibble%<>%select(c(ob_time,air_temperature,rltv_hum))
  
}


give_title<-function(parenthesis,weather){
  if (weather=="wind_speed"){
    x=paste("Wind speed"," (",parenthesis,")",sep="")
  } else {
    if (weather=="air_temperature"){
      x=paste("Air temperature"," (",parenthesis,")",sep="")
    } else {
      if (weather=="rltv_hum"){
        x=paste("Relative Humidity"," (",parenthesis,")",sep="")
      } else {
        if (weather=="visibility"){
          x=paste("Visibility"," (",parenthesis,")",sep="")
        }
      }
    }
  }
}

give_ylabel<-function(ylabel,weather){
  if (weather=="wind_speed"){
    x=paste(ylabel,"wind speed",sep=" ")
  } else {
    if (weather=="air_temperature"){
      x=paste(ylabel,"air temperature",sep=" ")
    } else {
      if (weather=="rltv_hum"){
        x=paste(ylabel,"relative humidity",sep=" ")
      } else {
        if (weather=="visibility"){
          x=paste(ylabel,"visibility",sep=" ")
        }
      }
    }
  }
}

give_table<-function(temp){
  #steps for summary table
  # 1. filter last 7 days (remove rows)
  # 2. remove columns that are not needed
  # 3. change ob_time to contain only dates
  # 4. groupby ob_time and summarise (with mean)
  # 5. convert to wide format for beter visibility.
  if(!is_empty(temp)){
    temp<-temp%>%filter(ob_time>=as.Date("2020-11-24"))
    temp<-temp%>%select(ob_time,wind_speed,air_temperature,rltv_hum,visibility,Site_Name)
    temp<-temp%>%mutate(ob_time=as.Date(ob_time))
    temp<-temp%>%group_by(ob_time,Site_Name)%>%summarise_all(list(mean),na.rm=TRUE)
    temp
    round(temp[,3:6],2)
    te<-bind_cols(temp[,1:2],round(temp[,3:6],2))
    te
  }
}

#hutton_table<-function(){
  
#}

draw_plots<-function(temp,time,weather){
  temp%<>%select(ob_time,hour,day,month,!!sym(weather),Site_Name)       #keep just the necessary columns
  req(time)
  if (time==1){         #Selected "Raw hourly data" & "Week hours"
    temp%<>%mutate(Weekdays=wday(ob_time))      #make a column with Weekdays (1-7)
    temp%<>%mutate(Week_hours=(Weekdays-1)*24+hour)     #make a column with the hours (1-168)
    ggplot(temp,aes(Week_hours,!!sym(weather),colour=Site_Name,na.rm=TRUE))+geom_point(alpha=0.5)+
      ggtitle(give_title("Raw hourly data",weather))+
      xlab("Weekly hours")+
      ylab(give_ylabel("Hourly",weather))
  }
  else {
    if (time==2){         #Selected "Raw hourly data" & "24 hours"
      ggplot(temp,aes(hour,!!sym(weather),colour=Site_Name,na.rm=TRUE))+geom_point(alpha=0.3)+
        ggtitle(give_title("Raw hourly data",weather))+
        xlab("24 hours")+
        ylab(give_ylabel("Hourly",weather))
    } else {
      if (time==3){         #Selected "Daily averages" & "Calendar time"
        temp<-temp%>%mutate(ob_time=as.Date(ob_time))       #from ob_time which holds the date time keep the date
        temp<-temp%>%group_by(ob_time,Site_Name)%>%summarise(avg=mean(!!sym(weather),na.rm = TRUE))   #get daily average based on date and site
        ggplot(temp,aes(ob_time,avg,colour=Site_Name,na.rm=TRUE))+geom_line()+
          ggtitle(give_title("Daily averages",weather))+
          xlab("Calendar time (Days)")+
          ylab(give_ylabel("Daily",weather))
      } else {
        if(time==4){          #Selected "Daily averages" & "Week days"
          temp<-temp%>%mutate(Weekdays=wday(ob_time))     #make a column Weekdats (1-7)
          temp<-temp%>%mutate(ob_time=as.Date(ob_time))       #keep from ob_time with date time just the date
          temp<-temp%>%group_by(ob_time,Weekdays,Site_Name)%>%summarise(avg=mean(!!sym(weather),na.rm=TRUE))    #get average based on SiteName, date and weekday
          ggplot(temp,aes(Weekdays,avg,colour=Site_Name,na.rm=TRUE))+geom_point(alpha=0.5)+
            ggtitle(give_title("Daily averages",weather))+
            xlab("Week days")+
            ylab(give_ylabel("Daily",weather))
        } else {
          if(time==5){          #Selected "Monthly averages" & "Calendar time"
            temp<-temp%>%mutate(ob_time=month(ob_time))     #make a column with Months (1-12)
            temp<-temp%>%group_by(ob_time,Site_Name)%>%summarise(avg=mean(!!sym(weather),na.rm=TRUE))     #get average per month and site
            ggplot(temp,aes(ob_time,avg,colour=Site_Name,na.rm=TRUE))+geom_line()+
              ggtitle(give_title("Monthly averages",weather))+
              xlab("Calendar time (Months)")+
              ylab(give_ylabel("Monthly",weather))
          } else {
            if(time==6){          #Selected "Daily maxima" & "Calendar time"
              temp<-temp%>%mutate(ob_time=as.Date(ob_time))       #make-transform ob_time column with dates only
              temp<-temp%>%group_by(ob_time,Site_Name)%>%summarise(max=max(!!sym(weather),na.rm=TRUE))      #get max for each date and site name
              ggplot(temp,aes(ob_time,max,colour=Site_Name,na.rm=TRUE))+geom_line()+
                ggtitle(give_title("Daily maxima",weather))+
                xlab("Calendar time (Days)")+
                ylab(give_ylabel("Daily",weather))
            } else {
              if(time==7){          #Selected "Daily maxima" & "Week days"
                temp<-temp%>%mutate(Weekdays=wday(ob_time))     #make a new column with Weekdays (1-7)
                temp<-temp%>%mutate(ob_time=as.Date(ob_time))       #transform ob_time with only date
                temp<-temp%>%group_by(ob_time,Weekdays,Site_Name)%>%summarise(max=max(!!sym(weather),na.rm=TRUE))     #for each date,weekday and site name
                ggplot(temp,aes(Weekdays,max,colour=Site_Name,na.rm=TRUE))+geom_point(alpha=0.5)+
                  ggtitle(give_title("Daily maxima",weather))+
                  xlab("Week days")+
                  ylab(give_ylabel("Daily",weather))
              } else {
                if(time==8){          #Selected "Daily minima" & "Calendar time"
                  temp<-temp%>%mutate(ob_time=as.Date(ob_time))       #transform ob_time with only date
                  temp<-temp%>%group_by(ob_time,Site_Name)%>%summarise(min=min(!!sym(weather),na.rm=TRUE))      #get minima based on date and site name
                  ggplot(temp,aes(ob_time,min,colour=Site_Name,na.rm=TRUE))+geom_line()+
                    ggtitle(give_title("Daily minima",weather))+
                    xlab("Calendar time (Days)")+
                    ylab(give_ylabel("Daily",weather))
                } else {
                  if(time==9){          #Selected "Daily minima" & "Week days"
                    temp<-temp%>%mutate(Weekdays=wday(ob_time))     #make a column Weekdays (1-7)
                    temp<-temp%>%mutate(ob_time=as.Date(ob_time))       #transform ob_time to keep just date
                    temp<-temp%>%group_by(ob_time,Weekdays,Site_Name)%>%summarise(min=min(!!sym(weather),na.rm=TRUE))     #get min based on ob_time,weekdays,site name
                    ggplot(temp,aes(Weekdays,min,colour=Site_Name,na.rm=TRUE))+geom_point(alpha=0.5)+
                      ggtitle(give_title("Daily minima",weather))+
                      xlab("Week days")+
                      ylab(give_ylabel("Daily",weather))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}