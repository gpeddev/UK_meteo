# UK_meteo

## University of Glasgow - Online MSc on Data Analytics
### Project for R Programming
This is the end project for the course of R Programming

##### Project 

In this project you will write a Shiny app which visualises meteorological data collected at several locations across the United
Kingdom from 1st January 2020 to 30th November 2020.

The data is available on Moodle for you to download as a series of CSV files, which have been obtained from the Met Office In-
tegrated Data Archive System (see https://catalogue.ceda.ac.uk/uuid/220a65615218d5c9cc9e4785a3234bd0 ).
Data have been obtained from 20 weather stations. The file Sites.csv contains information about each weather station.
There is one CSV file for each weather station which contains hourly weather measurements on air temperature, wind speed,
relative humidity and visibility. The file names are of the form Site_<Site_ID>.csv . Each row of these files contains
hourly measurements of the weather measurements at that station. For some stations there are some missing observations
at certain time points.

The weather variables of interest that are being reported are:

**wind_speed** Average wind speed in knots.

**air_temperature** Air temperature measured in ◦ C . Note, temperature has been rounded to the nearest 0.1 ◦ C

**rltv_hum** Relative humidity, measured as a (%)

**visibility** Visibility, measured in metres.

Your task here is to develop a Shiny app which both visualises and summarises the data. Your app should also calculate and
report a diagnostic called the Hutton Criteria used to alert farmers of the risk of potato blight forming on potato crops.

The Hutton Criteria occurs in a particular day when both the follwing criteria are met

* The two previous days have a minimum temperature of 10 ◦ C
* The two previous days have at least six hours of relative humidity of 90% or higher

The app should have the following functionality:
* The app should be able to produce a variety of time series plots of relevant summary statistics against time.
* The user should be able to select from which weather variable and from which stations (at most 5) the quantities to be
shown.
* The user should be able to decide what aggregation to plot. This should include the following aggregations:
  - Raw hourly data (no aggregation);
  - Daily averages;
  - Monthly averages;
  - Daily maxima;
  - Daily minima;
* The user should be able to choose how time is to be handled and what the x-axis should represent. Choices should
include
  - Calendar time;
  - Day or hour within the week (going from 0 to 7 (days) or 0 to 168 (hours)); and
  - Hour in the day (going from 0 to 24 (hours)).
For calendar time the plot should be a line plot, but for the other two choices the plot should be a scatter plot.

* Make sure that suitable axis labels and legends are used.

* The app should also show a second plot with the location of the weather station(s). You can use the package maps
or ggmaps for this, or any other mapping library of your choice. The code below shows the location of all measuring
stations together with a map of the United Kingdom (assuming the data from Sites.csv is stored in a data frame
sites ).
```
maps::map("world","UK")
points(sites$Longitude, sites$Latitude, pch=16, col="red")
```
* Below the plot there should be a table providing a daily mean summary of all the weather variables for each site selected
for the last seven days of data available (this should be from November 24th to November 30th).
* The user should be able to download the table as a CSV file and the plots and table together as a Word document
(generated using rmarkdown ).
* For a given weather site selected by the user, report the Hutton criteria on a monthly basis (which can be selected by
the user) for each day of the month. You can report this either graphically or in a table. (For this final part, you may
want to create a separate tab in your app to report the criteria if you find this easier).
You can either use classical R plots or ggplot2 for any plots you create.


You can test the application on shinyapps.io
[link](https://gpeddev.shinyapps.io/project_meteo/)
