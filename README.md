# namma_app_shiny

Air Quality has been a major concern. The Shiny app we present here is for visualising data collected using mobile measurements (sensors on a moving platform). We can use this app for visualising the data collected on air pollutants like PM2.5, Black Carbon and Ultrafine particles, with portable instruments (which are capable of providing high temporal resolution output, for instance, 1 Hertz data) mounted in a CNG-fuelled car.

The mobile air pollution monitoring campaign is carried out in select neighbourhoods in Bengaluru city. 
- The individual pollutant files are merged with GPS data.The resulting files can be easily accessed through the app and used to visualise the data. 
- With the click of a button, the app generates: summary statistics, time series plots and high resolution spatial map of pollutant concentrations. 
- Mapping parameters (pollutants) can be selected from a drop-down menu.

- One of the challenges this app addresses is that of managing high frequency data (~1 hertz) generated using a mobile platform. 
- The app allows team members to easily access data from the storage location and visualise the data in near-real time, without requiring knowledge in R. 
- The app reduces the time consumed for analysing each pollutant individually.
- It helps perform quality checks on the data at near real time and in instant visualisations of pollution hot spots. 
- The time series plots of each pollutant help understand the temporal patterns and health of the instruments. 
- This app will help air pollution researchers and amateurs interested in conducting individual mobile experiments using portable air-quality sensors, which are easily available in the market, and obtain high-quality data.

A simple app to integrate all data correction methods and to give a single output file of the mobile monitoring air quality data. The link to the app hosted on shinyapps.io is: https://nammaappairquality.shinyapps.io/ilkconsultancy/.

