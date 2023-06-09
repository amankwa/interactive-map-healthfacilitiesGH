# interactive-map-healthfacilitiesGH
An interactive map for locating health facilities in Ghana.

## Motivation:
The main motivation behind this simple shiny app is to make it easier to locate health facilities in Ghana along with other vital information.

## Data:
Data for the Health Facilities application is sourced from Open Government Data Platform. The original data is available as a csv file called "Health Facilities in Ghana". The raw data file, processed data file and my shiny code is made available on my github.

I have tried my best to comment and explain where necessary.

## Data limitation:
One of the limitations of this application is that the data is not accurate. If I plot the raw data I see a few health facility locations in Ghana. There are 1,795 missing data out of the total of 3,758 entries. All the missing data are coordinates i.e 897 longitudes and 898 latitudes. 

Hence when we create the shiny application we will ignore these points. I am in a process of updating these points. I have updated some of the health facilities with missing coordinates by searching on google map and assigning the appropriate coordinates. I have also added some health facilities that were not part of the original dataset downloaded from the government open data platform. The new dataset(Health.Facility.in.Ghana.csv) has all these updates.

## Help:
In case you observe the data projected on map is incorrect and you have more accurate or additional information that could be updated it will be great if you can email me (nanakwamester@gmail.com) and I will update the same.
