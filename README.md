Unraviling Crime Patterns During the Pandemic
---------------------------------------------

Data collection from several cities across the country beginning in
mid-March. Early on during the pandemic – it was reported that crime had
dropped in several cities when stay-at-home orders were enacted. And
early on crime did fall. Further test however revealed several patterns
within the data and datasets were cleaned. The Post selected 27 cities
to create an aggregated rate of major crimes across the nation which
includes rape, robbery, homicide, burglary, auto theft and theft from
vehicle. Data shows an overall drop in crime in March through early May.
More cities will be added throughout the year as data become available.

Those 27 cities collectively saw the daily rate of crime on a rolling
21-day rate drop from 280 crimes per 100,000 to about 230 crimes per
100,000 people. The decrease was largely fueled by a decrease in
property-related crimes like simple theft, theft from cars and burglary.

But new patterns emerged in May – a period marked by the death of George
Floyd and the lifting of stay-at-home orders, both which criminal
experts say may or may not be related to recent crimes patterns. But
since then, crime – particularly violent crime – grew. The data was
collected via open web portals from each respective city. The data was
then cleaned and standardized using R and UCR definitions for violent
and property crimes. More than 800,000 reported crimes were collected
from 2020.

An additional two million crimes were collected for years 2018 and 2019
to compare current crime trends. The rolling 21-day average is
calculated by dividing crimes majority white or black tract population.
The overall rate was created by dividing all crimes by combined
population of each city. The rates for majority Asian, Black, Latino and
White neighborhoods were created totaling the populations of each Census
tract with a majority racial group present and then dividing crimes by
the extracted total population figures.

The data was collected via open web portals from each city. It was
standardized using the statistical program R and UCR definitions to
categorize violent and property crimes.

More than 800,000 reported crimes were collected from 2020. An
additional 2.2 million crimes were collected for 2018 and 2019 to
compare trends and create a yearly average. The crimes were geocoded to
Census tracts and merged with data on demographics.

A rolling 21-day average was calculated by dividing the total crimes in
neighborhoods with a majority Asian, Black, Latino or White population
by the sum population of those Census tracts. The location of some
crimes like sexual assaults or homicides were suppressed to protect a
victim’s privacy or because the crime was still under investigation,
bringing small limitations to the data.

Overall Crime Rate: How crime unfolded during the pandemic
==========================================================

Crimes began to dip in March. Figure.1 shows that dip which corresponds
to a period in which businesses closed their doors, schools taught
virtually and many workers were asked to work from home.

    absolute <- all_cities_scales %>%  
      ggplot() +
      aes(x = date, y = three_week_rate, color = year) +
      geom_step() +
      theme_minimal() +
      labs(
        title = "Rolling Rate Per 100k Residents",
        subtitle = " ",
        caption = "John D. Harden / The Washington Post",
        x = "Rate per 100k",
        y = "Date"
      ) + 
      theme(legend.position = "top")

    absolute

![](https://github.com/Jdharden/wp_covid_crime/blob/master/output_images/absolute_crime-1.png?raw=true)

#### Granular Crime Rates: Crime in neighborhoods of color

Crime models revealed a disparity between majority Black and majority
White neighborhoods – particularly violent crime. A Washington Post
analysis of crime data showed that the disparity grew by more than 100
percent during the pandemic.

![](https://github.com/Jdharden/wp_covid_crime/blob/master/output_images/unnamed-chunk-1-1.png?raw=true)

#### A Look at Violent Crime

Homicide rates grew faster than most crime rates among the 27 analyzed
by the Post. Here’s the aggregated rate of homicides among the 27
cities.

![](https://github.com/Jdharden/wp_covid_crime/blob/master/output_images/homicide_graph-1.png?raw=true)

#### Case Studies: Chicago, L.A. St. Louis and Dallas

You can also embed plots, for example:

    ## `summarise()` regrouping output by 'city' (override with `.groups` argument)

![](https://github.com/Jdharden/wp_covid_crime/blob/master/output_images/case_studies-1.png?raw=true)

### Limitations of data

The data is based on data collected from open data portals from
respective cities. Police departments from each city encourage infependent verification of data. In a few cities,
the longitude or latitudes are suppressed for some crimes that include
rape and sexual assault and pending homicide investigations. Police
departments suggest independent verification of crimes for complete
accuracy of the reporting process.
