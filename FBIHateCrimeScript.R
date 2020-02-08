library(dplyr)
library(plotly)
library(ggplot2)

data <- tbl_df(
          read.csv("table13.csv")
        )
NortheastRegion <- c("Connecticut", "Delaware", "Maryland", "Massachusetts", "Maine", "New_Hampshire", "New_Jersey",
                     "New_York", "Pennsylvania", "Rhode_Island", "Vermont", "Virginia", "West_Virginia", "DC")
SoutheastRegion <- c("Alabama", "Florida", "Georgia", "Kentucky", "Louisiana", "Mississippi", "North_Carolina", "South_Carolina",
                     "Tennessee")
MidwestRegion <- c("Arkansas", "Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri",
                   "Nebraska", "North_Dakota", "Ohio", "South_Dakota", "Wisconsin")
IntermountainRegion <- c("Arizona", "Colorado", "Montana", "New_Mexico", "Oklahoma", "Texas", "Utah", "Wyoming")
PacificRegion <- c("California", "Idaho", "Nevada", "Oregon", "Washington")
AlaskaRegion <- c("Alaska")
finaldata <- data %>%
             rowwise() %>% 
             mutate(totalHateCrimeCount = sum(X1st.quarter, X2nd.quarter, X3rd.quarter, X4th.quarter, na.rm = TRUE)) %>%
             mutate(Population = as.numeric(gsub(",", "", Population))) %>%
             mutate(Region = case_when(!is.na(match(State, NortheastRegion)) ~ "Northeast Region",
                                       !is.na(match(State, SoutheastRegion)) ~ "Southeast Region",
                                       !is.na(match(State, MidwestRegion)) ~ "Midwest Region",
                                       !is.na(match(State, IntermountainRegion)) ~ "Intermountain Region",
                                       !is.na(match(State, PacificRegion)) ~ "Pacific Region",
                                       !is.na(match(State, AlaskaRegion)) ~ "Alaska Region"))
rm("data")
byRegionTotals <- finaldata %>%
                  group_by(Region) %>%
                  summarize(totalHateCrimes = sum(totalHateCrimeCount))
byRegionTotalsPie <- plot_ly(byRegionTotals, labels = ~Region, values = ~totalHateCrimes, type = 'pie',
                             title = 'FBI Hate Crimes By Region', showlegend = FALSE,
                             textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
                             titlefont = list(size = 23)) 
byRegionCrimeType <- finaldata %>%
                     group_by(Region) %>%
                     summarize(Race = sum(Race, na.rm = TRUE),
                               Religion = sum(Religion, na.rm = TRUE),
                               SexualOrientation = sum(Sexual.orientation, na.rm = TRUE),
                               Ethnicity = sum(Ethnicity, na.rm = TRUE),
                               Disability = sum(Disability, na.rm = TRUE),
                               Gender = sum(Gender, na.rm = TRUE),
                               GenderIdentity = sum(Gender.Identity, na.rm = TRUE))
crimePlot <- function(dataSet, plotTitle) {
  plot <- plot_ly(dataSet, 
          labels = c("Race",
                     "Religion",
                     "Sexual Orientation",
                     "Ethnicity",
                     "Disability",
                     "Gender",
                     "Gender Identity"),
          values = c(sum(dataSet$Race, na.rm = TRUE),
                     sum(dataSet$Religion, na.rm = TRUE),
                     sum(dataSet$Sexual.orientation, na.rm = TRUE),
                     sum(dataSet$Ethnicity, na.rm = TRUE),
                     sum(dataSet$Disability, na.rm = TRUE),
                     sum(dataSet$Gender, na.rm = TRUE),
                     sum(dataSet$Gender.Identity, na.rm = TRUE)),                                                                
          type = 'pie', title = plotTitle, showlegend = FALSE,
          textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
          titlefont = list(size=23)) 
    return(plot)
}
byCrimeTotalPie <- crimePlot(byRegionCrimeType, 'FBI Hate Crimes By Type')
byCrimeSEPie <- crimePlot(filter(byRegionCrimeType, Region == "Northeast Region"), 'FBI Hate Crimes By Type - Northeast')
byCrimeNEPie <- crimePlot(filter(byRegionCrimeType, Region == "Southeast Region"), 'FBI Hate Crimes By Type - Southeast')
byCrimeMWPie <- crimePlot(filter(byRegionCrimeType, Region == "Midwest Region"), 'FBI Hate Crimes By Type - Midwest')
byCrimeIMPie <- crimePlot(filter(byRegionCrimeType, Region == "Intermountain Region"), 'FBI Hate Crimes By Type - Intermountain')
byCrimePCFPie <- crimePlot(filter(byRegionCrimeType, Region == "Pacific Region"), 'FBI Hate Crimes By Type - Pacific')
byCrimeAKPie <- crimePlot(filter(byRegionCrimeType, Region == "Alaska Region"), 'FBI Hate Crimes By Type - Alaska')
byRegionCrimePerCapita <- finaldata %>%
                          group_by(Region) %>%
                          summarize(crimePerCapita = sum(totalHateCrimeCount)/sum(Population, na.rm = TRUE))
byRegionCrimePerCapitaBarChart <- plot_ly(byRegionCrimePerCapita, x = ~Region, y = ~crimePerCapita,
                                          type = 'bar') %>%
                                  layout(yaxis = list(title = 'Crime Per Capita'))
raceAndReligion <- finaldata %>%
                   group_by(State) %>%
                   summarize(Race = sum(Race, na.rm = TRUE), Religion = sum(Religion, na.rm = TRUE)) %>%
                   plot_ly(x = ~Race, color = I("black")) %>%
                   add_markers(y = ~Religion, showlegend = FALSE) %>%
                   add_lines(y = ~loess(Race ~ Religion),
                             line = list(color = '#07A4B5'),
                             name = "Loess Smoother", showlegend = TRUE)