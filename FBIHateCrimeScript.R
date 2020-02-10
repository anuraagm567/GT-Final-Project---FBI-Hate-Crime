library(dplyr)
library(plotly)
library(ggplot2)
library(ggpmisc)

options(warn=-1)

#Data Setup
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

#Pie Charts
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
                     sum(dataSet$SexualOrientation, na.rm = TRUE),
                     sum(dataSet$Ethnicity, na.rm = TRUE),
                     sum(dataSet$Disability, na.rm = TRUE),
                     sum(dataSet$Gender, na.rm = TRUE),
                     sum(dataSet$GenderIdentity, na.rm = TRUE)),                                                                
          type = 'pie', title = plotTitle, showlegend = FALSE,
          textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
          titlefont = list(size=23)) 
    return(plot)
}
byCrimeTotalPie <- crimePlot(byRegionCrimeType, 'FBI Hate Crimes By Type')
byCrimeNEPie <- crimePlot(filter(byRegionCrimeType, Region == "Northeast Region"), 'FBI Hate Crimes By Type - Northeast')
byCrimeSEPie <- crimePlot(filter(byRegionCrimeType, Region == "Southeast Region"), 'FBI Hate Crimes By Type - Southeast')
byCrimeMWPie <- crimePlot(filter(byRegionCrimeType, Region == "Midwest Region"), 'FBI Hate Crimes By Type - Midwest')
byCrimeIMPie <- crimePlot(filter(byRegionCrimeType, Region == "Intermountain Region"), 'FBI Hate Crimes By Type - Intermountain')
byCrimePCFPie <- crimePlot(filter(byRegionCrimeType, Region == "Pacific Region"), 'FBI Hate Crimes By Type - Pacific')
byCrimeAKPie <- crimePlot(filter(byRegionCrimeType, Region == "Alaska Region"), 'FBI Hate Crimes By Type - Alaska')

#Bar Charts
#Crime Per Capita by Region
byRegionCrimePerCapita <- finaldata %>%
                          group_by(Region) %>%
                          summarize(crimePerCapita = sum(totalHateCrimeCount)/sum(Population, na.rm = TRUE))
byRegionCrimePerCapitaBarChart <- plot_ly(byRegionCrimePerCapita, x = ~Region, y = ~crimePerCapita,
                                          type = 'bar') %>%
                                  layout(yaxis = list(title = 'FBI Hate Crimes Per Capita'))

#Crime Per Capita by Type
byTypeOfCrimePerCapita <- c(Race = sum(finaldata$Race, na.rm = TRUE)/sum(finaldata$Population, na.rm = TRUE),
                            Religion = sum(finaldata$Religion, na.rm = TRUE)/sum(finaldata$Population, na.rm = TRUE),
                            Sexual_Orientation = sum(finaldata$Sexual.orientation, na.rm = TRUE)/sum(finaldata$Population, na.rm = TRUE),
                            Ethnicity = sum(finaldata$Ethnicity, na.rm = TRUE)/sum(finaldata$Population, na.rm = TRUE),
                            Disability = sum(finaldata$Disability, na.rm = TRUE)/sum(finaldata$Population, na.rm = TRUE),
                            Gender = sum(finaldata$Gender, na.rm = TRUE)/sum(finaldata$Population, na.rm = TRUE),
                            GenderIdentity = sum(finaldata$Gender.Identity, na.rm = TRUE)/sum(finaldata$Population, na.rm = TRUE))
byTypeOfCrimePerCapitaBarChart <- plot_ly(x = c("Race", "Religion", "Sexual Orientation", "Ethnicity",
                                                "Disability", "Gender", "Gender Identity"),
                                          y = byTypeOfCrimePerCapita,
                                          type = 'bar') %>%
                                  layout(yaxis = list(title = 'FBI Hate Crimes Per Capita by Type'))

#Linear Regression Plots
#Correlation between different crime types, grouped by State
raceAndReligion <- finaldata %>%
                   group_by(State) %>%
                   summarize(Race = sum(Race, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE),
                             Religion = sum(Religion, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE)) 
rrformula <- lm(raceAndReligion$Religion ~ raceAndReligion$Race)
raceAndReligionPlot <- raceAndReligion %>%
                       ggplot(aes(x = Race, y = Religion)) + geom_point() + geom_smooth(method = lm) +
                       stat_poly_eq(formula = rrformula, aes(label = paste(..eq.label.., ..rr.label.., 
                       sep = "*plain(\",\")~")), parse = TRUE) + xlab('Proportion of FBI Hate Crimes by Race') +
                       ylab('Proportion of FBI Hate Crimes by Religion')

raceAndEthnicity <- finaldata %>%
                    group_by(State) %>%
                    summarize(Race = sum(Race, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE),
                              Ethnicity = sum(Ethnicity, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE)) 
reformula <- lm(raceAndEthnicity$Ethnicity ~ raceAndEthnicity$Race)
raceAndEthnicityPlot <- raceAndEthnicity %>%
                        ggplot(aes(x = Race, y = Ethnicity)) + geom_point() + geom_smooth(method = lm) +
                        stat_poly_eq(formula = reformula, aes(label = paste(..eq.label.., ..rr.label.., 
                        sep = "*plain(\",\")~")), parse = TRUE) + xlab('Proportion of FBI Hate Crimes by Race') +
                        ylab('Proportion of FBI Hate Crimes by Ethnicity')

raceAndSO <- finaldata %>%
             group_by(State) %>%
             summarize(Race = sum(Race, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE),
                       SO = sum(Sexual.orientation, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE))
rsoformula <- lm(raceAndSO$SO ~ raceAndSO$Race)
raceAndSOPlot <- raceAndSO %>%
                 ggplot(aes(x = Race, y = SO)) + geom_point() + geom_smooth(method = lm) +
                 stat_poly_eq(formula = rsoformula, aes(label = paste(..eq.label.., ..rr.label.., 
                 sep = "*plain(\",\")~")), parse = TRUE) + xlab('Proportion of FBI Hate Crimes by Race') +
                 ylab('Proportion of FBI Hate Crimes by Sexual Orientation')

religionAndEthnicity <- finaldata %>%
                        group_by(State) %>%
                        summarize(Religion = sum(Religion, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE),
                                  Ethnicity = sum(Ethnicity, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE)) 
releformula <- lm(religionAndEthnicity$Ethnicity ~ religionAndEthnicity$Religion)
religionAndEthnicityPlot <- religionAndEthnicity %>%
                            ggplot(aes(x = Religion, y = Ethnicity)) + geom_point() + geom_smooth(method = lm) +
                            stat_poly_eq(formula = releformula, aes(label = paste(..eq.label.., ..rr.label.., 
                            sep = "*plain(\",\")~")), parse = TRUE) + xlab('Proportion of FBI Hate Crimes by Religion') +
                            ylab('Proportion of FBI Hate Crimes by Ethnicity')

religionAndSO <- finaldata %>%
                 group_by(State) %>%
                 summarize(Religion = sum(Religion, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE),
                           SO = sum(Sexual.orientation, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE))
relsoformula <- lm(religionAndSO$SO ~ religionAndSO$Religion)
religionAndSOPlot <- religionAndSO %>%
                     ggplot(aes(x = Religion, y = SO)) + geom_point() + geom_smooth(method = lm) +
                     stat_poly_eq(formula = relsoformula, aes(label = paste(..eq.label.., ..rr.label.., 
                     sep = "*plain(\",\")~")), parse = TRUE) + xlab('Proportion of FBI Hate Crimes by Religion') +
                     ylab('Proportion of FBI Hate Crimes by Sexual Orientation')

SOAndEthnicity <- finaldata %>%
                  group_by(State) %>%
                  summarize(SO = sum(Sexual.orientation, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE),
                                     Ethnicity = sum(Ethnicity, na.rm = TRUE)/sum(totalHateCrimeCount, na.rm = TRUE)) 
soeformula <- lm(SOAndEthnicity$Ethnicity ~ SOAndEthnicity$SO)
SOAndEthnicityPlot <- SOAndEthnicity %>%
                      ggplot(aes(x = SO, y = Ethnicity)) + geom_point() + geom_smooth(method = lm) +
                      stat_poly_eq(formula = soeformula, aes(label = paste(..eq.label.., ..rr.label.., 
                      sep = "*plain(\",\")~")), parse = TRUE) + xlab('Proportion of FBI Hate Crimes by Sexual Orientation') +
                      ylab('Proportion of FBI Hate Crimes by Ethnicity')

#Confidence Intervals
confint(rrformula) #Race And Religion
confint(reformula) #Race And Ethnicity
confint(rsoformula) #Race And Sexual Orientation
confint(releformula) #Religion and Ethnicity
confint(relsoformula) #Religion and Sexual Orientation
confint(soeformula) #Sexual Orientation and Ethnicity

#Overall Summary of Regression Lines
summary(rrformula) #Race And Religion
summary(reformula) #Race And Ethnicity
summary(rsoformula) #Race And Sexual Orientation
summary(releformula) #Religion and Ethnicity
summary(relsoformula) #Religion and Sexual Orientation
summary(soeformula) #Sexual Orientation and Ethnicity