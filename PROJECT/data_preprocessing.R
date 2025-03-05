library(dplyr)
library(readr)

#ecidata
voterturnout <- read_csv("C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\Turnout.csv.csv")
head(voterturnout)

voterturnout <- voterturnout %>% rename(States = `Name Of State/Ut`)
voterturnout <- voterturnout %>% rename(Turnout = `Voters - Vtr %`)
turnout_cleaned<-voterturnout %>% select(States,Turnout)

project <- read_csv("C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\project.csv")
head(project)

project<-turnout_cleaned
head(project)

write_csv(project, "C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\project.csv")

#MoUPA data
poverty <- read_csv("C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\poverty.csv")
head(poverty)
poverty<-poverty%>%rename(BPL = `Total - %age of Persons`)
poverty_cleaned <- poverty %>% select(States,BPL) 

project<-project %>% left_join(poverty_cleaned, by="States")
head(project)

colSums(is.na(project))
project[!complete.cases(project), ]

setdiff(poverty_cleaned$States, project$States)  # Shows names in df1 not in df2
setdiff(project$States, poverty_cleaned$States)  # Shows names in df2 not in df1

poverty_cleaned$States[poverty_cleaned$States == "ArunachalPradesh"] <- "Arunachal Pradesh"  
poverty_cleaned$States[poverty_cleaned$States == "Jammu & Kashmir"] <- "Jammu and Kashmir"  
poverty_cleaned$States[poverty_cleaned$States == "Trlpura"] <- "Tripura"  
project$States[project$States == "NCT OF Delhi"] <- "Delhi"  
poverty_cleaned$States[poverty_cleaned$States == "All India"] <- "Total"  

project<-project %>% left_join(poverty_cleaned, by="States")
head(project)
project<-na.omit(project)

write_csv(project, "C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\project.csv")

#ncrb data

crimes<-read_csv("C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\crcCAW_2014_1.csv")
head(crimes)

crimes_cleaned <- crimes %>% filter(grepl("Total Crimes against Women",`Crime Head`))
str(crimes_cleaned)
head(crimes_cleaned)
nrow(crimes_cleaned)

crimes_cleaned <- crimes_cleaned %>% rename(States = `States/UTs`)
crimes_cleaned <- crimes_cleaned %>% select(States,`2014`)

project<-project %>% left_join(crimes_cleaned, by="States")
project[!complete.cases(project), ]

setdiff(crimes_cleaned$States, project$States)  
setdiff(project$States,crimes_cleaned$States)  

crimes_cleaned$States[crimes_cleaned$States == "Delhi UT"] <- "Delhi"  
crimes_cleaned$States[crimes_cleaned$States == "Jammu & Kashmir"] <- "Jammu and Kashmir"  
crimes_cleaned$States[crimes_cleaned$States == "Total (All India)"] <- "Total" 

head(project)
project$`2014.x`<-NULL
names(project)[4] <- "CRW"

project<-project %>% left_join(crimes_cleaned, by="States")

print(project, n = Inf)
project <-  project[-c(28, 31, 32), ]  #removing small sample spaces like lakshwadweep and puducherry

write_csv(project, "C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\project.csv")

#nfhs data

survey <- read_csv("C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\datafile.csv")

survey_reduced <- survey %>% select(`State/UT`, `Women (age 15-49)  with 10 or more years of schooling (%)`, `Women age 20-24 years married before age 18 years (%)`)
head(survey_reduced)

names(survey_reduced)[1] <- "States"
names(survey_reduced)[2] <- "literacyF"
names(survey_reduced)[3] <- "Childmarr"

#converting districtwise data into statewise

survey_cleaned <- survey_reduced %>% 
  group_by(States) %>%  
  summarise(
    LiteracyF = mean(literacyF, na.rm = TRUE),  
    ChildMarr = mean(as.numeric(Childmarr), na.rm = TRUE)  
  )

head(survey_cleaned)

setdiff(survey_cleaned$States, project$States)  
setdiff(project$States,survey_cleaned$States)  

survey_cleaned$States[survey_cleaned$States == "NCT of Delhi"] <- "Delhi"  
survey_cleaned$States[survey_cleaned$States == "Jammu & Kashmir"] <- "Jammu and Kashmir"  
survey_cleaned$States[survey_cleaned$States == "Maharastra"] <- "Maharashtra" 

project<-project %>% left_join(survey_cleaned, by="States")

head(project)
write_csv(project, "C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\project.csv")

project <-  project[-c( 31), ]  #removing "total" row


#nso data

nso <- read_csv("C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\nso.csv")

#averaging out rural and urban population data

nso_cleaned <- nso %>%
  mutate(
    literacyM = (`2011 - Rural - Male` + `2011 - Urban - Male`) / 2,
    literacyF = (`2011 - Rural - Female` + `2011 - Urban - Female`) / 2,
    literacyTotal = (`2011 - Rural - Person` + `2011 - Urban - Female`) / 2
  ) %>%
  select(`All India/State/Union Territory`,literacyM,literacyF,literacyTotal)

nso_cleaned <- nso_cleaned %>% rename(States = `All India/State/Union Territory`)

setdiff(nso_cleaned$States, project$States)  
setdiff(project$States,nso_cleaned$States) 

nso_cleaned$States[nso_cleaned$States == "Chhatisgarh"] <- "Chhattisgarh"  
nso_cleaned$States[nso_cleaned$States == "Uttaranchal"] <- "Uttarakhand" 

project<-project %>% left_join(nso_cleaned, by="States")

write_csv(project, "C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\project.csv")

#reorder columns

project_reorder <- project[, c(1,8, 7, 9,5,3,2,4,6)]
head(project_reorder)

write_csv(project_reorder, "C:\\Users\\navee\\OneDrive\\Desktop\\PROJECT\\project.csv")
