if (!endsWith(getwd(), "C:/Users/marku/LRZ Sync+Share/MasterThesisSyntaxcoloring/22-RStudio")) {
  setwd("C:/Users/marku/LRZ Sync+Share/MasterThesisSyntaxcoloring/22-RStudio")
}
### GLOBALS
participants <- c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P14")
stimulis <- c("a1", "c2", "d3", "p4", "c1", "a2", "p3", "d4")
stimulistarts <- c("a", "c", "d", "p")
javaSkillLevels <- c("No Experience", "Beginner", "Advanced", "Expert")
# a1, a2, c1, c2, ...
stimulicolors <- c("#10C1F1", "#0B84A5", "#A98941", "#F6C85F", "#B37EC8", "#6F4E7C", "#658B41", "#9DD866")
orderedgroupcolors <- c("#10C1F1", "#F6C85F", "#B37EC8", "#9DD866", "#0B84A5", "#A98941", "#6F4E7C", "#658B41")
source("utils.r")

### DATA
survey <- read.csv2("C:/Users/marku/Documents/_OTH/Data/SurveyResults.csv", na="NA") %>% select(-DataReview, -Personal.Notes)
surveyg <- melt(setDT(survey), measure = patterns("(.+)Time", "(.+)Sureness", "(.+)Correct"),
                variable.name = "Presented.Stimulus.name", value.name = c("Time", "Sureness", "Correct"), na.rm = TRUE) %>%
  mutate(Presented.Stimulus.name = stimulis[Presented.Stimulus.name])
metrics <- read.csv2("C:/Users/marku/Documents/_OTH/Data/MasterThesisSyntaxcoloringMetrics.tsv", sep = "\t", na="NA") %>%
  select(!(starts_with("Number_of_Events") | starts_with("Time_to_first_Event"))) %>%
  filter(TOI == "Coding") %>%
  mutate(Exercise = paste0(Exercise, ColorScheme)) %>%
  rename(Participant.name = Participant, Presented.Stimulus.name = Exercise)
dfraw <- read.csv2("C:/Users/marku/Documents/_OTH/Data/MasterThesisSyntaxcoloringDataExport.tsv", sep = "\t", na="NA",
                   # filter out only columns we find important
                   colClasses = c("NULL", NA, NA, "NULL", "NULL", NA, "NULL", "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", "NULL", "NULL", "NULL", NA, "NULL", "NULL", "NULL", "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", NA, "NULL", "NULL", "NULL", NA, NA, NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA, NA, NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, NA))
dfg <- dfraw %>% groupTimeseries()

### Fun
for (type in c("Fixation", "Saccade")) {
  cat("\nPlotting", type)
  for (s in stimulistarts) {
    for (p in participants) {
      BP_PStoMinute(type, p, s)
    }
  }

  for (p in participants) {
    BP_PtoS(type, p)
  }
  
  for (s in stimulistarts) {
    BP_StoX(type, s)
    BP_StoTimeStepGrouped(type, s)
    BP_StoMinuteGrouped(type, s)
    LP_StoMinuteGrouped(type, s)
  }
  for (s in stimulis) {
    BP_StoMinuteGrouped(type, s)
    LP_StoMinuteGrouped(type, s)
  }
  BP_StoAll(type)
  BP_StoAllGrouped(type, useCount = FALSE)
  BP_StoAllGrouped(type, useCount = TRUE)
  BP_StoAllGroupedRate(type)
  BP_JavaSkill(type)
  BP_JavaSkillGrouped(type)
  BP_CorrectAnswersGrouped(type)
  Corr_General(type)
  Corr_Metrics(type)
}
print("\nPlotting rest")
devnull <- Corr_PerStimulus()
devnull <- BP_JavaSkill_WRONGLIMIT("Fixation")
devnull <- BP_JavaSkill_WRONGZOOM("Fixation")
devnull <- BP_YearsGrouped("Fixation")
devnull <- P_RatioFixationTotal(TRUE)
devnull <- P_RatioFixationTotal(FALSE)
devnull <- BP_SacAmp()
devnull <- BP_SacAmpGrouped()
devnull <- BP_JavaSkillSacAmp()
devnull <- BP_YearsSacAmp()
devnull <- BP_SurveyOpinion()
devnull <- M_Calibration()

sw <- T_ShapiroWilk()
ggdensity(sw$ASD, ggtheme = theme_minimal(), xlab = "ASD")
ggsave_("export/LP_Density_ASD.pdf", scaleX = 0.5, scaleY = 0.5)
ggdensity(sw$ASA, ggtheme = theme_minimal(), xlab = "ASA")
ggsave_("export/LP_Density_ASA.pdf", scaleX = 0.5, scaleY = 0.5)

# FIN
devnull <- BP_EXPLANATION()
devnull <- NULL
