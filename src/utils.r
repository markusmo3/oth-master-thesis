library(rlang)
library(tidyverse)
library(ggplot2)
library(lazyeval)
library(data.table)
library(ggcorrplot)
library(expss)
library(ggsignif)
library(scales)
library("ggpubr")
library(xtable)
options(scipen=10)
if (!dir.exists("export/")) {
  dir.create("export/")
}
if (!dir.exists("export/ByTime/")) {
  dir.create("export/ByTime/")
}
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

### Main function to group 3ms time series entry of the same type and to the same fixation point together
groupTimeseries <- function(df) {
  return(df %>%
           subset(select=c(Computer.timestamp, Eye.movement.type, Participant.name, 
                           Presented.Stimulus.name, Fixation.point.X, Fixation.point.Y,
                           Gaze.point.X, Gaze.point.Y,
                           Pupil.diameter.left, Pupil.diameter.right)) %>%
           arrange(Participant.name, Presented.Stimulus.name, Computer.timestamp) %>%
           group_by(Participant.name, Presented.Stimulus.name) %>%
           filter(Presented.Stimulus.name != "") %>%
           mutate(grp = cumsum(Eye.movement.type != dplyr::lag(Eye.movement.type, default=Eye.movement.type[1]) | 
                                 Presented.Stimulus.name != dplyr::lag(Presented.Stimulus.name, default=Presented.Stimulus.name[1]) | 
                                 Participant.name != dplyr::lag(Participant.name, default=Participant.name[1])),
                  endt = dplyr::lead(Computer.timestamp, default=Computer.timestamp[n()])) %>%
           group_by(Eye.movement.type, Presented.Stimulus.name, Participant.name, grp) %>%
           summarize(startt = Computer.timestamp[1], endt = endt[n()],
                     SacAmpX = Gaze.point.X[n()] - Gaze.point.X[1],
                     SacAmpY = Gaze.point.Y[n()] - Gaze.point.Y[1],
                     Pupil.diameter.left = mean(Pupil.diameter.left),
                     Pupil.diameter.right = mean(Pupil.diameter.right)) %>%  #, x = Fixation.point.X[1], y = Fixation.point.Y[1]
           mutate(dur = endt-startt, SacAmp = ifelse((Eye.movement.type == "Saccade"), sqrt(SacAmpX^2 + SacAmpY^2), NA)) %>%
           arrange(startt) %>%
           ungroup() %>%
           select(-grp) %>%
           mutate(dur = dur / 1000)
  )
}

meanOverExercise <- function (df) {
  return (df %>% 
            group_by(Participant.name, Presented.Stimulus.name, Eye.movement.type) %>%
            mutate(cnt = 1) %>%
            summarize(dur = mean(dur), cnt = sum(cnt), SacAmp = mean(SacAmp, na.rm=TRUE)) %>%
            ungroup())
}

ggsave_ <- function(..., scaleY = 1, scaleX = 1) {
  ggsave(width = 6.5 * scaleX, height = 6.5*0.75 * scaleY, units = "in", ...)
  cat('.')
}

BP <- function(df, title, subtitle = NULL, x, xlab, y, ylab, limitY = TRUE,
               fill = NULL, filllab = "", includeDataSize = TRUE, includeTitles = FALSE) {
  arg <- match.call()
  dfs <- if (includeDataSize) {
    df %>% count(eval(arg$x)) %>% mutate(name = paste0(`eval(arg$x)`, " (", n, ")"))
  } else {
    df %>% count(eval(arg$x)) %>% mutate(name = `eval(arg$x)`)
  }
  labels <- list()
  for(i in 1:8) {
    labels[toString(dfs[[1]][i])] <- dfs[["name"]][i]
  }
  
  xname <- if (includeDataSize) {
    paste(xlab, "(data size)")
  } else {
    xlab
  }
  
  aes <- if (is.null(arg$fill)) {
    aes(y = eval(arg$y), x = eval(arg$x))
  } else {
    aes(y = eval(arg$y), x = eval(arg$x), fill = eval(arg$fill))
  }
  
  p <- ggplot(data = df, aes) +
    stat_boxplot(geom ='errorbar', width = 0.5) +
    geom_boxplot() + 
    scale_x_discrete(name = xname, labels = labels) +
    scale_y_continuous(name = ylab) +
    labs(fill = filllab) +
    # stat_summary(fun = mean, geom = "point", shape = 4, size = 5, color = "black", fill = "black") +
    stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), width = .75, linetype = "dashed") +
    theme_light() +
    theme(text = element_text(family = "serif"))
  if (includeTitles) {
    p <- p + labs(title = title, subtitle = subtitle, fill = filllab)
  }
  if (limitY) {
    p <- p + coord_cartesian(ylim=quantile(eval(arg$y, envir = df), c(0.05, 0.95), na.rm = TRUE))
  }
  return(p)
}

BP_JavaSkill <- function(type) {
  df <- dfg %>%
    merge(select(survey, Participant.name, JavaSkill), by="Participant.name") %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis)
  
  BP(df, paste(type, "Time per Java Skill"),
     x = factor(JavaSkill, levels = c(0,1,2,3), labels = javaSkillLevels), xlab = "Java Skill",
     y = dur, ylab = paste0("Average ", type, " Duration in ms")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave_(paste0("export/", "BP_JavaSkill_", type, ".pdf"), scaleX = 0.5)
  return(df)
}
BP_JavaSkill_WRONGLIMIT <- function(type) {
  df <- dfg %>%
    merge(select(survey, Participant.name, JavaSkill), by="Participant.name") %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis)
  
  BP(df, paste(type, "Time per Java Skill"),
     x = factor(JavaSkill, levels = c(0,1,2,3), labels = javaSkillLevels), xlab = "Java Skill",
     y = dur, ylab = paste0("Average ", type, " Duration in ms")) +
    ylim(quantile(df$dur, c(0.05, 0.95))) + ylab(paste0("Average ", type, " Duration in ms")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave_(paste0("export/", "BP_JavaSkill_", type, "_WRONGLIMIT.pdf"), scaleX = 0.5)
  return(df)
}
BP_JavaSkill_WRONGZOOM <- function(type) {
  df <- dfg %>%
    merge(select(survey, Participant.name, JavaSkill), by="Participant.name") %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis)
  
  BP(df, paste(type, "Time per Java Skill"),
     x = factor(JavaSkill, levels = c(0,1,2,3), labels = javaSkillLevels), xlab = "Java Skill",
     y = dur, ylab = paste0("Average ", type, " Duration in ms"), limitY = FALSE) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave_(paste0("export/", "BP_JavaSkill_", type, "_WRONGZOOM.pdf"), scaleX = 0.5)
  return(df)
}

BP_JavaSkillGrouped <- function(type) {
  df <- dfg %>% meanOverExercise() %>%
    merge(select(survey, Participant.name, JavaSkill), by="Participant.name") %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis)
  
  BP(df, paste(type, "Time per Java Skill"), subtitle = "(Grouped by Participant and Exercise)",
     x = factor(JavaSkill, levels = c(0,1,2,3), labels = javaSkillLevels), xlab = "Java Skill",
     y = dur, ylab = paste0("Average ", type, " Duration in ms"), limitY = FALSE) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave_(paste("export/", "BP_JavaSkillGrouped_", type, ".pdf", sep = ""), scaleX = 0.5)
  return(df)
}

BP_YearsGrouped <- function(type) {
  df <- dfg %>% meanOverExercise() %>%
    merge(select(survey, Participant.name, YearsOfGeneralProg, YearsOfProfProg), by="Participant.name") %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis) %>%
    mutate(YearsOfGeneralProg = factor(case_when(YearsOfGeneralProg < 7 ~ "0-6", YearsOfGeneralProg < 10 ~ "7-9", TRUE ~ "10+"), levels = c("0-6", "7-9", "10+")),
           YearsOfProfProg = factor(case_when(YearsOfProfProg < 3 ~ "0-2", YearsOfProfProg < 5 ~ "3-4", TRUE ~ "5+"), levels = c("0-2", "3-4", "5+")))
  
  BP(df, paste(type, "Time over gen. years"), subtitle = "(Grouped by Participant and Exercise)",
     x = YearsOfGeneralProg, xlab = "Years",
     y = dur, ylab = paste0("Average ", type, " Duration in ms"), limitY = FALSE) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave_(paste("export/", "BP_YearsGrouped_general_", type, ".pdf", sep = ""), scaleX = 0.5)
  BP(df, paste(type, "Time over prof. years"), subtitle = "(Grouped by Participant and Exercise)",
     x = YearsOfProfProg, xlab = "Years",
     y = dur, ylab = paste0("Average ", type, " Duration in ms"), limitY = FALSE) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave_(paste("export/", "BP_YearsGrouped_prof_", type, ".pdf", sep = ""), scaleX = 0.5)
  return(df)
}

BP_JavaSkillSacAmp <- function() {
  type <- "Saccade"
  df <- dfg %>% meanOverExercise() %>%
    merge(select(survey, Participant.name, JavaSkill), by="Participant.name") %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis)
  
  BP(df, paste(type, "Amplitude per Java Skill"), subtitle = "(Grouped by Participant and Exercise)",
     x = factor(JavaSkill, levels = c(0,1,2,3), labels = javaSkillLevels), xlab = "Java Skill",
     y = SacAmp, ylab = "Saccade Amplitude in pixels", limitY = FALSE) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave_(paste("export/", "BP_JavaSkillSacAmp_", type, ".pdf", sep = ""), scaleX = 0.5)
  return(df)
}

BP_YearsSacAmp <- function() {
  type <- "Saccade"
  df <- dfg %>% meanOverExercise() %>%
    merge(select(survey, Participant.name, YearsOfGeneralProg, YearsOfProfProg), by="Participant.name") %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis) %>%
    mutate(YearsOfGeneralProg = factor(case_when(YearsOfGeneralProg < 7 ~ "0-6", YearsOfGeneralProg < 10 ~ "7-9", TRUE ~ "10+"), levels = c("0-6", "7-9", "10+")),
           YearsOfProfProg = factor(case_when(YearsOfProfProg < 3 ~ "0-2", YearsOfProfProg < 5 ~ "3-4", TRUE ~ "5+"), levels = c("0-2", "3-4", "5+")))
  
  BP(df, paste(type, "Amplitude per gen. years"), subtitle = "(Grouped by Participant and Exercise)",
     x = YearsOfGeneralProg, xlab = "Years",
     y = SacAmp, ylab = "Saccade Amplitude in pixels", limitY = FALSE) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave_(paste("export/", "BP_YearsSacAmp_general_", type, ".pdf", sep = ""), scaleX = 0.5)
  return(df)
}

BP_CorrectAnswersGrouped <- function(type) {
  df <- dfg %>% meanOverExercise() %>%
    merge(surveyg %>% group_by(Participant.name) %>%
            mutate(Correct = ifelse(Correct == "Yes", 1, 0)) %>%
            summarize(Correct = sum(Correct)) %>%
            select(Participant.name, Correct), by="Participant.name") %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis)
  
  BP(df, paste(type, "Time per correct answers"), subtitle = "(Pre-Grouped by Participant and Exercise)",
     x = factor(Correct, levels = c(0,1,2,3,4)), xlab = "# of correct answers",
     y = dur, ylab = paste0("Average ", type, " Duration in ms"), limitY = FALSE)
  ggsave_(paste("export/", "BP_CorrectAnswersGrouped_", type, ".pdf", sep = ""))
  return(df)
}

BP_PtoS <- function(type, p) {
  df <- dfg %>% filter(Participant.name == p) %>% 
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis) %>%
    mutate(Exercise = substr(Presented.Stimulus.name, 1, 1))
  
  BP(df, paste(type, "Time per Stimulus by Participant"),
     x = factor(Presented.Stimulus.name, levels = stimulis), xlab = "Stimulus",
     y = dur, ylab = paste0("Average ", type, " Duration in ms"),
     fill = Exercise, filllab = "Exercise")
  ggsave_(paste("export/", "BP_PtoS_", type, "_", p, ".pdf", sep = ""))
  
  return(df)
}

BP_StoX <- function(type, s) {
  # df <- dfraw %>% filter(Presented.Stimulus.name == s) %>% 
  df <- dfg %>% filter(startsWith(Presented.Stimulus.name, s)) %>% 
    rename(Stimulus = Presented.Stimulus.name) %>%
    filter(Eye.movement.type == type)
  
  BP(df, paste(type, "Time per Participant during Stimulus"),
     x = Participant.name, xlab = "Participant",
     y = dur, ylab = paste0("Average ", type, " Duration in ms"),
     fill = Stimulus, filllab = "Stimulus") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave_(paste("export/", "BP_StoP_", type, "_", s, ".pdf", sep = ""))
  
  BP(df, paste(type, "Time per Stimulus"),
     x = Stimulus, xlab = "Stimulus",
     y = dur, ylab = paste0("Average ", type, " Duration in ms"),
     fill = Stimulus, filllab = "Stimulus")
  ggsave_(paste("export/", "BP_StoS_", type, "_", s, ".pdf", sep = ""))
  
  return(df)
}

BP_StoAll <- function(type) {
  # df <- dfraw %>% filter(Presented.Stimulus.name == s) %>% 
  df <- dfg %>%
    rename(Stimulus = Presented.Stimulus.name) %>%
    filter(Eye.movement.type == type)
  
  BP(df, paste(type, "Time per Stimulus"),
     x = factor(Stimulus, stimulis), xlab = "Stimulus",
     y = dur, ylab = paste0("Average ", type, " Duration in ms"),
     fill = Stimulus, filllab = "Stimulus") +
    scale_fill_manual(values=stimulicolors)
  ggsave_(paste("export/", "BP_StoAll_", type, ".pdf", sep = ""))
  
  return(df)
}

BP_StoAllGrouped <- function(type, useCount) {
  # df <- dfraw %>% filter(Presented.Stimulus.name == s) %>% 
  label <- if (useCount) "Count" else "Time"
  ylabel <- if (useCount) paste("Total", type, "Count") else paste("Average", type, "Duration in ms")
  df <- dfg %>%
    
    group_by(Participant.name, Presented.Stimulus.name, Eye.movement.type) %>%
    mutate(cnt = 1) %>%
    summarize(dur = mean(dur), cnt = sum(cnt)) %>%
    ungroup() %>%
    
    rename(Stimulus = Presented.Stimulus.name) %>%
    mutate(Stimulus = factor(Stimulus, levels = stimulis), useCount = useCount,
           Group=case_when(Stimulus %in% c("a1", "c2", "d3", "p4") ~ "Group A", TRUE ~ "Group B")) %>%
    filter(Eye.movement.type == type)
  
  BP(df, paste(type, label, "per Stimulus"),
     x = Stimulus, xlab = "Stimulus",
     y = ifelse(useCount, cnt, dur), ylab = ylabel, limitY=FALSE) +
    # , fill = Stimulus, filllab = "Stimulus"
    # scale_fill_manual(values=stimulicolors) +
    facet_wrap(~ Group, scales = "free_x")
  ggsave_(paste("export/", "BP_StoAllGrouped_", type, "_", label, ".pdf", sep = ""))
  
  return(df)
}

BP_StoAllGroupedRate <- function(type) {
  # df <- dfraw %>% filter(Presented.Stimulus.name == s) %>%
  ylabel <- "Fixations per second"
  df <- dfg %>%
    group_by(Participant.name, Presented.Stimulus.name, Eye.movement.type) %>%
    mutate(cnt = 1) %>%
    summarize(dur = mean(dur), cnt = sum(cnt)) %>%
    ungroup() %>%
    
    merge(select(surveyg, "Participant.name", "Presented.Stimulus.name", "Time"), by=c("Participant.name", "Presented.Stimulus.name")) %>%
    rename(Stimulus = Presented.Stimulus.name) %>%
    mutate(Stimulus = factor(Stimulus, levels = stimulis),
           Group=case_when(Stimulus %in% c("a1", "c2", "d3", "p4") ~ "Group A", TRUE ~ "Group B")) %>%
    filter(Eye.movement.type == type) %>%
    mutate(cnt = cnt/(Time/1000))
  
  BP(df, paste(type, label, "per Stimulus"),
     x = Stimulus, xlab = "Stimulus",
     y = cnt, ylab = ylabel, limitY=FALSE) +
    # geom_point(aes(color=Participant.name)) +
    # , fill = Stimulus, filllab = "Stimulus"
    # scale_fill_manual(values=stimulicolors) +
    facet_wrap(~ Group, scales = "free_x")
  ggsave_(paste("export/", "BP_StoAllGroupedRate_", type, ".pdf", sep = ""))
  
  return(df)
}

BP_PStoMinute <- function(type, p, s) {
  df <- dfg %>% filter(Participant.name == p & startsWith(Presented.Stimulus.name, s)) %>% 
    mutate(Minute = floor((startt - startt[1] + 6e7) / 60000000)) %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis) %>%
    mutate(Minute = formatC(Minute, width = 2, flag = "0"))
  
  BP(df, paste(type, "Time over Time by Participant", p, "during Stimulus", s),
     x = Minute, xlab = "Minute",
     y = dur, ylab = paste0("Average ", type, " Duration in ms")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave_(paste("export/ByTime/", "BP_PStoMinute_", type, "_", p, s, ".pdf", sep = ""))
  return(df)
}

BP_StoMinuteGrouped <- function(type, s) {
  df <- dfg %>% filter(startsWith(Presented.Stimulus.name, s)) %>% 
    group_by(Participant.name) %>%
    mutate(Minute = floor((startt - startt[1] + 6e7) / 60000000)) %>%
    ungroup() %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis) %>%
    mutate(Minute = formatC(Minute, width = 2, flag = "0"))
  
  BP(df, paste(type, "Time over Time during Stimulus", s),
     x = Minute, xlab = "Minute",
     y = dur, ylab = paste0("Average ", type, " Duration in ms")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave_(paste("export/ByTime/", "BP_StoMinuteGrouped_", type, "_", s, ".pdf", sep = ""))
  return(df)
}

LP_StoMinuteGrouped <- function(type, s) {
  df <- dfg %>% filter(startsWith(Presented.Stimulus.name, s)) %>%
    merge(select(surveyg, "Participant.name", "Group"), by="Participant.name") %>%
    group_by(Group, Participant.name) %>%
    mutate(Minute = floor((startt - startt[1] + 6e7) / 60000000)) %>%
    ungroup() %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis) %>%
    group_by(Group, Participant.name, Minute) %>%
    summarize(dur = mean(dur)) %>%
    filter(Minute > 0) %>%
    mutate(Minute = formatC(Minute, width = 2, flag = "0"))
  
  # override <- df %>% group_by(Group, Participant.name) %>% summarize() %>% 
  #   mutate(shape = ifelse(Group == "A", 16, 17), linetype = ifelse(Group == "A", 1, 2))
  # pal <- c()
  # for(i in 1:13) {
  #   pal[toString(participants[i])] <- hue_pal()(13)[i]
  # }
  ggplot(data = df, mapping = aes(y = dur, x = Minute, group = Participant.name)) +
    geom_line(size=1, aes(color=Participant.name, linetype=Group)) + 
    geom_point(size=2, aes(color=Participant.name, shape=Group)) + 
    scale_x_discrete(name = "Minute") +
    scale_y_continuous(name = paste0("Average ", type, " Duration in ms")) +
    # scale_linetype_manual(values = c(1,2), drop = FALSE) + 
    # scale_shape_manual(values = c(16,17), drop = FALSE) + 
    # scale_color_manual(values = pal) +
    # labs(title = paste(type, "Time over Time during Stimulus", s), color = "Participant") +
    labs(color = "Participant") +
    theme_minimal() +
    theme(legend.position = "bottom", text = element_text(family = "serif")) +
    guides(col = guide_legend(order = 1))
  # guides(col = guide_legend(order = 1, override.aes = list(shape = override$shape, linetype = override$linetype)))
  ##### INSIDE THE GRAPH:
  # theme(legend.position = c(1, 1),
  #       legend.direction = "horizontal",
  #       legend.justification = c("right", "top"),
  #       legend.box.just = "left",
  #       legend.background = element_rect(fill="white", color="white", size=1),
  #       legend.margin = margin(6, 6, 6, 6)) +
  # guides(col = guide_legend(nrow=4, order = 1))
  # ggsave_(scale = 1.5, paste("export/ByTime/", "LP_StoMinuteGrouped_", type, "_", s, ".pdf", sep = ""))
  ggsave_(scaleY = 1.1, paste("export/ByTime/", "LP_StoMinuteGrouped_", type, "_", s, ".pdf", sep = ""))
  return(df)
}

BP_StoTimeStepGrouped <- function(type, s) {
  df <- dfg %>% filter(startsWith(Presented.Stimulus.name, s)) %>% 
    group_by(Participant.name) %>%
    mutate(TimeStep = floor((startt - startt[1])*10 / (startt[n()]+1 - startt[1]) )) %>%
    ungroup() %>%
    filter(Eye.movement.type == type & Presented.Stimulus.name %in% stimulis) %>%
    mutate(TimeStep = formatC(TimeStep, width = 1, flag = "0"))
  
  BP(df, paste(type, "Time over Time during Stimulus", s),
     x = TimeStep, xlab = "TimeStep",
     y = dur, ylab = paste0("Average ", type, " Duration in ms")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave_(paste("export/ByTime/", "BP_StoTimeStepGrouped_", type, "_", s, ".pdf", sep = ""))
  return(df)
}

BP_SacAmp <- function() {
  type <- "Saccade"
  df <- dfg %>% 
    filter(Eye.movement.type == type) %>%
    mutate(Stimulus = Presented.Stimulus.name)
  
  BP(df, paste(type, "Amplitude per Stimulus"),
     x = factor(Stimulus, stimulis), xlab = "Stimulus",
     y = SacAmp, ylab = "Saccade Amplitude in pixels")
  ggsave_(paste("export/", "BP_SacAmp_", type, ".pdf", sep = ""))
  
  return(df)
}

BP_SacAmpGrouped <- function() {
  type <- "Saccade"
  df <- dfg %>% 
    filter(Eye.movement.type == type) %>%
    group_by(Participant.name, Presented.Stimulus.name) %>%
    summarize(SacAmp = mean(SacAmp, na.rm=TRUE)) %>%
    ungroup() %>%
    rename(Stimulus = Presented.Stimulus.name) %>%
    mutate(Group=case_when(Stimulus %in% c("a1", "c2", "d3", "p4") ~ "Group A", TRUE ~ "Group B"))
  
  BP(df, paste(type, "Amplitude per Stimulus"), subtitle = "(Pre-Grouped by Participant and Exercise)",
     x = factor(Stimulus, stimulis), xlab = "Stimulus",
     y = SacAmp, ylab = "Saccade Amplitude in pixels", limitY = FALSE) +
    scale_fill_manual(values=stimulicolors) +
    # geom_signif(comparisons = list(c(1, 4)), map_signif_level=TRUE) +
    facet_wrap(~ Group, scales = "free_x")
  ggsave_(paste("export/", "BP_SacAmpGrouped_", type, ".pdf", sep = ""))
  
  return(df)
}

P_RatioFixationTotal <- function(withoutErrors = TRUE) {
  df2 <- dfg %>%
    group_by(Participant.name, Presented.Stimulus.name, Eye.movement.type) %>%
    summarize(dur = sum(dur)) %>%
    filter(withoutErrors | Eye.movement.type %in% c("Fixation", "Saccade")) %>%
    group_by(Participant.name, Presented.Stimulus.name) %>%
    mutate(ratio = dur / sum(dur)) %>%
    filter(Eye.movement.type == "Fixation")
  
  note <- if(withoutErrors) "withoutError" else "withError"
  ggplot(data = df2, mapping = aes(x = Participant.name, y = factor(Presented.Stimulus.name, levels = stimulis), color = ratio, label = format(ratio, digits=2))) +
    geom_point(size = 12) + geom_text(color = "white", size = 3) +
    scale_x_discrete(name = "Participant") +
    scale_y_discrete(name = "Stimulus") +
    labs(title = paste("Ratio of Fixation vs Total - ", note)) +
    theme(legend.position=c(1, 0), legend.justification = c(1,0), text = element_text(family = "serif"))
  ggsave_(paste("export/P_RationFixationTotal_", note, ".pdf", sep=""))
}

replaceNaIntelligent <- function(df, to, from) {
  df[c(to)][is.na(df[c(to)])] <- df[c(from)][is.na(df[c(to)])]
  return(df)
}

Corr_General <- function(type) {
  df <- dfg %>% filter(Eye.movement.type == type) %>%
    group_by(Participant.name, Presented.Stimulus.name, Eye.movement.type) %>% 
    summarize(meandur = mean(dur)) %>%
    merge(survey, by="Participant.name") %>%
    replaceNaIntelligent("a1Time", "a2Time") %>%
    replaceNaIntelligent("c2Time", "c1Time") %>%
    replaceNaIntelligent("d3Time", "d4Time") %>%
    replaceNaIntelligent("p4Time", "p3Time") %>%
    replaceNaIntelligent("a1Sureness", "a2Sureness") %>%
    replaceNaIntelligent("c2Sureness", "c1Sureness") %>%
    replaceNaIntelligent("d3Sureness", "d4Sureness") %>%
    replaceNaIntelligent("p4Sureness", "p3Sureness") %>%
    select(meandur, YearsOfGeneralProg, YearsOfProfProg, JavaSkill, 
           BeforeFindRelevantSymbols,BeforeMemorize,BeforeUnderstandStructure,BeforeMoreEfficient,AfterFindRelevantSymbols,AfterMemorize,AfterUnderstandStructure,AfterMoreEfficient,
           a1Time, c2Time, d3Time, p4Time,
           a1Sureness, c2Sureness, d3Sureness, p4Sureness,
    ) 
  corr <- df %>% cor() %>% round(1)
  corp <- df %>% cor_pmat()
  ggcorrplot(corr, type = "lower", lab = TRUE, lab_size = 3, p.mat = corp) +
    theme(text = element_text(family = "serif"))
  ggsave_(paste("export/Corr_General_", type, ".pdf", sep=""), scale = 1.5)
  return(df)
}

Corr_PerStimulus <- function() {
  dfF <- dfg %>% meanOverExercise() %>% filter(Eye.movement.type == "Fixation") %>%
    rename(durF = dur, cntF = cnt) %>%
    select(Participant.name, Presented.Stimulus.name, durF, cntF)
  df <- dfg %>% meanOverExercise() %>% filter(Eye.movement.type == "Saccade") %>%
    merge(surveyg, by=c("Participant.name", "Presented.Stimulus.name")) %>%
    merge(dfF, by=c("Participant.name", "Presented.Stimulus.name")) %>%
    mutate(Correct = ifelse(Correct == "Yes", 1, 0)) %>%
    select(dur, cnt, SacAmp, durF, cntF, YearsOfGeneralProg, YearsOfProfProg, JavaSkill,
           # BeforeFindRelevantSymbols,BeforeMemorize,BeforeUnderstandStructure,BeforeMoreEfficient,AfterFindRelevantSymbols,AfterMemorize,AfterUnderstandStructure,AfterMoreEfficient,
           # Time, Sureness, Correct, ifelse(type == "Saccade", "SacAmp", 0)
           Time, Sureness, Correct
    )
  
  corr <- df %>% cor() %>% round(1)
  corp <- df %>% cor_pmat()
  
  axis_labels <- c("Duration of Saccades",
                   "Count of Saccades",
                   "Saccade amplitude",
                   "Duration of Fixations",
                   "Count of Fixations",
                   "Years of general programming",
                   "Years of prof. programming",
                   "Java skill level",
                   "Total duration of exercise",
                   "Sureness",
                   "Correctness"
  )
  
  ggcorrplot(corr, type = "full", lab = TRUE, lab_size = 4, p.mat = corp,
             outline.color = "white", show.legend = FALSE,
             colors = c("#6D9EC1", "white", "#E46726")
             # colors = c("#F8696B", "#FFEB84", "#63BE7B")
  ) +
    scale_x_discrete(labels = axis_labels) +
    scale_y_discrete(labels = axis_labels) +
    theme(text = element_text(family = "serif"))
  ggsave_(paste("export/Corr_PerStimulus", ".pdf", sep=""), scaleY = 1.2)
  return(corr)
}

Corr_Metrics <- function(type) {
  df <- metrics %>%
    merge(surveyg, by=c("Participant.name", "Presented.Stimulus.name")) %>%
    mutate(Correct = ifelse(Correct == "Yes", 1, 0)) %>%
    select_if(is.numeric)
  corr <- df %>% cor() %>% round(1)
  corp <- df %>% cor_pmat()
  ggcorrplot(corr, type = "lower", lab = TRUE, lab_size = 3, p.mat = corp) +
    theme(text = element_text(family = "serif"))
  ggsave_(paste0("export/Corr_Metrics_", type, ".pdf"), scale = 2.5)
  return(df)
}

BP_SurveyOpinion <- function() {
  df <- survey %>% select(Participant.name, starts_with("Before"), starts_with("After"))
  df <- melt(setDT(df), measure = patterns("(After|Before)(.+)"),
             variable.name = "Type", value.name = "Value")
  
  BP(df, title = "In my opinion, highlighting helps me...", subtitle = NULL,
     x = factor(Type, levels = c("BeforeFindRelevantSymbols",
                                 "BeforeMemorize",
                                 "BeforeUnderstandStructure",
                                 "BeforeMoreEfficient",
                                 "AfterFindRelevantSymbols",
                                 "AfterMemorize",
                                 "AfterUnderstandStructure",
                                 "AfterMoreEfficient"),
                labels = c(
                  "Find relevant symbols (Before)",
                  "Memorize (Before)",
                  "Understand structure (Before)",
                  "Be more efficient (Before)",
                  "Find relevant symbols (After)",
                  "Memorize (After)",
                  "Understand structure (After)",
                  "Be more efficient (After)"
                )), xlab = "Type",
     y = Value, ylab = "Likert Scala (1-5)", limitY = FALSE,
     fill = Type,
     includeDataSize = FALSE, includeTitles = TRUE) +
    scale_fill_manual(values=orderedgroupcolors) +
    theme(legend.position = "none") +
    scale_x_discrete(name = "Type", limits=rev) +
    coord_flip()
  ggsave_(paste0("export/BP_SurveyOpinion", ".pdf"), scaleY = 0.75)
  return(df)
}

BP_EXPLANATION <- function(family = "serif"){
  
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  sample_df$values[101:200] <- 201:300
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x, 
                                     probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile", 
                          "50th percentile (median)",
                          "75th percentile")
    
    IQR <- diff(quartiles[c(1,3)])
    mean <- mean(x)
    
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile (median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "IQRPOS" = quartiles[1] + IQR/2,
                "mean" = mean,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  xs <- 1.40
  empty_df = sample_df[FALSE,]
  explain_plot <- BP(sample_df, title = "EXPLANATION", x = parameter, xlab = "", y = values, ylab = "", includeDataSize = FALSE) +
    # theme_minimal(base_size = 5, base_family = family) +
    theme_minimal() +
    geom_segment(aes(x = 2 * xs, xend = 2*xs, y = ggplot_output[["25th percentile"]], yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = xs, xend = 2*xs, y = ggplot_output[["25th percentile"]], yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = xs, xend = 2*xs, y = ggplot_output[["75th percentile"]], yend = ggplot_output[["75th percentile"]])) +
    geom_label(aes(x = xs*2 + 0.05, y = ggplot_output[["IQRPOS"]]), label = "Interquartile\nrange (IQR)",
               fontface = "bold", fill = "white", label.size = 0, vjust=0.5) +
    geom_label(data = empty_df, aes(x = c(xs,xs), y = c(ggplot_output[["upper_whisker"]], ggplot_output[["lower_whisker"]]),
                                    label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile", "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
               fontface = "bold", vjust = 0.5, fill = "white", label.size = 0) +
    geom_label(aes(x = c(xs), y =  ggplot_output[["lower_dots"]], label = "Outside value: Value is >1.5 times and\n<3 times the interquartile range\nbeyond either end of the box"),
               vjust = 0.5, fill = "white", label.size = 0) +
    # geom_label(aes(x = c(xs+0.8), y =  ggplot_output[["lower_dots"]], label = "-"), vjust = 0.5, fill = "white", label.size = 0) +
    # geom_label(aes(x = xs,
    #               y = ggplot_output[["lower_dots"]],
    #               label = ""),
    # vjust = 1.5, fill = "white", label.size = 0) +
    geom_label(data = empty_df, aes(x = xs, y = ggplot_output[["quartiles"]],
                                    label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.4,0.4),
               fill = "white", label.size = 0) +
    geom_label(aes(x = xs, y =  ggplot_output[["mean"]], label = "Average (mean)"), vjust = 0.4, fill = "white", label.size = 0) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3) +
    coord_cartesian(xlim = c(1.2,3.0), ylim = c(-500, 850))
  ggsave_(paste0("export/BP_EXPLANATION", ".pdf"), scaleX = 0.5)
  
  
  # Detach and reattach ggplot2
  detach("package:tidyverse", unload = TRUE)
  detach("package:ggcorrplot", unload = TRUE)
  detach("package:ggplot2", unload = TRUE)
  library(ggplot2)
  library(ggcorrplot)
  library(tidyverse)
}

M_Calibration <- function() {
  df <- dfraw %>% group_by(Participant.name, Presented.Stimulus.name, 
                           Average.calibration.accuracy..degrees.,
                           Average.validation.accuracy..degrees.,
                           Average.calibration.accuracy..pixels.,
                           Average.validation.accuracy..pixels.) %>%
    filter(Presented.Stimulus.name != "") %>%
    summarize() %>%
    group_by(Participant.name, Average.calibration.accuracy..degrees., Average.validation.accuracy..degrees.,
             Average.calibration.accuracy..pixels., Average.validation.accuracy..pixels.) %>%
    summarize(Presented.Stimulus.name = paste(Presented.Stimulus.name, collapse = ",")) %>%
    rename(Average.calibration.accuracy.degrees = Average.calibration.accuracy..degrees.,
           Average.validation.accuracy.degrees = Average.validation.accuracy..degrees.,
           Average.calibration.accuracy.pixels = Average.calibration.accuracy..pixels.,
           Average.validation.accuracy.pixels = Average.validation.accuracy..pixels.)
  write.table(df, file = "export/M_Calibration.txt", sep = "\t", row.names = FALSE, quote = FALSE)
  write.table(df, file = "export/M_Calibration.tsv", sep = "\t", row.names = FALSE)
  
  df <- melt(setDT(df), measure = patterns("Average(.+)"),
             variable.name = "Type", value.name = "Value", na.rm = TRUE)
  
  dfDeg <- df %>% filter(endsWith(as.character(Type), "degrees")) %>% 
    mutate(TypeString = ifelse(startsWith(as.character(Type), "Average.calibration"), "Calibration", "Validation"))
  BP(dfDeg, title = "Average Calibration Accuracy", subtitle = NULL,
     x = TypeString, xlab = "Type",
     y = Value, ylab = "Accuracy in Degrees", limitY = FALSE)
  ggsave_(paste0("export/M_Calibration_degrees", ".pdf"), scaleX = 0.5)
  
  dfPx <- df %>% filter(endsWith(as.character(Type), "pixels")) %>%
    mutate(TypeString = ifelse(startsWith(as.character(Type), "Average.calibration"), "Calibration", "Validation"))
  BP(dfPx, title = "Average Calibration Accuracy", subtitle = NULL,
     x = TypeString, xlab = "Type",
     y = Value, ylab = "Accuracy in Pixels", limitY = FALSE)
  ggsave_(paste0("export/M_Calibration_pixels", ".pdf"), scaleX = 0.5)
  
  return(df)
}

M_Performance <- function() {
  df <- surveyg
  
  ggplot(data = df, mapping = aes(x = Participant.name, y = factor(Presented.Stimulus.name, levels = stimulis),
                                  shape = Correct, color = Sureness, size = Time/1000/60, label = format(Time/1000/60, digits=2))) +
    geom_point() + 
    # geom_text(color = "white", size = 3) +
    geom_text(size = 3, vjust = -1.5, colour="black") +
    scale_x_discrete(name = "Participant") +
    scale_y_discrete(name = "Stimulus") +
    scale_shape_manual(values = c(15,16)) +
    labs(title = paste("Ratio of Fixation vs Total - ")) +
    theme(text = element_text(family = "serif"))
  
  ggsave_(paste0("export/M_Performance", ".pdf"))
  return(df)
}

M_Correctness <- function() {
  df <- surveyg %>%
    mutate(Exercise = substr(Presented.Stimulus.name, 1, 1), Group = paste0("Group ", Group)) %>%
    rename(Participant = Participant.name)
  
  ggplot(data = df, mapping = aes(x = Participant, y = Exercise,
                                  fill = Correct, alpha = Sureness)) +
    geom_raster() + 
    facet_wrap(~ Group, scales = "free") +
    # coord_equal() +
    # labs(title = paste("Correctness and Sureness of Answers")) +
    theme_minimal() +
    theme(text = element_text(family = "serif"))
  
  ggsave_(paste0("export/M_Correctness", ".pdf"), scaleY = 0.6)
  return(df)
}

T_Best <- function() {
  df <- dfg %>%
    merge(surveyg, by=c("Participant.name", "Presented.Stimulus.name")) %>%
    mutate(Highlighting = str_sub(Presented.Stimulus.name, 2),
           Correct = ifelse(Correct == "Yes", 1, 0)) %>% 
    group_by(Highlighting)
  
  df2 <- df %>% summarize(AFD = mean(dur[Eye.movement.type == "Fixation"]),
                          ASD = mean(dur[Eye.movement.type == "Saccade"]),
                          ASA = mean(SacAmp[Eye.movement.type == "Saccade"], na.rm = TRUE),
                          Time = mean(Time/1000/60),
                          Correctness = mean(Correct),
                          Sureness = mean(Sureness)
  ) %>%
    mutate(Highlighting = paste("Highlighting", Highlighting)) %>%
    mutate_if(is.numeric, function(x) {
      return(format(x, digits = 3, nsmall = 3))
    })
  
  row.names(df2) <- df2$Highlighting
  t_df2 <- t(df2)[-1,]
  print(t_df2)
  print(xtable(t_df2, type = "latex", caption = "Direct comparison of the most important metrics over the Highlighting schemes", label = "tab:best"))
  return(df)
}
# tmp <- T_Best()
# print(tmp)
# tmp <- T_Best() %>% filter(Eye.movement.type == "Fixation") %>% mutate(Highlighting = toString(Highlighting))
# res.aov <- aov(dur ~ Highlighting, data = tmp)
# print(summary(res.aov))
# print(TukeyHSD(res.aov))

T_ShapiroWilk <- function() {
  df <- dfg %>%
    merge(surveyg, by=c("Participant.name", "Presented.Stimulus.name")) %>%
    mutate(Highlighting = str_sub(Presented.Stimulus.name, 2),
           Correct = ifelse(Correct == "Yes", 1, 0),
           Presented.Stimulus.name = paste0(Presented.Stimulus.name, "   ")) %>% 
    group_by(Group, Participant.name, Highlighting) %>%
    # group_by(Group, Participant.name, Highlighting, Presented.Stimulus.name) %>%
    summarize(AFD = mean(dur[Eye.movement.type == "Fixation"]),
              ASD = mean(dur[Eye.movement.type == "Saccade"]),
              ASA = mean(SacAmp[Eye.movement.type == "Saccade"], na.rm = TRUE),
              Time = mean(Time/1000/60),
              Correctness = mean(Correct),
              Sureness = mean(Sureness),
              count = n(),
              sd = sd(dur[Eye.movement.type == "Fixation"])) %>%
    arrange(Participant.name, Highlighting)
  # results <- data.frame(metric = c("AFD", "ASD", "ASA", "Time", "Correctness", "Sureness")) %>%
  #   mutate(sw = shapiro.test(metric)) %>%
  #   mutate(W = sw$W, p = sw$p.value) %>%
  #   select(-sw)
  Metrics <- c("AFD", "ASD", "ASA", "Time", "Correctness", "Sureness")
  W <- c()
  p.value.shapiro <- c()
  p.value.kruskal <- c()
  for(metric in Metrics) {
    print(paste("#####", metric, "#####"))
    ress <- shapiro.test(df[[metric]])
    W <- append(W, format(ress$statistic, digits = 3))
    p.value.shapiro <- append(p.value.shapiro, format(ress$p.value, digits = 3))
    
    resk <- kruskal.test(df[[metric]], df$Highlighting, tmp)
    p.value.kruskal <- append(p.value.kruskal, format(resk$p.value, digits = 3))
    if (metric %in% c("ASD", "ASA")) {
      ptt <- pairwise.t.test(df[[metric]], df$Highlighting, paired = TRUE)
      print(ptt)
    } else {#if (metric == "Correctness") {
      pwt <- pairwise.wilcox.test(df[[metric]], df$Highlighting, p.adj = "BH", paired = TRUE)
      print(pwt)
    }
  }
  df2 <- data.frame(Metrics, W, p.value.shapiro, p.value.kruskal)
  print(xtable(df2, type = "latex", caption = "Values of the Shapiro-Wilk test", label = "tab:shapiro-wilk-test"), include.rownames = FALSE)
  return(df)
}

# ggqqplot(tmp$ASA)


# sw <- T_ShapiroWilk()
# measure = tmp$AFD
# ggdensity(measure)
# ggqqplot(measure)
# print(shapiro.test(measure))
# print(kruskal.test(AFD ~ Presented.Stimulus.name, tmp))
# print(wilcox.test(tmp$AFD[tmp$Presented.Stimulus.name == "p3"],
#                   tmp$AFD[tmp$Presented.Stimulus.name == "d4"], paired = TRUE))
# print(pwt)