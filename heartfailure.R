library(tidyverse)
library(skimr)      
library(ggthemes)
library(patchwork)  
library(corrplot)

library(rsample)    
#library(DescTools)  
library(pglm)
library(sjPlot)     
library(caret)      
library(mlr)        
library(rpart)      
library(rpart.plot)
library(ranger)     
library(lightgbm)   

# load data 
ROOT = "../heartfailure"
palette_ro = c("#ee2f35", "#fa7211", "#fbd600", "#75c731", "#1fb86e", "#0488cf", "#7b44ab")
df = read_csv(str_c(ROOT, "/heart_failure_clinical_records_dataset.csv"))


# Dataset
head(df, 50) %>% 
  DT::datatable()

# Data size and structure
glimpse(df)

# Data summary
skim(df)

# Data visualizations
f_features = c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking", "DEATH_EVENT")
df_n = df
df = df %>% mutate_at(f_features, as.factor)

# Distribution of the binary features 
## Features vs target (count)
p1 = ggplot(df, aes(x = anaemia, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)"))+
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Anaemia") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p2 = ggplot(df, aes(x = diabetes, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Diabetes") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p3 = ggplot(df, aes(x = high_blood_pressure, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "High blood pressure") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p4 = ggplot(df, aes(x = sex, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (Female)", "1 (Male)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Sex") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p5 = ggplot(df, aes(x = smoking, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Smoking") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

p6 = ggplot(df, aes(x = DEATH_EVENT, fill = DEATH_EVENT)) +
  geom_bar(stat = "count", position = "stack", show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  geom_label(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5),
             size = 5, show.legend = FALSE)

((p1 + p2 + p3) / (p4 + p5 + p6)) +
  plot_annotation(title = "Distribution of the binary features and DEATH_EVENT")

# Features vs target (percentage)
p1 = ggplot(df, aes(y = reorder(anaemia, as.numeric(anaemia) * -1), fill = DEATH_EVENT)) +
  geom_bar(position = "fill", show.legend = FALSE) + 
  scale_y_discrete(labels  = c("1 (True)", "0 (False)"))+
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(subtitle = "Anaemia") +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_blank(), axis.text.x = element_blank())

p2 = ggplot(df, aes(y = reorder(diabetes, as.numeric(diabetes) * -1), fill = DEATH_EVENT)) +
  geom_bar(position = "fill", show.legend = FALSE) + 
  scale_y_discrete(labels  = c("1 (True)", "0 (False)"))+
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(subtitle = "Diabetes") +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_blank(), axis.text.x = element_blank())

p3 = ggplot(df, aes(y = reorder(high_blood_pressure, as.numeric(high_blood_pressure) * -1), fill = DEATH_EVENT)) +
  geom_bar(position = "fill", show.legend = FALSE) + 
  scale_y_discrete(labels  = c("1 (True)", "0 (False)"))+
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(subtitle = "High blood pressure") +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_blank(), axis.text.x = element_blank())

p4 = ggplot(df, aes(y = reorder(sex, as.numeric(sex) * -1), fill = DEATH_EVENT)) +
  geom_bar(position = "fill", show.legend = FALSE) + 
  scale_y_discrete(labels  = c("1 (Male)", "0 (Female)"))+
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(subtitle = "Sex") +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_blank(), axis.text.x = element_blank())

p5 = ggplot(df, aes(y = reorder(smoking, as.numeric(smoking) * -1), fill = DEATH_EVENT)) +
  geom_bar(position = "fill", show.legend = TRUE) + 
  scale_y_discrete(labels  = c("1 (True)", "0 (False)"))+
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  labs(subtitle = "Smoking") +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_blank(), legend.position = "bottom", legend.direction = "horizontal") +
  guides(fill = guide_legend(reverse = TRUE))

(p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 1)) +
  plot_annotation(title = "Distribution of the binary features and DEATH_EVENT")

# Age
p1 = ggplot(df, aes(x = age)) + 
  geom_histogram(binwidth = 5, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 5))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  scale_x_continuous(breaks = seq(40, 100, 10)) +
  geom_vline(xintercept = median(df$age), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$age)-5, y = 50,
           label = str_c("Min.     : ", min(df$age),
                         "\nMedian : ", median(df$age),
                         "\nMean    : ", round(mean(df$age), 1),
                         "\nMax.    : ", max(df$age))) +
  labs(title = "age distribution") +
  theme_minimal(base_size = 12)

p2 = ggplot(df, aes(x = age, fill = DEATH_EVENT)) + 
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  scale_x_continuous(breaks = seq(40, 100, 10)) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$age), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$age), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$age)-10, y = 0.03,
           label = str_c("Survived median: ", median(filter(df, DEATH_EVENT == 0)$age),
                         "\nDead median: ", median(filter(df, DEATH_EVENT == 1)$age))) +
  
  labs(title = "Relationship between age and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p1 / p2

# Creatinine phosphokinase
p1 = ggplot(df, aes(x = creatinine_phosphokinase)) + 
  geom_histogram(binwidth = 100, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 100))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  geom_vline(xintercept = median(df$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$creatinine_phosphokinase)-1000, y = 75,
           label = str_c("Min.     : ", min(df$creatinine_phosphokinase),
                         "\nMedian : ", median(df$creatinine_phosphokinase),
                         "\nMean    : ", round(mean(df$creatinine_phosphokinase), 1),
                         "\nMax.    : ", max(df$creatinine_phosphokinase))) +
  labs(title = "creatinine_phosphokinase distribution") +
  theme_minimal(base_size = 12)

p2 = ggplot(df, aes(x = creatinine_phosphokinase, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$creatinine_phosphokinase)-1400, y = 0.0015,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$creatinine_phosphokinase),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$creatinine_phosphokinase))) +
  
  labs(title = "Relationship between creatinine_phosphokinase and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p3 = ggplot(df, aes(x = creatinine_phosphokinase, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$creatinine_phosphokinase), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$creatinine_phosphokinase)-4500, y = 0.7,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$creatinine_phosphokinase),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$creatinine_phosphokinase))) +
  
  labs(title = "Relationship between creatinine_phosphokinase and DEATH_EVENT (log scale)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  scale_x_log10() +
  annotation_logticks()

p1 / p2 / p3


# Ejection fraction
p1 = ggplot(df, aes(x = ejection_fraction)) + 
  geom_histogram(binwidth = 1, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 1))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  scale_x_continuous(breaks = seq(10, 80, 10)) +
  geom_vline(xintercept = median(df$ejection_fraction), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$ejection_fraction)-6, y = 45,
           label = str_c("Min.     : ", min(df$ejection_fraction),
                         "\nMedian : ", median(df$ejection_fraction),
                         "\nMean    : ", round(mean(df$ejection_fraction), 1),
                         "\nMax.    : ", max(df$ejection_fraction))) +
  labs(title = "ejection_fraction distribution") +
  theme_minimal(base_size = 12)

p2 = ggplot(df, aes(x = ejection_fraction, fill = DEATH_EVENT)) + 
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  scale_x_continuous(breaks = seq(10, 80, 10)) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$ejection_fraction), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$ejection_fraction), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$age)-26, y = 0.045,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$ejection_fraction),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$ejection_fraction))) +
  
  labs(title = "Relationship between ejection_fraction and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p1 / p2

# Platelets
p1 = ggplot(df, aes(x = platelets)) + 
  geom_histogram(binwidth = 20000, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 20000))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  geom_vline(xintercept = median(df$platelets), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$platelets)-100000, y = 40,
           label = str_c("Min.     : ", min(df$platelets),
                         "\nMedian : ", median(df$platelets),
                         "\nMean    : ", round(mean(df$platelets), 1),
                         "\nMax.    : ", max(df$platelets))) +
  labs(title = "platelets distribution") +
  theme_minimal(base_size = 12)

p2 = ggplot(df, aes(x = platelets, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$platelets), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$platelets), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$platelets)-180000, y = 0.000005,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$platelets),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$platelets))) +
  
  labs(title = "Relationship between platelets and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p1 / p2

# Serum creatinine
p1 = ggplot(df, aes(x = serum_creatinine)) + 
  geom_histogram(binwidth = 0.2, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 0.2))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  geom_vline(xintercept = median(df$serum_creatinine), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$serum_creatinine)-1, y = 70,
           label = str_c("Min.     : ", min(df$serum_creatinine),
                         "\nMedian : ", median(df$serum_creatinine),
                         "\nMean    : ", round(mean(df$serum_creatinine), 1),
                         "\nMax.    : ", max(df$serum_creatinine))) +
  labs(title = "serum_creatinine distribution") +
  theme_minimal(base_size = 12)

p2 = ggplot(df, aes(x = serum_creatinine, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$serum_creatinine), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$serum_creatinine), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$serum_creatinine)-1.6, y = 1.25,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$serum_creatinine),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$serum_creatinine))) +
  
  labs(title = "Relationship between serum_creatinine and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p3 = ggplot(df, aes(x = serum_creatinine, fill = factor(DEATH_EVENT))) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$serum_creatinine), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$serum_creatinine), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$serum_creatinine)-3.2, y = 3,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$serum_creatinine),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$serum_creatinine))) +
  
  labs(title = "Relationship between serum_creatinine and DEATH_EVENT (log scale)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  scale_x_log10()

p1 / p2 / p3

# Serum sodium
p1 = ggplot(df, aes(x = serum_sodium)) + 
  geom_histogram(binwidth = 1, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 1))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  scale_x_continuous(breaks = seq(110, 150, 10)) +
  geom_vline(xintercept = median(df$serum_sodium), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = min(df$serum_sodium)+4, y = 36,
           label = str_c("Min.     : ", min(df$serum_sodium),
                         "\nMedian : ", median(df$serum_sodium),
                         "\nMean    : ", round(mean(df$serum_sodium), 1),
                         "\nMax.    : ", max(df$serum_sodium))) +
  labs(title = "serum_sodium distribution") +
  theme_minimal(base_size = 12)

p2 = ggplot(df, aes(x = serum_sodium, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  scale_x_continuous(breaks = seq(110, 150, 10)) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$serum_sodium), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$serum_sodium), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = min(df$serum_sodium)+5, y = 0.1,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$serum_sodium),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$serum_sodium))) +
  
  labs(title = "Relationship between serum_sodium and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p1 / p2

# Time
p1 = ggplot(df, aes(x = time)) + 
  geom_histogram(binwidth = 10, colour = "white", fill = palette_ro[6], alpha = 0.5) +
  geom_density(eval(bquote(aes(y = ..count.. * 10))), colour = palette_ro[6], fill = palette_ro[6], alpha = 0.25) +
  scale_x_continuous(breaks = seq(0, 300, 50)) +
  geom_vline(xintercept = median(df$time), linetype="longdash", colour = palette_ro[6]) +
  annotate(geom = "text",
           x = max(df$time)-30, y = 22,
           label = str_c("Min.     : ", min(df$time),
                         "\nMedian : ", median(df$time),
                         "\nMean    : ", round(mean(df$time), 1),
                         "\nMax.    : ", max(df$time))) +
  labs(title = "time distribution") +
  theme_minimal(base_size = 12)

p2 = ggplot(df, aes(x = time, fill = DEATH_EVENT)) +
  geom_density(alpha = 0.64) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "DEATH_EVENT",
                    labels = c("0 (False)", "1 (True)")) +
  scale_x_continuous(breaks = seq(0, 300, 50)) +
  
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 0)$time), linetype="longdash", colour = palette_ro[2]) +
  geom_vline(xintercept = median(filter(df, DEATH_EVENT == 1)$time), linetype="longdash", colour = palette_ro[7]) +
  annotate(geom = "text",
           x = max(df$time)-50, y = 0.008,
           label = str_c("Survived Median: ", median(filter(df, DEATH_EVENT == 0)$time),
                         "\nDead Median: ", median(filter(df, DEATH_EVENT == 1)$time))) +
  
  labs(title = "Relationship between time and DEATH_EVENT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

p1 / p2

ggplot(df, aes(x = serum_creatinine, y = ejection_fraction, colour = DEATH_EVENT)) +
  geom_point() +
  geom_abline(intercept=0, slope=15, colour="grey25", linetype="dashed") +
  scale_colour_manual(values = c(palette_ro[2], palette_ro[7]),
                      name = "parient status\n(DEATH_EVENT)",
                      labels = c("survived", "dead")) +
  labs(title = "Scatterplot of serum_creatinine versus ejection_fraction") +
  theme_minimal(base_size = 12)

# correlation matrix
cor(df_n) %>%
  corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
           addCoef.col = TRUE,
           p.mat = cor.mtest(df_n)$p,
           sig.level = 0.05)

cor(df_n) %>%
  corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
           p.mat = cor.mtest(df_n)$p,
           insig = "p-value", sig.level = -1)

# Data processing 
set.seed(777)
df_split = initial_split(df, prop = 4/5, strata = DEATH_EVENT)
train = training(df_split)
test = testing(df_split)

set.seed(777)
df_n_split = initial_split(df_n, prop = 4/5, strata = DEATH_EVENT)
train_n = training(df_n_split)
test_n = testing(df_n_split)

# F1 score 
f1 = function(precision,recall){
  f1 = 2*(precision*recall)/(precision+recall)
  return(f1)
}

# Logistic Regression
lr1 = glm(DEATH_EVENT ~ .,
          family=binomial(logit), data=train)
tab_model(lr1, show.r2 = FALSE, transform = NULL,
          digits = 3, digits.p = 4)

lr2 = step(lr1)
tab_model(lr2, show.r2 = FALSE, transform = NULL,
          digits = 3, digits.p = 4)

lr3 = glm(DEATH_EVENT ~ ejection_fraction + serum_creatinine + serum_sodium + time + time*age + high_blood_pressure * time, family=binomial(logit), data=train)
lr4 = step(lr3)
tab_model(lr4, show.r2 = FALSE, transform = NULL, digits = 3, digits.p = 4)

odds = c(round( exp(lr4$coefficients["age"]*10), digits=3 ),
         round( exp(lr4$coefficients["ejection_fraction"]), digits=3 ),
         round( exp(lr4$coefficients["serum_creatinine"]), digits=3 ),
         round( exp(lr4$coefficients["serum_sodium"]), digits=3 ),
         round( exp(lr4$coefficients["time"]*7), digits=3 ))


data.frame(variables = names(odds), odds = odds) %>%
  mutate(description = c("Odds ratio of death for age 10 years older",
                         "Odds ratio of death if ejection fraction id 1% higher",
                         "Odds ratio of death if serum creatinine level is 1 mg/dL higher",
                         "Odds ratio of death if serum sodium level is 1 mg/dL higher",
                         "Odds ratio of death with 1 week (7 days) longer follow-up time"))


pred = as.factor(predict(lr4, newdata=test, type="response") >= 0.5) %>%
  fct_recode("0" = "FALSE", "1" = "TRUE")
confusionMatrix(pred, test$DEATH_EVENT, positive = "1")

acc_lr = confusionMatrix(pred, test$DEATH_EVENT)$overall["Accuracy"]
rcl_lr = confusionMatrix(pred, test$DEATH_EVENT)$byClass["Specificity"]
prc_lr = confusionMatrix(pred, test$DEATH_EVENT)$byClass["Pos Pred Value"] # precision
f1_lr = f1(prc_lr,rcl_lr)


# SVM (Support vector machine)
task_train = makeClassifTask(data=train, target="DEATH_EVENT")
task_test = makeClassifTask(data=test, target="DEATH_EVENT")
task_df = makeClassifTask(data=df, target="DEATH_EVENT")

set.seed(777)
lrn_svm = makeLearner("classif.svm", predict.type = "prob", par.vals = list(kernel="linear", cost=0.1))
svm = train(lrn_svm, task_train)

plotLearnerPrediction(lrn_svm, task_df, features = c("serum_creatinine", "ejection_fraction")) +
  scale_fill_manual(values=c(palette_ro[2], palette_ro[7])) +
  theme_minimal(base_size = 12)

pred = getPredictionResponse(predict(svm, task_test))
confusionMatrix(pred, test$DEATH_EVENT, positive = "1")

acc_svm = confusionMatrix(pred, test$DEATH_EVENT)$overall["Accuracy"]
rcl_svm = confusionMatrix(pred, test$DEATH_EVENT)$byClass["Specificity"]
prc_svm = confusionMatrix(pred, test$DEATH_EVENT)$byClass["Pos Pred Value"]
f1_svm = f1(prc_svm,rcl_svm)


## Decision tree
set.seed(777)
cart = rpart(DEATH_EVENT ~ .,
             data = train, method = "class",
             control=rpart.control(minsplit=10,
                                   minbucket=5,
                                   maxdepth=10,
                                   cp=0.03))
prp(cart,
    type = 4,
    extra = 101,
    nn = TRUE,
    tweak = 1.0,
    space = 0.1,
    shadow.col = "grey",
    col = "black",
    split.col = palette_ro[5],
    branch.col = palette_ro[4],
    fallen.leaves = FALSE,
    roundint = FALSE,
    box.col = c(palette_ro[2], palette_ro[7])[cart$frame$yval])

pred = as.factor(predict(cart, newdata=test)[, 2] >= 0.5) %>%
  fct_recode("0" = "FALSE", "1" = "TRUE")
confusionMatrix(pred, test$DEATH_EVENT, positive = "1")

set.seed(777)
cart = rpart(DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + serum_sodium + time,
             data = train, method = "class",
             control=rpart.control(minsplit=20,
                                   minbucket=10,
                                   maxdepth=10,
                                   cp=0.03))

prp(cart,
    type = 4,
    extra = 101,
    nn = TRUE,
    tweak = 1.0,
    space = 0.1,
    shadow.col = "grey",
    col = "black",
    split.col = palette_ro[5],
    branch.col = palette_ro[4],
    fallen.leaves = FALSE,
    roundint = FALSE,
    box.col = c(palette_ro[2], palette_ro[7])[cart$frame$yval])

pred = as.factor(predict(cart, newdata=test)[, 2] >= 0.5) %>%
  fct_recode("0" = "FALSE", "1" = "TRUE")
confusionMatrix(pred, test$DEATH_EVENT, positive = "1")

acc_cart = confusionMatrix(pred, test$DEATH_EVENT)$overall["Accuracy"]
rcl_cart = confusionMatrix(pred, test$DEATH_EVENT)$byClass["Specificity"]
prc_cart = confusionMatrix(pred, test$DEATH_EVENT)$byClass["Pos Pred Value"]
f1_cart = f1(prc_cart,rcl_cart)


set.seed(777)
cart = rpart(DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + serum_sodium,
             data = train, method = "class",
             control=rpart.control(minsplit=10,
                                   minbucket=5,
                                   maxdepth=10,
                                   cp=0.01))

prp(cart,
    type = 4,
    extra = 101,
    nn = TRUE,
    tweak = 1.0,
    space = 0.1,
    shadow.col = "grey",
    col = "black",
    split.col = palette_ro[5],
    branch.col = palette_ro[4],
    fallen.leaves = FALSE,
    roundint = FALSE,
    box.col = c(palette_ro[2], palette_ro[7])[cart$frame$yval])

pred = as.factor(predict(cart, newdata=test)[, 2] >= 0.5) %>%
  fct_recode("0" = "FALSE", "1" = "TRUE")
confusionMatrix(pred, test$DEATH_EVENT, positive = "1")

# Random Forest
set.seed(777)
rf = ranger(DEATH_EVENT ~.,
            data = train,
            mtry = 2, num.trees = 500, write.forest=TRUE, importance = "permutation")

data.frame(variables = names(importance(rf, method = "janitza")),
           feature_importance = importance(rf, method = "janitza")) %>%
  ggplot(aes(x = feature_importance,
             y = reorder(variables, X = feature_importance))) +
  geom_bar(stat = "identity",
           fill = palette_ro[6],
           alpha=0.9) +
  labs(y = "features", title = "Feature importance of random forest") +
  theme_minimal(base_size = 12)

pred = predict(rf, data=test)$predictions
confusionMatrix(pred, test$DEATH_EVENT, positive = "1")

acc_rf = confusionMatrix(pred, test$DEATH_EVENT)$overall["Accuracy"]
rcl_rf = confusionMatrix(pred, test$DEATH_EVENT)$byClass["Specificity"]
prc_rf = confusionMatrix(pred, test$DEATH_EVENT)$byClass["Pos Pred Value"]
f1_rf = f1(prc_rf,rcl_rf)

set.seed(777)
rf = ranger(DEATH_EVENT ~ age + ejection_fraction + serum_creatinine,
            data = train,
            mtry = 3, num.trees = 100, write.forest=TRUE, importance = "permutation")


data.frame(variables = names(importance(rf, method = "janitza")),
           feature_importance = importance(rf, method = "janitza")) %>%
  ggplot(aes(x = feature_importance,
             y = reorder(variables, X = feature_importance))) +
  geom_bar(stat = "identity",
           fill = palette_ro[6],
           alpha=0.9) +
  labs(y = "features", title = "Feature importance of random forest") +
  theme_minimal(base_size = 12)


pred = predict(rf, data=test)$predictions
confusionMatrix(pred, test$DEATH_EVENT, positive = "1")

# GBDT (Gradient Boosting Decision Tree)
train_lgb = lgb.Dataset(data = as.matrix(select(train_n, -DEATH_EVENT)), label = train_n$DEATH_EVENT)

set.seed(777)
lgb = lightgbm(data = train_lgb, objective = "binary", verbosity = -1,
               learning_rate = 0.1,
               colsample_bytree = 0.8)

lgb.importance(lgb) %>%
  ggplot(aes(x = Gain,
             y = reorder(Feature, X = Gain))) +
  geom_bar(stat = "identity",
           fill = palette_ro[6],
           alpha=0.9) +
  labs(x ="feature_importance", y = "features", title = "Feature importance of LightGBM") +
  theme_minimal(base_size = 12)


pred = as.factor(predict(lgb, as.matrix(select(test_n, -DEATH_EVENT))) >= 0.5) %>%
  fct_recode("0" = "FALSE", "1" = "TRUE")
confusionMatrix(pred, as.factor(test_n$DEATH_EVENT), positive = "1")

acc_lgb = confusionMatrix(pred, as.factor(test_n$DEATH_EVENT))$overall["Accuracy"]
rcl_lgb = confusionMatrix(pred, as.factor(test_n$DEATH_EVENT))$byClass["Specificity"]
prc_lgb = confusionMatrix(pred, test$DEATH_EVENT)$byClass["Pos Pred Value"]
f1_lgb = f1(prc_lgb,rcl_lgb)


# Result
pastel_colors = c("#6490ED", "#ADD8E6", "#2282B7", "#00008B")

data.frame(algorithm = c("logistic\nregression", "SVM", "decision\ntree", "random\nforest", "LightGBM"),
           Accuracy = c(acc_lr, acc_svm, acc_cart, acc_rf, acc_lgb)*100,
           Recall = c(rcl_lr, rcl_svm, rcl_cart, rcl_rf, rcl_lgb)*100, 
           Precision = c(prc_lr, prc_svm, prc_cart, prc_rf, prc_lgb)*100,
           F1 = c(f1_lr, f1_svm, f1_cart, f1_rf, f1_lgb)*100) %>%
  pivot_longer(col = -algorithm, names_to = "metrics", values_to = "percent") %>%
  ggplot(aes(x = reorder(algorithm, X = percent), y = percent, fill = metrics)) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.9) +
  geom_text(aes(group = metrics, label = str_c(sprintf("%2.1f", percent), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.2, size = 2.2) +
  scale_fill_manual(values = pastel_colors) +
  labs(x = "algorithm", title = "Metrics of different classifier models") +
  theme_minimal(base_size = 12)