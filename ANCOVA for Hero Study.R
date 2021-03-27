library(ggpubr)
library(tidyverse)

### ---
### Read in clean data (data cleaned in Excel)
### All data standardized for analyses
### Outliers removed
### NOTE: Outcome variables are difference scores from pre to post
### ---

df1 <- read.csv("Hero Data.csv")

### ---
### Guide to outcome variables:
### SE = Self-esteem
### PWB = Well-being
### IRI = Interpersonal reactivity index
### Lonely = Loneliness
### Hope = Hope
### Creativity = Creativity
### Inspiration = Inspiration
### ---

### ---
### Guide to covariates
### InspirPower = Inspirational power of chosen expressive writing figure
### Closeness = Rated closeness to chosen inspirational figure
### Depth = Qualitatively assessed depth of expressive writing
### ---

### ---
### Run ANCOVA analyses for each outcome variable
### 4 experimental groups (factual, fictional, historical, celebrity)
### 1 control group (write about schedule)
### ---

### Self-esteem ANCOVA
res1 <- aov(NormImpSE ~ InspirPower + Closeness + Depth + Group, data = df1)
summary(res1)

### Well-being ANCOVA
res2 <- aov(NormImpPWB ~ InspirPower + Closeness + PrePWB + Depth + Group, data = df1)
summary(res2)

### IRI ANCOVA
res3 <- aov(NormImpIRI ~ Group*Closeness, data = df1)
summary(res3)

### Loneliness ANCOVA
res4 <- aov(NormImpLonely ~ InspirPower + Closeness + Group, data = df1)
summary(res4)

### Hope ANCOVA
### Significant!
res5 <- aov(NormImpHope ~ InspirPower + Closeness + PreHope + Group, data = df1)
summary(res5)

### Creativity ANCOVA
res6 <- aov(NormImpCreativity ~ InspirPower + Closeness + Depth + Group, data = df1)
summary(res6)

### Inspiration ANCOVA
res7 <- aov(NormImpInspiration ~ InspirPower + Closeness + Depth + Group, data = df1)
summary(res7)
