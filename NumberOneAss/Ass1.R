codebook <- read.csv("green_roof_plants_codebook.csv")
data <- read.csv("green_roof_plants_data.csv")
data <- data.frame(data)
attach(data)
#Use a table and boxplots to describe the roof biodiversity
#and planting outcomes variables for all sites combined
#(i.e. not grouped by year, climate type, or other variables), 
#considering measures of center and spread.

boxplot(species_richness)
quantile(species_richness, probs = c(0, 0.25, 0.50, 0.75, 1))
boxplot(shannon_diversity)
quantile(shannon_diversity)

#roof biodiversity variables
species_richness_norm <- rnorm(200, mean=mean(species_richness, na.rm = TRUE), sd = sd(species_richness, na.rm = TRUE))
shannon_diversity_norm <- rnorm(200, mean=mean(shannon_diversity, na.rm = TRUE), sd = sd(shannon_diversity, na.rm = TRUE))
evenness_norm <- rnorm(200, mean = mean(evenness, na.rm = TRUE), sd = sd(evenness, na.rm = TRUE))
#planting outcomes variables
bare_substrate_norm <- rnorm(200, mean = mean(bare_substrate, na.rm = TRUE), sd = sd(bare_substrate, na.rm = TRUE))
total_plant_cover_norm <- rnorm(200, mean = mean(total_plant_cover, na.rm = TRUE), sd = sd(total_plant_cover, na.rm = TRUE))
vascular_plant_cover_norm <- rnorm(200, mean = mean(vascular_plant_cover, na.rm = TRUE), sd = sd(vascular_plant_cover, na.rm = TRUE))
moss_norm <- rnorm(200, mean = mean(moss, na.rm = TRUE), sd = sd(moss, na.rm = TRUE))

boxplot(species_richness, species_richness_norm, shannon_diversity, shannon_diversity_norm, evenness, evenness_norm,
        bare_substrate, bare_substrate_norm, total_plant_cover, total_plant_cover_norm, vascular_plant_cover,
        vascular_plant_cover_norm, moss, moss_norm,
        main = "Roof biodiversity and planting outcomes variables for all sites combined", 
        at = c(1,2,4,5,7,8,10,11,13,14,16,17,19,20), 
        names = c("Sr", "normal", "Sd", "normal", "E", "normal", "BS", "normal",
                  "TPC", "normal", "VPC", "normal", "m", "normal"), 
        las = 2,
        col = c("orange", "red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)
legend("topright", c("Species richness(SR)", "Shannon Diversity (SD)", 
                     "Evenness (E)", "Bare Substrate (BS)", 
                     "Total Plant Cover (TPC)", "Vascular plant cover (VPC)", "moss (Moss)"),
                      border = "black", fill = c("orange"),
                      c("Normal"),
                      border = "black",
                      fill = c("red"))
legend("rightbottom", c("Normal, Normal distribution with the same mean and standard deviation"),
       border = "black", fill = c("red"))


#How do measurements of planting outcomes differ between years? 
#Note: due to data collection challenges, the specific plots cannot be assumed to be the same across years.

#In order to compare in one boxplot, i reshape the data and extract data per year per variable

bs_2016 <- bare_substrate[year == 2016] #bs is bare_substrate
bs_2017 <- bare_substrate[year == 2017]
tpc_2016 <- total_plant_cover[year == 2016] #tpc is total plant cover
tpc_2017 <- total_plant_cover[year == 2017]
vpc_2016 <- vascular_plant_cover[year == 2016] #vpc is vascular plant cover
vpc_2017 <- vascular_plant_cover[year == 2017]
moss_2016 <- moss[year == 2016]
moss_2017 <- moss[year == 2017]

boxplot(bs_2016, bs_2017, tpc_2016, tpc_2017, vpc_2016, vpc_2017, moss_2016, moss_2017,
        main = "Planting outcome variables for each year",
        at = c(1,2,4,5,7,8,10,11),
        names = c("BS 2016", "Bs 2017", 
                  "TPC 2016", "TPC 2017",
                  "VPC 2016", "VPC 2017",
                  "Moss 2016", "Moss 2017"),
        xlab = "year",
        col = c("orange", "orange", "red", "red", "blue", "blue", "green", "green"),
        border = "black")
legend("topright", c("Bare Substrate (BS)", "Total Plant Cover (TPC)", 
                     "Vascular plant cover (VPC)", "Moss (Moss)"),
       border = "black",
       fill = c("orange", "red", "blue", "green"))

boxplot(bare_substrate~year,
        data=data,
        main = "bare_substrate for each year",
        xlab = "Year",
        ylab = "Percenßtage of Bare substrate",
        col = "orange",
        border = "brown")

boxplot(bare_substrate~year, total_plant_cover~year, vascular_plant_cover~year, moss~year,
         main = "Planting outcome variables for each year",
         at = c(1,2,3,4),
         names = c("Bare substrate (%)", "Total plant cover (%)",
                   "Vascular plant cover (%)", "Moss cover ('%)",),
         xlab = "Year",
         col = c("orange", "red"),
         border = "black",
         fill = c("red")
         )





#normality
shapiro.test(data$total_plant_cover[data$year == 2016])
shapiro.test(data$total_plant_cover[data$year == 2017])
#Q-Q plot
qqnorm(data$total_plant_cover[year == 2016], main = 'qq plot normality total plant cover 2016', col = 'blue')
qqline(data$total_plant_cover[year == 2016], col = 'red')
qqnorm(data$total_plant_cover[year == 2017], main = 'qq plot normality total plant cover 2017', col = 'blue')
qqline(data$total_plant_cover[year == 2017], col = 'red')

#Wilcoxn signed rank
length(total_plant_cover[year == 2016])
length(total_plant_cover[year == 2017])
tpc_2016 <- na.omit(total_plant_cover[year == 2016])
tpc_2017 <- na.omit(total_plant_cover[year == 2017])
min_length <- min(length(tpc_2016), length(tpc_2017))
tpc_2016 <- tpc_2016[1:min_length]
tpc_2017 <- tpc_2017[1:min_length]
paired_wilcox <- wilcox.test(tpc_2017, tpc_2016, mu = 0, var.equal = TRUE, paired = FALSE, alternative = "two.sided" )
paired_wilcox

#normality
shapiro.test(data$vascular_plant_cover[data$year == 2016])
shapiro.test(data$vascular_plant_cover[data$year == 2017])

#Q-Q plot
qqnorm(data$vascular_plant_cover[year == 2016], main = 'qq plot normality vascular plant cover 2016', col = 'blue')
qqline(data$vascular_plant_cover[year == 2016], col = 'red')
qqnorm(data$vascular_plant_cover[year == 2017], main = 'qq plot normality vascular plant cover 2017', col = 'blue')
qqline(data$vascular_plant_cover[year == 2017], col = 'red')

#wilcoxon signed rank
length(vascular_plant_cover[year == 2016])
length(vascular_plant_cover[year == 2017])
vpc_2016 <- na.omit(vascular_plant_cover[year == 2016])
vpc_2017 <- na.omit(vascular_plant_cover[year == 2017])
min_length <- min(length(vpc_2016), length(vpc_2017))
vpc_2016 <- vpc_2016[1:min_length]
vpc_2017 <- vpc_2017[1:min_length]
paired_wilcox <- wilcox.test(vpc_2017, vpc_2016, mu = 0, var.equal = TRUE, paired = FALSE, alternative = "two.sided")
paired_wilcox

#Normality
shapiro.test(data$moss[data$year == 2016])
shapiro.test(data$moss[data$year == 2017])

#Q-Q plot
qqnorm(data$moss[year == 2016], main = 'qq plot normality moss 2016', col = 'blue')
qqline(data$moss[year == 2016], col = 'red')
qqnorm(data$moss[year == 2017], main = 'qq plot normality moss 2017', col = 'blue')
qqline(data$moss[year == 2017], col = 'red')

#Wilcoxon signed rank 
length(moss[year == 2016])
length(moss[year == 2017])
moss_2016 <- na.omit(moss[year == 2016])
moss_2017 <- na.omit(moss[year == 2017])
min_length <- min(length(moss_2016), length(moss_2017))
moss_2016 <- moss_2016[1:min_length]
moss_2017 <- moss_2017[1:min_length]
paired_wilcox <- wilcox.test(moss_2017, moss_2016, mu = 0, var.equal = TRUE, paired = FALSE, alternative = "two.sided")
paired_wilcox


#How do measurements of roof biodiversity differ across both climate types (variable koppen_climate) and years?
bartlett.test(species_richness ~ interaction(koppen_climate, year), data = data)
year_factor <- as.factor(data$year)
koppen_factor <- as.factor(data$koppen_climate)
two.anova <- aov(species_richness ~koppen_factor * year_factor, data = data)
summary(two.anova)
ggline(data, x = "Koppen_climate", y = "species_richness", color = 'year', add = c("mean_se", "dotplot"),
       palette = c("green", "yellow"), ylab = "species richness", xlab = "climate type")
TukeyHSD(two.anova, conf.level = .95)
plot(TukeyHSD(two.anova, conf.level = .95), las = 2)

bartlett.test(shannon_diversity ~ interaction(koppen_climate, year), data = data)
two.anova <- aov(shannon_diversity ~ koppen_climate * year, data = data)
summary(two.anova)
residual <- residuals(two.anova)
shapiro.test(residual)
friedman.test(shannon_diversity ~ koppen_factor | year_factor, data = data)

bartlett.test(evenness ~ interaction(koppen_climate, year), data = data)
two.anova <- aov(evenness ~ koppen_climate * year, data = data)
summary(two.anova)
residual <- residuals(two.anova)
shapiro.test(residual)
