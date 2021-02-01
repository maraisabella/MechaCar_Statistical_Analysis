## Deliverable 1 

library(tidyverse)

mecha_car_data <- read.csv(file="MechaCar_mpg.csv", stringsAsFactors=F, check.names=F)

summary(lm(mpg~vehicle_weight+vehicle_length+spoiler_angle+ground_clearance+AWD,data = mecha_car))


# Deliverable 2
coils <- read.csv('Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

total_summary <- coils %>% summarize(Mean=mean(PSI),Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

lot_summary <- coils %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median =median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

# Deliverable 3

t.test(suspension_coil_data$PSI,mu=1500) #compare sample versus population means

lot <- function(data, lot_num){

filtered_data <- data[deframe(data['Manufacturing_Lot']) == lot_num,]

return (filtered_data)
}

lot1_data <- lot(suspension_coil_data, "Lot1")
lot1_data

t.test((lot1_data$PSI),mu=1500)

lot1 <- subset(suspension_coil_data$PSI, suspension_coil_data$Manufacturing_Lot == "Lot1") 

t.test(lot1, mu=1500)

lot2 <- subset(suspension_coil_data$PSI, suspension_coil_data$Manufacturing_Lot == "Lot2") 

t.test(lot2, mu=1500)

lot3 <- subset(suspension_coil_data$PSI, suspension_coil_data$Manufacturing_Lot == "Lot3") 

t.test(lot3, mu=1500)
