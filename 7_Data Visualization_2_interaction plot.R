# Load necessary packages
library(effects)
library(ggplot2)
library(psych)
library("lme4")

# Load Data
data<-read.csv("data.csv")

# Set Model

modelL4<-lmer(TM_L ~ (CRE+INT)*(T_CR+T_EF)+(CRE+INT|run_id),data)
modelI4<- lmer(TM_I ~ (CRE+INT)*(T_EF+T_CR)+(CRE+INT|run_id),data)
# Calculate effects for interaction between INT and T_CR
eff.p1 <- effect("INT*T_CR", modelL4,
                 KR = TRUE, xlevels = list(T_CR = c(-2.94, 0, 2.94), INT = c(0, 1), se = TRUE))

# Print effect details
eff.p1

# Create a plot for the effects of INT on T_CR
plot(eff.p1, x.var = "INT")

# Convert the effects data to a data frame
eff.p1 <- as.data.frame(eff.p1)



# Describe the 'T_CR' variable in the 'data' dataset
describe(data$T_CR)

# Further processing and visualization of the effects data
eff.p1$T_CR <- as.factor(eff.p1$T_CR)
eff.p1$INT <- as.factor(eff.p1$INT)
levels(eff.p1$INT) <- c("Low", "High", NA)

# Calculate and print specific effect differences
effect_diff1 <- eff.p1[2, 3] - eff.p1[1, 3]
effect_diff2 <- eff.p1[6, 3] - eff.p1[5, 3]
print(effect_diff1)
print(effect_diff2)

# Create a plot for the effects of INT and T_CR interaction
ggplot(data = eff.p1, aes(x = INT, y = fit, group = T_CR)) +
  geom_line(size = 1, aes(color = T_CR)) +
  geom_point(size = 4, aes(color = T_CR, shape = T_CR)) +
  ylab("Intention of Lecture") +
  xlab("Task Interest") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  scale_x_discrete(expand = c(0, 0.3)) +
  scale_color_discrete(name = "Teacher Creativity", labels = c("SD -1", "Average", "SD +1")) +
  scale_shape_discrete(name = "Teacher Creativity", labels = c("SD -1", "Average", "SD +1"))

# Calculate effects for interaction between INT and T_EF
eff.p2 <- effect("INT*T_EF", modelI4,
                 KR = TRUE,
                 xlevels = list(T_EF = c(-1.894, 0, 1.894), INT = c(0, 1)))

# Convert the effects data to a data frame
eff.p2 <- as.data.frame(eff.p2)

# Further processing and visualization of the effects data
eff.p2$T_EF <- as.factor(eff.p2$T_EF)
eff.p2$INT <- as.factor(eff.p2$INT)
levels(eff.p2$INT) <- c("Low", "High", NA)

# Calculate and print specific effect differences
effect_diff3 <- eff.p2[2, 3] - eff.p2[1, 3]
effect_diff4 <- eff.p2[6, 3] - eff.p2[5, 3]
print(effect_diff3)
print(effect_diff4)

# Create a plot for the effects of INT and T_EF interaction
ggplot(data = eff.p2, aes(x = INT, y = fit, group = T_EF)) +
  geom_line(size = 1, aes(color = T_EF)) +
  geom_point(size = 4, aes(color = T_EF, shape = T_EF)) +
  ylab("Inquiry Learning Intention") +
  xlab("Task Interest") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  scale_x_discrete(expand = c(0, 0.3)) +
  scale_color_discrete(name = "Teacher Efficacy", labels = c("SD -1", "Average", "SD +1")) +
  scale_shape_discrete(name = "Teacher Efficacy", labels = c("SD -1", "Average", "SD +1"))

# Calculate effects for T_EF
eff.p3 <- effect("T_EF", modelL4,
                 KR = TRUE,
                 xlevels = list(T_EF = c(-1.894, 1.894)))

# Convert the effects data to a data frame
eff.p3 <- as.data.frame(eff.p3)

# Further processing and visualization of the effects data
eff.p3$T_EF <- as.factor(eff.p3$T_EF)
levels(eff.p3$T_EF) <- c("SD -1", "SD +1", NA)

# Create a plot for the effects of T_EF
ggplot(data = eff.p3, aes(x = T_EF, y = fit)) +
  geom_point(size = 4) +
  ylab("Intention of Lecture") +
  xlab("Teacher Efficacy") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  scale_x_discrete(expand = c(0, 0.3))

