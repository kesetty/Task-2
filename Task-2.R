# Load the Titanic dataset (make sure the file is in the working directory)
titanic_data <-titanic_train
# View the first few rows of the dataset
head(titanic_data)
# Check for missing values in the dataset
colSums(is.na(titanic_data))
# Impute missing Age with the median
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)
# Check again for missing values
colSums(is.na(titanic_data))
# Convert categorical variables to factors
titanic_data$Survived <- factor(titanic_data$Survived, levels = c(0, 1), labels = c("No", "Yes"))
titanic_data$Pclass <- factor(titanic_data$Pclass)
titanic_data$Sex <- factor(titanic_data$Sex)
titanic_data$Embarked <- factor(titanic_data$Embarked)
# Summary statistics
skim(titanic_data)
summary(titanic_data)
# Histogram for Age
ggplot(titanic_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# Histogram for Fare
ggplot(titanic_data, aes(x = Fare)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Fare Distribution", x = "Fare", y = "Count")
# Bar chart for Survived
ggplot(titanic_data, aes(x = Survived)) +
  geom_bar(fill = c("lightblue", "lightcoral")) +
  labs(title = "Survival Distribution", x = "Survived", y = "Count")

# Bar chart for Sex
ggplot(titanic_data, aes(x = Sex)) +
  geom_bar(fill = c("lightblue", "lightpink")) +
  labs(title = "Gender Distribution", x = "Sex", y = "Count")

# Bar chart for Pclass
ggplot(titanic_data, aes(x = Pclass)) +
  geom_bar(fill = c("lightgreen", "lightyellow", "lightpink")) +
  labs(title = "Passenger Class Distribution", x = "Pclass", y = "Count")
# Select numeric columns for correlation
numeric_data <- titanic_data %>% select(Age, Fare)

# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle")
# Bar plot: Survival by Sex
ggplot(titanic_data, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Sex", x = "Sex", y = "Count")

# Bar plot: Survival by Passenger Class
ggplot(titanic_data, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Passenger Class", x = "Pclass", y = "Count")

# Box plot: Age vs Survival
ggplot(titanic_data, aes(x = Survived, y = Age, fill = Survived)) +
  geom_boxplot() +
  labs(title = "Age vs Survival", x = "Survived", y = "Age")
# Boxplot: Fare vs Survival grouped by Pclass and Sex
ggplot(titanic_data, aes(x = Pclass, y = Fare, color = Survived)) +
  geom_boxplot() +
  facet_wrap(~ Sex) +
  labs(title = "Fare vs Survival by Pclass and Sex", x = "Pclass", y = "Fare")
