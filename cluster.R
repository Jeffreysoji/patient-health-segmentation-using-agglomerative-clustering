# ==========================================
# Patient Health Segmentation using 
# Agglomerative Hierarchical Clustering in R
# ==========================================

# ------------------------------------------
# 0. Load Required Libraries
# ------------------------------------------
# If you don't have these installed, run: 
# install.packages(c("tidyverse", "cluster", "factoextra", "corrplot"))

library(tidyverse)  # For data manipulation and visualization
library(cluster)    # For hierarchical clustering
library(factoextra) # For beautiful clustering visualizations
library(corrplot)   # For correlation matrix visualization

# ------------------------------------------
# 1. Data Import & Simulation
# ------------------------------------------

# --- SIMULATED DATA (For demonstration) ---
set.seed(42) # For reproducibility
n_patients <- 100

patient_data <- data.frame(
  Patient_ID = 1:n_patients,
  Age = round(rnorm(n_patients, 50, 15)),
  Gender = sample(c("M", "F"), n_patients, replace = TRUE),
  Blood_Pressure = round(rnorm(n_patients, 130, 20)),
  Cholesterol_Level = round(rnorm(n_patients, 200, 40)),
  BMI = round(rnorm(n_patients, 26, 5), 1),
  Glucose_Level = round(rnorm(n_patients, 110, 30)),
  Heart_Rate = round(rnorm(n_patients, 75, 12))
)

# Introduce some missing values for preprocessing demonstration
patient_data$Glucose_Level[c(5, 22, 50)] <- NA
patient_data$BMI[c(12, 85)] <- NA

# --- LOADING YOUR REAL DATA ---
# To use your real data, uncomment the line below and replace with your file path:
# patient_data <- read.csv("your_patient_data_file.csv") 


# ------------------------------------------
# 2. Data Preprocessing (Missing Values)
# ------------------------------------------
cat("--- Missing Values Check Before Cleaning ---\n")
print(colSums(is.na(patient_data)))

# Handling missing values by replacing them with the median (robust to outliers)
patient_data$Glucose_Level[is.na(patient_data$Glucose_Level)] <- median(patient_data$Glucose_Level, na.rm = TRUE)
patient_data$BMI[is.na(patient_data$BMI)] <- median(patient_data$BMI, na.rm = TRUE)

cat("\n--- Missing Values Check After Cleaning ---\n")
print(colSums(is.na(patient_data)))


# ------------------------------------------
# 3. Descriptive Statistics
# ------------------------------------------
cat("\n--- Descriptive Statistics Summary ---\n")
summary(patient_data)


# ------------------------------------------
# 4. Exploratory Data Analysis (EDA) & Visualization
# ------------------------------------------

# A. Correlation Matrix (Numerical variables only)
numerical_data <- patient_data %>% 
  select(Age, Blood_Pressure, Cholesterol_Level, BMI, Glucose_Level, Heart_Rate)

cor_matrix <- cor(numerical_data)
corrplot(cor_matrix, method = "color", addCoef.col = "black", 
         tl.col = "black", title = "Correlation Matrix of Health Indicators", mar=c(0,0,1,0))

# B. Boxplots for Outlier Detection
numerical_long <- numerical_data %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(numerical_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Outlier Detection in Health Attributes", x = "Variables", y = "Value") +
  theme(legend.position = "none")


# ------------------------------------------
# 5. Feature Scaling (Standardization)
# ------------------------------------------
# Clustering is sensitive to data scale. We scale numerical data to mean=0, sd=1.
scaled_data <- scale(numerical_data)


# ------------------------------------------
# 6. Agglomerative Hierarchical Clustering
# ------------------------------------------

# A. Compute Distance Matrix (Euclidean Distance)
dist_matrix <- dist(scaled_data, method = "euclidean")

# B. Apply Agglomerative Clustering (using Ward's linkage for compact clusters)
hc_ward <- hclust(dist_matrix, method = "ward.D2")


# ------------------------------------------
# 7. Cluster Visualization (Dendrogram & Silhouette)
# ------------------------------------------

# A. Standard Dendrogram (Visualizing the Hierarchy)
plot(hc_ward, labels = patient_data$Patient_ID, hang = -1,
     main = "Dendrogram of Patient Health Segmentation",
     xlab = "Patient IDs", ylab = "Euclidean Distance")

# Draw boxes around the clusters (Let's assume 3 groups: High-risk, Moderate-risk, Low-risk)
rect.hclust(hc_ward, k = 3, border = 2:4)

# B. Beautiful Visualizations using 'factoextra'
# Let's find the optimal number of clusters using the Silhouette method
fviz_nbclust(scaled_data, hcut, method = "silhouette") +
  labs(title = "Optimal Number of Clusters (Silhouette Method)")

# Cut tree into 3 clusters
patient_clusters <- cutree(hc_ward, k = 3)

# Add cluster labels back to original dataset for interpretation
patient_data$Cluster <- as.factor(patient_clusters)

# Visualizing patient data points on a 2D Scatter plot (PCA Projection)
fviz_cluster(list(data = scaled_data, cluster = patient_clusters),
             palette = "jco",
             ellipse.type = "convex",
             repel = TRUE,
             ggtheme = theme_minimal(),
             main = "Patient Health Clusters (PCA Projection)")


# ------------------------------------------
# 8. Cluster Interpretation & Evaluation
# ------------------------------------------

# Group dataset by cluster and find average metrics for each health profile
cluster_summary <- patient_data %>%
  group_by(Cluster) %>%
  summarise(
    Patient_Count = n(),
    Avg_Age = mean(Age),
    Avg_BP = mean(Blood_Pressure),
    Avg_Cholesterol = mean(Cholesterol_Level),
    Avg_BMI = mean(BMI),
    Avg_Glucose = mean(Glucose_Level),
    Avg_Heart_Rate = mean(Heart_Rate)
  )

cat("\n--- Cluster Profile Summary ---\n")
print(cluster_summary)