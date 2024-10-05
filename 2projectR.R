library(readxl)


 #######################FEW STEPS BASED ON STEP 4######################################################
mydata = read_excel("C:/Users/debar/OneDrive/Desktop/mcdonalds.xls")
View(mydata)


#reading the column names and size of the data
R=colnames(mydata)
R
R=dim(mydata)
R


R=summary(mydata[, c(1, 2, 4, 5)]) 
R


##forming histogram wrt age
R=library("lattice")
R=histogram(~ Age, data = mydata)
R

##descriptive analysis wrt age for the data
R= summary(mydata$Age)
R

##boxplot
R=boxplot(mydata$Age, horizontal = TRUE, xlab = "Age")
R

str(mydata)
colnames(mydata)
mydata$spicy
model_matrix <- model.matrix(~ spicy - 1, data = mydata)
model_matrix1 <- model.matrix(~ cheap - 1, data = mydata)
model_matrix2<- model.matrix(~ Gender - 1, data = mydata)
model_matrix3<- model.matrix(~ healthy -1, data= mydata)

mydata_dummy <- cbind(mydata, spicy = model_matrix, cheap = model_matrix1, Gender = model_matrix2, healthy = model_matrix3)

############boxplot for spicy#############

barplot(table(mydata_dummy$spicy), 
        main = "Bar Plot for Spicy Preferences", 
        xlab = "Spicy (0 = No, 1 = Yes)", 
        ylab = "Count", 
        col = c("lightblue", "darkblue"))


########### Bar plot for 'cheap' dummy variable###############
barplot(table(mydata_dummy$cheap), 
        main = "Bar Plot for Cheap Preferences", 
        xlab = "Cheap (0 = No, 1 = Yes)", 
        ylab = "Count", 
        col = c("lightgreen", "darkgreen"))


###############Bar plot for 'Gender' dummy variable###########
barplot(table(mydata_dummy$Gender), 
        main = "Bar Plot for Gender", 
        xlab = "Gender (0 = Female, 1 = Male)", 
        ylab = "Count", 
        col = c("lightpink", "lightblue"))

# Bar plot for 'spicy' by cluster
spicy_by_cluster <- table(mydata$spicy, mydata$cluster)
barplot(spicy_by_cluster, beside = TRUE, 
        main = "Spicy Preferences by Cluster", 
        xlab = "Cluster", 
        ylab = "Count", 
        col = c("lightblue", "darkblue"), 
        legend = rownames(spicy_by_cluster))








######################################FEW STEPS BASED ON STEP 5##################################
###Principal Component Analysis 
pca_result <- prcomp(model_matrix, scale. = TRUE)
pca_result
summary(pca_result)

pca_result$x

####Hierarchical Methods

combined_matrix <- cbind(model_matrix, model_matrix1, model_matrix2)
dist_matrix <- dist(combined_matrix, method = "euclidean")
hclust_result <- hclust(dist_matrix, method = "complete")
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", 
     xlab = "", sub = "", cex = 0.9)
clusters <- cutree(hclust_result, k = 3)
mydata$cluster <- clusters
table(mydata$cluster)


############steps based on step 6##############################



############dendrogram branches by cluster#########
plot(hclust_result, labels = FALSE, main = "Colored Dendrogram", xlab = "", sub = "")
rect.hclust(hclust_result, k = 3, border = "red"    
            
            