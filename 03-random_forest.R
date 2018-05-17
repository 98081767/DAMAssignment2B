#Some general tips before you proceed:
# 1. Install the following packages before you proceed: randomForest
# 2. Download documentation for rpart from CRAN: 
#    https://cran.r-project.org/web/packages/rpart/rpart.pdf 
# 3. Run the code line by line and examine the results.
# 4. Understand what each step does. Check out the environment panel (on the right
#    in RStudio) to see more about the variables created in each step.
# 5. Check the randomForest documentation and understand what each parameter does (homework)
# 


install.packages("rpart")
install.packages("rpart.plot")
install.packages("mlbench")

setwd("C:/Users/arche/Documents/UTS/36106 Data Algorithms and Meaning/Workshop 3/decision tree and random forest exercises")

#set working directory if needed (modify path as needed)
#setwd("C:/Users/Kailash/Documents/dam_lecture_3")
#load required libraries - rpart for classification and regression trees
library(rpart)
#mlbench for Glass dataset
library(mlbench)
#load Glass
data("Glass")
#set seed to ensure reproducible results
set.seed(42)
#split into training and test sets
Glass[,"train"] <- ifelse(runif(nrow(Glass))<0.8,1,0)
#separate training and test sets
trainGlass <- Glass[Glass$train==1,]
testGlass <- Glass[Glass$train==0,]
#get column index of train flag
trainColNum <- grep("train",names(trainGlass))
#remove train flag column from train and test sets
trainGlass <- trainGlass[,-trainColNum]
testGlass <- testGlass[,-trainColNum]
#get column index of predicted variable in dataset
typeColNum <- grep("Type",names(Glass))
#build model
rpart_model <- rpart(Type ~.,data = trainGlass, method="class") #use method ="anova" for regression problems
#plot tree
prp(rpart_model)
#...and the moment of reckoning
rpart_predict <- predict(rpart_model,testGlass[,-typeColNum],type="class")
mean(rpart_predict==testGlass$Type)


library(randomForest)
data("Glass")
set.seed(42)
Glass[,"train"] <- ifelse(runif(nrow(Glass))<0.8,1,0)
#write dataframe to disk to check
#write.csv(Glass,"Glass.csv")
#separate training and test sets
trainGlass <- Glass[Glass$train==1,]
testGlass <- Glass[Glass$train==0,]
trainColNum <- grep("train",names(trainGlass))
typeColNum <- grep("Type",names(Glass))
trainGlass <- trainGlass[,-trainColNum]
testGlass <- testGlass[,-trainColNum]
#Build random forest model
Glass.rf <- randomForest(Type ~.,data = trainGlass, 
                       importance=TRUE, xtest=testGlass[,-typeColNum],ntree=1000)
#model summary
summary(Glass.rf)
#variables contained in model 
names(Glass.rf)

#predictions for test set
test_predictions_rf <- data.frame(testGlass,Glass.rf$test$predicted)
write.csv(test_predictions_rf,file="test_predictions_rf.csv")

#accuracy for test set
mean(Glass.rf$test$predicted==testGlass$Type)
#confusion matrix
table(Glass.rf$test$predicted,testGlass$Type)


#quantitative measure of variable importance
importance(Glass.rf)
#sorted plot of importance
varImpPlot(Glass.rf)


#visualisationation


install.packages("dplyr")
install.packages("ggraph")
install.packages("igraph")



library(dplyr)
library(ggraph)
library(igraph)

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}


#tree_func(final_model = Glass.rf, 5)