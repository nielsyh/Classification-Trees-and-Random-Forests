if (!require("data.tree")){
  install.packages("data.tree", dependencies = TRUE)
  library(data.tree)
}


#fake data input
data <- read.csv("C:/dm/credit.txt")


#calcs impurity for a given node (1).
impurity <- function(data = c()) {
  l<- length(data)
  class_zero <- length(data[data == 0])
  class_uno <- length(data[data == 1])
  res <- (class_zero/l) * (class_uno/l)
  return(res)
}

#calcualtes reduction with the gini-index. 
impurity_reduction <- function(orig = c(), uno = c(), dos=c()){
  l <- length(orig)
  l_uno <- length(uno)
  l_dos <- length(dos)
  res <- impurity(orig) - ( (l_uno/l)*impurity(uno) + (l_dos/l)*impurity(dos) )
  return(res)
}

#returns bestsplit, tested on income.
bestsplit <-function(num_data = c(), class_data = c()){
  num_sorted <- sort(unique(num_data))
  splitpoints <- (num_sorted[1:(length(num_sorted) - 1)]+num_sorted[2:length(num_sorted)])/2
  orig <- impurity(class_data)
  best <- 0
  val <- 0
  
  for (i in splitpoints){
    res <- impurity_reduction(class_data, class_data[num_data > i], class_data[num_data <= i])
    if(res > val){
      val <- res
      best <- i
    }
  }
  return(best)
}

#function to create a node
node.create <- function (node.label = "", node.type = "left", type = "binary", node.val = "", y = c()) {
  # Error checking
  if (type != "binary" && type != "numerical"){
    stop("Node can either be binary or numerical!")
  }
  
  if (node.type != "left" && node.type != "right" && node.type != "root") {
    stop("A node can either be left or right or root")
  }
  
  node <- Node$new()
  node$type <- node.type
  node$val <- node.val
  node$name <- node.label
  node$attr <- node.label
  node$y <- y
  
  if (node.type == "left"){
    node$name <- paste(node.label, "<=", node.val, sep = '')
  } else if (node.type == "right") {
    node$name <- paste(node.label, ">", node.val, sep = '')
  }
  
  # Increment the depth of the child = node of parent + 1
  # Maybe here perform some checks about the depth of the tree.
  node$isTerminal <- FALSE
  
  return (node)
}

#TODO add nfeat implementation
#Required tree.grow function
tree.grow <- function(data = c(), nmin = 2, minleaf = 2, nfeat = (ncol(data)) - 1 ) {
  # Sanity checks
  if (is.null(data)){
    stop("Feature table cannot be empty or null")
  }
  
  if (is.null(data)) {
    stop("Class label cannot be empty or null")
  }
  
  if (minleaf <= 1) {
    stop("Must have at least 2 observations on a leaf node")
  }
  
  if (nmin <= 0) {
    stop("Minimum number of observations for a node has to be positive")
  }
  
  if (nfeat > ncol(data)) {
    stop("Cannot take a sample larger than the population.")
  }
  
  if(nfeat < (ncol(data) - 1)){
    sample <- cbind(data[, sample.random.columns(data[-(ncol(data))],nfeat), drop = FALSE], class=data[,ncol(data)])
    data <- sample
  }
  
  # Create the tree's root node.
  root <- node.create(node.label = "Classification Tree", node.type = "root", node.val = 0, y = data)
  tree <- tree.grow.rec(root, nmin = nmin, minleaf = minleaf)
  return(tree)
}

#recursive function to build a tree.
tree.grow.rec <- function(node = NULL, nmin = 2, minleaf = 2){
  node.data <- node$y
  
  tmp <- impurity(node.data[,ncol(node.data)])

  if(is.null(node.data)){
    print('no data')
    return(node)
  }
  if(nrow(node.data) < nmin){
    print('should be leaf?')
    return(node)
  }
  if(impurity(node.data[,ncol(node.data)]) == 0){
    print('Leaf because pure')
    return(node)
    }
  
  if(nrow(node.data) < minleaf){
    print('no valid split')
    return(node)
  }
  
  # FIND BEST ROW WITH BEST IMPUR REDUCTION FOR ALL POSSIBLE SPLITS
  
  split.row <- NULL
  split.value <- NULL
  reduction.max <- 0
  
  #todo add minleaf requirement
  #skip first and last column
  for(row in 1:(ncol(node.data)-1)){
    #only split when there is more then 1 unique data value
    

    if(length(unique(node.data[, row])) > 1){
      #split
      bs <- bestsplit(node.data[,row], node.data[,ncol(node.data)])
      #get reduction on this split
      reduction.total <- impurity_reduction(node.data[,ncol(node.data)],  node.data[,ncol(node.data)][node.data[,row]>bs],  node.data[,ncol(node.data)][node.data[,row]<=bs] )
      #check if this split is the best until now, if yes -> remember the split.
      if(reduction.total > reduction.max){
        reduction.max <- reduction.total
        split.row <- row
        split.value <- bs
      }
    }
  }
  
  #check if found split.
  if(is.null(split.value)){
    print('no split possible, return node')
    return(node)
  }
  
  #make right and left children
  leftChild <- node.create(node.label = 1, node.type = "left", node.val = split.value, y = node.data[node.data[,split.row] <= split.value,])
  rightChild <- node.create(node.label = 1, node.type = "right", node.val = split.value, y = node.data[node.data[,split.row] > split.value,])
  
  #recurse
  tree.grow.rec(leftChild,nmin, minleaf)
  tree.grow.rec(rightChild,nmin, minleaf)
  
  #add children to parent
  node$AddChildNode(leftChild)
  node$AddChildNode(rightChild)
  
  return(node)
}

#PAVLOS function to get random indexes
sample.random.columns <- function(X, n) {
  if (n == ncol(X)){
    return (sort(c(1:ncol(X)), decreasing = FALSE))
  }
  return (sort(c(sample(1:ncol(X), n, replace=F)), decreasing = FALSE))
}

#Input column x with tree object
#funciton returns y which is the result of classifing x with attached tree obj.
tree.classify <- function(x = c(), tr = NULL){
  y <- 0
  
  

  return(y)
  
}


