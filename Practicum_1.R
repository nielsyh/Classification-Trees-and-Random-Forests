if (!require("data.tree")){
  install.packages("data.tree", dependencies = TRUE)
  library(data.tree)
}

if (!require("ISLR")){
  install.packages("ISLR", dependencies = TRUE)
  library(ISLR)
}


data <- read.csv("C:/dm/credit.txt")


#calcs impurity for a given node (1).
impurity <- function(data = c()) {
  l<- length(data)
  class_zero <- length(data[data == 0])
  class_uno <- length(data[data == 1])
  res <- (class_zero/l) * (class_uno/l)
  res
}

#calcualtes reduction with the gini-index. 
impurity_reduction <- function(orig = c(), uno = c(), dos=c()){
  l <- length(orig)
  l_uno <- length(uno)
  l_dos <- length(dos)
  res <- impurity(orig) - ( (l_uno/l)*impurity(uno) + (l_dos/l)*impurity(dos) )
  res
}

#returns bestsplit, tested on income.
bestsplit <-function(num_data = c(), class_data = c()){
  num_sorted <- sort(unique(num_data))
  splitpoints <- (num_sorted[1:(length(num_sorted) - 1)]+num_sorted[2:length(num_sorted)])/2
  
  orig <- impurity(class_data)
  
  best <- 0
  val <- 0
  
  for (i in splitpoints){
    
    #print(i)
    res <- impurity_reduction(class_data, class_data[num_data > i], class_data[num_data <= i])
    #print(res)
    if(res > val){
      #print('new best')
      val <- res
      best <- i
    }
  }
  best
  
}

create.node <- function (node.label = "", node.type = "left", type = "binary", node.val = "", y = c()) {
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


tree.grow <- function(data = c(), nmin = 2, minleaf = 2, nfeat = ncol(data)) {
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
  
  # Get a vector of features to consider for a split
  #features.to.consider <- sample.random.columns(x, nfeat)
  # Create a subset of the data
  #subsetted.data.x <- x[, features.to.consider, drop = FALSE]
  
  
  # Create the tree's root node.
  root <- create.node(node.label = "Classification Tree", node.type = "root", node.val = 0, y = data)
  tree <- tree.grow.rec(root, nmin = nmin, minleaf = minleaf)
  
}

tree.grow.rec <- function(node = NULL, nmin = 2, minleaf = 2){
  
  node.data <- node$y
  
  if(is.null(node.data)){stop('no data')}
  if(nrow(node.data) <= nmin){
    print('should be leaf?')
  }
  if(impurity(node.data) == 0){stop('Leaf because pure')}
  
  if(nrow(node.data) <= minleaf){
    print('no valid split')
  }
  
  
  # FIND BEST ROW WITH BEST IMPUR REDUCTION FOR ALL POSSIBLE SPLITS
  
  split.row <- NULL
  split.value <- 0
  reduction.max <- 0
  
  #skip first and last column
  for(row in 1:(ncol(node.data)-1)){
    #print(data[,row])
    #print(bestsplit(data[,row], data[,ncol(data)]))
    bs <- bestsplit(node.data[,row], node.data[,ncol(node.data)])
    #print(impurity_reduction(data[,ncol(data)],  data[,ncol(data)][data[,row]>bs],  data[,ncol(data)][data[,row]<=bs] ))
    
    reduction.total <- impurity_reduction(node.data[,ncol(node.data)],  node.data[,ncol(node.data)][node.data[,row]>bs],  node.data[,ncol(node.data)][node.data[,row]<=bs] )
    
    if(reduction.total > reduction.max){
      reduction.max <- reduction.total
      split.row <- row
      split.value <- bs
    }
  }
  ### END PUT THESE IN NODES
  
  
  leftChild <- create.node(node.label = 1, node.type = "left", node.val = split.value, y = node.data[node.data[,split.row] <= split.value,])
  rightChild <- create.node(node.label = 1, node.type = "right", node.val = split.value, y = node.data[node.data[,split.row] > split.value,])
  
  #recurse
  tree.grow.rec(leftChild,nmin, minleaf)
  tree.grow.rec(rightChild,nmin, minleaf)
  
  node$AddChildNode(leftChild)
  node$AddChildNode(rightChild)
  
  
  
  #print(node)
}

create.node <- function (node.label = "", node.type = "left", type = "binary", node.val = "", y = c()) {
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



