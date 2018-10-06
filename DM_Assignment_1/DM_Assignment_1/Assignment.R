if (!require("data.tree")) {
    install.packages("data.tree", dependencies = TRUE)
    library(data.tree)
}

#calcs impurity for a given node (1).
impurity <- function(data = c()) {
    l <- length(data)
    class_zero <- length(data[data == 0])
    class_uno <- length(data[data == 1])
    res <- (class_zero / l) * (class_uno / l)
    return(res)
}

#calculates impurity reduction using the gini-index method. 
impurity_reduction <- function(orig = c(), uno = c(), dos = c()) {
    l <- length(orig)
    l_uno <- length(uno)
    l_dos <- length(dos)
    res <- impurity(orig) - ((l_uno / l) * impurity(uno) + (l_dos / l) * impurity(dos))
    return(res)
}

#returns bestsplit, tested on income. numeric data -> data to be slit, class_data -> classification of the numeric data. 
bestsplit <- function(num_data = c(), class_data = c()) {
    # sort numbers 
    num_sorted <- sort(unique(num_data))
    # find all split points => halfway
    splitpoints <- (num_sorted[1:(length(num_sorted) - 1)] + num_sorted[2:length(num_sorted)]) / 2

    orig <- impurity(class_data)

    best <- 0
    val <- 0

    #Check for all splits which one gives the highest impurity reduction.
    for (i in splitpoints) {
        res <- impurity_reduction(class_data, class_data[num_data > i], class_data[num_data <= i])
        if (res > val) {
            val <- res
            best <- i
        }
    }
    return(best)
}



#function to create a node
node.create <- function(node.label = "", node.type = "left", type = "binary", node.val = "", y = c()) {
    # Error checking
    if (type != "binary" && type != "numerical") {
        stop("Node can either be binary or numerical!")
    }

    if (node.type != "left" && node.type != "right" && node.type != "root") {
        stop("A node can either be left or right or root")
    }

    node <- Node$new()
    node$type <- node.type
    node$val <- node.val
    node$name <- node.label
    node$y <- y

    if (node.type == "left") {
        node$name <- paste(node.label, "<=", node.val, sep = '')
    } else if (node.type == "right") {
        node$name <- paste(node.label, ">", node.val, sep = '')
    }

    node$isTerminal <- FALSE

    return(node)
}

#Required tree.grow function
tree.grow <- function(data = c(), nmin = 2, minleaf = 2, nfeat = (ncol(data)) - 1) {
    # Sanity checks
    if (is.null(data)) {
        stop("Feature table cannot be empty or null")
    }

    if (is.null(data)) {
        stop("Class label cannot be empty or null")
    }

    if (minleaf < 1) {
        stop("Must have at least 2 observations on a leaf node")
    }

    if (nmin <= 0) {
        stop("Minimum number of observations for a node has to be positive")
    }

    if (nfeat > ncol(data)) {
        stop("Cannot take a sample larger than the population.")
    }

    if (nfeat < (ncol(data) - 1)) {
        sample <- cbind(data[, sample.random.columns(data[-(ncol(data))], nfeat), drop = FALSE], class = data[, ncol(data)])
        data <- sample
    }

    # Create the tree's root node.
    root <- node.create(node.label = "Classification Tree", node.type = "root", node.val = 0, y = data)
    # Recurse on root node.
    tree <- tree.grow.rec(root, nmin = nmin, minleaf = minleaf)
    return(tree)
}

tree.grow.bag <- function(data = c(), nmin = 2, minleaf = 2, nfeat = (ncol(data)) - 1, m) {
    result = c()
    for (i in 1:m) {
        iTree = tree.grow(data, nmin, minleaf, nfeat)
        result[[i]] <- itree
    }
    return(result)
}


tree.classify.bag <- function(trees, matrix) {
    results = c()
    for (row in 1:nrow) {
        row <- x[index,];
        r = c()
        n = 0
        for (tree in trees) {
            class <- tree.classify(row)
            r[[n]] = class
            n = n + 1
        }
        result_class = tree.majorityVote(r)
        c[[row]]=result_class
    }
    return (results)
}


tree.majorityVote <- function(predictions) {
    zeros = 0
    ones = 0

    for (i in predictions) {
        if (i == 1) {
            ones = ones+1
        }
        else {
            zeros = zeros + 1
        }
    }

    if (zeros > ones) return(0)
    if (ones > zeros) return(1)


    #if they're equal, we must choose one randomly
    rand <- sample(1:100, 1)
    if (rand <= 50) return (0)
    else return (1)
}

tree.majority <- function(node) {
    width = ncol(node$y)
    height = nrow(node$y)

    classes = node$y[width:width]
    agg = 0
    for (i in classes) {
        for (l in i)
            agg = agg + l
    }
    total = agg / height
    if (total >= 0.5) return(1)
    return(0)
}

tree.traverse <- function(row, currentNode) {
    ch = length(currentNode$children)

    if (ch == 0) {
        data = currentNode$y
        return(tree.majority(currentNode))
    }

    split_column <- currentNode$split_col
    split_value <- currentNode$split_val

    val <- row[split_column]

    if (val <= split_value) {
        return(tree.traverse(row, currentNode$children[[1]]));
    }
    else {
        return(tree.traverse(row, currentNode$children[[2]]));
    }
}

#recursive function to build a tree.
tree.grow.rec <- function(node = NULL, nmin = 2, minleaf = 2) {
    node.data <- node$y

    if (is.null(node.data)) {
        #print('no data')
        return(node)
    }
    if (nrow(node.data) < nmin) {
        #print('should be leaf?')
        return(node)
    }
    if (impurity(node.data[, ncol(node.data)]) == 0) {
        #print('Leaf because pure')
        return(node)
    }

    if (ncol(node.data) < minleaf) {
        #print('no valid split')
        return(node)
    }

    # FIND BEST col WITH BEST IMPUR REDUCTION FOR ALL POSSIBLE SPLITS
    split.col <- NULL
    split.value <- NULL
    reduction.max <- 0

    #skip first and last column ATLEAST FOR TEST DATA..
    for (col in 1:(ncol(node.data) - 1)) {

        #only split when there is more then 1 unique data value, otherwise there is no posssible split.
        if (length(unique(node.data[, col])) > 1) {

            #bs means bestsplit for that col of data.
            bs <- bestsplit(node.data[, col], node.data[, ncol(node.data)])

            #get reduction on this split
            reduction.total <- impurity_reduction(node.data[, ncol(node.data)], node.data[, ncol(node.data)][node.data[, col] > bs], node.data[, ncol(node.data)][node.data[, col] <= bs])

            #check if this split is the best until now, if yes -> remember the split.
            if (reduction.total > reduction.max) {
                reduction.max <- reduction.total
                split.col <- col
                split.value <- bs
            }
        }
    }

    #check if found split.
    if (is.null(split.value)) {
        #print('no split possible, return node')
        return(node)
    }

    #make right and left children
    leftChild <- node.create(node.label = 1, node.type = "left", node.val = split.value, y = node.data[node.data[, split.col] <= split.value,])
    rightChild <- node.create(node.label = 1, node.type = "right", node.val = split.value, y = node.data[node.data[, split.col] > split.value,])

    node$split_col = split.col
    node$split_val = split.value

    #recurse
    tree.grow.rec(leftChild, nmin, minleaf)
    tree.grow.rec(rightChild, nmin, minleaf)

    #add children to parent
    node$AddChildNode(leftChild)
    node$AddChildNode(rightChild)

    return(node)
}

sample.random.columns <- function(X, n) {
    if (n == ncol(X)) {
        return(sort(c(1:ncol(X)), decreasing = FALSE))
    }
    return(sort(c(sample(1:ncol(X), n, replace = F)), decreasing = FALSE))
}

# Here x is a data matrix containing the attribute values of the cases for
# which predictions are required, and tr is a tree object created
# with the function tree.grow
tree.classify <- function(x = c(), tr = NULL) {
    y <- 0
    for (index in 1:nrow(x)) {
        row = x[index,];
        result = tree.traverse(row, tr)
        #print(result)
    }
}


#fake data input
data <- read.csv("C:/dm/credit.txt")
#this is our built tree
tree <- tree.grow(data)
tree.classify(data, tree)