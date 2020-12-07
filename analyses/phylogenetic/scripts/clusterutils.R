require(ggplot2)
require(ggsci)
require(ggtree)
require(phytools)




date_epiweek <- function(date) {
  return( (as.numeric(format.Date(as.Date(date), format="%U")) + 1 ) %% 52)
}


epiweek_date <- function(week) {
  weeks <- seq.Date(as.Date("2019-12-22"), as.Date("2020-12-31"), by="week")
  return( c(weeks[week+1], weeks[week+1]+6))
}


# Select all sequence labels matching a cluster name using cluster lookup table
getClusterIds <- function(clusterName, clusters) as.character(clusters$sequence_label[clusters$cluster == clusterName, drop=TRUE])



# Label clusters
annotateClusters <- function(treeplot, clusternodes, hilightCluster=TRUE, labelCluster=TRUE, alpha=0.4, cols="#872434", ...) {
  
  if (length(cols) < length(clusternodes)) {
      cols <- rep(cols, length(clusternodes))
  }
  
  for (i in 1:length(clusternodes)) {
      clustername <- ifelse(is.null(names(clusternodes)), paste("Cluster",i), names(clusternodes)[i])
    
      if (hilightCluster) {
          treeplot <- treeplot + geom_hilight(node=clusternodes[i], fill=cols[i], alpha=alpha)
      }
      
      if (labelCluster) {
          treeplot <- treeplot + geom_cladelabel(node=clusternodes[i], label=clustername, color=cols[i], ...)
      }
  }
  
  return(treeplot)
  
}

#' Get a subset of an MCC tree around a cluster, plot that subset and highlight the cluster
#'
#' @param clusterName   Name of cluster in the cluster lookup table
#' @param clusters      Cluster lookup table (data.frame) with cluster names in column XXX and sequence labels in column XXX (can contain other info too)
#' @param tree          A ggtree object (a phylo object may work with some tweaks)
#' @param metadata      Metadata for tree taxa in a data frame. Must contain a column with name "taxon" and the exact taxa in the tree and a column with name "date" containing Date objects.
#' @param levels_back   The number of levels to go up in the tree past the cluster's root. If negative just plot the cluster in the whole tree
#' @param showParent    Highlight the cluster and iits parent cluster (one level up)
#' @param ...           Options to pass to getTreePlot
#' 
getClusterTreePlot <- function(clusterName, clusters, tree, metadata, shadecols=c("#69913B", "#872434"),
                               levels_back=-1, showParent=FALSE, labelCluster=FALSE, outputpath="", ...) {
  
  clusterIds  <- getClusterIds(clusterName, clusters)
  clusterMRCA <- getMRCA(tree@phylo, clusterIds)
  
  
  if (levels_back >= 0) {
      clusterTree <- treeio::tree_subset(tree, clusterMRCA, levels_back=levels_back)
  } else {
      clusterTree <- tree
  }
  
  clusterMRCANew  <- getMRCA(clusterTree@phylo, clusterIds)
  parent          <- clusterTree@phylo$edge[clusterTree@phylo$edge[,2] == clusterMRCANew,1]
  clusterTreeMeta <- metadata[match(clusterTree@phylo$tip.label, metadata$sequence_name), ] 
  
  p <- getTreePlot(clusterTree, clusterTreeMeta, plotCountry=FALSE, highlighted_tips=clusterIds, ...) 
  
  if (!is.null(shadecols)) {
      if (showParent) { 
          p <- annotateClusters(p, c(clusterMRCANew,parent), labelCluster=labelCluster, alpha=0.25, shadecols=shadecols) 
      } else {
          names(clusterMRCANew) <- clusterName
          p <- annotateClusters(p, clusterMRCANew, labelCluster=labelCluster, alpha=0.25, shadecols=shadecols) 
      }
  }
  
  return(p)

}

# Primarily used for parsimony clusters and TreeTime / Treedater trees.
saveCluster <- function(clusterName, clusters, tree, metadata, outputpath, intervals=NULL) {
    
    clusterIds   <- getClusterIds(clusterName, clusters)
    clusterMRCA  <- getMRCA(tree@phylo, clusterIds)
    
    # Only save non-singleton clusters
    if (!is.null(clusterMRCA)) {
      
        clusterTree  <- treeio::tree_subset(tree, clusterMRCA, levels_back=0)
        clusterDates <- metadata$sample_date[match(clusterTree@phylo$tip.label, metadata$taxon_label)]

        intervals               <- beastio::getTreeIntervals(clusterTree@phylo)
        mostrecent              <- decimal_date(max(clusterDates, na.rm=TRUE))
        intervals$date          <- mostrecent - intervals$height
        intervals$calendar_date <- lubridate::round_date(lubridate::date_decimal(intervals$date), unit="day")
        intervals$taxon         <- clusterTree@phylo$tip.label[intervals$node]
        
        write.csv(intervals, paste0(outputpath, clusterName, ".csv"), row.names=FALSE, quote=FALSE)
        write.tree(clusterTree@phylo, paste0(outputpath, clusterName, ".tree"))
        
    } else {
        # Singletons  
      
        # For singletons don't save anything and just return the sample itself
        clusterTree <- NULL
        sampleDate  <- metadata$sample_date[metadata$taxon_label == clusterIds[1]]
        intervals   <- data.frame(nodetype      = "sample", 
                                  date          = lubridate::decimal_date(sampleDate), 
                                  calendar_date = sampleDate, 
                                  taxon         = clusterIds[1])
    }
    
    return(list(tree=clusterTree, intervals=intervals))
}







# Trade-off extra conditional for less function calls (which is more expensive?)
clusterDFS <- function(tree, node, cluster, treetable, debug = FALSE) {
  
  if (debug > 1) cat(sprintf("%d, ", node))
  
  treetable$cluster[node] <- cluster
  treetable$visited[node] <- TRUE
  
  location <- treetable$location[node]
  
  # First go down to parent and up all of its other children
  parent <- treetable$parents[node]
  if (parent > 0 && !treetable$visited[parent] && treetable$location[parent] == location) {
    treetable <- clusterDFS(tree, parent, cluster, treetable, debug = debug)
  }
  
  # Then go up all children
  children <- which(treetable$parents == node)
  for (child in children) {
    if (!treetable$visited[child] && treetable$location[child] == location) {
      treetable <- clusterDFS(tree, child, cluster, treetable, debug = debug)
    }
  }
  
  return(treetable)
}



#' Update locations of tree nodes with a new threshold. Only update `location`. 
#' All other nodes will simply be labelled as "non-`locaton`".
#' 
#' THIS FUNCTION HAS NOT BEEN TESTED ON TREES WITH >2 LOCATIONS!
#' 
#' @param tree     Coloured input tree. Should be a tidytree (ggtree) tree object with at least 
#'                 `location.set` and `location.set.prob` annotated in `tree@data`.
#' @param location The location name in `tree@data$location.set` to update
#' @param cutoff   The probability threshold for assigning a node `location`. A node is assigned
#'                 to `location` if the probability >= cutoff. 
#'           
updateNodeLocation <- function(tree, location, cutoff) {
    
    tree@data$location.updated <- sapply(seq_len(nrow(tree@data)), 
                                         function(i) {
                                            p <- tree@data$location.set.prob[[i]][which(tree@data$location.set[[i]] == location)]
                                            if (length(p) > 0 && p >= cutoff) {
                                              return(location)
                                            } else {
                                              return(paste0("non",location))
                                            }
                                         })
    return(tree)
}


#' Use a depth-first seach to extract all clusters in a tree in a given location (colouring).
#' 
#' @param tree      Coloured input tree. Should be a tidytree (ggtree) tree object with at least 
#'                  `location` and `location.prob` annotated in `tree@data`.
#' @param location  The location name in `tree@data$location` to extract clusters for.
#' @param cutoff    The probability threshold for assigning a node to `location`. Default in 
#'                  BEAST is to simply pick the location with the highest probability.
#'                  To use the default leave as NA. If the cutoff is being used `tree@data`
#'                  should also contain `location.set` and `location.set.prob` 
#'                  (the tree should be an MCC tree). 
#' @param maxheight Maximum height for a node to be labelled as location. If it's unlikely that any 
#'                  any nodes older than date D were in location, then set maxheight to mostrecent - D.
#' @param prefix    Prefix for the cluster names in the output table
#' 
#' @value A data frame where each row represents a node in the tree, with columns for 
#'          - the node's parent, 
#'          - length of edge leading to the node, 
#'          - height of the node in the tree, 
#'          - node type (sample/coalescent), 
#'          - taxon name, 
#'          - location, 
#'          - whether the DFS visited the node, 
#'          - cluster the node belongs to.
#'        Rownames are node numbers (node i is on row i).
getClusterTable <- function(tree, location, prefix = "cluster_", cutoff=NA, maxheight=NA, debug = FALSE) {
  
  start.time <- Sys.time()
  
  # Sometimes tip labels are in quotes
  tree@phylo$tip.label <- gsub("'","", tree@phylo$tip.label)
  
  # Update location if needed
  if (!is.na(cutoff)) {
      tree <- updateNodeLocation(tree, location, cutoff)
  } else {
      tree@data$location.updated <- tree@data$location
  }
  
  ttree <- tree@phylo
  
  treetable <- beastio::getTreeIntervals(ttree, raw=TRUE)          # Nodes should be in order 
  treetable$taxon     <- c(ttree$tip.label, rep(NA, ttree$Nnode))  # Should be in order
  treetable$location  <- as.factor(tree@data$location.updated[order(as.numeric(tree@data$node))])
  treetable$visited   <- rep(FALSE, nrow(treetable))
  treetable$cluster   <- rep(NA, nrow(treetable))
  
  # Update locations that are too old to be in location
  if (!is.na(maxheight)) {
      treetable$location[treetable$types == "coalescent" & treetable$location == "UK" & treetable$heights > maxheight] <- paste0("non",location)
  }
  
  # Check node ordering
  if (any(as.numeric(rownames(treetable)) != 1:nrow(treetable))) {
    stop("Error: Something wrong with node ordering")
  }
  
  break.time <- Sys.time()
  if (debug) cat(sprintf("Setup complete (%s)\n", format(break.time-start.time,format="%S")))
  
  cluster <- 1
  for (i in 1:nrow(treetable)) {
      if (treetable$location[i] == location && is.na(treetable$cluster[i])) {
          start.time <- Sys.time()
          if (debug) cat(sprintf("Starting cluster search from node %d... ", i))
          if (debug > 1) cat("\n\tVisiting node ")
          
          treetable <- clusterDFS(ttree, i, paste0(prefix, cluster), treetable, debug = debug)
          cluster   <- cluster + 1
          
          break.time <- Sys.time()
          if (debug) cat(sprintf("done! (%s)\n", format(break.time-start.time,format="%S")))
      }
  }
  treetable$cluster <- as.factor(treetable$cluster)
  
  return(treetable)
}

