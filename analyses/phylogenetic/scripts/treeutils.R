require(rjson)
require(treeio)
require(ggplot2)
require(ggsci)
require(ggtree)
require(phytools)


#selectFromSeq <- function(id, group, sep="\\|") {
#return(strsplit(id,split=sep)[[1]][group]) 
#}

#' Select an element from a string of tuples separated by `sep`
#' 
#' @param id The string of tuples
#' @param group The index of the tuple to select (use -1 for the last element)
#' @param sep The separator between tuples
#' 
selectFromSeq <- function(id, group, sep="\\|") {
  if (group > 0) {
    return(strsplit(id,split=sep)[[1]][group]) 
  } else {
    parts <- strsplit(id,split=sep)[[1]]
    return(parts[length(parts)])
  }
}

#' Get parts of a list of ordered tuples separated by `sep`
#' 
#' @param labels The list of tuples as strings
#' @param group The element in the tuples to return (use -1 for the last element)
#' @param sep The separator between tuples
#' 
getSeqParts <- function(labels, group=1, sep="\\|") {
  return(sapply(labels, selectFromSeq, group=group, sep=sep))
}

#' Given metadata table create new tip labels in a tree with the fields, separted by sep
#' 
annotateTree <- function(tree, metadata, taxon="sequence_name", 
                         fields = c("sequence_name", "country", "collection_date"), 
                         sep="|") {
  
  treemeta       <- metadata[match(tree$tip.label, metadata[[taxon]]), ]
  treemeta$taxon <- unname(apply(treemeta[, fields], 1, paste, collapse = sep))
  tree$tip.label <- as.character(treemeta$taxon)
  
  return(tree)
}

#' Convert a phylo object to a treedata object (treeio/ggtree)
#' Add node ages to the treedata
phylo2treedata <- function(phylo) {
  
  if (class(phylo)[1] != "phylo") {
    # Quick hack to satisfy treeio
    if ("phylo" %in% class(phylo)) {
      class(phylo) <- "phylo"
    } else {
      stop("Error: Tree not of class phylo")
    }
  }
  intervals <- beastio::getTreeIntervals(phylo)
  
  heights <- tidytree::tibble(node = intervals$node, height=intervals$height, nodetype=intervals$nodetype)
  new("treedata", file="", treetext=write.tree(phylo), 
      phylo=phylo, data=heights)
}


#' Return ggtree object that can be plotted/saved
#' 
#' @param tree          A ggtree object
#' @param metadata      A data frame with metadata for sequences. First column should labeled taxon and contain the taxa in the tree
#' @param size          Line width
#' @param plotIds       Plot sequence labels
#' @param plotNodes     Plot internal node labels
#' @param plotScale     Plot a scale bar
#' @param plotSupport   Plot posterior support on nodes of BEAST trees. No support for plotting bootstrap support at this stage.
#' @param plotCountry   Colour tips by country
#' @param plotTipPoints Plot points on tips
#' @param plotLegend    Add a legend (if there is anything to add)
#' @param plotAxis      If time tree, add an axis from plotStart to plotEnd, with ticks every xtickBy. 
#'                      Otherwise, add an axis with ticks at mutations from the root.
#' @param alignIds      Align sequence labels
#' @param scaleWidth    Length of the scale bar (in s/s)
#' @param nodepal       Palette to use for country colours
#' @param cols List of colours used, in order (foreground/tree, labels, highlights)
#' @param highlighted_taxa Sequence labels to highlight
#' @param highlighted_tips Tips to highlight (with points)
#' @param legendPosition   Where should the legend be
#' @param seqlen           Length of sequences in the alignment. Used to draw axis for non-timetrees
#' @param timeTree         Usually true for BEAST trees
#' @param plotStart, plotEnd, xtickBy Defines the axis for timetrees
#' 
getTreePlot <- function(tree, metadata, size=0.25, 
                        plotIds=TRUE, plotNodes=FALSE, plotNodeBars=FALSE, plotScale=FALSE, plotSupport=TRUE, 
                        plotCountry=TRUE, plotTipPoints=FALSE, plotLegend=TRUE, plotAxis=TRUE,
                        alignIds=FALSE, scaleWidth=0.005, nodepal=scale_color_aaas(),
                        cols=c("#002147", "#69913B", "#872434", "#4891DC"), 
                        highlighted_taxa=c(), highlighted_tips=c(), nodeBars="height_0.95_HPD",
                        legendPosition=c(0.05,0.75), 
                        seqlen=1000,
                        timeTree=TRUE, plotStart=2009, plotEnd=2019, xtickBy=0.5, ...) {
  
  
  # Base tree
  p <- ggtree(tree, ladderize=TRUE, size=size, color=cols[1], ...) %<+% metadata
  
  # Plot location
  if (plotCountry) {
    p <- p + geom_tippoint(aes(color=location), size=1) + nodePal
  }
  
  # Plot tip points (either all tips, or just a highlighted subset)
  if (plotTipPoints || length(highlighted_tips) > 0) {
    #p <- p + geom_tippoint(aes(subset=chinese), color=cols[3], size=0.5)
    if (length(highlighted_tips) > 0) {
        p <- p + geom_tippoint(aes(subset=(label %in% highlighted_tips)), color=cols[3], size=1)
    } else {
        p <- p + geom_tippoint(color=cols[1], size=1)
    }
  }
  
  # Add posterior support on nodes
  if (plotSupport) {
      if (timeTree) {
          p <- p + geom_point2(aes(subset=!is.na(posterior) & posterior >= 0.8), color=cols[1], size=1)     
      } else {
          # Should adjust this for bootstrap, but later, when we actually use bootstrapped trees...
          #p <- p + geom_point2(aes(subset=!is.na(posterior) & posterior >= 0.75 & posterior < 0.95), color=cols[1], size=0.5) + 
          #        #geom_text2(aes(subset=(!is.na(posterior) & posterior >= 0.75 & posterior < 0.95), label=posterior), vjust=-0.5, hjust=1.25, size=1, color=cols[1]) +
          #         geom_point2(aes(subset=!is.na(posterior) & posterior >= 0.95), color=cols[3], size=0.5) + 
          #         geom_text2(aes(subset=(!is.na(posterior) & posterior >= 0.95), label=posterior), vjust=-0.5, hjust=1.25, size=1, color=cols[3]) 
      }
        
  }
  
  # Add sequence ids on tips
  if (plotIds) {
    p <- p + geom_tiplab(aes(subset=!(label %in% highlighted_taxa)), size=1, hjust=-0.05, align=alignIds, color=cols[2]) 
    p <- p + geom_tiplab(aes(subset=(label %in% highlighted_taxa)),  size=1, hjust=-0.05, align=alignIds, color=cols[3], fontface="bold") 
  }
  
  
  # Add internal node labels (useful for finding clades and debugging)
  if (plotNodes) {
    p <- p + geom_text2(aes(label=node), vjust=1.5, hjust=1.25, size=2, color=cols[2]) 
  }
  
  if (plotNodeBars) {
    p <- p + geom_range(range=nodeBars, color=cols[4], alpha=0.5, size=1.5, center='height') 
  }
  
  
  # Add legend
  if (plotLegend) {
    p <- p + theme(axis.line.y = element_blank(), axis.ticks.y=element_blank(), 
                   legend.direction = "vertical", legend.position = legendPosition, 
                   legend.justification = c("left","center"), 
                   legend.text = element_text(size = 10), legend.title = element_text(size=10))
  }
  
  
  # Add scale bar
  if (plotScale) {
    p <- p + geom_treescale(y=-0.02*tree@phylo$Nnode, fontsize=2.5, linesize=0.25, offset=-0.02*tree@phylo$Nnode, width=scaleWidth) 
  }
  
  # Add axis
  if (plotAxis) {
    p <- p + theme_tree2(axis.line.y = element_blank(), axis.ticks.y=element_blank(), 
                         panel.grid.major = element_line(colour = "grey50", size=0.5, linetype = "dotted"), 
                         legend.direction = "vertical",  legend.position = c(1,0.05),
                         legend.justification = c("right","bottom"), legend.background = element_blank() ,
                         legend.text = element_text(size = 10), legend.title = element_blank()) 
    
    if (timeTree) {
        mrca           <- max(getBranchingTimes(tree@phylo))
        mostrecent     <- decimal_date(max(metadata$date, na.rm=TRUE))
        if (plotIds) {
            plotLimits <- c(-(mrca*0.05), mrca*1.5)
        } else {
            plotLimits=c(-(mrca*0.05), mrca*1.05)
        }
        
        p <- p + scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks = (mrca - mostrecent + seq(plotStart,plotEnd,by=xtickBy)), labels=function(x) x - mrca + mostrecent, limits=plotLimits)
    } else {
        mrca  <- max(getBranchingTimes(tree@phylo))
        if (plotIds) {
            mutations <- 0:(1.5*ceiling(mrca*seqlen))
        } else {
            mutations <- 0:ceiling(mrca*seqlen)
        }
        p <- p + scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks = mutations/seqlen, labels=mutations, limits=c(0, max(mutations)/seqlen))
    }
  }
  
  return(p)
}



#' Adds an extra column to the data table for the tree containing information that can be CORRECTLY added to the
#' plot as node bars using geom_range. 
#' 
#' By default, ggtree::geom_range() does not add the correct bars for the height_range and height_0.95_HPD. In 
#' order to get the right upper and lower limits the values in the data table need to be rescaled. 
#'
#' @param tree treedata structure (ggtree structure). This has to be a beast tree.
addNodeHeights <- function(tree, nodes=NULL, inputrange="", center="", outputrange="") {
  
  n <- nrow(tree@data)
  tree@data$height_0.95_HPD_rescaled <- sapply(1:n, function(x) 2*tree@data$height[x] - tree@data$height_0.95_HPD[[x]])

  return(tree)
}


#' Check if treestr is a beast tree
#' Not very careful - the result could still be an invalid tree
#' 
#' NOT USED ANYMORE
checktree <- function(treestr) {
  
  treestr <- trimws(treestr)  
  header  <- substr(treestr,1,regexpr("=",treestr)[[1]])
  
  if (nchar(header) > 0) {
    headparts <- strsplit(header, '\\s+')
    if (length(headparts[[1]]) == 3 && headparts[[1]][[1]] == "tree" && regexpr(";",treestr)[[1]] == nchar(treestr))
      return(TRUE)
  }
  
  return(FALSE)
}

#' Read BEAST trees file into a list
#' 
#' List has two parts, header and trees
#' Header consists of everything before the newick trees
#' Trees is a list of newick strings with "tree <treename> = " pasted in front
#' 
#' The original file can be reconstructed as:
#' 
#' header\n
#' for t in trees
#'     t\n
#' End;\n
#' 
read.beast.tolist <- function(filename) {
  
  treelines <- readLines(filename)
  
  header <- c()
  trees  <- c()
  
  i <- 1
  while(!grepl("begin trees;", treelines[i], ignore.case=TRUE)) {
    i <- i + 1
  }
  i <- i + 1
  while(!grepl(";",treelines[i])) {
    i <- i + 1
  }
  header <- treelines[1:i]
  
  treelines   <- treelines[(i+1):length(treelines)]
  trees       <- treelines[grep("^\\s*tree", treelines, ignore.case = TRUE)]
  
  treeheaders <- sapply(seq_along(trees), function(i) { trimws(substr(trees[i], 1, regexpr("\\(", trees[i])[[1]]-1 )) })
  treestates <- sapply(seq_along(treeheaders), function(i) { 
    match <- regexpr("STATE_[0-9]+", treeheaders[i])
    trimws(substr(treeheaders[i], match[[1]], match[[1]]+attr(match, "match.length")))
  })  
  names(trees) <- treestates
  
  return(list(header=paste(header,collapse="\n"), trees=trees, treeheaders=treeheaders))
}


#' Write a BEAST NEXUS file read in with read.beast.tolist to a file 
#' as a NEXUS tree with all annotations intact (files should be identical)
#' 
#' Use saveStates to only save specific states
#' 
write.beast.treelist <- function(treelist, file = "", saveStates=NULL) {
  
  if (!is.null(saveStates)) {
    saveStates <- sprintf("STATE_%d", saveStates)
    treelist$trees <- treelist$trees[saveStates]
  }
  
  
  write(treelist$header, file=file, append=FALSE)
  for (tree in treelist$trees) {
    write(tree, file=file, append=TRUE)
  }
  write("End;", file=file, append=TRUE)
  
}


#' Read groups of groupsize BEAST trees at a time from a file using 
#' ggtree's read.beast 
#' (significantly faster and less memory intensive than reading in the 
#' whole file using read.beast)
#' 
read.beast.groups <- function(treesfile, groupsize=500, skip=0, ntrees=NA, debug=FALSE) {
  
  # Read trees from file into list
  start.time <- Sys.time()
  treelist   <- read.beast.tolist(treesfile)
  break.time <- Sys.time()
  if (debug) cat(sprintf("Read %d trees from file (%s)\n",length(treelist$trees), format(break.time-start.time,format="%S")))
  
  if (skip > 0) {
    treelist$trees <- treelist$trees[-c(1:skip)]
    if (debug) cat(sprintf("Skipping first %d trees\n", skip))
  }
  
  if (!is.na(ntrees) && ntrees < length(treelist$trees)) {
      oldlen <- length(treelist$trees)
      treelist$trees <- treelist$trees[1:ntrees]
      if (debug) cat(sprintf("Reading in %d trees (skipping %d)\n", ntrees, oldlen - ntrees))
  }
  
  # Get a new temp file
  outname <- tempfile()
  
  beasttrees <- c()
  groups     <- ceiling(length(treelist$trees)/groupsize)
  if (debug) cat(sprintf("Reading in %d groups of %d trees each\n", groups, groupsize))
  
  
  for (i in 1:groups) {
    
    # Can reuse the same temp file 
    outfile <- file(outname,"w")
    
    if (debug) cat(sprintf("Group %d: Using temp file %s\n", i, outname))
    
    cat(treelist$header, file=outfile)
    cat("\n", file=outfile)
    
    # Write group of trees to temp file
    starttree <- ((i-1)*groupsize) + 1
    endtree   <- min(length(treelist$trees), i*groupsize)
    for (j in starttree:endtree) {
      cat(treelist$trees[[j]], file=outfile)
      cat("\n", file=outfile)
    }
    cat("End;\n", file=outfile)
    close(outfile)
    
    # Read in ggtree
    beasttrees <- c(beasttrees,read.beast(outname))
    
    
    old.time   <- break.time
    break.time <- Sys.time()
    if (debug) cat(sprintf("Group %d processed (%s)\n",i,format(break.time-old.time,format="%S")))
  }
  
  # Set file name in the treedata structure to the original filename instead of the temp file
  for (i in 1:length(beasttrees)) {
      beasttrees[[i]]@file <- treesfile
  }
  
  
  if (debug) cat(sprintf("All trees read and processed (%s)\n",format(break.time-start.time,format="%S")))
  
  # Remove temp file
  unlink(outname)
  
  class(beasttrees) <- "treedataList"
  return(beasttrees)
}
