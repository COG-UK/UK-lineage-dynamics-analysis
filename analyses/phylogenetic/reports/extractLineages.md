COVID-19 UK introductions
================
Louis du Plessis
Last modified: 11 Sep 2020

-   [Summary](#summary)
    -   [Input](#input)
    -   [Steps](#steps)
    -   [Output](#output)
-   [Extract clusters](#extract-clusters)
-   [Extract cluster samples](#extract-cluster-samples)
-   [Session info](#session-info)

Summary
=======

This notebook extracts TMRCAs and other summary statistics of the DTA clusters across all replicate trees from BEAST.

Input
-----

-   Metadata table in `.csv` format. Should contain a `sequence_name` and `sample_date` column.
-   Set of trees for each treefile with DTA reconstructions save in ../results/BEAST\_dta/2020-08-18/output/.

Steps
-----

-   Extract a table of nodes and heights for each lineage in each tree.
-   Extract TMRCAs and sizes for each lineage in each tree.
-   Extract subtrees for all lineages with at least 100 tips.
-   Extract lineage assignment for all UK sequences in each tree replicate.

Output
------

-   `.csv` files with nodes and heights for each lineage in each tree.
-   `.csv` file with lineage assignments for all UK sequences in each tree replicate.
-   `.Rds` files for each tree (easier to load, since it's already a treedata data structure).
-   Newick trees for all lineages with at least 100 tips.

Extract clusters
================

**Loading A-DTA-20200818.combined.trees:** 1558.744 sec elapsed

-   Most recent tip: 2020-06-08 (2020.43442622951)
-   Maximum UK node height: 0.374316939890832

-   Saving and compressing: 30.329 sec elapsed

-   Extract clusters from trees: 486.221 sec elapsed

**Loading B-DTA-20200818.combined.trees:** 3499.876 sec elapsed

-   Most recent tip: 2020-06-10 (2020.43989071038)
-   Maximum UK node height: 0.379781420765084

-   Saving and compressing: 78.154 sec elapsed

-   Extract clusters from trees: 3986.857 sec elapsed

**Loading B.1.1-DTA-20200818.combined.trees:** 6426.152 sec elapsed

-   Most recent tip: 2020-06-21 (2020.46994535519)
-   Maximum UK node height: 0.4098360655737

-   Saving and compressing: 143.417 sec elapsed

-   Extract clusters from trees: 12261.395 sec elapsed

**Loading B.1.pruned-DTA-20200818.combined.trees:** 5166.131 sec elapsed

-   Most recent tip: 2020-06-22 (2020.47267759563)
-   Maximum UK node height: 0.412568306011053

-   Saving and compressing: 109.678 sec elapsed

-   Extract clusters from trees: 7940.154 sec elapsed

**Loading B.1.X-DTA-20200818.combined.trees:** 4655.068 sec elapsed

-   Most recent tip: 2020-06-15 (2020.45355191257)
-   Maximum UK node height: 0.393442622950943

-   Saving and compressing: 98.146 sec elapsed

-   Extract clusters from trees: 3745.334 sec elapsed

Extract cluster samples
=======================

Processing A-DTA-20200818.combined: 90.031 sec elapsed

Processing B-DTA-20200818.combined: 196.143 sec elapsed

Processing B.1.1-DTA-20200818.combined: 354.869 sec elapsed

Processing B.1.pruned-DTA-20200818.combined: 268.665 sec elapsed

Processing B.1.X-DTA-20200818.combined: 212.928 sec elapsed

Session info
============

    ## R version 3.5.1 (2018-07-02)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Sierra 10.12.6
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] phytools_0.6-99 maps_3.3.0      ggtree_2.1.1    ggsci_2.9      
    ##  [5] treeio_1.11.2   rjson_0.2.20    ggplot2_3.2.1   beastio_0.3.3  
    ##  [9] tidytree_0.3.1  ape_5.3         lubridate_1.7.4 dplyr_0.8.3    
    ## [13] tictoc_1.0     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.3              lattice_0.20-38         tidyr_1.0.0            
    ##  [4] gtools_3.8.1            assertthat_0.2.1        zeallot_0.1.0          
    ##  [7] digest_0.6.23           R6_2.4.1                backports_1.1.5        
    ## [10] evaluate_0.14           coda_0.19-3             pillar_1.4.2           
    ## [13] rlang_0.4.2             lazyeval_0.2.2          phangorn_2.5.5         
    ## [16] Matrix_1.2-18           combinat_0.0-8          rmarkdown_2.3          
    ## [19] stringr_1.4.0           igraph_1.2.4.2          munsell_0.5.0          
    ## [22] compiler_3.5.1          numDeriv_2016.8-1.1     xfun_0.15              
    ## [25] pkgconfig_2.0.3         mnormt_1.5-5            htmltools_0.4.0        
    ## [28] tidyselect_0.2.5        tibble_2.1.3            expm_0.999-4           
    ## [31] codetools_0.2-16        quadprog_1.5-8          crayon_1.3.4           
    ## [34] withr_2.1.2             MASS_7.3-51.4           grid_3.5.1             
    ## [37] nlme_3.1-143            jsonlite_1.6            gtable_0.3.0           
    ## [40] lifecycle_0.1.0         magrittr_1.5            scales_1.1.0           
    ## [43] stringi_1.4.3           scatterplot3d_0.3-41    rvcheck_0.1.7          
    ## [46] vctrs_0.2.1             fastmatch_1.1-0         tools_3.5.1            
    ## [49] glue_1.3.1              purrr_0.3.3             plotrix_3.7-7          
    ## [52] yaml_2.2.0              colorspace_1.4-1        BiocManager_1.30.10    
    ## [55] animation_2.6           clusterGeneration_1.3.4 knitr_1.29
