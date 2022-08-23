# PhD-Codes
Codes written for my PhD undertaken at the University of Bristol from 2018 until 2022.
PhD Thesis title: _Quantifying Pterosaur Launch: biomechanical modelling of an ornithocheiraean pterosaur._

Included scripts relate to Chapters 3 and 4 and are for use with the R software package. Graphing scripts require the Tidyverse ggplot2 library and the patchwork library
Code is written to be purely functional and have not been generalised or optimised for distrubution, these are exactly as I used them to perform my analyses. I am sure most of you could clean these up pretty quickly but they make sense to me.

Included scripts are as follows:

Scripts related to Chapter 3:

MM_combined.R               # combining the moment outputs
mm_mc_graph.R               # graphing function for the muscle moments
MM_Rds_fusion.R             # combining all the .rds files
mm_sense_comp_graph.R       # moment arm graphing with sensitivity
mm_sense_comp_graph2.R      # moment arm graphing with sensitivity
mm_sense_comp_graph3.R      # moment arm graphing with sensitivity
MM1_Monte_Carlo_function.R  # monte carlo script that varied the moment arms
MM2_Monte_Carlo_function.R  # monte carlo script that varied the fmax
mma_mc_graph.R              # graphing for moment arms
MMA_Monte_Carlo.R           # original monte carlo design preloop
MMA_Monte_Carlo_function - Original.R   # monte carlo function with functioning loop from Tom Smith
MMA_Monte_Carlo_function.R              # test function for varying the moment arms muscles
MMc_Monte_Carlo_function.R              # final monte carlo with all the loops
