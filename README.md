# PhD-Codes
Codes written for my PhD undertaken at the University of Bristol from 2018 until 2022.
PhD Thesis title: _Quantifying Pterosaur Launch: biomechanical modelling of an ornithocheiraean pterosaur._

Included scripts relate to Chapters 3 and 4 and are for use with the R software package. Graphing scripts require the Tidyverse ggplot2 library and the patchwork library
Code is written to be purely functional and have not been generalised or optimised for distrubution, these are exactly as I used them to perform my analyses. I am sure most of you could clean these up pretty quickly but they make sense to me.

Included scripts are as follows:

# Scripts related to Chapter 3:

#Graphs
mm_mc_graph.R               
mm_sense_comp_graph.R       
mm_sense_comp_graph2.R      
mm_sense_comp_graph3.R      

#Analysis
MM_combined.R               # combining the moment outputs

MM_Rds_fusion.R             # combining all the .rds files

MM1_Monte_Carlo_function.R  # monte carlo script that varied the moment arms

MM2_Monte_Carlo_function.R  # monte carlo script that varied the fmax

mma_mc_graph.R              # graphing for moment arms

MMA_Monte_Carlo.R           # monte carlo design preloop

MMA_Monte_Carlo_function - Original.R   # monte carlo with functioning loop from Tom Smith

MMA_Monte_Carlo_function.R              # test function for varying the moment arms muscles

MMc_Monte_Carlo_function.R              # final monte carlo with all the loops


# Scripts relating to Chapter 4:

#graphs for acceleration forces, requires patchwork
launch_acceleration_force_graphs_multi.R  
launch_acceleration_force_graphs_multi_an.R

#graphs for heights
launch_lines_2_burst_graph.R
launch_lines_2_counter_graph.R
launch_lines_2_graph.R
launch_max_error.R
launch_ribbons_2_graph.R

#Analysis
averager.R #Thanks to Joe Flannery Sutherland for fixing this
launch_calc_time2.R
launch_calc_time2_anaero.R
launch_calc_time2c.R
launch_calc_time2c_anaero.R
launch_calc2.R
launchforce_loop.R
launchforce_loop_anaerobic.R


