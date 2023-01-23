# SCI118UOA_ForensicGlassAnalysis

## Project Summary

In forensics, it is common and effective practice to analyse glass fragments from the scene and suspects to gain evidence of placing a suspect at the crime scene. 
This kind of analysis involves comparing the physical and chemical attributes of glass fragments that exist on both the person and at the crime scene, 
and assessing the significance in a likeness that they share. \
However, it is often the case that only a very small sample of glass fragments can be recovered; which limits both the strength and variety of analysis and hypothesis 
testing that can be conducted. \
Professor James Curran and his associates conducted work into developing ideas to strengthen the approach towards comparing, and more specifically grouping fragments 
together based on particularly the glass's *Refractive Index* and *chemical compound concentration*. They theorised and tested their algorithms against conventional methods in forensic glass analysis and appeared successful in their approach. [@CURRAN1997241; @TRIGGS19971]\
\
This project aims to build on these ideas and develop a publicly available **R package** downloadable from the *Comprehensive R Archive Network (CRAN)* which 
incorporates their most successful algorithm for discriminating and grouping glass fragments based on their refractive index: The *Scott-Knott Modification 2 (SKM2)*. 
We are also interested in extending the *Scott-Knott* idea to a more modern and rigorous *multivariate* context; which utilises multiple variable data from the glass 
fragments to discern and group glass fragments in the same context. This algorithm will also be made available for use in the R package labelled `SK4FGA`. 
\
Within this repository are all the important files I used in the development of this package, including the R script used for empirically estimating the distribution of 
\lambda.
