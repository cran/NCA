# NCA 3.3.1

- Changed precision of the effect sizes in simple summary
- Bugfix in nca_outliers: not all scope outliers were reported 

# NCA 3.3.0
- Add the number of potential outliers (k) to nca_outliers
  This allows to analyse combinations potential outliers.
- Add nca_outliers argument `min.diff`: a minimum for effect size difference.
- Add nca_outliers argument `max.results`: the maximum number of potential outliers reported.
- Add `corners`, `flip.x` and `flip.y` to nca_outliers.
  Their functionality is identical to the nca_alysis arguments. 
- The scope argument of nca_analysis now allows `NA`  
- Reset the seed after running tests
