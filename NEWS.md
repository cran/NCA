# NCA 3.3.2

- nca_outliers: fix bug when no outliers if k == 1
- nca_outliers: force conversion of data to dataframe
- nca_outliers: display '# not shown' outliers
- nca_outliers: add the condensed parameter
- nca_outliers: add the scope parameter
- nca_analysis: set fit to `NA` if fit > 100%
- nca_analysis: add ceiling-specific peers

# NCA 3.3.1

- Changed precision of the effect sizes in simple summary
- Bugfix in nca_outliers: not all scope outliers were reported

# NCA 3.3.0

- Add the number of potential outliers (k) to nca_outliers
  This allows to analyse combinations potential outliers.
- Add nca_outliers argument `min.diff`: a minimum for effect size difference.
- Add nca_outliers argument `max.results`: the maximum number of potential
  outliers reported.
- Add `corner`, `flip.x` and `flip.y` to nca_outliers.
  Their functionality is identical to the nca_alysis arguments.
- The scope argument of nca_analysis now allows `NA`
- Reset the seed after running tests

# NCA 3.2.1

- Parallelize outliers
- Fix bug if no outliers present

# NCA 3.2.0

- Introduce nca_outliers
- Rename column names in example2
- Remove SFA and LH lines
- Add URL for R-Studio
- Fix bug in cutoff

# NCA 3.1.1

- Fix bug in bottleneck: epsilon error

# NCA 3.1.0

- Introduce corner argument
- Augement the 'steps' argument on analysis: allow list of values
- Add # of cases when bottleneck == 'percentile'
- Add Plotly graph
- Various bugfixes

# NCA 3.0.3

- Change various messages
- Start cluster based on nof tasks
- Display percentiles as percentages for Y
- Bottleneck precision changes
- Various bugfixes

# NCA 3.0.2

- Change parallelization for Windows
- Limit test.rep for small h's and enforce unique samples
- Enforce step size > 0
- Appended startup message
- Added effect aggregation
- Make point color user-configurable
- Various bugfixes

# NCA 3.0.1

- Fix bugs in bottleneck table

# NCA 3.0

- Add the fit measure to the summary output
- Add peers to model
- Add rownames to peers
- Add statistical tests
- Add precision for p value
- Add qr.tau parameter

# NCA 2.0

- Split `nca` into `nca_analysis`, use `nca` for simple analysis
- Introduce `nca_output` for summary, bottleneck and plots
- Display bottleneck in console, not in window
- Ceilings as vector
- Allow user to define list of columns for each type of variable
- Introduce flipping, configurable for Y and each X separately
- Introduce step.size parameter
- Add the scope param, with a theoretical scope for every X
- Clip lines to scope region
- Removed 'slow' peer detection
- Update documentation accordingly
- Various bugfixes

# NCA 1.1

- Add missing bottlenecks (qr and ce_vrs)
- Optionally better detection of peers
- Various bugfixes
- Update documentation

# NCA 1.0

- Initial release
