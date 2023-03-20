# gmwmx version 1.0.0

Initial release 

# Update gmwmx version 1.0.1

- Change vignette load_estimate_compare_models.Rmd so that no estimation is performed when compiling the vignette. Saved estimated models in inst/extdata.

# Update gmwmx version 1.0.2

- Modified how is inverted the Sigma matrix in estimate_gmwmx() by now using ltsa::TrenchInverse to exploit toeplitz structure of the matrix. 
- Modified how is computed the Wavelet Variance of the GLS residuals to be performed on the available wavelets coefficients in presence of missing observations.
- Corrected bug in estimate_hector() that led to non-existent names for the vector of estimated functional parameters, causing problems when printing the estimated model.
- Corrected bug in estimate_hector() that provided the wrong value FullCov to the argument LikelihoodMethod for Hector ctl file. The default is now set to AmmarGrag and the user can specify it with the new argument likelihood_method of the function estimate_hector().

# Update gmwmx version 1.0.3

- Minor modification of the acknowledgements.
- Extend documentation.
- Add support for point estimates for the GMWMX-1 for model that includes a Random Walk in the stochastic model.
- Add reference to doi: 10.1007/s00190-023-01702-8.
- Change number of decimals for std of functional parameters in `print.gnsstsmodel`.

