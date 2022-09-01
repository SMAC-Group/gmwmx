# gmwmx version 1.0.0

Initial release 

# Update gmwmx version 1.0.1

- Change vignette load_estimate_compare_models.Rmd so that no estimation is performed when compiling the vignette. Saved estimated models in inst/extdata.


# Update gmwmx version 1.0.2

- Modified how is inverted the Sigma matrix in estimate_gmwmx() by now using ltsa::TrenchInverse to exploit toeplitz structure of the matrix. 
- Modified how is computed the Wavelet Variance of the GLS residuals to be performed on the available wavelets coefficients in presence of missing observations.
- Corrected bug in estimate_hector() that led to non-existent names for the vector of estimated functional parameters, causing problems when printing the estimated model.
- Corrected bug in estimate_hector() that provided the wrong value FullCov to the argument LikelihoodMethod for Hector ctl file. The default is now set to AmmarGrag and the user can specify it with the new argument likelihood_method of the function estimate_hector().