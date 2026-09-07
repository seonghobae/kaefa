# kaefa (development version)

* Reduced local R workflow load for prose-only Markdown changes while keeping
  `docs/product/kaefa-core-api-contract.md` inside the package contract checks.
* Added a regression contract for the local workflow path filters and aligned
  the architecture documentation with the live central/local gate boundary.

# kaefa 0.1.428.1

## New Features

* Added fitdistrplus integration for empirical theta prior estimation
  - `fitThetaPrior()`: Fit distributions to raw scores using fitdistrplus
  - `testThetaPriorCalibration()`: Test if calibration works for non-nominal models by comparing raw score distributions with theta estimates
  - `applyThetaPrior()`: Apply fitted distribution during model calibration with automatic validation
  - New vignette "Setting Theta Priors with fitdistrplus" documenting the functionality
  - Comprehensive examples in `examples/` directory

* Added fitdistrplus as a package dependency

## Purpose

This update addresses the need to set theta priors based on empirical raw score distributions, particularly useful for:
- Non-representative samples
- Validating model calibration for non-nominal models
- Documenting empirical ability distributions
- Improving reproducibility of IRT analyses

# kaefa 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.


