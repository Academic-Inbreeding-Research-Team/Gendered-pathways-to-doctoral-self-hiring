# Figure source data and plotting code

This repository contains figure-level source data and R plotting scripts associated with the accompanying research study. It is intended to make the reported visualizations transparent and facilitate their reproduction from the provided, processed figure data.

## Repository structure

The two principal directories are:

| Directory | Contents |
|---|---|
| `source_data/` | Excel files containing data underlying individual figure panels. |
| `plot_code/` | R scripts used to create figures. |

 `Fig` denotes a main-text figure, while `ED` denotes an Extended Data figure; panel letters identify the corresponding subfigure.

## Reproducing the figures

1. Download or clone the repository.
2. Install R and the packages required by the relevant plotting script. Consult the `library()` and package-qualified calls within each `.R` file for dependencies.
3. Set the working directory to the repository root and update any machine-specific input or output paths in the script so that they point to the local `source_data/` directory and your chosen output directory.
4. Run the corresponding R script, either from an R session or from the command line. For example, after checking paths and dependencies, run `Rscript plot_code/Fig3.R`.

Individual scripts may use data objects or intermediate files beyond the examples listed above. Consult the script and accompanying files for its specific input requirements. Plot dimensions, output formats and other graphics settings are determined by the individual scripts.

## Contact

For questions about the supplied source data or plotting scripts, please use the repository's Issues page, if enabled.
