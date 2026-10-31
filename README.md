# Figure plotting code from source data

R scripts for plotting Figures 1–4 and Extended Data Figures 1–3 from the accompanying Excel source data.

Each script reads exported results, applies the formatting needed for plotting, and assembles a multi-panel figure. Estimates and confidence intervals are read from the workbooks; exported density curves are plotted directly. The scripts do not refit statistical models or require person-level data, `.RData` files, or the original analysis environment.

## Repository structure

Keep the four source-data folders beside the code folder:

```text
repository/
├── plot_code/
│   ├── Fig1.R
│   ├── Fig2.R
│   ├── Fig3.R
│   ├── Fig4.R
│   ├── EDFig1.R
│   ├── EDFig2.R
│   └── EDFig3.R
```

Preserve source folder names, file names, capitalization, and worksheet names. Existing PDF/TIFF figures serve as references and are not read at runtime. Extended Data Table 1 is outside the scope of these seven scripts.

## Requirements

Install the dependencies in R:

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "ggplot2", "patchwork", "scales",
  "gridExtra", "cowplot", "ggrepel",
  "sf", "rnaturalearth", "rnaturalearthdata"
))
```

`grid` is included with R. Figure 1 additionally uses the mapping and layout packages above, including the medium-resolution Natural Earth basemap supplied through `rnaturalearth` and `rnaturalearthdata`.

Use a version of `patchwork` that provides `free()`, which is called by Figure 2. Package versions have not been pinned, and a tested R/package environment is not yet documented.

## Run the scripts

In an interactive R session, set the working directory to the repository root and run the script for the desired figure:

```r
source("plot_code/Fig1.R", encoding = "UTF-8")
source("plot_code/Fig2.R", encoding = "UTF-8")
source("plot_code/Fig3.R", encoding = "UTF-8")
source("plot_code/Fig4.R", encoding = "UTF-8")
source("plot_code/EDFig1.R", encoding = "UTF-8")
source("plot_code/EDFig2.R", encoding = "UTF-8")
source("plot_code/EDFig3.R", encoding = "UTF-8")
```

Alternatively, set the working directory to `plot_code/` and run, for example, `source("Fig1.R", encoding = "UTF-8")`. Each script checks the working directory first and then its parent for the source-data folder. Paths are resolved from the working directory, not from the location of the script itself.

For a different directory layout, set `DATA_ROOT` in the script to the directory containing the four source-data folders. No personal absolute paths are embedded in the code.

Use a fresh R session for each script when checking reproducibility. Each script defines its plotting objects, but some object names are reused across figures.

## Figure objects and export

The scripts display the assembled figures and leave the following objects in the R session:

| Script | Final figure object |
| --- | --- |
| `Fig1.R` | `FIG1` |
| `Fig2.R` | `P_FIG2` |
| `Fig3.R` | `P_FIG3` |
| `Fig4.R` | `P_FIG4` |
| `EDFig1.R` | `P_ED1` |
| `EDFig2.R` | `P_EDFIG2` |
| `EDFig3.R` | `P_EDFIG3` |

The scripts contain no explicit figure-export calls and do not overwrite Excel inputs. To save a figure, export its object explicitly. For example, after running `Fig3.R`:

```r
ggplot2::ggsave(
  filename = "Fig3_reproduced.pdf",
  plot = P_FIG3,
  device = "pdf",
  width = 12,
  height = 9,
  units = "in",
  bg = "white"
)
```

Figure size, graphics device, and installed fonts can affect text placement. In a non-interactive R session, the default graphics device may create `Rplots.pdf` when plots are drawn; select an explicit graphics device when running in batch mode.

## Source-data and panel mapping

Panel letters refer to the final figures. Worksheet names are shown for multi-sheet workbooks or where explicitly selected in the code.

| Figure | Source folder | Panel-to-workbook mapping |
| --- | --- | --- |
| Figure 1 | `fig1&edfig1` | a: `1a.xlsx`; b: `1b.xlsx`; c: `1c.xlsx`; d: `1d.xlsx` (`Fig1d`) |
| Figure 2 | `fig2&edfig2` | a: `2a.xlsx` (`top`, `bottom`); b: `2b.xlsx` (`Self_hiring_probability`, `Pathway_composition`); c: `2c.xlsx` (`fig2c`); d: `2d.xlsx` (`density_curves`) |
| Figure 3 | `fig3` | a: `3a.xlsx` (`top`, `bottom`); b: `3b.xlsx`; c: `3c.xlsx`; d: `3d.xlsx` |
| Figure 4 | `fig4&edfig3$edtable` | a: `4a.xlsx`; b: `4b.xlsx`; c: `4c.xlsx` (`Curves`); d: `4d.xlsx` (`Results`); e: `4e.xlsx` (`Results`) |
| Extended Data Figure 1 | `fig1&edfig1` | a: `ED1a.xlsx`; b: `ED1b.xlsx` (`ed1b_selected`); c: `ED1c.xlsx` (`ed1c`); d: `ED1d.xlsx` (`ed1d`) |
| Extended Data Figure 2 | `fig2&edfig2` | a: `ed2a.xlsx` (`ed2a`); b: `ed2b.xlsx` (`b_tab`); c: `ed2c.xlsx` (`top`, `bottom`); d: `ed2d.xlsx` (`curves`); e: `ed2e.xlsx` (`left`, `right`) |
| Extended Data Figure 3 | `fig4&edfig3$edtable` | a: `ED3a.xlsx` (`Publications`, `JIF`); b: `ED3b.xlsx` (`Density`, `Median`); c: `ED3c.xlsx` (`Density`, `Median`); d: `ED3d.xlsx` (`Publications`, `JIF`); e: `ED3e.xlsx` (`Results`); f: `ED3f.xlsx` |

