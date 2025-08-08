# CEnTR*IMPACT Toolkit App

Welcome to the CEnTR*IMPACT Toolkit! This guide will help you get the app running on your computer, even if you're new to R and RStudio.

This toolkit provides an ensemble of metrics designed by and for community-engaged scholars to evaluate and communicate the impact of their work through quantitative measures that complement qualitative narratives.

![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg) ![GitHub release (latest by date)](https://img.shields.io/github/v/release/CENTR-IMPACT/centrimpact_app) ![License: MIT](https://img.shields.io/badge/license-MIT-green.svg) [![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)

## Prerequisites

Before starting, make sure you have a stable internet connection for downloading software and packages.

## Step 1: Install R and RStudio

1. Go to https://posit.co/download/rstudio-desktop/
2. Follow the instructions to install both R and RStudio Desktop (free version)
3. Install R first, then RStudio
4. Once installed, open RStudio

## Step 2: Install Essential Packages

Copy and paste the following code into the RStudio console (the bottom-left panel) and press Enter:

```r
# Install essential packages needed for setup
install.packages(c("devtools", "renv"))
```

Wait for these packages to install completely.

## Step 3: Install the centrimpact Package

In the RStudio console, run:

```r
# Install the centrimpact package from GitHub
devtools::install_github("CENTR-IMPACT/centrimpact")
```

## Step 4: Fork and Download the Development Repository

1. Go to https://github.com/CENTR-IMPACT/centrimpact_dev
2. Click the "Fork" button in the top-right corner to create your own copy
3. On your forked repository page, click the green "Code" button
4. Click "Download ZIP"
5. Extract the ZIP file to a location you'll remember (like your Desktop or Documents folder)

## Step 5: Open the Project in RStudio

1. In RStudio, go to File → Open Project
2. Navigate to the extracted folder
3. Look for the file `centrimpact_dev.Rproj` in the base directory
4. Click on it and select "Open"

## Step 6: Install All Required Packages

Now that you have the project open, install all the necessary packages using renv:

```r
# Restore all packages needed for the project
renv::restore()
```

This will automatically install all the packages the app needs. Type "y" when prompted to proceed with the installation. This may take several minutes to complete.

## Step 7: Set Up the Working Directory

1. In RStudio, look for the "Files" pane (usually in the bottom-right)
2. Navigate to the `shinymgr` folder by clicking on it
3. Once you're in the shinymgr folder, click on the gear icon (⚙️) → "Set As Working Directory"

## Step 8: Launch the App

In the RStudio console, run:

```r
shinymgr::launch_shinymgr(getwd())
```

The app should now launch in your web browser!

## Troubleshooting

- **If renv::restore() fails**: Make sure you have a stable internet connection and try again
- **If you get package errors**: Restart RStudio and run `renv::restore()` again
- **If the app doesn't launch**: Check that you're in the correct directory (shinymgr folder) and that all previous steps were completed
- **If you see permission errors**: Make sure you have write permissions in the folder where you extracted the files

## Getting Help

If you encounter issues:
1. Check that you followed each step in order
2. Restart RStudio and try again
3. Ask for help from your instructor or teaching assistant

## Next Steps

Once the app is running, you can begin exploring the CEnTR*IMPACT Toolkit features. Refer to the user documentation for guidance on using the various tools and analyses available.

---

*This toolkit is developed by the CEnTR-IMPACT team. For more information, visit our GitHub repository.*

---

 * **IMPORTANT:** CEnTR*IMPACT should never be used as a single-value indicator for consequential decisions like promotion, tenure, or funding.
 * **DISCLAIMER:** This web application is provided as-is without warranty. Users are responsible for backing up their data. Session timeouts or technical issues may result in data loss.