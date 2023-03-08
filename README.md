# SampleSizer: A Shiny App for Calculating Sample Size

Welcome to the SampleSizer Shiny app! This app is designed to help researchers and statisticians calculate the necessary sample size for a study.

## Features

The SampleSizer app includes the following features:

-   Calculation of sample size for mean, proportion, and incidence rate outcomes
-   Support for cluster designs
-   Support sample size calculation for multiple scenarios
-   Customizable color and x-axis variables for data visualization
-   A tidi-tidy bit of data validation
-   Validators to check input data range

![SampleSizer in action](fig/example_v0.2.0_pre-alpha.gif)

## Installation

To run the SampleSizer app on your local machine, you have two options:

1.  Clone this repository and run either the **`server.R`** or **`ui.R`** file in RStudio.

2.  Download and launch the app by running the following command in R, after installing the **`shiny`** package if it is not already installed:

```{r}
# Install.package("shiny) - # install the package if needed
shiny::runGitHub("SampleSize", "songyosr")
```

You can also access the app online at [**https://songyosr.shinyapps.io/Sample_Size/**](https://songyosr.shinyapps.io/Sample_Size/). However, please note that this version may not always be available, as it is hosted on a limited free user account.

## Usage

To use the app, simply select the outcome type, input method, and other relevant parameters, and then input the data points in the numeric input fields with icons. Click the "Calculate" button to generate the sample size. The app will display the resulting sample size, along with a reactable data frame and a plotly chart for data visualization.

## Short-term Goals [as time permit, lol]

-   Adding options to round (ceiling) the estimated sample size/number of cluster
-   Changing to a new formula that takes in the unequal size arm
-   Adding option for odds ratio for method
-   Reworking on the main panel, mainly focus on organizing the output part (still having no idea what to do)
-   Creating a test_run dataset

## Contributing

If you have any feedback or suggestions for the SampleSizer app, please feel free to submit a pull request or raise an issue on the GitHub repository.

## Author

The SampleSizer app was created by [Songyos Rajborirug](https://github.com/songyosr), who's using this project as an excuse to practice with Shiny. If you find any bugs, it's probably because he's still learning.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
