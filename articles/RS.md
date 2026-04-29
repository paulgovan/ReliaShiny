# Repairable Systems Analysis

## Landing

Upon launching the app, you’ll be greeted by the Landing page. From
here, you can:

- Access the Project Site.
- Download an example dataset.
- Navigate to the Repairable Systems menu.

![](https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/Landing.png?raw=true)

## Data

For demonstration, we’ll use the preloaded “Simple Data Set” dataset.
First, navigate to the Repairable Systems menu and click on the Data
sub-menu. Next, under the Data Selection box, select the appropriate
columns for System ID, Event Time, and Event Indicator.

At this stage, your app should resemble the following:

![](https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/Repairable%20Systems/RepairableData.png?raw=true)

You can explore additional options for data arrangement, but for this
example, we’ll proceed with the default settings.

## Modeling

Next, navigate to the Model sub-menu to build your Repairable Systems
model. The app will generate an NHPP Plot using the Power Law NHPP model
by default. Feel free to experiment with different model configurations
to tailor the analysis to your needs.

Next to the plot, you’ll find additional settings for further
customization.

![](https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/Repairable%20Systems/RepairablePlot.png?raw=true)

Next, visit the Exposure Plot tab to visualize the event rate over time.
As with the previous plot, various customization options are available.

![](https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/Repairable%20Systems/ExposurePlot.png?raw=true)

Finally, visit the MCF Plot tab to create a Mean Cumulative Function
plot. As with the previous plots, various customization options are
available.

![](https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/Repairable%20Systems/MCFPlot.png?raw=true)
