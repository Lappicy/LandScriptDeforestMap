# LandScriptDeforestMap
Deforestation analysis on classified images

The following code is aimed to help with deforestation analysis, using classified images - such as from MapBiomas.

## Download the package in R/Rstudio

To download the package through **R**, you must have downloaded the *devtools* package (https://cran.r-project.org/web/packages/devtools/index.html). If using windows as an OS (Operating System), it is needed to first download **Rtools**, from the website (https://cran.r-project.org/bin/windows/Rtools/).

To install and open the *devtools* package using the command line in R, run the following codes:
```r
install.packages("devtools")
library(devtools)
```
If the package (*devtools*) ir properly installed and opened, you must then install, and load, the *LandScriptDeforestMap* package through the github link with the code:
```r
devtools::install_github("Lappicy/LandScriptDeforestMap")
library(LandScriptDeforestMap)
```

## Usage
By defining a geospatial file (geopackage or shapefile) and a folder with the needed images, this software will produce three main results for a proposed mesh (the refinement may be adjusted by an argument on the functions used):
1. A geospatial file with the frequency for each class inside each grid cell (from the proposed mesh);
2. Compare the deforestation time series with the growth of known classes (agriculture, pasture, urban and mining);
3. A map showing heatmaps for the acumulated, or for a given year, deforestation (or one of the other known classes).

## Practical example
As an example, within this github there is data for a study case of the indigenous territory of Uaça 1 and 2, inside Brazil.

### Data used
To open the data used here, one must simply write at the console:
```r
data("Uaca_1_2")
data("MapBiomas_71_Uaca")
```

The "Uaca_1_2" is a sf object with the boundaries for Uaça 1 and 2. The "MapBiomas_71_Uaca" is a list, that contains the collection 7.1 from MapBiomas for the years 1985 to 2021 (37 elements on this list). These images were already cut to only have the envolving rectangle of Uaça 1 and 2. This was made to make the folder lighter, but using the original images should not affect the results or the processing time by any significant standards.

### Other way to acess data
Another way is to manually download everything from this github directory. There is an "Example application" folder, within it you can find a folder entitles "Data" which has 2 subdirectories. One, "GPKG", has a geopackage file with the boundaries of Uaça 1 and 2, and a folder entitles "Mapbiomas71" has MapBiomas images MapBiomas (collection 7.1) from 1985 to 2021.

### The code itself
There are only three main codes one must run for this practical example. One does the analysis and creates a sf object with all the necessary data for future analysis. 

### Analysis
To run the analysis (this may take a few minutes, up to ~15 depending on your computer specifications), one must simply define the geospatial data used (Uaça) and the folder on which you downloaded the images or the MapBiomas object created (MapBiomas_71_Uaca). This will output an object with a table like format, named "FinalAnalysis". The first and last two lines of it may be seen after the code.
```r
FinalAnalysis <-
  Growth.Analysis(geo.file = Uaca_1_2,
                  tif.folder = MapBiomas_71_Uaca,
                  mesh.size = 0.045,
                  output.folder = "Results/",
                  output.name = "Analysis_Uaca.txt",
                  MAPBIOMAS = 7.1)
```

| ID_mesh |	Country | Category | Name | Year | Deforestation | Reforestation | Growth_Urban | Growth_Mining | Growth_Pasture | Growth_Agriculture | Forest | NonForest | Water | Others | Urban | Mining | Pasture | Agriculture | 0 | 11 | 12 | 15 | 3 | 33 | 4 | 41 |
| :----------: | :----------: | :--------------------: | :--------------------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: | :----------: |
| 1	| Brazil | Indigenous Land | Uaçá I and II | 1985 | NA | NA | NA | NA | NA | NA | 10.6983 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | NA | NA | NA | NA | 10.6983 | NA | NA | NA |
| 1 |	Brazil | Indigenous Land | Uaçá I and II | 1986 | 0 | 0 | 0 | 0 | 0 | 0 | 10.6983 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | NA | NA | NA | NA | 10.6983 | NA | NA | NA |
| ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... | ... |
| 222 | Brazil | Indigenous Land |	Uaçá I and II |	2020 | 0 | 0 | 0 | 0 | 0 | 0 | 0.1008 | 0.234 | 0 | 0 | 0 | 0 | 0 | 0 | NA | NA | 0.234 | NA | 0.1008 | NA | NA | NA |
| 222 | Brazil | Indigenous Land |	Uaçá I and II |	2021 | 0 | 0 | 0 | 0 | 0 | 0 | 0.1008 | 0.234 | 0 | 0 | 0 | 0 | 0 | 0 | NA | NA | 0.234 | NA | 0.1008 | NA | NA | NA |

### Correlation analysis with gg.deforestation.cor function
Below it is shown the correlation of different known classes in Uaça 1 and 2. To run this one must write:
```r
gg.deforestation.cor(proxy.table = FinalAnalysis,
                     type.one = "Growth",
                     type.two = "Deforestation",
                     color.one = c("purple", "grey50", "#EA9999", "darkorange"),
                     color.two = "darkgreen",
                     save.as = "Results/Deforestation vs Growth.png",
                     title.name = "Analysis for Uaça 1 e 2")
```
![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Results/Deforestation%20vs%20Growth.png?raw=true)

### Output map from the mesh.map function
Lastely, we bring the result of the outputed map for the acumulated deforestation in Uaça 1 and 2. To achieve this, you should run the following code:
```r
mesh.map(mesh.data = FinalAnalysis,
         save.map.as = "Results/Map acumulated deforestation.png",
         map.height = 2300, map.width = 2100, map.units = "px")
```
![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Results/Map%20acumulated%20deforestation.png?raw=true)

## Other practical examples
The code proposed in here was widely teste throughout the Amazon region, using many different spatial resolutions. Below we bring also some examples of possible outputs, using the Guiana Shield Region. The first image shows the gridded map, which encompasses 6 countries (Brazil, Columbia, French Guiana, Guyana, Suriname and Venezuela) that belongs to the Guiana Shield Region. We may see through this map where the deforestation is more agressive, as well as showing the behaviour for other 3 known classes from MapBiomas. In addition to the different countries, we also used (with a specific geospatial file) where the protected areas where - and, therefore, giving a more important information for the deforestation patterns.

![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Others/Guiana%20Shield%20Example.png?raw=true)

We could take a closer look to the correlations for Brazil. Where the function automatically gives us the highest correlation coefficient found between deforestation and the other classes (+0.70 for Pasture).

![alt text](https://github.com/Lappicy/DeforestMapBiomas/blob/main/Example%20application/Others/Brazil%20example.png?raw=true)


