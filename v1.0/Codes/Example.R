# Read data from package ####
example.files()


# Caverna do Maroaga (Presidente Figueiredo) example ####
FinalAnalysis <- Growth.Analysis(geo.file = CavernaMaroaga,
                                 tif.folder = MapBiomas_8_example,
                                 mesh.size = 0.045,
                                 output.folder = "Results/",
                                 output.name = "Analysis_CavernaMaroaga",
                                 MAPBIOMAS = 8)


# Graph number 1 (time series + correlation) ####
graphical.timeseries(proxy.table = FinalAnalysis,
                     comparison.names = c("Growth_Agriculture", "Growth_Mining",
                                          "Growth_Pasture", "Growth_Urban"),
                     comparison.color = c("darkorange", "grey50",  "#EA9999", "purple"),
                     save.as = "Results/Maroaga Deforestation vs Growth.png",
                     title.name = "Analysis for Caverna do Maroaga")


# Graph number 2 (mesh map) ####
map.layout(mesh.data = FinalAnalysis,
           class = "Deforestation",
           year.used = "all",
           col.limits = c(0, 1, 2, 5),
           save.map.as = "Results/Map acumulated deforestation.png")

