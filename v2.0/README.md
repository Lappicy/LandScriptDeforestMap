# LandScriptDeforestMap 2.0.0

Esta versão reúne:

- funções científicas atualizadas;
- suporte às versões 4, 7.1, 8 e 10 do MapBiomas;
- gráficos e mapas atualizados;
- aplicação R Shiny completa;
- dados de exemplo.

## Instalação

```r
install.packages("remotes")
remotes::install_github(
  "Lappicy/LandScriptDeforestMap",
  subdir = "v2.0"
)

library(LandScriptDeforestMap)
```

## Aplicação Shiny

```r
LandScriptDeforestMap::runLandScriptApp()
```

A aplicação é copiada para uma pasta temporária antes da execução. Isso evita
que resultados sejam gravados dentro da pasta de instalação do pacote.

## Uso pelas funções

As funções atualizadas podem ser chamadas normalmente depois de carregar o
pacote:

```r
example.files()

malha <- create.mesh(
  geo.file = CavernaMaroaga,
  mesh.size = 0.045
)
```

Os scripts originais recebidos para a versão 2.0 também são preservados em:

```r
system.file("original-v2-scripts", package = "LandScriptDeforestMap")
```
