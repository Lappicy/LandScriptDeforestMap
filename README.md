# LandScriptDeforestMap

Pacote R para avaliar desmatamento e mudanças de classes em imagens classificadas
de sensoriamento remoto.

Este repositório mantém duas versões instaláveis e independentes:

- `v1.0/`: versão original publicada do pacote;
- `v2.0/`: funções atualizadas e aplicação web R Shiny.

Cada pasta contém um pacote R completo. Por isso, a versão desejada deve ser
informada no argumento `subdir`.

## Instalação da versão 2.0 (mais atual)

```r
install.packages("remotes")
remotes::install_github(
  "Lappicy/LandScriptDeforestMap",
  subdir = "v2.0"
)

library(LandScriptDeforestMap)
```

Para iniciar a plataforma Shiny incluída na versão 2.0:

```r
LandScriptDeforestMap::runLandScriptApp()
```

## Instalação de versões anteriores

Na hora de instalar, basta substituir o subdiretório para a versão desejada (como "v1.0" por exemplo)

```r
install.packages("remotes")
remotes::install_github(
  "Lappicy/LandScriptDeforestMap",
  subdir = "v1.0"
)

library(LandScriptDeforestMap)
```

## Importante sobre as versões

As duas versões usam o mesmo nome de pacote: `LandScriptDeforestMap`. Portanto,
instalar uma delas na mesma biblioteca do R substitui a versão anteriormente
instalada.

Para conferir a versão ativa:

```r
packageVersion("LandScriptDeforestMap")
```

## Citação

Lappicy, T.; Cabral, A. I. R.; Da Silva, R. G. P.; Arguelho, J. S.;
de Andrade, S. P. B.; Pereira, A. K.; Laques, A.-E.; Saito, C. H. (2024).
*LandScriptDeforestMap: An R package to evaluate deforestation in remote
sensing images*. SoftwareX, 27, 101799.
https://doi.org/10.1016/j.softx.2024.101799

O software possui Certificado de Registro de Programa de Computador no INPI,
processo `BR512024004176-1`.

## Licença

MIT. O uso é aberto; ao utilizar o pacote, os resultados ou a plataforma,
lembre-se de citar o trabalho.
