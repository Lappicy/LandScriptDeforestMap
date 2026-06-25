# LandScriptDeforestMap

Pacote R para avaliar desmatamento e mudanças de classes em imagens classificadas de sensoriamento remoto.

Este repositório mantém duas versões instaláveis e independentes:

- `v1.0/`: versão original publicada do pacote;
- `v2.0/`: funções atualizadas e aplicação web R Shiny.

Cada pasta contém um pacote R completo. Por isso, a versão desejada deve ser
informada no argumento `subdir`.

## Instalação passo a passo

### 1. Instalar o R

Baixe e instale o R pelo site oficial do CRAN:

- Windows: <https://cran.r-project.org/bin/windows/base/>
- macOS: <https://cran.r-project.org/bin/macosx/>

Recomenda-se usar uma versão recente do R. A versão `v2.0` do pacote necessita de uma versão mais atual que a 
`4.3.0`.

### 2. Windows: instalar o Rtools

No Windows, instale também o Rtools, porque alguns pacotes podem precisar ser compilados durante a instalação.

Baixe pelo site oficial:

<https://cran.r-project.org/bin/windows/Rtools/>

Escolha o Rtools compatível com a sua versão do R. Por exemplo:

- R 4.5.x ou superior indicado pela página do CRAN: Rtools 4.5;
- R 4.4.x: Rtools 4.4;
- R 4.3.x: Rtools 4.3.

Depois de instalar, reinicie o R/RStudio.

### 3. macOS

No macOS, não existe Rtools e, portanto, não necessita de nenhum download a mais.

### 4. Instalar o pacote `remotes`

Abra o R ou RStudio e execute:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
```

### 5. Instalar a versão 2.0 (mais atual)

```r
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

Para instalar versões anteriores, basta substituir o subdiretório para a versão desejada (como "v1.0" por exemplo)

```r
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
