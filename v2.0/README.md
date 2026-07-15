# LandScriptDeforestMap 2.0.0

Esta versão reúne:

- funções científicas atualizadas;
- suporte às versões 4, 7.1, 8 e 10 do MapBiomas;
- gráficos e mapas atualizados;
- gráficos e mapas com alternância entre português do Brasil e inglês;
- aplicação R Shiny completa;
- dados de exemplo.

## Instalação passo a passo

### 1. Instalar o R

Baixe e instale o R pelo site oficial do CRAN:

- Windows: <https://cran.r-project.org/bin/windows/base/>
- macOS: <https://cran.r-project.org/bin/macosx/>

Esta versão do `LandScriptDeforestMap` requer `R >= 4.3.0`.

### 2. Windows: instalar o Rtools

Se você estiver no Windows, instale também o Rtools. Ele é usado quando algum
pacote precisa ser compilado durante a instalação.

Baixe pelo site oficial:

<https://cran.r-project.org/bin/windows/Rtools/>

Escolha a versão do Rtools compatível com a sua versão do R. Exemplos:

- R 4.5.x ou superior indicado pela página do CRAN: Rtools 4.5;
- R 4.4.x: Rtools 4.4;
- R 4.3.x: Rtools 4.3.

Depois de instalar, reinicie o R/RStudio.

### 3. macOS

No macOS, não se instala Rtools. Normalmente basta instalar o R pelo CRAN e usar
os pacotes binários.

Se algum pacote precisar ser compilado a partir do código-fonte, pode ser
necessário instalar ferramentas de desenvolvimento, como Xcode e compilador
Fortran:

<https://cran.r-project.org/bin/macosx/tools/>

### 4. Instalar o pacote `remotes`

No R/RStudio, execute:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
```

### 5. Instalar o LandScriptDeforestMap 2.0 pelo GitHub

```r
remotes::install_github(
  "Lappicy/LandScriptDeforestMap",
  subdir = "v2.0",
  upgrade = "never"
)

library(LandScriptDeforestMap)
```

## Aplicação Shiny

```r
LandScriptDeforestMap::runLandScriptApp()
```

A aplicação é copiada para uma pasta temporária antes da execução. Isso evita
que resultados sejam gravados dentro da pasta de instalação do pacote.
Essa cópia contém apenas os arquivos da interface. Os rasters classificados
são lidos diretamente da pasta selecionada e não são duplicados no disco
interno nem na pasta proxy.

Os resultados podem ser baixados em um único arquivo ZIP. Esse ZIP pode ser
carregado novamente na aba **Gráficos e mapas**, que seleciona automaticamente
o GeoPackage completo e a camada `mesh`.

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
