---
title: "Análisis de Frecuencias de Letras en Libros para poder realizar el diagrama de Huffman"
output: 
  pdf_document: 
    toc: true
    latex_engine: xelatex
    keep_tex: true
    
author: Dylan Canning
---


# Introducción

En este documento explico como he extraido y analizado las frecuencias de letras de un conjunto de libros en español. Los libros los he descargado de Project Gutenberg utilizando el [gutenberg-bulk-downloader](https://github.com/puntonim/gutenberg-bulk-downloader).

Al haberlo descargado del Project Gutenberg no tienen Copy-Right y se pueden utilizar de manera libre.

```{r setup, include=FALSE}
library(tidyverse)
library(tokenizers)
library(knitr)
```

# Función para leer archivos de forma segura

La siguiente función `read_file` se utiliza para leer el contenido de un archivo de texto. Esta función lee el archivo en codificación "iso-8859-1" (el default de la biblioteca gutenberg.

```{r}
read_file <- function(file_path) {
  result <- tryCatch({
    data <- readr::read_file(file_path, locale = readr::locale(encoding = "iso-8859-1"))
    return(tibble(content = data, file = basename(file_path)))
  }, warning = function(warning_msg) {
    return(NULL)
  }, error = function(error_msg) {
    return(NULL)
  })
  return(result)
}
```

# Leyendo todos los archivos de texto del directorio

A continuación, defino el directorio donde se encuentran almacenados los libros y leeo cada uno de ellos con la función de antes.

```{r}
directory <- "/Users/dylan/Desktop/ProyectoRedes/gutenberg-bulk-downloader/_storage/"
file_paths <- list.files(directory, pattern = "*.txt", full.names = TRUE)
all_books <- map_dfr(file_paths, read_file)
```

# Preparación y limpieza de los datos de texto

Antes de calcular las frecuencias de las letras, hay que limpiar los datos. Elimino todos los caracteres que no son letras y convierto todo el texto a mayúsculas. (Me da error con las minisculas).

```{r}
cleaned_books <- all_books %>%
  mutate(cleaned_content = str_remove_all(content, "[^A-Za-z]")) %>%  # Remove everything except letters
  mutate(cleaned_content = str_to_upper(cleaned_content))  # Convert to uppercase
```

# Cálculo de frecuencias de letras

Una vez el texto esta limpio, procedo a calcular la frecuencia de cada letra. Posteriormente saco el porcentaje en proporcion al total de letras de todos los libros leidos (100).

```{r}
letter_counts <- cleaned_books %>%
  unnest(Letra = tokenizers::tokenize_characters(cleaned_content, strip_non_alphanum = TRUE, simplify = TRUE)) %>%
  count(Letra) %>%
  filter(str_length(Letra) == 1)

total_letters <- sum(letter_counts$n)
letter_counts <- letter_counts %>%
  mutate(Porcentaje = n / total_letters * 100)
kable(letter_counts, caption = "Frecuencias de Letras en Libros", format = "latex")
```
