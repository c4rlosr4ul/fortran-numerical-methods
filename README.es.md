# Métodos Numéricos en Fortran
[![Idioma: Inglés](https://img.shields.io/badge/Language-English-blue)](./README.md)
[![Idioma: Español](https://img.shields.io/badge/Language-Español-green)](./README.es.md)

Este repositorio contiene implementaciones de diversos métodos numéricos en el lenguaje de programación Fortran. Los métodos incluyen la solución de ecuaciones diferenciales ordinarias, interpolación numérica y búsqueda de raíces de ecuaciones, entre otros.

## Contenido

- `class-source`: Contiene los archivos de código fuente de clases a implementar en futuros desarrollos.
- `data`: Contiene archivos de datos utilizados como entrada para los programas de ejemplo.
- `modules`: Contiene módulos de Fortran para métodos numéricos específicos.
- `outputs`: Contiene archivos de salida generados por los programas de ejemplos.
- `source`: Contiene el código fuente principal de los métodos numéricos de los cuales se crean los módulos.
- `trash`: Directorio utilizado para almacenar archivos temporales y en proceso de organización.

## Ejemplos de programas principales

- `main_edo.f90`: Programa principal para resolver ecuaciones diferenciales ordinarias.
- `main_interpolacion.f90`: Programa principal para realizar interpolación numérica.
- `main_raices.f90`: Programa principal para buscar raíces de ecuaciones.

## Cómo compilar y ejecutar

Para compilar y ejecutar los ejemplos de programas principales, sigue estos pasos:

1. Navega al directorio raíz del repositorio.

2. Compila el programa principal de tu elección utilizando `gfortran`. Por ejemplo, para compilar `main_edo.f90`, ejecuta:

   ```bash
   gfortran modules/solucion_edo.f90 main_edo.f90 -o main_edo.out
   ```

3. Ejecuta el programa compilado. Por ejemplo, para ejecutar `main_edo.out`, ejecuta:

   ```bash
   ./main_edo.out
   ```

## Contribuciones

Las contribuciones son bienvenidas. Por favor, crea una rama de tu fork y envía una solicitud de pull (pull request) con tus cambios.

## Licencia

Este proyecto está licenciado bajo la Licencia MIT. Consulta el archivo [LICENSE](LICENSE) para obtener más detalles.
