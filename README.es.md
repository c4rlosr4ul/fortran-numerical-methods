# Métodos Numéricos en Fortran

[![Idioma: Inglés](https://img.shields.io/badge/Language-English-blue)](./README.md) [![Idioma: Español](https://img.shields.io/badge/Language-Espa%C3%B1ol-green)](./README.es.md)

Bienvenido a nuestro repositorio dedicado a las implementaciones de varios métodos numéricos utilizando el lenguaje de programación Fortran. Ofrecemos una amplia gama de métodos que pueden ser utilizados en muchos campos de estudio, desde la resolución de ecuaciones diferenciales ordinarias, interpolación numérica, hasta la búsqueda de raíces de ecuaciones, y mucho más. 

## Estructura del Repositorio

-   `data`: Este directorio almacena archivos de datos que sirven como entrada para nuestros programas de demostración.
-   `modules`: Este directorio contiene módulos de Fortran para distintos métodos numéricos.
-   `outputs`: Este directorio se utiliza para almacenar archivos de salida generados por nuestros programas de demostración.
-   `trash`: Este es un directorio de retención temporal para archivos efímeros y archivos que están actualmente en proceso de organización.

## Programas Destacados y Implementaciones de Métodos

A continuación se presenta una lista de nuestros programas principales y los métodos numéricos que implementan. Cada entrada de la tabla indica el estado actual de implementación. Continuamos trabajando en este repositorio y esperamos completar los métodos inacabados pronto.

### Programas Principales y Sus Métodos

| Programa Principal                | Método                        | Estado |
|-----------------------------|-------------------------------|--------|
| `main_derivative.f90`       | Derivadas Numéricas         | ✔️      |
| `main_integration.f90`      | Método de Trapecios            | ✔️      |
|                             | Método de Simpson 1/3          | ✔️      |
|                             | Método de Simpson 3/8          | ✔️      |
|                             | Método de Romberg                | ✔️      |
| `main_interpolation.f90`    | Interpolación de Lagrange        | ✔️      |
|                             | Interpolación hacia Adelante de Newton| ✔️      |
|                             | Interpolación hacia Atrás de Newton| ❌    |
| `main_ode.f90`              | Método de Euler (1er Orden)    | ✔️      |
|                             | Método de Runge-Kutta 2º Orden (1er Orden)| ✔️  |
|                             | Método de Runge-Kutta 4º Orden (1er Orden)| ✔️  |
|                             | Método de Euler (2do Orden)    | ✔️      |
|                             | Método de Verlet (2do Orden)     | ✔️      |
|                             | Método de Runge-Kutta 2º Orden (2do Orden)| ✔️  |
|                             | Método de Runge-Kutta 4º Orden (2do Orden)| ✔️  |
|                             | Método de Euler-Cromer (Sistema de 1er Orden)| ✔️ |
|                             | Método de Runge-Kutta 2º Orden (Sistema de 1er Orden)| ✔️ |
|                             | Método de Runge-Kutta 4º Orden (Sistema de 1er Orden)| ✔️ |
|                             | Método de Runge-Kutta 2º Orden (Sistema de 2do Orden)| ✔️ |
|                            

 | Método de Runge-Kutta 4º Orden (Sistema de 2do Orden)| ✔️ |
| `main_random_methods.f90`   | Método de Monte Carlo            | ✔️      |
|                             | Integración de Monte Carlo       | ✔️      |
|                             | Integral Doble de Monte Carlo   | ...     |

### Clave de Estado

✔️ - Completado y disponible para su uso.

❌ - Aún no implementado.

... - Parcialmente implementado o en desarrollo.

Este repositorio está en activo desarrollo. Si un método no está actualmente disponible o en desarrollo, por favor revise más tarde para actualizaciones. Las contribuciones son siempre bienvenidas.

[Se pueden encontrar métodos adicionales y su respectivo uso de ejemplo en el directorio completo](#).

## Instrucciones de Compilación y Ejecución

Siga estos pasos para compilar y ejecutar cualquiera de los programas principales:

1.  Navegue al directorio raíz del repositorio.
    
2.  Compile el programa principal deseado usando `gfortran`. Por ejemplo, para compilar `main_derivative.f90`, ejecute:
    
```bash
    gfortran modules/edo_solution.f90 main_edo.f90 -o main_edo.out 
```
3.  Ejecute el programa compilado. Por ejemplo, para ejecutar `main_derivative.out`, ejecute:
```bash
    ./main_edo.out 

```
## Contribuir

Apreciamos y damos la bienvenida a todas las contribuciones. Amablemente cree una rama desde su fork y envíe una solicitud de extracción detallando sus cambios.

## Licencia

Este proyecto opera bajo la Licencia MIT. Consulte el archivo [LICENSE](LICENSE) para obtener detalles más completos. 

Por favor note: Es esencial proporcionar una copia de la licencia o un enlace a ella. El readme menciona que el proyecto está bajo la Licencia MIT pero no proporciona una copia o un enlace. La parte faltante de la licencia hace que no esté claro qué reglas se aplican a las personas que quieren usar, cambiar o contribuir al software.
