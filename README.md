# Trabajo Fin de Grado 

**Autor**: Manuel Rocamora Valenti  
**Título**: Respuesta al tratamiento con Pembrolizumab en
pacientes con cáncer de pulmón no microcítico 
**Grado**: Ciencia de Datos  
**Universidad**: Universidad Politécnica de Valencia

---

##  Descripción

Este repositorio contiene el código, figuras y documentación generados como parte del Trabajo Fin de Grado, cuyo objetivo ha sido interpretar la evolución clínica de pacientes con cáncer de pulmón no microcítico tratados con pembrolizumab. El análisis se ha centrado en modelos explicativos y técnicas multivariantes aplicadas a un conjunto real de datos clínicos anonimizados procedentes de un hospital de Valencia.

---

## Contenido del proyecto

- `src/`: Scripts principales del análisis (`PCA`, `PLS-DA`, `sPLS-Cox`, visualizaciones).
- `figures/`: Gráficos generados automáticamente en el análisis.
- `report/`: Documento en LaTeX con el manuscrito final.
- `results/`: Resultados intermedios y objetos serializados.

---

##  Objetivos principales

- Interpretar perfiles clínicos en relación con la evolución del tratamiento.
- Explorar asociaciones entre variables clínicas, inmunológicas y analíticas.
- Evaluar factores relacionados con la supervivencia mediante modelos penalizados de regresión de Cox con reducción de dimensionalidad.
- Facilitar visualizaciones interpretables que apoyen la toma de decisiones clínicas.

---

##  Métodos empleados

- **Análisis de Componentes Principales (PCA)**: Exploración estructural y reducción de dimensionalidad.
- **PLS-DA (Partial Least Squares Discriminant Analysis)**: Asociación entre perfiles y respuesta clínica observada.
- **sPLS-Cox (Sparse Partial Least Squares para modelos de supervivencia)**: Evaluación de la supervivencia libre de progresión y supervivencia global.
- **Transformaciones longitudinales**: Slope, media, diferencias y coeficientes de variación de biomarcadores.
- **Validación cruzada Leave-One-Out (LOO)**: Para todos los modelos aplicados.

---

##  Requisitos

El análisis se ha desarrollado íntegramente en **R** (versión ≥ 4.2). Algunas de las principales dependencias son:

- `mixOmics`
- `survival`
- `Coxmos`
- `ggplot2`
- `tidyverse`
- `mice`
- `VennDiagram`

Para instalar los paquetes necesarios:

```r
install.packages(c("mixOmics", "survival", "ggplot2", "tidyverse", "mice", "VennDiagram"))
# Coxmos debe instalarse desde GitHub:
remotes::install_github("datadiarist/Coxmos")