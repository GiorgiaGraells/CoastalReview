Respaldo Review
================

## Documentos y archivos

**ggmaps FEE.R** se realizaon los mapas de artículos publicados por país
o ciudad, considerando la población por país y ciudad (tambien
crecimiento). Se intentó modificar el codigo con la descarga mediante
**screaping.R** para generar **Pop2020.csv**, pero en realidad esos
datos solo eran estimaciones de poblacion del 2018-2019 y 2020 mundial.
el archivo **DF\_by\_citySF.rds** cuenta con la info de **UNdata.csv**
con datos correctos, pero el codigo donde se realizó el filtrado
considerando solo los articulos de **Articulos\_al2020.csv**, fue
borrado sin querer. el archivo **Correccion\_UNdata\_pobl** tomó
**UNdata.csv** y lo modifico (para recuperar lo que fue borrado) y se
dejaron los datos necearios apra el trabajo, siendo guardado como
**UNdata\_corregido.rds**

**Figuras\_mar2020\_review\_coastal** Posee todas las figuras del review
actualizados. ultima actualizacion con correcciones de coautorees,
comentarios dde arreglos de tablas y arreglos en las poblaciones a
partir de **DF\_by\_citySF.rds**

**2020\_15mar\_COASTALREVIEW\_Giorgia** El último manuscrito en RMD,
contiene parte importante de las referencias
**tesis\_ref\_2019sept.bib**, pero el actualizado está en word por las
revisiones. Ultimo manuscrito con correcciones coautores solo en formato
word

**NuevoRedes.R** realiza el análisis de redes de referencias cruzadas
entre paradigmas del review. Se parte de **DFbib.rds**. El producto es
la red **red.RDS** que luego es graficado en
**Figuras\_mar2020\_review\_coastal**

Se incluye lo trabajado en la revisión de lo que se publicará archivos
word

Arreglo de figuras en **Figuras\_mar2020\_review\_coastal** con
correcciones en GIMP en word

Figuras para STOTEN
**Figuras\_sept2020\_review\_coastal\_urban\_ecology.Rmd**. Kniteo a
pdf, se cambio el Yaml para que las figuras quedaran guardadas en una
carptea en pdf. Se trabajo en gimp, abrir imagne con 500 pdi y luego
modificando tamaño imagen según journal (una columna, una y media o dos
columnas dde ancho) con Imgas-Scale image…- cambiar tamaño del ancho
Cortar y pegar para ordenar paradigmas en leyenda (flatten image para
rellenar hoyos del corte y poner todo en blanco)
