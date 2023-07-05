# Análisis predictivo de la popularidad de una canción en Spotify

Este repositorio contiene el código en R utilizado para modelar y predecir la popularidad de una canción en Spotify. Estos archivos de código pertenecen al TFM del Master en Minería de Datos e Inteligencia de Negocio de la Universidad Complutense de Madrid.

Detalle de cada uno de los archivos:
- **DATOS.zip**: incluye los datos utilizados para el desarrollo de la investigación, incluye el corpus de noticias falsas, stopwords, y CORPES de la RAE.
- **00-Exploración.R**: incluye la limpieza y unificación de todos los datasets con los que contamos para crear la base de datos final y su posterior análisis exploratorio y depuración.
- **01-Modelización.R**: incluye la preparación de la base de datos para la modelización (estandarización y dummificación), la selección de variables a partir de la construcción de modelos AIC y BIC y la ejecución de todos los modelos predictivos tratados, finalizando con el ensamblado de modelos.
- **Funciones.zip**: funciones de Validación Cruzada Repetida para todos los modelos de Machine Learning.
