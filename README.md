# Problem set 1 
#En este script usted encontrará los comando ejecutados para realizar el primer problem set del curso BD and ML for applied economics. 
#Para que pueda replicar el ejercicio debe primero cargar las bases de datos disponibles en la url a través de un proceso de web scrapping y luego unirlas en un data frame. 
#Seguido a esto cargue los paquetes y empiece la limpieza inicial de la base, limitando la base a la población de interes (población mayor de edad ocupada). Esto es lo que denominamos base2. 
#Corra todos los códigos correspondientes al análisis descriptivo para responder a las preguntas 

## 1. Construir un modelo de ingresos individuales 

#### a. Describa brevemente el conjunto de datos, el proceso de adquisición de los datos y si existen restricciones para acceder a estos datos o extraerlos.

#### b. Limpieza y descripción de datos. Nos centraremos únicamente en las personas empleadas mayores de dieciocho (18) años. Restrinja los datos a estos individuos y realice un análisis descriptivo de las variables utilizadas en el conjunto de problemas. Tenga en cuenta que la GEIH: • Contiene múltiples medidas de ingresos. Elija las que mejor se adapten a su análisis. • Hay muchas observaciones con datos faltantes. Le dejo a usted encontrar una manera de manejar estos datos faltantes.
#### Como mínimo, debe incluir una tabla de estadísticas descriptivas. Con análisis y la justificación de sus elecciones de datos.


#Luego, desde la línea 171 hasta la 230 podrá correr el modelo correspondiente al punto: 
## 2. Estimar ingresos por edad para individuos de la muestra. 

#### • Discusión de la variable elegida como medida de los ingresos. • Interpretación de los coeficientes y su significado. • Discusión del ajuste de muestra del modelo • Gráfica del perfil estimado de ingresos por edad implícito en la ecuación anterior. • Discusión de la "edad máxima" con sus respectivos intervalos de confianza. (use bootstrap para construir los intervalos de confianza).

#Tenga en cuenta que la mayoría del código se basa en la base denominada base2, la cual tiene el filtro de edad y de ocupación. Posteriormente necesitará las bases base_mujeres y base_hombres para análisis diferenciados. 

#Para esta parte de la brecha de género corra desde la línea 232 hasta la 356

## 3. Brecha salarial de género.


#### a. Estime y discuta brecha de ingresos incondicional.


#### b. Estime, grafique y discuta el perfil de edad-ingresos pronosticado y las "edades máximas" implícitas con los respectivos intervalos de confianza por género.


#### c. Estime una brecha salarial condicional que incorpore variables de control tales como características laborales y laborales similares. Para resaltar que los resultados son los mismos usando la regresión "larga" o FWL, incluya una comparación de ambos en sus resultados Incluya una tercera estimación usando FWL y bootstrap para construir el error estándar.


#### • Discusión de la variable elegida como medida de la Renta, si es igual o diferente al punto anterior. • Interpretación de los coeficientes "femeninos", una comparación entre los modelos y el ajuste en la muestra. • Discusión sobre las edades máximas implícitas y su similitud/diferencia estadística. • Discusión reflexiva sobre la brecha de ingresos incondicional y condicional, buscando responder si los cambios en el coeficiente son evidencia de un problema de selección, un "problema de discriminación" o ninguno de estos problemas.

#Finalmente, para el modelo predictivo corra el script desde la línea 357 en adelante. Tenga en cuenta que las bases más importantes para este punto son la de entrenamiento denominada train y la de teteo denominada test. Con estas se hará la mayoría de ejercicios correspondientes a: 

## 4. Evaluar el poder predictivo de estas especificaciones.


#### a. Dividir la muestra en dos: una muestra de entrenamiento (70%) y otra de prueba (30%). (No olvide establecer una semilla para lograr la reproducibilidad. En R, por ejemplo, puede usar set.seed(10101), donde 10101 es la semilla).


#### b. Informe y compare el rendimiento predictivo de todas las especificaciones anteriores con al menos cinco (5) especificaciones adicionales que exploran las no linealidades y la complejidad.


#### c. Comente i. Sobre la métrica de rendimiento que ha elegido y su justificación para elegirla. ii. Acerca de la especificación con el error de predicción más bajo. iii. Para la especificación, explore aquellas observaciones que parecen "errar el blanco". Para hacerlo, calcule la estadística de influencia para cada observación en la muestra de prueba y examine su distribución. ¿Están las observaciones en las colas de la distribución de la estadística de influencia? ¿Son estos valores atípicos personas potenciales que la DIAN debería investigar, o son simplemente el producto de un modelo asombrado?


#### d. LOOCV. Para los dos modelos con el error predictivo más bajo en la sección anterior, calcule el error predictivo utilizando la validación cruzada Leave-one-out (LOOCV). Compare los resultados del error de prueba con los obtenidos con el enfoque del conjunto de validación y explore los vínculos potenciales con la estadística de influencia.



# Variables

#### p6040 = edad
#### p6050 = ingreso
#### p6050 = sexo
#### se requiere: 1-cambiar por 0 todas las observaciones < 18 en edad. 2-cambiar valores en sexo  0=hombre y 1=mujer*
#### loging = logaritmo del ingreso