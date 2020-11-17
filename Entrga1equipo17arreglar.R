

# Para ejecutar al programa basta con pulsar en Run desde esta linea o posterior.

{
presentar_tablero <- function(tablero_a_mostrar){
        View(tablero_a_mostrar)
}

game_set_up <- function(){
        
        # Inserción del número de filas del tablero
        filas <- readline("Inserte el numero de filas del tablero por favor ")
        entero <- 0
        positivo <- 0
        if(grepl("^[0-9]+$", filas)){
                filas <- as.integer(filas)
                entero <- TRUE
                if(filas > 0){
                        positivo <- TRUE
                } else {
                        positivo <- FALSE
                }
                
        } else{
                entero <- FALSE
        }
        # Bucle en caso de que el valor introducido no sea entero positivo
        while(entero == FALSE | positivo == FALSE){
                print("El valor a introducir debe ser entero positivo")
                filas <- readline("Vuelva a insertar el numero de filas del tablero por favor ")
                if(grepl("^[0-9]+$", filas)){
                        filas <- as.integer(filas)
                        entero <- TRUE
                        if(filas > 0){
                                positivo <- TRUE
                        } else {
                                positivo <- FALSE
                        }
                        
                } else{
                        entero <- FALSE
                }
        }
        
        # Inserción del número de columnas del tablero
        columnas <- readline("Inserte el numero de columnas del tablero por favor ")
        if(grepl("^[0-9]+$", columnas)){
                columnas <- as.integer(columnas)
                entero <- TRUE
                if(columnas > 0){
                        positivo <- TRUE
                } else {
                        positivo <- FALSE
                }
                
        } else{
                entero <- FALSE
        }
        
        # Bucle en caso de que el valor introducido no sea entero positivo
        while(entero == FALSE | positivo == FALSE){
                print("El valor a introducir debe ser entero positivo")
                columnas <- readline("Vuelva a insertar el numero de columnas del tablero por favor ")
                if(grepl("^[0-9]+$", columnas)){
                        columnas <- as.integer(columnas)
                        entero <- TRUE
                        if(columnas > 0){
                                positivo <- TRUE
                        } else {
                                positivo <- FALSE
                        }
                        
                } else{
                        entero <- FALSE
                }
        }
        
        # Creación de una matriz vacía con filas y columnas especificadas anteriormente
        grid_gen_0 <- matrix(data = "", nrow = filas, ncol = columnas) 
        colnames(grid_gen_0) <- 1:ncol(grid_gen_0)
        
        insercion <- function(cero_o_x){
                # Seleccionamos el tipo de inserción: 1 para automático, 2 para manual       
                
                if(cero_o_x == "X"){
                        tipo_insercion <- readline("
¿Cómo desea introducir las células vivas?: 
Escriba 1 para MANUAL
Escriba 2 para AUTOMÁTICO 
Escriba 3 para GRÁFICAMENTE")
                        
                        if(grepl("^[1-3]+$", tipo_insercion)){
                                entre_uno_tres <- TRUE
                                tipo_insercion <- as.integer(tipo_insercion)
                        } else{
                                entre_uno_tres <- FALSE
                        }
                        
                        # Bucle en caso de que el valor introducido no sea 1 o 2
                        while(entre_uno_tres == FALSE){
                                
                                print("El valor debe ser 1 o 2")
                                tipo_insercion <- readline("
Vuelva a intentarlo. 
¿Cómo desea introducir las células vivas?: 
Escriba 1 para MANUAL
Escriba 2 para AUTOMÁTICO 
Escriba 3 para GRÁFICAMENTE")
                                
                                if(grepl("^[1-3]+$", tipo_insercion)){
                                        entre_uno_tres <- TRUE
                                        tipo_insercion <- as.integer(tipo_insercion)
                                } else {
                                        entre_uno_tres <- FALSE
                                }
                        }
                } else if(cero_o_x == 0){
                        tipo_insercion <- readline("
¿Cómo desea introducir las casillas inhabitables?: 
Escriba 1 para MANUAL
Escriba 2 para AUTOMÁTICO 
Escriba 3 para GRÁFICAMENTE")
                        
                        if(grepl("^[1-3]+$", tipo_insercion)){
                                entre_uno_tres <- TRUE
                                tipo_insercion <- as.integer(tipo_insercion)
                        } else{
                                entre_uno_tres <- FALSE
                        }
                        
                        # Bucle en caso de que el valor introducido no sea 1 o 2
                        while(entre_uno_tres == FALSE){
                                
                                print("El valor debe ser 1 o 2")
                                tipo_insercion <- readline("
Vuelva a intentarlo. 
¿Cómo desea introducir las casillas inhabitables?: 
Escriba 1 para MANUAL
Escriba 2 para AUTOMÁTICO 
Escriba 3 para GRÁFICAMENTE")
                                
                                if(grepl("^[1-3]+$", tipo_insercion)){
                                        entre_uno_tres <- TRUE
                                        tipo_insercion <- as.integer(tipo_insercion)
                                } else {
                                        entre_uno_tres <- FALSE
                                }
                        }
                }
                
                option <- 0
                es_numero <- FALSE
                
                # Mecanismo de relleno del tablero de manera manual
                if(tipo_insercion == 1){
                        
                        # Bucle que pide por teclado la opción que desea el usuario 
                        # mientras dicha opción sea distinta a 3 (que equivale a Terminar la creación del tablero inicial)
                        
                        while(option != 3){
                                son_2_coordenadas <- FALSE
                                coord_son_numeros_enteros <- FALSE
                                coord_filas_estan_dentro_dimension <- FALSE
                                coord_columnas_estan_dentro_dimension <- FALSE
                                coord_son_positivas <- FALSE
                                
                                if(cero_o_x == "X"){
                                        option <- readline("
Qué deseas hacer? 
1: Añadir célula viva
2: Eliminar célula viva
3: Terminar")
                                        
                                        if(grepl("^[1-3]+$", option)){
                                                es_numero <- TRUE
                                                option <- as.integer(option)
                                        } else {
                                                es_numero <- FALSE
                                        }
                                        
                                        # Bucle que se repite mientras la opción introducida no sea correcta
                                        while(es_numero == FALSE){
                                                print("El valor debe ser 1, 2 o 3")
                                                option <- readline("
Volvamos a intentarlo.
Qué deseas hacer? 
1: Añadir célula viva
2: Eliminar célula viva
3: Terminar")
                                                if(grepl("^[1-3]+$", option)){
                                                        es_numero <- TRUE
                                                        option <- as.integer(option)
                                                } else {
                                                        es_numero <- FALSE
                                                }
                                        }
                                        
                                } else if (cero_o_x == 0){
                                        option <- readline("
Qué deseas hacer? 
1: Añadir casilla inhabitable
2: Eliminar casilla inhabitable
3: Terminar")
                                        
                                        if(grepl("^[1-3]+$", option)){
                                                es_numero <- TRUE
                                                option <- as.integer(option)
                                        } else {
                                                es_numero <- FALSE
                                        }
                                        
                                        # Bucle que se repite mientras la opción introducida no sea correcta
                                        while(es_numero == FALSE){
                                                print("El valor debe ser 1, 2 o 3")
                                                option <- readline("
Volvamos a intentarlo.
Qué deseas hacer? 
1: Añadir casilla inhabitable
2: Eliminar casilla inhabitable
3: Terminar")
                                                if(grepl("^[1-3]+$", option)){
                                                        es_numero <- TRUE
                                                        option <- as.integer(option)
                                                } else {
                                                        es_numero <- FALSE
                                                }
                                        }
                                        
                                }
                                
                                # Si el usuario quiere insertar o eliminar una célula, deberá especificar sus coordenadas
                                # El fragmento de código siguiente establece las condiciones que deben cumplir las coordenadas introducidas
                                
                                if(option == 1 | option == 2){
                                        coordenadas <- 
                                                readline("Inserte las 2 coordenadas separadas por una coma 
IMPORTANTE: ¡LAS COORDENADAS DEBEN SER NÚMEROS ENTEROS!  ")
                                        coordenadas <- strsplit(coordenadas, ",")[[1]] # la función strsplit devuelve una lista de un elemento, por ello seleccionamos el primer y unico vector que contiene la lista
                                        if(length(coordenadas) == 2){
                                                son_2_coordenadas <- TRUE
                                                if(all(grepl("^[0-9]+$", coordenadas)) == TRUE){
                                                        coord_son_numeros_enteros <- TRUE
                                                        coordenadas <- as.integer(coordenadas)
                                                        if((coordenadas[1] > nrow(grid_gen_0)) | (coordenadas[2] > ncol(grid_gen_0) | coordenadas[1] == 0 | coordenadas[2] == 0)){
                                                                if((coordenadas[1] > nrow(grid_gen_0)) & (coordenadas[2] <= ncol(grid_gen_0))){
                                                                        coord_filas_estan_dentro_dimension <- FALSE
                                                                        coord_columnas_estan_dentro_dimension <- TRUE
                                                                        if(coordenadas[1] == 0 | coordenadas[2] == 0){
                                                                                coord_son_positivas <- FALSE
                                                                        } else{
                                                                                coord_son_positivas <- TRUE
                                                                        }
                                                                } else if((coordenadas[1] <= nrow(grid_gen_0)) & (coordenadas[2] > ncol(grid_gen_0))){
                                                                        coord_filas_estan_dentro_dimension <- TRUE
                                                                        coord_columnas_estan_dentro_dimension <- FALSE
                                                                        if(coordenadas[1] == 0 | coordenadas[2] == 0){
                                                                                coord_son_positivas <- FALSE
                                                                        } else {
                                                                                coord_son_positivas <- TRUE
                                                                        }
                                                                } else if((coordenadas[1] > nrow(grid_gen_0)) & (coordenadas[2] > ncol(grid_gen_0))) {
                                                                        coord_filas_estan_dentro_dimension <- FALSE
                                                                        coord_columnas_estan_dentro_dimension <- FALSE
                                                                        if(coordenadas[1] == 0 | coordenadas[2] == 0){
                                                                                coord_son_positivas <- FALSE
                                                                        } else {
                                                                                coord_son_positivas <- TRUE
                                                                        }
                                                                } 
                                                        } else if((coordenadas[1] <= nrow(grid_gen_0)) & (coordenadas[2] <= ncol(grid_gen_0) & (coordenadas[1] != 0) & (coordenadas[2] != 0))){
                                                                coord_filas_estan_dentro_dimension <- TRUE
                                                                coord_columnas_estan_dentro_dimension <- TRUE
                                                                coord_son_positivas <- TRUE
                                                        }
                                                } else{
                                                        coord_son_numeros_enteros <- FALSE
                                                }
                                        } else if(length(coordenadas) != 2){
                                                son_2_coordenadas <- FALSE
                                                if(all(grepl("^[0-9]+$", coordenadas)) == TRUE){
                                                        coord_son_numeros_enteros <- TRUE
                                                        coordenadas <- as.integer(coordenadas)
                                                } else{
                                                        coord_son_numeros_enteros <- FALSE
                                                }
                                        }
                                        
                                        # Bucle en caso de que las coordenadas no sean correctas
                                        while(son_2_coordenadas == FALSE | coord_son_numeros_enteros == FALSE | coord_filas_estan_dentro_dimension == FALSE | coord_columnas_estan_dentro_dimension == FALSE | coord_son_positivas == FALSE ){
                                                if(son_2_coordenadas == FALSE & coord_son_numeros_enteros == FALSE){
                                                        print("No ha introducido 2 coordenadas, y al menos una de ellas no es un numéro entero")
                                                        coordenadas <- 
                                                                readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas numéricas separadas por una coma ")
                                                } else if(son_2_coordenadas == FALSE & coord_son_numeros_enteros == TRUE){
                                                        cat("Debe introducir exactamente 2 coordenadas numéricas.\n 
No ha insertado el número exacto de coordenadas pedidas!")
                                                        coordenadas <- readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas numéricas separadas por una coma.  ")
                                                } else if(son_2_coordenadas == TRUE & coord_son_numeros_enteros == FALSE){
                                                        print("Ha introducido 2 coordenadas pero al menos una de ellas no es un número entero")
                                                        coordenadas <- 
                                                                readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas numéricas separadas por una coma.  ")
                                                } else if(son_2_coordenadas == TRUE & coord_son_numeros_enteros == TRUE & coord_son_positivas == TRUE){
                                                        if(coord_filas_estan_dentro_dimension == FALSE & coord_columnas_estan_dentro_dimension == TRUE){
                                                                print("La primera coordenada introducida es mayor que el número de filas del tablero y debe ser menor o igual ")
                                                                coordenadas <- 
                                                                        readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas numéricas separadas por una coma.  ")
                                                        } else if(coord_filas_estan_dentro_dimension == TRUE & coord_columnas_estan_dentro_dimension == FALSE){
                                                                print("La segunda coordenada introducida es mayor que el número de columnas del tablero y debe ser menor o igual.")
                                                                coordenadas <- 
                                                                        readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas numéricas separadas por una coma.   ")
                                                        } else if(coord_filas_estan_dentro_dimension == FALSE & coord_columnas_estan_dentro_dimension == TRUE){
                                                                print("La primera coordenada introducida es mayor que el número de filas del tablero y debe ser menor o igual. ")
                                                                coordenadas <- 
                                                                        readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas numéricas separadas por una coma.  ")
                                                        } else if(coord_filas_estan_dentro_dimension == FALSE & coord_columnas_estan_dentro_dimension == FALSE){
                                                                print("La primera coordenada es mayor que el número de filas del tablero y la segunda mayor al número de columnas. ")
                                                                coordenadas <- 
                                                                        readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas numéricas separadas por una coma. ")
                                                        } 
                                                } else if(son_2_coordenadas == TRUE & coord_son_numeros_enteros == TRUE & coord_son_positivas == FALSE){
                                                        print("Al menos una de las coordenadas insertada es cero. ")
                                                        coordenadas <- 
                                                                readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas numéricas separadas por una coma.  ")
                                                }
                                                
                                                coordenadas <- strsplit(coordenadas, ",")[[1]] # la función strsplit devuelve una lista de un elemento, por ello seleccionamos el primer y unico vector que contiene la lista
                                                if(length(coordenadas) == 2){
                                                        son_2_coordenadas <- TRUE
                                                        if(all(grepl("^[0-9]+$", coordenadas)) == TRUE){
                                                                coord_son_numeros_enteros <- TRUE
                                                                coordenadas <- as.integer(coordenadas)
                                                                if((coordenadas[1] > nrow(grid_gen_0)) | (coordenadas[2] > ncol(grid_gen_0) | coordenadas[1] == 0 | coordenadas[2] == 0)){
                                                                        if((coordenadas[1] > nrow(grid_gen_0)) & (coordenadas[2] <= ncol(grid_gen_0))){
                                                                                coord_filas_estan_dentro_dimension <- FALSE
                                                                                coord_columnas_estan_dentro_dimension <- TRUE
                                                                                if(coordenadas[1] == 0 | coordenadas[2] == 0){
                                                                                        coord_son_positivas <- FALSE
                                                                                } else{
                                                                                        coord_son_positivas <- TRUE
                                                                                }
                                                                        } else if((coordenadas[1] <= nrow(grid_gen_0)) & (coordenadas[2] > ncol(grid_gen_0))){
                                                                                coord_filas_estan_dentro_dimension <- TRUE
                                                                                coord_columnas_estan_dentro_dimension <- FALSE
                                                                                if(coordenadas[1] == 0 | coordenadas[2] == 0){
                                                                                        coord_son_positivas <- FALSE
                                                                                } else{
                                                                                        coord_son_positivas <- TRUE
                                                                                }
                                                                        } else if((coordenadas[1] > nrow(grid_gen_0)) & (coordenadas[2] > ncol(grid_gen_0))) {
                                                                                coord_filas_estan_dentro_dimension <- FALSE
                                                                                coord_columnas_estan_dentro_dimension <- FALSE
                                                                                if(coordenadas[1] == 0 | coordenadas[2] == 0){
                                                                                        coord_son_positivas <- FALSE
                                                                                } else{
                                                                                        coord_son_positivas <- TRUE
                                                                                }
                                                                        } 
                                                                } else if((coordenadas[1] <= nrow(grid_gen_0)) & (coordenadas[2] <= ncol(grid_gen_0) & (coordenadas[1] != 0) & (coordenadas[2] != 0))){
                                                                        coord_filas_estan_dentro_dimension <- TRUE
                                                                        coord_columnas_estan_dentro_dimension <- TRUE
                                                                        coord_son_positivas <- TRUE
                                                                        
                                                                }
                                                        } else{
                                                                coord_son_numeros_enteros <- FALSE
                                                        }
                                                } else if(length(coordenadas) != 2){
                                                        son_2_coordenadas <- FALSE
                                                        if(all(grepl("^[0-9]+$", coordenadas)) == TRUE){
                                                                coord_son_numeros_enteros <- TRUE
                                                                coordenadas <- as.integer(coordenadas)
                                                        } else{
                                                                coord_son_numeros_enteros <- FALSE
                                                        }
                                                }
                                        }
                                        
                                        # Una vez los datos introducidos son correctos, procedemos  a las 3 opciones: Añadir célula, eliminar célula o finalizar programa (lo cual generará el tablero de la generación 1)
                                        if(son_2_coordenadas == TRUE & coord_son_numeros_enteros == TRUE & coord_filas_estan_dentro_dimension == TRUE & coord_columnas_estan_dentro_dimension == TRUE){
                                                if(cero_o_x == "X"){
                                                        if(option == 1){
                                                                print(grid_gen_0[coordenadas[1], coordenadas[2]])
                                                                if(grid_gen_0[coordenadas[1], coordenadas[2]] == "inhab"){
                                                                        presentar_tablero(grid_gen_0)
                                                                        print("Esta casilla es inhabitable")
                                                                        
                                                                } else if(grid_gen_0[coordenadas[1], coordenadas[2]] == "X"){
                                                                        presentar_tablero(grid_gen_0)
                                                                        print("Esta célula ya está viva")
                                                                } else if(grid_gen_0[coordenadas[1], coordenadas[2]] == ""){
                                                                        grid_gen_0[coordenadas[1], coordenadas[2]] <- "X"
                                                                        presentar_tablero(grid_gen_0)
                                                                }
                                                        } else if(option == 2){
                                                                if(grid_gen_0[coordenadas[1], coordenadas[2]] == "inhab"){
                                                                        presentar_tablero(grid_gen_0)
                                                                        print("Esta casilla es inhabitable")
                                                                        
                                                                } else if(grid_gen_0[coordenadas[1], coordenadas[2]] == ""){
                                                                        presentar_tablero(grid_gen_0)
                                                                        print("Esta célula ya está muerta")
                                                                } else if(grid_gen_0[coordenadas[1], coordenadas[2]] == "X"){
                                                                        grid_gen_0[coordenadas[1], coordenadas[2]] <- ""
                                                                        presentar_tablero(grid_gen_0)
                                                                }
                                                        }
                                                } else if(cero_o_x == 0){
                                                        if(option == 1){
                                                                if(grid_gen_0[coordenadas[1], coordenadas[2]] == "" | grid_gen_0[coordenadas[1], coordenadas[2]] == "X"){
                                                                        grid_gen_0[coordenadas[1], coordenadas[2]] <- "inhab"
                                                                        print(grid_gen_0[coordenadas[1], coordenadas[2]])
                                                                        presentar_tablero(grid_gen_0)
                                                                } else {
                                                                        presentar_tablero(grid_gen_0)
                                                                        print("Esta celda ya es inhabitable")
                                                                } 
                                                        } else if(option == 2){
                                                                if(grid_gen_0[coordenadas[1], coordenadas[2]] == "inhab"){
                                                                        grid_gen_0[coordenadas[1], coordenadas[2]] <- ""
                                                                        presentar_tablero(grid_gen_0)
                                                                } else {
                                                                        presentar_tablero(grid_gen_0)
                                                                        print("Esta celda ya es habitable")
                                                                }
                                                        }
                                                }
                                                
                                        }
                                        
                                } else if(option == 3){
                                        print("Programa finalizado")
                                }
                                
                        }
                        
                        entero <- FALSE
                        fila_aleatoria <- 0
                        columna_aleatoria <- 0
                        cells_menor_que_dimension <- FALSE
                        dimension <- nrow(grid_gen_0) * ncol(grid_gen_0)
                        
                        
                        # Inserción automática de los datos
                        
                } else if(tipo_insercion == 2){
                        if(cero_o_x == "X"){
                                cells <- 
                                        readline("Ahora, seleccione el número de células vivas que quiere introducir por favor ")
                                if(grepl("^[0-9]+$", cells)){
                                        entero <- TRUE
                                        cells <- as.integer(cells)
                                        if(cells > (nrow(grid_gen_0) * ncol(grid_gen_0))){
                                                cells_menor_que_dimension <- FALSE
                                        } else {
                                                cells_menor_que_dimension <- TRUE
                                        }
                                } else {
                                        entero <- FALSE
                                }
                                while(entero == FALSE | cells_menor_que_dimension == FALSE ){
                                        if(entero == FALSE & cells_menor_que_dimension == FALSE){
                                                print(paste("Debe introducir un número entero de células vivas, y este tiene que ser menor a "), nrow(grid_gen_0) * ncol(grid_gen_0), " (el número de celdas del tablón)")
                                                cells <- 
                                                        readline("Volvamos a intentarlo:
Seleccione el número de células vivas que quiere introducir por favor ")
                                        } else if(entero == FALSE & cells_menor_que_dimension == TRUE){
                                                print(paste("Ha introducido un número no entero de células vivas. El número de células debe ser menor a "), nrow(grid_gen_0) * ncol(grid_gen_0), " (el número de celdas del tablón)")
                                                cells <- 
                                                        readline("Volvamos a intentarlo:
Seleccione el número de células vivas que quiere introducir por favor ")
                                        } else if(entero == TRUE & cells_menor_que_dimension == FALSE){
                                                cat(paste("Ha introducido un número entero de células vivas mayor que "), nrow(grid_gen_0) * ncol(grid_gen_0), 
                                                    " (el número de celdas del tablón)", "\nIntroduzca un número de células vivas menor que ", nrow(grid_gen_0) * ncol(grid_gen_0))
                                                cells <- 
                                                        readline("Volvamos a intentarlo:
Seleccione el número de células vivas que quiere introducir por favor ")
                                        }
                                        if(grepl("^[0-9]+$", cells)){
                                                entero <- TRUE
                                                cells <- as.integer(cells)
                                                if(cells > (nrow(grid_gen_0) * ncol(grid_gen_0))){
                                                        cells_menor_que_dimension <- FALSE
                                                } else {
                                                        cells_menor_que_dimension <- TRUE
                                                }
                                        } else {
                                                entero <- FALSE
                                        }
                                        
                                }
                                if(entero == TRUE & cells_menor_que_dimension == TRUE){
                                        if(cells != 0){
                                                fila_aleatoria <- sample(1:nrow(grid_gen_0), size = cells, replace = TRUE)
                                                columna_aleatoria <- sample(1:ncol(grid_gen_0), size = cells, replace = TRUE)
                                                for(i in 1:cells){
                                                        grid_gen_0[fila_aleatoria[i], columna_aleatoria[i]] <- "X"
                                                }
                                                presentar_tablero(grid_gen_0)
                                        } else {
                                                presentar_tablero(grid_gen_0)
                                                print("No ha insertado ninguna célula viva")
                                        }
                                }
                        } else if(cero_o_x == 0){
                                cells <- 
                                        readline("Ahora, seleccione el número de casillas inhabitables que quiere introducir por favor ")
                                if(grepl("^[0-9]+$", cells)){
                                        entero <- TRUE
                                        cells <- as.integer(cells)
                                        if(cells > (nrow(grid_gen_0) * ncol(grid_gen_0))){
                                                cells_menor_que_dimension <- FALSE
                                        } else {
                                                cells_menor_que_dimension <- TRUE
                                        }
                                } else {
                                        entero <- FALSE
                                }
                                while(entero == FALSE | cells_menor_que_dimension == FALSE ){
                                        if(entero == FALSE & cells_menor_que_dimension == FALSE){
                                                print(paste("Debe introducir un número entero de casillas inhabitables, y este tiene que ser menor a "), nrow(grid_gen_0) * ncol(grid_gen_0), " (el número de celdas del tablón)")
                                                cells <- 
                                                        readline("Volvamos a intentarlo:
Seleccione el número de casillas inhabitables que quiere introducir por favor ")
                                        } else if(entero == FALSE & cells_menor_que_dimension == TRUE){
                                                print(paste("Ha introducido un número no entero de casillas inhabitables. El número de casillas debe ser menor a "), nrow(grid_gen_0) * ncol(grid_gen_0), " (el número de celdas del tablón)")
                                                cells <- 
                                                        readline("Volvamos a intentarlo:
Seleccione el número de casillas inhabitables que quiere introducir por favor ")
                                        } else if(entero == TRUE & cells_menor_que_dimension == FALSE){
                                                cat(paste("Ha introducido un número entero de casillas inhabitables mayor que "), nrow(grid_gen_0) * ncol(grid_gen_0), 
                                                    " (el número de celdas del tablón)", "\nIntroduzca un número de casillas inhabitables menor que ", nrow(grid_gen_0) * ncol(grid_gen_0))
                                                cells <- 
                                                        readline("Volvamos a intentarlo:
Seleccione el número de casillas inhabitables que quiere introducir por favor ")
                                        }
                                        if(grepl("^[0-9]+$", cells)){
                                                entero <- TRUE
                                                cells <- as.integer(cells)
                                                if(cells > (nrow(grid_gen_0) * ncol(grid_gen_0))){
                                                        cells_menor_que_dimension <- FALSE
                                                } else {
                                                        cells_menor_que_dimension <- TRUE
                                                }
                                        } else {
                                                entero <- FALSE
                                        }
                                        
                                }
                                if(entero == TRUE & cells_menor_que_dimension == TRUE){
                                        if(cells != 0){
                                                fila_aleatoria <- sample(1:nrow(grid_gen_0), size = cells, replace = TRUE)
                                                columna_aleatoria <- sample(1:ncol(grid_gen_0), size = cells, replace = TRUE)
                                                for(i in 1:cells){
                                                        grid_gen_0[fila_aleatoria[i], columna_aleatoria[i]] <- "X"
                                                }
                                                presentar_tablero(grid_gen_0)
                                        } else {
                                                presentar_tablero(grid_gen_0)
                                                print("No ha insertado ninguna casilla inhabitable")
                                        }
                                }
                        }
                } else if(tipo_insercion == 3){
                        print("Inserta X en las células que quieres que estén vivas y 0 en las casillas que quieres que sean inhabitables")
                        grid_gen_0 <- edit(grid_gen_0)
                        while(!all(grid_gen_0 == "X" | grid_gen_0 == "" | grid_gen_0 == 0)){
                                print("El tablero solo puede contener celdas vacías, celdas ocupadas y celdas inhabitables")
                                grid_gen_0 <- edit(grid_gen_0)
                        }
                }
        }
        grid_gen_0 <- insercion(cero_o_x = 0)
        grid_gen_0 <- insercion(cero_o_x = "X")
        return(grid_gen_0)
        
}


tablero_generacion_inicial <- game_set_up()
contador_generacion <- 1

#Bucle para que el usuario elija la variante del juego
entre_uno_dos <- FALSE
while(entre_uno_dos == FALSE){
        vecindario <- readline("
Escoja la variante del juego que quiere usar 
1- VECINDARIO NORMAL
2- VECINDARIO EXTENDIDO")
        
        if(grepl("^[1-2]+$", vecindario)){
                entre_uno_dos <- TRUE
                vecindario <- as.integer(vecindario)
        } else {
                entre_uno_dos <- FALSE
                print("El número a introducir debe ser 1 o 2, Probemos otra vez")
                       
        }
}

crear_tabla_n <- function(vecindario){
        browser()
        
        # Imprimimos la tabla de la generación 0 en presentar_tablero
        presentar_tablero(tablero_generacion_inicial)
        filas <- nrow(tablero_generacion_inicial)
        columnas <- ncol(tablero_generacion_inicial)
        grid_gen_n <- matrix(data = tablero_generacion_inicial, nrow = filas, ncol = columnas)
        colnames(grid_gen_n) <- 1:ncol(grid_gen_n)
        
        # Usamos estos bucles anidados para generar el tablero de la siguiente generación
        for (i in 1:filas) {
                for (l in 1:columnas) {
                        if(tablero_generacion_inicial[i, l] == ""){
                                # Cuando buscamos células inicializamos la variable neighbour a 0, porque aunque el bucle vaya a pasar por ella, 
                                # va a detectar que no esta viva y no la va a contar como su propia vecina
                                neighbour <- 0
                                # Iteramos entre el máximo entre 1 y la fila i-1, y el mínimo entre el número de filas y i + 1
                                # Esto nos permite que nuestro bucle no salga del tablero
                                for(i2 in max(1,(i-vecindario)):min(filas,(i+vecindario))){
                                        for(l2 in max(1,(l-2)):min(columnas,(l+2))){
                                                # Regla de reproducción
                                                if(tablero_generacion_inicial[i2, l2] == "X"){
                                                        neighbour <- neighbour + 1
                                                        if(neighbour == 3){
                                                                grid_gen_n[i, l] <- "X"
                                                                
                                                        }
                                                }
                                        }       
                                }
                                
                                # Buscamos células vivas
                        } else if (tablero_generacion_inicial[i,l] == "X"){
                                # En este caso inicializamos los vecinos en -1, porque al estar la célula [i,l] viva, 
                                # el bucle la contará como vecina de sí misma
                                # Entonces si inicializamos en -1 compensamos y conseguimos el número real de vecinos a su alrededor
                                neighbour <- -1
                                for(i2 in max(1,(i-vecindario)):min(filas,(i+vecindario))){
                                        for(l2 in max(1,(l-1)):min(columnas,(l+1))){
                                                if(tablero_generacion_inicial[i2,l2] == "X"){
                                                        neighbour <- neighbour + 1
                                                        #Regla de supervivencia
                                                        #Como el bucle tambien tiene en cuenta la celula viva que está estudiando necesitamos que sea 1 mas
                                                        if(neighbour == 2 | neighbour == 3){
                                                                grid_gen_n[i, l] <- tablero_generacion_inicial[i ,l]
                                                        }
                                                        #Regla de soledad
                                                        #Mismo mecanismo que la anterior
                                                        if(neighbour == 0 | neighbour == 1){
                                                                grid_gen_n[i, l] <- ""
                                                        }
                                                        #Regla de superpoblación
                                                        #Mismo mecanismo que la anterior
                                                        if(neighbour >= 4){
                                                                grid_gen_n[i, l] <- ""  
                                                        }
                                                } 
                                                
                                        }
                                        
                                }
                        }
                } 
        }
        presentar_tablero(grid_gen_n)
        par(mfrow = c(1,1), mar=c(0, 0, 0, 0), oma = c(1, 1, 1, 1))
        plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
        text(0, 0, labels = paste("Generación", contador_generacion) , cex = 3)
        return(grid_gen_n)
}

crear_tabla_n(vecindario)
tablero_generacion_1 <- crear_tabla_n(vecindario)
quieres_mas_generaciones <- ""
while(quieres_mas_generaciones == ""){
        quieres_mas_generaciones <- readline("Pulse enter si quiere avanzar a la siguiente generación. Pulse otro caracter en caso contrario ")
        if(quieres_mas_generaciones == ""){
                contador_generacion <- contador_generacion + 1
                tablero_generacion_inicial <- crear_tabla_n(vecindario)
                crear_tabla_n(vecindario)
        } else {
                if(contador_generacion == 1){
                        par(mfrow = c(1,1), mar=c(0, 0, 0, 0), oma = c(1, 1, 1, 1))
                        plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
                        text(0, 0, labels = paste("Generación", contador_generacion) , cex = 3)
                        tablero_generacion_1
                } else {
                        par(mfrow = c(2,1), mar=c(0, 0, 0, 0), oma = c(11, 1, 11, 1))
                        plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
                        text(0, 0, labels = paste("Generación", contador_generacion) , cex = 3)
                        plot(0, 0, type = "n", xlim = c(-0.25, 0.25), ylim = c(-0.25, 0.25), xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
                        text(0, 0, labels = "Juego Finalizado", cex = 1.5, col = "red")
                        tablero_generacion_inicial
                }
        }

}

}

#arreglar Ahora, seleccione el número de células vivas que quiere introducir por favor 20
#Error in if (cells > (nrow(grid_gen_0) * ncol(grid_gen_0))) { : 
#                argumento tiene longitud cero

