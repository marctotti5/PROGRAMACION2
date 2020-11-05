

# Para ejecutar al programa basta con pulsar en Run desde esta linea o posterior:

{
game_set_up <- function(){
        
        # Bucle para introducir las filas
        entero <- FALSE
        positivo <- FALSE
        while(entero == FALSE | positivo == FALSE){
                filas <- readline("Inserte el numero de filas del tablero por favor ")
                if(grepl("^[0-9]+$", filas)){
                        filas <- as.integer(filas)
                        entero <- TRUE
                        if(filas > 0){
                                positivo <- TRUE
                        } else {
                                positivo <- FALSE
                                print("El n?mero de filas no puede ser 0")
                        }
                        
                } else{
                        entero <- FALSE
                        print("Debe introducir un n?mero entero posiivo")
                }
        }
        
        # Bucle para introducir las columnas
        entero <- FALSE
        positivo <- FALSE
        while(entero == FALSE | positivo == FALSE){
                columnas <- readline("Inserte el numero de columnas del tablero por favor ")
                if(grepl("^[0-9]+$", columnas)){
                        columnas <- as.integer(columnas)
                        entero <- TRUE
                        if(columnas > 0){
                                positivo <- TRUE
                        } else {
                                positivo <- FALSE
                                print("El n?mero de columnas no puede ser 0")
                        }
                        
                } else{
                        entero <- FALSE
                        print("Debe introducir un n?mero entero posiivo")
                }
        }

        # Creaci?n de una matriz vac?a con filas y columnas especificadas anteriormente
        grid_gen_0 <- matrix(data = "", nrow = filas, ncol = columnas) 
        colnames(grid_gen_0) <- 1:ncol(grid_gen_0)
        
        # Seleccionamos el tipo de inserci?n: 1 para autom?tico, 2 para manual       
        entre_uno_dos <- FALSE
        while(entre_uno_dos == FALSE){
tipo_insercion <- readline("
?C?mo desea introducir las c?lulas vivas?: 
Escriba 1 para MANUAL
Escriba 2 para AUTOM?TICO ")
                
                if(grepl("^[1-2]+$", tipo_insercion)){
                        entre_uno_dos <- TRUE
                        tipo_insercion <- as.integer(tipo_insercion)
                } else {
                        entre_uno_dos <- FALSE
                        print("Debe introducir 1 o 2")
                }
        }
        
        option <- 0
        
        # Mecanismo de relleno del tablero de manera manual
        if(tipo_insercion == 1){
                
                # Bucle que pide por teclado la opci?n que desea el usuario 
                # mientras dicha opci?n sea distinta a 3 (que equivale a Terminar la creaci?n del tablero inicial)
                
                while(option != 3){
                        son_2_coordenadas <- FALSE
                        coord_son_numeros_enteros <- FALSE
                        coord_filas_estan_dentro_dimension <- FALSE
                        coord_columnas_estan_dentro_dimension <- FALSE
                        coord_son_positivas <- FALSE
                        es_numero <- FALSE
                        

                        while(es_numero == FALSE){
option <- readline("
Qu? deseas hacer? 
1: A?adir c?lula viva
2: Eliminar c?lula viva
3: Terminar")
                                if(grepl("^[1-3]+$", option)){
                                        es_numero <- TRUE
                                        option <- as.integer(option)
                                } else {
                                        es_numero <- FALSE
                                        print("El valor debe ser 1, 2 o 3")
                                }
                        }
                        
                        # Si el usuario quiere insertar o eliminar una c?lula, deber? especificar sus coordenadas
                        # El fragmento de c?digo siguiente establece las condiciones que deben cumplir las coordenadas introducidas

                        if(option == 1 | option == 2){
coordenadas <- 
readline("Inserte las 2 coordenadas separadas por una coma 
IMPORTANTE: ?LAS COORDENADAS DEBEN SER N?MEROS ENTEROS!  ")
                                coordenadas <- strsplit(coordenadas, ",")[[1]] # la funci?n strsplit devuelve una lista de un elemento, por ello seleccionamos el primer y unico vector que contiene la lista
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
                                                print("No ha introducido 2 coordenadas, y al menos una de ellas no es un n?mero entero")
coordenadas <- 
readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas num?ricas separadas por una coma ")
                                        } else if(son_2_coordenadas == FALSE & coord_son_numeros_enteros == TRUE){
                                                cat("Debe introducir exactamente 2 coordenadas num?ricas.\n 
No ha insertado el n?mero exacto de coordenadas pedidas!")
coordenadas <- readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas num?ricas separadas por una coma.  ")
                                        } else if(son_2_coordenadas == TRUE & coord_son_numeros_enteros == FALSE){
                                                print("Ha introducido 2 coordenadas pero al menos una de ellas no es un n?mero entero")
coordenadas <- 
readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas num?ricas separadas por una coma.  ")
                                        } else if(son_2_coordenadas == TRUE & coord_son_numeros_enteros == TRUE & coord_son_positivas == TRUE){
                                                if(coord_filas_estan_dentro_dimension == FALSE & coord_columnas_estan_dentro_dimension == TRUE){
                                                        print("La primera coordenada introducida es mayor que el n?mero de filas del tablero y debe ser menor o igual ")
coordenadas <- 
readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas num?ricas separadas por una coma.  ")
                                                } else if(coord_filas_estan_dentro_dimension == TRUE & coord_columnas_estan_dentro_dimension == FALSE){
                                                        print("La segunda coordenada introducida es mayor que el n?mero de columnas del tablero y debe ser menor o igual.")
coordenadas <- 
readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas num?ricas separadas por una coma.   ")
                                                } else if(coord_filas_estan_dentro_dimension == FALSE & coord_columnas_estan_dentro_dimension == TRUE){
                                                        print("La primera coordenada introducida es mayor que el n?mero de filas del tablero y debe ser menor o igual. ")
coordenadas <- 
readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas num?ricas separadas por una coma.  ")
                                                } else if(coord_filas_estan_dentro_dimension == FALSE & coord_columnas_estan_dentro_dimension == FALSE){
                                                        print("La primera coordenada es mayor que el n?mero de filas del tablero y la segunda mayor al n?mero de columnas. ")
coordenadas <- 
readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas num?ricas separadas por una coma. ")
                                                } 
                                        } else if(son_2_coordenadas == TRUE & coord_son_numeros_enteros == TRUE & coord_son_positivas == FALSE){
                                                print("Al menos una de las coordenadas insertada es cero. ")
coordenadas <- 
readline("Volvamos a intentarlo.
Inserte exactamente 2 coordenadas num?ricas separadas por una coma.  ")
                                        }
                                        
                                        coordenadas <- strsplit(coordenadas, ",")[[1]] # la funci?n strsplit devuelve una lista de un elemento, por ello seleccionamos el primer y unico vector que contiene la lista
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
                                
                                # Una vez los datos introducidos son correctos, procedemos  a las 3 opciones: A?adir c?lula, eliminar c?lula o finalizar programa (lo cual generar? el tablero de la generaci?n 1)
                                if(son_2_coordenadas == TRUE & coord_son_numeros_enteros == TRUE & coord_filas_estan_dentro_dimension == TRUE & coord_columnas_estan_dentro_dimension == TRUE){
                                        if(option == 1){
                                                if(grid_gen_0[coordenadas[1], coordenadas[2]] == ""){
                                                        grid_gen_0[coordenadas[1], coordenadas[2]] <- "X"
                                                        View(grid_gen_0)
                                                } else {
                                                        View(grid_gen_0)
                                                        print("Esta c?lula ya est? viva")
                                                } 
                                        } else if(option == 2){
                                                if(grid_gen_0[coordenadas[1], coordenadas[2]] == "X"){
                                                        grid_gen_0[coordenadas[1], coordenadas[2]] <- ""
                                                        View(grid_gen_0)
                                                } else {
                                                        View(grid_gen_0)
                                                        print("Esta c?lula ya est? muerta")
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
        
        # Inserci?n autom?tica de los datos
        
        } else if(tipo_insercion == 2){
cells <- 
readline("Ahora, seleccione el n?mero de c?lulas vivas que quiere introducir por favor ")
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
                                print(paste("Debe introducir un n?mero entero de c?lulas vivas, y este tiene que ser menor a "), nrow(grid_gen_0) * ncol(grid_gen_0), " (el n?mero de celdas del tablero)")
cells <- 
readline("Volvamos a intentarlo:
Seleccione el n?mero de c?lulas vivas que quiere introducir por favor ")
                        } else if(entero == FALSE & cells_menor_que_dimension == TRUE){
                                print(paste("Ha introducido un n?mero no entero de c?lulas vivas. El n?mero de c?lulas debe ser menor a "), nrow(grid_gen_0) * ncol(grid_gen_0), " (el n?mero de celdas del tablero)")
cells <- 
readline("Volvamos a intentarlo:
Seleccione el n?mero de c?lulas vivas que quiere introducir por favor ")
                        } else if(entero == TRUE & cells_menor_que_dimension == FALSE){
                                cat(paste("Ha introducido un n?mero entero de c?lulas vivas mayor que "), nrow(grid_gen_0) * ncol(grid_gen_0), 
                                    " (el n?mero de celdas del tablero)", "Introduzca un n?mero de c?lulas vivas menor que ", nrow(grid_gen_0) * ncol(grid_gen_0))
cells <- 
readline("Volvamos a intentarlo:
Seleccione el n?mero de c?lulas vivas que quiere introducir por favor ")
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
                                View(grid_gen_0)
                        } else {
                                View(grid_gen_0)
                                print("No ha insertado ninguna c?lula viva")
                        }
                }
        }
        return(grid_gen_0)
}


crear_tabla_n <- function(tablero_generacion_n){
        # Imprimimos la tabla de la generaci?nn 0 en View
        View(tablero_generacion_n)
        filas <- nrow(tablero_generacion_n)
        columnas <- ncol(tablero_generacion_n)
        grid_gen_1 <- matrix(data = tablero_generacion_n, nrow = filas, ncol = columnas)
        colnames(grid_gen_1) <- 1:ncol(grid_gen_1)
        
        # Usamos estos bucles anidados para generar el tablero de la siguiente generaci?n
        for (i in 1:filas) {
                for (l in 1:columnas) {
                        if(tablero_generacion_n[i, l] == ""){
                                # Cuando buscamos c?lulas inicializamos la variable neighbour a 0, porque aunque el bucle vaya a pasar por ella, 
                                # va a detectar que no esta viva y no la va a contar como su propia vecina
                                neighbour <- 0
                                # Iteramos entre el m?ximo entre 1 y la fila i-1, y el m?nimo entre el n?mero de filas y i + 1
                                # Esto nos permite que nuestro bucle no salga del tablero
                                for(i2 in max(1,(i-1)):min(filas,(i+1))){
                                        for(l2 in max(1,(l-1)):min(columnas,(l+1))){
                                                # Regla de reproducci?n
                                                if(tablero_generacion_n[i2, l2] == "X"){
                                                        neighbour <- neighbour + 1
                                                        if(neighbour == 3){
                                                                grid_gen_1[i, l] <- "X"
                                                                
                                                        }
                                                }
                                        }       
                                }
                                
                                # Buscamos c?lulas vivas
                        } else if (tablero_generacion_n[i,l] == "X"){
                                # En este caso inicializamos los vecinos en -1, porque al estar la c?lula [i,l] viva, 
                                # el bucle la contar? como vecina de s? misma
                                # Entonces si inicializamos en -1 compensamos y conseguimos el n?mero real de vecinos a su alrededor
                                neighbour <- -1
                                for(i2 in max(1,(i-1)):min(filas,(i+1))){
                                        for(l2 in max(1,(l-1)):min(columnas,(l+1))){
                                                if(tablero_generacion_n[i2,l2] == "X"){
                                                        neighbour <- neighbour + 1
                                                        #Regla de supervivencia
                                                        #Como el bucle tambien tiene en cuenta la celula viva que est? estudiando necesitamos que sea 1 mas
                                                        if(neighbour == 2 | neighbour == 3){
                                                                grid_gen_1[i, l] <- tablero_generacion_n[i ,l]
                                                        }
                                                        #Regla de soledad
                                                        #Mismo mecanismo que la anterior
                                                        if(neighbour == 0 | neighbour == 1){
                                                                grid_gen_1[i, l] <- ""
                                                        }
                                                        #Regla de superpoblaci?n
                                                        #Mismo mecanismo que la anterior
                                                        if(neighbour >= 4){
                                                                grid_gen_1[i, l] <- ""  
                                                        }
                                                } 
                                                
                                        }
                                        
                                }
                        }
                } 
        }
        edit(grid_gen_1)
}
tablero_generacion_0 <- game_set_up()
crear_tabla_n(tablero_generacion_0)
}

# Si quieres volver a jugar, ejecuta el c?digo entero otra vez!
