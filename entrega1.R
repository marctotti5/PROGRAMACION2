
{
game_set_up <- function(){
        filas <- readline("Inserte el numero de filas del tablero por favor ")
        if(grepl("^[0-9]+$", filas) & filas != "0"){
                entero <- TRUE
                filas <- as.integer(filas)
        } else{
                entero <- FALSE
        }
        
        while(entero == FALSE){
                print("El valor a introducir debe ser entero positivo")
                filas <- readline("Vuelva a insertar el número de filas del tablero por favor ")
                if(grepl("^[0-9]+$", filas) & filas != "0"){
                        entero <- TRUE
                        filas <- as.integer(filas)
                } else{
                        # falta especificar el tipo de dato incorrecto que ha metido
                        entero <- FALSE
                }   
        }
        
        columnas <- readline("Inserte el numero de columnas del tablero por favor ")
        if(grepl("^[0-9]+$", columnas) & columnas != "0"){
                entero <- TRUE
                columnas <- as.integer(columnas)
        } else{
                entero <- FALSE
        }
        while(entero == FALSE){
                print("El valor a introducir debe ser entero positivo")
                columnas <- readline("Vuelva a insertar el número de columnas del tablero por favor ")
                if(grepl("^[0-9]+$", columnas) & columnas != "0"){
                        entero <- TRUE
                        columnas <- as.integer(columnas)
                } else{
                        # falta especificar el tipo de dato incorrecto que ha metido
                        entero <- FALSE
                }   
        }
        
        grid_gen_0 <- matrix(data = "", nrow = filas, ncol = columnas) 
        colnames(grid_gen_0) <- 1:ncol(grid_gen_0)
        
tipo_insercion <- readline("
¿Como desea introducir las células vivas?: 
Escriba 1 para MANUAL
Escriba 2 para AUTOMÁTICO 
Por favor inserte el número (no ponga manual o automático) ")
        
        if(grepl("^[1-2]+$", tipo_insercion)){
                entre_uno_dos <- TRUE
                tipo_insercion <- as.integer(tipo_insercion)
        } else{
                entre_uno_dos <- FALSE
        }
        
        while(entre_uno_dos == FALSE){
                
                print("El valor debe ser 1 o 2")
tipo_insercion <- readline("
Vuelva a intentarlo. 
¿Como desea introducir las células vivas?: 
Escriba 1 para MANUAL
Escriba 2 para AUTOMÁTICO 
Por favor inserte el número (no ponga manual o automático) ")
                
                if(grepl("^[1-2]+$", tipo_insercion)){
                        entre_uno_dos <- TRUE
                        tipo_insercion <- as.integer(tipo_insercion)
                } else {
                        entre_uno_dos <- FALSE
                }
        }
        
        option <- 0
        es_numero <- FALSE
        if(tipo_insercion == 1){
                while(option != 3){
                        son_2_coordenadas <- FALSE
                        coord_son_numeros_enteros <- FALSE
                        coord_filas_estan_dentro_dimension <- FALSE
                        coord_columnas_estan_dentro_dimension <- FALSE
                        
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
                        
                        
                        if(option == 1 | option == 2){
coordenadas <- 
readline("Inserte las 2 coordenadas separadas por una coma (la primera coordenada se corresponde con el número de fila y la segunda el de columna)
IMPORTANTE: ¡LAS COORDENADAS DEBEN SER NÚMEROS ENTEROS!  ")
                                coordenadas <- strsplit(coordenadas, ",")[[1]] # la función strsplit devuelve una lista de un elemento, por ello seleccionamos el primer y unico vector que contiene la lista
                                if(length(coordenadas) == 2){
                                        if(all(grepl("^[0-9]+$", coordenadas)) == TRUE){
                                                coordenadas <- as.integer(coordenadas)
                                                if((coordenadas[1] > nrow(grid_gen_0)) | (coordenadas[2] > ncol(grid_gen_0))){
                                                        if((coordenadas[1] > nrow(grid_gen_0)) & (coordenadas[2] <= ncol(grid_gen_0))){
                                                                son_2_coordenadas <- TRUE
                                                                coord_son_numeros_enteros <- TRUE
                                                                coord_filas_estan_dentro_dimension <- FALSE
                                                                coord_columnas_estan_dentro_dimension <- TRUE
                                                        } else if((coordenadas[1] <= nrow(grid_gen_0)) & (coordenadas[2] > ncol(grid_gen_0))){
                                                                son_2_coordenadas <- TRUE
                                                                coord_son_numeros_enteros <- TRUE
                                                                coord_filas_estan_dentro_dimension <- TRUE
                                                                coord_columnas_estan_dentro_dimension <- FALSE
                                                        } else if((coordenadas[1] > nrow(grid_gen_0)) & (coordenadas[2] > ncol(grid_gen_0))) {
                                                                son_2_coordenadas <- TRUE
                                                                coord_son_numeros_enteros <- TRUE
                                                                coord_filas_estan_dentro_dimension <- FALSE
                                                                coord_columnas_estan_dentro_dimension <- FALSE
                                                        } 
                                                } else if((coordenadas[1] <= nrow(grid_gen_0)) & (coordenadas[2] <= ncol(grid_gen_0))){
                                                        son_2_coordenadas <- TRUE
                                                        coord_son_numeros_enteros <- TRUE
                                                        coord_filas_estan_dentro_dimension <- TRUE
                                                        coord_columnas_estan_dentro_dimension <- TRUE
                                                }
                                        } else{
                                                son_2_coordenadas <- TRUE
                                                coord_son_numeros_enteros <- FALSE
                                        }
                                } else if(length(coordenadas) != 2){
                                        if(all(grepl("^[0-9]+$", coordenadas)) == TRUE){
                                                coordenadas <- as.integer(coordenadas)
                                                son_2_coordenadas <- FALSE
                                                coord_son_numeros_enteros <- TRUE
                                        } else{
                                                son_2_coordenadas <- FALSE
                                                coord_son_numeros_enteros <- FALSE
                                        }
                                }
                                
                                while(son_2_coordenadas == FALSE | coord_son_numeros_enteros == FALSE | coord_filas_estan_dentro_dimension == FALSE | coord_columnas_estan_dentro_dimension == FALSE){
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
                                        } else if(son_2_coordenadas == TRUE & coord_son_numeros_enteros == TRUE){
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
Inserte exactamente 2 coordenadas numéricas separadas por una coma.
Nota: La primera coordenada debe ser menor o igual al número de filas del tablero 
y la segunda menor o igual al número de columnas")
                                                }
                                        }
                                        
                                        coordenadas <- strsplit(coordenadas, ",")[[1]] # la función strsplit devuelve una lista de un elemento, por ello seleccionamos el primer y unico vector que contiene la lista
                                        if(length(coordenadas) == 2){
                                                if(all(grepl("^[0-9]+$", coordenadas)) == TRUE){
                                                        coordenadas <- as.integer(coordenadas)
                                                        if((coordenadas[1] > nrow(grid_gen_0)) | (coordenadas[2] > ncol(grid_gen_0))){
                                                                if((coordenadas[1] > nrow(grid_gen_0)) & (coordenadas[2] <= ncol(grid_gen_0))){
                                                                        son_2_coordenadas <- TRUE
                                                                        coord_son_numeros_enteros <- TRUE
                                                                        coord_filas_estan_dentro_dimension <- FALSE
                                                                        coord_columnas_estan_dentro_dimension <- TRUE
                                                                } else if((coordenadas[1] <= nrow(grid_gen_0)) & (coordenadas[2] > ncol(grid_gen_0))){
                                                                        son_2_coordenadas <- TRUE
                                                                        coord_son_numeros_enteros <- TRUE
                                                                        coord_filas_estan_dentro_dimension <- TRUE
                                                                        coord_columnas_estan_dentro_dimension <- FALSE
                                                                } else if((coordenadas[1] > nrow(grid_gen_0)) & (coordenadas[2] > ncol(grid_gen_0))) {
                                                                        son_2_coordenadas <- TRUE
                                                                        coord_son_numeros_enteros <- TRUE
                                                                        coord_filas_estan_dentro_dimension <- FALSE
                                                                        coord_columnas_estan_dentro_dimension <- FALSE
                                                                } 
                                                        } else if((coordenadas[1] <= nrow(grid_gen_0)) & (coordenadas[2] <= ncol(grid_gen_0))){
                                                                son_2_coordenadas <- TRUE
                                                                coord_son_numeros_enteros <- TRUE
                                                                coord_filas_estan_dentro_dimension <- TRUE
                                                                coord_columnas_estan_dentro_dimension <- TRUE
                                                        }
                                                } else{
                                                        son_2_coordenadas <- TRUE
                                                        coord_son_numeros_enteros <- FALSE
                                                }
                                        } else if(length(coordenadas) != 2){
                                                if(all(grepl("^[0-9]+$", coordenadas)) == TRUE){
                                                        coordenadas <- as.integer(coordenadas)
                                                        son_2_coordenadas <- FALSE
                                                        coord_son_numeros_enteros <- TRUE
                                                } else{
                                                        son_2_coordenadas <- FALSE
                                                        coord_son_numeros_enteros <- FALSE
                                                }
                                        }
                                }
                                if(son_2_coordenadas == TRUE & coord_son_numeros_enteros == TRUE & coord_filas_estan_dentro_dimension == TRUE & coord_columnas_estan_dentro_dimension == TRUE){
                                        if(option == 1){
                                                if(grid_gen_0[coordenadas[1], coordenadas[2]] == ""){
                                                        grid_gen_0[coordenadas[1], coordenadas[2]] <- "X"
                                                        edit(grid_gen_0)
                                                } else {
                                                        edit(grid_gen_0)
                                                        print("Esta célula ya está viva")
                                                } 
                                        } else if(option == 2){
                                                if(grid_gen_0[coordenadas[1], coordenadas[2]] == "X"){
                                                        grid_gen_0[coordenadas[1], coordenadas[2]] <- ""
                                                        edit(grid_gen_0)
                                                } else {
                                                        edit(grid_gen_0)
                                                        print("Esta célula ya está muerta")
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
        
        } else if(tipo_insercion == 2){
cells <- 
readline("Ahora, seleccione el número de células vivas que quiere introducir por favor ")
                if(grepl("^[0-9]+$", cells)){
                        cells <- as.integer(cells)
                        if(cells > (nrow(grid_gen_0) * ncol(grid_gen_0))){
                                entero <- TRUE
                                cells_menor_que_dimension <- FALSE
                        } else {
                                entero <- TRUE
                                cells_menor_que_dimension <- TRUE
                        }
                } else {
                        entero <- FALSE
                }
                while(entero == FALSE | cells_menor_que_dimension == FALSE){
                        if(entero == FALSE & cells_menor_que_dimension == FALSE){
                                print(paste("Debe introducir un número entero de células vivas, y este tiene que ser menor a "), nrow(grid_gen_0) * ncol(grid_gen_0), " (el número de celdas del tablón)")
cells <- 
readline("Volvamos a intentarlo:
Ahora, seleccione el número de células vivas que quiere introducir por favor ")
                        } else if(entero == FALSE & cells_menor_que_dimension == TRUE){
                                print(paste("Ha introducido un número no entero de células vivas. El número de células debe ser menor a "), nrow(grid_gen_0) * ncol(grid_gen_0), " (el número de celdas del tablón)")
cells <- 
readline("Volvamos a intentarlo:
Ahora, seleccione el número de células vivas que quiere introducir por favor ")
                        } else if(entero == TRUE & cells_menor_que_dimension == FALSE){
                                cat(paste("Ha introducido un número entero de células vivas mayor que "), nrow(grid_gen_0) * ncol(grid_gen_0), 
                                    " (el número de celdas del tablón)", "\nIntroduzca un número de células vivas menor que ", nrow(grid_gen_0) * ncol(grid_gen_0))
cells <- 
readline("Volvamos a intentarlo:
Ahora, seleccione el número de células vivas que quiere introducir por favor ")
                        }
                        if(grepl("^[0-9]+$", cells)){
                                cells <- as.integer(cells)
                                if(cells > (nrow(grid_gen_0) * ncol(grid_gen_0))){
                                        entero <- TRUE
                                        cells_menor_que_dimension <- FALSE
                                } else {
                                        entero <- TRUE
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
                                edit(grid_gen_0)
                        } else {
                                edit(grid_gen_0)
                                print("No ha insertado ninguna célula viva")
                        }
                }
        }
        grid_gen_0 <<- grid_gen_0
}

game_set_up()


reglas <- function(){
        edit(grid_gen_0)
        filas <- nrow(grid_gen_0)
        columnas <- ncol(grid_gen_0)
        grid_gen_1 <- matrix(data = grid_gen_0, nrow = filas, ncol = columnas)
        colnames(grid_gen_1) <- 1:ncol(grid_gen_1)
        
        for (i in 1:filas) {
                for (l in 1:columnas) {
                        if(grid_gen_0[i, l] == ""){
                                neighbour <- 0
                                for(i2 in max(1,(i-1)):min(filas,(i+1))){
                                        for(l2 in max(1,(l-1)):min(columnas,(l+1))){
                                                # Regla de reproducción
                                                if(grid_gen_0[i2, l2] == "X"){
                                                        neighbour <- neighbour + 1
                                                        if(neighbour == 3){
                                                                grid_gen_1[i, l] <- "X"
                                                                
                                                        }
                                                }
                                        }       
                                }
                        } else if (grid_gen_0[i,l] == "X"){
                                neighbour <- -1
                                for(i2 in max(1,(i-1)):min(filas,(i+1))){
                                        for(l2 in max(1,(l-1)):min(columnas,(l+1))){
                                                if(grid_gen_0[i2,l2] == "X"){
                                                        neighbour <- neighbour + 1
                                                        #Regla de supervivencia
                                                        #Como el bucle tambien tiene en cuenta la celula viva que está estudiando necesitamos que sea 1 mas
                                                        if(neighbour == 2 | neighbour == 3){
                                                                grid_gen_1[i, l] <- grid_gen_0[i ,l]
                                                        }
                                                        #Regla de soledad
                                                        #Mismo mecanismo que la anterior
                                                        if(neighbour == 0 | neighbour == 1){
                                                                grid_gen_1[i, l] <- ""
                                                        }
                                                        #Regla de superpoblación
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
reglas()
}






reproduction_jorge <- 


for (i in 1:filas) {
        for (l in 1:columnas) {
                if(grid_gen_0[i, l] == ""){
                        neighbour <- 0
                        for(i2 in max(1,(i-1)):min(filas,(i+1))){
                                for(l2 in max(1,(l-1)):min(columnas,(l+1))){
                                        # Regla de reproducción
                                        if(grid_gen_0[i2,l2] == "X"){
                                                neighbour <- neighbour + 1
                                                if(neighbour == 3){
                                                        grid_gen_1[i, l] <- "X"
                                                        
                                                }
                                        }
                                }       
                        }
                } else if (grid_gen_0[i,l] == "X"){
                        neighbour <- -1
                        for(i2 in max(1,(i-1)):min(filas,(i+1))){
                                for(l2 in max(1,(l-1)):min(columnas,(l+1))){
                                        if(grid_gen_0[i2,l2] == "X"){
                                                neighbour <- neighbour + 1
                                                #Regla de supervivencia
                                                #Como el bucle tambien tiene en cuenta la celula viva que está estudiando necesitamos que sea 1 mas
                                                if(neighbour == 2 | neighbour == 3){
                                                        grid_gen_1[i, l] <- grid_gen_0[i , j]
                                                }
                                                #Regla de soledad
                                                #Mismo mecanismo que la anterior
                                                if(neighbour == 0 | neighbour == 1){
                                                        grid_gen_1[i, l] <- ""
                                                }
                                                #Regla de superpoblación
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



regla_marc <-         

        for(i in 1:nrow(grid_gen_0)){
                neighbour <- 0
                for(j in 1:ncol(grid_gen_0)){
                        for(k in (max(1, (j - 1))):(min((j + 1), columnas))){
                                for(l in (max(1, (i - 1))):(min((i + 1), filas))){
                                        if(grid_gen_0[i, j] == ""){
                                                neighbour <- 0
                                                if(grid_gen_0[l, k] == "X"){
                                                        
                                                        # Regla de reproducción
                                                        neighbour <- neighbour + 1
                                                        if(neighbour == 3){
                                                                grid_gen_1[i, j] == "X"
                                                        } 
                                                }
                                        } else if(grid_gen_0[i, j] == "X"){
                                                neighbour <- -1
                                                if(grid_gen_0[l, k] == "X"){
                                                        neighbour <- neighbour + 1
                                                        
                                                        # Regla de supervivencia
                                                        if(neighbour == 2 | neighbour == 3){
                                                                grid_gen_1[i, j] <- grid_gen_0[i,j]
                                                                # Regla de soledad: 
                                                        } else if(neighbour == 0 | neighbour == 1){
                                                                grid_gen_1[i, j] <- ""
                                                                # Regla de superpoblación
                                                        } else if(neighbour >= 4){
                                                                grid_gen_1[i, j] <- ""
                                                        }
                                                }
                                        } 
                                }
                        }
                }
        }