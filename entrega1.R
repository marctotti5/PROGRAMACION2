
library(tidyverse)
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
        
        grid_gen_0 <- matrix(data = FALSE, nrow = filas, ncol = columnas) 
        entero <- FALSE
        fila_aleatoria <- 0
        columna_aleatoria <- 0
        dimension <- nrow(grid_gen_0) * ncol(grid_gen_0)
        
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
        son_2_coordenadas <- FALSE
        
        if(tipo_insercion == 1){
                while(option != 3){
                        son_2_coordenadas <- FALSE
                        es_numero <- FALSE
                       option <- readline("
Qué deseas hacer? 
1: Añadir célula viva
2: Eliminar célula viva
3: Terminar")
                       son_2_coordenadas <- FALSE
                       while(es_numero == FALSE){
                               if(grepl("^[1-3]+$", option)){
                                       es_numero <- TRUE
                                       option <- as.integer(option)
                                       son_2_coordenadas <- FALSE
                                       if(option == 1){
                                               coordenadas <- readline("Inserte las coordenadas separadas por comas  ")
                                               coordenadas <- str_split(coordenadas, ",")[[1]]
                                               coordenadas_splitted <- vector()
                                               for(i in 1:length(coordenadas)){
                                                       if(grepl("^[0-9]+$", coordenadas[i])){
                                                               coordenadas_splitted[i] <- as.integer(coordenadas[i])
                                                       } 
                                               }
                                               
                                               while(son_2_coordenadas == FALSE){
                                                       # A partir de aquí no entra
                                                       son_2_coordenadas <- TRUE
                                                       if(length(coordenadas_splitted) == 2){
                                                               if(grid_gen_0[coordenadas_splitted[1], coordenadas_splitted[2]] == FALSE){
                                                                       grid_gen_0[coordenadas_splitted[1], coordenadas_splitted[2]] <- TRUE
                                                                       print(grid_gen_0)
                                                               } else{
                                                                       print("Esta célula ya esta viva")
                                                                       son_2_coordenadas <- TRUE
                                                               }
                                                       } else {
                                                               print("Las coordenadas deben ser un vector de 2 elementos")
                                                               son_2_coordenadas <- TRUE
                                                       }
                                                       
                                               }
                                       }
                                } else{
                                       print("Inserte un número entre 1 y 3 por favor ")
                                       es_numero == FALSE
                               }
                       }
                       

                }
                
        } else if(tipo_insercion == 2){
                while(entero == FALSE){
                        cells <- readline("Ahora, seleccione el número de células vivas que quiere introducir por favor ")
                        if(grepl("^[0-9]+$", cells)){
                                entero <- TRUE
                                cells <- as.integer(cells)
                                if(cells > (nrow(grid_gen_0) * ncol(grid_gen_0))){
                                        entero <- FALSE      
                                } else if(cells == 0){
                                        grid_gen_0
                                } else {
                                        set.seed(1:cells)
                                        fila_aleatoria <- sample(1:nrow(grid_gen_0), size = cells, replace = TRUE)
                                        columna_aleatoria <- sample(1:ncol(grid_gen_0), size = cells, replace = TRUE)
                                        for(i in 1:cells){
                                                grid_gen_0[fila_aleatoria[i], columna_aleatoria[i]] <- TRUE
                                        }
                                        print(grid_gen_0)
                                }
                                
                        } else{
                                print(paste("El número de células vivas a introducir debe ser entero positivo y menor que ", dimension))
                        }
                }
        }
        grid_gen_1 <- matrix(data = grid_gen_0, nrow = filas, ncol = columnas)
        reproduction_rule <- function(){
        for (i in 1:filas) {
                for (l in 1:columnas) {
                        if(grid_gen_0[i, l]==FALSE){
                                parents <- 0
                                for(i2 in max(1,(i-1)):min(filas,(i+1))){
                                        for(l2 in max(1,(l-1)):min(columnas,(l+1))){
                                                if(grid_gen_0[i2,l2]==TRUE){
                                                        parents <- parents+1
                                                        if(parents==3){
                                                                grid_gen_1[i,l] <- TRUE
                                                                
                                                        }
                                                }
                                        }       
                                }
                        }
                }
        }
        
}
}
game_set_up()







reproduction_rule()
print("Generacion 1")
print(grid_gen_1)

