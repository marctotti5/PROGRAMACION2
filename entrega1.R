game_set_up <- function(){
        print("Inserte el numero de filas y columnas del tablero, en ese orden separado por comas")
        dim <- scan(nmax = 2, what = vector(mode = "numeric", length = 2), sep = ",")
        grid_gen_0 <- matrix(data = FALSE, ncol = dim[2], nrow = dim[1])
        print(grid_gen_0)
        
        print("Ahora, seleccione el número de celulas vivas que quiere introducir")
        cells <- scan(nmax = 1)
        print("A conticuacion seleccione la posicion de cada una introduciendo filas y columnas en ese orden separadas por comas")
        for(i in 1:cells){
                new_cell <- scan(nmax = 2, what = vector(mode = "numeric", length = 2), sep = ",")
                grid_gen_0[new_cell[2],new_cell[1]] = TRUE
        }
        print("Generacion 0")
        print(grid_gen_0)
        reproduction_rule <- function(){
                for (i in 1:dim[1]) {
                        for (l in 1:dim[2]) {
                                if(grid_gen_0[i,l]==FALSE){
                                        parents <- 0
                                        for(i2 in max(1,(i-1)):min(dim[1],(i+1))){
                                         for(l2 in max(1,(l-1)):min(dim[2],(l+1))){
                                                 if(grid_gen_0[i2,l2]==TRUE){
                                                         parents <- parents+1
                                                         if(parents==3){
                                                                 grid_gen_0[i,l] <- TRUE
                                                                 
                                                         }
                                                 }
                                         }       
                                        }
                                }
                        }
                }
                
        }
        reproduction_rule()
        print("Generacion 1")
        print(grid_gen_0)
}
game_set_up()
♦
