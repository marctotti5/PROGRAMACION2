game_set_up <- function(){
        print("Inserte el numero de filas y columnas del tablero, en ese orden")
        dim <- scan(nmax = 2)
        grid <- matrix(data = FALSE, ncol = dim[2], nrow = dim[1])
        print(grid)
        
        print("ahora, seleccione el número de celulas vivas que quiere introducir")
        cells <- scan(nmax = 1)
        print("A conticuacion seleccione la posicion de cada una introduciendo filas y columnas en ese orden en lineas diferentes")
        for(i in 1:cells){
                new_cell <- scan(nmax = 2)
                grid[new_cell[2],new_cell[1]] = TRUE
                print(grid)
        }
        print("generacion 0")
        print(grid)
        reproduction_rule <- function(){
                parents <- 0
                for (i in 1:dim[1]) {
                        for (l in 1:dim[2]) {
                                if(grid[i,l]==FALSE){
                                        for(i2 in (i-1):(i+1)){
                                         for(l2 in (l-1):(l+1)){
                                                 if(grid[i2,l2]==TRUE){
                                                         parents <- parents+1
                                                         if(parents==3){
                                                                 grid[i,l] == TRUE
                                                         }
                                                 }
                                         }       
                                        }
                                }
                        }
                }
        }
        print("generacion 1")
        reproduction_rule()
}
game_set_up()
