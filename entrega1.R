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
        }
        print("generacion 0")
        print(grid)
        reproduction_rule <- function(){
                for (i in 1:dim[1]) {
                        for (l in 1:dim[2]) {
                                if(grid[i,l]==FALSE){
                                        parents <- 0
                                        for(i2 in max(1,(i-1)):min(dim[1],(i+1))){
                                         for(l2 in max(1,(l-1)):min(dim[2],l+1)){
                                                 if(grid[i2,l2]==TRUE){
                                                         parents <- parents+1
                                                         if(parents==3){
                                                                 grid[i,l] = TRUE
                                                         }
                                                 }
                                         }       
                                        }
                                }
                        }
                }
        }
        reproduction_rule()
        print("generacion 1")
        print(grid)
}
game_set_up()

