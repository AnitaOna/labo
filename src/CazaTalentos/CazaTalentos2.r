

set.seed( 308803 )

#calcula cuantos encestes logra un jugador con indice de enceste prob
#haciendo qyt tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}


#defino los jugadores
mejor      <- 0.7
peloton    <- ( 5101:5299 ) / 1000
jugadores  <- c( mejor, peloton )

#veo que tiene el vector
jugadores


#hago que los 60 jugadores tiren 10 veces cada uno
mapply( ftirar, jugadores, 100 )

primero_ganador  <- 0

for( i in 1:10000 ){  #diez mil experimentos

  vaciertos  <- mapply( ftirar, jugadores, 100 )  #10 tiros libres cada jugador

  mejor  <- which.max( vaciertos )
  if( mejor == 1 )  primero_ganador  <- primero_ganador + 1
}


print(  primero_ganador )
