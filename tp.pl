% Tokmak Vladyslav

mago(harry, mestiza, [coraje, amistad, orgullo, inteligencia]).
mago(ron, pura, [amistad, diversion, coraje]).
mago(hermione, impura, [inteligencia, coraje, responsabilidad, amistad, orgullo]).
mago(hannahAbbott, mestiza, [amistad, diversion]).
mago(draco, pura, [inteligencia, orgullo]).
mago(lunaLovegood, mestiza, [inteligencia, responsabilidad, amistad, coraje]).

odia(harry,slytherin).
odia(draco,hufflepuff).

casa(gryffindor).
casa(hufflepuff).
casa(ravenclaw).
casa(slytherin).

caracteriza(gryffindor,amistad).
caracteriza(gryffindor,coraje).
caracteriza(slytherin,orgullo).
caracteriza(slytherin,inteligencia).
caracteriza(ravenclaw,inteligencia).
caracteriza(ravenclaw,responsabilidad).
caracteriza(hufflepuff,amistad).
caracteriza(hufflepuff,diversion).

lugarProhibido(bosque,50).
lugarProhibido(seccionRestringida,10).
lugarProhibido(tercerPiso,75).

alumnoFavorito(flitwick, hermione).
alumnoFavorito(snape, draco).
alumnoOdiado(snape, harry).

hizo(ron, buenaAccion(jugarAlAjedrez, 50)).
hizo(harry, fueraDeCama).
hizo(hermione, irA(tercerPiso)).
hizo(hermione, responder('Donde se encuentra un Bezoar', 15, snape)).
hizo(hermione, responder('Wingardium Leviosa', 25, flitwick)).
hizo(ron, irA(bosque)).
hizo(draco, irA(mazmorras)).

% AUX

caracteristicasMago(Mago,Caracteristica):- mago(Mago,_,Caracteristicas), 
											member(Caracteristica,Caracteristicas).

% PUNTO 1  

permiteEntrar(Casa, Mago):- mago(Mago,_,_), 
                             casa(Casa), 
							 Casa \= slytherin.
						
permiteEntrar(slytherin, Mago):- mago(Mago,Sangre,_), 
								  Sangre \= impura.
								
% PUNTO 2
											 
tieneCaracter(Mago,Casa):- mago(Mago,_,_), 
						   casa(Casa),
	                       forall(caracteriza(Casa,Caracteristica), caracteristicasMago(Mago,Caracteristica)).
						   

											 
% PUNTO 3

casaPosible(Mago,Casa):- tieneCaracter(Mago,Casa),
						 permiteEntrar(Casa,Mago),
	                     not(odia(Mago,Casa)).

% PUNTO 4

cadenaDeAmistades(Magos):- forall(member(Mago,Magos), caracteristicasMago(Mago, amistad)),
	                        estanEnMismaCasa(Magos).

estanEnMismaCasa([Mago1, Mago2 | RestoDeMagos]):- casaPosible(Mago1, Casa),
											      casaPosible(Mago2, Casa),
											      estanEnMismaCasa([Mago2 | RestoDeMagos]).
	
% Casas asignadas

esDe(harry, gryffindor).
esDe(ron, gryffindor).
esDe(hermione, gryffindor).
esDe(lunaLovegood, ravenclaw).
esDe(draco, slytherin).
esDe(hannahAbbott, hufflepuff).
   
% PUNTO 5

esBuenAlumno(Mago):- hizo(Mago,_),
					 forall(hizo(Mago,Accion), (puntosPorAccion(Accion,Puntos), Puntos >= 0)).
					  
puntosPorAccion(buenaAccion(_,Puntos), Puntos).

puntosPorAccion(fueraDeCama, -50).

puntosPorAccion(irA(Lugar), 0):- not(lugarProhibido(Lugar,_)).

puntosPorAccion(irA(Lugar),Puntos):- lugarProhibido(Lugar,PuntosNegativos),
									 Puntos is -1 * PuntosNegativos.

puntosPorAccion(responder(Pregunta,Dificultad,Profesor),Puntos):- hizo(Mago,responde(Pregunta,Dificultad,Profesor)),
															tipoAlumnoParaProfesor(Mago,Profesor,Multiplicador),
															Puntos is Multiplicador * Dificultad.

tipoAlumnoParaProfesor(Mago,Profesor,2):- alumnoFavorito(Profesor,Mago).

tipoAlumnoParaProfesor(Mago,Profesor,0):- alumnoOdiado(Profesor,Mago).

tipoAlumnoParaProfesor(Mago,Profesor,1):- not(alumnoFavorito(Profesor,Mago)),
										  not(alumnoOdiado(Profesor,Mago)).

% PUNTO 6

puntosDeCasa(Casa, SumatoriaTotal):- casa(Casa),
									 findall(Puntos, (esDe(Mago,Casa), puntosEchosPorMago(Mago, Puntos)), TotalDePuntos),
									 sumlist(TotalDePuntos,SumatoriaTotal).
									 
puntosEchosPorMago(Mago,TotalPuntosMago):- findall(Puntos,(hizo(Mago,Accion), puntosPorAccion(Accion,Puntos)),PuntosPorAccion),
									       sumlist(PuntosPorAccion,TotalPuntosMago).

% PUNTO 7

casaGanadora(Casa):- puntosDeCasa(Casa, Puntos),
					 forall((puntosDeCasa(OtraCasa,PuntosOtraCasa), OtraCasa \= Casa), Puntos > PuntosOtraCasa).