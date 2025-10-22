% Archivo: base_datos.pl
:- module(base_datos, [
    pronombre/5,
    verbo/4,
    objeto/3,
    articulo/4,
    interrogativo/2,
    categoria_semantica/2,
    compatible_verbo_objeto/2
]).

/* ============================
   === Pronombres ============
   pronombre(Número, Persona, Género, Español, Inglés)
   Género = masculino | femenino | neutro
   Persona = primera | segunda | tercera
   ============================ */

% --- Singular ---
pronombre(singular, primera, neutro, yo, i).
pronombre(singular, segunda, neutro, tu, you).
pronombre(singular, tercera, masculino, el, he).
pronombre(singular, tercera, femenino, ella, she).
pronombre(singular, tercera, neutro, eso, it).
pronombre(singular, tercera, neutro, usted, you).
pronombre(singular, tercera, neutro, ello, it).
pronombre(singular, tercera, neutro, le, it).

% --- Plural ---
pronombre(plural, primera, neutro, nosotros, we).
pronombre(plural, segunda, neutro, ustedes, you).
pronombre(plural, tercera, masculino, ellos, they).
pronombre(plural, tercera, femenino, ellas, they).

% --- Sustantivos propios ---
pronombre(singular, tercera, neutro, X, X) :-
    member(X, [juan, maria, pedro, luisa, carlos, ana, laura]).
pronombre(plural, tercera, neutro, X, X) :-
    member(X, [juan_y_maria, pedro_y_luisa, carlos_y_ana, laura_y_ana]).
pronombre(singular, tercera, femenino, X, X) :-
    member(X, [ana, laura, maria, luisa]).
pronombre(singular, tercera, masculino, X, X) :-
    member(X, [juan, pedro, carlos, miguel]).
pronombre(plural, tercera, femenino, X, X) :-
    member(X, [maria_y_luisa, ana_y_laura]).
pronombre(plural, tercera, masculino, X, X) :-
    member(X, [juan_y_pedro, carlos_y_miguel]).

/* ============================
   === Verbos ============
   verbo(RaizIngles, Numero, Persona, FormaEspanol)
   Persona = primera | segunda | tercera
   Numero = singular | plural
   ============================ */

% === 1. eat - comer
verbo(eat, singular, primera, X) :- member(X, [como, devoro, ingiero]).
verbo(eat, singular, segunda, X) :- member(X, [comes, devoras, ingieres]).
verbo(eat, singular, tercera, X) :- member(X, [come, devora, ingiere]).
verbo(eat, plural, primera, X) :- member(X, [comemos, devoramos, ingerimos]).
verbo(eat, plural, segunda, X) :- member(X, [comen, devoran, ingieren]).
verbo(eat, plural, tercera, X) :- member(X, [comen, devoran, ingieren]).

% === 2. drink - beber
verbo(drink, singular, primera, bebo).
verbo(drink, singular, segunda, bebes).
verbo(drink, singular, tercera, bebe).
verbo(drink, plural, primera, bebemos).
verbo(drink, plural, segunda, beben).
verbo(drink, plural, tercera, beben).

% === 3. go - ir
verbo(go, singular, primera, voy).
verbo(go, singular, segunda, vas).
verbo(go, singular, tercera, va).
verbo(go, plural, primera, vamos).
verbo(go, plural, segunda, van).
verbo(go, plural, tercera, van).

% === 4. come - venir
verbo(come, singular, primera, vengo).
verbo(come, singular, segunda, vienes).
verbo(come, singular, tercera, viene).
verbo(come, plural, primera, venimos).
verbo(come, plural, segunda, vienen).
verbo(come, plural, tercera, vienen).

% === 5. run - correr
verbo(run, singular, primera, X) :- member(X, [corro, sprinto]).
verbo(run, singular, segunda, X) :- member(X, [corres, sprintas]).
verbo(run, singular, tercera, X) :- member(X, [corre, sprinta]).
verbo(run, plural, primera, X) :- member(X, [corremos, sprintamos]).
verbo(run, plural, segunda, X) :- member(X, [corren, sprintan]).
verbo(run, plural, tercera, X) :- member(X, [corren, sprintan]).

% === 6. walk - caminar
verbo(walk, singular, primera, X) :- member(X, [camino, paseo]).
verbo(walk, singular, segunda, X) :- member(X, [caminas, paseas]).
verbo(walk, singular, tercera, X) :- member(X, [camina, pasea]).
verbo(walk, plural, primera, X) :- member(X, [caminamos, paseamos]).
verbo(walk, plural, segunda, X) :- member(X, [caminan, pasean]).
verbo(walk, plural, tercera, X) :- member(X, [caminan, pasean]).

% === 7. read - leer
verbo(read, singular, primera, leo).
verbo(read, singular, segunda, lees).
verbo(read, singular, tercera, lee).
verbo(read, plural, primera, leemos).
verbo(read, plural, segunda, leen).
verbo(read, plural, tercera, leen).

% === 8. write - escribir
verbo(write, singular, primera, escribo).
verbo(write, singular, segunda, escribes).
verbo(write, singular, tercera, escribe).
verbo(write, plural, primera, escribimos).
verbo(write, plural, segunda, escriben).
verbo(write, plural, tercera, escriben).

% === 9. speak - hablar
verbo(speak, singular, primera, hablo).
verbo(speak, singular, segunda, hablas).
verbo(speak, singular, tercera, habla).
verbo(speak, plural, primera, hablamos).
verbo(speak, plural, segunda, hablan).
verbo(speak, plural, tercera, hablan).

% === 10. listen - escuchar
verbo(listen, singular, primera, escucho).
verbo(listen, singular, segunda, escuchas).
verbo(listen, singular, tercera, escucha).
verbo(listen, plural, primera, escuchamos).
verbo(listen, plural, segunda, escuchan).
verbo(listen, plural, tercera, escuchan).

% === 11. see - ver
verbo(see, singular, primera, X) :- member(X, [veo, miro, odservo]).
verbo(see, singular, segunda, X) :- member(X, [ves, miras, observas]).
verbo(see, singular, tercera, X) :- member(X, [ve, mira, odserva]).
verbo(see, plural, primera, X) :- member(X, [vemos, miramos, odservamos]).
verbo(see, plural, segunda, X) :- member(X, [ven, miran, odservan]).
verbo(see, plural, tercera, X) :- member(X, [ven, miran, odservan]).

% === 12. look - mirar
verbo(look, singular, primera, X) :- member(X, [miro, miro, observo]).
verbo(look, singular, segunda, X) :- member(X, [miras, miras, observas]).
verbo(look, singular, tercera, X) :- member(X, [mira, mira, observa]).
verbo(look, plural, primera, X) :- member(X, [miramos, miramos, observamos]).
verbo(look, plural, segunda, X) :- member(X, [miran, miran, observan]).
verbo(look, plural, tercera, X) :- member(X, [miran, miran, observan]).

% === 13. love - amar
verbo(love, singular, primera, X) :- member(X, [amo, quiero, adoro]).
verbo(love, singular, segunda, X) :- member(X, [amas, quieres, adoras]).
verbo(love, singular, tercera, X) :- member(X, [ama, quiere, adora]).
verbo(love, plural, primera, X) :- member(X, [amamos, queremos, adoramos]).
verbo(love, plural, segunda, X) :- member(X, [aman, quieren, adoran]).
verbo(love, plural, tercera, X) :- member(X, [aman, quieren, adoran]).

% === 14. have - tener
verbo(have, singular, primera, X) :- member(X, [tengo, poseo, cuento_con]).
verbo(have, singular, segunda, X) :- member(X, [tienes, posees, cuentas_con]).
verbo(has, singular, tercera, X) :- member(X, [tiene, posee, cuenta_con]).
verbo(have, plural, primera, X) :- member(X, [tenemos, poseemos, contamos_con]).
verbo(have, plural, segunda, X) :- member(X, [tienen, poseen, cuentan_con]).
verbo(have, plural, tercera, X) :- member(X, [tienen, poseen, cuentan_con]).

% === 15. want - querer
verbo(want, singular, primera, quiero).
verbo(want, singular, segunda, quieres).
verbo(want, singular, tercera, quiere).
verbo(want, plural, primera, queremos).
verbo(want, plural, segunda, quieren).
verbo(want, plural, tercera, quieren).

% === 16. need - necesitar
verbo(need, singular, primera, necesito).
verbo(need, singular, segunda, necesitas).
verbo(need, singular, tercera, necesita).
verbo(need, plural, primera, necesitamos).
verbo(need, plural, segunda, necesitan).
verbo(need, plural, tercera, necesitan).

% === 17. play - jugar
verbo(play, singular, primera, juego).
verbo(play, singular, segunda, juegas).
verbo(play, singular, tercera, juega).
verbo(play, plural, primera, jugamos).
verbo(play, plural, segunda, juegan).
verbo(play, plural, tercera, juegan).

% === 18. open - abrir
verbo(open, singular, primera, abro).
verbo(open, singular, segunda, abres).
verbo(open, singular, tercera, abre).
verbo(open, plural, primera, abrimos).
verbo(open, plural, segunda, abren).
verbo(open, plural, tercera, abren).

% === 19. close - cerrar
verbo(close, singular, primera, cierro).
verbo(close, singular, segunda, cierras).
verbo(close, singular, tercera, cierra).
verbo(close, plural, primera, cerramos).
verbo(close, plural, segunda, cierran).
verbo(close, plural, tercera, cierran).

% === 20. cook - cocinar
verbo(cook, singular, primera, cocino).
verbo(cook, singular, segunda, cocinas).
verbo(cook, singular, tercera, cocina).
verbo(cook, plural, primera, cocinamos).
verbo(cook, plural, segunda, cocinan).
verbo(cook, plural, tercera, cocinan).

% === 21. sing - cantar
verbo(sing, singular, primera, X):- member(X, [canto, entono]).
verbo(sing, singular, segunda, X):- member(X, [cantas, entonas]).
verbo(sing, singular, tercera, X):- member(X, [canta, entona]).
verbo(sing, plural, primera, X):- member(X, [cantamos, entonamos]).
verbo(sing, plural, segunda, X):- member(X, [cantan, entonan]).
verbo(sing, plural, tercera, X):- member(X, [cantan, entonan]).

% === 22. dance - bailar
verbo(dance, singular, primera, X) :- member(X, [bailo, danzo]).
verbo(dance, singular, segunda, X) :- member(X, [bailas, danzas]).
verbo(dance, singular, tercera, X) :- member(X, [baila, danza]).
verbo(dance, plural, primera, X) :- member(X, [bailamos, danzamos]).
verbo(dance, plural, segunda, X) :- member(X, [bailan, danzan]).
verbo(dance, plural, tercera, X) :- member(X, [bailan, danzan]).

% === 23. sleep - dormir
verbo(sleep, singular, primera, duermo).
verbo(sleep, singular, segunda, duermes).
verbo(sleep, singular, tercera, duerme).
verbo(sleep, plural, primera, dormimos).
verbo(sleep, plural, segunda, duermen).
verbo(sleep, plural, tercera, duermen).

% === 24. study - estudiar
verbo(study, singular, primera, estudio).
verbo(study, singular, segunda, estudias).
verbo(study, singular, tercera, estudia).
verbo(study, plural, primera, estudiamos).
verbo(study, plural, segunda, estudian).
verbo(study, plural, tercera, estudian).

% === 25. work - trabajar
verbo(work, singular, primera, trabajo).
verbo(work, singular, segunda, trabajas).
verbo(work, singular, tercera, trabaja).
verbo(work, plural, primera, trabajamos).
verbo(work, plural, segunda, trabajan).
verbo(work, plural, tercera, trabajan).


% objeto(Singular/Plural,Inglés, Español)

% --- Singular ---
objeto(singular, apple, manzana).
objeto(singular, book, libro).
objeto(singular, water, agua).
objeto(singular, door, puerta).
objeto(singular, window, ventana).
objeto(singular, ball, pelota).
objeto(singular, X, Y) :-
    member(X, [car, automobile, vehicle]),
    member(Y, [carro, coche, automovil]).
objeto(singular, house, X):-
    member(X, [casa, hogar, vivienda]).
objeto(singular, table, mesa).
objeto(singular, chair, silla).
objeto(singular, pencil, lapiz).
objeto(singular, pen, X) :-
    member(X, [boligrafo, pluma, lapicero]).
objeto(singular, school, escuela).
objeto(singular, teacher, X) :-
    member(X, [maestro, profesor, docente]).
objeto(singular, student, X) :-
    member(X, [estudiante, alumno, aprendiz]).
objeto(singular, X, Y) :-
    member(X, [phone, mobile, cellphone, smartphone]),
    member(Y, [telefono, movil, celular]).
objeto(singular, bread, pan).
objeto(singular, dog, perro).
objeto(singular, cat, gato).
objeto(singular, food, comida).
objeto(singular, friend, amigo).
objeto(singular, family, familia).
objeto(singular, city, ciudad).
objeto(singular, country, pais).
objeto(singular, job, trabajo).
objeto(singular, movie, pelicula).
objeto(singular, song, cancion).
objeto(singular, game, juego).
objeto(singular, computer, computadora).

% --- Plural ---
objeto(plural, apples, manzanas).
objeto(plural, books, libros).
objeto(plural, waters, aguas).
objeto(plural, doors, puertas).
objeto(plural, windows, ventanas).
objeto(plural, balls, pelotas).
objeto(plural, X, Y) :-
    member(X, [cars, automobiles, vehicles]),
    member(Y, [carros, coches, automoviles]).
objeto(plural, houses, X) :-
    member(X, [casas, hogares, viviendas]).
objeto(plural, tables, mesas).
objeto(plural, chairs, sillas).
objeto(plural, pencils, lapices).
objeto(plural, pens, X) :-
    member(X, [boligrafos, plumas, lapiceros]).
objeto(plural, schools, escuelas).
objeto(plural, teachers, X) :-
    member(X, [maestros, profesores, docentes]).
objeto(plural, students, X) :-
    member(X, [estudiantes, alumnos, aprendices]).
objeto(plural, X, Y) :-
    member(X, [phones, mobiles, cellphones, smartphones]),
    member(Y, [telefonos, moviles, celulares]).
objeto(plural, breads, panes).
objeto(plural, dogs, perros).
objeto(plural, cats, gatos).
objeto(plural, foods, comidas).
objeto(plural, friends, amigos).
objeto(plural, families, familias).
objeto(plural, cities, ciudades).
objeto(plural, countries, paises).
objeto(plural, jobs, trabajos).
objeto(plural, movies, peliculas).
objeto(plural, songs, canciones).
objeto(plural, games, juegos).
objeto(plural, computers, computadoras).


% articulo(definido/Indefinido, singular/plural, Inglés, Español)

% Definidos
articulo(definido, singular, el, the).
articulo(definido, singular, la, the).
articulo(definido, plural, los, the).
articulo(definido, plural, las, the).

% Indefinidos
articulo(indefinido, singular, un, a).
articulo(indefinido, singular, una, a).
articulo(indefinido, plural, unos, some).
articulo(indefinido, plural, unas, some).

/* ============================
   === Palabras Interrogativas ===
   interrogativo(Español, Inglés)
   ============================ */

interrogativo(que, what).
interrogativo(donde, where).
interrogativo(cuando, when).
interrogativo(quien, who).
interrogativo(por_que, why).
interrogativo(como, how).

/* ============================
   === Categorías Semánticas ===
   categoria_semantica(Objeto, Categoria)
   ============================ */

% Comida
categoria_semantica(apple, comida).
categoria_semantica(apples, comida).
categoria_semantica(manzana, comida).
categoria_semantica(manzanas, comida).
categoria_semantica(bread, comida).
categoria_semantica(breads, comida).
categoria_semantica(pan, comida).
categoria_semantica(panes, comida).
categoria_semantica(water, bebida).
categoria_semantica(waters, bebida).
categoria_semantica(agua, bebida).
categoria_semantica(aguas, bebida).
categoria_semantica(food, comida).
categoria_semantica(foods, comida).
categoria_semantica(comida, comida).
categoria_semantica(comidas, comida).

% Objetos inanimados
categoria_semantica(book, objeto_inanimado).
categoria_semantica(books, objeto_inanimado).
categoria_semantica(libro, objeto_inanimado).
categoria_semantica(libros, objeto_inanimado).
categoria_semantica(door, objeto_inanimado).
categoria_semantica(doors, objeto_inanimado).
categoria_semantica(puerta, objeto_inanimado).
categoria_semantica(puertas, objeto_inanimado).
categoria_semantica(window, objeto_inanimado).
categoria_semantica(windows, objeto_inanimado).
categoria_semantica(ventana, objeto_inanimado).
categoria_semantica(ventanas, objeto_inanimado).
categoria_semantica(ball, objeto_inanimado).
categoria_semantica(balls, objeto_inanimado).
categoria_semantica(pelota, objeto_inanimado).
categoria_semantica(pelotas, objeto_inanimado).
categoria_semantica(car, objeto_inanimado).
categoria_semantica(cars, objeto_inanimado).
categoria_semantica(automobile, objeto_inanimado).
categoria_semantica(automobiles, objeto_inanimado).
categoria_semantica(vehicle, objeto_inanimado).
categoria_semantica(vehicles, objeto_inanimado).
categoria_semantica(carro, objeto_inanimado).
categoria_semantica(carros, objeto_inanimado).
categoria_semantica(coche, objeto_inanimado).
categoria_semantica(coches, objeto_inanimado).
categoria_semantica(automovil, objeto_inanimado).
categoria_semantica(automoviles, objeto_inanimado).
categoria_semantica(house, objeto_inanimado).
categoria_semantica(houses, objeto_inanimado).
categoria_semantica(casa, objeto_inanimado).
categoria_semantica(hogar, objeto_inanimado).
categoria_semantica(vivienda, objeto_inanimado).
categoria_semantica(casas, objeto_inanimado).
categoria_semantica(hogares, objeto_inanimado).
categoria_semantica(viviendas, objeto_inanimado).
categoria_semantica(table, objeto_inanimado).
categoria_semantica(tables, objeto_inanimado).
categoria_semantica(mesa, objeto_inanimado).
categoria_semantica(mesas, objeto_inanimado).
categoria_semantica(chair, objeto_inanimado).
categoria_semantica(chairs, objeto_inanimado).
categoria_semantica(silla, objeto_inanimado).
categoria_semantica(sillas, objeto_inanimado).
categoria_semantica(pencil, objeto_inanimado).
categoria_semantica(pencils, objeto_inanimado).
categoria_semantica(lapiz, objeto_inanimado).
categoria_semantica(lapices, objeto_inanimado).
categoria_semantica(pen, objeto_inanimado).
categoria_semantica(pens, objeto_inanimado).
categoria_semantica(pluma, objeto_inanimado).
categoria_semantica(plumas, objeto_inanimado).
categoria_semantica(lapicero, objeto_inanimado).
categoria_semantica(lapiceros, objeto_inanimado).
categoria_semantica(boligrafo, objeto_inanimado).
categoria_semantica(boligrafos, objeto_inanimado).
categoria_semantica(phone, objeto_inanimado).
categoria_semantica(phones, objeto_inanimado).
categoria_semantica(mobile, objeto_inanimado).
categoria_semantica(mobiles, objeto_inanimado).
categoria_semantica(cellphone, objeto_inanimado).
categoria_semantica(cellphones, objeto_inanimado).
categoria_semantica(smartphone, objeto_inanimado).
categoria_semantica(smartphones, objeto_inanimado).
categoria_semantica(telefono, objeto_inanimado).
categoria_semantica(telefonos, objeto_inanimado).
categoria_semantica(movil, objeto_inanimado).
categoria_semantica(moviles, objeto_inanimado).
categoria_semantica(celular, objeto_inanimado).
categoria_semantica(celulares, objeto_inanimado).
categoria_semantica(computer, objeto_inanimado).
categoria_semantica(computers, objeto_inanimado).
categoria_semantica(computadora, objeto_inanimado).
categoria_semantica(computadoras, objeto_inanimado).

% Lugares
categoria_semantica(school, lugar).
categoria_semantica(schools, lugar).
categoria_semantica(escuela, lugar).
categoria_semantica(escuelas, lugar).
categoria_semantica(city, lugar).
categoria_semantica(cities, lugar).
categoria_semantica(ciudad, lugar).
categoria_semantica(ciudades, lugar).
categoria_semantica(country, lugar).
categoria_semantica(countries, lugar).
categoria_semantica(pais, lugar).
categoria_semantica(paises, lugar).

% Seres vivos (animales)
categoria_semantica(dog, animal).
categoria_semantica(dogs, animal).
categoria_semantica(perro, animal).
categoria_semantica(perros, animal).
categoria_semantica(cat, animal).
categoria_semantica(cats, animal).
categoria_semantica(gato, animal).
categoria_semantica(gatos, animal).

% Personas
categoria_semantica(teacher, persona).
categoria_semantica(teachers, persona).
categoria_semantica(maestro, persona).
categoria_semantica(profesor, persona).
categoria_semantica(docente, persona).
categoria_semantica(profesores, persona).
categoria_semantica(maestros, persona).
categoria_semantica(docentes, persona).
categoria_semantica(alumno, persona).
categoria_semantica(alumnos, persona).
categoria_semantica(aprendiz, persona).
categoria_semantica(aprendices, persona).
categoria_semantica(student, persona).
categoria_semantica(students, persona).
categoria_semantica(estudiante, persona).
categoria_semantica(estudiantes, persona).
categoria_semantica(friend, persona).
categoria_semantica(friends, persona).
categoria_semantica(amigo, persona).
categoria_semantica(amigos, persona).
categoria_semantica(family, persona).
categoria_semantica(families, persona).
categoria_semantica(familia, persona).
categoria_semantica(familias, persona).

% Abstractos
categoria_semantica(job, abstracto).
categoria_semantica(jobs, abstracto).
categoria_semantica(trabajo, abstracto).
categoria_semantica(trabajos, abstracto).
categoria_semantica(movie, entretenimiento).
categoria_semantica(movies, entretenimiento).
categoria_semantica(pelicula, entretenimiento).
categoria_semantica(peliculas, entretenimiento).
categoria_semantica(song, entretenimiento).
categoria_semantica(songs, entretenimiento).
categoria_semantica(cancion, entretenimiento).
categoria_semantica(canciones, entretenimiento).
categoria_semantica(game, entretenimiento).
categoria_semantica(games, entretenimiento).
categoria_semantica(juego, entretenimiento).
categoria_semantica(juegos, entretenimiento).

/* ============================
   === Compatibilidad Verbo-Objeto ===
   compatible_verbo_objeto(Verbo, CategoriaObjeto)
   Define qué verbos son compatibles con qué categorías
   ============================ */

% Comer - solo comida/bebida
compatible_verbo_objeto(eat, comida).
compatible_verbo_objeto(eat, bebida).

% Beber - solo bebida
compatible_verbo_objeto(drink, bebida).

% Leer - objetos con texto
compatible_verbo_objeto(read, objeto_inanimado).  % libros, etc.
compatible_verbo_objeto(read, entretenimiento).

% Escribir - puede escribir objetos
compatible_verbo_objeto(write, objeto_inanimado).

% Ver - casi todo
compatible_verbo_objeto(see, persona).
compatible_verbo_objeto(see, animal).
compatible_verbo_objeto(see, objeto_inanimado).
compatible_verbo_objeto(see, lugar).
compatible_verbo_objeto(see, entretenimiento).

% Mirar - casi todo
compatible_verbo_objeto(look, persona).
compatible_verbo_objeto(look, animal).
compatible_verbo_objeto(look, objeto_inanimado).
compatible_verbo_objeto(look, lugar).
compatible_verbo_objeto(look, entretenimiento).

% Amar - personas, animales, cosas
compatible_verbo_objeto(love, persona).
compatible_verbo_objeto(love, animal).
compatible_verbo_objeto(love, objeto_inanimado).
compatible_verbo_objeto(love, entretenimiento).

% Tener - casi todo
compatible_verbo_objeto(have, persona).
compatible_verbo_objeto(have, animal).
compatible_verbo_objeto(have, objeto_inanimado).
compatible_verbo_objeto(have, abstracto).
compatible_verbo_objeto(have, comida).
compatible_verbo_objeto(have, bebida).

% Querer - casi todo
compatible_verbo_objeto(want, persona).
compatible_verbo_objeto(want, animal).
compatible_verbo_objeto(want, objeto_inanimado).
compatible_verbo_objeto(want, abstracto).
compatible_verbo_objeto(want, comida).
compatible_verbo_objeto(want, bebida).
compatible_verbo_objeto(want, entretenimiento).

% Necesitar - casi todo
compatible_verbo_objeto(need, persona).
compatible_verbo_objeto(need, animal).
compatible_verbo_objeto(need, objeto_inanimado).
compatible_verbo_objeto(need, abstracto).
compatible_verbo_objeto(need, comida).
compatible_verbo_objeto(need, bebida).

% Jugar - entretenimiento, objetos
compatible_verbo_objeto(play, entretenimiento).
compatible_verbo_objeto(play, objeto_inanimado).

% Abrir/Cerrar - objetos físicos, lugares
compatible_verbo_objeto(open, objeto_inanimado).
compatible_verbo_objeto(open, lugar).
compatible_verbo_objeto(close, objeto_inanimado).
compatible_verbo_objeto(close, lugar).

% Cocinar - comida
compatible_verbo_objeto(cook, comida).

% Cantar/Bailar - entretenimiento
compatible_verbo_objeto(sing, entretenimiento).
compatible_verbo_objeto(dance, entretenimiento).

% Estudiar - objetos de estudio, abstractos
compatible_verbo_objeto(study, objeto_inanimado).
compatible_verbo_objeto(study, abstracto).

% Trabajar - abstracto
compatible_verbo_objeto(work, abstracto).

% Escuchar/Hablar - personas, entretenimiento
compatible_verbo_objeto(listen, persona).
compatible_verbo_objeto(listen, entretenimiento).
compatible_verbo_objeto(speak, persona).
