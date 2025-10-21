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


/* ============================
   === Verbos ============
   verbo(RaizIngles, Numero, Persona, FormaEspanol)
   Persona = primera | segunda | tercera
   Numero = singular | plural
   ============================ */

% === 1. eat - comer
verbo(eat, singular, primera, como).
verbo(eat, singular, segunda, comes).
verbo(eat, singular, tercera, come).
verbo(eat, plural, primera, comemos).
verbo(eat, plural, segunda, comen).
verbo(eat, plural, tercera, comen).

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
verbo(run, singular, primera, corro).
verbo(run, singular, segunda, corres).
verbo(run, singular, tercera, corre).
verbo(run, plural, primera, corremos).
verbo(run, plural, segunda, corren).
verbo(run, plural, tercera, corren).

% === 6. walk - caminar
verbo(walk, singular, primera, camino).
verbo(walk, singular, segunda, caminas).
verbo(walk, singular, tercera, camina).
verbo(walk, plural, primera, caminamos).
verbo(walk, plural, segunda, caminan).
verbo(walk, plural, tercera, caminan).

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
verbo(see, singular, primera, veo).
verbo(see, singular, segunda, ves).
verbo(see, singular, tercera, ve).
verbo(see, plural, primera, vemos).
verbo(see, plural, segunda, ven).
verbo(see, plural, tercera, ven).

% === 12. look - mirar
verbo(look, singular, primera, miro).
verbo(look, singular, segunda, miras).
verbo(look, singular, tercera, mira).
verbo(look, plural, primera, miramos).
verbo(look, plural, segunda, miran).
verbo(look, plural, tercera, miran).

% === 13. love - amar
verbo(love, singular, primera, amo).
verbo(love, singular, segunda, amas).
verbo(love, singular, tercera, ama).
verbo(love, plural, primera, amamos).
verbo(love, plural, segunda, aman).
verbo(love, plural, tercera, aman).

% === 14. have - tener
verbo(have, singular, primera, tengo).
verbo(have, singular, segunda, tienes).
verbo(have, singular, tercera, tiene).
verbo(have, plural, primera, tenemos).
verbo(have, plural, segunda, tienen).
verbo(have, plural, tercera, tienen).

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
verbo(sing, singular, primera, canto).
verbo(sing, singular, segunda, cantas).
verbo(sing, singular, tercera, canta).
verbo(sing, plural, primera, cantamos).
verbo(sing, plural, segunda, cantan).
verbo(sing, plural, tercera, cantan).

% === 22. dance - bailar
verbo(dance, singular, primera, bailo).
verbo(dance, singular, segunda, bailas).
verbo(dance, singular, tercera, baila).
verbo(dance, plural, primera, bailamos).
verbo(dance, plural, segunda, bailan).
verbo(dance, plural, tercera, bailan).

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
objeto(singular, car, carro).
objeto(singular, house, casa).
objeto(singular, table, mesa).
objeto(singular, chair, silla).
objeto(singular, pencil, lapiz).
objeto(singular, pen, boligrafo).
objeto(singular, school, escuela).
objeto(singular, teacher, maestro).
objeto(singular, student, estudiante).
objeto(singular, phone, telefono).
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
objeto(plural, cars, carros).
objeto(plural, houses, casas).
objeto(plural, tables, mesas).
objeto(plural, chairs, sillas).
objeto(plural, pencils, lapices).
objeto(plural, pens, boligrafos).
objeto(plural, schools, escuelas).
objeto(plural, teachers, maestros).
objeto(plural, students, estudiantes).
objeto(plural, phones, telefonos).
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
categoria_semantica(carro, objeto_inanimado).
categoria_semantica(carros, objeto_inanimado).
categoria_semantica(house, objeto_inanimado).
categoria_semantica(houses, objeto_inanimado).
categoria_semantica(casa, objeto_inanimado).
categoria_semantica(casas, objeto_inanimado).
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
categoria_semantica(boligrafo, objeto_inanimado).
categoria_semantica(boligrafos, objeto_inanimado).
categoria_semantica(phone, objeto_inanimado).
categoria_semantica(phones, objeto_inanimado).
categoria_semantica(telefono, objeto_inanimado).
categoria_semantica(telefonos, objeto_inanimado).
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
categoria_semantica(maestros, persona).
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
