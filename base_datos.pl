% Archivo: base_datos.pl
:- module(base_datos, [
    pronombre/5,
    verbo/4,
    objeto/3,
    articulo/4,
    interrogativo/2
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
