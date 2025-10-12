% Archivo: base_datos.pl
:- module(base_datos, [
    pronombre/3,
    verbo/2,
    objeto/3,
    articulo/4
]).
% pronombre(Singular/Plural, Español, Inglés)

% --- Singular ---
pronombre(singular, yo, i).
pronombre(singular, tu, you).
pronombre(singular, el, he).
pronombre(singular, ella, she).
pronombre(singular, eso, it).

% --- Plural ---
pronombre(plural, nosotros, we).
pronombre(plural, ustedes, you).
pronombre(plural, ellos, they).


% verbo(Inglés, Español)

verbo(eat, come).
verbo(drink, bebe).
verbo(go, ir).
verbo(come, viene).
verbo(run, corre).
verbo(walk, camina).
verbo(read, lee).
verbo(write, escribe).
verbo(speak, habla).
verbo(listen, escucha).
verbo(see, ve).
verbo(look, mira).
verbo(love, ama).
verbo(have, tiene).
verbo(want, quiere).
verbo(need, necesita).
verbo(play, juega).
verbo(open, abre).
verbo(close, cierra).
verbo(cook, cocina).
verbo(sing, canta).
verbo(dance, baila).
verbo(sleep, duerme).
verbo(study, estudia).
verbo(work, trabaja).
verbo(travel, viaja).
verbo(buy, compra).
verbo(sell, vende).
%verbo(teach, enseñar).
verbo(learn, aprende).

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
