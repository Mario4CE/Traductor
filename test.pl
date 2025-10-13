/* ======================================================
   Archivo de pruebas para las gramáticas
   Ejecutar:  ?- run_tests.
   ====================================================== */

:- begin_tests(gramatica).

:- use_module(base_datos).
:- use_module(bnf_es).
:- use_module(bnf_en).

/* === Declarativas === */

test(declarativa_es_1) :-
    phrase(oracion_es(S, P), [el, come, la, manzana]),
    S == sujeto(singular, tercera, el),
    P == predicado(verbo_objeto(come, articulo_sustantivo(la, manzana))).

test(declarativa_en_1) :-
    phrase(oracion_en(S, P), [she, eat, the, apple]),
    S == sujeto(singular, tercera, she),
    P == predicado(verbo_objeto(eat, articulo_sustantivo(the, apple))).

/* === Preguntas === */

test(pregunta_es_1) :-
    phrase(pregunta_es(S, V, C), ["¿", ella, come, manzanas, ?]),
    S == sujeto(singular, tercera, ella),
    V == come,
    C == sustantivo_plural(manzanas).

test(pregunta_en_1) :-
    phrase(pregunta_en(Aux, S, V, C), [does, he, eat, apples, ?]),
    Aux == auxiliar(does),
    S == sujeto(singular, tercera, he),
    V == eat,
    C == sustantivo_plural(apples).

/* === Exclamaciones === */

test(exclamacion_es_1) :-
    phrase(exclamacion_es(S, P, _), ["¡", ella, come, manzanas, "!"]),
    S == sujeto(singular, tercera, ella),
    P == predicado(verbo_objeto(come, sustantivo_plural(manzanas))).

test(exclamacion_en_1) :-
    phrase(exclamacion_en(S, P, _), [she, eat, apples, "!"]),
    S == sujeto(singular, tercera, she),
    P == predicado(verbo_objeto(eat, sustantivo_plural(apples))).

:- end_tests(gramatica).
