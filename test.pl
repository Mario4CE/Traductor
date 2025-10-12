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
    phrase(oracion_es(S, P), [ella, come, la, manzana]),
    S == sujeto(pronombre(ella)),
    P == predicado(verbo_objeto(come, articulo_sustantivo(la, manzana))).

test(declarativa_en_1) :-
    phrase(oracion_en(S, P), [she, eats, the, apple]),
    S == sujeto(pronombre(she)),
    P == predicado(verbo_objeto(eats, articulo_sustantivo(the, apple))).

/* === Preguntas === */

test(pregunta_es_1) :-
    phrase(pregunta_es(S, V, C), ["¿", ella, come, manzanas, ?]),
    S == sujeto(pronombre(ella)),
    V == come,
    C == sustantivo_plural(manzanas).

test(pregunta_en_1) :-
    phrase(pregunta_en(Aux, S, V, C), [does, he, eat, apples, ?]),
    Aux == auxiliar(does),
    S == sujeto(pronombre(he)),
    V == eat,
    C == articulo_sustantivo_plural(_, apples).

/* === Exclamaciones === */

test(exclamacion_es_1) :-
    phrase(exclamacion_es(S, P, _), ["¡", ella, come, manzanas, !]),
    S == sujeto(pronombre(ella)),
    P == predicado(verbo_objeto(come, sustantivo_plural(manzanas))).

test(exclamacion_en_1) :-
    phrase(exclamacion_en(S, P, _), [she, eats, apples, !]),
    S == sujeto(pronombre(she)),
    P == predicado(verbo_objeto(eats, sustantivo_plural(apples))).

:- end_tests(gramatica).