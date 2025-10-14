/* ======================================================
   Tests compatibles con gramáticas existentes + nuevos tests wh
   ====================================================== */

:- begin_tests(gramatica).

:- use_module(base_datos).
:- use_module(bnf_es).
:- use_module(bnf_en).

/* === TESTS EXISTENTES (SIN CAMBIOS) === */

test(declarativa_es_1) :-
    phrase(oracion_es(S, P), [el, come, la, manzana]),
    S == sujeto(singular, tercera, el),
    P == predicado(verbo_objeto(come, articulo_sustantivo(la, manzana))).

test(declarativa_en_1) :-
    phrase(oracion_en(S, P), [she, eat, the, apple]),
    S == sujeto(singular, tercera, she),
    P == predicado(verbo_objeto(eat, articulo_sustantivo(the, apple))).

test(pregunta_es_1) :-  % ← MANTIENE 3 args
    phrase(pregunta_es(S, V, C), ["¿", ella, come, manzanas, ?]),
    S == sujeto(singular, tercera, ella),
    V == come,
    C == sustantivo_plural(manzanas).

test(pregunta_en_1) :-  % ← MANTIENE 4 args
    phrase(pregunta_en(Aux, S, V, C), [does, he, eat, apples, ?]),
    Aux == auxiliar(does),
    S == sujeto(singular, tercera, he),
    V == eat,
    C == sustantivo_plural(apples).

test(exclamacion_es_1) :-
    phrase(exclamacion_es(S, P, _), ["¡", ella, come, manzanas, "!"]),
    S == sujeto(singular, tercera, ella),
    P == predicado(verbo_objeto(come, sustantivo_plural(manzanas))).

test(exclamacion_en_1) :-
    phrase(exclamacion_en(S, P, _), [she, eat, apples, "!"]),
    S == sujeto(singular, tercera, she),
    P == predicado(verbo_objeto(eat, sustantivo_plural(apples))).

/* === NUEVOS TESTS WH (ARIDADES CORREGIDAS) === */

/* === TESTS WH CORREGIDOS === */

/* === Español (ya funcionan con pregunta_wh_es//3) === */
test(pregunta_wh_es_1) :-
    phrase(pregunta_wh_es(que, come, ella), ["¿", que, come, ella, ?]).

test(pregunta_wh_es_2) :-
    phrase(pregunta_wh_es(quien, come, sustantivo_plural(manzanas)), ["¿", quien, come, manzanas, ?]).

/* === Inglés Wh-Questions (AJUSTADOS A 4 args) === */

test(pregunta_wh_en_1) :-  % What does she eat?
    phrase(pregunta_wh_en(what, does, she, eat), [what, does, she, eat, ?]).

test(pregunta_wh_en_2) :-  % Who eats apples?
    phrase(pregunta_wh_en(who, eats, sustantivo_plural(apples), _), [who, eats, apples, ?]).

test(pregunta_wh_en_verifica) :-
    phrase(pregunta_wh_en(what, does, she, eat), [what, does, she, eat, ?]).

:- end_tests(gramatica).
