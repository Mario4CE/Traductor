/* ======================================================
   TESTS NEGATIVOS - Español / Inglés
   ====================================================== */
:- use_module(base_datos).
:- use_module(bnf_negativo_es).
:- use_module(bnf_negativo_en).

/* ==========================
   ORACIONES NEGATIVAS - ESPAÑOL
   ========================== */

test_negativa_es_1 :-
    write('Test N1: "el no come la manzana"'), nl,
    (   phrase(oracion_negativa_es(_S, _P), [el, no, come, la, manzana])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

test_negativa_es_2 :-
    write('Test N2: "ella no devora manzanas"'), nl,
    (   phrase(oracion_negativa_es(_S, _P), [ella, no, devora, manzanas])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

/* Pregunta negativa */
test_negativa_es_3 :-
    write('Test N3: "¿ella no come manzanas?"'), nl,
    (   phrase(pregunta_negativa_es(_S, _V, _C), ["¿", ella, no, come, manzanas, ?])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

/* Exclamación negativa */
test_negativa_es_4 :-
    write('Test N4: "¡ella no come manzanas!"'), nl,
    (   phrase(exclamacion_negativa_es(_Sujeto, _Predicado), 
               ["¡", ella, no, come, manzanas, "!"])
    ->  write('  ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

/* ==========================
   ORACIONES NEGATIVAS - INGLÉS
   ========================== */

test_negativa_en_1 :-
    write('Test N1 (EN): "she does not eat the apple"'), nl,
    (   phrase(oracion_negativa_en(_S, _P), [she, does, not, eat, the, apple])
    ->  write('  ✓ ACCEPTED'), nl
    ;   write('  ✗ REJECTED'), nl
    ), nl.

test_negativa_en_2 :-
    write('Test N2 (EN): "he does not eat apples"'), nl,
    (   phrase(oracion_negativa_en(_S, _P), [he, does, not, eat, apples])
    ->  write('  ✓ ACCEPTED'), nl
    ;   write('  ✗ REJECTED'), nl
    ), nl.

/* Pregunta negativa */
test_negativa_en_3 :-
    write('Test N3 (EN): "Does she not eat the apples?"'), nl,
    (   phrase(pregunta_negativa_en(_Aux, _Suj, _Verb, _Comp), 
               [does, she, not, eat, apples, "?"])
    ->  write('  ✓ ACCEPTED'), nl
    ;   write('  ✗ REJECTED'), nl
    ), nl.

/* Exclamación negativa */
test_negativa_en_4 :-
    write('Test N4 (EN): "She does not eat apples!"'), nl,
    (   phrase(exclamacion_negativa_en(_Sujeto, _Predicado),
               [she, does, not, eats, apples, "!"])
    ->  write('  ACEPTADO'), nl
    ;   write('  RECHAZADO'), nl
    ), nl.

/* ==========================
   EJECUTAR TODAS LAS PRUEBAS
   ========================== */

ejecutar_negativas :-
    write('========================================'), nl,
    write('PRUEBAS ORACIONES NEGATIVAS'), nl,
    write('========================================'), nl, nl,
    test_negativa_es_1,
    test_negativa_es_2,
    test_negativa_es_3,
    test_negativa_es_4,
    test_negativa_en_1,
    test_negativa_en_2,
    test_negativa_en_3,
    test_negativa_en_4,
    nl,
    write('========================================'), nl,
    write('PRUEBAS COMPLETADAS'), nl,
    write('========================================'), nl.

% Para ejecutar las pruebas, llama a:
% ?- ejecutar_negativas.