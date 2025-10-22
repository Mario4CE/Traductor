/* ======================================================
   Tests de Validación - Conjugación y Coherencia Semántica
   ====================================================== */

:- use_module(base_datos).
:- use_module(bnf_es).
:- use_module(bnf_en).

/* ======================================================
   PRUEBAS DE CONJUGACIÓN SINGULAR/PLURAL
   ====================================================== */

% Test 1: Artículo singular + sustantivo singular (CORRECTO)
test_conjugacion_1 :-
    write('Test 1: "juan devora la manzana" (artículo singular + sustantivo singular)'),
    nl,
    (   phrase(oracion_es(_S, _P), [juan, devora, la, manzana])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

% Test 2: Artículo plural + sustantivo plural (CORRECTO)
test_conjugacion_2 :-
    write('Test 2: "el come las manzanas" (artículo plural + sustantivo plural)'),
    nl,
    (   phrase(oracion_es(_S, _P), [el, come, las, manzanas])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

% Test 3: Artículo singular + sustantivo plural (INCORRECTO - debe fallar)
test_conjugacion_3 :-
    write('Test 3: "el come la manzanas" (artículo singular + sustantivo plural)'),
    nl,
    (   phrase(oracion_es(_, _), [el, come, la, manzanas])
    ->  write('  ✗ ERROR: No debería aceptarse'), nl
    ;   write('  ✓ CORRECTO: RECHAZADO (concordancia incorrecta)'), nl
    ), nl.

% Test 4: Artículo plural + sustantivo singular (INCORRECTO - debe fallar)
test_conjugacion_4 :-
    write('Test 4: "el come las manzana" (artículo plural + sustantivo singular)'),
    nl,
    (   phrase(oracion_es(_, _), [el, come, las, manzana])
    ->  write('  ✗ ERROR: No debería aceptarse'), nl
    ;   write('  ✓ CORRECTO: RECHAZADO (concordancia incorrecta)'), nl
    ), nl.

% Test 5: Inglés - artículo "a" + sustantivo singular (CORRECTO)
test_conjugacion_5 :-
    write('Test 5 (EN): "she eat a apple" (artículo singular + sustantivo singular)'),
    nl,
    (   phrase(oracion_en(_S, _P), [she, eat, a, apple])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

% Test 6: Inglés - artículo "the" + sustantivo plural (CORRECTO)
test_conjugacion_6 :-
    write('Test 6 (EN): "she eat the apples" (artículo plural compatible)'),
    nl,
    (   phrase(oracion_en(_S, _P), [she, eat, the, apples])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

/* ======================================================
   PRUEBAS DE COHERENCIA SEMÁNTICA
   ====================================================== */

% Test 7: Comer manzana (CORRECTO - comida)
test_semantica_1 :-
    write('Test 7: "ella come la manzana" (verbo comer + comida)'),
    nl,
    (   phrase(oracion_es(_S, _P), [ella, come, la, manzana])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

% Test 8: Comer zapato (INCORRECTO - objeto inanimado no comestible)
test_semantica_2 :-
    write('Test 8: "ella come el libro" (verbo comer + objeto no comestible)'),
    nl,
    (   phrase(oracion_es(_, _), [ella, come, el, libro])
    ->  write('  ✗ ERROR: No debería aceptarse (incoherente)'), nl
    ;   write('  ✓ CORRECTO: RECHAZADO (semánticamente inválido)'), nl
    ), nl.

% Test 9: Beber agua (CORRECTO - bebida)
test_semantica_3 :-
    write('Test 9: "el bebe el agua" (verbo beber + bebida)'),
    nl,
    (   phrase(oracion_es(_S, _P), [el, bebe, el, agua])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

% Test 10: Beber libro (INCORRECTO - no es bebida)
test_semantica_4 :-
    write('Test 10: "el bebe el libro" (verbo beber + no bebida)'),
    nl,
    (   phrase(oracion_es(_, _), [el, bebe, el, libro])
    ->  write('  ✗ ERROR: No debería aceptarse (incoherente)'), nl
    ;   write('  ✓ CORRECTO: RECHAZADO (semánticamente inválido)'), nl
    ), nl.

% Test 11: Leer libro (CORRECTO)
test_semantica_5 :-
    write('Test 11: "ella lee el libro" (verbo leer + objeto legible)'),
    nl,
    (   phrase(oracion_es(_S, _P), [ella, lee, el, libro])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

% Test 12: Cocinar comida (CORRECTO)
test_semantica_6 :-
    write('Test 12: "el cocina el pan" (verbo cocinar + comida)'),
    nl,
    (   phrase(oracion_es(_S, _P), [el, cocina, el, pan])
    ->  write('  ✓ ACEPTADO'), nl
    ;   write('  ✗ RECHAZADO'), nl
    ), nl.

% Test 13: Cocinar computadora (INCORRECTO)
test_semantica_7 :-
    write('Test 13: "el cocina la computadora" (verbo cocinar + no comida)'),
    nl,
    (   phrase(oracion_es(_, _), [el, cocina, la, computadora])
    ->  write('  ✗ ERROR: No debería aceptarse (incoherente)'), nl
    ;   write('  ✓ CORRECTO: RECHAZADO (semánticamente inválido)'), nl
    ), nl.

% Test 14: Inglés - Eat apple (CORRECTO)
test_semantica_8 :-
    write('Test 14 (EN): "she eat the apple" (verb eat + food)'),
    nl,
    (   phrase(oracion_en(_S, _P), [she, eat, the, apple])
    ->  write('  ✓ ACCEPTED'), nl
    ;   write('  ✗ REJECTED'), nl
    ), nl.

% Test 15: Inglés - Eat book (INCORRECTO)
test_semantica_9 :-
    write('Test 15 (EN): "she eat the book" (verb eat + non-food)'),
    nl,
    (   phrase(oracion_en(_, _), [she, eat, the, book])
    ->  write('  ✗ ERROR: Should not be accepted (incoherent)'), nl
    ;   write('  ✓ CORRECT: REJECTED (semantically invalid)'), nl
    ), nl.

/* ======================================================
   EJECUCIÓN DE TODAS LAS PRUEBAS
   ====================================================== */

ejecutar_todas_las_pruebas :-
    write('========================================'), nl,
    write('PRUEBAS DE CONJUGACIÓN SINGULAR/PLURAL'), nl,
    write('========================================'), nl, nl,
    test_conjugacion_1,
    test_conjugacion_2,
    test_conjugacion_3,
    test_conjugacion_4,
    test_conjugacion_5,
    test_conjugacion_6,
    nl,
    write('========================================'), nl,
    write('PRUEBAS DE COHERENCIA SEMÁNTICA'), nl,
    write('========================================'), nl, nl,
    test_semantica_1,
    test_semantica_2,
    test_semantica_3,
    test_semantica_4,
    test_semantica_5,
    test_semantica_6,
    test_semantica_7,
    test_semantica_8,
    test_semantica_9,
    nl,
    write('========================================'), nl,
    write('PRUEBAS COMPLETADAS'), nl,
    write('========================================'), nl.

/* Para ejecutar en SWI-Prolog:
   ?- [test_validaciones].
   ?- ejecutar_todas_las_pruebas.
*/
