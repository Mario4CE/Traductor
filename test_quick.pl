/* Script de prueba rápida */
:- use_module(base_datos).
:- use_module(bnf_es).
:- use_module(bnf_en).

test_quick :-
    write('=== PRUEBAS RÁPIDAS ==='), nl, nl,
    
    write('Test 1: "el come la manzana" (válido) -> '),
    (phrase(oracion_es(_, _), [el, come, la, manzana]) -> write('✓ OK') ; write('✗ FAIL')), nl,
    
    write('Test 2: "el come las manzanas" (válido - plural) -> '),
    (phrase(oracion_es(_, _), [el, come, las, manzanas]) -> write('✓ OK') ; write('✗ FAIL')), nl,
    
    write('Test 3: "el come la manzanas" (inválido - concordancia) -> '),
    (\+ phrase(oracion_es(_, _), [el, come, la, manzanas]) -> write('✓ OK (rechazado)') ; write('✗ FAIL (aceptado)')), nl,
    
    write('Test 4: "ella come el libro" (inválido - semántica) -> '),
    (\+ phrase(oracion_es(_, _), [ella, come, el, libro]) -> write('✓ OK (rechazado)') ; write('✗ FAIL (aceptado)')), nl,
    
    write('Test 5: "el bebe el agua" (válido) -> '),
    (phrase(oracion_es(_, _), [el, bebe, el, agua]) -> write('✓ OK') ; write('✗ FAIL')), nl,
    
    write('Test 6: "el bebe el libro" (inválido - semántica) -> '),
    (\+ phrase(oracion_es(_, _), [el, bebe, el, libro]) -> write('✓ OK (rechazado)') ; write('✗ FAIL (aceptado)')), nl,
    
    write('Test 7: "she eat the apple" (válido EN) -> '),
    (phrase(oracion_en(_, _), [she, eat, the, apple]) -> write('✓ OK') ; write('✗ FAIL')), nl,
    
    write('Test 8: "she eat the apples" (válido EN - plural) -> '),
    (phrase(oracion_en(_, _), [she, eat, the, apples]) -> write('✓ OK') ; write('✗ FAIL')), nl,
    
    write('Test 9: "she eat the book" (inválido EN - semántica) -> '),
    (\+ phrase(oracion_en(_, _), [she, eat, the, book]) -> write('✓ OK (rechazado)') ; write('✗ FAIL (aceptado)')), nl,
    
    nl, 
    write('==================='), nl,
    write('Tests completados.'), nl,
    write('==================='), nl.

:- initialization(test_quick, halt).
