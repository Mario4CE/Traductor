/* ======================================================
   EJEMPLOS DE USO - Sistema Traductor
   Casos de prueba rápidos para verificar el sistema
   ====================================================== */

:- use_module(base_datos).
:- use_module(bnf_es).
:- use_module(bnf_en).

/* ======================================================
   CASOS DE PRUEBA RÁPIDOS
   ====================================================== */

% Prueba rápida 1: Oraciones válidas en español
ejemplo_1 :-
    write('Ejemplo 1: Oraciones válidas en español'), nl,
    phrase(oracion_es(S1, P1), [ella, come, la, manzana]),
    write('  ✓ ella come la manzana'), nl,
    phrase(oracion_es(S2, P2), [el, bebe, el, agua]),
    write('  ✓ el bebe el agua'), nl,
    phrase(oracion_es(S3, P3), [ella, lee, el, libro]),
    write('  ✓ ella lee el libro'), nl,
    nl.

% Prueba rápida 2: Oraciones inválidas - concordancia
ejemplo_2 :-
    write('Ejemplo 2: Errores de concordancia'), nl,
    (   phrase(oracion_es(_, _), [ella, come, la, manzanas])
    ->  write('  ✗ ERROR: No debería aceptar "la manzanas"'), nl
    ;   write('  ✓ Correctamente rechaza "la manzanas"'), nl
    ),
    (   phrase(oracion_es(_, _), [el, bebe, las, agua])
    ->  write('  ✗ ERROR: No debería aceptar "las agua"'), nl
    ;   write('  ✓ Correctamente rechaza "las agua"'), nl
    ),
    nl.

% Prueba rápida 3: Oraciones inválidas - semántica
ejemplo_3 :-
    write('Ejemplo 3: Errores semánticos'), nl,
    (   phrase(oracion_es(_, _), [ella, come, el, libro])
    ->  write('  ✗ ERROR: No debería aceptar "come el libro"'), nl
    ;   write('  ✓ Correctamente rechaza "come el libro"'), nl
    ),
    (   phrase(oracion_es(_, _), [el, bebe, la, silla])
    ->  write('  ✗ ERROR: No debería aceptar "bebe la silla"'), nl
    ;   write('  ✓ Correctamente rechaza "bebe la silla"'), nl
    ),
    nl.

% Prueba rápida 4: Inglés
ejemplo_4 :-
    write('Ejemplo 4: Oraciones en inglés'), nl,
    phrase(oracion_en(S1, P1), [she, eat, the, apple]),
    write('  ✓ she eat the apple'), nl,
    (   phrase(oracion_en(_, _), [she, eat, the, book])
    ->  write('  ✗ ERROR: No debería aceptar "eat the book"'), nl
    ;   write('  ✓ Correctamente rechaza "eat the book"'), nl
    ),
    nl.

% Ejecutar todos los ejemplos
ejemplos_rapidos :-
    write('========================================'), nl,
    write('EJEMPLOS RÁPIDOS'), nl,
    write('========================================'), nl, nl,
    ejemplo_1,
    ejemplo_2,
    ejemplo_3,
    ejemplo_4,
    write('========================================'), nl,
    write('EJEMPLOS COMPLETADOS'), nl,
    write('========================================'), nl.

/* ======================================================
   CASOS DE USO POR CATEGORÍA
   ====================================================== */

% Verbos de alimentación
ejemplos_comida :-
    write('=== VERBOS DE ALIMENTACIÓN ==='), nl, nl,
    
    write('COMER (eat):'), nl,
    phrase(oracion_es(_, _), [ella, come, la, manzana]),
    write('  ✓ ella come la manzana'), nl,
    phrase(oracion_es(_, _), [el, come, el, pan]),
    write('  ✓ el come el pan'), nl,
    nl,
    
    write('BEBER (drink):'), nl,
    phrase(oracion_es(_, _), [el, bebe, el, agua]),
    write('  ✓ el bebe el agua'), nl,
    nl,
    
    write('COCINAR (cook):'), nl,
    phrase(oracion_es(_, _), [ella, cocina, el, pan]),
    write('  ✓ ella cocina el pan'), nl,
    phrase(oracion_es(_, _), [el, cocina, la, manzana]),
    write('  ✓ el cocina la manzana'), nl,
    nl.

% Verbos de conocimiento
ejemplos_conocimiento :-
    write('=== VERBOS DE CONOCIMIENTO ==='), nl, nl,
    
    write('LEER (read):'), nl,
    phrase(oracion_es(_, _), [ella, lee, el, libro]),
    write('  ✓ ella lee el libro'), nl,
    nl,
    
    write('ESCRIBIR (write):'), nl,
    phrase(oracion_es(_, _), [el, escribe, el, libro]),
    write('  ✓ el escribe el libro'), nl,
    nl,
    
    write('ESTUDIAR (study):'), nl,
    phrase(oracion_es(_, _), [ella, estudia, el, libro]),
    write('  ✓ ella estudia el libro'), nl,
    nl.

% Verbos de percepción
ejemplos_percepcion :-
    write('=== VERBOS DE PERCEPCIÓN ==='), nl, nl,
    
    write('VER (see):'), nl,
    phrase(oracion_es(_, _), [ella, ve, el, libro]),
    write('  ✓ ella ve el libro'), nl,
    phrase(oracion_es(_, _), [el, ve, el, perro]),
    write('  ✓ el ve el perro'), nl,
    nl,
    
    write('MIRAR (look):'), nl,
    phrase(oracion_es(_, _), [ella, mira, la, pelicula]),
    write('  ✓ ella mira la pelicula'), nl,
    nl.

% Todos los casos de uso
todos_los_ejemplos :-
    write('========================================'), nl,
    write('CASOS DE USO POR CATEGORÍA'), nl,
    write('========================================'), nl, nl,
    ejemplos_comida,
    ejemplos_conocimiento,
    ejemplos_percepcion,
    write('========================================'), nl.

/* ======================================================
   COMPARACIÓN ESPAÑOL-INGLÉS
   ====================================================== */

comparar_idiomas :-
    write('=== COMPARACIÓN ESPAÑOL-INGLÉS ==='), nl, nl,
    
    write('ESPAÑOL: ella come la manzana'), nl,
    phrase(oracion_es(S1, P1), [ella, come, la, manzana]),
    write('  Sujeto: '), write(S1), nl,
    write('  Predicado: '), write(P1), nl,
    nl,
    
    write('INGLÉS: she eat the apple'), nl,
    phrase(oracion_en(S2, P2), [she, eat, the, apple]),
    write('  Subject: '), write(S2), nl,
    write('  Predicate: '), write(P2), nl,
    nl,
    
    write('Ambas tienen la misma estructura semántica'), nl,
    nl.

/* ======================================================
   TABLA DE COMPATIBILIDADES
   ====================================================== */

mostrar_compatibilidades :-
    write('========================================'), nl,
    write('TABLA DE COMPATIBILIDADES VERBO-OBJETO'), nl,
    write('========================================'), nl, nl,
    
    write('COMER (eat):'), nl,
    write('  ✓ comida: manzana, pan'), nl,
    write('  ✓ bebida: agua'), nl,
    write('  ✗ objeto_inanimado: libro, silla'), nl,
    nl,
    
    write('BEBER (drink):'), nl,
    write('  ✓ bebida: agua'), nl,
    write('  ✗ comida: manzana, pan'), nl,
    write('  ✗ objeto_inanimado: libro, silla'), nl,
    nl,
    
    write('LEER (read):'), nl,
    write('  ✓ objeto_inanimado: libro'), nl,
    write('  ✓ entretenimiento: pelicula'), nl,
    write('  ✗ comida: manzana'), nl,
    write('  ✗ bebida: agua'), nl,
    nl,
    
    write('VER (see):'), nl,
    write('  ✓ persona: maestro, amigo'), nl,
    write('  ✓ animal: perro, gato'), nl,
    write('  ✓ objeto_inanimado: libro, casa'), nl,
    write('  ✓ lugar: ciudad, escuela'), nl,
    write('  ✓ entretenimiento: pelicula, juego'), nl,
    nl.

/* ======================================================
   VERIFICACIÓN DE CATEGORÍAS
   ====================================================== */

verificar_categoria(Objeto) :-
    (   categoria_semantica(Objeto, Cat)
    ->  write('  "'), write(Objeto), write('" es de categoría: '), write(Cat), nl
    ;   write('  "'), write(Objeto), write('" no tiene categoría definida'), nl
    ).

verificar_categorias :-
    write('=== VERIFICACIÓN DE CATEGORÍAS ==='), nl, nl,
    verificar_categoria(manzana),
    verificar_categoria(agua),
    verificar_categoria(libro),
    verificar_categoria(perro),
    verificar_categoria(maestro),
    verificar_categoria(ciudad),
    verificar_categoria(pelicula),
    verificar_categoria(trabajo),
    nl.

/* ======================================================
   MENÚ PRINCIPAL
   ====================================================== */

menu_ejemplos :-
    write('========================================'), nl,
    write('MENÚ DE EJEMPLOS'), nl,
    write('========================================'), nl, nl,
    write('Comandos disponibles:'), nl, nl,
    write('ejemplos_rapidos.         - Ejemplos básicos'), nl,
    write('todos_los_ejemplos.       - Casos de uso por categoría'), nl,
    write('ejemplos_comida.          - Verbos de alimentación'), nl,
    write('ejemplos_conocimiento.    - Verbos de conocimiento'), nl,
    write('ejemplos_percepcion.      - Verbos de percepción'), nl,
    write('comparar_idiomas.         - Comparación ES-EN'), nl,
    write('mostrar_compatibilidades. - Tabla de compatibilidades'), nl,
    write('verificar_categorias.     - Ver categorías de objetos'), nl,
    nl.

% Mostrar menú al cargar
:- initialization(menu_ejemplos).

/* ======================================================
   PARA USAR ESTE ARCHIVO:
   
   ?- [ejemplos].
   ?- ejemplos_rapidos.
   ?- todos_los_ejemplos.
   ?- comparar_idiomas.
   ====================================================== */
