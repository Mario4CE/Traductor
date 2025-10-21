/* ======================================================
   Demostración Interactiva del Sistema Traductor
   ====================================================== */

:- use_module(base_datos).
:- use_module(bnf_es).
:- use_module(bnf_en).

/* ======================================================
   EJEMPLOS INTERACTIVOS
   ====================================================== */

% Analizar oración en español
analizar_es(Oracion) :-
    write('Analizando: '), write(Oracion), nl,
    (   phrase(oracion_es(S, P), Oracion)
    ->  write('✓ VÁLIDA'), nl,
        write('  Sujeto: '), write(S), nl,
        write('  Predicado: '), write(P), nl
    ;   write('✗ INVÁLIDA'), nl,
        write('  Razones posibles:'), nl,
        write('  - Concordancia incorrecta (artículo-sustantivo)'), nl,
        write('  - Coherencia semántica (verbo-objeto incompatibles)'), nl,
        write('  - Estructura gramatical incorrecta'), nl
    ), nl.

% Analizar oración en inglés
analizar_en(Sentence) :-
    write('Analyzing: '), write(Sentence), nl,
    (   phrase(oracion_en(S, P), Sentence)
    ->  write('✓ VALID'), nl,
        write('  Subject: '), write(S), nl,
        write('  Predicate: '), write(P), nl
    ;   write('✗ INVALID'), nl,
        write('  Possible reasons:'), nl,
        write('  - Incorrect agreement (article-noun)'), nl,
        write('  - Semantic coherence (verb-object incompatible)'), nl,
        write('  - Incorrect grammatical structure'), nl
    ), nl.

/* ======================================================
   DEMOSTRACIÓN GUIADA
   ====================================================== */

demo :-
    write('========================================'), nl,
    write('DEMOSTRACIÓN DEL SISTEMA TRADUCTOR'), nl,
    write('========================================'), nl, nl,
    
    write('=== 1. ORACIONES VÁLIDAS ==='), nl, nl,
    
    write('--- Español ---'), nl,
    analizar_es([ella, come, la, manzana]),
    analizar_es([el, bebe, el, agua]),
    analizar_es([ella, lee, el, libro]),
    
    write('--- Inglés ---'), nl,
    analizar_en([she, eat, the, apple]),
    analizar_en([he, drink, the, water]),
    analizar_en([she, read, the, book]),
    
    nl,
    write('=== 2. ERRORES DE CONCORDANCIA ==='), nl, nl,
    
    write('--- Español ---'), nl,
    analizar_es([ella, come, la, manzanas]),  % Singular + plural
    analizar_es([el, bebe, las, agua]),       % Plural + singular
    
    write('--- Inglés ---'), nl,
    analizar_en([she, eat, a, apples]),       % Singular article + plural noun
    
    nl,
    write('=== 3. ERRORES SEMÁNTICOS ==='), nl, nl,
    
    write('--- Español ---'), nl,
    analizar_es([ella, come, el, libro]),      % Comer libro
    analizar_es([el, bebe, la, silla]),        % Beber silla
    analizar_es([ella, cocina, el, telefono]), % Cocinar teléfono
    
    write('--- Inglés ---'), nl,
    analizar_en([she, eat, the, book]),        % Eat book
    analizar_en([he, drink, the, chair]),      % Drink chair
    analizar_en([she, cook, the, phone]),      % Cook phone
    
    nl,
    write('========================================'), nl,
    write('DEMOSTRACIÓN COMPLETADA'), nl,
    write('========================================'), nl.

/* ======================================================
   PRUEBAS ESPECÍFICAS
   ====================================================== */

% Probar conjugación
test_conjugacion :-
    write('=== PRUEBAS DE CONJUGACIÓN ==='), nl, nl,
    
    write('✓ Correcto: Artículo singular + sustantivo singular'), nl,
    analizar_es([el, come, la, manzana]),
    
    write('✓ Correcto: Artículo plural + sustantivo plural'), nl,
    analizar_es([el, come, las, manzanas]),
    
    write('✗ Incorrecto: Artículo singular + sustantivo plural'), nl,
    analizar_es([el, come, la, manzanas]),
    
    write('✗ Incorrecto: Artículo plural + sustantivo singular'), nl,
    analizar_es([el, come, las, manzana]).

% Probar coherencia
test_coherencia :-
    write('=== PRUEBAS DE COHERENCIA SEMÁNTICA ==='), nl, nl,
    
    write('✓ Coherente: Comer comida'), nl,
    analizar_es([ella, come, la, manzana]),
    
    write('✗ Incoherente: Comer objeto no comestible'), nl,
    analizar_es([ella, come, el, libro]),
    
    write('✓ Coherente: Beber bebida'), nl,
    analizar_es([el, bebe, el, agua]),
    
    write('✗ Incoherente: Beber objeto no bebible'), nl,
    analizar_es([el, bebe, el, libro]),
    
    write('✓ Coherente: Leer objeto legible'), nl,
    analizar_es([ella, lee, el, libro]),
    
    write('✓ Coherente: Cocinar comida'), nl,
    analizar_es([el, cocina, el, pan]),
    
    write('✗ Incoherente: Cocinar objeto no comestible'), nl,
    analizar_es([el, cocina, la, computadora]).

% Listar combinaciones válidas
listar_validas :-
    write('=== COMBINACIONES VERBO-OBJETO VÁLIDAS ==='), nl, nl,
    
    write('COMER (eat): comida, bebida'), nl,
    write('  Ejemplos: manzana, pan, agua'), nl, nl,
    
    write('BEBER (drink): bebida'), nl,
    write('  Ejemplos: agua'), nl, nl,
    
    write('LEER (read): objeto_inanimado, entretenimiento'), nl,
    write('  Ejemplos: libro, pelicula'), nl, nl,
    
    write('COCINAR (cook): comida'), nl,
    write('  Ejemplos: manzana, pan'), nl, nl,
    
    write('VER (see): persona, animal, objeto_inanimado, lugar, entretenimiento'), nl,
    write('  Ejemplos: maestro, perro, libro, ciudad, pelicula'), nl, nl,
    
    write('TENER/QUERER/NECESITAR (have/want/need): casi todos'), nl,
    write('  Ejemplos: libro, perro, amigo, trabajo'), nl.

/* ======================================================
   AYUDA PARA EL USUARIO
   ====================================================== */

ayuda :-
    write('========================================'), nl,
    write('COMANDOS DISPONIBLES'), nl,
    write('========================================'), nl, nl,
    
    write('demo.                    - Ejecuta demostración completa'), nl,
    write('test_conjugacion.        - Prueba validación de concordancia'), nl,
    write('test_coherencia.         - Prueba validación semántica'), nl,
    write('listar_validas.          - Lista combinaciones válidas'), nl, nl,
    
    write('analizar_es([lista]).    - Analiza oración en español'), nl,
    write('  Ejemplo: analizar_es([ella, come, la, manzana]).'), nl, nl,
    
    write('analizar_en([list]).     - Analiza oración en inglés'), nl,
    write('  Ejemplo: analizar_en([she, eat, the, apple]).'), nl, nl,
    
    write('ayuda.                   - Muestra esta ayuda'), nl,
    write('========================================'), nl.

% Mostrar ayuda al cargar el archivo
:- initialization(ayuda).

/* Para usar este archivo:
   1. Cargar: ?- [demo].
   2. Ejecutar: ?- demo.
   
   O probar comandos individuales:
   ?- analizar_es([ella, come, la, manzana]).
   ?- test_conjugacion.
   ?- test_coherencia.
*/
