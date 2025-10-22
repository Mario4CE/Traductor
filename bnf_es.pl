/* ======================================================
   Gramática - Español 
   ====================================================== */

:- module(bnf_es, [
    oracion_es//2,
    pregunta_es//3,
    pregunta_wh_es//4,   
    pregunta_wh_es//3,   
    exclamacion_es//3
]).

:- use_module(base_datos).

/* ==========================
   === Componentes Base ===
   ========================== */

oracion_es(sujeto(Numero, Persona, Suj), predicado(verbo_objeto(Verbo, Comp))) -->
    sujeto_es(Numero, Persona, Suj), 
    predicado_es(Numero, Persona, Verbo, Comp), 
    { validar_coherencia_semantica(Verbo, Comp) }, !.

sujeto_es(Numero, Persona, Pron) --> 
    [Palabra], 
    { pronombre(Numero, Persona, _, Palabra, _), Pron = Palabra }, !.

sujeto_es(singular, tercera, Sust) --> 
    articulo_es(singular), 
    sustantivo_es(Sust), !.

sujeto_es(plural, tercera, SustP) --> 
    articulo_es(plural), 
    sustantivo_plural_es(SustP), !.

predicado_es(Numero, Persona, Verbo, Comp) --> 
    verbo_es(Numero, Persona, Verbo), 
    complemento_es(Comp), !.

complemento_es(articulo_sustantivo(Sust)) --> 
    articulo_es(singular), 
    sustantivo_es(Sust), !.

complemento_es(articulo_sustantivo(SustP)) --> 
    articulo_es(plural), 
    sustantivo_plural_es(SustP), !.

complemento_es(sustantivo_plural(SustP)) --> 
    sustantivo_plural_es(SustP), !.

articulo_es(Num) --> 
    [Palabra], 
    { articulo(_, Num, Palabra, _) }, !.

sustantivo_es(Sust) --> 
    [Palabra], 
    { objeto(singular, _, Palabra), Sust = Palabra }, !.

sustantivo_plural_es(SustP) --> 
    [Palabra], 
    { objeto(plural, _, Palabra), SustP = Palabra }, !.

verbo_es(Numero, Persona, Verbo) --> 
    [Palabra], 
    { verbo(_, Numero, Persona, Palabra), Verbo = Palabra }, !.

/* ============================
   === Validación Semántica ===
   ============================ */

validar_coherencia_semantica(Verbo, Comp) :-
    extraer_objeto_complemento(Comp, Objeto),
    obtener_raiz_verbo(Verbo, RaizVerbo),
    % Si el objeto tiene categoría, debe ser compatible
    \+ (categoria_semantica(Objeto, Categoria),
        \+ compatible_verbo_objeto(RaizVerbo, Categoria)).

extraer_objeto_complemento(articulo_sustantivo(Sust), Sust).
extraer_objeto_complemento(sustantivo_plural(Sust), Sust).

obtener_raiz_verbo(Verbo, Raiz) :-
    verbo(Raiz, _, _, Verbo), !.
obtener_raiz_verbo(Verbo, Verbo).

/* ==========================
   === PREGUNTAS SÍ/NO (3 args - Tests existentes) ===
   ========================== */

pregunta_es(sujeto(Num, Per, S), Verbo, Comp) -->
    ["¿"], sujeto_es(Num, Per, S), verbo_es(Num, Per, Verbo),
    complemento_es(Comp), 
    { validar_coherencia_semantica(Verbo, Comp) },
    [?], !.

pregunta_es(Verbo, sujeto(Num, Per, S), Comp) -->
    ["¿"], verbo_es(Num, Per, Verbo), sujeto_es(Num, Per, S),
    complemento_es(Comp), 
    { validar_coherencia_semantica(Verbo, Comp) },
    [?], !.

/* ==========================
   === PREGUNTAS WH ===
   ========================== */

% //4: ¿Qué come ella manzanas? (Wh, V, S, C)
pregunta_wh_es(WhWord, Verbo, Sujeto, Comp) -->
    ["¿"], [WhWord], { interrogativo(WhWord, _) },
    verbo_es(singular, tercera, Verbo),
    sujeto_es(singular, tercera, Sujeto),
    complemento_es(Comp), 
    { validar_coherencia_semantica(Verbo, Comp) },
    [?], !.

% //3: ¿Quién come manzanas? (Wh, V, Comp)
pregunta_wh_es(WhWord, Verbo, Comp) -->
    ["¿"], [WhWord], { interrogativo(WhWord, _) },
    verbo_es(singular, tercera, Verbo),
    complemento_es(Comp), 
    { validar_coherencia_semantica(Verbo, Comp) },
    [?], !.

% //3: ¿Qué come ella? (Wh, V, Sujeto)
pregunta_wh_es(WhWord, Verbo, Sujeto) -->
    ["¿"], [WhWord], { interrogativo(WhWord, _) },
    verbo_es(singular, tercera, Verbo),
    sujeto_es(singular, tercera, Sujeto),
    [?], !.


/* ==========================
   === Exclamaciones ===
   ========================== */

exclamacion_es(sujeto(Numero, Persona, S), predicado(verbo_objeto(Verbo, Comp)), exclamacion) -->
    ["¡"], sujeto_es(Numero, Persona, S), verbo_es(Numero, Persona, Verbo),
    complemento_es(Comp), 
    { validar_coherencia_semantica(Verbo, Comp) },
    ["!"], !.