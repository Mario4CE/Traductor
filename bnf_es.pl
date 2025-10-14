/* ======================================================
   Gramática - Español (ARIDADES EXACTAS PARA TESTS)
   ====================================================== */

:- module(bnf_es, [
    oracion_es//2,
    pregunta_es//3,
    pregunta_wh_es//4,   % Para test _1 (4 args)
    pregunta_wh_es//3,   % Para test _2 y verifica (3 args)
    exclamacion_es//3
]).

:- use_module(base_datos).

/* ==========================
   === Componentes Base ===
   ========================== */

oracion_es(sujeto(Numero, Persona, Suj), predicado(verbo_objeto(Verbo, Comp))) -->
    sujeto_es(Numero, Persona, Suj), predicado_es(Numero, Persona, Verbo, Comp), !.

sujeto_es(Numero, Persona, Pron) --> [Palabra], { pronombre(Numero, Persona, _, Palabra, _), Pron = Palabra }, !.
sujeto_es(singular, tercera, Sust) --> articulo_es(_), sustantivo_es(Sust), !.
sujeto_es(plural, tercera, SustP) --> articulo_es(_), sustantivo_plural_es(SustP), !.

predicado_es(Numero, Persona, Verbo, Comp) --> verbo_es(Numero, Persona, Verbo), complemento_es(Comp), !.

complemento_es(articulo_sustantivo(Art, Sust)) --> articulo_es(Art), sustantivo_es(Sust), !.
complemento_es(sustantivo_plural(SustP)) --> sustantivo_plural_es(SustP), !.

articulo_es(Art) --> [Palabra], { articulo(_, _, Palabra, _), Art = Palabra }, !.
sustantivo_es(Sust) --> [Palabra], { objeto(singular, _, Palabra), Sust = Palabra }, !.
sustantivo_plural_es(SustP) --> [Palabra], { objeto(plural, _, Palabra), SustP = Palabra }, !.
verbo_es(Numero, Persona, Verbo) --> [Palabra], { verbo(_, Numero, Persona, Palabra), Verbo = Palabra }, !.

/* ==========================
   === PREGUNTAS SÍ/NO (3 args - Tests existentes) ===
   ========================== */

pregunta_es(sujeto(Num, Per, S), Verbo, Comp) -->
    ["¿"], sujeto_es(Num, Per, S), verbo_es(Num, Per, Verbo),
    complemento_es(Comp), [?], !.

pregunta_es(Verbo, sujeto(Num, Per, S), Comp) -->
    ["¿"], verbo_es(Num, Per, Verbo), sujeto_es(Num, Per, S),
    complemento_es(Comp), [?], !.

/* ==========================
   === PREGUNTAS WH (Multi-aridad para tests) ===
   ========================== */

% //4 para test _1: ¿Qué come ella? (Wh, Verbo, S, Comp - Comp opcional)
pregunta_wh_es(WhWord, Verbo, S, Comp) -->
    ["¿"], [WhWord], { interrogativo(WhWord, _) },
    verbo_es(singular, tercera, Verbo),
    sujeto_es(singular, tercera, S),
    ( complemento_es(Comp) ; [] ), [?], !.

% //3 para test _2 y verifica: ¿Quién come manzanas? (Wh, Verbo, Comp)
pregunta_wh_es(WhWord, Verbo, Comp) -->
    ["¿"], [WhWord], { interrogativo(WhWord, _) },
    verbo_es(singular, tercera, Verbo),
    complemento_es(Comp), [?], !.

/* ==========================
   === Exclamaciones ===
   ========================== */

exclamacion_es(sujeto(Numero, Persona, S), predicado(verbo_objeto(Verbo, Comp)), exclamacion) -->
    ["¡"], sujeto_es(Numero, Persona, S), verbo_es(Numero, Persona, Verbo),
    complemento_es(Comp), ["!"], !.