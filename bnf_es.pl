/* ======================================================
   Gramática - Español
   Soporta:
   - Oraciones declarativas
   - Preguntas (¿...? con y sin inversión)
   - Exclamaciones (¡...!)
   ====================================================== */

:- module(bnf_es, [
    oracion_es//2,
    pregunta_es//3,
    exclamacion_es//3
]).

:- use_module(base_datos).

/* ==========================
   === Oraciones declarativas ===
   ========================== */

oracion_es(sujeto(S), predicado(P)) --> sujeto_es(S), predicado_es(P).

sujeto_es(pronombre(Pron)) --> 
    [Palabra],
    { pronombre(_, Pron, Palabra) }.

sujeto_es(articulo_sustantivo(Art, Sust)) -->
    articulo_es(Art),
    sustantivo_es(Sust).

sujeto_es(articulo_sustantivo_plural(Art, SustP)) -->
    articulo_es(Art),
    sustantivo_plural_es(SustP).

predicado_es(verbo_objeto(Verbo, Comp)) -->
    verbo_es(Verbo),
    complemento_es(Comp).

complemento_es(articulo_sustantivo(Art, Sust)) -->
    articulo_es(Art),
    sustantivo_es(Sust).

complemento_es(articulo_sustantivo_plural(Art, SustP)) -->
    articulo_es(Art),
    sustantivo_plural_es(SustP).

complemento_es(sustantivo(Sust)) -->
    sustantivo_es(Sust).

complemento_es(sustantivo_plural(SustP)) -->
    sustantivo_plural_es(SustP).

articulo_es(Art) -->
    [Palabra],
    { articulo(_, _, Palabra, _), Art = Palabra }.

sustantivo_es(Sust) -->
    [Palabra],
    { objeto(singular, _, Palabra), Sust = Palabra }.

sustantivo_plural_es(SustP) -->
    [Palabra],
    { objeto(plural, _, Palabra), SustP = Palabra }.

verbo_es(Verbo) -->
    [Palabra],
    { verbo(_, Palabra), Verbo = Palabra }.

/* ==========================
   === Preguntas ===
   ========================== */

% Ej: ¿Ella come manzanas?
pregunta_es(sujeto(S), verbo(V), complemento(C)) -->
    ["¿"],
    sujeto_es(S),
    verbo_es(V),
    complemento_es(C),
    [?].

% Ej: ¿Come ella manzanas?
pregunta_es(verbo(V), sujeto(S), complemento(C)) -->
    ["¿"],
    verbo_es(V),
    sujeto_es(S),
    complemento_es(C),
    [?].

/* ==========================
   === Exclamaciones ===
   ========================== */

% Ej: ¡Ella come manzanas!
exclamacion_es(sujeto(S), predicado(P), exclamacion) -->
    ["¡"],
    oracion_es(S, P),
    [!].

