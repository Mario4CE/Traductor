/* ======================================================
   Gramática para traducción Español ↔ Inglés
   ====================================================== */

:- module(gramatica, [
    oracion_es/3, % Gramática en Español
    oracion_en/3  % Gramática en Inglés
]).

/* ==========================
   Gramática en Español
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
   Gramática en Inglés
   ========================== */

oracion_en(sujeto(S), predicado(P)) --> sujeto_en(S), predicado_en(P).

sujeto_en(pronombre(Pron)) -->
    [Palabra],
    { pronombre(_, _, Palabra), Pron = Palabra }.

sujeto_en(articulo_sustantivo(Art, Sust)) -->
    articulo_en(Art),
    sustantivo_en(Sust).

sujeto_en(articulo_sustantivo_plural(Art, SustP)) -->
    articulo_en(Art),
    sustantivo_plural_en(SustP).

predicado_en(verbo_objeto(Verbo, Comp)) -->
    verbo_en(Verbo),
    complemento_en(Comp).

complemento_en(articulo_sustantivo(Art, Sust)) -->
    articulo_en(Art),
    sustantivo_en(Sust).

complemento_en(articulo_sustantivo_plural(Art, SustP)) -->
    articulo_en(Art),
    sustantivo_plural_en(SustP).

complemento_en(sustantivo(Sust)) -->
    sustantivo_en(Sust).

complemento_en(sustantivo_plural(SustP)) -->
    sustantivo_plural_en(SustP).

articulo_en(Art) -->
    [Palabra],
    { articulo(_, _, _, Palabra), Art = Palabra }.

sustantivo_en(Sust) -->
    [Palabra],
    { objeto(singular, Palabra, _), Sust = Palabra }.

sustantivo_plural_en(SustP) -->
    [Palabra],
    { objeto(plural, Palabra, _), SustP = Palabra }.

verbo_en(Verbo) -->
    [Palabra],
    { verbo(Palabra, _), Verbo = Palabra }.
