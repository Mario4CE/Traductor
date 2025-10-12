/* ======================================================
   Gramática - Inglés
   Soporta:
   - Oraciones declarativas
   - Preguntas con do/does
   - Exclamaciones
   ====================================================== */

:- module(bnf_en, [
    oracion_en//2,
    pregunta_en//4,
    exclamacion_en//3
]).

:- use_module(base_datos).

/* ==========================
   === Oraciones declarativas ===
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

/* ==========================
   === Preguntas ===
   ========================== */

% Ej: Does he eat apples ?
pregunta_en(auxiliar(Aux), sujeto(S), verbo(V), complemento(C)) -->
    [Aux],
    { member(Aux, [do, does]) },
    sujeto_en(S),
    verbo_en(V),
    complemento_en(C),
    [?].

/* ==========================
   === Exclamaciones ===
   ========================== */

% Ej: She eats apples !
exclamacion_en(sujeto(S), predicado(P), exclamacion) -->
    oracion_en(S, P),
    [!].
