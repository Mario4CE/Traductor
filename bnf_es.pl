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

oracion_es(sujeto(Numero, Persona, Suj), predicado(verbo_objeto(Verbo, Comp))) -->
    sujeto_es(Numero, Persona, Suj),
    predicado_es(Numero, Persona, Verbo, Comp), !.

/* ========= Sujeto ========= */

% Pronombres personales: yo, tú, él...
sujeto_es(Numero, Persona, Pron) -->
    [Palabra],
    { pronombre(Numero, Persona, _, Palabra, _),
      Pron = Palabra }, !.

% Artículo + sustantivo singular
sujeto_es(singular, tercera, Sust) -->
    articulo_es(Art),
    sustantivo_es(Sust),
    { Art \= el }, !. % Evita conflicto con pronombre 'el'

% Artículo + sustantivo plural
sujeto_es(plural, tercera, SustP) -->
    articulo_es(_),
    sustantivo_plural_es(SustP), !.

/* ========= Predicado ========= */

predicado_es(Numero, Persona, Verbo, Comp) -->
    verbo_es(Numero, Persona, Verbo),
    complemento_es(Comp), !.

/* ========= Complementos ========= */

complemento_es(articulo_sustantivo(Art, Sust)) -->
    articulo_es(Art),
    sustantivo_es(Sust), !.

complemento_es(sustantivo_plural(SustP)) -->
    sustantivo_plural_es(SustP), !.

/* ========= Artículos / Sustantivos ========= */

articulo_es(Art) -->
    [Palabra],
    { articulo(_, _, Palabra, _), Art = Palabra }, !.

sustantivo_es(Sust) -->
    [Palabra],
    { objeto(singular, _, Palabra), Sust = Palabra }, !.

sustantivo_plural_es(SustP) -->
    [Palabra],
    { objeto(plural, _, Palabra), SustP = Palabra }, !.

/* ========= Verbos ========= */

verbo_es(Numero, Persona, Verbo) -->
    [Palabra],
    { verbo(_, Numero, Persona, Palabra), Verbo = Palabra }, !.

/* ==========================
   === Preguntas ===
   ========================== */

% Ej: ¿Ella come manzanas?
pregunta_es(sujeto(Numero, Persona, S), Verbo, sustantivo_plural(Complemento)) -->
    ["¿"],
    sujeto_es(Numero, Persona, S),
    verbo_es(Numero, Persona, Verbo),
    sustantivo_plural_es(Complemento),
    [?], !.

% Ej: ¿Comes tú manzanas?
pregunta_es(Verbo, sujeto(Numero, Persona, S), sustantivo_plural(Complemento)) -->
    ["¿"],
    verbo_es(Numero, Persona, Verbo),
    sujeto_es(Numero, Persona, S),
    sustantivo_plural_es(Complemento),
    [?], !.

/* ==========================
   === Exclamaciones ===
   ========================== */

% Ej: ¡Ella come manzanas!
exclamacion_es(sujeto(Numero, Persona, S), predicado(verbo_objeto(Verbo, sustantivo_plural(Comp))), exclamacion) -->
    ["¡"],
    sujeto_es(Numero, Persona, S),
    verbo_es(Numero, Persona, Verbo),
    sustantivo_plural_es(Comp),
    ["!"], !. % Los de exclamacion van con comillas ya que prolog no reconoce ¡ y ! lo toma como corte