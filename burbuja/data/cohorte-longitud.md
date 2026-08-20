# Cohorte para medir el efecto de acortar los posts

Registrada el 2026-08-21. Para volver a medir los thanks de estos mensajes
**a la misma edad** (sugerencia: a los 7 días de cada uno), no ahora.

El microdato de cada mensaje da el total acumulado:
`article[data-content=post-ID] .message-microdata meta[itemprop=userInteractionCount]`

## Antes (formato largo, media 681 palabras)

58707216 58707218 58707233 58707276 58707289 58707303 58707323
58708520 58708538 58708558 58708579 58708591
58710888 58710908 58710920 58710931 58710950 58710964

Medidos el 20-08 con pocas horas de vida: media 1,17. **Ese número está
sesgado a la baja**, ver abajo.

## Después (formato corto, media 171 palabras)

58711677 58711684 58711689 58711693

## Por qué no vale medir recién publicado

Los thanks se acumulan con el tiempo. Medido el 21-08 sobre 15 mensajes:

    0-6 h    n=10    0,50 thanks/post
    6-18 h   n=5     1,00 thanks/post

O sea que la cifra dobla entre las primeras horas y el mismo día. Comparar un
lote recién publicado contra otro más viejo mide la edad, no el formato.

## Por qué el ratio global no sirve

Perfil a 21-08-2026: 3.372 reputación / 1.630 mensajes = 2,07.

Es una media de toda la vida de la cuenta. Para moverla una décima haciendo 4
thanks por mensaje harían falta unos 90 mensajes nuevos. No detecta nada en
semanas.
