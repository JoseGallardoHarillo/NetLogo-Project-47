extensions [ CSV ]

__includes ["MCTS.nls" "A-star.nls"  "ID3_C45.nls" "LayoutSpace.nls"  "DF.nls" "GeneticAlgorithm.nls"] ;librería de montecarlo

globals[
  play? ;Indicará si la partida ha terminado con el agente 47 ha muerto
  level ;Sin uso
  dead? ;Sin uso
  estatuaRecuperada? ;Indica si la estatua ha sido recuperada
  keys ;Indica las llaves disponibles actualmente
  keydesp ;Indica si se ha obtenido o no la llave del despacho
  action ;Usado para poder direccionar al agente/caminar
  eliminado? ;Indica si el objetivo ha sido eliminado
  monte? ;Indica si los patrullas te han detectado
  tam ;Sin uso
  tamA ;Tamaño del mundo para A*
  played? ;Indica si has jugado o no (se utiliza una vez hayas sido detectado)
  ahogado ;Usado en Montecarlo
  score ;Sin uso
  victoria ;Indica si la partida puede ser terminada con el contrato cumplido
  cont1 ;Contadores para los patrones de los patrullas
  cont2
  cont3
  cont4
  ;Variables globales de ID3
  decision-tree
  DataFrame
  ;Variables globales de score
  scoreIdeal
  contadorPasos
]

patches-own [
  value   ; Guarda el valor de cada patch para la matriz que representa el mapa (1: paredes, 2:agente 47,
  ;6-9:patrullas, 47: salida, 4: objetivo)
  id ;Identifica los patches de ineres para luego ser usados en AG
]

breed [ objetivos objetivo ]

breed [ agentes agente ]

breed [patrullas patrulla]

turtles-own[
  valueA ;Valor que tendrán las propias tortugas
]

to draw-cells ;Método que he usado para dibujar el mapa, usalo para poner más paredes o quitarlas si quieres (pared: gray -3; suelo: gray + 3, entre otros)
  while [mouse-down?]
    [ ask patch mouse-xcor mouse-ycor
      [set pcolor fgcolor]
      display ]
end

;NOTA: El juego está ahora mismo programado de forma que los patrullas te querrán pillar mediante A*,
;pero descomentando las lineas que se te indicarán más abajo podrás probar el montecarlos aunque no funcione bien

to new  ;Creación del mapa y sus componentes

  ca

  import-world "nivel1"

  set tamA int(world-width / 2)
  set victoria false

  ;Indicadores de items objetidos
  set estatuaRecuperada? false
  set keydesp false
  set estatuaRecuperada? false
  set eliminado? false

  ;Variable que se usará para diferenciar el turno del jugador y la máquina en montecarlo/A*
  set played? false

  ;Variable que activará el montecarlo/A*
  set monte? false

  ;Contadores para las rutas de los patrullas
  set cont1 0
  set cont2 0
  set cont3 0
  set cont4 0

  ;El montecarlo en caso de querer activarse no cuenta con los items así que se debe descomentar esta línea
  ;ask patches with [(pcolor != cyan) and (pcolor != (gray + 3)) and (pcolor != (gray - 3))] [set pcolor (gray + 3)]

  ;generador de patrullas
  ask patrullas[
    set color orange
    set heading 90]

  colocaPatrullas

  ;Asignacion de valores a tortugas

  ask agentes [set valueA 2]
  ask objetivos [set valueA 4]
  ask patrullas [set valueA (count patrullas + 5)]

  let i count patrullas
  while[i > 0][
    ask one-of patrullas with [valueA = (count patrullas + 5)][set valueA (i + 5)]
    set i i - 1
  ]



  ;Asignación de valores a patches
  tableros

  ;Asignación de ids
  asignaId

  ;AG para sacar un camino optimo e incluso mínimo para recorrer los distintos puntos de inerés
  AI:Initial-Population 50
  AI:ExternalUpdate

  let ag [content] of AI:GeneticAlgorithm 100 50 0 0.5 ;Saca el cromosoma del mejor
  set ag fput 6 ag ;Añadimos al cromosoma los dos puntos de interés que tenían un orden claro y por ello no han sido clasificados, el inicio y el fin
  set ag lput 7 ag

  let nPasos length(ag)
  set i 0

  set scoreIdeal 0

  while [i < nPasos - 1][ ;En el bucle se va sacando los caminos mínimos en el orden que han sido asignados a los puntos de interés y saca el scoreIdeal con el número total de pasos necesarios para llegar desde el inicio hasta el fin

    let ori item i ag
    let dest item (i + 1) ag

    let corx [pxcor] of patches with [id = ori]
    let cory [pycor] of patches with [id = ori]
    let s0 (list (first corx) (first cory))

    let corxD [pxcor] of patches with [id = dest]
    let coryD [pycor] of patches with [id = dest]
    let goal (list (first corxD) (first coryD))

    let actions CalculaScore s0 goal
    set scoreIdeal scoreIdeal + length(actions)
    set i i + 1
  ]

  reset-ticks
end


to asignaId

  ask one-of patches with [pcolor = yellow] [set id 1]
  ask one-of patches with [pcolor = yellow and id = 0] [set id 2]
  ask patches with [pcolor = lime] [set id 3]
  ask patches with [pcolor = magenta][set id 4]
  ask patches with [value = 4][set id 5]

  ;id de patches fuera de AG
  ask patches with [value = 2][set id 6]
  ask patches with [pcolor = cyan][set id 7]
end

to-report CalculaScore[s0 goal] ;El algoritmo de A*, la función devuelve la lista don la ruta más corta
  let actions false
  let path (A* s0 goal false false) ;Función de A*
  if path != false [
    highlight-path path
    set actions (map [ s -> first [rule] of s ] path)
    ;print (word "Actions to sort it: " actions) ;Si quieres ver los caminos para imitarlos descomenta
  ]
  report actions
end


to tableros ;Asignación de valores a patches
  ask patches[

    (ifelse
      ((pcolor = (gray - 3)) or (pcolor = (brown - 2)) or (pcolor = (pink)))[set value 1]
      (pcolor = cyan) [set value 47]
      [set value 0])
  ] ;Pone los valores correcpondientes a los patches


  ;Lo mismo pero con los patches que llevan determinados agentes
  ask one-of agentes[ask patch xcor ycor [set value [valueA] of myself]]
  ask one-of objetivos[ask patch xcor ycor [set value [valueA] of myself]]

  let i count patrullas
  while[i > 0][
    ask patrullas with [valueA = i + 5][ask patch xcor ycor [set value [valueA] of myself]]
    set i i - 1
  ]

end

to colocaPatrullas

  create-patrullas 4
  ask patrullas [set color orange]
  ask one-of patrullas [set color green]
  ask patrullas with [color = green] [set xcor 2 set ycor -4 set heading 0]
  ask one-of patrullas with [color = orange] [set color violet]
  ask patrullas with [color = violet] [set xcor -4 set ycor 0 set heading 0]
  ask one-of patrullas with [color = orange] [set color yellow]
  ask patrullas with [color = yellow] [set xcor 2 set ycor 0 set heading 90]
  ask patrullas with [color = orange] [set xcor -2 set ycor 2 set heading 270]

end

to compruebaZona[v] ;Función que hace comprobar a cada patrulla si esta el agente delante de ellos (a dos cuadros de distancias como mucho te puede ver)

  ;si alguno de los v es 2, Y además el valor no tiene un gris delante, entonces es true

  ask patrullas with [[value] of patch-ahead 0 = v][
    let v1 false
    let v2 false
    let v3 false

    if(patch-ahead 1 != NOBODY)[set v1 [value] of patch-ahead 1]
    if(patch-ahead 2 != NOBODY)[set v2 [value] of patch-ahead 2]
    if(patch-ahead 3 != NOBODY)[set v3 [value] of patch-ahead 3]

    if((v1 = 2) or ((v2 = 2) and (v1 = 0)) or ((v3 = 2) and (v1 = 0) and (v2 = 0))) [set monte? true]

  ]

end

to play
  (ifelse
    (play? = false)[ ask agentes[die] user-message "Contrato fallido :(" stop]
    (victoria = true)[

      let clasificacion false
      let res (contadorPasos - scoreIdeal)
      (ifelse
        (contadorPasos >= scoreIdeal and res <= 10)[set res (10 - res)] ;Penalizació a cuanto más largo sea la ruta tomada
        (contadorPasos < scoreIdeal) [set res 10] ;Mejor incluso que el ideal
        [set res 0]
      )

      if(monte? = true)[set res 0] ;Si te detectan te penaliza garrafalmente (Si se consigue cumplir el contrato sin morir antes claro)
      if(estatuaRecuperada? = true)[set res res + 5] ;Compensación al conseguir la estatua

      (ifelse ;Clasificaciones segun el score
        (res = 0) [set clasificacion "Patoso"]
        (res < 5) [set clasificacion "Principiante"]
        (res = 5) [set clasificacion "Promesa"]
        (res < 10) [set clasificacion "Fantasma"]
        (res = 10) [set clasificacion "Asesino Silencioso"]
      )

      ask agentes[die] user-message "Contrato cumplido!!!!" user-message (word "Score: "res "\n"clasificacion)
      stop
    ]
  [set play? true])
  let corxA [xcor] of agentes
  let coryA [ycor] of agentes

  let PosA (list (first corxA) (first coryA))

  ask patrullas [if((xcor = first PosA) and (ycor = last PosA))[ set played? false set play? false]] ;Esta linea permite a los patrullas matar al agente al tocarlos

  tableros

  let i count patrullas

  ifelse(monte? = false)[ ;monte? es una variable que activará la alerta de que te han detectado y por ende la activación de montecarlo (o A* en la modificación)

    while[i > 0][
      compruebaZona (i + 5)
      set i i - 1
    ]

  ]

  [
    ask patrullas [set color red - 2]


    ;wait .1

    ;jugador automatico
    if (played? = true) [

      set i 1


      ;MONTECARLOS en los NPC, PARA PROBAR DESCOMENTA Y COMENTA EL A*
      ;let m MCTS:UCT (list (board-to-state) 3) Max_iterations ;devuelve la direccion indicada en grados y el patrullas que debe mover la máquina                                                ;show m
      ;ask patrullas with [value = (first m)][
      ;  set heading (last m)
      ;  fd 1
      ;]

      ;if MCTS:get-result (list (board-to-state) 2) 2 = 0 [
       ; user-message "Contrato fallido, te han matado"
       ; stop
      ;]

      ;Cordenadas del agente
      let corx [xcor] of one-of agentes
      let cory [ycor] of one-of agentes
      let s0 (list corx cory)


      ;Cada patrulla hará un A* al agente y dará el primer paso de sus acciones
      set i count patrullas
      while[i > 0][
        ;vamos actualizando el tablero
        tableros

        ;Coordenadas de cada patrulla
        let corxP [xcor] of patrullas with [valueA = (i + 5)]
        let coryP [ycor] of patrullas with [valueA = (i + 5)]
        let s0P (list (first corxP) (first coryP))

        let actionsP CalculaScore s0P s0

        ask patrullas with [valueA = (i + 5)][
          set heading (first actionsP)
          if(([value] of patch-ahead 1 = 0) or ([value] of patch-ahead 1 = 2))[fd 1]
        ]

        set i i - 1
      ]

      set played? false ;La máquina una vez llegada aquí ya ha jugado su turno, ahora es tu turno
    ]
  ]

  mover-agente ;Función encargada del tratamiento de movimientos

  display
end


to mover-agente  ;; Observer Procedure
  if (action != 0)
    [ if (action = 1)
        [ izquierda ]
      if (action = 2)
        [ derecha ]
      if (action = 3)
        [ abajo ]
      if (action = 4)
        [ arriba ]
      if (action = 5)
        [ caminar ]
      set action 0
    ]
end

to caminar


  ;Siempre y cuando no coincida cualquiera de los siguientes casos se evaluará el siguiente patch al que pasará en teoría
  ask agentes [
    ifelse(((ycor != max-pycor) or (heading != 0)) and ((ycor != min-pycor) or (heading != 180)) and ((xcor != max-pycor) or (heading != 90)) and ((xcor != min-pycor) or (heading != 270)))[
      revisaItem
    ]
    [
    set played? true
    ]
  ]

  ;Necesario por si se quiere poner a los patrullas a caminar aleatoriamente

 ; let op [0 1] ;0: dar un paso, 1: rotar

  ;let ldir [0 90 180 270] ;direcciones a las que puede rotar el patrulla


  if (monte? = false)[
    ;código para que vayan random por el mapa:

    ;ask patrullas[
    ;  let opRandom one-of op
    ;  let dirRandom one-of ldir


     ; ifelse(opRandom = 0)[set heading dirRandom]
     ; [
      ;  if(((ycor != max-pycor) or (heading != 0)) and ((ycor != min-pycor) or (heading != 180)) and ((xcor != max-pycor) or (heading != 90)) and ((xcor != min-pycor) or (heading != 270)) and ([pcolor] of patch-ahead 1 = (gray + 3)))[
       ;   fd 1
      ;  ]
      ;]
  ;]

    ;código con los movimientos planeados

    planPa1
    planPa2
    planPa3
    planPa4
    set contadorPasos contadorPasos + 1
  ]

  ;set opRandom one-of op
  ;set dirRandom one-of ldir


end

to planPa1

  ask patrullas with [color = orange][
    (ifelse
      (cont1 = 6) [set heading 0]
      (cont1 = 2) [set heading 90]
      (cont1 = 10) [set heading 270]
      (cont1 = 8) [set heading 180]
      [fd 1]
    )

    ifelse(cont1 = 11)[set cont1 0][set cont1 cont1 + 1]
  ]
end

to planPa2

  ask patrullas with [color = violet][
    (ifelse
      ((cont2 = 0) or (cont2 = 1)) [set heading 90]
      ((cont2 = 2) or (cont2 = 3)) [set heading 0]
    )

    ifelse(cont2 = 3)[set cont2 0][set cont2 cont2 + 1]
  ]
end

to planPa3

  ask patrullas with [color = yellow][
    (ifelse
      (cont3 = 1) [set heading 180]
      (cont3 = 5) [set heading 270]
      (cont3 = 8) [set heading 0]
      (cont3 = 12) [set heading 90]
      [fd 1]
    )

    ifelse(cont3 = 13)[set cont3 0][set cont3 cont3 + 1]
  ]
end

to planPa4

  ask patrullas with [color = green][
    (ifelse
      (cont4 = 0) [set heading 90]
      ((cont4 = 1) or (cont4 = 3)) [set heading 0]
      (cont4 = 2) [set heading 270]
    )

    ifelse(cont4 = 3)[set cont4 0][set cont4 cont4 + 1]
  ]
end


;Las siguientes cuatro funciones servirán para que puedas orientar al agente 47 para tras haber desidido la dirección dar un paso
to arriba
  ask agentes
    [ set heading 0
    ]
end

to abajo
  ask agentes
    [ set heading 180
    ]
end

to derecha
  ask agentes
    [ set heading 90
    ]
end

to izquierda
  ask agentes
    [ set heading 270
    ]
end

to revisaItem

  ask agentes[

    (if-else

      ([pcolor] of patch-ahead 1 = (gray + 3)) [fd 1] ;Si es un espacio libre/suelo damos un paso
      ([pcolor] of patch-ahead 1 = yellow) ;Si es una llave la cogemos e incrementamos el llavero
      [
        fd 1
        ask patch-ahead 0[
          set pcolor gray + 3
          set keys keys + 1
          output-print (word "+1 llave obtenida")
        ]
      ]
      (([pcolor] of patch-ahead 1 = (brown - 2)) and (keys > 0)) ;Si es una puerta siempre y cuando tengamos una llave podremos pasar por el patch indicado
      [
        fd 1
        ask patch-ahead 0[
          set pcolor gray + 3
          set keys keys - 1
          output-print (word "puerta abierta")
        ]
      ]
      ([pcolor] of patch-ahead 1 = magenta) ;Si es la llave del despacho la conseguimos y ponemos a true el flag que nos hará pasar al despacho del objetivo
      [
        fd 1
        ask patch-ahead 0[
          set pcolor gray + 3
          set keydesp true
          output-print (word "llave del despacho obtenida")
        ]
      ]

      (([pcolor] of patch-ahead 1 = pink) and keydesp = true) ;Si es la puerta del despacho y tenemos la llave del despacho podremos pasar

      [
        fd 1
        ask patch-ahead 0[
          set pcolor gray + 3
          output-print (word "a por el objetivo 47")
        ]
      ]

      ([pcolor] of patch-ahead 1 = lime)[ ;Si es la estatua la cogemos y conseguimos el plus de puntuación
        fd 1
        ask patch-ahead 0[
          set pcolor gray + 3
          set estatuaRecuperada? true
          output-print (word "estatua recuperada")
        ]
      ]

      ([pcolor] of patch-ahead 1 = cyan)[ ;Si es la salida siempre y cuando el objetivo haya sido eliminado pues la partida terminará

        fd 1
        if(eliminado? = true)[
          set victoria true
        ]

      ]
    )

    if((([ycor] of agentes) = ([ycor] of objetivos)) and (([xcor] of agentes) = ([xcor] of objetivos)) and (([heading] of agentes) = ([heading] of objetivos)))[ ;Si se ejecuta una muerte por la espalda al objetivo entonces la eliminación se habrá cumplido

      ask objetivos with [color = red][
        set color white
        output-print (word "objetivo eliminado, buen trabajo 47")
        set eliminado? true
      ]
    ]

  ]

  if(monte? = true)[
    set played? true
  ]

end

;zona Montecarlo

to-report MCTS:get-content [s]
  report first s
end

to-report MCTS:get-playerJustMoved [s]
  report last s
end

to-report MCTS:create-state [c p]
  report (list c p)
end

; Obtiene los posibles movimientos, tanto en la perspectiva del agente 47 como del patrulla
to-report MCTS:get-rules [s]
  let p MCTS:get-playerJustMoved s
  let lr []


  ifelse(p = 2)[ ;Si es el turno del jugador entonces solo contará con las alternativas de este
    set lr compruebaReglas s p ;[90, 180 ...]
  ]

  [ ;Si es el turno de la máquina entonces sacará las alternativas con las posibles fichas a mover
    let i count patrullas

    while[i > 0][
      let slr compruebaReglas s (i + 5)
      foreach slr [
        x -> let sl [] set sl lput (i + 5) sl set sl lput x sl set lr lput sl lr
      ]
      set i i - 1
    ]


  ]




  if((empty? lr) and (p = 2))[set ahogado true] ;Si el jugador no puede hacer más movimientos es porque esta rodeado y por ende ha perdido

  report lr ;devuelve las direcciones a las que puede caminar ya sea el agente como el patrulla
end

to-report compruebaReglas[s j] ;Comprueba cada alternativa que pueda ejecutar el jugador j
  let c MCTS:get-content s
  let p MCTS:get-playerJustMoved s
  let slr []

    let l halla-coordenadas s j ;Coordenadas del jugador en el tablero

    let corx first l
    let cory last l

    if(corx != false)[
      if(cory != 0)[

        let sl1 item (cory - 1) c
        let v1 item corx sl1

        if((v1 != 6) and (v1 != 7) and (v1 != 8) and (v1 != 9) and (v1 != 1) and (v1 != 2))[
          set slr lput 0 slr
        ]
      ]

      if(corx != (world-width - 1))[

        let sl2 item cory c
        let v2 item (corx + 1) sl2

        if((v2 != 6) and (v2 != 7) and (v2 != 8) and (v2 != 9) and (v2 != 1) and (v2 != 2))[
          set slr lput 90 slr
        ]
      ]

      if(cory != (world-width - 1))[
        let sl3 item (cory + 1) c
        let v3 item corx sl3

        if((v3 != 6) and (v3 != 7) and (v3 != 8) and (v3 != 9) and (v3 != 1) and (v3 != 2))[
          set slr lput 180 slr
        ]
      ]

      if(corx != 0)[
        let sl4 item cory c
        let v4 item (corx - 1) sl4

        if((v4 != 6) and (v4 != 7) and (v4 != 8) and (v4 != 9) and (v4 != 1) and (v4 != 2))[
          set slr lput 270 slr
        ]
      ]
    ]

  report slr


end

to-report halla-coordenadas[s p] ;Sala las coordenadas del jugador p

  let c MCTS:get-content s

  let i 0

  let corx false

  let sl []

  while[(corx = false) and i < (length c)][
    set sl item i c
    set corx position p sl
    set i i + 1

  ]

  let cory position sl c

  report (list corx cory)
end


to-report MCTS:apply [r s] ;Aplica la alternativa elegida

  let p MCTS:get-playerJustMoved s
  let c MCTS:get-content s

  ifelse(p = 2)[ ;Si esta jugando el jugador
    set c aplicar s p r c
  ]

  [;Si está jugando la máquina

    let j first r ;Separamos al patrulla de la dirección
    let ac last r
    set c aplicar s j ac c
  ]

  ifelse
  (p = 2)[set p 3] ;Cambio de jugador pal proximo turno
  [set p 2]

  let nstate MCTS:create-state c p
  print nstate
  report nstate ;2: jugador 3: patrulla
end

to-report aplicar[s j r c] ;Aplicamos la alternativa r a c con la información de s con el jugador j

  let l halla-coordenadas s j

  let corx first l

  let cory last l

  let sl item cory c ;fila
  let punto item corx sl ;columna

  (ifelse
      (r = 90)[
        let movimiento-der corx + 1
        set sl replace-item movimiento-der sl j
        set c replace-item cory c sl
    ]
      (r = 180)[

      let dir-fila-inferior cory + 1 ;en c
        let fila-inferior item dir-fila-inferior c ;en c
        let movimiento-abajo item corx fila-inferior
        set fila-inferior replace-item corx fila-inferior j
        set c replace-item dir-fila-inferior c fila-inferior
      ]

      (r = 270)[
        let movimiento-izq corx - 1
        set sl replace-item movimiento-izq sl j
        set c replace-item cory c sl
      ]

      [
        let dir-fila-superior cory - 1
        let fila-superior item dir-fila-superior c
        let movimiento-arriba item corx fila-superior
        set fila-superior replace-item corx fila-superior j
        set c replace-item dir-fila-superior c fila-superior
      ])

  ;ponemos el patch donde se encontraba a 0
  set sl replace-item corx sl 0
  set c replace-item cory c sl

  report c
end

to-report MCTS:get-result [s p];Como esta planteado montecarlo indica solo la victoria de la máquina cuando hay agente ahogado
  report 1
end

to-report board-to-state
  ;saca una lista con el valor de cada losa ([1 0 0 2 0...])
  let b map [x -> [value] of x] (sort patches)

  report (lista-a-matriz b)
end

to-report lista-a-matriz[lista] ;Transforma una lista en una matriz
  let matriz []
  let i world-width
  while[i > 0][
    let sl []
    set matriz lput sl matriz
    set i i - 1
  ]

  let cont 0
  let s 0
  set i 0

  while [i < (world-width * world-width)] [

    let sl item s matriz
    set sl (lput (item i lista) sl )
    set cont cont + 1
    set matriz replace-item s matriz sl
    if (cont = world-width) [
      set s s + 1
      set cont 0
    ]

    set i i + 1
  ]


  report matriz

end

;zona A*

to-report applicable-transitions ;Devuelve la lista de alternativas

  let la []

  set la lput (list 90 1 ([ x -> (list ((first x) + 1) (last x))])) la ;Arriba
  set la lput (list 180 1 ([ x -> (list (first x)((last x) - 1))])) la ;Abajo
  set la lput (list 270 1 ([ x -> (list ((first x) - 1) (last x))])) la ;Izquierda
  set la lput (list 0 1 ([ x -> (list (first x)((last x) + 1))])) la ;Derecha
  report la

end

to-report valid? [x]
  let valido true

  let corx first x
  let cory last x

  ifelse(patch corx cory = NOBODY) [set valido false][ ;Si la coordenada no existe o es una pared no e válida

    ask patch corx cory [if(pcolor = (gray - 3)) [set valido false]]]

  report valido
end

to-report AI:children-states
 let lc filter [ s -> valid? (first s) ] ;filtra las alternativas validas
                (map [ t -> (list (run-result (last t) content) t) ] ;mapeamos el contenido con la funcion (last) de cada aplicacion definida en applicable-transitions
                     applicable-transitions)

  report lc
end

to-report AI:heuristic [#Goal]
  let c [content] of current-state
  let corx first c
  let cory last c
  let corxG first #Goal
  let coryG last #Goal

  report abs((corx - corxG) + (cory - coryG)) ;Diferencia entre las coordenadas del goal y la localización actual
end

to-report AI:final-state? [params]
  report content = params ;Será el estado final si la coordenada actual es la goal
end

to-report AI:equal? [a b]
  report a = b
end

to highlight-path [path]
  foreach path [ s ->
    ask s [
      set color red set thickness .4
    ]
  ]
end

;ID3

to load-DF
  ca
  ask patches [set pcolor white]
  set decision-tree nobody
  ; Read the dataset file
  let fname user-file
  set DataFrame DF:load fname
  if DataFrame != false [
     ;Print the dataset
    output-print (word "Original Dataset: " fname)
    output-print DF:pp DataFrame
    set Numerical-attributes (word (map [x -> (word "\"" x "\"")] DF:header DataFrame))
  ]
end

;Función para crear el arbol de decisión
to main-ID3 [att-target]
  ct
  if DataFrame != false
  [
    ; Aplicamos el algoritmo ID3
    set decision-tree ID3:ID3 att-target DataFrame
    layout
  ]
end

to layout
    ; Layout the Decision Tree
    ID3:format
    set #LayoutNodes ID3:nodes
    set #LayoutEdges ID3:links
    set #LayoutNode0 ID3:node 0
    layout-space "V"
end


; Función con la que podremos preguntar por un contrato concreto para que nos indique si puede ser cumplido o no
to test
  ask decision-tree [print  (word "-----------------------\nPosible de cumplir? "ID3:evaluate read-from-string contrato"\n-----------------------")]
end

;Zona AG

; Creación de la población inicial.
to AI:Initial-Population [#population]
  create-AI:individuals #population [
    let N count patches with [pcolor = yellow or pcolor = magenta or pcolor = lime or value = 4]
    set content []
    while[N > 0][
      set content lput N content
      set N N - 1
    ]
    set content shuffle content
    AI:Compute-fitness
    hide-turtle
  ]
end

; Cálculo de fitness
to AI:Compute-fitness
  let res 0
  let N length(content) - 1
  let i 0
  let llaves 0
  let ld false

  let corx [xcor] of one-of agentes
  let cory [ycor] of one-of agentes
  let s0 (list corx cory)
  let v 0
  let w 0

  while [i <= N][

    let d 0
    ifelse(i = 0)[ ;Si es el comienzo calculamos la distancia entre la inicial y el primer patch
      set v item i content
      set w item (i + 1) content
      let corxD [pxcor] of patches with [id = v]
      let coryD [pycor] of patches with [id = v]
      let goalD (list corxD coryD)
      ifelse(v = 1 or v = 2)[set llaves llaves + 1][set res res - 10000] ;Si el primer patch por el que se va a pasar es una llave, se suma una llave al llavero, si no se penaliza al tener que ser la primera una llave si o si
      if(v = 4)[set ld true] ;Si el primero es la llave del despacho la consideramos conseguida
    ]
    [

      set v item (i - 1) content
      set w item i content
      let corxO [pxcor] of patches with [id = v]
      let coryO [pycor] of patches with [id = v]
      let goalO (list corxO coryO)
      let corxD [pxcor] of patches with [id = w]
      let coryD [pycor] of patches with [id = w]
      let goalD (list corxO coryO)

      (ifelse
        (w = 1 or w = 2)[set llaves llaves + 1] ;Si se trata de una llave se añade al llavero
        (w = 4)[set ld true] ;Si es la llave del despacho la consideramos obtenida
        (w = 5 and ld = false)[set res res - 1000] ;Si es el objetivo y todavía nos falta la llave del despacho se penaliza
        ((w != 1) and (w != 2) and (w != 5) and llaves > 0)[set llaves llaves - 1] ;Si se trata de la estatua o la llave del despacho y hay una llave a nuestra disposicion, entonces descartamos una llave sin penalizar nada
      )
    ]
    set i i + 1
  ]
  set fitness res ;El fitness será res
end

to-report AI:Crossover [c1 c2] ;Con la estructura que he escogido no puedo partir el cromosona mediante los cortes y uniones sencillas del algoritmo, no se usará
  let cut-point 1 + random (length c1 - 1)
  report list (sentence (sublist c1 0 cut-point)
                        (sublist c2 cut-point length c2))
  (sentence (sublist c2 0 cut-point)
                        (sublist c1 cut-point length c1))
end

;Mutará de forma que dos genes aleatorios del cromosoma se intercambiarán
to AI:mutate [#mutation-ratio]

  let v random 5
  let vitem item v content
  let w 0
  ifelse(v = length(content) - 1)[set w 0] [set w v + 1]
  let witem item w content
  set content replace-item v content witem
  set content replace-item w content vitem
end

to AI:ExternalUpdate ;No hace nada en lo que estoy haciendo
  display
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
578
379
-1
-1
40.0
1
10
1
1
1
0
0
0
1
-4
4
-4
4
1
1
1
ticks
30.0

INPUTBOX
23
68
178
128
fgcolor
85.0
1
0
Color

BUTTON
389
440
476
473
draw-cells
draw-cells
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
68
154
131
187
New
new
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
66
254
129
287
Up
if (play? = true)[set action 4]
NIL
1
T
OBSERVER
NIL
W
NIL
NIL
0

BUTTON
66
289
129
322
Down
if (play? = true) [set action 3]
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
0

BUTTON
0
289
63
322
Left
if (play? = true)[set action 1]
NIL
1
T
OBSERVER
NIL
A
NIL
NIL
0

BUTTON
131
289
194
322
Right
if (play? = true) [set action 2]
NIL
1
T
OBSERVER
NIL
D
NIL
NIL
1

BUTTON
68
202
131
235
play
play
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
209
387
311
432
llaves obtenidas
keys
0
1
11

MONITOR
327
435
384
480
estatua
estatuarecuperada?
17
1
11

MONITOR
210
435
324
480
llave del despacho
keydesp
17
1
11

MONITOR
313
387
414
432
objetivo eliminado
eliminado?
17
1
11

BUTTON
58
348
135
381
Caminar
if (play? = true) [set action 5]
NIL
1
T
OBSERVER
NIL
K
NIL
NIL
1

OUTPUT
586
110
1236
540
11

SLIDER
13
19
185
52
Max_iterations
Max_iterations
0
6000
100.0
100
1
NIL
HORIZONTAL

MONITOR
417
387
474
432
monte?
monte?
17
1
11

MONITOR
477
387
534
432
NIL
played?
17
1
11

BUTTON
590
10
653
43
Load
load-df
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
654
10
764
43
ID3 Contratos
main-ID3 Target-Attribute\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1241
110
1336
170
Target-Attribute
Cumplido?
1
0
String

INPUTBOX
1009
43
1286
103
Numerical-attributes
[\"Arma\" \"Sitio\" \"Ganzua?\" \"Salida\" \"Cumplido?\"]
1
0
String

BUTTON
764
10
896
43
Clasificar contrato
test
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
589
43
1001
103
contrato
[[\"Arma\" \"AK\"] [ \"Sitio\" \"Rusia\"] [\"Ganzua?\" \"No\"] [\"Salida\" \"Barco\"] ]
1
0
String

MONITOR
479
433
574
478
NIL
contadorPasos
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
