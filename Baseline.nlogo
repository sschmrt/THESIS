; Marta Schwarz
; Msc Urban Environmental Management
; Master Thesis
; Supervisor Arend Ligtenberg
; This was my old code

extensions [gis table csv]

breed [peds ped]
breed [bikes bike]

globals [conflict-table destination-features destination-tables time mean-speed stddev-speed flow-cum polygons waitingpoint dataset wgs84-dataset study-area-patches]
peds-own [speedx speedy state break-timer my-destination initialheading origin]
bikes-own [speedx speedy state break-timer my-destination initialheading intendedheading origin]
patches-own [ obstacle? destination-type function-id waiting]

;; Part 1: Setup the Environment

; Setup modelling environment
; Setup modeling environment
to setup
  clear-all
  reset-ticks
  tick
  check-conflict
  set conflict-table table:make

  ; Load the GeoJSON dataset
  set dataset gis:load-dataset "C:/Users/marta/Desktop/THESIS/Layers/Thesis_Simple.geojson"
  set waitingpoint gis:load-dataset "C:/Users/marta/Desktop/THESIS/Layers/Zones.geojson"

  ; Draw dataset for visualization
  gis:set-drawing-color red
  gis:draw dataset 0.1

  ; Identify patches in the study area
  set study-area-patches patches with [is-in-study-area? self]
  define-obstacles

  ask study-area-patches [
    set pcolor green
  ]

  ; Identify waiting patches
  gis:apply-coverage waitingpoint "WAITING" waiting

  ; Classify destination patches
  classify-destination-patches

  ; Setup table with percentages of destination by direction
  setup-destination-tables

  ; Initialize variables
  set Nb-peds 100
  set Nb-bikes 100
  spawn-agents
end


;; Rules for spawning
to spawn-agents
  ;; Spawn pedestrians based on probability for each direction
  if random-float 1 < ped_N [
    if any? patches with [destination-type = "north"] [
      create-peds 1 [
        move-to one-of patches with [destination-type = "north"]
        pen-down
        set state 1
        set break-timer 0
        set origin "N"
        set shape "circle"
        set color cyan
        set size 0.2
        assign-pedspeed
        assign-destinations
      ]
    ]
  ]
  if random-float 1 < ped_S [
    if any? patches with [destination-type = "south"] [
      create-peds 1 [
         move-to one-of patches with [destination-type = "south"]
        set state 1
        set break-timer 0
        set origin "S"
        set shape "circle"
        set color cyan
        set size 0.2
        assign-pedspeed
        assign-destinations
      ]
    ]
  ]
  if random-float 1 < ped_E [
    if any? patches with [destination-type = "east"] [
      create-peds 1 [
         move-to one-of patches with [destination-type = "east"]
        set state 1
        set break-timer 0
        set origin "E"
        set shape "circle"
        set color cyan
        set size 0.2
        assign-pedspeed
        assign-destinations
      ]
    ]
  ]
  if random-float 1 < ped_W [
   if any? patches with [destination-type = "west"] [
      create-peds 1 [
       move-to one-of patches with [destination-type = "west"]
        set state 1
        set break-timer 0
        set origin "W"
        set shape "circle"
        set color cyan
        set size 0.2
        assign-pedspeed
        assign-destinations
      ]
    ]
  ]

  ;; Spawn bikes based on probability for each direction
  if random-float 1 < bik_N [
    if any? patches with [destination-type = "north"] [
      create-bikes 1 [
       move-to one-of patches with [destination-type = "north"]
        set state 1
        set break-timer 0
        set origin "N"
        set shape "circle"
        set color magenta
        set size 0.45
        assign-bikespeed
        assign-destinations
      ]
    ]
  ]
  if random-float 1 < bik_S [
    if any? patches with [destination-type = "south"] [
      create-bikes 1 [
        move-to one-of patches with [destination-type = "south"]
        set state 1
        set break-timer 0
        set origin "S"
        set shape "circle"
        set color magenta
        set size 0.
        assign-bikespeed
        assign-destinations
      ]
    ]
  ]
  if random-float 1 < bik_E [
    if any? patches with [destination-type = "east"] [
      create-bikes 1 [
        move-to one-of patches with [destination-type = "east"]
        set state 1
        set break-timer 0
        set origin "E"
        set shape "circle"
        set color magenta
        set size 0.45
        assign-bikespeed
        assign-destinations
      ]
    ]
  ]
  if random-float 1 < bik_W [
    if any? patches with [destination-type = "west"] [
      create-bikes 1 [
        move-to one-of patches with [destination-type = "west"]
        set state 1
        set break-timer 0
        set origin "W"
        set shape "circle"
        set color magenta
        set size 0.45
        assign-bikespeed
        assign-destinations
      ]
    ]
  ]
end

; Rules for initial speed
to assign-pedspeed
  ;; Generate a random base speed for the turtle
  let base-speed random-float 1.5  ;;

  ;; Decompose the base speed into x and y components based on the heading
  set speedx base-speed * cos heading
  set speedy base-speed * sin heading
end

to assign-bikespeed
  ;; Generate a random base speed for the turtle
  let base-speed random-float 5  ;;

  ;; Decompose the base speed into x and y components based on the heading
  set speedx base-speed * cos heading
  set speedy base-speed * sin heading
end

;; Rules for destinations
to assign-destinations
  ;; Assign destinations for pedestrians
  ask peds [
    if my-destination != nobody [
      ;; The agent's origin attribute (e.g., "south")
      let myorigin origin ;;

      ;; Match the agent's origin with the table's origin (first column)
      let origin-probabilities filter [row -> item 0 row = origin] destination-tables

      ;; Initialize cumulative probability and random number
      let rand random-float 1
      let cumulative-probability 0

      ;; Iterate through the filtered probabilities
      foreach origin-probabilities [
        [row] ->
        ;; Add the current row's probability to the cumulative probability
        set cumulative-probability cumulative-probability + item 2 row

        ;; Check if the random number falls within the cumulative probability
        if rand < cumulative-probability [
          ;; Assign the destination based on the destination type in the row
          set my-destination one-of patches with [destination-type = item 1 row]
          stop ;; Exit the loop once the destination is assigned
        ]
      ]
    ]
  ]

  ;; Assign destinations for bikes
  ask bikes [
    if my-destination != nobody [
      ;; The agent's origin attribute (e.g., "south")
      let myorigin origin ;; Replace `origin` with the actual attribute name storing the agent's origin

      ;; Match the agent's origin with the table's origin (first column)
      let origin-probabilities filter [row -> item 0 row = origin] destination-tables

      ;; Initialize cumulative probability and random number
      let rand random-float 1
      let cumulative-probability 0

      ;; Iterate through the filtered probabilities
      foreach origin-probabilities [
        [row] ->
        ;; Add the current row's probability to the cumulative probability
        set cumulative-probability cumulative-probability + item 2 row

        ;; Check if the random number falls within the cumulative probability
        if rand < cumulative-probability [
          ;; Assign the destination based on the destination type in the row
          set my-destination one-of patches with [destination-type = item 1 row]
          stop ;; Exit the loop once the destination is assigned
        ]
      ]
    ]
  ]
end

;; Part 2: Multilayered approach to modelling

;; Pathfinding Layer
to move-to-goal
  ask peds [
    if my-destination != nobody [

      face my-destination
      move-to my-destination
      fd 1
      if distance my-destination < 0.5 [ ;
        die
      ]
    ]
  ]
  ask bikes [
    if my-destination != nobody [
      face my-destination
      move-to my-destination
      fd 1
      if distance my-destination < 0.5 [ ;
        die
      ]
    ]
  ]
end

;; Interaction and Obstacle Layer
to move
  if ticks >= 3600 [ stop ]
  set time precision (time + dt) 5
  tick-advance 1

  ;; Pedestrians
  ask peds [
    if state = 1 [
      ;; Initialize repulsion forces
      let repx 0
      let repy 0
     if my-destination != nobody and my-destination != 0 and is-patch? my-destination [
        set initialheading towards my-destination
      ]
        ;; Fallback behavior if my-destination is invalid

        set initialheading random 360  ;; Assign a random heading


      ;; Avoid both other pedestrians and bikes with SFT
      ask (other peds in-radius (D)) [
        let dist-ped distance myself
        if dist-ped > 0 [
          set repx repx + A * exp((1 - dist-ped) / D) * sin(towards myself) * (1 - cos(towards myself - initialheading))
          set repy repy + A * exp((1 - dist-ped) / D) * cos(towards myself) * (1 - cos(towards myself - initialheading))
        ]
      ]
      ask ( bikes in-radius (D)) [
        let dist-bike distance myself
        if dist-bike > 0 [
          set repx repx + A * exp((1 - dist-bike) / D) * sin(towards myself)
          set repy repy + A * exp((1 - dist-bike) / D) * cos(towards myself)
        ]
      ]

      ;; Avoid obstacles
      ask patches with [obstacle?] in-radius (1.5 * D) [
        let dist-obs distance myself
        if dist-obs > 0 [
          set repx repx + A * exp((1 - dist-obs) / D) * sin(towards myself)
          set repy repy + A * exp((1 - dist-obs) / D) * cos(towards myself)
        ]
      ]

      ;; Check if current-target is valid before using towards
      if my-destination != nobody [
  face my-destination
      ]

      ;; Adjust movement with repulsion and path-following
      set speedx speedx + dt * (repx + (V0-ped * sin initialheading - speedx) / Tr)
      set speedy speedy + dt * (repy + (V0-ped * cos initialheading - speedy) / Tr)

       ;; Limit Pedestrian Speed
      let current-speed-mps sqrt (speedx ^ 2 + speedy ^ 2) ; Current speed in meters per second
      let min-ped-speed-mps (4 / 3.6)             ; Convert 4 km/h to m/s
      let max-ped-speed-mps (6 / 3.6)             ; Convert 6 km/h to m/s

      if current-speed-mps < min-ped-speed-mps [
        let scale-factor min-ped-speed-mps / current-speed-mps
        set speedx speedx * scale-factor
        set speedy speedy * scale-factor
      ]
      if current-speed-mps > max-ped-speed-mps [
        let scale-factor max-ped-speed-mps / current-speed-mps
        set speedx speedx * scale-factor
        set speedy speedy * scale-factor
      ]

      ;; Update heading
      let total-vx (V0-ped * sin initialheading) + repx
      let total-vy (V0-ped * cos initialheading) + repy
      set heading atan total-vx total-vy

      ;; Move agent based on computed speeds
      let next-patch patch-at (xcor + speedx * dt) (ycor + speedy * dt)
      if next-patch != nobody and member? next-patch study-area-patches [
        move-to next-patch
      ]
      ;; Check for break
      let stop-probability [waiting] of patch-here  ;; get the patch's value
if (random-float 1 < stop-probability) [
  set state 0  ;; stop the pedestrian
]

    ]

    ;; Handle state 0: Pedestrian is on a break (not moving)
    if state = 0 [
      if break-timer = 0 [
      set break-timer random 5 + 1 ;; Random value between 1 and 5
    ]

    ;; Ensure the agent remains stationary
    set speedx 0
    set speedy 0

    ;; Countdown the break-timer
    set break-timer break-timer - 1

    ;; Check if the break-timer has expired
    if break-timer <= 0 [
      set state 1 ;; Resume movement after the break
    ]
  ]
  ;; Bikes
  ask bikes [
    if state = 1 [
      ;; Initialize repulsion forces
      let repx 0
      let repy 0
      if my-destination != nobody [
  set initialheading towards my-destination
]

      ;; Avoid both other bikes and pedestrians using SFT
      ask (other bikes in-radius (1.5 * D)) [
        let dist-bike distance myself
        if dist-bike > 0 [
          set repx repx + A * exp((1 - dist-bike) / D) * sin(towards myself) * (1 - 0.5 * cos(towards myself - initialheading))
          set repy repy + A * exp((1 - dist-bike) / D) * cos(towards myself) * (1 - 0.5 * cos(towards myself - initialheading))
        ]
      ]
      ask (peds in-radius (1.5 * D)) [
        let dist-ped distance myself
        if dist-ped > 0 [
          set repx repx + A * exp((1 - dist-ped) / D) * sin(towards myself)
          set repy repy + A * exp((1 - dist-ped) / D) * cos(towards myself)
        ]
      ]

      ;; Avoid obstacles
      ask patches with [obstacle?] in-radius (1.8 * D) [
        let dist-obs distance myself
        if dist-obs > 0 [
          set repx repx + A * exp((1 - dist-obs) / D) * sin(towards myself)
          set repy repy + A * exp((1 - dist-obs) / D) * cos(towards myself)
        ]
      ]

      ;; Check if current-target is valid before using towards
     if my-destination != nobody [
  face my-destination
]

      ;; Adjust movement while preserving angular inertia
      set speedx speedx + dt * (repx + (V0-bike * sin initialheading - speedx) / (Tr * 2))
      set speedy speedy + dt * (repy + (V0-bike * cos initialheading - speedy) / (Tr * 2))

      ;;  Limit Bike Speed
      let current-speed-mps sqrt (speedx ^ 2 + speedy ^ 2) ; Current speed in meters per second
      let min-bike-speed-mps (14 / 3.6)            ; Convert 14 km/h to m/s
      let max-bike-speed-mps (18 / 3.6)            ; Convert 18 km/h to m/s

      if current-speed-mps < min-bike-speed-mps [
        let scale-factor min-bike-speed-mps / current-speed-mps
        set speedx speedx * scale-factor
        set speedy speedy * scale-factor
      ]
      if current-speed-mps > max-bike-speed-mps [
        let scale-factor max-bike-speed-mps / current-speed-mps
        set speedx speedx * scale-factor
        set speedy speedy * scale-factor
      ]


      ;; Angular inertia: smooth direction changes
     if my-destination != nobody [
      let total-vx (V0-ped * sin initialheading) + repx
      let total-vy (V0-ped * cos initialheading) + repy
      set intendedheading atan total-vx total-vy
  set heading heading + (intendedheading - heading) * 0.2
]

      ;; Move bike based on computed speeds
      let next-patch patch-at (xcor + speedx * dt) (ycor + speedy * dt)
      if next-patch != nobody and member? next-patch study-area-patches [
        move-to next-patch
      ]

      ;; Check for break
      let stop-probability [waiting] of patch-here  ;; get the patch's value
if (random-float 1 < stop-probability) [
  set state 0  ;; stop the pedestrian
]

    ]

    ;; Handle state 0: Bike is on a break (not moving)
    if state = 0 [
      if break-timer = 0 [
      set break-timer random 5 + 1 ;; Random value between 1 and 5
    ]

    ;; Ensure the agent remains stationary
    set speedx 0
    set speedy 0

    ;; Countdown the break-timer
    set break-timer break-timer - 1

    ;; Check if the break-timer has expired
    if break-timer <= 0 [
      set state 1 ;; Resume movement after the break
    ]
  ]
  ]
  ]
  ;; Additional simulation updates
  move-to-goal
  check-conflict
  update-stats-and-flow
end


;; Part 3: Define the spatial attributes of the model

;; Define the study area
to-report is-in-study-area? [study-patch]
  let in-area? false
  foreach gis:feature-list-of dataset [
    [feature] ->
    let function-value gis:property-value feature "id"
    if (function-value = 1 ) and gis:intersects? study-patch feature [
      set in-area? true
    ]
  ]
  report in-area?
end

;; Define the destination patches in NSEW
to classify-destination-patches
  ask patches [
    ; Debugging information
    print self
    foreach gis:feature-list-of dataset [
      [feature] ->
      ;; Get the function value of the polygon
      let function-value gis:property-value feature "function"

      ;; Assign function-value to patches that intersect with the feature
      ask patches with [ gis:intersects? self feature ] [
        if function-value = 1 [ set destination-type "east" ]
        if function-value = 2 [ set destination-type "north" ]
        if function-value = 3 [ set destination-type "west" ]
        if function-value = 4 [ set destination-type "south" ]
      ]
    ]
  ]
end

;; Define obstacles: pillars and people who are not moving
to define-obstacles
  ask patches [
    set obstacle? false
    let my-polygon nobody
    foreach gis:feature-list-of dataset [
      [feature] ->
      if gis:intersects? self feature [
        set my-polygon feature
      ]
    ]
    ; Define pillars as obstacles
    if my-polygon != nobody [
      let polygon-id gis:property-value my-polygon "function"
      if polygon-id = 666 [
        set obstacle? true
        set pcolor pink  ;; Mark obstacles visually
      ]
    ]
  ]

  ; Mark agents with state = 0 as obstacles
  ask peds with [state = 0] [
    set obstacle? true
    set color white  ;; Mark resting pedestrians visually
  ]
  ask bikes with [state = 0] [
    set obstacle? true
    set color white  ;; Mark resting pedestrians visually
  ]
end


;; Part 4: Log the outcomes of each simulation

;; Define outputs
to update-stats-and-flow
  let peds-with-speed [ self ] of peds with [state > -1]
  let bikes-with-speed [ self ] of bikes with [state > -1]

  ; Update mean and standard deviation of speed for peds
  if not empty? peds-with-speed [
    let peds-agentset turtle-set peds-with-speed
    set mean-speed mean-speed + mean [sqrt(speedx ^ 2 + speedy ^ 2)] of peds-agentset
    if count peds-agentset > 1 [
      set stddev-speed stddev-speed + sqrt(variance [sqrt(speedx ^ 2 + speedy ^ 2)] of peds-agentset)
    ]
  ]

  ; Update mean and standard deviation of speed for bikes
  if not empty? bikes-with-speed [
    let bikes-agentset turtle-set bikes-with-speed
    set mean-speed mean-speed + mean [sqrt(speedx ^ 2 + speedy ^ 2)] of bikes-agentset
    if count bikes-agentset > 1 [
      set stddev-speed stddev-speed + sqrt(variance [sqrt(speedx ^ 2 + speedy ^ 2)] of bikes-agentset)
    ]
  ]

  ; Update cumulative flow for peds crossing the center
  ask peds with [
    (xcor > 0 and xcor - speedx * dt <= 0) or
    (xcor < 0 and xcor - speedx * dt >= 0) or
    (ycor > 0 and ycor - speedy * dt <= 0) or
    (ycor < 0 and ycor - speedy * dt >= 0)
  ] [
    set flow-cum flow-cum + 1
  ]

  ; Update cumulative flow for bikes crossing the center
  ask bikes with [
    (xcor > 0 and xcor - speedx * dt <= 0) or
    (xcor < 0 and xcor - speedx * dt >= 0) or
    (ycor > 0 and ycor - speedy * dt <= 0) or
    (ycor < 0 and ycor - speedy * dt >= 0)
  ] [
    set flow-cum flow-cum + 1
  ]


  plot! ; Update the plots
end

to setup-destination-tables
  ;; Hardcoded origin-destination-probability table
  set destination-tables [
    ["north" "north" 0.01]
    ["north" "south" 0.4]
    ["north" "east" 0.3]
    ["north" "west" 0.29]
    ["south" "north" 0.45]
    ["south" "south" 0.01]
    ["south" "east" 0.3]
    ["south" "west" 0.3]
    ["east" "north" 0.4]
    ["east" "south" 0.3]
    ["east" "east" 0.01]
    ["west" "west" 0.29]
    ["west" "north" 0.4]
    ["west" "south" 0.3]
    ["west" "east" 0.29]
    ["west" "west" 0.01]
  ]
end

;; Check conflict in  study area
to check-conflict
  ask turtles [
    let my-polygon nobody
    foreach gis:feature-list-of waitingpoint [
      [feature] ->
      if gis:intersects? patch-here feature [
        set my-polygon feature
      ]
    ]

    ;; Only proceed if the turtle is in a relevant polygon
    if my-polygon != nobody [
      let polygon-id gis:property-value my-polygon "ID"
      if member? polygon-id [1 2 3 4 5 6 7 8] [
        let severe-count 0
        let moderate-count 0
        let mild-count 0

        ;; Check all other agents
        ask other turtles [
          let distancetoother distance myself  ;; Calculate distance between agents

          ;; Categorize conflict severity
          if distancetoother <= 1.5 [set severe-count severe-count + 1]
          if distancetoother > 1.5 and d <= 2.5 [set moderate-count moderate-count + 1]
          if distancetoother > 2.5 and d <= 3.5 [set mild-count mild-count + 1]
        ]

        ;; Store results in the table
        ifelse table:has-key? conflict-table polygon-id [
          ;; Get old values and update them
          let old-values table:get conflict-table polygon-id
          table:put conflict-table polygon-id (list
            (item 0 old-values + severe-count)
            (item 1 old-values + moderate-count)
            (item 2 old-values + mild-count)
          )
        ]
        [
          ;; If polygon ID is not in the table yet, add it
          table:put conflict-table polygon-id (list severe-count moderate-count mild-count)
        ]
      ]
    ]
  ]
end

;; Output conflict
to report-conflicts
  print "Polygon ID | Severe | Moderate | Mild"
  foreach table:keys conflict-table [
    ? -> let data table:get conflict-table ?
    print (word ? " | " item 0 data " | " item 1 data " | " item 2 data)
  ]
end

;; Plot your output!
to plot!
  set-current-plot "Speed"
  set-current-plot-pen "Mean"
  plotxy time (mean-speed / ticks)
  set-current-plot-pen "Stddev"
  plotxy time (stddev-speed / ticks)
  set-current-plot "Mean flow"
  set-plot-y-range 0 2
  set-current-plot-pen "Spatial"
  plotxy time ((mean-speed / ticks) * Nb-peds / world-width / world-height)
  set-current-plot-pen "Temporal"
  plotxy time (flow-cum / time / world-height)
end
@#$#@#$#@
GRAPHICS-WINDOW
542
11
960
430
-1
-1
10.0
1
10
1
1
1
0
1
1
1
-20
20
-20
20
0
0
1
Ticks
30.0

SLIDER
3
10
95
43
Nb-peds
Nb-peds
0
7000
100.0
1
1
NIL
HORIZONTAL

BUTTON
388
15
443
49
NIL
Setup
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
448
16
503
49
NIL
Move
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
204
324
506
444
Mean flow
Time
Flow
0.0
0.0
0.0
0.0
true
true
"" ""
PENS
"Spatial" 1.0 0 -11053225 true "" ""
"Temporal" 1.0 0 -11881837 true "" ""

PLOT
204
70
505
190
Speed
Time
Speed
0.0
0.0
0.0
0.0
true
true
"" ""
PENS
"Mean" 1.0 0 -11053225 true "" ""
"Stddev" 1.0 0 -11881837 true "" ""

SLIDER
105
105
197
138
V0-bike
V0-bike
0
16
3.0
1
1
NIL
HORIZONTAL

MONITOR
167
10
237
55
Mean speed
mean [sqrt(speedx ^ 2 + speedy ^ 2)] of peds with [state > -1]
5
1
11

MONITOR
240
10
317
55
Speed stddev
stddev-speed / ticks
5
1
11

MONITOR
105
10
163
55
Density
Nb-peds / world-width / world-height
5
1
11

MONITOR
320
10
381
55
Flow
flow-cum / time / world-height
5
1
11

PLOT
203
196
506
316
Fundamental diagram
Density
Flow
0.0
0.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -11053225 true "" ""

PLOT
5
350
165
470
Speed stddev
Density
Stddev
0.0
0.0
0.0
0.7
true
false
"" ""
PENS
"default" 1.0 0 -11053225 true "" ""

SLIDER
105
255
197
288
dt
dt
0
1
1.0
.01
1
NIL
HORIZONTAL

SLIDER
105
177
197
210
A
A
0
5
4.2
.1
1
NIL
HORIZONTAL

SLIDER
105
141
197
174
Tr
Tr
.1
2
0.5
.1
1
NIL
HORIZONTAL

SLIDER
103
217
195
250
p
p
0
1
1.0
.05
1
NIL
HORIZONTAL

SLIDER
0
54
92
87
Nb-Bikes
Nb-Bikes
0
7000
100.0
1
1
NIL
HORIZONTAL

SLIDER
0
129
92
162
ped_S
ped_S
0
1
0.6
.1
1
NIL
HORIZONTAL

SLIDER
0
92
95
125
ped_N
ped_N
0
1
0.9
.1
1
NIL
HORIZONTAL

SLIDER
1
167
93
200
ped_E
ped_E
0
1
0.8
.1
1
NIL
HORIZONTAL

SLIDER
0
204
93
237
ped_W
ped_W
0
1
0.4
.1
1
NIL
HORIZONTAL

SLIDER
0
240
94
273
bik_N
bik_N
0
1
0.7
0.1
1
NIL
HORIZONTAL

SLIDER
1
275
93
308
bik_S
bik_S
0
1
0.9
.1
1
NIL
HORIZONTAL

SLIDER
0
313
95
346
bik_E
bik_E
0
1
0.6
.1
1
NIL
HORIZONTAL

SLIDER
103
334
198
367
bik_W
bik_W
0
1
0.7
.1
1
NIL
HORIZONTAL

SLIDER
103
295
195
328
D
D
0
5
4.8
0.1
1
NIL
HORIZONTAL

SLIDER
107
66
199
99
V0-ped
V0-ped
0
10
10.0
1
1
NIL
HORIZONTAL

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

bike
false
1
Line -7500403 false 163 183 228 184
Circle -7500403 false false 213 184 22
Circle -7500403 false false 156 187 16
Circle -16777216 false false 28 148 95
Circle -16777216 false false 24 144 102
Circle -16777216 false false 174 144 102
Circle -16777216 false false 177 148 95
Polygon -2674135 true true 75 195 90 90 98 92 97 107 192 122 207 83 215 85 202 123 211 133 225 195 165 195 164 188 214 188 202 133 94 116 82 195
Polygon -2674135 true true 208 83 164 193 171 196 217 85
Polygon -2674135 true true 165 188 91 120 90 131 164 196
Line -7500403 false 159 173 170 219
Line -7500403 false 155 172 166 172
Line -7500403 false 166 219 177 219
Polygon -16777216 true false 187 92 198 92 208 97 217 100 231 93 231 84 216 82 201 83 184 85
Polygon -7500403 true true 71 86 98 93 101 85 74 81
Rectangle -16777216 true false 75 75 75 90
Polygon -16777216 true false 70 87 70 72 78 71 78 89
Circle -7500403 false false 153 184 22
Line -7500403 false 159 206 228 205

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

fish 3
false
0
Polygon -7500403 true true 137 105 124 83 103 76 77 75 53 104 47 136
Polygon -7500403 true true 226 194 223 229 207 243 178 237 169 203 167 175
Polygon -7500403 true true 137 195 124 217 103 224 77 225 53 196 47 164
Polygon -7500403 true true 40 123 32 109 16 108 0 130 0 151 7 182 23 190 40 179 47 145
Polygon -7500403 true true 45 120 90 105 195 90 275 120 294 152 285 165 293 171 270 195 210 210 150 210 45 180
Circle -1184463 true false 244 128 26
Circle -16777216 true false 248 135 14
Line -16777216 false 48 121 133 96
Line -16777216 false 48 179 133 204
Polygon -7500403 true true 241 106 241 77 217 71 190 75 167 99 182 125
Line -16777216 false 226 102 158 95
Line -16777216 false 171 208 225 205
Polygon -1 true false 252 111 232 103 213 132 210 165 223 193 229 204 247 201 237 170 236 137
Polygon -1 true false 135 98 140 137 135 204 154 210 167 209 170 176 160 156 163 126 171 117 156 96
Polygon -16777216 true false 192 117 171 118 162 126 158 148 160 165 168 175 188 183 211 186 217 185 206 181 172 171 164 156 166 133 174 121
Polygon -1 true false 40 121 46 147 42 163 37 179 56 178 65 159 67 128 59 116

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
NetLogo 6.4.0
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
