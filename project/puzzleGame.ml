exception Fail;;

(* definizione tipo stato -> una lista di interi*)
type stato = St of int list;;

(* definizione del tipo di grafo*)
type 'a graph = Graph of ('a -> 'a list);;

(* funzione che permette di trovare l'indice della casella vuota *)
let findBlank (St(state)) =
  let rec aux index = function
    [] -> raise Fail
    | x::rest -> if x = 0 then index
    else aux (index+1) rest
  in aux 0 state;;

(* funzione che permette di scambiare due caselle con indice x e y*)
let scambia state x y =
  let rec aux index x y = function
    [] -> []
    | hd::rest ->
      if index = x then (List.nth state y)::aux (index+1) x y rest
      else if index = y then (List.nth state x)::aux (index+1) x y rest
      else hd::aux (index+1) x y rest
    in aux 0 x y state;;

(* funzioni di movimento con opportuni controlli*)
let muoviSU (St(state)) = 
  let blankIndex = 
    findBlank (St(state)) 
  in if blankIndex < 4 then raise Fail
  else St(scambia state blankIndex (blankIndex-4));;

let muoviGIU (St(state)) = 
  let blankIndex = 
    findBlank (St(state)) 
  in if blankIndex > 11 then raise Fail
  else St(scambia state blankIndex (blankIndex+4));;

let muoviDX (St(state)) = 
  let blankIndex = 
    findBlank (St(state)) 
  in if (blankIndex mod 4) = 3 then raise Fail
  else St(scambia state blankIndex (blankIndex+1));;

let muoviSX (St(state)) = 
  let blankIndex = 
    findBlank (St(state)) 
  in if (blankIndex mod 4) = 0 then raise Fail
  else St(scambia state blankIndex (blankIndex-1));;


(* funzione test obiettivo *)
let isGoal state = 
  let statoGoal = St[1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;0]
in state = statoGoal;;


(* funzioni di comodo per stampa layout *)
let stampaStato (St(state)) = 
    let stampaNum x = print_int(x); if x>9 then print_string(" ") else print_string("  ")
    in let aCapo x = if (x mod 4) = 3 then print_newline()
  in let rec aux index = function
    [] -> print_newline()
    | x::rest -> stampaNum x; aCapo index; aux (index+1) rest
in aux 0 state;;

let rec stampaCammino = function
  [] -> print_newline()
  | x::rest -> stampaStato x; stampaCammino rest;;


(* funzione successori che rappresenta il grafo *)
let giocoDel15 state =
  let rec aux = function
    [] -> []
    | f::rest ->
      try f state :: aux rest
      with Fail -> aux rest
  in aux [muoviSU; muoviGIU; muoviDX; muoviSX];;

let g = Graph giocoDel15;;

(* funzioni per controllare se una eventuale configurazione rispetta il pattern di istanza: 
16 numeri tra 0 e 15 senza ripetizioni *)
let istanzaValida (St(state)) =
  let checkNumber x (St(state)) =
      let rec aux = function
      [] -> false
      | hd::tail -> (x = hd) || aux tail
    in aux state
  in let rec aux index = function
  [] -> (index = 16)
  | x::rest -> (checkNumber index (St(state))) && aux (index+1) rest
in aux 0 state;;

(* funzioni per controllare se una eventuale configurazione è risolvibile *)
let countInversion (St(state)) =
  let rec checkInversion number inversion  = function
    [] -> inversion
    | x::xrest -> 
      if ((number > x) && not(x = 0)) 
        then checkInversion number (inversion+1) xrest
        else checkInversion number inversion xrest
  in let rec aux = function
    [] -> 0
    | y::yrest -> (checkInversion y 0 yrest) + aux yrest
in aux state;;

let checkSolvability (St(state)) =
  let stato = (St(state))
  in let inversionOddEven = ((countInversion stato) mod 2)
  in let rowIndexBlankOddEven = ((((findBlank stato / 4)+1) + 1) mod 2)
in ((rowIndexBlankOddEven = 0) && (inversionOddEven = 1)) || 
    (( rowIndexBlankOddEven = 1) && (inversionOddEven= 0));;




(* implementazione algoritmo di ricerca *)

(* Best First utilizzando Manhattan *)
let distanzaBf_Manhattan cammino =
  let distanzaManhattan x1 x2 y1 y2 =
    ((abs(x1-x2)) + (abs(y1 -y2)))
  in let heuristic (St(state)) =
    let colonna x = x mod 4
    in let riga x = x / 4
    in let rec aux_Hn index count_Hn = function
      [] -> count_Hn
      | num::rest -> 
        if num = 0 then
          aux_Hn (index+1) (count_Hn) rest
        else
          aux_Hn (index+1) (count_Hn + (distanzaManhattan (colonna index) (colonna (num-1)) (riga index) (riga (num-1)))) rest
  in aux_Hn 0 0 state
in (heuristic (List.hd cammino));;


(* Best First utilizzando caselle fuori posto *)
let distanzaBF_Misplaced cammino =
  let misplaced x index =
    if (x = (index +1)) then 0 else 1
  in let heuristic (St(state)) =
    let rec aux_Hn index count_Hn= function
      [] -> count_Hn
      | num::rest -> 
        if num = 0 then
          aux_Hn (index+1) (count_Hn) rest
        else
          aux_Hn (index+1) (count_Hn + (misplaced num index)) rest
  in aux_Hn 0 0 state
in (heuristic (List.hd cammino));;


(* A* utilizzando Manhattan + costo cammini *)
let distanzaA_Manhattan cammino =
  let distanzaManhattan x1 x2 y1 y2 = 
    ((abs(x1-x2)) + (abs(y1 -y2)))
  in let heuristic (St(state)) =
    let colonna x = x mod 4
    in let riga x = x / 4
    in let rec aux_Hn index count_Hn = function
      [] -> count_Hn
      | num::rest -> 
        if num = 0 then
          aux_Hn (index+1) (count_Hn) rest
        else
          aux_Hn (index+1) (count_Hn + (distanzaManhattan (colonna index) (colonna (num-1)) (riga index) (riga (num-1)))) rest
  in aux_Hn 0 0 state
  in let costocammino cammino =
    let rec aux_Gn count_Gn = function
    [] -> count_Gn
    | x::rest -> aux_Gn (count_Gn + 1) rest
  in aux_Gn 0 cammino
in (heuristic (List.hd cammino)) + (costocammino cammino);;


(* A* utilizzando caselle fuori posto + costo cammini *)
let distanzaA_Misplaced cammino =
  let misplaced x index = 
    if (x = (index +1)) then 0 else 1
  in let heuristic (St(state)) =
    let rec aux_Hn index count_Hn= function
      [] -> count_Hn
      | num::rest -> 
        if num = 0 then
          aux_Hn (index+1) (count_Hn) rest
        else
          aux_Hn (index+1) (count_Hn + (misplaced num index)) rest
  in aux_Hn 0 0 state
  in let costocammino cammino =
    let rec aux_Gn count_Gn = function
    [] -> count_Gn
    | x::rest -> aux_Gn (count_Gn + 1) rest
  in aux_Gn 0 cammino
in (heuristic (List.hd cammino)) + (costocammino cammino);;


(* funzioni di comodo per calcolare i conflitti lineari *)
let checkConflict num1 num2 index1 index2 =
    let colonna x = x mod 4
    in let riga x = x / 4
    in let rigaGoal x = (x - 1) / 4
    in let colonnaGoal x = (x - 1) mod 4
    in let posizioneGoal x = x - 1
  in if ((((riga index1 = riga (index2)) && ((rigaGoal num1 = rigaGoal num2))) || 
    ((colonna index1 = colonna (index2))) && (colonnaGoal num1 = colonnaGoal num2)) && 
    ((index1 > index2 && ((posizioneGoal num1) < (posizioneGoal num2))))) 
    then 2 else 0 ;;

let checkLinearConflict x index (St(state)) =
  let rec aux count indexY = function
    [] -> count
    | hd::tail ->
      if indexY = index then
        count
      else
        if hd = 0 then
          aux (count) (indexY+1) tail
        else
          aux (count + checkConflict x hd index indexY) (indexY+1) tail
in aux 0 0 state;;

(* A* utilizzando linear conflict + Manhattan + costo cammini *)
let distanzaA_conflict cammino =
  let distanzaManhattan x1 x2 y1 y2 = 
    ((abs(x1-x2)) + (abs(y1 -y2)))
  in let heuristic (St(state)) =
    let colonna x = x mod 4
    in let riga x = x / 4
    in let rec aux_Hn index count_Hn = function
      [] -> count_Hn
      | num::rest -> 
        if num = 0 then
          aux_Hn (index+1) (count_Hn) rest
        else
          aux_Hn (index+1) (count_Hn 
          + (distanzaManhattan (colonna index) (colonna (num-1)) (riga index) (riga (num-1))) 
          + (checkLinearConflict num index (St(state)))) rest
    in aux_Hn 0 0 state
  in let costocammino cammino =
    let rec aux_Gn count_Gn = function
    [] -> count_Gn
    | x::rest -> aux_Gn (count_Gn + 1) rest
  in aux_Gn 0 cammino
in (heuristic (List.hd cammino)) + (costocammino cammino);;


(* funzione di comparazione per l'ordinamento della coda di priorità *)
let funzioneValutazione = distanzaA_conflict;;
let confrontaCammino cammino1 cammino2 =
  let piuvicino (cammino1, cammino2) =
    (funzioneValutazione cammino1) < (funzioneValutazione cammino2)
  in if List.hd cammino1 = List.hd cammino2
    then 0
    else if piuvicino (cammino1, cammino2)
      then -1
      else 1;;

let search (St(state)) (Graph succ) =
  let estendi cammino (Graph succ) =
    (*stampaCammino cammino;*)
    List.map (function x -> x::cammino) (List.filter (function x -> (not (List.mem x cammino))) (succ (List.hd cammino)))
  in let rec search_aux = function
    [] -> raise NotFound
    | cammino::rest -> 
      if isGoal (List.hd cammino)
        then List.rev cammino
        else search_aux (List.sort confrontaCammino (rest @ ((estendi cammino (Graph succ)))))
in search_aux [[(St(state))]];;


(* funzioni di avvio *)
let start stato_iniziale = stampaCammino (search stato_iniziale g);;

(* nel caso si voglia il tempo necessario alla risoluzione *)
let start_Wtime stato_iniziale =
  let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "execution time: %fs\n" (Sys.time() -. t);
    fx
in time start stato_iniziale;;

let statoIniziale = St[14;5;10;7;
                      4;15;1;11;
                      2;13;3;12;
                      9;8;6;0];;


                      
(* main *)
let main () =
  if (istanzaValida statoIniziale) && (checkSolvability statoIniziale) then
    start_Wtime statoIniziale
  else
    print_string("Istanza di input non valida!");
    print_newline();;
  main();;
