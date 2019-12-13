(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System
open External
open Gfx
open System.Text
open Engine

type CharInfo with
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    static member internal path = pixel.filled Color.Black
    member this.isWall = this = pixel.wall


// TASK 1: implement the maze type
type maze (w, h) =

    //variabile che contiene la posizione attuale
    let mutable (posizione_attuale: int*int) = 0,0
    
    //variabile che contiene la matrice di bool
    let mutable (lab: (bool*bool)[,]) = (Array2D.create w h (false,false)) //il primo elemento indica se la cella è visitata, il secondo se è un muro 

    //lista che simula lo stack, inizialmente lista vuota
    let mutable (stack_pos: (int*int)list) = []

    //variabile che conta il numero di celle visitate
    let mutable (count_visited: int) = 0

    //variabile che salva la direzione valida
    let mutable (conf_move:int) = 0


    let pos_finale = lab.[28,29] <-(false,true)//(29,29)
    let test = lab.[0,10]<-(false,false)
       
    //funzione che genera la posizione iniziale
    member private this.pos_iniziale () = //(0,0) 
        let rec aux () =
            let num = rnd_int 0 (w-1)
            if ((num%2)=0) then num
            else aux()
        in (0, (aux()))

    //funzione che ritorna il primo elemento di una coppia
    member this.first_element (coppia) = 
        match coppia with
        |(x,y) -> x

    //funzione che ritorna il secondo elemento di una coppia
    member this.second_element (coppia) = 
        match coppia with
        |(x,y) -> y

    //funzione che controlla se una casella esiste e in tal caso se non è già stata visitata
    member private this.is_avaiable (riga, colonna) = 
        if ((riga<0) || (riga > (h-1)) || (colonna<0) || (colonna > (w - 1))) then false
        else match lab.[riga, colonna] with
              |(true,_) -> false
              |(false,_) -> true
    
    //funzione che controlla se una posizione è un una strada
    member this.isPath(riga, colonna) = 
        match lab.[riga,colonna] with
        |(_,true) -> true
        |_ -> false    

    //funzione che calcola la posizione successiva in base ad una direzione (dettata da un int) -> 0=North, 1=East, 2=South, 3=West
    member private this.next_pos(x: int) = 
        match x with
        |0 -> ((this.first_element(posizione_attuale))-2), (this.second_element(posizione_attuale))
        |1 -> (this.first_element(posizione_attuale)), ((this.second_element(posizione_attuale))+2)
        |2 -> ((this.first_element(posizione_attuale))+2), (this.second_element(posizione_attuale))
        |3 -> (this.first_element(posizione_attuale)), ((this.second_element(posizione_attuale))-2)
        |_ -> failwith "ERRORE, DIREZIONE IMPREVISTA"

    //funzione che randomizza un numero compreso tra 0 e 3 (SOLO NEI CASI POSSIBILI), che indica le direzioni -> 0=North, 1=East, 2=South, 3=West
    member private this.rnd_dir () = 
        let rec generate_move() =
           let move = (rnd_int 0 3) 
           in if this.is_avaiable(this.next_pos(move)) then move //controlla se la nuova posizione (next_pos(move)) esiste e se non è già stata visitata
              else generate_move()
        in
        conf_move <- (generate_move())
        this.next_pos (conf_move)
                                
    //funzione che calcola la posizione di mezzo dato lo spostamento randomico precedentemente generato
    member private this.middle_pos (x: int) = 
        match x with
        |0 ->  ((this.first_element(posizione_attuale))+1), (this.second_element(posizione_attuale))
        |1 ->  (this.first_element(posizione_attuale)), ((this.second_element(posizione_attuale))-1)
        |2 ->  ((this.first_element(posizione_attuale))-1), (this.second_element(posizione_attuale)) //movimenti invertito (chiedere a Mario, Dio Can)
        |3 ->  (this.first_element(posizione_attuale)), ((this.second_element(posizione_attuale))+1)  
        |_ -> failwith "ERRORE, DIREZIONE IMPREVISTA"

    //funzione che setta la cella di mezzo come path 
    member private this.set_middle_position () = 
        lab.[(this.first_element(this.middle_pos(conf_move))), (this.second_element(this.middle_pos(conf_move)))] <- (false,true)
                        
                     
    //funzione che controlla se è necessario il back tracking (se tutte le celle adiacenti sono visitate): int*int->bool
    member private this.is_isolated(riga, colonna) = 
        (not(this.is_avaiable(riga,(colonna+2)))) && (not(this.is_avaiable(riga, (colonna-2)))) && (not(this.is_avaiable((riga+2), colonna))) && (not(this.is_avaiable((riga-2), colonna)))

    //funzione che riorsivamente trova la prima cella in cui non tutte le celle adiacenti sono visitate
    member private this.find_with_backtracking () = 
        let rec aux stack_pos = //funzione ausiliaria che scorre la lista ricorsivamente alla ricerca di una coppia in cui non è più necessario continuare il backtracking
            match stack_pos with
                |[] -> failwith "ERRORE, non è possibile che il contatore non abbia raggiunto il limite che non ci siano celle in cui non ci si possa muovere" 
                |x::xs -> if (this.is_isolated(x)) then (aux xs) //caso in cui è necessario un ulteriore passo indietro
                          else (x, (x::xs)) //è arrivato ad una coppia in cui ci sono ancora 1 o più celle adiacenti in cui potersi muovere e restituisce tale cella  
        in aux stack_pos

    //ALGORITMO che modifica la lista di bool
    member this.generate() =
    
        posizione_attuale <- this.pos_iniziale()
    
        while (count_visited<(((w*h)/4)-1)) do
            lab.[this.first_element(posizione_attuale), this.second_element(posizione_attuale)] <- (true,true)
            stack_pos <- [posizione_attuale]@(stack_pos)
            count_visited <- count_visited+1
            if this.is_isolated(posizione_attuale) then 
                    posizione_attuale <- this.first_element(this.find_with_backtracking())
                    stack_pos <- this.second_element(this.find_with_backtracking())
                    posizione_attuale <- this.rnd_dir()
                    this.set_middle_position()
            else
                    posizione_attuale <- this.rnd_dir()
                    this.set_middle_position()
        
        
    //per esporare la matrice                      
    member this.export_matrix() = lab
    

    //member private __.generate = ()

[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

let W = 84
let H = 42

let main () =       
    let engine = new engine (W, H)
    let w_maze = 30
    let h_maze = 30
    let mutable player_pos = (0,0)
    let maze = new maze (w_maze,h_maze)
    maze.generate()

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -1., 0.
            | 'd' -> 1., 0.
            | _   -> 0., 0.
        //check bounds
        let dxi = int dx
        let dyi = int dy
        let try_move = (((maze.first_element (player_pos))+dxi),((maze.second_element (player_pos))+dyi)) //aggiunge dx dy a player pos
        if maze.isPath (try_move) 
            then st.player.move_by (dx,dy)
                 player_pos <- try_move
        else st.player.move_by (0,0)
        
        //todo: quando il player arriva a fine labirino (28,29), il programma si stopperà o farà altro
        st, key.KeyChar = 'q'


    // create maze and player
    
    for i in 0 .. (w_maze-1) do
        for j in 0 .. (h_maze-1) do
            if not(maze.isPath(i,j)) then 
                ignore <| engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Red, pixel.filled Color.Red), (i+5), (j+5), 0)
            else ()
   
    let player = engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled Color.White, pixel.filled Color.Gray), 5,5, 0)

    // initialize state
    let st0 = { 
        player = player
        }
    // start engine
    engine.loop_on_key my_update st0
    