
open Printf 
let len = Array.length 
(* Printexc.record_backtrace true;;  *)

(*
Algo : 
étape 1 : max_flow 
- G -> G flot 
- on augmente le flot tant qu'on peut

étape 2 : min cut 
- on cherche les arrêtes au bord : un sommet attègnable, l'autre non (l'arrête est saturée et l'un n'est pas accéssible par des arrêtes non saturé)

Correction : 
|f| = C car f est maximal (THM min flow max cut), or C est éxactement le cardinal d'une coupe 
l'idée est que pour x,y dans X ,cX , f(x,y) = c(x,y), donc |f| = C

Complexité : 
O(np(n + p)), où l’on note n = |S| et p = |A|.
*)


let augm c f g s p = 
    let n = len g in 
    let prev = Array.make n (-1) in 
    let file = Queue.create () in
    Queue.push s file;
    prev.(s) <- s;
    while not (Queue.is_empty file) do 
        let u = Queue.pop file in 
        let traite v = 
            if prev.(v) = -1 && c.(u).(v) > f.(u).(v) then begin 
                    prev.(v) <- u;
                    Queue.push v file;
            end 
        in List.iter traite g.(u)
    done;
    prev

let fill c f p prev = 
    let k = ref p in 
    let mini = ref max_int in 
    while (!k <> prev.(!k)) do 
        mini := min (!mini) (c.(prev.(!k)).(!k) - f.(prev.(!k)).(!k));
        k := prev.(!k);
    done;

    k := p;
    while (!k <> prev.(!k)) do 
        f.(prev.(!k)).(!k) <- f.(prev.(!k)).(!k) + (!mini);
        f.(!k).(prev.(!k)) <- f.(!k).(prev.(!k)) - (!mini);
        k := prev.(!k);
    done

let max_flow g s p =
        let n = len g in 
        let c = Array.make_matrix n n 0 in 
        let f = Array.make_matrix n n 0 in 
        
        for i = 0 to n-1 do 
            let traite i j = c.(i).(j) <- 1 in
            List.iter (traite i) g.(i) 
        done;

        let rec loop () = match augm c f g s p with 
        | prev when prev.(p) = -1 -> f,c
        | prev -> fill c f p prev ; loop()
    in loop() 


(* coupe minimale pour un graphe non orienté, en 2 partie contenant s d'une part et p d'autre part *)
let min_cut g s p = 
        let n = len g in 
        let f,c = max_flow g s p in 
        let file = Queue.create () in 
        let vue = Array.make n true in 
        let l = ref [] in 

        Queue.push s file;
        vue.(s) <- false;
        while not (Queue.is_empty file) do 
            let u = Queue.pop file in 
            let traite v = 
                if vue.(v)  && f.(u).(v) <> c.(u).(v) then begin 
                        vue.(v) <- false;
                        Queue.push v file;
                end 
            in 
            List.iter traite g.(u)
        done;
        
        for i = 0 to n-1 do 
            if not (vue.(i)) then begin 
                let valid j = f.(i).(j) = c.(i).(j) && vue.(j) in 
                let l' = List.filter valid g.(i) |> List.map (fun j -> i,j) in 
                l := l'@(!l);
            end 
        done;

        printf "%d\n" (List.length (!l));
        List.iter (fun (i,j) -> printf "%d %d\n" i j) (!l)


(**
   @param n Le nombre de villes
   @param m Le nombre d'aqueducs entre les villes
   @param l Les aqueducs
   @param h La ville de Rome
   @param t La ville de Tivoli
*)
    (** TODO Afficher le nombre minimal d'aqueducs qu'il faudrait couper pour
    isoler totalement les villes de Rome et de Tivoli, ainsi qu'une proposition
    des aqueducs à détruire. *)
    
  
  let () =
    let n = read_int () in
    let m = read_int () in
    let l = List.init m (fun _ -> read_line () |> fun x -> if x = "" then [] else String.split_on_char ' ' x |> List.rev_map int_of_string |> List.rev ) in
    let h = read_int () in
    let t = read_int () in

    let g = Array.make n [] in 
    let traite l = match l with 
    | [a;b] -> g.(a) <- (b)::g.(a);g.(b) <- (a)::g.(b)
    | _ -> () in 
    List.iter traite l;

    min_cut g h t 
  



