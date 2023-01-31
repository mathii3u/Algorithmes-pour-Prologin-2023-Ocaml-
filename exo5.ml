type 'a arbre = 
| V 
| N of 'a * 'a arbre * 'a arbre 

(* Solution 1 
l'idée : on sait facilement donner à chaque sommet son numéro dans le parcours en largeur, le probleme est de reconstruire l'arbre
, on règle ce problème en donnant un indentifiant unique à chaque noeud (fonction numérote), il suffit d'attribuer à chaque identifiant son numéro (boucle while),
il reste alors à remplacer l'identifiant par le numéro dans le parcours en largeur (fonction remplace) 
*)


let numerote t =
    let i =ref 0 in 
    let rec num t' = match t' with 
    | V -> V
    | N(x,g,d) -> let g' = num g in
                  let d' = num d in 
                  incr i; 
                  N((x,(!i)-1),g',d') 
    in
    let t' = num t in 
    t', !i

let rec remplace t arr = match t with 
| V -> V 
| N((x,i),g,d) -> N( (x,arr.(i)) , remplace g arr , remplace d arr)

let solve1 t = 
    let t' , n = numerote t in 
    let arr = Array.make n 0 in 
    let file = Queue.create () in 
    let count = ref 0 in 
    Queue.push t' file;
    while not (Queue.is_empty file) do 
        match Queue.pop file with 
        | V -> ()
        | N((x,i),g,d) -> arr.(i) <- !count;
                          incr count;
                          Queue.push g file;
                          Queue.push d file;
    done;
    remplace t' arr


    

(* Solution 2 
on utilise la fonction aux qui à une forêt associe une forêt numérotée et dans l'odre inverse de la foret précédente (ce comprend bien car on pop et on renvoit ajoute à la fin), 
en effet on prend un élément à une profondeur , puis on traite tout le reste de la foret avant de traiter nos sous arbres 

1) N(x,g,d)<- foret  
2) foret <- d <- g
3) g' <- d' <- foret' 
4) foret' <- N((x,i),g',d')

*)


let solve2 a = 
    let rec aux file i = 
      if Queue.is_empty file then Queue.create ()
      else match Queue.pop file with 
           | V -> let file' = (aux file i) in Queue.push V file' ; file'
           | N(x,g,d) -> 
              Queue.push d file ;
              Queue.push g file ;
              let file' = aux file (i+1) in 
              let g' = Queue.pop file' in 
              let d' = Queue.pop file' in 
              Queue.push  ( N((x,i),g',d')) file' ;
              file'
    in 
    let file = Queue.create () in 
    Queue.push a file;
    let file' = aux file 0 in
    Queue.pop file'
 



