open Images;;
open OImages;;
open Info;;
open Graphics;;
open Graphic_image;;

let seuil = 1.0;; (* Seuil histogramme *)

let transformer image = (*Transforme en matrice de pixels une image *)
  let w, h = (image#width, image#height) in
  let img_mat = (Array.make_matrix h w 0) in
    for i = 0 to (w - 1) do
      for j = 0 to (h - 1) do
	let pixel = image#get i j in
          img_mat.(j).(i) <- (Graphics.rgb pixel.r pixel.g pixel.b)
      done;
    done;
    img_mat
;;

let histogramme img = (* histogramme projeté sur x *)
	let h = Array.length img in
	let w = Array.length img.(0) in
	let histo = Array.make w 0. in

	for x = 0 to (w - 1) do
		for y = 0 to (h - 1) do
			histo.(x)<- histo.(x) +. float_of_int( (0xFFFFFF - img.(y).(x)) land 0xFF);
		done;

		histo.(x) <- (ceil ((histo.(x) /. (float_of_int h)) *. 100. /. 255.) *. 100.) /. 100.; (* Moyenne de noir sur la colonne *)
	done;

	histo
;;


let reduction img = (* Enleve les bordures blanches *)
	let h = Array.length img in
	let w = Array.length img.(0) in
	let l1 = ref 0 in
	let l2 = ref (w - 1) in
	let h1 = ref 0 in
	let h2 = ref (h - 1) in

	(try (* Tant que la premiere colonne est blanche *)
	for i = 0 to (w - 1) do
		for j = 0 to (h - 1) do
			if img.(j).(i) <> 0xFFFFFF then raise Exit;
		done;
		l1 := !l1 + 1
	done;
	with Exit -> ());

  (try (* Tant que la derniere colonne est blanche *)
  for i = (w - 1) downto 0 do
    for j = 0 to (h - 1) do
      if img.(j).(i) <> 0xFFFFFF then raise Exit;
    done;
    l2 := !l2 - 1 
  done;
  with Exit -> ());

  (try (* Tant que la premiere ligne est blanche *)
  for j = 0 to (h - 1) do
    for i = 0 to (w - 1) do
      if img.(j).(i) <> 0xFFFFFF then raise Exit;
    done;
    h1 := !h1 + 1
  done;
  with Exit -> ());

  (try (* Tant que la derniere ligne est blanche *)
  for j = (h - 1) downto 0 do
    for i = 0 to (w - 1) do
      if img.(j).(i) <> 0xFFFFFF then raise Exit;
    done;
    h2 := !h2 - 1
  done;
  with Exit -> ());

	if (!h2 < !h1) then failwith "h2 < h1: image blanche ?";
	if (!l2 < !l1) then failwith "l2 < l1: image blanche ?";

	let imageFinale = Array.make_matrix (!h2 - !h1 + 1) (!l2 - !l1 + 1) 0 in (* Construit la matrice sans les bordures blanches *)
	for i = 0 to (!l2 - !l1) do
		for j = 0 to (!h2 - !h1) do
			imageFinale.(j).(i)<-img.(j + !h1).(i + !l1);
		done;
	done;

	imageFinale
;;

let extraire img a b = (* Extrait un chiffre d'une suite de chiffres *)
	let h = Array.length img in
	let image = Array.make_matrix h (b - a + 1) 0 in
	
	for j = 0 to (h - 1) do
		for i = a to b do
			image.(j).(i - a) <- img.(j).(i);
		done;
	done;

	image
;;

let blocs img bH bV = (* Construit une matrice de blocs avec le pourcentage de noir dans chaque bloc *)
(* bH: nombre de blocs horizontaux, bV: blocs verticaux *)
  let w, h = (Array.length img.(0), Array.length img) in
  let tableauBlocs = Array.make_matrix bV bH 0. in
  let x = ref 0 and y = ref 0 and a = ref 0 and c = ref 0 in

  for j = 0 to (h - 1) do
    for i = 0 to (w - 1) do
			(* on calcule dans quel ligne (x) on se situe dans la matrice de blocs *)
      c := int_of_float(ceil(float_of_int(w) /. float_of_int(bH))); (* nombre de pixels par bloc *)
      a := w - (!c - 1)*bH;
      if ((i + 1) <= (!c)*(!a)) then x := (i / (!c)) else x := ( ((i - (!a)*(!c)) / (!c - 1))) + (!a);

			(* ... puis dans quelle colonne (y) *)
      c := int_of_float(ceil(float_of_int(h) /. float_of_int(bV)));
      a := h - (!c - 1)*bV;
      if ((j + 1) <= (!c)*(!a)) then y := (j / (!c)) else y := ( ((j - (!a)*(!c)) / (!c - 1))) + (!a);

			(* Puis on ajoute le taux de noir du pixel courant *)
			(* Comme on est dans le gris, on filtre land 0xFF car le code couleur est répétitif: 0xYZYZYZ *)
      tableauBlocs.(!y).(!x)<- tableauBlocs.(!y).(!x) +. float_of_int( (0xFFFFFF - img.(j).(i)) land 0xFF);

    done;
  done;

	(* On fait la moyenne (on divise par le nombre de pixels correspondant à chaque bloc *)
  for j = 0 to (bV - 1) do
    for i = 0 to (bH - 1) do
			(* On calcule le nombre de pixels en abscisse correspondant à ce bloc *)
      c := int_of_float(ceil(float_of_int(w) /. float_of_int(bH)));
      a := w - (!c - 1)*bH;
      if (i+1) <= (!a) then x := !c else x := !c - 1;

			(* ... puis en ordonnées *)
      c := int_of_float(ceil(float_of_int(h) /. float_of_int(bV)));
      a := h - (!c - 1)*bV;
      if (j+1) <= (!a) then y := !c else y := !c - 1;

			(* On divise pour avoir la moyenne *)
      tableauBlocs.(j).(i) <- (ceil ((((tableauBlocs.(j).(i) /. float_of_int( !x * !y )) *. 100.) /. 255.)*.100.))/.100.;
		(* (ceil number *. 100.) /. 100. permet d'arrondir au deuxième chiffre après la virgule *)
    done;
  done;

  tableauBlocs

;;

let fonctionSeuil a =
  if a >= 0. then 1 else (-1);;

let sommation poids entree s =
	let x, y = (Array.length poids.(0), Array.length poids) in
	(* entree correspond a la matrice de pourcentage de noir *)

  let r = ref 0. in

	(* somme pondérée *)
  for j = 0 to (y - 1) do
		for i = 0 to (x - 1) do
    r := !r +. poids.(j).(i) *. entree.(j).(i);
		done;
  done;

  r := !r -. s;
  !r
;;

let rec repertorierExemples n p nombre caractere =
	(* Créé une liste de [(matrice de couleurs, caractere)] pour un caractère donné *)
	(* nombre represente le nombre d'exemples par caractère *)
	
	if nombre = 0 then [] else
	let img = reduction( transformer( OImages.rgb24 (OImages.load ("images/"^caractere^"/"^string_of_int nombre^".png") []))) in
	let img = blocs img p n in
		(img , caractere)::(repertorierExemples n p (nombre - 1) caractere);
;;

let creerExemples n p nombre =
	(* Créé la liste d'exemples d'apprentissage *)
	let liste = ref [] in
	for i = 0 to 9 do
		liste := !liste@(repertorierExemples n p nombre (string_of_int i));
	done;

	!liste;;

let perceptron forme poids exemples n p =
	(* forme représente le caractère que reconnaît ce perceptron *)

  let reconnus = ref 0 in (* Nombre d'exemples reconnus *)
	let entree = ref [|[||]|] in
  let resultat = ref 0 in
	let caractere = ref "" in
  let objectif = ref 0 in
	let liste = ref exemples in (* La liste des exemples *)
	
	while !reconnus <> List.length exemples do
    match !liste with
      |[] -> liste := exemples; (* Dès qu'on a vu tous les exemples, on recommence jusqu'à ce qu'ils soient tous reconnus d'affilée *)
      |t::q ->  entree := fst t; caractere := snd t; (* entree correspond à la matrice de couleurs *)
				if !caractere = forme then objectif := 1 else objectif := (-1);
        resultat := fonctionSeuil(sommation poids !entree 0.);
        if (!resultat <> !objectif) then (* Si le caractère n'est pas reconnu, on modifie les poids *)
          begin
          reconnus := 0; (* Il faut revoir tous les exemples *)

					(* On modifie les poids selon la méthode du perceptron *)
					for j = 0 to (n - 1) do
    				for i = 0 to (p - 1) do
							poids.(j).(i) <- poids.(j).(i) +. (float_of_int(!objectif) -. float_of_int(!resultat)) *. (!entree.(j).(i));
    				done;
  				done;

          end
         else reconnus := !reconnus + 1; (* Si le caractère est reconnu, on incrémente *)

        liste := q;
  done;

  poids;;

let n = 9 and p = 8;; (* Nombre de blocs par colonne et par ligne *)
let exemple = creerExemples n p 19;; (* Créé la base d'exemples *)

let stats = Array.make 19 0.;;
let poids = Array.make 10 (Array.make_matrix n p 0.);; (* On initialise les poids à 0 *)

for chiffre = 0 to 9 do (* On apprend chaque chiffre *)
	poids.(chiffre) <- perceptron (string_of_int chiffre) (Array.make_matrix n p 0.) exemple n p;
done;;

let resultats imageTest = (* Résultat des sommes pondérées pour le chiffre à tester *)
	for i = 0 to 9 do
		stats.(i)<-sommation poids.(i) imageTest 0.;
	done
;;

let quelChiffre statistiques = (* Renvoie le chiffre reconnu *)
	let r = ref "aucun" in
	let m = ref 0. in

	for chiffre = 0 to 9 do
		if statistiques.(chiffre) > !m then begin
			m := statistiques.(chiffre);
			r := string_of_int(chiffre);
		end;
	done;

	!r;;

let tester image a b = (* Teste un chiffre de l'image *)
	let img = extraire image a b in (* Extrait le chiffre situé entre l'abscisse a et b *)
	let img = reduction img in
	let noir = blocs img p n in (* Matrice de couleurs *)

	resultats noir;

	let a = quelChiffre stats in
		print_string a; (* Affiche le chiffre reconnu *)
;;

let phaseTest fichier = (* Phase de test: on teste une nouvelle image, que l'on va découper en caractères *)
  let image = (OImages.rgb24 (OImages.load fichier [])) in (* Charge l'image de test *)
  let img = (transformer image) in

  let histo = histogramme img in

  let a = ref (-1) in  
  let b = ref (-1) in

  for i = 0 to (Array.length img.(0) - 1) do
    if (histo.(i) > seuil) then (* Si le pourcentage de noir de la colonne dépasse le seuil *)
			begin
				if (!a = (-1)) then a := i;
			end
    else 
      begin
      if (!a <> (-1)) then begin
        b := i - 1; (* On décale le curseur de droite *)
        tester img !a !b;
        a := (-1);
        b := (-1);
        end;
    end;
  done;

	if (!a <> (-1)) then tester img (!a) (Array.length img.(0) - 1); (* Si l'image contient au moins un chiffre on teste *)
;;



while true do
  phaseTest "test.png";
  let _ = read_line () in ()

done;;



