
(* testing for Units code *)

open Units;;

let masses = [| "g"; "lb"; "oz"; "slug"; "ton"; "tonl"; "tonm"; "ct"; "gr" |];;
let dists  = [| "m"; "ft"; "in"; "yd"; "mi"; "pc"; "AU"; "Ang"; "furlong"; "pt";
"pica"; "nmi"; "lyr" |];;
let times  = [| "s"; "min"; "hr"; "day"; "yr" |];;
let currs  = [| "A" |];;
let temps  = [| "K"; "R" |];;
let forces = [| "N"; "lbf"; "dyn"; "kip" |];;
let enrgs  = [| "J"; "erg"; "BTU"; "cal"; "eV" |];;
let freqs  = [| "Hz" |];;
let pows   = [| "W"; "hp" |];;
let press  = [| "Pa"; "atm"; "bar"; "mmHg"; "inHg" |];;
let volts  = [| "V" |];;
let resis  = [| "Ohm" |];;
let caps   = [| "F" |];;
let inds   = [| "H" |];;
let mags   = [| "T"; "G" |];;
let fluxs  = [| "Wb"; "Mx" |];;

let all_groups = [| masses; dists; times; currs; temps; forces; enrgs; freqs;
pows; press; volts; resis; caps; inds; mags; fluxs |];;

let shuffle arr = 
   for k = 0 to 1000 do
      let i1 = Random.int (Array.length arr) in
      let i2 = Random.int (Array.length arr) in
      let temp = arr.(i1) in
      arr.(i1) <- arr.(i2);
      arr.(i2) <- temp
   done;;

(* For each group of consistent units, compute conversion factors from each
 * element to the next and take the product; should be very nearly 1.0.
 * Repeat many times, shuffling the order of the units each iteration. *)
for group = 0 to pred (Array.length all_groups) do
   Printf.printf "Testing unit group %d/%d\n" (succ group) (Array.length all_groups);
   flush stdout;
   for iter = 0 to 1000 do
      shuffle all_groups.(group);
      let cfactor = ref 1.0 in
      let len = Array.length all_groups.(group) in
      begin try
         for i = 0 to pred len do
            let u1 = unit_of_string all_groups.(group).(i)
            and u2 = unit_of_string all_groups.(group).((i + 1) mod len) in
            cfactor := !cfactor *. (conversion_factor u1 u2)
         done
      with Units_error s ->
         Printf.printf "Caught Units exception: \"%s\"\n" s;
         print_endline "units_array:";
         Array.iter print_endline all_groups.(group);
         failwith s
      end;
      if abs_float (!cfactor -. 1.0) > 1e-15 then begin
         Printf.printf "Encountered bad round-trip conversion factor: %.18g\n"
         !cfactor;
         print_endline "units array:";
         Array.iter print_endline all_groups.(group);
         failwith ""
      end else
         ()
   done
done;;









(* arch-tag: DO_NOT_CHANGE_4d4f58ad-d94a-40ad-9582-4782ed4828a2 *)
