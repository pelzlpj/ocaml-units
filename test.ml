
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
   for iter = 0 to 100 do
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


(* exceedingly contrived example to test expansion and grouping of units *)
let s1  = "g^3*lb/oz*slug^-1/ton^2*slug/lb*tonl^4*Mtonm^-3*ct/gr^2*tonm^2"
and s2  = "*km^3*ft/in^2*mi^-4*yd^-2/AU/pc*Ang^4*furlong*yd^5/pt"
and s3  = "*s^-3*min*hr^2*day*min*nyr*min^-2"
and s4  = "*A^4"
and s5  = "*R^2/K"
and s6  = "*mN^3/lbf/dyn^2/kip^3*lbf/lbf^2*Gkip^3"
and s7  = "*dJ/merg^2/BTU^3*erg*cal^3*eV*eV"
and s8  = "*Hz^2"
and s9  = "*W^-3*hp^3"
and s10 = "*mPa*atm^0.0*bar^3/mmHg^2/inHg"
and s11 = "*V^-2"
and s12 = "*Ohm^2"
and s13 = "*F^2*F^-3"
and s14 = "*H"
and s15 = "*T^4/G^3"
and s16 = "*Wb^-1/Mx*nWb^2" in
let total_s = 
   s1 ^ s2 ^ s3 ^ s4 ^ s5 ^ s6 ^ s7 ^ s8 ^ s9 ^ s10 ^ s11 ^ 
  s12 ^ s13 ^ s14 ^ s15 ^ s16 
in
let contrived = unit_of_string total_s in
let grouped_units = group_units contrived false in
let gu_str = string_of_unit grouped_units.factors in
print_endline ("grouped      = " ^ (string_of_float grouped_units.coeff) ^ gu_str);
print_endline ("expected     = 1.76804093547e-37G*H*F^-1*Ohm^2*V^-2*mPa*Hz^2*eV^2*lbf^-1*K*A^4*tonm^2*km^3*day");
let stand_units = standardize_units contrived in
print_endline ("standardized = " ^ (string_of_float stand_units.coeff) ^ 
(string_of_unit stand_units.factors));
print_endline ("expected     = 8.81537531531e-63K*A^-3*kg^7*m^9*s^-13");;


(* arch-tag: DO_NOT_CHANGE_4d4f58ad-d94a-40ad-9582-4782ed4828a2 *)
