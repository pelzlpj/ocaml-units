(* ocaml-units -- a module for handling standard operations on
 *                physical units
 * 
 * Copyright (C) 2007 Paul Pelzl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * Please send feedback, bug reports, patches, etc. to 
 * <pelzlpj@eecs.umich.edu>.
 *)


exception Units_error of string
let units_failwith ss =
   raise (Units_error ss)


let c_of_f ff = {
   Complex.re = ff;
   Complex.im = 0.0;
}

let cpow f1 f2 = Complex.pow (c_of_f f1) (c_of_f f2)

type prefix_t =
   | NoPrefix
   | Yocto
   | Zepto
   | Atto
   | Femto
   | Pico
   | Nano
   | Micro
   | Milli
   | Centi
   | Deci
   | Deka
   | Hecto
   | Kilo
   | Mega
   | Giga
   | Tera
   | Peta
   | Exa
   | Zetta
   | Yotta


(* A unit is viewed as a composite of one or more base units.  The Map structure
 * works well for this purpose; the string representation of a unit is the key,
 * and this key maps to the (float) power of that unit.  This data structure
 * allows nice lookup semantics when multiplying units together, etc. *)
module SMap = Map.Make (String)


(* A unit is defined in terms of a coefficient and an SMap representing a bunch
 * of dimensionally-orthogonal base units. *)
type unit_def_t = {
   coeff      : float;
   comp_units : float SMap.t
}


(* A *representation* of a unit involves both the prefix and the string
 * that identifies the base unit. (e.g. Kilo and "g") *)
type unit_rep_t = {
   prefix : prefix_t;
   base   : string
}

   
(* The table of base units maps string representations of units to the preferred
 * SI prefix.  Typically most base units will prefer NoPrefix, but (for example)
 * the SI standard prefers Kilo for the unit 'g'. *)
type base_unit_table_t = prefix_t SMap.t

(* The unit definition table maps string representations of units to
 * sets of base units. *)
type unit_table_t = unit_def_t SMap.t

let empty_unit_table = SMap.empty

(* Multiply a single unit by a set of units, collecting like terms. *)
let mult_aux (unit_str : string) (unit_pow : float) (unit_set : float SMap.t) =
   let existing_pow =
      if SMap.mem unit_str unit_set then
         SMap.find unit_str unit_set
      else
         0.0
   in
   let new_pow = existing_pow +. unit_pow in
   if new_pow <> 0.0 then
      SMap.add unit_str new_pow unit_set
   else
      (* if unit powers happen to cancel exactly, remove them completely *)
      SMap.remove unit_str unit_set


(* Create a new base unit with a preferred SI prefix. *)
let add_base_unit (unit_str : string) (preferred_prefix : prefix_t) 
(base_table : base_unit_table_t) (table : unit_table_t) =
   if SMap.mem unit_str base_table then
      units_failwith ("base unit \"" ^ unit_str ^ "\" already declared")
   else
      (* We also enter this base unit into the main unit definion table
       * as an A->A mapping, so that base units can be "expanded" just
       * like any other unit.  (Base units expand to themselves.) *)
      let unit_def = {
         coeff = 1.0; 
         comp_units = SMap.add unit_str 1.0 SMap.empty
      } in (
         SMap.add unit_str preferred_prefix base_table,
         SMap.add unit_str unit_def table
      )


(* Given a leading coefficient and a set of unit definitions (mappings to base
 * units) considered to be multiplied together, compute an equivalent singular
 * unit definition (collecting like terms).
 *
 * In effect, this is doing an operation like 
 * (3)*(5_m/s)*(10_m^2)*(0.5_g) -> 75_m^3*g/s . *)
let collect (terms : float * (unit_def_t SMap.t)) : unit_def_t =
   let (leading_coeff, uncollected) = terms in
   let collect_single (unit_str : string) (unit_def : unit_def_t)
   (collection : unit_def_t) = {
         coeff      = collection.coeff *. unit_def.coeff;
         comp_units = SMap.fold mult_aux unit_def.comp_units 
                      collection.comp_units
   }
   in
   SMap.fold collect_single uncollected 
   {coeff = leading_coeff; comp_units = SMap.empty}


(* Given a unit definition (mapping to other known units), expand
 * out all the known units in terms of base units.  The leading
 * coefficient is prepended.
 *
 * In effect, this is doing an operation like
 * 0.1_N*Hz^2 -> (100)*(1_g*m/s^2)*(1_s^-2) . *)
let expand (unit_def : unit_def_t) (table : unit_table_t) : 
(float * (unit_def_t SMap.t)) =
   let expand_single (unit_str : string) (unit_pow : float) =
      let base_def = SMap.find unit_str table in
      (* The powers of all the base units need to be multiplied
       * by the power of this unit, and the coefficient needs
       * to be exponentiated. *)
      let exponentiate base_unit_pow = base_unit_pow *. unit_pow in {  
         coeff      = base_def.coeff ** unit_pow;
         comp_units = SMap.map exponentiate base_def.comp_units
      }
   in
   let base_expansion = SMap.mapi expand_single unit_def.comp_units in
   (unit_def.coeff, base_expansion)


(* Add a unit definition to the table.  The definition is immediately 
 * recast in terms of base units only, and is stored in this form. *)
let add_unit (unit_str : string) (unit_def : unit_def_t) 
(table : unit_table_t) : unit_table_t =
   if SMap.mem unit_str table then
      units_failwith ("unit \"" ^ unit_str ^ "\" already declared")
   else
      begin try
         let unit_base_def = collect (expand unit_def table) in
         SMap.add unit_str unit_base_def table
      with Not_found ->
         units_failwith ("unit \"" ^ unit_str ^ "\" depends on an undefined unit")
      end


let prefix_string_table = Hashtbl.create 25;;
Hashtbl.add prefix_string_table "y" Yocto;
Hashtbl.add prefix_string_table "z" Zepto;
Hashtbl.add prefix_string_table "a" Atto;
Hashtbl.add prefix_string_table "f" Femto;
Hashtbl.add prefix_string_table "p" Pico;
Hashtbl.add prefix_string_table "n" Nano;
Hashtbl.add prefix_string_table "u" Micro;
Hashtbl.add prefix_string_table "m" Milli;
Hashtbl.add prefix_string_table "c" Centi;
Hashtbl.add prefix_string_table "d" Deci;
Hashtbl.add prefix_string_table "da" Deka;
Hashtbl.add prefix_string_table "h" Hecto;
Hashtbl.add prefix_string_table "k" Kilo;
Hashtbl.add prefix_string_table "M" Mega;
Hashtbl.add prefix_string_table "G" Giga;
Hashtbl.add prefix_string_table "T" Tera;
Hashtbl.add prefix_string_table "P" Peta;
Hashtbl.add prefix_string_table "E" Exa;
Hashtbl.add prefix_string_table "Z" Zetta;
Hashtbl.add prefix_string_table "Y" Yotta;;

let prefix_list = ["y"; "z"; "a"; "f"; "p"; "n"; "u"; "m"; 
"c"; "da"; "d"; "h"; "k"; "M"; "G"; "T"; "P"; "E"; "Z"; "Y"];;

let prefix_value (pre : prefix_t) = 
   match pre with
   | NoPrefix -> 1.0
   | Yocto    -> 1e-24
   | Zepto    -> 1e-21
   | Atto     -> 1e-18
   | Femto    -> 1e-15
   | Pico     -> 1e-12
   | Nano     -> 1e-9
   | Micro    -> 1e-6
   | Milli    -> 1e-3
   | Centi    -> 0.01
   | Deci     -> 0.1
   | Deka     -> 10.0
   | Hecto    -> 100.0
   | Kilo     -> 1e3
   | Mega     -> 1e6
   | Giga     -> 1e9
   | Tera     -> 1e12
   | Peta     -> 1e15
   | Exa      -> 1e18
   | Zetta    -> 1e21
   | Yotta    -> 1e24;;

let string_of_prefix (pre : prefix_t) = 
   match pre with
   | NoPrefix -> ""
   | Yocto    -> "y"
   | Zepto    -> "z"
   | Atto     -> "a"
   | Femto    -> "f"
   | Pico     -> "p"
   | Nano     -> "n"
   | Micro    -> "u"
   | Milli    -> "m"
   | Centi    -> "c"
   | Deci     -> "d"
   | Deka     -> "da"
   | Hecto    -> "h"
   | Kilo     -> "k"
   | Mega     -> "M"
   | Giga     -> "G"
   | Tera     -> "T"
   | Peta     -> "P"
   | Exa      -> "E"
   | Zetta    -> "Z"
   | Yotta    -> "Y";;


(* Is 'pre' a prefix of 'word'? *)
let is_prefix pre word =
   if String.length word >= String.length pre then
      pre = (String.sub word 0 (String.length pre))
   else
      false;;


(* Given a string like "kg", try to parse it as the representation of a unit
 * with prefix. *)
let unit_rep_of_string (ss : string) (table : unit_table_t) : unit_rep_t =
   let rec test_prefixes plist =
      match plist with
      | [] ->
         units_failwith "failed to match a unit prefix"
      | head :: tail ->
         if is_prefix head ss then
            let suffix = Str.string_after ss (String.length head) in
            if SMap.mem suffix table then
               let si_prefix = Hashtbl.find prefix_string_table head in {
                  prefix = si_prefix;
                  base   = suffix
               }
            else
               test_prefixes tail
         else
            test_prefixes tail
   in
   (* Look first for matches with no prefix, so we can catch
    * units like "mmHg" *)
   if SMap.mem ss table then {
      prefix = NoPrefix;
      base   = ss
   } else
      begin try
         test_prefixes prefix_list
      with Units_error _ ->
         units_failwith ("unrecognized unit \"" ^ ss ^ "\"")
      end


(* Is this string a known unit (possibly with a prefix)? *)
let is_known_unit (ss : string) (table : unit_table_t) =
   try
      let _ = unit_rep_of_string ss table in
      true
   with _ ->
      false


(* Convert a string into an appropriate set of units.  Units should be
 * multiplied with '*', divided with '/', and raised to powers with '^'.
 * So the following would be a valid example: "kg^2*m/s^2/h*ft^-2". *)
let units_of_string (ss : string) (table : unit_table_t) : float SMap.t =
   let mult_regex = Str.regexp "\\*"
   and div_regex  = Str.regexp "/" 
   and pow_regex  = Str.regexp "\\^" in
   (* Given a string like "mm^3" parse it into a unit representation
    * and floating-point power. *)
   let unit_of_term (tt : string) : (string * float) =
      match Str.split pow_regex tt with
      | [] -> 
         units_failwith "empty power split in unit_of_string()"
      | unit_str :: [] ->
         if String.contains tt '^' then
            units_failwith "illegal unit exponentiation syntax"
         else if is_known_unit unit_str table then
            (unit_str, 1.0)
         else
            units_failwith ("unrecognized unit \"" ^ unit_str ^ "\"")
      | unit_str :: pow_str :: [] ->
         let pow =
            try float_of_string pow_str
            with _ -> units_failwith ("illegal unit power: \"" ^ pow_str ^ "\"")
         in
         if is_known_unit unit_str table then
            (unit_str, pow)
         else
            units_failwith ("unrecognized unit \"" ^ unit_str ^ "\"")
      | _ -> 
         units_failwith ("too many exponentiations in unit term \"" ^ tt ^ "\"")
   in
   let rec build_unit_set mlist set =
      match mlist with
      | [] ->
         set
      | head :: tail ->
         let div_list = Str.split div_regex head in
         if List.length div_list = 0 then
            units_failwith "empty unit string"
         else if List.length div_list = 1 && String.contains head '/' then
            units_failwith "illegal unit division syntax"
         else
            (* the tail of div_list consists of terms which followed division
             * operators, so we negate the exponents before multiplying out
             * these terms *)
            let mult_inverse set div_str =
               let (div_unit_str, div_unit_pow) = unit_of_term div_str in
               mult_aux div_unit_str (~-. div_unit_pow) set
            in
            let set_with_inverses = 
               List.fold_left mult_inverse set (List.tl div_list)
            in
            (* the head of div_list is multiplied, not divided, because it
             * preceded a division operator *)
            let (mult_unit_str, mult_unit_pow) = unit_of_term (List.hd div_list) in
            let next_set = mult_aux mult_unit_str mult_unit_pow set_with_inverses in
            build_unit_set tail next_set
   in
   let mult_terms = Str.split mult_regex ss in
   build_unit_set mult_terms SMap.empty
            

(* Generate a string representation of a set of units.
 * FIXME: this will generate an alphabetical unit ordering.
 * That's probably a little too simplistic. *)
let string_of_units (u : float SMap.t) : string =
   let gen_string unit_str unit_pow str_list =
      if unit_pow <> 0.0 then
         let str_rep = 
            if unit_pow <> 1.0 then
               Printf.sprintf "%s^%g" unit_str unit_pow
            else
               unit_str
         in
         str_rep :: str_list
      else
         str_list
   in
   let str_list_rev = SMap.fold gen_string u [] in
   String.concat "*" (List.rev str_list_rev)


