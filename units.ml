

exception Unit_error of string;;
let unit_failwith ss =
   raise (Unit_error ss);;

type mass_fund_t = 
   | Gram
   | Pound;;

type distance_fund_t =
   | Meter
   | Foot
   | Mile;;

type time_fund_t =
   | Second
   | Minute
   | Hour;;

type current_fund_t = Ampere;;
   
type temperature_fund_t = Kelvin;;

type composite_t =
   | Newton
   | Coulomb
   | Hertz

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
   | Yotta;;


type mass_unit_t        = prefix_t * mass_fund_t;;
type distance_unit_t    = prefix_t * distance_fund_t;;
type time_unit_t        = prefix_t * time_fund_t;;
type current_unit_t     = prefix_t * current_fund_t;;
type temperature_unit_t = prefix_t * temperature_fund_t;;
type composite_unit_t   = prefix_t * composite_t;;

type fund_unit_t =
   | Mass of mass_unit_t
   | Distance of distance_unit_t
   | Time of time_unit_t
   | Current of current_unit_t
   | Temperature of temperature_unit_t
   | Composite of composite_unit_t

type unit_component_power_t = {
   component : fund_unit_t;
   power     : float
};;

type unit_t = {
   coeff      : float;
   components : unit_component_power_t list
};;


let unit_string_table = Hashtbl.create 50;;
Hashtbl.add unit_string_table "g"    ( Mass        ( NoPrefix, Gram));
Hashtbl.add unit_string_table "lb"   ( Mass        ( NoPrefix, Pound));
Hashtbl.add unit_string_table "m"    ( Distance    ( NoPrefix, Meter));
Hashtbl.add unit_string_table "ft"   ( Distance    ( NoPrefix, Foot));
Hashtbl.add unit_string_table "mi"   ( Distance    ( NoPrefix, Mile));
Hashtbl.add unit_string_table "s"    ( Time        ( NoPrefix, Second));
Hashtbl.add unit_string_table "min"  ( Time        ( NoPrefix, Minute));
Hashtbl.add unit_string_table "h"    ( Time        ( NoPrefix, Hour));
Hashtbl.add unit_string_table "A"    ( Current     ( NoPrefix, Ampere));
Hashtbl.add unit_string_table "K"    ( Temperature ( NoPrefix, Kelvin));
Hashtbl.add unit_string_table "N"    ( Composite   ( NoPrefix, Newton));
Hashtbl.add unit_string_table "C"    ( Composite   ( NoPrefix, Coulomb));
Hashtbl.add unit_string_table "Hz"   ( Composite   ( NoPrefix, Hertz));;

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


(* expand out composite components into fundamental units of 
 * space, time, mass, current *)
let expand_component (uc : unit_component_power_t) =
   match uc.component with
   | Composite (pre, c) ->
      begin match c with
      | Newton -> {
         coeff = prefix_value pre;
         components = [
            {component = Mass (Kilo, Gram); power = 1.0};
            {component = Distance (NoPrefix, Meter); power = 1.0};
            {component = Time (NoPrefix, Second); power = ~-. 2.0}
         ] }
      | Coulomb -> {
         coeff = prefix_value pre;
         components = [
            {component = Current (NoPrefix, Ampere); power = 1.0};
            {component = Time (NoPrefix, Second); power = 1.0}
         ] }
      | Hertz -> {
         coeff = prefix_value pre;
         components = [
            {component = Time (NoPrefix, Second); power = ~-. 1.0}
         ] }
      end
   | _ -> {
      coeff = 1.0;
      components = [ uc ]
      };;


(* compute conversion factors between fundamental units of mass *)
let rec convert_mass (m1 : mass_fund_t) (m2 : mass_fund_t) =
   match m1 with
   | Gram ->
      begin match m2 with
      | Gram  -> 1.0
      | Pound -> 0.00220462262
      end
   | Pound ->
      begin match m2 with
      | Pound -> 1.0
      | _     -> 1.0 /. (convert_mass m2 m1)
      end;;

(* compute conversion factors between fundamental units of distance *)
let rec convert_distance (d1 : distance_fund_t) (d2 : distance_fund_t) =
   match d1 with
   | Meter ->
      begin match d2 with
      | Meter -> 1.0
      | Foot  -> 3.2808399
      | Mile  -> 0.000621371192
      end
   | Foot ->
      begin match d2 with
      | Foot -> 1.0
      | Mile -> 1.0 /. 5280.0
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | Mile ->
      begin match d2 with
      | Mile -> 1.0
      | _    -> 1.0 /. (convert_distance d2 d1)
      end;;

(* compute conversion factors between fundamental units of time *)
let rec convert_time (t1 : time_fund_t) (t2 : time_fund_t) =
   match t1 with
   | Second ->
      begin match t2 with
      | Second -> 1.0
      | Minute -> 1.0 /. 60.0
      | Hour   -> 1.0 /. 3600.0
      end
   | Minute ->
      begin match t2 with
      | Minute -> 1.0
      | Hour   -> 1.0 /. 60.0
      | _      -> 1.0 /. (convert_time t2 t1)
      end
   | Hour ->
      begin match t2 with
      | Hour -> 1.0
      | _    -> 1.0 /. (convert_time t2 t1)
      end;;

(* compute conversion factors between fundamental units of current *)
let rec convert_current (c1 : current_fund_t) (c2 : current_fund_t) =
   match c1 with
   | Ampere ->
      begin match c2 with
      | Ampere -> 1.0
      end;;

(* compute conversion factors between fundamental units of temperature *)
let rec convert_temperature (t1 : temperature_fund_t) 
(t2 : temperature_fund_t) =
   match t1 with
   | Kelvin ->
      begin match t2 with
      | Kelvin  -> 1.0
      end;;

(* compute conversion factors between composite units *)
let rec convert_composite (c1 : composite_t) (c2 : composite_t) =
   match c1 with
   | Newton ->
      begin match c2 with
      | Newton -> 1.0
      | _ -> unit_failwith "Inconsistent composite units"
      end
   | Coulomb ->
      begin match c2 with
      | Coulomb -> 1.0
      | _ -> unit_failwith "Inconsistent composite units"
      end
   | Hertz ->
      begin match c2 with
      | Hertz -> 1.0
      | _ -> unit_failwith "Inconsistent composite units"
      end;;
   

(* compute the conversion factor between two generic unit components *)
let convert_component (u1 : unit_component_power_t) (u2 : unit_component_power_t) =
   if u1.power <> u2.power then begin
      Printf.printf "pow1 = %f, pow2 = %f\n" u1.power u2.power;
      unit_failwith "Units have inconsistent power"
   end else
      match u1.component with
      | Mass (pre1, m1) ->
         begin match u2.component with
         | Mass (pre2, m2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_mass m1 m2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end
      | Distance (pre1, d1) ->
         begin match u2.component with
         | Distance (pre2, d2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_distance d1 d2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end
      | Time (pre1, t1) ->
         begin match u2.component with
         | Time (pre2, t2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_time t1 t2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end
      | Current (pre1, c1) ->
         begin match u2.component with
         | Current (pre2, c2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_current c1 c2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end
      | Temperature (pre1, t1) ->
         begin match u2.component with
         | Temperature (pre2, t2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_temperature t1 t2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end
      | Composite (pre1, c1) ->
         begin match u2.component with
         | Composite (pre2, c2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_composite c1 c2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end


(* collect units together, i.e. kg*m^2*kg -> kg^2*mg^2.
 * Mismatched units of mass, distance, etc. will be rematched, i.e.
 * s*min -> 60 s^2.  If 'standardize' = true, then units of mass, distance,
 * etc. will be converted to fundamental units (kg, m, s, etc.). *)
let group_units (ulist : unit_t) (standardize : bool) =
   let total_mass : unit_component_power_t option ref        = ref None
   and total_distance : unit_component_power_t option ref    = ref None
   and total_time : unit_component_power_t option ref        = ref None
   and total_current : unit_component_power_t option ref     = ref None
   and total_temperature : unit_component_power_t option ref = ref None
   and total_composite : unit_component_power_t option ref   = ref None
   and total_coeff                                           = ref 1.0 in
   let process_component uc =
      let process_component_aux total target_comp =
         begin match !total with
         | None ->
            if standardize then begin
               let target_unit = {
                  component = target_comp;
                  power     = uc.power
               } in
               let conversion = convert_component uc target_unit in
               total_coeff := !total_coeff *. conversion;
               total       := Some target_unit
            end else
               total := Some uc
         | Some tot ->
               let conversion = convert_component uc
               {component = tot.component; power = uc.power} in
               total_coeff := !total_coeff *. conversion;
               let p = tot.power +. uc.power in
               if p = 0.0 then
                  total := None
               else
                  total := Some {component = tot.component; power = p}
         end
      in
      match uc.component with
      | Mass m ->
         process_component_aux total_mass (Mass (Kilo, Gram))
      | Distance d ->
         process_component_aux total_distance (Distance (NoPrefix, Meter))
      | Time t ->
         process_component_aux total_time (Time (NoPrefix, Second))
      | Current c ->
         process_component_aux total_current (Current (NoPrefix, Ampere))
      | Temperature t ->
         process_component_aux total_temperature (Temperature (NoPrefix, Kelvin))
      | Composite c ->
         if standardize then
            unit_failwith "Encountered Composite argument to group_units() with standardized = true"
         else 
            begin match !total_composite with
            | None ->
               total_composite := Some uc
            | Some tc ->
               let conversion = convert_component uc
               {component = tc.component; power = uc.power} in
               total_coeff := !total_coeff *. conversion;
               let p = tc.power +. uc.power in
               if p = 0.0 then
                  total_composite := None
               else
                  total_composite := Some {component = tc.component; power = p}
            end
   in
   List.iter process_component ulist.components;
   let result_components = ref [] in
   begin match !total_time with
   | None -> ()
   | Some t -> result_components := t :: !result_components
   end;
   begin match !total_distance with
   | None -> ()
   | Some d -> result_components := d :: !result_components
   end;
   begin match !total_mass with
   | None -> ()
   | Some m -> result_components := m :: !result_components
   end;
   begin match !total_current with
   | None -> ()
   | Some c -> result_components := c :: !result_components
   end;
   begin match !total_composite with
   | None -> ()
   | Some c -> result_components := c :: !result_components
   end;
   {coeff = ulist.coeff *. !total_coeff; components = !result_components};; 
   

(* expand out any composite units, leaving a list that depends
 * only on the standard categories of mass, distance, time, etc. *)
let expand_units (u : unit_t) =
   let rec expand_units_aux in_list out_list scalar =
      match in_list with
      | [] -> {
         coeff = scalar;
         components = out_list
         }
      | head :: tail ->
         let temp = expand_component head in
         expand_units_aux tail (temp.components @ out_list) 
         (scalar *. temp.coeff)
   in
   expand_units_aux u.components [] u.coeff;;


(* refactor a list of units into a simplified set of fundamental
 * units of mass, distance, time, etc. *)
let standardize_units (u : unit_t) =
   group_units (expand_units u) true;;


(* compute the conversion factor between two units *)
let conversion_factor (u1 : unit_t) (u2 : unit_t) =
   let s1 = standardize_units u1
   and s2 = standardize_units u2 in
   if s1.components = s2.components then
      s1.coeff /. s2.coeff
   else
      unit_failwith "Inconsistent units.";;


let is_prefix pre word =
   if String.length word >= String.length pre then
      pre = (String.sub word 0 (String.length pre))
   else
      false;;

let fund_unit_of_string ss =
   let rec test_prefixes plist =
      match plist with
      | [] ->
         unit_failwith "Failed to match a unit prefix"
      | head :: tail ->
         if is_prefix head ss then
            let pre_len = String.length head in
            try
               let comp = Hashtbl.find unit_string_table 
               (Str.string_after ss pre_len) in
               let pre = Hashtbl.find prefix_string_table head in
               begin match comp with
               | Mass (_, u)        -> Mass (pre, u)
               | Distance (_, d)    -> Distance (pre, d)
               | Time (_, t)        -> Time (pre, t)
               | Current (_, c)     -> Current (pre, c)
               | Temperature (_, t) -> Temperature (pre, t)
               | Composite (_, c)   -> Composite (pre, c)
               end
            with Not_found ->
               test_prefixes tail
         else
            test_prefixes tail
   in
   try
      Hashtbl.find unit_string_table ss
   with Not_found ->
      try
         test_prefixes prefix_list
      with Unit_error _ ->
         let err_str = Printf.sprintf "Unrecognized unit \"%s\"" ss in
         unit_failwith err_str;;


(* convert a string into an appropriate unit list.  Units should be
 * multiplied with '*', divided with '/', and raised to powers with '^'.
 * So the following would be a valid example: "kg^2*m/s^2/h*ft^-2". *)
let unit_of_string ss =
   let mult_regex = Str.regexp "\\*"
   and div_regex  = Str.regexp "/" 
   and pow_regex  = Str.regexp "\\^" in
   let mult_list  = Str.split mult_regex ss in
   let component_of_term (tt : string) : unit_component_power_t =
      let split_term = Str.split pow_regex tt in
      let len = List.length split_term in
      if len = 0 then
         unit_failwith "Empty power split in unit_of_string()"
      else if len = 1 then
         {component = fund_unit_of_string (List.hd split_term); power = 1.0}
      else if len = 2 then
         let pow =
            let pow_str = List.hd (List.tl split_term) in
            try float_of_string pow_str
            with _ -> 
               let err_msg = Printf.sprintf "Illegal unit power: \"%s\"" pow_str in
               unit_failwith err_msg
         in
         {component = fund_unit_of_string (List.hd split_term); power = pow}
      else
         let err_msg = 
            Printf.sprintf "Too many exponentiations in unit term \"%s\"" tt
         in
         unit_failwith err_msg
   in
   let rec process_div_terms dlist result =
      match dlist with
      | [] ->
         result
      | head :: tail ->
         let comp = component_of_term head in
         let new_result = 
            {component = comp.component; power = ~-. (comp.power)} :: 
            result
         in
         process_div_terms tail new_result
   in
   let rec process_mult_terms mlist result = 
      match mlist with
      | [] ->
         result
      | head :: tail ->
         let div_list = Str.split div_regex head in
         let div_list_comp = 
            if List.length div_list = 0 then
               unit_failwith "Empty unit string"
            else
               component_of_term (List.hd div_list) :: (process_div_terms
               (List.tl div_list) [])
         in
         process_mult_terms tail (div_list_comp @ result)
   in
   let mult_terms = Str.split mult_regex ss in
   {coeff = 1.0; components = process_mult_terms mult_terms []};;
            






(* arch-tag: DO_NOT_CHANGE_a25e50f6-dfff-434a-96fe-5d599fcaaaa3 *)
