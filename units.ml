

exception Unit_error of string;;
let unit_failwith ss =
   raise (Unit_error ss);;

type mass_fund_t = 
   | Gram
   | PoundMass
   | Ounce
   | Slug
   | TroyPound
   | ShortTon
   | LongTon
   | MetricTon
   | Carat
   | Grain

type distance_fund_t =
   | Meter
   | Foot
   | Inch
   | Yard
   | Mile
   | Parsec
   | AstronomicalUnit
   | Angstrom
   | Furlong
   | Point
   | Pica
   | NauticalMile
   | Lightyear;;

type time_fund_t =
   | Second
   | Minute
   | Hour
   | Day
   | Year;;

type current_fund_t = Ampere;;
   
type temperature_fund_t = 
   | Kelvin
   | Rankine;;

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

type dimension_t =
   | Mass of mass_unit_t
   | Distance of distance_unit_t
   | Time of time_unit_t
   | Current of current_unit_t
   | Temperature of temperature_unit_t
   | Composite of composite_unit_t

type unit_factor_power_t = {
   factor : dimension_t;
   power  : float
};;

type unit_t = {
   coeff   : float;
   factors : unit_factor_power_t list
};;


let unit_string_table = Hashtbl.create 50;;
Hashtbl.add unit_string_table "g"       ( Mass        ( NoPrefix, Gram));
Hashtbl.add unit_string_table "lb"      ( Mass        ( NoPrefix, PoundMass));
Hashtbl.add unit_string_table "oz"      ( Mass        ( NoPrefix, Ounce));
Hashtbl.add unit_string_table "slug"    ( Mass        ( NoPrefix, Slug));
Hashtbl.add unit_string_table "ton"     ( Mass        ( NoPrefix, ShortTon));
Hashtbl.add unit_string_table "tonl"    ( Mass        ( NoPrefix, LongTon));
Hashtbl.add unit_string_table "tonm"    ( Mass        ( NoPrefix, MetricTon));
Hashtbl.add unit_string_table "ct"      ( Mass        ( NoPrefix, Carat));
Hashtbl.add unit_string_table "gr"      ( Mass        ( NoPrefix, Grain));
Hashtbl.add unit_string_table "m"       ( Distance    ( NoPrefix, Meter));
Hashtbl.add unit_string_table "ft"      ( Distance    ( NoPrefix, Foot));
Hashtbl.add unit_string_table "in"      ( Distance    ( NoPrefix, Inch));
Hashtbl.add unit_string_table "yd"      ( Distance    ( NoPrefix, Yard));
Hashtbl.add unit_string_table "mi"      ( Distance    ( NoPrefix, Mile));
Hashtbl.add unit_string_table "pc"      ( Distance    ( NoPrefix, Parsec));
Hashtbl.add unit_string_table "AU"      ( Distance    ( NoPrefix, AstronomicalUnit));
Hashtbl.add unit_string_table "Ang"     ( Distance    ( NoPrefix, Angstrom));
Hashtbl.add unit_string_table "furlong" ( Distance    ( NoPrefix, Furlong));
Hashtbl.add unit_string_table "pt"      ( Distance    ( NoPrefix, Point));
Hashtbl.add unit_string_table "pica"    ( Distance    ( NoPrefix, Pica));
Hashtbl.add unit_string_table "nmi"     ( Distance    ( NoPrefix, NauticalMile));
Hashtbl.add unit_string_table "lyr"     ( Distance    ( NoPrefix, Lightyear));
Hashtbl.add unit_string_table "s"       ( Time        ( NoPrefix, Second));
Hashtbl.add unit_string_table "min"     ( Time        ( NoPrefix, Minute));
Hashtbl.add unit_string_table "hr"      ( Time        ( NoPrefix, Hour));
Hashtbl.add unit_string_table "day"     ( Time        ( NoPrefix, Day));
Hashtbl.add unit_string_table "yr"      ( Time        ( NoPrefix, Year));
Hashtbl.add unit_string_table "A"       ( Current     ( NoPrefix, Ampere));
Hashtbl.add unit_string_table "K"       ( Temperature ( NoPrefix, Kelvin));
Hashtbl.add unit_string_table "R"       ( Temperature ( NoPrefix, Rankine));
Hashtbl.add unit_string_table "N"       ( Composite   ( NoPrefix, Newton));
Hashtbl.add unit_string_table "C"       ( Composite   ( NoPrefix, Coulomb));
Hashtbl.add unit_string_table "Hz"      ( Composite   ( NoPrefix, Hertz));;

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


(* expand out composite factors into fundamental units of 
 * space, time, mass, current *)
let expand_factor (uc : unit_factor_power_t) =
   match uc.factor with
   | Composite (pre, c) ->
      begin match c with
      | Newton -> {
         coeff = prefix_value pre;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0};
            {factor = Distance (NoPrefix, Meter); power = 1.0};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0}
         ] }
      | Coulomb -> {
         coeff = prefix_value pre;
         factors = [
            {factor = Current (NoPrefix, Ampere); power = 1.0};
            {factor = Time (NoPrefix, Second); power = 1.0}
         ] }
      | Hertz -> {
         coeff = prefix_value pre;
         factors = [
            {factor = Time (NoPrefix, Second); power = ~-. 1.0}
         ] }
      end
   | _ -> {
      coeff = 1.0;
      factors = [ uc ]
      };;


(* compute conversion factors between fundamental units of mass *)
let rec convert_mass (m1 : mass_fund_t) (m2 : mass_fund_t) =
   match m1 with
   | Gram ->
      begin match m2 with
      | Gram  -> 1.0
      | PoundMass -> 0.00220462262
      | Ounce -> 1.0 /. 28.3495231
      | Slug -> 1.0 /. 14593.9029
      | TroyPound -> 1.0 /. 373.2417216
      | ShortTon -> 1.0 /. 907184.740004
      | LongTon -> 1.0 /. 1016046.9088
      | MetricTon -> 1.0e-6
      | Carat -> 5.0
      | Grain -> 15.4323583529
      end
   | PoundMass ->
      begin match m2 with
      | PoundMass -> 1.0
      | Ounce -> 16.0
      | Slug -> 1.0 /. 32.1740485564
      | TroyPound -> 1.0 /. 0.822857142856
      | ShortTon -> 0.0005
      | LongTon -> 1.0 /. 2240.0
      | MetricTon -> 0.00045359237
      | Carat -> 2267.96185
      | Grain -> 7000.0
      | _     -> 1.0 /. (convert_mass m2 m1)
      end
   | Ounce ->
      begin match m2 with
      | Ounce -> 1.0
      | Slug -> 1.0 /. 514.784776904
      | TroyPound -> 1.0 /. 13.1657142857
      | ShortTon -> 1.0 /. 32000.0
      | LongTon -> 1.0 /. 35840.0
      | MetricTon -> 35273.9619496
      | Carat -> 141.747615625
      | Grain -> 437.5
      | _     -> 1.0 /. (convert_mass m2 m1)
      end
   | Slug ->
      begin match m2 with
      | Slug -> 1.0
      | TroyPound -> 39.1004062319
      | ShortTon -> 1.60870242782e-2
      | LongTon -> 1.0 /. 69.6213283844
      | MetricTon -> 1.0 /. 68.5217658568
      | Carat -> 1.0 /. 72969.5146858
      | Grain -> 225218.339895
      | _     -> 1.0 /. (convert_mass m2 m1)
      end
   | TroyPound ->
      begin match m2 with
      | TroyPound -> 1.0
      | ShortTon -> 1.0 /. 2430.55555555
      | LongTon -> 1.0 /. 2722.22222222
      | MetricTon -> 3.732417216e-4
      | Carat -> 1866.208608
      | Grain -> 5760.0
      | _     -> 1.0 /. (convert_mass m2 m1)
      end
   | ShortTon ->
      begin match m2 with
      | ShortTon -> 1.0
      | LongTon -> 1.0 /. 1.12
      | MetricTon -> 0.907184740004
      | Carat -> 4535923.7
      | Grain -> 14.0e6
      | _     -> 1.0 /. (convert_mass m2 m1)
      end
   | LongTon ->
      begin match m2 with
      | LongTon -> 1.0
      | MetricTon -> 1.0160469088
      | Carat -> 5080234.54401
      | Grain -> 1.568e7
      | _     -> 1.0 /. (convert_mass m2 m1)
      end
   | MetricTon ->
      begin match m2 with
      | MetricTon -> 1.0
      | Carat -> 5.0e6
      | Grain -> 15432358.3529
      | _     -> 1.0 /. (convert_mass m2 m1)
      end
   | Carat ->
      begin match m2 with
      | Carat -> 1.0
      | Grain -> 1.0 /. 0.32399455
      | _     -> 1.0 /. (convert_mass m2 m1)
      end
   | Grain ->
      begin match m2 with
      | Grain -> 1.0
      | _     -> 1.0 /. (convert_mass m2 m1)
      end;;


(* compute conversion factors between fundamental units of distance *)
let rec convert_distance (d1 : distance_fund_t) (d2 : distance_fund_t) =
   match d1 with
   | Meter ->
      begin match d2 with
      | Meter -> 1.0
      | Foot  -> 1.0 /. 0.3048
      | Inch  -> 1.0 /. 0.0254
      | Yard  -> 1.0 /. 0.9144
      | Mile  -> 0.000621371192
      | Parsec -> 1.0 /. 3.085678e16
      | AstronomicalUnit -> 1.0 /. 1.49598e11
      | Angstrom -> 1.0e10
      | Furlong -> 1.0 /. 201.168
      | Point -> 72.0 /. 0.0254
      | Pica  -> 6.0 /. 0.0254
      | NauticalMile -> 1.0 /. 1852.0
      | Lightyear -> 1.05702341054e-16
      end
   | Foot ->
      begin match d2 with
      | Foot -> 1.0
      | Inch -> 12.0
      | Yard -> 1.0 /. 3.0
      | Mile -> 1.0 /. 5280.0
      | Parsec -> 9.8778939e-18
      | AstronomicalUnit -> 1.0 /. 4.90807087e11
      | Angstrom -> 3.048e9
      | Furlong -> 1.0 /. 660.0
      | Point -> 864.0
      | Pica -> 72.0
      | NauticalMile -> 0.3048 /. 1852.0
      | Lightyear -> 3.22180735531e-17
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | Inch ->
      begin match d2 with
      | Inch -> 1.0
      | Yard -> 1.0 /. 36.0
      | Mile -> 1.0 /. 63360.0
      | Parsec -> 8.2315783e-19
      | AstronomicalUnit -> 1.0 /. 5.88968504e12
      | Angstrom -> 2.54e8
      | Furlong -> 1.0 /. 7920.0
      | Point -> 72.0
      | Pica -> 6.0
      | NauticalMile -> 0.0254 /. 1852.0
      | Lightyear -> 2.68483946276e-18
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | Yard ->
      begin match d2 with
      | Yard -> 1.0
      | Mile -> 1.0 /. 1760.0
      | Parsec -> 2.9633682e-17
      | AstronomicalUnit -> 1.0 /. 1.63602362e11
      | Angstrom -> 9.144e9
      | Furlong -> 1.0 /. 220.0
      | Point -> 2592.0
      | Pica -> 216.0
      | NauticalMile -> 0.9144 /. 1852.0
      | Lightyear -> 9.66542206594e-17
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | Mile ->
      begin match d2 with
      | Mile -> 1.0
      | Parsec -> 5.2155280e-14
      | AstronomicalUnit -> 1.0 /. 92955887.6
      | Angstrom -> 1.609344e13
      | Furlong -> 1.0 /. 0.125
      | Point -> 4561920.0
      | Pica -> 380160.0
      | NauticalMile -> 1609.344 /. 1852.0
      | Lightyear -> 1.70111428361e-13
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | Parsec ->
      begin match d2 with
      | Parsec -> 1.0
      | AstronomicalUnit -> 206264.806
      | Angstrom -> 3.08568025e-26
      | Furlong -> 1.53388225e14
      | Point -> 8.74681015e19
      | Pica -> 7.28900846e18
      | NauticalMile -> 1.66613404e13
      | Lightyear -> 3.26163407982
      | _    -> 1.0 /. (convert_distance d2 d1)
      end 
   | AstronomicalUnit ->
      begin match d2 with
      | AstronomicalUnit -> 1.0
      | Angstrom -> 1.49598e21
      | Furlong -> 743647101.0
      | Point -> 4.24057323e14
      | Pica -> 3.53381102e13
      | NauticalMile -> 80776457.9
      | Lightyear -> 1.0 /. 63239.7139591
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | Angstrom ->
      begin match d2 with
      | Angstrom -> 1.0
      | Furlong -> 1.0 /. 2.01168e12
      | Point -> 72.0 /. 2.54e8
      | Pica -> 6.0 /. 2.54e8
      | NauticalMile -> 1.0 /. 1.85200e13
      | Lightyear -> 1.05702341054e-26
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | Furlong ->
      begin match d2 with
      | Furlong -> 1.0
      | Point -> 570240.0
      | Pica -> 47520.0
      | NauticalMile -> 660.0 *. 0.3048 /. 1852.0
      | Lightyear -> 2.12639285e-14
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | Point ->
      begin match d2 with
      | Point -> 1.0
      | Pica -> 1.0 /. 12.0
      | NauticalMile -> 0.0254 /. 72.0 /. 1852.0
      | Lightyear -> 3.7289437e-20
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | Pica ->
      begin match d2 with
      | Pica -> 1.0
      | NauticalMile -> 0.0254 /. 6.0 /. 1852.0
      | Lightyear -> 4.47473244e-19
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | NauticalMile ->
      begin match d2 with
      | NauticalMile -> 1.0
      | Lightyear -> 1.95760735631e-13
      | _    -> 1.0 /. (convert_distance d2 d1)
      end
   | Lightyear ->
      begin match d2 with
      | Lightyear -> 1.0
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
      | Day    -> 1.0 /. 86400.0
      | Year   -> 3.16887646408e-8
      end
   | Minute ->
      begin match t2 with
      | Minute -> 1.0
      | Hour   -> 1.0 /. 60.0
      | Day    -> 1.0 /. 1440.0
      | Year -> 1.90132587845e-6
      | _      -> 1.0 /. (convert_time t2 t1)
      end
   | Hour ->
      begin match t2 with
      | Hour -> 1.0
      | Day  -> 1.0 /. 24.0
      | Year -> 1.14079552707e-4
      | _    -> 1.0 /. (convert_time t2 t1)
      end
   | Day ->
      begin match t2 with
      | Day -> 1.0
      | Year -> 2.73790926497e-3
      | _   -> 1.0 /. (convert_time t2 t1)
      end
   | Year ->
      begin match t2 with
      | Year -> 1.0
      | _   -> 1.0 /. (convert_time t2 t1)
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
      | Rankine -> 9.0 /. 5.0
      end
   | Rankine ->
      begin match t2 with
      | Rankine -> 1.0
      | _ -> 1.0 /. (convert_temperature t2 t1)
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
   

(* compute the conversion factor between two generic unit factors *)
let convert_factor (u1 : unit_factor_power_t) (u2 : unit_factor_power_t) =
   if u1.power <> u2.power then begin
      Printf.printf "pow1 = %f, pow2 = %f\n" u1.power u2.power;
      unit_failwith "Units have inconsistent power"
   end else
      match u1.factor with
      | Mass (pre1, m1) ->
         begin match u2.factor with
         | Mass (pre2, m2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_mass m1 m2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end
      | Distance (pre1, d1) ->
         begin match u2.factor with
         | Distance (pre2, d2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_distance d1 d2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end
      | Time (pre1, t1) ->
         begin match u2.factor with
         | Time (pre2, t2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_time t1 t2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end
      | Current (pre1, c1) ->
         begin match u2.factor with
         | Current (pre2, c2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_current c1 c2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end
      | Temperature (pre1, t1) ->
         begin match u2.factor with
         | Temperature (pre2, t2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_temperature t1 t2) ** u1.power
         | _ -> unit_failwith "Inconsistent units"
         end
      | Composite (pre1, c1) ->
         begin match u2.factor with
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
   let total_mass : unit_factor_power_t option ref        = ref None
   and total_distance : unit_factor_power_t option ref    = ref None
   and total_time : unit_factor_power_t option ref        = ref None
   and total_current : unit_factor_power_t option ref     = ref None
   and total_temperature : unit_factor_power_t option ref = ref None
   and total_composite : unit_factor_power_t option ref   = ref None
   and total_coeff                                        = ref 1.0 in
   let process_factor uc =
      let process_factor_aux total target_comp =
         begin match !total with
         | None ->
            if standardize then begin
               let target_unit = {
                  factor = target_comp;
                  power  = uc.power
               } in
               let conversion = convert_factor uc target_unit in
               total_coeff := !total_coeff *. conversion;
               total       := Some target_unit
            end else
               total := Some uc
         | Some tot ->
               let conversion = convert_factor uc
               {factor = tot.factor; power = uc.power} in
               total_coeff := !total_coeff *. conversion;
               let p = tot.power +. uc.power in
               if p = 0.0 then
                  total := None
               else
                  total := Some {factor = tot.factor; power = p}
         end
      in
      match uc.factor with
      | Mass m ->
         process_factor_aux total_mass (Mass (Kilo, Gram))
      | Distance d ->
         process_factor_aux total_distance (Distance (NoPrefix, Meter))
      | Time t ->
         process_factor_aux total_time (Time (NoPrefix, Second))
      | Current c ->
         process_factor_aux total_current (Current (NoPrefix, Ampere))
      | Temperature t ->
         process_factor_aux total_temperature (Temperature (NoPrefix, Kelvin))
      | Composite c ->
         if standardize then
            unit_failwith "Encountered Composite argument to group_units() with standardized = true"
         else 
            begin match !total_composite with
            | None ->
               total_composite := Some uc
            | Some tc ->
               let conversion = convert_factor uc
               {factor = tc.factor; power = uc.power} in
               total_coeff := !total_coeff *. conversion;
               let p = tc.power +. uc.power in
               if p = 0.0 then
                  total_composite := None
               else
                  total_composite := Some {factor = tc.factor; power = p}
            end
   in
   List.iter process_factor ulist.factors;
   let result_factors = ref [] in
   begin match !total_time with
   | None   -> ()
   | Some t -> result_factors := t :: !result_factors
   end;
   begin match !total_distance with
   | None -> ()
   | Some d -> result_factors := d :: !result_factors
   end;
   begin match !total_mass with
   | None -> ()
   | Some m -> result_factors := m :: !result_factors
   end;
   begin match !total_current with
   | None -> ()
   | Some c -> result_factors := c :: !result_factors
   end;
   begin match !total_composite with
   | None -> ()
   | Some c -> result_factors := c :: !result_factors
   end;
   {coeff = ulist.coeff *. !total_coeff; factors = !result_factors};; 
   

(* expand out any composite units, leaving a list that depends
 * only on the standard categories of mass, distance, time, etc. *)
let expand_units (u : unit_t) =
   let rec expand_units_aux in_list out_list scalar =
      match in_list with
      | [] -> {
         coeff   = scalar;
         factors = out_list
         }
      | head :: tail ->
         let temp = expand_factor head in
         expand_units_aux tail (temp.factors @ out_list) 
         (scalar *. temp.coeff)
   in
   expand_units_aux u.factors [] u.coeff;;


(* refactor a list of units into a simplified set of fundamental
 * units of mass, distance, time, etc. *)
let standardize_units (u : unit_t) =
   group_units (expand_units u) true;;


(* compute the conversion factor between two units *)
let conversion_factor (u1 : unit_t) (u2 : unit_t) =
   let s1 = standardize_units u1
   and s2 = standardize_units u2 in
   if s1.factors = s2.factors then
      s1.coeff /. s2.coeff
   else
      unit_failwith "Inconsistent units.";;


let is_prefix pre word =
   if String.length word >= String.length pre then
      pre = (String.sub word 0 (String.length pre))
   else
      false;;

let dimension_of_string ss =
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
   let factor_of_term (tt : string) : unit_factor_power_t =
      let split_term = Str.split pow_regex tt in
      let len = List.length split_term in
      if len = 0 then
         unit_failwith "Empty power split in unit_of_string()"
      else if len = 1 then
         {factor = dimension_of_string (List.hd split_term); power = 1.0}
      else if len = 2 then
         let pow =
            let pow_str = List.hd (List.tl split_term) in
            try float_of_string pow_str
            with _ -> 
               let err_msg = Printf.sprintf "Illegal unit power: \"%s\"" pow_str in
               unit_failwith err_msg
         in
         {factor = dimension_of_string (List.hd split_term); power = pow}
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
         let comp = factor_of_term head in
         let new_result = 
            {factor = comp.factor; power = ~-. (comp.power)} :: 
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
               factor_of_term (List.hd div_list) :: (process_div_terms
               (List.tl div_list) [])
         in
         process_mult_terms tail (div_list_comp @ result)
   in
   let mult_terms = Str.split mult_regex ss in
   {coeff = 1.0; factors = process_mult_terms mult_terms []};;
            






(* arch-tag: DO_NOT_CHANGE_a25e50f6-dfff-434a-96fe-5d599fcaaaa3 *)
