(* ocaml-units -- a module for handling standard operations on
 *                physical units
 * 
 * Copyright (C) 2004 Paul Pelzl
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


exception Units_error of string;;
let unit_failwith ss =
   raise (Units_error ss);;


let c_of_f ff = {
   Complex.re = ff;
   Complex.im = 0.0;
};;

let cpow f1 f2 = Complex.pow (c_of_f f1) (c_of_f f2);;

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
   | Grain;;

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

type amount_fund_t = Mole;;

type intensity_fund_t = Candela;;

type composite_t =
   | Newton
   | PoundForce
   | Dyne
   | Kip
   | Joule
   | Erg
   | Calorie
   | Btu
   | ElectronVolt
   | Coulomb
   | Hertz
   | Watt
   | Horsepower
   | Pascal
   | Atmosphere
   | Bar
   | MillimetersMercury
   | InchesMercury
   | Volt
   | Ohm
   | Farad
   | Henry
   | Tesla
   | Gauss
   | Weber
   | Maxwell
   | Lumen
   | Lux;;

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
type amount_unit_t      = prefix_t * amount_fund_t;;
type intensity_unit_t   = prefix_t * intensity_fund_t;;
type composite_unit_t   = prefix_t * composite_t;;

type dimension_t =
   | Mass of mass_unit_t
   | Distance of distance_unit_t
   | Time of time_unit_t
   | Current of current_unit_t
   | Temperature of temperature_unit_t
   | Amount of amount_unit_t
   | Intensity of intensity_unit_t
   | Composite of composite_unit_t

type unit_factor_power_t = {
   factor : dimension_t;
   power  : float
};;

type unit_t = {
   coeff   : Complex.t;
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
Hashtbl.add unit_string_table "mol"     ( Amount      ( NoPrefix, Mole));
Hashtbl.add unit_string_table "cd"      ( Intensity   ( NoPrefix, Candela));
Hashtbl.add unit_string_table "N"       ( Composite   ( NoPrefix, Newton));
Hashtbl.add unit_string_table "lbf"     ( Composite   ( NoPrefix, PoundForce));
Hashtbl.add unit_string_table "dyn"     ( Composite   ( NoPrefix, Dyne));
Hashtbl.add unit_string_table "kip"     ( Composite   ( NoPrefix, Kip));
Hashtbl.add unit_string_table "J"       ( Composite   ( NoPrefix, Joule));
Hashtbl.add unit_string_table "erg"     ( Composite   ( NoPrefix, Erg));
Hashtbl.add unit_string_table "BTU"     ( Composite   ( NoPrefix, Btu));
Hashtbl.add unit_string_table "cal"     ( Composite   ( NoPrefix, Calorie));
Hashtbl.add unit_string_table "eV"      ( Composite   ( NoPrefix, ElectronVolt));
Hashtbl.add unit_string_table "C"       ( Composite   ( NoPrefix, Coulomb));
Hashtbl.add unit_string_table "Hz"      ( Composite   ( NoPrefix, Hertz));
Hashtbl.add unit_string_table "W"       ( Composite   ( NoPrefix, Watt));
Hashtbl.add unit_string_table "hp"      ( Composite   ( NoPrefix, Horsepower));
Hashtbl.add unit_string_table "Pa"      ( Composite   ( NoPrefix, Pascal));
Hashtbl.add unit_string_table "atm"     ( Composite   ( NoPrefix, Atmosphere));
Hashtbl.add unit_string_table "bar"     ( Composite   ( NoPrefix, Bar));
Hashtbl.add unit_string_table "mmHg"    ( Composite   ( NoPrefix, MillimetersMercury));
Hashtbl.add unit_string_table "inHg"    ( Composite   ( NoPrefix, InchesMercury));
Hashtbl.add unit_string_table "V"       ( Composite   ( NoPrefix, Volt));
Hashtbl.add unit_string_table "Ohm"     ( Composite   ( NoPrefix, Ohm));
Hashtbl.add unit_string_table "F"       ( Composite   ( NoPrefix, Farad));
Hashtbl.add unit_string_table "H"       ( Composite   ( NoPrefix, Henry));
Hashtbl.add unit_string_table "T"       ( Composite   ( NoPrefix, Tesla));
Hashtbl.add unit_string_table "G"       ( Composite   ( NoPrefix, Gauss));
Hashtbl.add unit_string_table "Wb"      ( Composite   ( NoPrefix, Weber));
Hashtbl.add unit_string_table "Mx"      ( Composite   ( NoPrefix, Maxwell));
Hashtbl.add unit_string_table "lm"      ( Composite   ( NoPrefix, Lumen));
Hashtbl.add unit_string_table "lx"      ( Composite   ( NoPrefix, Lux));;


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

let string_of_mass (m : mass_fund_t) =
   match m with
   | Gram      -> "g"
   | PoundMass -> "lb"
   | Ounce     -> "oz"
   | Slug      -> "slug"
   | TroyPound -> "lbt"
   | ShortTon  -> "ton"
   | LongTon   -> "tonl"
   | MetricTon -> "tonm"
   | Carat     -> "ct"
   | Grain     -> "gr";;

let string_of_distance (d : distance_fund_t) =
   match d with
   | Meter            -> "m"
   | Foot             -> "ft"
   | Inch             -> "in"
   | Yard             -> "yd"
   | Mile             -> "mi"
   | Parsec           -> "pc"
   | AstronomicalUnit -> "AU"
   | Angstrom         -> "Ang"
   | Furlong          -> "furlong"
   | Point            -> "pt"
   | Pica             -> "pica"
   | NauticalMile     -> "nmi"
   | Lightyear        -> "lyr";;

let string_of_time (d : time_fund_t) =
   match d with
   | Second -> "s"
   | Minute -> "min"
   | Hour   -> "hr"
   | Day    -> "day"
   | Year   -> "yr";;

let string_of_current (c : current_fund_t) = 
   match c with
   | Ampere -> "A";;

let string_of_temperature (t : temperature_fund_t) =
   match t with
   | Kelvin  -> "K"
   | Rankine -> "R";;

let string_of_amount (a : amount_fund_t) =
   match a with
   | Mole -> "mol";;

let string_of_intensity (i : intensity_fund_t) =
   match i with
   | Candela -> "cd";;

let string_of_composite (c : composite_t) = 
   match c with
   | Newton             -> "N"
   | PoundForce         -> "lbf"
   | Dyne               -> "dyn"
   | Kip                -> "kip"
   | Joule              -> "J"
   | Erg                -> "erg"
   | Calorie            -> "cal"
   | Btu                -> "BTU"
   | ElectronVolt       -> "eV"
   | Coulomb            -> "C"
   | Hertz              -> "Hz"
   | Watt               -> "W"
   | Horsepower         -> "hp"
   | Pascal             -> "Pa"
   | Atmosphere         -> "atm"
   | Bar                -> "bar"
   | MillimetersMercury -> "mmHg"
   | InchesMercury      -> "inHg"
   | Volt               -> "V"
   | Ohm                -> "Ohm"
   | Farad              -> "F"
   | Henry              -> "H"
   | Tesla              -> "T"
   | Gauss              -> "G"
   | Weber              -> "Wb"
   | Maxwell            -> "Mw"
   | Lumen              -> "lm"
   | Lux                -> "lx";;



(* expand out composite factors into fundamental units of 
 * space, time, mass, current *)
let expand_factor (uc : unit_factor_power_t) =
   match uc.factor with
   | Composite (pre, c) ->
      begin match c with
      | Newton -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | PoundForce -> {
         coeff = cpow (32.1740485564 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (NoPrefix, PoundMass); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Foot); power = 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Dyne -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (NoPrefix, Gram); power = 1.0 *. uc.power};
            {factor = Distance (Centi, Meter); power = 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Kip -> {
         coeff = cpow (32174.0485564 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (NoPrefix, PoundMass); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Foot); power = 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Joule -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Erg -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (NoPrefix, Gram); power = 1.0 *. uc.power};
            {factor = Distance (Centi, Meter); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Calorie -> {
         coeff = cpow (4.1868 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Btu -> {
         coeff = cpow (1055.05585252 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | ElectronVolt -> {
         coeff = cpow (1.60217733e-19 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Coulomb -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Current (NoPrefix, Ampere); power = 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = 1.0 *. uc.power}
         ] }
      | Hertz -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Time (NoPrefix, Second); power = ~-. 1.0 *. uc.power}
         ] }
      | Watt -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 3.0 *. uc.power}
         ] }
      | Horsepower -> {
         coeff = cpow (550.0 *. 32.1740485564 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (NoPrefix, PoundMass); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Foot); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 3.0 *. uc.power}
         ] }
      | Pascal -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = ~-. 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Atmosphere -> {
         coeff = cpow (101325.0 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = ~-. 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Bar -> {
         coeff = cpow (100000.0 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = ~-. 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | MillimetersMercury -> {
         coeff = cpow (133.322368421 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = ~-. 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | InchesMercury -> {
         coeff = cpow (3386.38815789 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = ~-. 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Volt -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 3.0 *. uc.power};
            {factor = Current (NoPrefix, Ampere); power = ~-. 1.0 *. uc.power}
         ] }
      | Ohm -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 3.0 *. uc.power};
            {factor = Current (NoPrefix, Ampere); power = ~-. 2.0 *. uc.power}
         ] }
      | Farad -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Current (NoPrefix, Ampere); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = 4.0 *. uc.power};
            {factor = Mass (Kilo, Gram); power = ~-. 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = ~-. 2.0 *. uc.power}
         ] }
      | Henry -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 2.0 *. uc.power};
            {factor = Current (NoPrefix, Ampere); power = ~-. 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power}
         ] }
      | Tesla -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power};
            {factor = Current (NoPrefix, Ampere); power = ~-. 1.0 *. uc.power}
         ] }
      | Gauss -> {
         coeff = cpow (0.0001 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power};
            {factor = Current (NoPrefix, Ampere); power = ~-. 1.0 *. uc.power}
         ] }
      | Weber -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power};
            {factor = Current (NoPrefix, Ampere); power = ~-. 1.0 *. uc.power}
         ] }
      | Maxwell -> {
         coeff = cpow (1.0e-8 *. (prefix_value pre)) uc.power;
         factors = [
            {factor = Mass (Kilo, Gram); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = 2.0 *. uc.power};
            {factor = Time (NoPrefix, Second); power = ~-. 2.0 *. uc.power};
            {factor = Current (NoPrefix, Ampere); power = ~-. 1.0 *. uc.power}
         ] }
      | Lumen -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Intensity (NoPrefix, Candela); power = 1.0 *. uc.power}
         ] }
      | Lux -> {
         coeff = cpow (prefix_value pre) uc.power;
         factors = [
            {factor = Intensity (NoPrefix, Candela); power = 1.0 *. uc.power};
            {factor = Distance (NoPrefix, Meter); power = ~-. 2.0 *. uc.power}
         ] }
      end
   | _ -> {
      coeff = Complex.one;
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


(* compute conversion factors between fundamental units of 'amount' *)
let rec convert_amount (a1 : amount_fund_t) (a2 : amount_fund_t) =
   match a1 with
   | Mole ->
      begin match a2 with
      | Mole -> 1.0
      end;;


(* compute conversion factors between fundamental units of intensity *)
let rec convert_intensity (i1 : intensity_fund_t) (i2 : intensity_fund_t) =
   match i1 with
   | Candela ->
      begin match i2 with
      | Candela -> 1.0
      end;;


(* compute conversion factors between composite units *)
let rec convert_composite (c1 : composite_t) (c2 : composite_t) =
   match c1 with
   | Newton ->
      begin match c2 with
      | Newton     -> 1.0
      | PoundForce -> 0.2248089431
      | Dyne       -> 100000.0
      | Kip        -> 0.0002248089431
      | _ -> unit_failwith "inconsistent composite units"
      end
   | PoundForce ->
      begin match c2 with
      | PoundForce -> 1.0
      | Newton     -> 1.0 /. (convert_composite c2 c1)
      | Dyne       -> 444822.161526
      | Kip        -> 0.001
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Dyne ->
      begin match c2 with
      | Dyne -> 1.0
      | Kip  -> 2.248089431e-9
      | Newton | PoundForce -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Kip ->
      begin match c2 with
      | Kip -> 1.0
      | Newton | PoundForce | Dyne -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Joule ->
      begin match c2 with
      | Joule -> 1.0
      | Erg -> 1.0e7
      | Calorie -> 1.0 /. 4.1868
      | Btu -> 9.47817120313e-4
      | ElectronVolt -> 1.0 /. 1.60217733e-19
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Erg ->
      begin match c2 with
      | Erg -> 1.0
      | Calorie -> 2.38845896627e-8
      | Btu -> 9.47817120313e-11
      | ElectronVolt -> 624150636309.0
      | Joule -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Calorie ->
      begin match c2 with
      | Calorie -> 1.0
      | Btu -> 3.96832071933e-3
      | ElectronVolt -> 2.6131938841e19
      | Joule | Erg -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Btu ->
      begin match c2 with
      | Btu -> 1.0
      | ElectronVolt -> 6.58513781755e21
      | Joule | Erg | Calorie -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | ElectronVolt ->
      begin match c2 with
      | ElectronVolt -> 1.0
      | Joule | Erg | Calorie | Btu -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Coulomb ->
      begin match c2 with
      | Coulomb -> 1.0
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Hertz ->
      begin match c2 with
      | Hertz -> 1.0
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Watt ->
      begin match c2 with
      | Watt -> 1.0
      | Horsepower -> 1.3410220896e-3
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Horsepower ->
      begin match c2 with
      | Horsepower -> 1.0
      | Watt -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Pascal ->
      begin match c2 with
      | Pascal -> 1.0
      | Atmosphere -> 9.86923266716e-6
      | Bar -> 1.0e-5
      | MillimetersMercury -> 7.50061682704e-3
      | InchesMercury -> 2.9529987508e-4
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Atmosphere ->
      begin match c2 with
      | Atmosphere -> 1.0
      | Bar -> 1.01325
      | MillimetersMercury -> 760.0
      | InchesMercury -> 29.9212598425
      | Pascal -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Bar ->
      begin match c2 with
      | Bar -> 1.0
      | MillimetersMercury -> 750.061682704
      | InchesMercury -> 29.529987508
      | Pascal | Atmosphere -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | MillimetersMercury ->
      begin match c2 with
      | MillimetersMercury -> 1.0
      | InchesMercury -> 3.93700787402e-2
      | Pascal | Atmosphere | Bar -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | InchesMercury ->
      begin match c2 with
      | InchesMercury -> 1.0
      | Pascal | Atmosphere | Bar | MillimetersMercury -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Volt ->
      begin match c2 with
      | Volt -> 1.0
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Ohm ->
      begin match c2 with
      | Ohm -> 1.0
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Farad ->
      begin match c2 with
      | Farad -> 1.0
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Henry ->
      begin match c2 with
      | Henry -> 1.0
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Tesla ->
      begin match c2 with
      | Tesla -> 1.0
      | Gauss -> 10000.0
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Gauss ->
      begin match c2 with
      | Gauss -> 1.0
      | Tesla -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Weber ->
      begin match c2 with
      | Weber -> 1.0
      | Maxwell -> 1.0e8
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Maxwell ->
      begin match c2 with
      | Maxwell -> 1.0
      | Weber -> 1.0 /. (convert_composite c2 c1)
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Lumen ->
      begin match c2 with
      | Lumen -> 1.0
      | _ -> unit_failwith "inconsistent composite units"
      end
   | Lux ->
      begin match c2 with
      | Lux -> 1.0
      | _ -> unit_failwith "inconsistent composite units"
      end

   

(* compute the conversion factor between two generic unit factors *)
let convert_factor (u1 : unit_factor_power_t) (u2 : unit_factor_power_t) =
   if u1.power <> u2.power then begin
      Printf.printf "pow1 = %f, pow2 = %f\n" u1.power u2.power;
      unit_failwith "units have inconsistent power"
   end else
      match u1.factor with
      | Mass (pre1, m1) ->
         begin match u2.factor with
         | Mass (pre2, m2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_mass m1 m2) ** u1.power
         | _ -> unit_failwith "inconsistent units"
         end
      | Distance (pre1, d1) ->
         begin match u2.factor with
         | Distance (pre2, d2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_distance d1 d2) ** u1.power
         | _ -> unit_failwith "inconsistent units"
         end
      | Time (pre1, t1) ->
         begin match u2.factor with
         | Time (pre2, t2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_time t1 t2) ** u1.power
         | _ -> unit_failwith "inconsistent units"
         end
      | Current (pre1, c1) ->
         begin match u2.factor with
         | Current (pre2, c2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_current c1 c2) ** u1.power
         | _ -> unit_failwith "inconsistent units"
         end
      | Temperature (pre1, t1) ->
         begin match u2.factor with
         | Temperature (pre2, t2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_temperature t1 t2) ** u1.power
         | _ -> unit_failwith "inconsistent units"
         end
      | Amount (pre1, a1) ->
         begin match u2.factor with
         | Amount (pre2, a2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_amount a1 a2) ** u1.power
         | _ -> unit_failwith "inconsistent units"
         end
      | Intensity (pre1, i1) ->
         begin match u2.factor with
         | Intensity (pre2, i2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_intensity i1 i2) ** u1.power
         | _ -> unit_failwith "inconsistent units"
         end
      | Composite (pre1, c1) ->
         begin match u2.factor with
         | Composite (pre2, c2) ->
            let coeff = (prefix_value pre1) /. (prefix_value pre2) in
            (coeff *. convert_composite c1 c2) ** u1.power
         | _ -> unit_failwith "inconsistent units"
         end


(* collect units together, i.e. kg*m^2*kg -> kg^2*mg^2.
 * Mismatched units of mass, distance, etc. will be rematched, i.e.
 * s*min -> 60 s^2.  If 'standardize' = true, then units of mass, distance,
 * etc. will be converted to fundamental units (kg, m, s, etc.). *)
let group_units (ulist : unit_t) (standardize : bool) =
   let total_mass        = ref None
   and total_distance    = ref None
   and total_time        = ref None
   and total_current     = ref None
   and total_temperature = ref None
   and total_amount      = ref None
   and total_intensity   = ref None
   and total_force       = ref None
   and total_energy      = ref None
   and total_charge      = ref None
   and total_freq        = ref None
   and total_power       = ref None
   and total_pressure    = ref None
   and total_voltage     = ref None
   and total_resist      = ref None
   and total_cap         = ref None
   and total_ind         = ref None
   and total_mag         = ref None
   and total_flux        = ref None
   and total_lumflux     = ref None
   and total_illuminance = ref None
   and total_coeff       = ref Complex.one in
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
               total_coeff := Complex.mul !total_coeff (c_of_f conversion);
               total       := Some target_unit
            end else
               total := Some uc
         | Some tot ->
            let conversion = convert_factor uc
            {factor = tot.factor; power = uc.power} in
            total_coeff := Complex.mul !total_coeff (c_of_f conversion);
            let p = tot.power +. uc.power in
            if p = 0.0 then
               total := None
            else
               total := Some {factor = tot.factor; power = p}
         end
      in
      let process_factor_composite_aux total =
         begin match !total with
         | None ->
            total := Some uc
         | Some t ->
            let conversion = convert_factor uc
            {factor = t.factor; power = uc.power} in
            total_coeff := Complex.mul !total_coeff (c_of_f conversion);
            let p = t.power +. uc.power in
            if p = 0.0 then
               total := None
            else
               total := Some {factor = t.factor; power = p}
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
      | Amount a ->
         process_factor_aux total_amount (Amount (NoPrefix, Mole))
      | Intensity i ->
         process_factor_aux total_intensity (Intensity (NoPrefix, Candela))
      | Composite (pre, co) ->
         if standardize then
            unit_failwith "Encountered Composite argument to group_units() with standardized = true"
         else 
            begin match co with
            | Newton | PoundForce | Dyne | Kip ->
               process_factor_composite_aux total_force
            | Joule | Erg | Calorie | Btu | ElectronVolt ->
               process_factor_composite_aux total_energy
            | Coulomb ->
               process_factor_composite_aux total_charge
            | Hertz ->
               process_factor_composite_aux total_freq
            | Watt | Horsepower ->
               process_factor_composite_aux total_power
            | Pascal | Atmosphere | Bar | MillimetersMercury | InchesMercury ->
               process_factor_composite_aux total_pressure
            | Volt ->
               process_factor_composite_aux total_voltage
            | Ohm ->
               process_factor_composite_aux total_resist
            | Farad ->
               process_factor_composite_aux total_cap
            | Henry ->
               process_factor_composite_aux total_ind
            | Tesla | Gauss ->
               process_factor_composite_aux total_mag
            | Weber | Maxwell ->
               process_factor_composite_aux total_flux
            | Lumen ->
               process_factor_composite_aux total_lumflux
            | Lux ->
               process_factor_composite_aux total_illuminance
            end
   in
   List.iter process_factor ulist.factors;
   let result_factors = ref [] in
   let join_factors category =
      begin match !category with
      | None   -> ()
      | Some f -> result_factors := f :: !result_factors
      end
   in
   let categories = [ total_time; total_distance; total_mass; total_current; 
   total_temperature; total_amount; total_intensity; total_force; total_energy; 
   total_charge; total_freq; total_power; total_pressure; total_voltage; total_resist;
   total_cap; total_ind; total_mag; total_flux; total_lumflux; total_illuminance ] in
   List.iter join_factors categories;
   {coeff = Complex.mul ulist.coeff !total_coeff; factors = !result_factors};; 
   

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
         (Complex.mul scalar temp.coeff)
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
      Complex.div s1.coeff s2.coeff
   else
      unit_failwith "inconsistent units";;


(* compute the conversion factor between two units,
 * ignoring the magnitude of the target unit coefficient. *)
let conversion_factor_unitary (u1 : unit_t) (u2 : unit_t) =
   let unitary_u2 = {
      coeff = Complex.one;
      factors = u2.factors
   } in
   conversion_factor u1 unitary_u2


(* multiply two units *)
let mult (u1 : unit_t) (u2 : unit_t) = 
   let new_unit = {
   coeff   = Complex.mul u1.coeff u2.coeff;
   factors = u1.factors @ u2.factors
   } in
   group_units new_unit false;;


(* divide one unit by another *)
let div (u1 : unit_t) (u2 : unit_t) =
   let flip_powers f = {
      factor = f.factor;
      power  = ~-. (f.power)
   } in
   let divisors = List.map flip_powers u2.factors in 
   let new_unit = {
      coeff   = Complex.div u1.coeff u2.coeff;
      factors = u1.factors @ divisors
   } in
   group_units new_unit false;;



(* raise a unit to a power *)
let pow (u : unit_t) (p : float) = 
   let update_powers f = {
      factor = f.factor;
      power  = f.power *. p
   } in {
      coeff   = Complex.pow u.coeff (c_of_f p);
      factors = List.map update_powers u.factors
   };;



let is_prefix pre word =
   if String.length word >= String.length pre then
      pre = (String.sub word 0 (String.length pre))
   else
      false;;

let dimension_of_string ss =
   let rec test_prefixes plist =
      match plist with
      | [] ->
         unit_failwith "failed to match a unit prefix"
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
               | Amount (_, a)      -> Amount (pre, a)
               | Intensity (_, i)   -> Intensity (pre, i)
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
      with Units_error _ ->
         let err_str = Printf.sprintf "unrecognized unit \"%s\"" ss in
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
         unit_failwith "empty power split in unit_of_string()"
      else if len = 1 then
         if String.contains tt '^' then
            unit_failwith "illegal unit exponentiation syntax"
         else
            {factor = dimension_of_string (List.hd split_term); power = 1.0}
      else if len = 2 then
         let pow =
            let pow_str = List.hd (List.tl split_term) in
            try float_of_string pow_str
            with _ -> 
               let err_msg = Printf.sprintf "illegal unit power: \"%s\"" pow_str in
               unit_failwith err_msg
         in
         {factor = dimension_of_string (List.hd split_term); power = pow}
      else
         let err_msg = 
            Printf.sprintf "too many exponentiations in unit term \"%s\"" tt
         in
         unit_failwith err_msg
   in
   let flip_powers ss = 
      let comp = factor_of_term ss in {
      factor = comp.factor;
      power  = ~-. (comp.power)
   } in
   let rec process_mult_terms mlist result = 
      match mlist with
      | [] ->
         result
      | head :: tail ->
         let div_list = Str.split div_regex head in
         let div_list_comp = 
            if List.length div_list = 0 then
               unit_failwith "empty unit string"
            else if List.length div_list = 1 && String.contains head '/' then
               unit_failwith "illegal unit division syntax"
            else
               factor_of_term (List.hd div_list) :: (List.map flip_powers
               (List.tl div_list))
         in
         process_mult_terms tail (List.rev_append div_list_comp result)
   in
   let mult_terms = Str.split mult_regex ss in
   let fact_rev = process_mult_terms mult_terms [] in
   group_units {coeff = Complex.one; factors = List.rev fact_rev} false;;
            

let string_of_unit (uu : unit_factor_power_t list) =
   let rec string_of_unit_aux in_list out_str =
      match in_list with
      | [] -> 
         out_str
      | head :: tail ->
         let mult_str =
            if out_str = "" then ""
            else out_str ^ "*"
         in
         let out_str2 = mult_str ^
            if head.power = 0.0 then
               ""
            else
               begin match head.factor with
               | Mass (pre, m) ->
                  (string_of_prefix pre) ^ (string_of_mass m)
               | Distance (pre, d) ->
                  (string_of_prefix pre) ^ (string_of_distance d)
               | Time (pre, t) ->
                  (string_of_prefix pre) ^ (string_of_time t)
               | Current (pre, c) ->
                  (string_of_prefix pre) ^ (string_of_current c)
               | Temperature (pre, t) ->
                  (string_of_prefix pre) ^ (string_of_temperature t)
               | Amount (pre, a) ->
                  (string_of_prefix pre) ^ (string_of_amount a)
               | Intensity (pre, i) ->
                  (string_of_prefix pre) ^ (string_of_intensity i)
               | Composite (pre, c) ->
                  (string_of_prefix pre) ^ (string_of_composite c)
               end
         in
         let out_str3 = out_str2 ^
            begin 
               if head.power = 1.0 || head.power = 0.0 then 
                  "" 
               else 
                  let pow_str = string_of_float head.power in
                  if pow_str.[pred (String.length pow_str)] = '.' then
                     "^" ^ (Str.string_before pow_str 
                     (pred (String.length pow_str)))
                  else
                     "^" ^ pow_str
            end
         in
         string_of_unit_aux tail out_str3
   in
   string_of_unit_aux uu "";;

   
(* create a unit with a specified coefficient *)
let unit_of_float_string ff ss =
   let u = unit_of_string ss in {
      coeff   = Complex.mul u.coeff (c_of_f ff);
      factors = u.factors
   };;


let unit_of_cpx_string cc ss =
   let u = unit_of_string ss in {
      coeff   = Complex.mul u.coeff cc;
      factors = u.factors
   };;





(* arch-tag: DO_NOT_CHANGE_a25e50f6-dfff-434a-96fe-5d599fcaaaa3 *)
