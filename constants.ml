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


open Units;;

let electron_charge      = unit_of_float_string 1.60217653e-19 "C"
let electron_mass        = unit_of_float_string 9.1093826e-31 "kg"
let gravitation          = unit_of_float_string 6.6742e-11 "m^3*kg^-1*s^-2"
let acceleration_gravity = unit_of_float_string 9.80665 "m*s^-2"




(* arch-tag: DO_NOT_CHANGE_1c6c88d4-3b57-4b5a-85fc-02372437ea3f *)
