open Units;;

let electron_charge = {
   coeff = 1.60217653e-19; 
   components = [ {
      component = Composite (NoPrefix, Coulomb);
      power     = 1.0
   } ]
};;

let electron_mass = {
   coeff = 9.1093826e-31;
   components = [ {
      component = Mass (Kilo, Gram);
      power     = 1.0
   } ]
};;

let gravitation = {
   coeff = 6.6742e-11;
   components = [ 
      {component = Distance (NoPrefix, Meter); power = 3.0};
      {component = Mass (Kilo, Gram); power = ~-. 1.0};
      {component = Time (NoPrefix, Second); power = ~-. 2.0}
   ]
};;

let acceleration_gravity = {
   coeff = 9.80665;
   components = [
      {component = Distance (NoPrefix, Meter); power = 1.0};
      {component = Time (NoPrefix, Second); power = ~-. 2.0}
   ]
};;





(* arch-tag: DO_NOT_CHANGE_1c6c88d4-3b57-4b5a-85fc-02372437ea3f *)
