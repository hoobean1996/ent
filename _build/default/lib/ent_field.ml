type typ = FInt
type default_value = DVInt of int

type field = {
  name : string;
  typ : typ;
  is_primary : bool;
  is_optional : bool;
  default : default_value option;
}

let field ?(primary = false) ?(optional = false) ?default ~typ name =
  { name; typ; is_primary = primary; is_optional = optional; default }
