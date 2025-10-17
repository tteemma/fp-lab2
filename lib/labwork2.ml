module RBDict = struct
  module type OrderedType = Rb_dict.OrderedType
  module type S = Rb_dict.S
  module Make = Rb_dict.Make
end
