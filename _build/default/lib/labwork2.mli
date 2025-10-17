module RBDict : sig
  module type OrderedType = Rb_dict.OrderedType

  module type S = Rb_dict.S

  module Make (Ord : OrderedType) : S with type key = Ord.t
end
