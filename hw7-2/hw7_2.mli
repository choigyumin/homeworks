module type SKI =
  sig
    type liquid = S | K | I | V of string | M of liquid * liquid
    val react : liquid -> liquid
    val pprint : liquid -> string
  end
module SkiLiquid : SKI
