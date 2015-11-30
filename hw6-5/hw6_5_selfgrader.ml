open CommonGrade
open Hw6_5

(* NOTE that this grader is not complete; any expression equivalent to the constant 0 is OK.  Read the specification. *)
let _ = output (fun () ->
  (diff (SUM [VAR "x"; CONST 1]) "x") = SUM [CONST 1; CONST 0])
let _ = output (fun () ->
  (diff (SUM [POWER("x",2); VAR "x"; CONST 1]) "x") = SUM [TIMES [CONST 2; POWER("x",1)]; CONST 1; CONST 0])
let _ = output (fun () ->
  (diff (SUM [VAR "x"; CONST 1]) "x") = SUM [CONST 1; CONST 0])
let _ = output (fun () ->
  (diff (SUM [VAR "x"; CONST 1]) "x") = SUM [CONST 1; CONST 0])
