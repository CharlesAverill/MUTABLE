open MUTABLE.Logging
open MUTABLE.Architecture

module Simpl = struct
  type opcode =
    | Incr
    (* incr dst src *)
    | Decr (* decr dst src *)

  type register = R1 | R2

  type value = VInt of int | VFloat of float

  type argument = AConst of value | AReg of register

  let base_state _ = VInt 0

  let incr_val (v : value) : value =
    match v with VInt i -> VInt (i + 1) | VFloat f -> VFloat (f +. 1.)

  let decr_val (v : value) : value =
    match v with VInt i -> VInt (i - 1) | VFloat f -> VFloat (f -. 1.)

  let step (s : (value, register) state) (instr : opcode * argument list) =
    match instr with
    | op, args -> (
      match op with
      | Incr -> (
        match args with
        | [dst; src] -> (
            let newval =
              match src with AConst c -> incr_val c | AReg r -> incr_val (s r)
            in
            match dst with
            | AConst _ ->
                Error "dst must be a register"
            | AReg reg ->
                Ok (update reg newval s) )
        | _ ->
            Error "Expected dst and src" )
      | Decr -> (
        match args with
        | [dst; src] -> (
            let newval =
              match src with AConst c -> decr_val c | AReg r -> decr_val (s r)
            in
            match dst with
            | AConst _ ->
                Error "dst must be a register"
            | AReg reg ->
                Ok (update reg newval s) )
        | _ ->
            Error "Expected dst and src" ) )

  let string_of_opcode = function Incr -> "incr" | Decr -> "decr"

  let string_of_register = function R1 -> "R1" | R2 -> "R2"

  let string_of_value = function
    | VInt i ->
        string_of_int i
    | VFloat f ->
        string_of_float f

  let string_of_argument = function
    | AConst c ->
        string_of_value c
    | AReg r ->
        string_of_register r

  let string_of_state s =
    String.concat "\n"
      (List.map
         (fun reg ->
           Printf.sprintf "%s: %s" (string_of_register reg)
             (string_of_value (s reg)) )
         [R1; R2] )
end

open Simpl

let prog = [(Simpl.Incr, [AReg R1; AReg R1])]

let _ =
  match interpret (module Simpl) prog with
  | Error e ->
      _log Log_Error e
  | Ok s ->
      print_endline (Simpl.string_of_state s)
