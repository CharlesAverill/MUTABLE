type ('argument, 'opcode) instruction = 'opcode * 'argument list

type ('value, 'register) state = 'register -> 'value

let update (reg : 'register) (newval : 'value)
    (oldstate : ('value, 'regsiter) state) : ('value, 'regsiter) state =
 fun r -> if r = reg then newval else oldstate r

module type Architecture = sig
  (* The names of instructions *)
  type opcode

  (* The names of registers *)
  type register

  (* What registers can contain *)
  type value

  (* Possible instruction argument types *)
  type argument

  (* The default state of the CPU *)
  val base_state : (value, register) state

  (* The small-step semantics of the architecture *)
  val step :
       (value, register) state
    -> (argument, opcode) instruction
    -> ((value, register) state, string) result

  (* For visibility *)
  val string_of_opcode : opcode -> string

  val string_of_register : register -> string

  val string_of_value : value -> string

  val string_of_argument : argument -> string

  val string_of_state : (value, register) state -> string
end

let interpret (type opcode register value argument)
    (module A : Architecture
      with type opcode = opcode
       and type register = register
       and type value = value
       and type argument = argument ) (o : (argument, opcode) instruction list)
    =
  List.fold_left
    (fun (sr : ((value, register) state, string) result)
         (i : (argument, opcode) instruction) ->
      match sr with Ok s -> A.step s i | _ -> sr )
    (Ok A.base_state) o
