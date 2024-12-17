(* operationalTypes.ml *)
open Ast;;
open Stack;;

type objectId = int

and
memoryLocation = 
| Object of objectId
| NullPointer 

and
functionClosure = var * block * stack

and
runtimeValue = 
| FieldValue of string
| IntegerValue of int
| Location of memoryLocation
| Closure of functionClosure

and
taintedRuntimeValue = 
| Valid of runtimeValue
| RuntimeError

and 
environment = (var * memoryLocation) list

and
frame =
| DeclarationFrame of environment
| CallFrame of environment * stack

and
stack = frame Stack.t

and
heapMemory = ((objectId * string) * taintedRuntimeValue) list

and
state = stack * heapMemory * int