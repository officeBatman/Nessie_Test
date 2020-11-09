module Nessie.IR.InstructionCode


let ofInstruction =
    function
    | BindNameInt _ -> InstructionCode.BindNameInt
    | BindNameNum _ -> InstructionCode.BindNameNum
    | BindName _ -> InstructionCode.BindName
    | UnbindName _ -> InstructionCode.UnbindName
    | LoadName _ -> InstructionCode.LoadName
    | Load _ -> InstructionCode.Load
    | LoadInt _ -> InstructionCode.LoadInt
    | LoadNum _ -> InstructionCode.LoadNum
    | Add _ -> InstructionCode.Add
    | Lambda _ -> InstructionCode.Lambda
    | App _ -> InstructionCode.App