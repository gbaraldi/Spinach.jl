module Spinach
using Reexport
import LLVM
# Write your package code here.
    @reexport import LLVM
    include("create_ast.jl")
    include("codegen.jl")
    export generate_IR, global_machine, run_module
end
