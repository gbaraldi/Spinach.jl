import LLVM

nfuncs = Ref(0)
struct Scope
    values::Dict{Symbol, LLVM.User}
    parent::Scope

    Scope() = new(Dict{Symbol, LLVM.AllocaInst}())
    Scope(parent::Scope) = new(Dict{Symbol, LLVM.User}(), parent)
end

# Look for variable in `scope` and if not found, look recursively in parent
function _get(scope::Scope, var::Symbol, default)
    v = get(scope.values, var, nothing)
    if v !== nothing
        return v
    else
        return isdefined(scope, :parent) ? _get(scope.parent, var, default) : default
    end
end

mutable struct CurrentScope
    scope::Scope
end

CurrentScope() = CurrentScope(Scope())
isglobalscope(c::CurrentScope) = !isdefined(c.scope, :parent)
open_scope!(currentscope::CurrentScope) = (currentscope.scope = Scope(currentscope.scope); currentscope)
Base.pop!(currentscope::CurrentScope) = (currentscope.scope = currentscope.scope.parent)
Base.setindex!(currentscope::CurrentScope, v::LLVM.User, var::Symbol) = currentscope.scope.values[var] = v
Base.get(currentscope::CurrentScope, var::Symbol, default) = _get(currentscope.scope, var, default)

mutable struct CodeGen
    ctx::LLVM.Context
    builder::LLVM.Builder
    current_scope::CurrentScope
    mod::LLVM.Module

    CodeGen(ctx::LLVM.Context) =
        new(
            ctx,
            LLVM.Builder(ctx),
            CurrentScope(),
            LLVM.Module("MSScheme"; ctx),
        )
end

current_scope(cg::CodeGen) = cg.current_scope
function new_scope(f, cg::CodeGen)
    open_scope!(current_scope(cg))
    f()
    pop!(current_scope(cg))
end

parse_expr(val::Float64, root = false) = NumberExprAST(val)
parse_expr(val::Symbol, root = false) = VarExprAST(val)
function parse_expr(expr::MyExpr, root = false)
    call = expr.call

    if call == :define 
        root || error("Syntax Error: Nested definitions not allowed \n $expr")
        val = expr.args[1]
        if val isa MyExpr
            fname = val.call
            args = val.args
            all(isa.(args, Symbol)) || error("Syntax Error: Only identifiers can be used as parameters")
            body = expr.args[end]
            newexpr = parse_expr(body)
            if (newexpr isa IfExprAST) && (newexpr.elsee isa CallExprAST)
                if newexpr.elsee.call == fname
                    return BodyTailExprAST(HeadFunExprAST(fname, Symbol[args...]),newexpr)
                end
            end
            return BodyFunExprAST(HeadFunExprAST(fname, Symbol[args...]), newexpr)
        end
        if val isa Symbol
            varname = val
            length(expr.args) != 2 && error("Syntax Error: variable definition can only have one expression \n $expr") 
            body = expr.args[end]
            newexpr =  parse_expr(body) 
            return DefVarExprAST(varname, newexpr)
        end

    elseif any(call .== (:+, :*, :/, :-))  #reduce functions
        return ReduceExprAST(call, parse_expr.(expr.args))

    elseif call == :if
        length(expr.args) != 3 && error("Syntax Error: if block requires the (if cond then else) Syntax \n $expr")
        cond = expr.args[1]
        then = expr.args[2]
        els = expr.args[3]
        return IfExprAST(parse_expr(cond), parse_expr(then), parse_expr(els))

    elseif any(call .== (:<, :>, :(==), :!=, :(<=), :(>=)))
        length(expr.args) != 2 && error("Syntax Error: Boolean Expression must have only two arguments \n $expr")
        op = call
        lhs = expr.args[1]
        rhs = expr.args[2]
        return BoolOpExprAST(op, parse_expr(rhs), parse_expr(lhs))
    
    else
        return CallExprAST(call, parse_expr.(expr.args))
    end
end


function create_entry_block_allocation(cg::CodeGen, fn::LLVM.Function, varname::String)
    local alloc
    LLVM.Builder(cg.ctx) do builder
        # Set the builder at the start of the function
        entry_block = LLVM.entry(fn)
        if isempty(LLVM.instructions(entry_block))
            LLVM.position!(builder, entry_block)
        else
            LLVM.position!(builder, first(LLVM.instructions(entry_block)))
        end
        alloc = LLVM.alloca!(builder, LLVM.DoubleType(cg.ctx), varname)
    end
    return alloc
end

function codegen(cg::CodeGen, expr::NumberExprAST)
    return LLVM.ConstantFP(LLVM.DoubleType(cg.ctx), expr.val)
end

function codegen(cg::CodeGen, expr::VarExprAST)
    V = get(current_scope(cg), expr.name, nothing)
    String(expr.name)
    V == nothing && error("Undefined Variable $(expr.name)")
    return LLVM.load!(cg.builder, V, String(expr.name))
end


function codegen(cg::CodeGen, expr::DefVarExprAST)
    local initval
    initval = codegen(cg, expr.init)
    varname = expr.varname
    local V
    if isglobalscope(current_scope(cg))
        V = LLVM.GlobalVariable(cg.mod, LLVM.DoubleType(cg.ctx), String(varname))
        LLVM.initializer!(V, initval)
    else
        func = LLVM.parent(LLVM.position(cg.builder))
        V = create_entry_block_allocation(cg, func, String(varname))
        LLVM.store!(cg.builder, initval, V)
    end
    current_scope(cg)[varname] = V
    return initval
end


function codegen(cg::CodeGen, expr::BoolOpExprAST)
    if isglobalscope(current_scope(cg))
        fname = Symbol(nfuncs[])
        nfuncs[] += 1
        head = HeadFunExprAST(fname,Symbol[])
        return codegen(cg, BodyFunExprAST(head, expr))
    end
    L = codegen(cg, expr.lhs)
    R = codegen(cg, expr.rhs)
    if expr.op == :(==)
        L = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealOEQ, L, R, "cmptmp")
        return LLVM.uitofp!(cg.builder, L, LLVM.DoubleType(cg.ctx), "booltmp")
    elseif expr.op == :(!=)
        L = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealONE, L, R, "cmptmp")
        return LLVM.uitofp!(cg.builder, L, LLVM.DoubleType(cg.ctx), "booltmp")
    elseif expr.op == :(<)
        L = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealOLT, L, R, "cmptmp")
        return LLVM.uitofp!(cg.builder, L, LLVM.DoubleType(cg.ctx), "booltmp")
    elseif expr.op == :(>)
        L = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealOGT, L, R, "cmptmp")
        return LLVM.uitofp!(cg.builder, L, LLVM.DoubleType(cg.ctx), "booltmp")
    elseif expr.op == :(<=)
        L = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealOLE, L, R, "cmptmp")
        return LLVM.uitofp!(cg.builder, L, LLVM.DoubleType(cg.ctx), "booltmp")
    elseif expr.op == :(>=)
        L = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealOGE, L, R, "cmptmp")
        return LLVM.uitofp!(cg.builder, L, LLVM.DoubleType(cg.ctx), "booltmp")
    else
        error("Operação booleana invalida $(expr.op)")
    end
end

function codegen(cg::CodeGen, expr::ReduceExprAST)
    if isglobalscope(current_scope(cg))
        fname = Symbol(nfuncs[])
        nfuncs[] += 1
        head = HeadFunExprAST(fname,Symbol[])
        return codegen(cg, BodyFunExprAST(head, expr))
    end
    op = expr.fun
    args = LLVM.Value[]
    for v in expr.args
        push!(args, codegen(cg, v))
    end
    tot = args[1]
    for arg in args[2:end]
        if op == :+
            tot = LLVM.fadd!(cg.builder, tot, arg, "addtmp")
        elseif op == :-
            tot = LLVM.fsub!(cg.builder, tot, arg, "addtmp")
        elseif op == :*
            tot = LLVM.fmul!(cg.builder, tot, arg, "addtmp")
        elseif op == :/
            tot = LLVM.fdiv!(cg.builder, tot, arg, "addtmp")
        else
            error("Não devia chegar aqui")
        end
    end
    return tot
end

function codegen(cg::CodeGen, expr::CallExprAST)
    if isglobalscope(current_scope(cg))
        fname = Symbol(nfuncs[])
        nfuncs[] += 1
        head = HeadFunExprAST(fname,Symbol[])
        return codegen(cg, BodyFunExprAST(head, expr))
    end
    if !haskey(LLVM.functions(cg.mod), String(expr.call))
        error("Undefined Function $(expr.call)")
    end
    func =  LLVM.functions(cg.mod)[String(expr.call)]

    if length(LLVM.parameters(func)) != length(expr.args)
        error("Incorrect number of arguments $fname requires $(length(expr.args)) arguments")
    end

    args = LLVM.Value[]
    for v in expr.args
        push!(args, codegen(cg, v))
    end

    return LLVM.call!(cg.builder, func, args, "calltmp")
end

function codegen(cg::CodeGen, expr::HeadFunExprAST)
    if haskey(LLVM.functions(cg.mod), String(expr.name))
            error("Can't redefine function")
    end
    args = [LLVM.DoubleType(cg.ctx) for i in 1:length(expr.args)]
    func_type = LLVM.FunctionType(LLVM.DoubleType(cg.ctx), args)
    func = LLVM.Function(cg.mod, String(expr.name), func_type)
    LLVM.linkage!(func, LLVM.API.LLVMExternalLinkage)

    for (i, param) in enumerate(LLVM.parameters(func))
        LLVM.name!(param, String(expr.args[i]))
    end
    return func
end

function codegen(cg::CodeGen, expr::BodyFunExprAST)
    # create new function...
    the_function = codegen(cg, expr.head)

    entry = LLVM.BasicBlock(the_function, "entry"; cg.ctx)
    LLVM.position!(cg.builder, entry)

    new_scope(cg) do
        for (i, param) in enumerate(LLVM.parameters(the_function))
            argname = expr.head.args[i]
            alloc = create_entry_block_allocation(cg, the_function, String(argname))
            LLVM.store!(cg.builder, param, alloc)
            current_scope(cg)[argname] = alloc
        end

        body = codegen(cg, expr.body)
        LLVM.ret!(cg.builder, body)
        LLVM.verify(the_function)
    end
    return the_function
end

function codegen(cg::CodeGen, expr::BodyTailExprAST)
    # create new function...
    the_function = codegen(cg, expr.head)

    entry = LLVM.BasicBlock(the_function, "entry"; cg.ctx)
    exit = LLVM.BasicBlock(the_function, "exit"; cg.ctx)
    tail = LLVM.BasicBlock(the_function, "tail"; cg.ctx)
    merge = LLVM.BasicBlock(the_function, "ifcont"; cg.ctx)
    body = expr.body
    local phi
    LLVM.position!(cg.builder, entry)
    new_scope(cg) do
        for (i, param) in enumerate(LLVM.parameters(the_function))
            argname = expr.head.args[i]
            alloc = create_entry_block_allocation(cg, the_function, String(argname))
            LLVM.store!(cg.builder, param, alloc)
            current_scope(cg)[argname] = alloc
        end
        
        #cond
        cond = codegen(cg, body.cond)
        zero = LLVM.ConstantFP(LLVM.DoubleType(cg.ctx), 0.0)
        condv = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealONE, cond, zero, "ifcond")
        LLVM.br!(cg.builder, condv, exit, tail)
        #exit
        LLVM.position!(cg.builder, exit)
        exitcg = codegen(cg, body.then)
        LLVM.br!(cg.builder, merge)
        exit_block = position(cg.builder)
        
        # tail
        LLVM.position!(cg.builder, tail)
        tailcg =codegen(cg, body.elsee)
        LLVM.tailcall!(tailcg, true)
        LLVM.ret!(cg.builder, tailcg)
        # LLVM.br!(cg.builder, merge)
        # tail_block = position(cg.builder)

        #merge
        LLVM.position!(cg.builder, merge)
        phi = LLVM.phi!(cg.builder, LLVM.DoubleType(cg.ctx), "iftmp")
        append!(LLVM.incoming(phi), [(exitcg, exit_block)])
        LLVM.ret!(cg.builder, phi)
        # LLVM.verify(the_function)
    end
    return the_function
end

function codegen(cg::CodeGen, expr::IfExprAST)
    if isglobalscope(current_scope(cg))
        fname = Symbol(nfuncs[])
        nfuncs[] += 1
        head = HeadFunExprAST(fname,Symbol[])
        return codegen(cg, BodyFunExprAST(head, expr))
    end
    
    func = LLVM.parent(LLVM.position(cg.builder))
    then = LLVM.BasicBlock(func, "then"; cg.ctx)
    elsee = LLVM.BasicBlock(func, "else"; cg.ctx)
    merge = LLVM.BasicBlock(func, "ifcont"; cg.ctx)
    
    local phi
    new_scope(cg) do
        # if
        cond = codegen(cg, expr.cond)
        zero = LLVM.ConstantFP(LLVM.DoubleType(cg.ctx), 0.0)
        condv = LLVM.fcmp!(cg.builder, LLVM.API.LLVMRealONE, cond, zero, "ifcond")
        LLVM.br!(cg.builder, condv, then, elsee)
        # then
        LLVM.position!(cg.builder, then)
        thencg = codegen(cg, expr.then)
        LLVM.br!(cg.builder, merge)
        then_block = position(cg.builder)

        # else
        LLVM.position!(cg.builder, elsee)
        elsecg = codegen(cg, expr.elsee)
        LLVM.br!(cg.builder, merge)
        else_block = position(cg.builder)

        # merge
        LLVM.position!(cg.builder, merge)
        phi = LLVM.phi!(cg.builder, LLVM.DoubleType(cg.ctx), "iftmp")
        append!(LLVM.incoming(phi), [(thencg, then_block), (elsecg, else_block)])
    end

    return phi
end

function generate_IR(code::String, ctx::LLVM.Context)
    nfuncs[] = 0
    exprs =global_machine(code)
    cg = CodeGen(ctx)
    for expr in exprs 
        codegen(cg, parse_expr(expr, true))
    end
    LLVM.verify(cg.mod)
    LLVM.dispose(cg.builder)
    return cg.mod
end

function run_module(mod::LLVM.Module, entry::String, optimize = false)
    res_jl = 0.0
    if optimize
        mod = optimize!(mod)
    end
    LLVM.JIT(mod) do engine
        if !haskey(LLVM.functions(engine), entry)
            error("Did not find entry function '$entry' in module")
        end
        f = LLVM.functions(engine)[entry]
        res = LLVM.run(engine, f)
        res_jl = convert(Float64, res, LLVM.DoubleType(LLVM.context(mod)))
        LLVM.dispose(res)
    end
    return res_jl
end

function write_objectfile(mod::LLVM.Module, path::String)
    host_triple = Sys.MACHINE # LLVM.triple() might be wrong (see LLVM.jl#108)
    host_t = LLVM.Target(triple=host_triple)
    LLVM.TargetMachine(host_t, host_triple) do tm
        LLVM.emit(tm, mod, LLVM.API.LLVMObjectFile, path)
    end
end

function optimize!(mod::LLVM.Module)
    LLVM.ModulePassManager() do pass_manager
        LLVM.instruction_combining!(pass_manager)
        LLVM.reassociate!(pass_manager)
        LLVM.gvn!(pass_manager)
        LLVM.cfgsimplification!(pass_manager)
        LLVM.promote_memory_to_register!(pass_manager)
        LLVM.run!(pass_manager, mod)
    end
    return mod
end

