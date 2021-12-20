# ENV["JULIA_DEBUG"] = Main
struct MyToken
	token::String
	tipo::Symbol
end

struct MyExpr
	call::Symbol
	args::Vector{Any}
end

abstract type MyExprAST end
struct NumberExprAST <: MyExprAST
	val::Float64
end

struct HeadFunExprAST <: MyExprAST
	name::Symbol
	args::Vector{Symbol}
end

struct IfExprAST <: MyExprAST
    cond::MyExprAST
    then::MyExprAST
    elsee::MyExprAST
end

struct BodyFunExprAST <: MyExprAST
	head::HeadFunExprAST
	body::MyExprAST
end

struct BodyTailExprAST <: MyExprAST
	head::HeadFunExprAST
	body::IfExprAST
end
BodyTailExprAST(expr::BodyFunExprAST) = BodyTailExprAST(expr.head, expr.body)


struct DefVarExprAST <: MyExprAST
	varname::Symbol
	init::MyExprAST
end

struct VarExprAST <: MyExprAST
	name::Symbol
end

struct ReduceExprAST <: MyExprAST
	fun::Symbol
    args::Vector{MyExprAST}
end

struct CallExprAST <: MyExprAST
    call::Symbol
    args::Vector{MyExprAST}
end


struct BoolOpExprAST <: MyExprAST
	op::Symbol
	rhs::MyExprAST
	lhs::MyExprAST
end


const cursor = Ref{Int}()

const specials = ['!', '@', '#', '$', '^', '&', '*', '-', '_', '+', '=', '<', '>', '/', '?', ';']

myisletter(char) = char in 'a':'z' || char in specials
myisnumber(char) = char in '0':'9' 
myisacii(char) = myisletter(char)||myisnumber(char)
myisatom(token) = token.tipo == :name || token.tipo == :number || token.tipo == :string 
myisopen(token) = token.tipo == :open
myisclosed(token) = token.tipo == :closed
myisname(token) = token.tipo == :name
myistring(token) = token.tipo == :string
myisnumber(token::MyToken) = token.tipo == :number
function lexer(str)
	if str[1] == '"'
		ind = findfirst('"',str[2:end])
		if ind === nothing
			throw(ErrorException("String não foi fechada"))
		end
		return MyToken(str[1:ind], :string)
	end
	if str[1] == '('
		return MyToken("(", :open)
	end
	if str[1] == ')'
		return MyToken(")", :closed)
	end
	if str[1] == '-' || str[1] == '+'
		if str[2] == ' '
			return MyToken(string(str[1]), :name)
		end
		ind = findfirst(!myisnumber,str[2:end]) + 1
		if ind === nothing
			error("+ and - can't be used in the start of identifiers")
		end
		if str[ind] == '.'
			ind2 = findfirst(!myisnumber,str[ind+1:end])
			if ind === nothing
				return MyToken(str, :number)
			end
			return MyToken(str[1:ind+ind2-1], :number)
		end
		return MyToken(str[1:ind-1], :number)
		ind 
		if all(myisnumber.(str[2:end]))
			return MyToken(str, :number)
		else
			error("+ and - can't be used in the start of identifiers")
		end
	end
	if myisnumber(str[1])
		ind = findfirst(!myisnumber,str)
		if ind === nothing
			return MyToken(str, :number)
		end
		if str[ind] == '.'
			ind2 = findfirst(!myisnumber,str[ind+1:end])
			if ind === nothing
				return MyToken(str, :number)
			end
			return MyToken(str[1:ind+ind2-1], :number)
		end
		return MyToken(str[1:ind-1], :number)
	end
	if myisletter(str[1])
		ind = findfirst(!myisacii,str)
		if ind === nothing
			return MyToken(str, :name)
		end
		return MyToken(str[1:ind-1], :name)
	end
	throw(ErrorException("Invalid char $(str[1])"))
end

function increment(token)
	cursor[] += length(token.token)
end

function gettoken(str)
	token = lexer(str[cursor[]:end])
	increment(token)
	return token
end

function walkspace(str)
	ind = findfirst(x-> x != ' '&& x != '\n',str[cursor[]:end])
	if ind === nothing
		return nothing
	end
	cursor[] += ind -1 
end

function pusharg!(expr::MyExpr, arg)
	push!(expr.args, arg)
	nothing
end

function parse_token(token::MyToken)
	if myisname(token)
		return Symbol(token.token)
	elseif myisnumber(token)
		return parse(Float64,token.token)
	elseif myistring(token)
		# return token.token
		throw(ArgumentError("Strings não implementadas"))
	else
		throw(ArgumentError("Erro ao processar token"))
	end
end

function expr_machine(str)
	walkspace(str)
	token = gettoken(str)
	@debug "Token gerado" token
	if !myisname(token)
		throw(ErrorException("Syntax error $(token.token)"))
	end
	expr = MyExpr(parse_token(token), [])
	@debug "expr: Estado 1 para 2"
	walkspace(str)
	token2 = gettoken(str)
	@debug "Token gerado" token2
	if myisopen(token2)
		@debug "expr: Estado 2 para 3 e chamou submáquina de expressões"
		pusharg!(expr, expr_machine(str)) 
		@debug "expr: A submáquina de expressões terminou e foi para o estado 4"
	elseif myisatom(token2)
		@debug "expr: Estado 2 para 4"
		pusharg!(expr, parse_token(token2)) 
	else 
		throw(ErrorException("Syntax error $(token2.token)"))
	end
	walkspace(str)
	token3 = gettoken(str)
	@debug "Token gerado" token3
	while !myisclosed(token3)
		if myisopen(token3)
			@debug "expr: Estado 4 para 3 e chamou submáquina de expressões"
			pusharg!(expr, expr_machine(str)) 
			@debug "expr: A submáquina de expressões terminou e foi para o estado 4"
		elseif myisatom(token3)
			@debug "expr: Estado 4 para 4"
			pusharg!(expr, parse_token(token3))
		else
			throw(ErrorException("Syntax error $(token3.token)"))
		end
		walkspace(str)
		token3 = gettoken(str)
		@debug "Token gerado" token3
	end
	return expr
end

function root_machine(str)
	walkspace(str)
	token = gettoken(str)
	@debug "Token gerado" token
	if myisopen(token)
		@debug "root: Chamou a submáquina de expressões"
		expr = expr_machine(str)
		@debug "root: A submáquina de expressões terminou"
	return expr
	end
	if myisatom(token)
		@debug "root: Estado 1 pra 2 com atómo"
		return parse_token(token)
	end

	throw(ErrorException("Syntax error $(token.token)"))
end

function global_machine(str)
	@debug "Inicio do processo"
	@debug "Código a ser processado" str
	cursor[] = 1
	if length(str) == 1
		return [root_machine(str)]
	end
	
	exprs = Any[]
	while cursor[] < length(str)
		@debug "Chamou a máquina da raiz da expressão"
		push!(exprs, root_machine(str))
		@debug "Retornou da máquina da raiz da expressão"
	end
	return exprs
end
