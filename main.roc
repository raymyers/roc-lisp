app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
}

import pf.Stdout
import pf.Stdin
import pf.Stderr
import pf.Task exposing [Task]

tokenize = \s ->
    s
    |> Str.replaceEach "(" " ( "
    |> Str.replaceEach ")" " ) "
    |> Str.replaceEach "\r" " "
    |> Str.replaceEach "\n" " "
    |> Str.replaceEach "\t" " "
    |> Str.split " "
    |> List.dropIf \e -> e == ""

expect tokenize "(1 2(3))" == ["(", "1", "2", "(", "3", ")", ")"]

Ast: [AtomNode Str, ListNode (List (Ast)), ErrorNode Str]

parseStr = \str -> readFromTokens (tokenize str)

readFromTokens: List Str -> List (Ast)
readFromTokens = \tokens ->
    if tokens == [] then
        []
    else
        (ast, moreTokens) = readOnceFromTokens tokens
        List.prepend (readFromTokens moreTokens) ast


expect (readFromTokens (tokenize "(1 2(3))")) ==
    [(ListNode [(AtomNode "1"), (AtomNode "2"), (ListNode [(AtomNode "3")])])]

readOnceFromTokens: List Str -> (Ast, List Str)
readOnceFromTokens = \tokens ->
    when tokens is
        [] -> (AtomNode "Nil", [])
        ["(", .. as rest] -> readListFromTokens rest []
        [")", .. as rest] -> (ErrorNode "Unexpected )", rest)
        [atom, .. as rest] -> (AtomNode atom, rest)

readListFromTokens: List Str, List (Ast) -> ([ListNode (List (Ast))], List Str)
readListFromTokens = \tokens, acc ->
    when tokens is
        [] ->
            errorNode = ErrorNode "Missing )"
            (ListNode (List.append acc errorNode), [])
        [")", .. as rest] -> (ListNode acc, rest)
        ["(", .. as rest] ->
            (listNode, nextRest) = readListFromTokens rest []
            readListFromTokens nextRest (List.append acc listNode)
        [atom, .. as rest] ->
            nextAcc = List.concat acc [AtomNode atom]
            readListFromTokens rest nextAcc

Val : [
    IVal (Int Signed32),
    SymVal Str,
    ErrVal Str,
    ListVal (List Val),
    LambdaVal (List Str) (List Ast),
    BuiltInVal Str
]

nilVal = ListVal []

Env : Dict Str Val


lispStr : Ast -> Str
lispStr = \ast ->
    # Convert expression back into a Lisp-readable string
    when ast is
        AtomNode s -> s
        ListNode asts ->
            childStrs = List.map asts lispStr
            childrenStr = Str.joinWith childStrs " "
            "( $(childrenStr) )"
        ErrorNode s -> s

expect
    results =
        "(1 2(5))"
        |> tokenize
        |> readFromTokens
        |> List.map lispStr
    dbg results
    results == ["( 1 2 ( 5 ) )"]




valStr : Val -> Str
valStr = \val ->
    # Convert expression back into a Lisp-readable string
    when val is
        IVal n -> Num.toStr n
        ListVal vals ->
            childStrs = List.map vals valStr
            childrenStr = Str.joinWith childStrs " "
            "( $(childrenStr) )"
        LambdaVal params body ->
            paramsStr = Str.joinWith params " "
            bodyStr = body |> List.map lispStr |> Str.joinWith " "
            "(lambda ($(paramsStr)) $(bodyStr)"
        BuiltInVal name ->
            "#builtIn-$(name)"
        SymVal s -> s
        ErrVal s -> s


builtIn2Arg = \name, f ->
    BuiltInVal name \args ->
        when args is
            [arg1, arg2] -> f arg1 arg2
            _ -> ErrVal "Wrong number of args for '$(name)'"
standardEnv =
    builtInPlus = builtIn2Arg "+" \a, b ->
        when (a, b) is
            (IVal iA, IVal iB) -> IVal (iA + iB)
            _ -> ErrVal "Cannot add non-int"
    Dict.empty {}
    |> Dict.insert "+" (BuiltInVal "+")

evalAtom : Str, Env -> Val
evalAtom = \s, env ->
    when Str.toI32 s is
        Ok n -> IVal n
        Err _ ->
            # Symbol
            when Dict.get env s is
                Ok val -> val
                Err _ -> ErrVal "Undefined symbol '$(s)'"

evalIf : List Ast, Env -> (Val, Env)
evalIf = \rest, env ->
    doErr = \s -> (ErrVal s, env)
    when rest is
        [test, conseq, alt] ->
            (testVal, env2) = eval test env
            when testVal is
                ErrVal err -> (ErrVal err, env2)
                ListVal [] -> eval alt env2
                _ -> eval conseq env2
        _ -> doErr "Wrong number of args for if"
quote : Ast -> Val
quote = \ast ->
    when ast is
        AtomNode s -> SymVal s
        ListNode children -> ListVal (List.map children quote)
        ErrorNode s -> ErrVal s
eval : Ast, Env -> (Val, Env)
eval = \ast, env ->
    # Evaluate an expression in an environment
    doErr = \s -> (ErrVal s, env)
    when ast is
        AtomNode s -> (evalAtom s env, env)
        ListNode items -> evalList items env
        ErrorNode s -> (ErrVal s, env)

applyBuiltIn : Str, List Ast, Env -> (Val, Env)
applyBuiltIn = \name, argForms, env ->
    when name is
        "+" ->
            when argForms is
                [] -> (IVal 0, env)
                [first, .. as rest] ->
                    (firstVal, env2) = eval first env
                    (restVal, env3) = applyBuiltIn "+" rest env2
                    when (firstVal, restVal) is
                        (IVal a, IVal b) -> (IVal (a + b), env3)
                        _ -> (ErrVal "TypeError in +", env3)
        _ -> (ErrVal "TypeError in +", env)
apply : Val, List Ast, Env -> (Val, Env)
apply = \fn, argForms, env ->
    doErr = \s -> (ErrVal s, env)
    when fn is
        LambdaVal params body ->
            if List.len params == List.len argForms then
                env3 = bindArgs params argForms env
                evalForms body env3
            else doErr "Wrong number of args"
        BuiltInVal name ->
            applyBuiltIn name argForms env
        _ -> doErr "Can't apply non-procedure"

evalList : List Ast, Env -> (Val, Env)
evalList = \items, env ->
    doErr = \s -> (ErrVal s, env)
    when items is
        [] -> (ListVal [], env)
        [first, .. as rest] ->
            when first is
                AtomNode "quote" ->
                    when rest is
                        [arg] -> (quote arg, env)
                        _ -> doErr "Wrong number of args for quote"
                AtomNode "if" -> evalIf rest env
                AtomNode "define" ->
                    when rest is
                        [AtomNode name, exp] ->
                            (val, env2) = eval exp env
                            env3 = Dict.insert env2 name val
                            (nilVal, env3)
                        [_, _] -> doErr "First arg of define must be a symbol"
                        _ -> doErr "Wrong number of args for define"
                AtomNode "set!" ->
                    when rest is
                        [AtomNode name, exp] ->
                            if Dict.contains env name then
                                (val, env2) = eval exp env
                                env3 = Dict.insert env2 name val
                                (nilVal, env3)
                            else doErr "Cannot set! on undefined name '$(name)'"
                        [_, _] -> doErr "First arg of set! must be a symbol"
                        _ -> doErr "Wrong number of args for set!"
                AtomNode "lambda" ->
                    when rest is
                        [ListNode params, .. as body] ->
                            # Check that they are symbols?
                            paramNames = List.map params lispStr
                            (LambdaVal paramNames body, env)
                        _ -> doErr "Invalid lambda, expected param list"
                ErrorNode s -> (ErrVal s, env)
                AtomNode s ->
                    firstVal = evalAtom s env
                    apply firstVal rest env
                ListNode asts ->
                    (firstVal, env2) = evalList asts env
                    apply firstVal rest env2
bindArgs : List Str, List Ast, Env -> Env
bindArgs = \params, args, env ->
    when (params, args) is
        ([], _) -> env
        (_, []) -> env
        ([param, .. as paramRest], [arg, .. as argRest]) ->
            (val, env2) = eval arg env
            env3 = Dict.insert env2 param val
            bindArgs paramRest argRest env3
        (_, _) -> env # Shouldn't be needed?

evalForms : List Ast, Env -> (Val, Env)
evalForms = \asts, env ->
    when asts is
        [] -> (ListVal [], env)
        [first] -> eval first env
        [first, .. as rest] ->
            (val, env2) = eval first env
            evalForms rest env2
readEvalPrint = \str ->
    env = standardEnv
    (val, env2) = str
        |> tokenize
        |> readFromTokens
        |> evalForms env
    valStr val

expect
    result = readEvalPrint "1"
    dbg result
    result == "1"

expect
    result = readEvalPrint "()"
    dbg result
    result == "(  )"


expect
    result = readEvalPrint "(quote a)"
    dbg result
    result == "a"

expect
    result = readEvalPrint "(if 1 2 3)"
    dbg result
    result == "2"
expect
    result = readEvalPrint "(define a 4) a"
    dbg result
    result == "4"
expect
    result = readEvalPrint "(define a 4) (set! a 3) a"
    dbg result
    result == "3"

expect
    result = readEvalPrint "(define a (lambda (b) b)) (a 1)"
    dbg result
    result == "1"
expect
    result = readEvalPrint "((lambda (b) b) 1)"
    dbg result
    result == "1"
expect
    dbg readEvalPrint "(+ 1 1)"
    "2" == readEvalPrint "(+ 1 1)"
#expect
#    result = readEvalPrint "(define a (lambda (b) b)) (a 5)"
#    dbg result
#    result == "5"


#FunVal = List a

#standard_env =
#    env :: Dict Str FunVal
#    env = Dict.empty {}
#        # sin, cos, sqrt, pi, ...
#        |> Dict.insert "+"
#    env
# =


#    |> Dict.insert "Philadelphia" 1_603_797
#    |> Dict.insert "Shanghai" 24_870_895
#    |> Dict.insert "Delhi" 16_787_941
#    |> Dict.insert "Amsterdam" 872_680


run : Task {} _
run =
    rep : Env -> Task [Done {}, Step Env] _
    rep = \env ->
        when Stdout.write "> " |> Task.result! is
            Ok _ ->
                when Stdin.line |> Task.result! is
                    Ok input ->
                        (val, env2) = input
                            |> tokenize
                            |> readFromTokens
                            |> evalForms env
                        when Stdout.line (valStr val) |> Task.result! is
                            Ok _ -> Task.ok (Step env2)
                            Err err -> Task.err err
                    Err (StdinErr EndOfFile) -> Task.ok (Done {})
                    Err err -> Task.err (StdoutErr (Other (Inspect.toStr err)))
            Err err -> Task.err err
    defaultEnv = Dict.empty {}
    Task.loop! defaultEnv rep

main = run |> Task.onErr printErr

printErr : _ -> Task {} _
printErr = \err ->
    when err is
        _ -> Stderr.line "Error: $(Inspect.toStr err)"
