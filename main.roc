app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
}

# Based on Peter Norvig's Python implementation
# https://norvig.com/lispy.html

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

Ast : [AtomNode Str, ListNode (List Ast)]

ReadErr : [MissingCloseParen, UnexpectedCloseParen]

parseStr = \str -> readFromTokens (tokenize str)

readFromTokens : List Str -> Result (List Ast) ReadErr
readFromTokens = \tokens ->
    if tokens == [] then
        Ok []
    else
        when readOnceFromTokens tokens is
            Ok (ast, moreTokens) ->
                when readFromTokens moreTokens is
                    Ok moreAsts -> Ok (List.prepend moreAsts ast)
                    Err err -> Err err

            Err err -> Err err

expect
    (readFromTokens (tokenize "(1 2(3))"))
    ==
    Ok [ListNode [AtomNode "1", AtomNode "2", ListNode [AtomNode "3"]]]

readOnceFromTokens : List Str -> Result (Ast, List Str) ReadErr
readOnceFromTokens = \tokens ->
    when tokens is
        [] -> Ok (AtomNode "Nil", [])
        ["(", .. as rest] -> readListFromTokens rest []
        [")", .. as rest] -> Err UnexpectedCloseParen
        [atom, .. as rest] -> Ok (AtomNode atom, rest)

readListFromTokens : List Str, List Ast -> Result ([ListNode (List Ast)], List Str) ReadErr
readListFromTokens = \tokens, acc ->
    when tokens is
        [] -> Err MissingCloseParen
        [")", .. as rest] -> Ok (ListNode acc, rest)
        ["(", .. as rest] ->
            when readListFromTokens rest [] is
                Ok (listNode, nextRest) ->
                    readListFromTokens nextRest (List.append acc listNode)

                Err err -> Err err

        [atom, .. as rest] ->
            nextAcc = List.concat acc [AtomNode atom]
            readListFromTokens rest nextAcc

Val : [
    IVal (Int Signed32),
    SymVal Str,
    ErrVal Str,
    ListVal (List Val),
    TVal,
    LambdaVal (List Str) (List Ast) Scope,
    BuiltInVal Str,
]

nilVal = ListVal []

Nat : Int Unsigned32
EnvKey : (Str, Nat)
Mem : Dict EnvKey Val
Scope : Dict Str EnvKey
Env : { mem : Mem, scope : Scope, nextSuffix : Nat }

envGet : Env, Str -> Result Val Str
envGet = \env, name ->
    when Dict.get env.scope name is
        Ok key ->
            when Dict.get env.mem key is
                Ok val -> Ok val
                Err _ -> Err "Name '$(name)' bound to missing reference (interpreter bug)"

        Err _ -> Err "Name '$(name)' not bound in scope"

envContains : Env, Str -> Bool
envContains = \env, name ->
    when Dict.get env.scope name is
        Ok key -> Bool.true
        Err _ -> Bool.false

envSet : Env, Str, Val -> Env
envSet = \env, name, val ->
    when Dict.get env.scope name is
        Ok key ->
            mem2 = Dict.insert env.mem key val
            { env & mem: mem2 }

        Err _ ->
            key = (name, env.nextSuffix)
            nextSuffix = env.nextSuffix + 1
            mem = Dict.insert env.mem key val
            scope = Dict.insert env.scope name key
            { mem, scope, nextSuffix }

envShadow : Env, Scope, Str, Val -> (Env, Scope)
envShadow = \env, auxScope, name, val ->
    key = (name, env.nextSuffix)
    nextSuffix = env.nextSuffix + 1
    mem = Dict.insert env.mem key val
    scope = Dict.insert env.scope name key
    auxScope2 = Dict.insert auxScope name key
    ({ mem, scope, nextSuffix }, auxScope2)

emptyEnv = { mem: Dict.empty {}, scope: Dict.empty {}, nextSuffix: 0 }

defaultEnv =
    setBuiltIn = \env, name ->
        envSet env name (BuiltInVal name)
    emptyEnv
    |> envSet "nil" (ListVal [])
    |> envSet "t" TVal
    |> setBuiltIn "+"
    |> setBuiltIn "-"
    |> setBuiltIn "*"
    |> setBuiltIn "/"
    |> setBuiltIn ">"
    |> setBuiltIn "<"
    |> setBuiltIn ">="
    |> setBuiltIn "<="
    |> setBuiltIn "="
    |> setBuiltIn "cons"
    |> setBuiltIn "car"
    |> setBuiltIn "cdr"
    |> setBuiltIn "length"
    |> setBuiltIn "list"
    |> setBuiltIn "list?"
    |> setBuiltIn "not"
    |> setBuiltIn "procedure?"
    |> setBuiltIn "symbol?"

lispStr : Ast -> Str
lispStr = \ast ->
    # Convert expression back into a Lisp-readable string
    when ast is
        AtomNode s -> s
        ListNode asts ->
            childStrs = List.map asts lispStr
            childrenStr = Str.joinWith childStrs " "
            "( $(childrenStr) )"

expect
    results =
        "(1 2(5))"
        |> tokenize
        |> readFromTokens
        |> Result.withDefault []
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

        TVal -> "t"
        LambdaVal params body _ ->
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

evalAtom : Str, Env -> Val
evalAtom = \s, env ->
    when Str.toI32 s is
        Ok n -> IVal n
        Err _ ->
            # Symbol
            when envGet env s is
                Ok val -> val
                Err msg -> ErrVal msg

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
eval : Ast, Env -> (Val, Env)
eval = \ast, env ->
    # Evaluate an expression in an environment
    doErr = \s -> (ErrVal s, env)
    when ast is
        AtomNode s -> (evalAtom s env, env)
        ListNode items -> evalList items env

applyBuiltIn : Str, List Ast, Env -> (Val, Env)
applyBuiltIn = \name, argForms, env ->
    naryReduceFn : List Ast, Val, Env, (Val, Val -> Val) -> (Val, Env)
    naryReduceFn = \argForms1, start, env1, fn ->
        when argForms1 is
            [] -> (start, env1)
            [first, .. as rest] ->
                (firstVal, env2) = eval first env1
                naryReduceFn rest (fn start firstVal) env2 fn
    binaryFn = \fn ->
        when argForms is
            [a, b] ->
                (aVal, env2) = eval a env
                (bVal, env3) = eval b env2
                (fn aVal bVal, env3)

            _ -> (ErrVal "$(name) requires 2 args", env)
    when name is
        "+" ->
            naryReduceFn argForms (IVal 0) env \a, b ->
                when (a, b) is
                    (IVal iA, IVal iB) -> IVal (iA + iB)
                    _ -> ErrVal "TypeError in +, args $(valStr a) $(valStr b)"

        "-" ->
            when argForms is
                [] -> (IVal 0, env)
                [first] ->
                    (firstVal, env2) = eval first env
                    when firstVal is
                        IVal a -> (IVal (0 - a), env2)
                        _ -> (ErrVal "TypeError in -, arg $(valStr firstVal)", env2)

                [first, .. as rest] ->
                    (firstVal, env2) = eval first env
                    naryReduceFn rest firstVal env2 \a, b ->
                        when (a, b) is
                            (IVal iA, IVal iB) -> IVal (iA - iB)
                            _ -> ErrVal "TypeError in -, args $(valStr a) $(valStr b)"

        "*" ->
            naryReduceFn argForms (IVal 1) env \a, b ->
                when (a, b) is
                    (IVal iA, IVal iB) -> IVal (iA * iB)
                    _ -> ErrVal "TypeError in *, args $(valStr a) $(valStr b)"

        "/" ->
            binaryFn \aVal, bVal ->
                when (aVal, bVal) is
                    (IVal iA, IVal iB) ->
                        when Num.divTruncChecked iA iB is
                            Ok n -> IVal n
                            Err DivByZero -> ErrVal "DivByZero"

                    _ -> ErrVal "TypeError in /, args $(valStr aVal) $(valStr bVal)"

        "<" ->
            binaryFn \aVal, bVal ->
                when (aVal, bVal) is
                    (IVal iA, IVal iB) ->
                        if iA < iB then TVal else nilVal

                    _ -> ErrVal "TypeError in <, args $(valStr aVal) $(valStr bVal)"

        ">" ->
            binaryFn \aVal, bVal ->
                when (aVal, bVal) is
                    (IVal iA, IVal iB) ->
                        if iA > iB then TVal else nilVal

                    _ -> ErrVal "TypeError in >, args $(valStr aVal) $(valStr bVal)"

        ">=" ->
            binaryFn \aVal, bVal ->
                when (aVal, bVal) is
                    (IVal iA, IVal iB) ->
                        if iA >= iB then TVal else nilVal

                    _ -> ErrVal "TypeError in >=, args $(valStr aVal) $(valStr bVal)"

        "<=" ->
            binaryFn \aVal, bVal ->
                when (aVal, bVal) is
                    (IVal iA, IVal iB) ->
                        if iA <= iB then TVal else nilVal

                    _ -> ErrVal "TypeError in <=, args $(valStr aVal) $(valStr bVal)"

        "cons" ->
            when argForms is
                [a, b] ->
                    (aVal, env2) = eval a env
                    (bVal, env3) = eval b env2
                    when bVal is
                        ListVal bVals -> (ListVal (List.prepend bVals aVal), env3)
                        _ -> (ErrVal "cons 2nd arg must be a list", env3)

                _ -> (ErrVal "cons requires 2 args", env)

        "car" ->
            when argForms is
                [a] ->
                    (aVal, env2) = eval a env
                    when aVal is
                        ListVal [] -> (ErrVal "car arg must be a non-empty list", env2)
                        ListVal [first, ..] -> (first, env2)
                        _ -> (ErrVal "car arg must be a list", env2)

                _ -> (ErrVal "car requires 1 arg, got $(Inspect.toStr argForms)", env)

        "cdr" ->
            when argForms is
                [a] ->
                    (aVal, env2) = eval a env
                    when aVal is
                        ListVal [] -> (ErrVal "cdr arg must be a non-empty list", env2)
                        ListVal [_, .. as rest] -> (ListVal rest, env2)
                        _ -> (ErrVal "cdr arg must be a list", env2)

                _ -> (ErrVal "cdr requires 1 arg", env)

        "length" ->
            when argForms is
                [a] ->
                    (aVal, env2) = eval a env
                    when aVal is
                        ListVal aVals -> (IVal (Num.intCast (List.len aVals)), env2)
                        _ -> (ErrVal "length arg must be a list", env2)

                _ -> (ErrVal "length requires 1 arg", env)

        "list" ->
            when argForms is
                [] -> (ListVal [], env)
                [first, .. as rest] ->
                    (firstVal, env2) = eval first env
                    (restVal, env3) = applyBuiltIn "list" rest env2
                    when restVal is
                        ListVal restVals -> (ListVal (List.prepend restVals firstVal), env3)
                        ErrVal _ -> (restVal, env3)
                        _ -> (ErrVal "list returned non-list (interpreter bug)", env3)

        "list?" ->
            when argForms is
                [item] ->
                    (itemVal, env2) = eval item env
                    when itemVal is
                        ListVal restVals -> (TVal, env2)
                        ErrVal _ -> (itemVal, env2)
                        _ -> (nilVal, env2)

                _ -> (ErrVal "list? requires 1 arg", env)

        "not" ->
            when argForms is
                [item] ->
                    (itemVal, env2) = eval item env
                    when itemVal is
                        ListVal [] -> (TVal, env2)
                        ErrVal _ -> (itemVal, env2)
                        _ -> (nilVal, env2)

                _ -> (ErrVal "not requires 1 arg", env)

        "procedure?" ->
            when argForms is
                [a] ->
                    (aVal, env2) = eval a env
                    when aVal is
                        LambdaVal _ _ _ -> (TVal, env2)
                        BuiltInVal _ -> (TVal, env2)
                        _ -> (nilVal, env2)

                _ -> (ErrVal "procedure? requires 1 arg", env)

        "symbol?" ->
            when argForms is
                [a] ->
                    (aVal, env2) = eval a env
                    when aVal is
                        SymVal _ -> (TVal, env2)
                        _ -> (nilVal, env2)

                _ -> (ErrVal "symbol? requires 1 arg", env)

        _ -> (ErrVal "Unknown built-in $(name)", env)
apply : Val, List Ast, Env -> (Val, Env)
apply = \fn, argForms, env ->
    doErr = \s -> (ErrVal s, env)
    when fn is
        LambdaVal params body scope ->
            if List.len params == List.len argForms then
                # Evaluate in the lambda's scope, then proceed with the old scope.
                env2 = bindArgs params argForms env scope
                (val, env3) = evalForms body env2
                (val, { env3 & scope: env.scope })
            else
                doErr "Wrong number of args"

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
                            # Set placeholder value for recursive lambda functions.
                            env2 = envSet env name nilVal
                            (val, env3) = eval exp env2
                            # Set real value
                            env4 = envSet env3 name val
                            (nilVal, env4)

                        [_, _] -> doErr "First arg of define must be a symbol"
                        _ -> doErr "Wrong number of args for define"

                AtomNode "set!" ->
                    when rest is
                        [AtomNode name, exp] ->
                            if envContains env name then
                                (val, env2) = eval exp env
                                env3 = envSet env2 name val
                                (nilVal, env3)
                            else
                                doErr "Cannot set! on undefined name '$(name)'"

                        [_, _] -> doErr "First arg of set! must be a symbol"
                        _ -> doErr "Wrong number of args for set!"

                AtomNode "lambda" ->
                    when rest is
                        [ListNode params, .. as body] ->
                            # Check that they are symbols?
                            paramNames = List.map params lispStr
                            (LambdaVal paramNames body env.scope, env)

                        _ -> doErr "Invalid lambda, expected param list"

                AtomNode s ->
                    firstVal = evalAtom s env
                    apply firstVal rest env

                ListNode asts ->
                    (firstVal, env2) = evalList asts env
                    apply firstVal rest env2
bindArgs : List Str, List Ast, Env, Scope -> Env
bindArgs = \params, args, env, scope ->
    # dbg params

    # dbg args

    when (params, args) is
        ([], _) -> { env & scope }
        (_, []) -> { env & scope }
        ([param, .. as paramRest], [arg, .. as argRest]) ->
            (val, env2) = eval arg env
            # dbg valStr val

            (env3, scope2) = envShadow env2 scope param val
            bindArgs paramRest argRest env3 scope2

        (_, _) -> env # Shouldn't be needed?

evalForms : List Ast, Env -> (Val, Env)
evalForms = \asts, env ->
    when asts is
        [] -> (ListVal [], env)
        [first] -> eval first env
        [first, .. as rest] ->
            (val, env2) = eval first env
            evalForms rest env2

readEvalPrint : Str -> Str
readEvalPrint = \str ->
    env = defaultEnv
    readResult = str |> tokenize |> readFromTokens
    when readResult is
        Ok asts ->
            (val, _) = asts |> evalForms env
            valStr val

        Err err -> Inspect.toStr err

run : Task {} _
run =
    rep : Env -> Task [Done {}, Step Env] _
    rep = \env ->
        when Stdout.write "> " |> Task.result! is
            Ok _ ->
                when Stdin.line |> Task.result! is
                    Ok input ->
                        readResult =
                            input
                            |> tokenize
                            |> readFromTokens
                        when readResult is
                            Ok asts ->
                                (val, env2) = evalForms asts env
                                when Stdout.line (valStr val) |> Task.result! is
                                    Ok _ -> Task.ok (Step env2)
                                    Err err -> Task.err err

                            Err err -> Task.err (StdoutErr (Other (Inspect.toStr err)))

                    Err (StdinErr EndOfFile) -> Task.ok (Done {})
                    Err err -> Task.err (StdoutErr (Other (Inspect.toStr err)))

            Err err -> Task.err err
    Task.loop! defaultEnv rep

main = run |> Task.onErr printErr

printErr : _ -> Task {} _
printErr = \err ->
    when err is
        _ -> Stderr.line "Error: $(Inspect.toStr err)"

# Test Env
expect
    env = emptyEnv
    (envGet env "a") == Err "Name 'a' not bound in scope"
expect
    env = envSet emptyEnv "a" (IVal 1)
    (envGet env "a") == Ok (IVal 1)

# Test readEvalPrint
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
    # Recursion
    result = readEvalPrint
        """
        (define rec (lambda (a) (if a nil (rec (not a)))))
        (rec t)
        (rec nil)
        """
    result == "(  )"
expect
    # Regression test for lambda scope issue
    result = readEvalPrint
        """
        (define fib (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 6)
        """
    result == "8"

expect
    result = readEvalPrint "((lambda (b) b) 1)"
    dbg result

    result == "1"

expect
    dbg readEvalPrint "(+ 1 1)"

    "2" == readEvalPrint "(+ 1 1)"
expect
    result = readEvalPrint "(- 1 1)"
    result == "0"
expect
    result = readEvalPrint "(- 1)"
    result == "-1"
expect
    result = readEvalPrint "(* 2 3)"
    result == "6"
expect
    result = readEvalPrint "(/ 5 2)"
    result == "2" # Truncating div
expect
    result = readEvalPrint "(cons 1 nil)"
    result == "( 1 )"
expect
    result = readEvalPrint "(car (cons 1 (cons 2 nil)))"
    result == "1"
expect
    result = readEvalPrint "(cdr (cons 1 (cons 2 nil)))"
    result == "( 2 )"
expect
    result = readEvalPrint "(length (cons 1 (cons 2 nil)))"
    result == "2"
expect
    result = readEvalPrint "(length (list 1 2))"
    result == "2"
expect
    result = readEvalPrint "(list? (list 1 2))"
    result == "t"
expect
    result = readEvalPrint "(list? nil)"
    result == "t"
expect
    result = readEvalPrint "(list? 43)"
    result == "(  )"
expect
    result = readEvalPrint "(not nil)"
    result == "t"
expect
    result = readEvalPrint "(not t)"
    result == "(  )"
expect
    result = readEvalPrint "(symbol? 4)"
    result == "(  )"
expect
    result = readEvalPrint "(symbol? (quote a))"
    result == "t"
expect
    result = readEvalPrint "(procedure? (lambda ()))"
    result == "t"
expect
    result = readEvalPrint "(procedure? +)"
    result == "t"
expect
    result = readEvalPrint "(procedure? (quote +))"
    result == "(  )"
expect
    # Lexical scope
    result = readEvalPrint "(define a 1) (define aGet (lambda () a)) (aGet)"
    dbg result

    result == "1"
expect
    # Lexical scope with update
    result = readEvalPrint "(define a 1) (define aGet (lambda () a)) (set! a 2) (aGet)"
    dbg result

    result == "2"
expect
    # Shadow Lexical scope
    result = readEvalPrint
        """
            (define a 1)
            (define aGet (lambda () a))
            (define getWrap (lambda (a) (aGet)))
            (getWrap 2)
        """
    dbg result

    result == "1"
expect
    result = readEvalPrint
        """
            (define max (lambda (a b) (if (< a b) b a)))
            (max 3 4)
        """
    result == "4"
expect
    result = readEvalPrint
        """
            (define abs (lambda (a) (if (< a 0) (- a) a)))
            (abs -5)
        """
    result == "5"
