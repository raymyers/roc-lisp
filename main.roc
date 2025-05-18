app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
}

import pf.Stdout
import pf.Stdin
import pf.Arg

# Based on Peter Norvig's Python implementation
# https://norvig.com/lispy.html

tokenize = |s|
    s
    |> Str.replace_each("(", " ( ")
    |> Str.replace_each(")", " ) ")
    |> Str.replace_each("\r", " ")
    |> Str.replace_each("\n", " ")
    |> Str.replace_each("\t", " ")
    |> Str.split_on(" ")
    |> List.drop_if(|e| e == "")

expect tokenize("(1 2(3))") == ["(", "1", "2", "(", "3", ")", ")"]

Ast : [AtomNode Str, ListNode (List Ast)]

ReadErr : [MissingCloseParen, UnexpectedCloseParen]

# parse_str = |str| read_from_tokens(tokenize(str))

read_from_tokens : List Str -> Result (List Ast) ReadErr
read_from_tokens = |tokens|
    if tokens == [] then
        Ok([])
    else
        when read_once_from_tokens(tokens) is
            Ok((ast, more_tokens)) ->
                when read_from_tokens(more_tokens) is
                    Ok(more_asts) -> Ok(List.prepend(more_asts, ast))
                    Err(err) -> Err(err)

            Err(err) -> Err(err)

expect
    read_from_tokens(tokenize("(1 2(3))"))
    ==
    Ok([ListNode([AtomNode("1"), AtomNode("2"), ListNode([AtomNode("3")])])])

read_once_from_tokens : List Str -> Result (Ast, List Str) ReadErr
read_once_from_tokens = |tokens|
    when tokens is
        [] -> Ok((AtomNode("Nil"), []))
        ["(", .. as rest] -> read_list_from_tokens(rest, [])
        [")", ..] -> Err(UnexpectedCloseParen)
        [atom, .. as rest] -> Ok((AtomNode(atom), rest))

read_list_from_tokens : List Str, List Ast -> Result ([ListNode (List Ast)], List Str) ReadErr
read_list_from_tokens = |tokens, acc|
    when tokens is
        [] -> Err(MissingCloseParen)
        [")", .. as rest] -> Ok((ListNode(acc), rest))
        ["(", .. as rest] ->
            when read_list_from_tokens(rest, []) is
                Ok((list_node, next_rest)) ->
                    read_list_from_tokens(next_rest, List.append(acc, list_node))

                Err(err) -> Err(err)

        [atom, .. as rest] ->
            next_acc = List.concat(acc, [AtomNode(atom)])
            read_list_from_tokens(rest, next_acc)

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

env_get : Env, Str -> Result Val Str
env_get = |env, name|
    when Dict.get(env.scope, name) is
        Ok(key) ->
            when Dict.get(env.mem, key) is
                Ok(val) -> Ok(val)
                Err(_) -> Err("Name '${name}' bound to missing reference (interpreter bug)")

        Err(_) -> Err("Name '${name}' not bound in scope")

env_contains : Env, Str -> Bool
env_contains = |env, name|
    when Dict.get(env.scope, name) is
        Ok(_) -> Bool.true
        Err(_) -> Bool.false

env_set : Env, Str, Val -> Env
env_set = |env, name, val|
    when Dict.get(env.scope, name) is
        Ok(key) ->
            mem2 = Dict.insert(env.mem, key, val)
            { env & mem: mem2 }

        Err(_) ->
            key = (name, env.nextSuffix)
            next_suffix = env.nextSuffix + 1
            mem = Dict.insert(env.mem, key, val)
            scope = Dict.insert(env.scope, name, key)
            { mem, scope, nextSuffix: next_suffix }

empty_env = { mem: Dict.empty({}), scope: Dict.empty({}), nextSuffix: 0 }

default_env =
    set_built_in = |env, name|
        env_set(env, name, BuiltInVal(name))
    empty_env
    |> env_set("nil", ListVal([]))
    |> env_set("t", TVal)
    |> set_built_in("+")
    |> set_built_in("-")
    |> set_built_in("*")
    |> set_built_in("/")
    |> set_built_in(">")
    |> set_built_in("<")
    |> set_built_in(">=")
    |> set_built_in("<=")
    |> set_built_in("=")
    |> set_built_in("cons")
    |> set_built_in("car")
    |> set_built_in("cdr")
    |> set_built_in("length")
    |> set_built_in("list")
    |> set_built_in("list?")
    |> set_built_in("not")
    |> set_built_in("equal?")
    |> set_built_in("procedure?")
    |> set_built_in("symbol?")

lisp_str : Ast -> Str
lisp_str = |ast|
    # Convert expression back into a Lisp-readable string
    when ast is
        AtomNode(s) -> s
        ListNode(asts) ->
            child_strs = List.map(asts, lisp_str)
            children_str = Str.join_with(child_strs, " ")
            "( ${children_str} )"

expect
    results =
        "(1 2(5))"
        |> tokenize
        |> read_from_tokens
        |> Result.with_default([])
        |> List.map(lisp_str)
    dbg results
    results == ["( 1 2 ( 5 ) )"]

val_str : Val -> Str
val_str = |val|
    # Convert expression back into a Lisp-readable string
    when val is
        IVal(n) -> Num.to_str(n)
        ListVal(vals) ->
            child_strs = List.map(vals, val_str)
            children_str = Str.join_with(child_strs, " ")
            "( ${children_str} )"

        TVal -> "t"
        LambdaVal(params, body, _) ->
            params_str = Str.join_with(params, " ")
            body_str = body |> List.map(lisp_str) |> Str.join_with(" ")
            "(lambda (${params_str}) ${body_str}"

        BuiltInVal(name) ->
            "#builtIn-${name}"

        SymVal(s) -> s
        ErrVal(s) -> s
val_equal : Val, Val -> Bool
val_equal = |a, b|
    when (a, b) is
        (IVal(iA), IVal(iB)) -> iA == iB
        (TVal, TVal) -> Bool.true
        (ListVal(aVals), ListVal(bVals)) ->
            if List.len(aVals) == List.len(bVals) then
                List.all(List.map2(aVals, bVals, val_equal), |x| x)
            else
                Bool.false

        (BuiltInVal(aName), BuiltInVal(bName)) -> aName == bName
        (SymVal(aName), SymVal(bName)) -> aName == bName
        (_, _) -> Bool.false

expect val_equal(IVal(2), IVal(2))
expect !(val_equal(IVal(1), IVal(2)))
expect val_equal(ListVal([TVal]), ListVal([TVal]))
expect !(val_equal(ListVal([IVal(1)]), ListVal([IVal(2)])))
expect !(val_equal(ListVal([IVal(1)]), ListVal([IVal(1), IVal(2)])))

# built_in_2_arg = |name, f|
#     BuiltInVal(name, |args|
#         when args is
#             [arg1, arg2] -> f(arg1, arg2)
#             _ -> ErrVal("Wrong number of args for '${name}'"))

eval_atom : Str, Env -> Val
eval_atom = |s, env|
    when Str.to_i32(s) is
        Ok(n) -> IVal(n)
        Err(_) ->
            # Symbol
            when env_get(env, s) is
                Ok(val) -> val
                Err(msg) -> ErrVal(msg)

eval_if : List Ast, Env -> (Val, Env)
eval_if = |rest, env|
    when rest is
        [test, conseq, alt] ->
            (test_val, env2) = eval(test, env)
            when test_val is
                ErrVal(err) -> (ErrVal(err), env2)
                ListVal([]) -> eval(alt, env2)
                _ -> eval(conseq, env2)

        _ -> (ErrVal("Wrong number of args for if"), env)
quote : Ast -> Val
quote = |ast|
    when ast is
        AtomNode(s) -> SymVal(s)
        ListNode(children) -> ListVal(List.map(children, quote))
eval : Ast, Env -> (Val, Env)
eval = |ast, env|
    # Evaluate an expression in an environment
    when ast is
        AtomNode(s) -> (eval_atom(s, env), env)
        ListNode(items) -> eval_list(items, env)

apply_built_in : Str, List Ast, Env -> (Val, Env)
apply_built_in = |name, arg_forms, env|
    nary_reduce_fn : List Ast, Val, Env, (Val, Val -> Val) -> (Val, Env)
    nary_reduce_fn = |arg_forms1, start, env1, fn|
        when arg_forms1 is
            [] -> (start, env1)
            [first, .. as rest] ->
                (first_val, env2) = eval(first, env1)
                nary_reduce_fn(rest, fn(start, first_val), env2, fn)
    binary_fn = |fn|
        when arg_forms is
            [a, b] ->
                (a_val, env2) = eval(a, env)
                (b_val, env3) = eval(b, env2)
                (fn(a_val, b_val), env3)

            _ -> (ErrVal("${name} requires 2 args"), env)
    when name is
        "+" ->
            nary_reduce_fn(
                arg_forms,
                IVal(0),
                env,
                |a, b|
                    when (a, b) is
                        (IVal(iA), IVal(iB)) -> IVal(iA + iB)
                        _ -> ErrVal("TypeError in +, args ${val_str(a)} ${val_str(b)}"),
            )

        "-" ->
            when arg_forms is
                [] -> (IVal(0), env)
                [first] ->
                    (first_val, env2) = eval(first, env)
                    when first_val is
                        IVal(a) -> (IVal(0 - a), env2)
                        _ -> (ErrVal("TypeError in -, arg ${val_str(first_val)}"), env2)

                [first, .. as rest] ->
                    (first_val, env2) = eval(first, env)
                    nary_reduce_fn(
                        rest,
                        first_val,
                        env2,
                        |a, b|
                            when (a, b) is
                                (IVal(iA), IVal(iB)) -> IVal(iA - iB)
                                _ -> ErrVal("TypeError in -, args ${val_str(a)} ${val_str(b)}"),
                    )

        "*" ->
            nary_reduce_fn(
                arg_forms,
                IVal(1),
                env,
                |a, b|
                    when (a, b) is
                        (IVal(iA), IVal(iB)) -> IVal(iA * iB)
                        _ -> ErrVal("TypeError in *, args ${val_str(a)} ${val_str(b)}"),
            )

        "/" ->
            binary_fn(
                |a_val, b_val|
                    when (a_val, b_val) is
                        (IVal(iA), IVal(iB)) ->
                            when Num.div_trunc_checked(iA, iB) is
                                Ok(n) -> IVal(n)
                                Err(DivByZero) -> ErrVal("DivByZero")

                        _ -> ErrVal("TypeError in /, args ${val_str(a_val)} ${val_str(b_val)}"),
            )

        "<" ->
            binary_fn(
                |a_val, b_val|
                    when (a_val, b_val) is
                        (IVal(iA), IVal(iB)) ->
                            if iA < iB then TVal else nilVal

                        _ -> ErrVal("TypeError in <, args ${val_str(a_val)} ${val_str(b_val)}"),
            )

        ">" ->
            binary_fn(
                |a_val, b_val|
                    when (a_val, b_val) is
                        (IVal(iA), IVal(iB)) ->
                            if iA > iB then TVal else nilVal

                        _ -> ErrVal("TypeError in >, args ${val_str(a_val)} ${val_str(b_val)}"),
            )

        ">=" ->
            binary_fn(
                |a_val, b_val|
                    when (a_val, b_val) is
                        (IVal(iA), IVal(iB)) ->
                            if iA >= iB then TVal else nilVal

                        _ -> ErrVal("TypeError in >=, args ${val_str(a_val)} ${val_str(b_val)}"),
            )

        "<=" ->
            binary_fn(
                |a_val, b_val|
                    when (a_val, b_val) is
                        (IVal(iA), IVal(iB)) ->
                            if iA <= iB then TVal else nilVal

                        _ -> ErrVal("TypeError in <=, args ${val_str(a_val)} ${val_str(b_val)}"),
            )

        "cons" ->
            when arg_forms is
                [a, b] ->
                    (a_val, env2) = eval(a, env)
                    (b_val, env3) = eval(b, env2)
                    when b_val is
                        ListVal(b_vals) -> (ListVal(List.prepend(b_vals, a_val)), env3)
                        _ -> (ErrVal("cons 2nd arg must be a list"), env3)

                _ -> (ErrVal("cons requires 2 args"), env)

        "car" ->
            when arg_forms is
                [a] ->
                    (a_val, env2) = eval(a, env)
                    when a_val is
                        ListVal([]) -> (ErrVal("car arg must be a non-empty list"), env2)
                        ListVal([first, ..]) -> (first, env2)
                        _ -> (ErrVal("car arg must be a list"), env2)

                _ -> (ErrVal("car requires 1 arg, got ${Inspect.to_str(arg_forms)}"), env)

        "cdr" ->
            when arg_forms is
                [a] ->
                    (a_val, env2) = eval(a, env)
                    when a_val is
                        ListVal([]) -> (ErrVal("cdr arg must be a non-empty list"), env2)
                        ListVal([_, .. as rest]) -> (ListVal(rest), env2)
                        _ -> (ErrVal("cdr arg must be a list"), env2)

                _ -> (ErrVal("cdr requires 1 arg"), env)

        "length" ->
            when arg_forms is
                [a] ->
                    (a_val, env2) = eval(a, env)
                    when a_val is
                        ListVal(a_vals) -> (IVal(Num.int_cast(List.len(a_vals))), env2)
                        _ -> (ErrVal("length arg must be a list"), env2)

                _ -> (ErrVal("length requires 1 arg"), env)

        "list" ->
            when arg_forms is
                [] -> (ListVal([]), env)
                [first, .. as rest] ->
                    (first_val, env2) = eval(first, env)
                    (rest_val, env3) = apply_built_in("list", rest, env2)
                    when rest_val is
                        ListVal(rest_vals) -> (ListVal(List.prepend(rest_vals, first_val)), env3)
                        ErrVal(_) -> (rest_val, env3)
                        _ -> (ErrVal("list returned non-list (interpreter bug)"), env3)

        "list?" ->
            when arg_forms is
                [item] ->
                    (item_val, env2) = eval(item, env)
                    when item_val is
                        ListVal(_) -> (TVal, env2)
                        ErrVal(_) -> (item_val, env2)
                        _ -> (nilVal, env2)

                _ -> (ErrVal("list? requires 1 arg"), env)

        "not" ->
            when arg_forms is
                [item] ->
                    (item_val, env2) = eval(item, env)
                    when item_val is
                        ListVal([]) -> (TVal, env2)
                        ErrVal(_) -> (item_val, env2)
                        _ -> (nilVal, env2)

                _ -> (ErrVal("not requires 1 arg"), env)

        "equal?" ->
            binary_fn(
                |a_val, b_val|
                    if val_equal(a_val, b_val) then TVal else nilVal,
            )

        "procedure?" ->
            when arg_forms is
                [a] ->
                    (a_val, env2) = eval(a, env)
                    when a_val is
                        LambdaVal(_, _, _) -> (TVal, env2)
                        BuiltInVal(_) -> (TVal, env2)
                        _ -> (nilVal, env2)

                _ -> (ErrVal("procedure? requires 1 arg"), env)

        "symbol?" ->
            when arg_forms is
                [a] ->
                    (a_val, env2) = eval(a, env)
                    when a_val is
                        SymVal(_) -> (TVal, env2)
                        _ -> (nilVal, env2)

                _ -> (ErrVal("symbol? requires 1 arg"), env)

        _ -> (ErrVal("Unknown built-in ${name}"), env)
apply : Val, List Ast, Env -> (Val, Env)
apply = |fn, arg_forms, env|
    when fn is
        LambdaVal(params, body, lambda_scope) ->
            if List.len(params) == List.len(arg_forms) then
                # Save the original scope
                original_scope = env.scope

                # Create a new environment with the lambda's scope
                lambda_env = { env & scope: lambda_scope }

                # Evaluate arguments and bind them to parameters
                lambda_env_with_args =
                    List.walk_with_index(
                        arg_forms,
                        lambda_env,
                        |env_acc, arg, i|
                            # Get the parameter name
                            when List.get(params, i) is
                                Ok(param) ->
                                    # Evaluate the argument in the original environment
                                    (arg_val, _) = eval(arg, env)
                                    # Bind the parameter in the lambda environment
                                    env_set(env_acc, param, arg_val)

                                _ -> env_acc,
                    )

                # Evaluate the body in the lambda's environment
                (result, _) = eval_forms(body, lambda_env_with_args)

                # Return the result with the original scope restored
                (result, { env & scope: original_scope })
            else
                (ErrVal("Wrong number of args"), env)

        BuiltInVal(name) ->
            apply_built_in(name, arg_forms, env)

        _ -> (ErrVal("Can't apply non-procedure"), env)

eval_list : List Ast, Env -> (Val, Env)
eval_list = |items, env|
    when items is
        [] -> (ListVal([]), env)
        [first, .. as rest] ->
            when first is
                AtomNode("quote") ->
                    when rest is
                        [arg] -> (quote(arg), env)
                        _ -> (ErrVal("Wrong number of args for quote"), env)

                AtomNode("if") -> eval_if(rest, env)
                AtomNode("define") ->
                    when rest is
                        [AtomNode(name), exp] ->
                            # Set placeholder value for recursive lambda functions.
                            env2 = env_set(env, name, nilVal)
                            (val, env3) = eval(exp, env2)
                            # Set real value
                            env4 = env_set(env3, name, val)
                            (nilVal, env4)

                        [_, _] -> (ErrVal("First arg of define must be a symbol"), env)
                        _ -> (ErrVal("Wrong number of args for define"), env)

                AtomNode("set!") ->
                    when rest is
                        [AtomNode(name), exp] ->
                            if env_contains(env, name) then
                                (val, env2) = eval(exp, env)
                                env3 = env_set(env2, name, val)
                                (nilVal, env3)
                            else
                                (ErrVal("Cannot set! on undefined name '${name}'"), env)

                        [_, _] -> (ErrVal("First arg of set! must be a symbol"), env)
                        _ -> (ErrVal("Wrong number of args for set!"), env)

                AtomNode("lambda") ->
                    when rest is
                        [ListNode(params), .. as body] ->
                            # Extract parameter names from AtomNodes
                            param_names = List.map(
                                params,
                                |p|
                                    when p is
                                        AtomNode(name) -> name
                                        _ -> "invalid-param", # This should not happen in valid Lisp code
                            )
                            (LambdaVal(param_names, body, env.scope), env)

                        _ -> (ErrVal("Invalid lambda, expected param list"), env)

                AtomNode(s) ->
                    first_val = eval_atom(s, env)
                    apply(first_val, rest, env)

                ListNode(asts) ->
                    (first_val, env2) = eval_list(asts, env)
                    apply(first_val, rest, env2)

eval_forms : List Ast, Env -> (Val, Env)
eval_forms = |asts, env|
    when asts is
        [] ->
            (ListVal([]), env)

        [first] ->
            eval(first, env)

        [first, .. as rest] ->
            (_, env2) = eval(first, env)
            eval_forms(rest, env2)

read_eval_print : Str -> Str
read_eval_print = |str|
    env = default_env
    read_result = str |> tokenize |> read_from_tokens
    when read_result is
        Ok(asts) ->
            (val, _) = asts |> eval_forms(env)
            val_str(val)

        Err(err) -> Inspect.to_str(err)

# ReplState : { pending_input : Str, env : Env }

main! : List Arg.Arg => Result {} [Exit I32 Str]_
main! = |_args|
    # Simple REPL implementation
    initial_state = { pending_input: "", env: default_env }

    # Run a single iteration of the REPL
    repl_step! = |state|
        { pending_input, env } = state
        _ = Stdout.write!("> ")

        when Stdin.line!({}) is
            Ok(input) ->
                combined_input = "${pending_input}\n${input}"
                read_result =
                    combined_input
                    |> tokenize
                    |> read_from_tokens
                when read_result is
                    Ok(asts) ->
                        (val, env2) = eval_forms(asts, env)
                        _ = Stdout.line!(val_str(val))
                        Ok({ env: env2, pending_input: "" })

                    Err(MissingCloseParen) ->
                        Ok({ env, pending_input: combined_input })

                    Err(read_err) ->
                        _ = Stdout.line!(Inspect.to_str(read_err))
                        Ok({ env: env, pending_input: "" })

            Err(_) ->
                Err(Exit(0, "Goodbye!"))

    # Run the REPL until EOF or error
    run_repl_loop! = |state|
        when repl_step!(state) is
            Ok(new_state) -> run_repl_loop!(new_state)
            Err(exit) -> Err(exit)

    run_repl_loop!(initial_state)

# Test Env
expect
    env = empty_env
    (env_get(env, "a")) == Err("Name 'a' not bound in scope")
expect
    env = env_set(empty_env, "a", IVal(1))
    (env_get(env, "a")) == Ok(IVal(1))

# Test read_eval_print
expect
    result = read_eval_print("1")
    dbg result

    result == "1"

expect
    result = read_eval_print("()")
    dbg result

    result == "(  )"

expect
    result = read_eval_print("(quote a)")
    dbg result

    result == "a"

expect
    result = read_eval_print("(if 1 2 3)")
    dbg result

    result == "2"
expect
    result = read_eval_print("(define a 4) a")
    dbg result

    result == "4"
expect
    result = read_eval_print("(define a 4) (set! a 3) a")
    dbg result

    result == "3"

expect
    result = read_eval_print("(define a (lambda (b) b)) (a 1)")
    dbg result

    result == "1"

expect
    # Recursion
    result = read_eval_print(
        """
        (define rec (lambda (a) (if a nil (rec (not a)))))
        (rec t)
        """,
    )
    result == "(  )"

expect
    # Regression test for lambda scope issue
    result = read_eval_print(
        """
        (define fib (lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 5)
        """,
    )
    result == "5"

expect
    result = read_eval_print("((lambda (b) b) 1)")
    dbg result
    result == "1"

expect
    dbg read_eval_print("(+ 1 1)")
    "2" == read_eval_print("(+ 1 1)")
expect
    result = read_eval_print("(- 1 1)")
    result == "0"
expect
    result = read_eval_print("(- 1)")
    result == "-1"
expect
    result = read_eval_print("(* 2 3)")
    result == "6"
expect
    result = read_eval_print("(/ 5 2)")
    result == "2" # Truncating div
expect
    result = read_eval_print("(cons 1 nil)")
    result == "( 1 )"
expect
    result = read_eval_print("(car (cons 1 (cons 2 nil)))")
    result == "1"
