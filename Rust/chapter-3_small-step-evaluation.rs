// Rust 0.7

enum Term {
    True, False, Zero,
    Succ(@Term), Pred(@Term), IsZero(@Term),
    If(@Term, @Term, @Term)
}

fn value(t: &Term) -> bool {
    match *t {
        True  -> true,
        False -> true,
        _     -> number(t)
    }

    fn number(t: &Term) -> bool {
        match *t {
            Zero     -> true,
            Succ(t2) -> number(t2),
            _        -> false
        }
    }
}

fn evaluate(t: &Term) -> @Term {
    if value(t) {
        t
    } else {
        evaluate(eval(t))
    }

    fn eval(t: &Term) -> @Term {
        match *t {
            If(@True,  t, _) => t,
            If(@False, _, f) => f,
            If(c,      t, f) => If(eval(c), t, f)
            Succ(t) => Succ(eval(t)),
            Pred(@VZero)     => @Zero,
            Pred(@Succ(t)) => t,
            Pred(t)        => Pred(eval(t),
            IsZero(t) =>
                match eval(t) {
                    @NV(@VZero)    => @VTrue,
                    @NV(@VSucc(_)) => @VFalse,
                    _              => fail!("\"iszero n\"'s \"n\" is evaluated in non number.")
                }
        }
    }
}

fn pretty(v: &Value) -> ~str {
    match *v {
        VTrue  => ~"True",
        VFalse => ~"False",
        NV(nv) => fmt!("%d", num(nv)).clone()
    }
}

fn num(nv: &NumberValue) -> int {
    match *nv {
        VSucc(t) => num(t) + 1,
        VZero    => 0
    }
}

fn main() {
    println(fmt!("%s", pretty(eval(@True))));
    println(fmt!("%s", pretty(eval(@Succ(@Zero)))));
    println(fmt!("%s", pretty(eval(
            @If(
                @IsZero(@Pred(@Succ(@Zero))),
                @Succ(@Succ(@Succ(@Zero))),
                @Pred(@Zero)
            )
    ))));
    println(fmt!("%s", pretty(eval(
            @IsZero(@Pred(@If(@True, @Succ(@Zero), @Zero)))
    ))));
}
