// Rust 0.7

enum Term {
    True, False, Zero,
    Succ(@Term), Pred(@Term), IsZero(@Term),
    If(@Term, @Term, @Term)
}

fn value(t: &Term) -> bool {
    fn number(t: &Term) -> bool {
        match *t {
            Zero     => true,
            Succ(t2) => number(t2),
            _        => false
        }
    }

    match *t {
        True  => true,
        False => true,
        _     => number(t)
    }
}

fn evaluate(t: &Term) -> @Term {
    fn eval(t: &Term) -> @Term {
        match *t {
            If(@True,  t, _) => @*t,
            If(@False, _, f) => @*f,
            If(c,      t, f) => @If(evaluate(c), t, f),
            Succ(t)          => @Succ(eval(t)),
            Pred(@Zero)      => @Zero,
            Pred(@Succ(t))   => @*t,
            Pred(t)          => @Pred(evaluate(t)),
            IsZero(@Zero)    => @True,
            IsZero(@Succ(_)) => @False,
            IsZero(t)        => @IsZero(evaluate(t)),
            _                => fail!("\"iszero n\"'s \"n\" is evaluated in non number.")
        }
    }

    if value(t) {
        @*t
    } else {
        evaluate(eval(t))
    }
}

fn pretty(v: &Term) -> ~str {
    match *v {
        True  => ~"True",
        False => ~"False",
        t     => fmt!("%d", num(&t)).clone()
    }
}

fn num(t: &Term) -> int {
    match *t {
        Succ(t1) => num(t1) + 1,
        Zero     => 0,
        _                => fail!("not a number")
    }
}

fn main() {
    println(fmt!("%s", pretty(evaluate(@True))));
    println(fmt!("%s", pretty(evaluate(@Succ(@Zero)))));
    println(fmt!("%s", pretty(evaluate(
            @If(
                @IsZero(@Pred(@Succ(@Zero))),
                @Succ(@Succ(@Succ(@Zero))),
                @Pred(@Zero)
            )
    ))));
    println(fmt!("%s", pretty(evaluate(
            @IsZero(@Pred(@If(@True, @Succ(@Zero), @Zero)))
    ))));
}
