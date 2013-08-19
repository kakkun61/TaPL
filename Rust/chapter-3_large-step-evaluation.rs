// Rust 0.7

enum Term {
    True, False, Zero,
    Succ(@Term), Pred(@Term), IsZero(@Term),
    If(@Term, @Term, @Term)
}

enum Value {
    VTrue, VFalse, NV(@NumberValue)
}

enum NumberValue {
    VZero, VSucc(@NumberValue)
}

fn eval(t: &Term) -> @Value {
    match *t {
        True  => @VTrue,
        False => @VFalse,
        Zero  => @NV(@VZero),
        If(c, t, f) =>
            match eval(c) {
                @VTrue  => eval(t),
                @VFalse => eval(f),
                _       => fail!("\"if\"'s conditon is evaluated in non boolean.")
            },
        Succ(t) =>
            match eval(t) {
                @NV(nv) => @NV(@VSucc(nv)),
                _       => fail!("\"succ n\"'s \"n\" is evaluated in non number.")
            },
        Pred(t) =>
            match eval(t) {
                @NV(@VZero)     => @NV(@VZero),
                @NV(@VSucc(nv)) => @NV(nv),
                _               => fail!("\"pred n\"'s \"n\" is evaluated in non number.")
            },
        IsZero(t) =>
            match eval(t) {
                @NV(@VZero)    => @VTrue,
                @NV(@VSucc(_)) => @VFalse,
                _              => fail!("\"iszero n\"'s \"n\" is evaluated in non number.")
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
