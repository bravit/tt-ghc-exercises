{- test cases for fullsimple, which is used as the basis for other 
   implementations (e.g., fullref)

   Relies on the path being setup to access to corresponding Syntax
   module for the implementation.
 -}

module FullSimpleTests (parseTests, evalTests) where

import Syntax

-- FORMAT: (test name, expected parse tree, input)
parseTests = [("comments", TmTrue, "/**** comment *****/true /* another*//**/;")
             ,("true",  TmTrue,  "true;")
             ,("false", TmFalse, "false;")
             ,("0",     TmZero,  "0;")
             ,("VarBind to type", TmBind "x" (VarBind TyBool), "x : Bool;")
             ,("lambda", TmAbs "x" TyBool (TmVar 0 1), "(lambda x:Bool. x);")
             ,("ascription", TmAscribe TmTrue TyBool, "true as Bool;")
             ,("nested lambda 1", 
               TmAbs "x" TyBool (TmAbs "y" TyNat (TmVar 0 2)),
               "(lambda x:Bool. (lambda y:Nat. y));")
             ,("nested lambda 2", 
               TmAbs "x" TyBool (TmAbs "y" TyNat (TmVar 1 2)),
               "(lambda x:Bool. (lambda y:Nat. x));")
             ,("application", TmApp (TmAbs "x" TyBool (TmVar 0 1)) TmTrue, 
               "(lambda x:Bool. x) true;")
             ,("app left-assoc", TmApp (TmApp TmTrue TmZero) TmFalse,
               "true 0 false;")
             ,("if", TmIf TmTrue TmZero TmFalse, 
               "if true then 0 else false;")
             ,("if + parens", TmIf TmTrue TmZero TmFalse, 
               "(if true then 0 else false);")
             ,("succ", TmSucc TmZero, "succ 0;")
             ,("pred", TmPred TmZero, "pred 0;")
             ,("iszero", TmIsZero TmZero, "iszero 0;")
             ,("unit", TmUnit, "unit;")
             ,("string", TmString "foo", "\"foo\";")
             ,("float", TmFloat 1.2, "1.2;")
             ,("timesfloat", TmTimesFloat (TmFloat 1.1) (TmFloat 1.2), 
               "timesfloat 1.1 1.2;")
             ,("wildcard", TmAbs "_" TyBool TmTrue, "lambda _:Bool. true;")
             ,("TyId", TmAbs "x" (TyId "A") TmTrue, "lambda x:A. true;")
             ,("TyAbbBind", TmBind "A" (TyAbbBind TyBool), "A = Bool;")
             ,("TyVarBind", TmBind "A" TyVarBind, "A;")
             ,("TmAbbBind", TmBind "x" (TmAbbBind TmZero (Just TyNat)), "x=0;")
             ,("let x", TmLet "x" TmTrue (TmVar 0 1), "let x = true in x;")
             ,("let _", TmLet "_" TmTrue TmFalse, "let _ = true in false;")
             ,("record of 0", TmRecord [], "{};")
             ,("record of 1, named", TmRecord [("x", TmFloat 1.1)], "{x=1.1};")
             ,("record of 1, unnamed", TmRecord [("1", TmFloat 1.1)], "{1.1};")
             ,("record of 2, no names", 
               TmRecord [("1", TmFloat 1.1), ("2", TmFloat 1.2)], 
               "{1.1, 1.2};")
             ,("record of 2, named", 
               TmRecord [("x", TmFloat 1.1), ("y", TmFloat 1.2)], 
               "{ x  = 1.1   , y=1.2};")
             ,("proj", TmProj (TmRecord []) "x", "{}.x;")
             ,("variant 1", TmTag "x" TmTrue (TyVariant [("x",TyBool)]),
               "<x=true> as <x:Bool>;")
             ,("variant 2", TmTag "x" TmTrue (TyVariant [("x",TyBool), ("y", TyNat)]),
               "<x=true> as <x:Bool,y:Nat>;")
             ,("case 1", TmCase (TmTag "x" TmTrue (TyVariant [("x",TyBool)]))
                         [("x", ("val", (TmVar 0 1)))],
               "case (<x=true> as <x:Bool>) of <x=val> ==> val;")
             ,("inert", TmInert TyBool, "inert[Bool];")
             ,("fix", TmFix (TmAbs "x" TyBool TmTrue), 
               "fix (lambda x:Bool. true);")
             ,("letrec", TmLet "x" (TmFix (TmAbs "x" TyBool TmTrue)) (TmVar 0 1),
               "letrec x:Bool = true in x;")
             ]

-- FORMAT: (test name, expected printed output, input)
evalTests = [("true",  "true : Bool",  "true;")
            ,("false", "false : Bool", "false;")
            ,("0",     "0 : Nat",      "0;")
            ,("varbind to type", "x : Bool\nx : Bool", "x : Bool; x;")
            ,("ascription", "true : Bool", "true as Bool;")
            ,("multiple varbinds to type",
              "x : Bool\nx : Bool\ny : Nat\nx : Bool\ny : Nat",
              "x : Bool; x;y : Nat; x; y;")
            ,("lambda", 
              "(lambda x:Bool. x) : Bool -> Bool",
              "(lambda x:Bool. x);")
            ,("lambda + freshname", 
              "x : Bool\n(lambda x':Bool. x') : Bool -> Bool",
              "x : Bool; (lambda x:Bool. x);")
            ,("application", 
              "true : Bool",
              "(lambda x:Bool. x) true;")
            ,("nested lambda 1", 
              "(lambda x:Bool. (lambda y:Nat. y)) : Bool -> Nat -> Nat",
              "(lambda x:Bool. (lambda y:Nat. y));")
            ,("apply nested lambda 1", 
              "0 : Nat",
              "(lambda x:Bool. (lambda y:Nat. y)) true 0;")
            ,("nested lambda 2", 
              "(lambda x:Bool. (lambda y:Nat. x)) : Bool -> Nat -> Bool",
              "(lambda x:Bool. (lambda y:Nat. x));")
            ,("nested lambda 3", 
              "(lambda f:Nat -> Nat. (lambda x:Nat. f (f x))) : (Nat -> Nat) -> Nat -> Nat",
              "lambda f:Nat->Nat. lambda x:Nat. f (f x);")
            ,("apply nested lambda 2", 
              "true : Bool",
              "(lambda x:Bool. (lambda y:Nat. x)) true 0;")
            ,("unit", "unit : Unit", "unit;")
            ,("string", "\"foo\" : String", "\"foo\";")
            ,("float", "1.2 : Float", "1.2;")
            ,("timesfloat", "1.32 : Float", 
               "timesfloat 1.1 1.2;")
            ,("times float after eval", "1.32 : Float", 
              "timesfloat 1.1 ((lambda x:Float. 1.2) 3.0);")
            ,("wildcard", "(lambda _:Bool. true) : Bool -> Bool", 
              "lambda _:Bool. true;")
            ,("apply wildcard", "true : Bool", "(lambda _:Bool. true) false;")
            ,("TyVarBind", "A", "A;")
            ,("TyId", "(lambda x:A. x) : A -> A", "lambda x:A. x;")
            ,("TyAbbBind", "Bfun :: *", "Bfun = Bool -> Bool;")
            ,("use of TyAbbBind", 
              "T :: *\n(lambda f:T. (lambda x:Nat. f (f x))) : T -> Nat -> Nat",
             "T = Nat->Nat; lambda f:T. lambda x:Nat. f (f x);")
            ,("TmAbbBind", "x : Nat\n0 : Nat", "x = 0; x;")
            ,("let x", "true : Bool", "let x = true in x;")
            ,("let _", "false : Bool", "let _ = true in false;")
            ,("nested lets", "1.32 : Float", 
              "let x = 1.2 in let y = 1.1 in timesfloat y x;")
            ,("eval let term", "1.32 : Float",
              "let x = (timesfloat 1.1 1.2) in x;")
            ,("shift let", "1.3 : Float",
              "(lambda x:Float. let y = x in timesfloat y 1.3) 1.0;")
            ,("record", "{x=(lambda x:Nat. x), y=(lambda z:Bool. z)} : {x:Nat -> Nat, y:Bool -> Bool}",
              "{x=lambda x:Nat.x, y=lambda z:Bool.z};")
            ,("proj 1", "(lambda x:Nat. x) : Nat -> Nat",
              "{x=lambda x:Nat.x, y=lambda z:Bool.z}.x;")
            ,("proj 2", "(lambda x:Nat. x) : Nat -> Nat",
              "{lambda x:Nat.x, lambda z:Bool.z}.1;")
            ,("proj 3", "(lambda z:Bool. z) : Bool -> Bool",
              "{lambda x:Nat.x, lambda z:Bool.z}.2;")
            ,("proj 4", "(lambda z:Nat. z) : Nat -> Nat",
              "{x=(lambda x:Nat->Nat.x)(lambda z:Nat. z),y=lambda z:Bool.z}.x;")
            ,("proj 5", "{x=true, y=false} : {x:Bool, y:Bool}", "{x=true, y=false};")
            ,("proj 6", "true : Bool", "{x=true, y=false}.x;")
            ,("proj 7", "{true, false} : {Bool, Bool}", "{true, false}; ")
            ,("proj 8", "true : Bool", "{true, false}.1;")
            ,("variant 1", "<x=true> as <x:Bool> : <x:Bool>",
              "<x=true> as <x:Bool>;")
            ,("variant 2", "<x=true> as <x:Bool, y:Nat> : <x:Bool, y:Nat>",
              "<x=true> as <x:Bool,y:Nat>;")
            ,("variant 3", "(lambda x:<a:Bool, b:Bool>. x) : <a:Bool, b:Bool> -> <a:Bool, b:Bool>",
              "lambda x:<a:Bool,b:Bool>. x;")
            ,("case 1", "true : Bool",
              "case (<x=true> as <x:Bool>) of <x=val> ==> val;")
            ,("case 2", "true : Bool",
              "case (<x=true> as <y:Nat,x:Bool>) of <x=val> ==> val;")
            ,("case 3", "false : Bool",
              "case ((lambda x:Bool. <y=x> as <z:Nat, y:Bool>) false) of <y=val> ==> val;")
            ,("case 4", "true : Bool",
              "case (<x=true> as <y:Bool, x:Bool>) of <x=val> ==> val | <y=val> ==> val;")
            ,("case 5", "1.32 : Float",
              "case (<x=1.1> as <x:Float>) of <x=val> ==> ((lambda z:Float. timesfloat z val) 1.2);")
            ,("unevaled case", 
              "(case <x=true> as <x:Bool, y:Nat, z:Nat> of <y=val> ==> val | <z=w> ==> w) : Nat",
              "case (<x=true> as <x:Bool, y:Nat, z:Nat>) of <y=val> ==> val | <z=w> ==> w;")
            ,("inert", "inert[Bool] : Bool", "inert[Bool];")
            ,("simple letrec", "true : Bool",
              "letrec x:Bool = true in x;")
            ,("iseven letrec 1", "true : Bool",
              testLetrec ++ "in iseven (succ (succ (succ (succ 0))));")
            ,("iseven letrec 2", "false : Bool",
              testLetrec ++ "in iseven (succ (succ (succ (succ (succ 0)))));")
            ,("simple fix", "true : Bool", 
              "fix (lambda x:Bool. true);")
            ,("iseven ff 1", "true : Bool",
              "(fix " ++ testIsevenFF ++ ") (succ (succ (succ (succ 0))));")
            ,("iseven ff 2", "false : Bool",
              "(fix " ++ testIsevenFF ++ ") (succ (succ (succ (succ (succ 0)))));")
            ]

testLetrec = "letrec iseven : Nat -> Bool = lambda x:Nat. " ++
             "if iszero x then true " ++ 
             "else if iszero (pred x) then false " ++ 
             "else iseven (pred (pred x))"
testIsevenFF = "(lambda ie:Nat -> Bool. lambda x:Nat. " ++ 
               "if iszero x then true  " ++ 
               "else if iszero (pred x) then false " ++ 
               "else ie (pred (pred x)))"

