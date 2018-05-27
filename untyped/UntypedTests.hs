{- Because untyped is supported by more than one implementation (e.g.,
   both untyped and fulluntyped, we extract eval tests into a common module
 -}

module UntypedTests where


-- Each element in list contains: (label, expected, input)
untypedEvalTests 
    = [("binder", "x ", "x/;"),
       ("binder + usage", "x \nx", "x/;x;"),
       ("multi-binder", "x \nx\ny \ny\nx", "x/;x;y/;y;x;"),
       ("app", "x \nx x", "x/;x x;"),
       ("lambda", "(lambda x. x)", "lambda x. x;"),
       ("lambda + app", "(lambda x. x x)", "lambda x. x x;"),
       ("binder + lambda", "x \n(lambda x'. x')", "x/; lambda x. x;"),
       ("apply lambda", "(lambda y. y)", "(lambda x. x) (lambda y. y);"),
       ("binder + apply lambda", "z \n(lambda y. y)", "z/;(lambda x. x) (lambda y. y);"),
       ("abs + app", "(lambda x. x x)", "(lambda x. x) (lambda x. x x);"),
                                                                             -- test the full sample file from the original untyped impl
       ("full test.f", "x \nx\n(lambda x'. x')\n(lambda x'. x' x')", 
        "/* Examples for testing */\n\nx/;\nx;\n\nlambda x. x;\n(lambda x. x) (lambda x. x x);\n"),
       ("nested lambdas", "(lambda b. b)", "(lambda x. (lambda y. x y x)) (lambda z. (lambda a. z)) (lambda b. b);")
      ]


