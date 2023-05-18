# Saber Programming Language

 - current situation: lambda calculus compiling to C (this doesn't quite work yet), with an Algorithm-J Hindley-Milner type inference pass.
 - What's needed to get the C to work is to generate C instead of pseudo C. This amounts to merely fixing the way types are specified, because C has the weirdest notation due to the "if you use the identifier this way you get a value of this type" logic. So function types have the weird `retT (*ident)(argT)` notation, for example. Some refactoring might be needed because printing a type requires the identifier, but that should be easy to do.
 - goal: described tersely at https://saber-lang.org
 - The executable is `dist-newstyle/build/x86_64-linux/ghc-8.8.4/saber-0.1.0.0/x/saber/build/saber/saber` in typical Cabal fashion.
 - Call it with no arguments to type code in the terminal (this isn't done the most clever way). Otherwise, add a filename like `saber main.sb`. I use the `main.sb` at the top level of the repository. 
