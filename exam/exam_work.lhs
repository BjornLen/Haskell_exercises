Exam 20/3-03
1: Rewrite so that x is not used in the definition.

> applo f x = (f x) + 1

So, apply f to the argument and add one. Written in another
way we have: f(x) = f, g(x) = x + 1. What we want is: f(x) + 1,
i.e. g(f(x)) = f(x) + 1. In haskell this exactly what the 
dot operator does: g(x).f(x) = g(f(x)).

> applo2 f = (1+).f

We could define a power function this way:

> powr n f  
>	| n == 0 = (1*)
>	| otherwise = (powr (n-1) f).f
> test_powr = (2*(2*(2*(3))) == (powr 3 (2*) 3))


2:

Why can't we write a replica of the prelude function replicate
in the following manner?:

> repl2  n x = take n [x,x..]

Which works fine for numbers and chars

> test_repl = (repl2 2 'a') == (replicate 2 'a') 

But not so fine for general elements

	> test_repl2 = (repl2 2 "a") == (replicate 2 "a") 

Won't even compile:
 No instance for (Enum [Char])
      arising from a use of `repl2'
    Possible fix: add an instance declaration for (Enum [Char])
    In the first argument of `(==)', namely `(repl2 2 "a")'
    In the expression: (repl2 2 "a") == (replicate 2 "a")
    In an equation for `test_repl2':
        test_repl2 = (repl2 2 "a") == (replicate 2 "a")

Since here the operation ["a","a"..] is invalid since
theres no natural way to continure, what should come
after? Perhaps its more clear if you had ["Bjorn", "Misha"..]
What are the values that follow? In haskell only types that 
can be enumerated can be used to fill list in the manner above.

So how would we do it?

> repl3 n x = take n (cycle [x])


3:

What is the type of the following functiong and what does it do?

> h f = fst . head . dropWhile( uncurry (/=)) . ps (iterate f)
>			where 	ps g x = zip (tail (g x)) (g x)

From iterate f x we get:
[x , f x, f ( f x) , ...]
From tail iterate f x we get
[f x, f (f x) , ...]

And these are zipped together, thus we get from ps:
[(x, f x), (f x, f ( f x)), ..]

Then dropWhile uncurry filters this list to only allow tuples where the 
elements are equal. fst and head then extract the first element of the first
such pair. Thus we should get 100 if we apply it to the function below: 

> f x  
>	| x < 10 = x + 1 
>	| otherwise = 100

> test_understood = (h f 1) == 100


:4

> f2 key x
>	| x == Just(a) = Just (a)
>	| x == key = Just(x)
>	| otherwise = Nothing






Tenta 18/12-01

1:
En funktion f har typen:
f::(Ord a, Num b) => a -> [b] -> [(a,[b])]
Beskriv denna typ i ord:

Så första viktiga egenskapen är (Ord a, Num b) =>
vilket ställer krav på vilka typer a och b får bestå av.
Syntaxen betyder att a måste vara av en ordnad typ i haskell,















