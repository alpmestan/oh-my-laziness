So, you know quite some bits of Haskell now and are willing to dig a bit deeper, understand when something in your code is going to get evaluated, and when it won't? You have just landed in the right place. This section will attempt to give some intuition as to how laziness is implemented in the GHC runtime, how to know when some argument of a function is evaluated, how to have a finer grained control of this, how to make some parts stricter, and how you can handle these matters directly in your data types rather than in your functions, and much more!

# Foreword

This article gives a modest shot at explaining how to gain a better control of evaluation in your Haskell code. Whether your goal is to write some fast number crunching code with plenty of tight loops or if you rather want to process gigabytes of streaming data, this article should provide you with the basic building blocks for succeeding. However, just using what you will learn in the section dedicated to strictness isn't enough. The strictness machinery in GHC isn't a magic bullet to make any code fast, obviously. You really eventually have to tailor your usage of strictness and laziness in your data types and functions to your specific use cases. Laziness can help a lot performance-wise too! If you're reading gigabytes of data, you most likely don't want to have it all in memory at the same time, but rather have some chunks that you'd read and process one by one. This requires  strictness and laziness to play well together. If your code uses too much strictness for your use case, it can end up eating huge amounts of memory behind your back where it could have used smaller chunks of memory over time. Both strictness and laziness should be used wisely!

# Hello, laziness!

You probably have heard that Haskell is a _call-by-need_ programming language, as opposed to _call-by-value_ like, say, C++, Python and
most of the mainstream programming languages. That just means that for example in C++, the following function:

~~~ {.cpp}
int f(int x, int y) { if x > 0 then x-1 else x+1; }
~~~

when called, will evaluate both its arguments, even if for example the `y` comes from a long computation that can throw exceptions or stay stuck in an infinite loop, although we can see it doesn't actually use `y`. The equivalent function in Haskell would be the following:

``` haskell
f :: Int -> Int -> Int
f x y = case x > 0 of
  True  -> x - 1
  False -> x + 1
```

It will never care about the value of `y` so it won't bother to actually evaluate it. How can we verify this? Let's just pass the product of all the
positive numbers starting from 1 as the second argument; this doesn't terminate, meaning that if `f` does give us back a result, it doesn't evaluate `y`. 

``` active haskell
f :: Int -> Int -> Int
f x y = case x > 0 of
  True  -> x - 1
  False -> x + 1

-- show
main = print $ f 1 (product [1..])
-- /show
```

So `x` *does* get evaluated as expected since the result depends on its value, but the second argument never gets inspected! We say that `f` is _strict_ in its first argument (since it forces its evaluation -- it doesn't care whether it will be the result of a very big computation or if it's just a constant) and _lazy_ in its second argument. The equivalent call to the C++ `f` function would just loop forever and is thus strict in both its arguments.

Another example, which happens to be simpler, is the standard (as in, is in the `Prelude`) `const` function:

``` haskell
const :: a -> b -> a
const x y = x
```

`const` doesn't inspect any of its argument _per se_, so it may seem lazy in both its arguments. But when a call to const is evaluated (that's the situation we are interested in, here, after all), its return value is evaluated too; that return value happens to be `x`. So it's strict in `x`. However, you can pass it whatever you want as the `y`, it won't ever be evaluated. This is a good general principle: a function obviously is strict in its return value, because when a function application needs to be evaluated, it needs to evaluate, in the body of the function, what gets returned. Starting from there, you can know what **must** be evaluated by looking at what the return value depends on invariably. Your function will be strict in these arguments, and lazy in the others.

You should however keep in mind that most languages aren't entirely strict/lazy/non-strict. The `||` operator in C++ for example isn't strict in its second argument. That is, in

~~~ {.cpp}
bool big_computation();

if (true || big_computation())
{
	...
}
~~~

the compiler will see that the first argument is true and the `big_computation` will hence usually be short-circuited. For an overview of different _evaluation strategies_, you may want head to [this wikipedia page](http://en.wikipedia.org/wiki/Evaluation_strategy "Evaluation Strategy").

In the same vein, we will soon see how we can in Haskell explicitly require some arguments or fields of data structures to be strict, thus forcing their evaluation at a moment when it may not yet be necessary. But before that, we shall go further in examining how laziness works and how it can affect your code.

## Diving deeper

### Towards NF and WHNF

You all know the `length` function, right? One possible definition is the following (tail-recursive).

``` active haskell
-- show
length1 :: [a] -> Int
length1 l = go l 0
  where go []     acc = 0
        go (x:xs) acc = go xs (acc+1)
-- /show
main = putStrLn ("length1 [1, 2, 3] = " ++ show (length1 [1,2,3]))
```

You probably have used it too, but in case you haven't, run the above snippet.

So, it seems to "deconstruct" the list by pattern matching on it, kind of forcing the evaluation of the list it is passed, not caring about how that list is produced, somehow being _strict_ in its argument. So, passing it an infinite list will not terminate, right? Indeed it won't.

Now let's imagine we want to compute the length of a (finite, this time) list of numbers. If one of these numbers is our friend `product [1..]`, it won't terminate either right? Let's run the following snippet to figure out.

``` active haskell
length1 :: [a] -> Int
length1 []     = 0
length1 (x:xs) = 1 + length1 xs
-- show
main = let x = product [1..] in print (length1 [1, x])
-- /show
```

Uh?! What happened? Wasn't length supposed to evaluate its argument? Well, it did, *up to some point*. Remember, GHC will only evaluate what's really necessary here. To make sense of this, let's first remember how the `[a]` type is defined (almost, since it gets a special treatment syntax-wise).

``` haskell
data [] a = [] | (:) a [a]
-- or alternatively, a definition you can actually give to GHC:
data List a = Nil | Cons a (List a)
```

So what we are doing in `length1` is somehow evaluating the list, isn't it? Well, not exactly. Let's examine how `length1 [1, x]` gets computed.

``` haskell
  length1 [1, x]
= length1 1:(x:[])      -- just rewriting for clarity
= 1 + length1 (x:[])    -- 1:(x:[]) satisfies the (x:xs) pattern, so that clause gets chosen
= 1 + 1 + length1 []    -- the very same goes for (x:[])
= 1 + 1 + 0             -- [], however, satisfies the first pattern, that of the empty list
= 2
```

This is all nice, but how is this helping us understand why length didn't care we gave it a list whose second element was a computation that doesn't terminate? Well, take another look at the evaluation of `length [1, x]`. Where/when did we actually need to know what values are stored in the list? We didn't at all. So we're evaluating, at each recursive step, if the list is the empty list or `x:xs` for some `x` and `xs`, but we're not evaluating anything further than this. 

Now, imagine we want a weird `length` function, that will only account for the positive numbers stored in the list. That is, we want `weirdLength [-1, 1, 2] == 2` for example, and `weirdLength` would behave just like `length` for lists containing only positive numbers. Let's write this function.

``` active haskell
weirdLength :: [Int] -> Int
weirdLength []                 = 0                  -- nothing surprising here, 
                                                    -- just like before
weirdLength (x:xs) | x < 0     = weirdLength xs     -- negative number, we do not 
                                                    -- account for it
                   | otherwise = 1 + weirdLength xs -- positive number, we account for it

main = let x = product [1..] in print (weirdLength [1, x])
```

This time, we're actually taking a look at what the values stored in the list look like, so this time `weirdLength [1,x]` won't terminate because the evaluation happens as follows:

``` haskell
  weirdLength [1, x]
= weirdLength 1:(x:[])   -- just rewriting for clarity
= 1 + weirdLength (x:[]) -- 1:(x:[]) satisfies the (x:xs) pattern, and 1 is positive, 
                         -- so we enter the last clause
= 1 + ???                
-- x:[] matches the second pattern (x:xs), but is x negative? 
-- We don't know unless we compute it, which the
-- runtime does... but it will never terminate.
```

As opposed to the usual `length`, we are evaluating everything there is in this list. Of course, the same goes for any of your algebraic data types. Consider the definition of `Maybe` and an accompanying function `isNothing`.

``` active haskell
-- Maybe is defined as:
-- data Maybe a = Nothing | Just a

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just x) = False

main = let x = product [1..] in do 
  print $ isNothing Nothing
  print $ isNothing (Just 4)
  print $ isNothing (Just x)
```

Of course, `isNothing Nothing` returns `True`, `isNothing (Just 4)` returns `False`. But what does `let x = product [1..] in isNothing (Just x)` return?

It returns False, because it doesn't need to inspect what's actually sitting next to the `Just` constructor. Admittedly, this discussion has been quite informal so far; there are more formal ways to talk about the different "dephts" to which we can evaluate expressions.

1. _Normal form_ (NF): 
     An expression in normal form has been fully evaluated, there's nothing that has been left unevaluated. Examples of expressions in NF are 
	 * `1`
	 * `"haskell"`
	 * `(2, True, Nothing)` 
	 * `\x -> x + 4`.
	 
2. _Weak head normal form_ (WHNF):
     An expression in weak head normal form is either a lambda abstraction or an expression that has its outermost constructor determined/evaluated but it may otherwise contain unevaluated sub expressions. Examples of expressions in WHNF are 
	 * `(1+1, 2)` -- which can be rewritten as `(,) (1+1) 2`, so the outermost constructor is (,)
	 * `Just (sum [1..10])` -- the outermost constructor here is `Just`
	 * `1 : computePrimesUpTo 100` -- the outermost constructor here is `(:)`
     * `\x -> 2+2`          -- lambda abstraction, whnf doesn't require to reduce the body

*Note*: an expression in *normal form* also is in *weak head normal form*, considering that if it has been fully evaluated, if it's a lambda abstraction then we're good by definition, and if it's another kind of expression, the outermost constructor is determined among other htings.
	 
So `length` and `isNothing` evaluate their arguments to _WHNF_ whereas `weirdLength` evaluates the list of ints we hand it to _NF_.

_Note:_ You may have heard about _Head Normal Form_ too, which exists but isn't implemented in GHC Haskell. You can read more about it [here](http://foldoc.org/head+normal+form).

### Exercises

Determine how each of these obviously useful functions will evaluate its arguments (that can be "doesn't evaluate at all").

``` haskell
concatenate :: [a] -> [a] -> [a]
concatenate [] l = l
concatenate (x:xs) l = x : concatenate xs l

mix :: [a] -> [a] -> [a]
mix [] l2 = l2
mix l1 [] = l1
mix (x1:x1s) (x2:x2s) = x1 : x2 : mix x1s x2s

silly :: [Int] -> [a] -> [a]
silly [] _ = []
silly _ [] = []
silly (x:xs) (y:ys) | x == 0    = silly xs ys
                    | otherwise = y : silly xs ys
```

Click the button below to see the answers.

@@@
- `concatenate` evaluates its first argument to WHNF (it never cares about what is `x` in the second clause, or any property about it, it just passes it along) and doesn't evaluate anything from its second argument `l`.
- `mix` will evaluate both lists to WHNF for the same reason `concatenate`'s first argument will get evaluated to WHNF.
- `silly` will however evaluate its first argument, the list of ints, to NF because it needs to know whether or not the `x` it has at hand is zero. The second list is only evaluated to WHNF, as in the previous examples.
@@@

## Laziness in the GHC runtime system

The GHC runtime implements laziness mostly through what it calls _thunks_. You can picture a _thunk_ as a box containing some expression that hasn't been evaluated yet. Whenever the actual value of that expression is needed, the runtime will _open_ the thunk and _evaluate_ what's inside it. That thunk may itself refer to other thunks that haven't been evaluated yet either, and will thus make the runtime open and evaluate them too. Once the expression is evaluated, the runtime system overwrites the thunk, replacing the "pointer" to the expression to evaluate by the actual result. For a deeper and longer explanation about laziness and the (GHC) Haskell heap, see Edward Z. Yang's _great_ blog post series which starts [here](http://blog.ezyang.com/2011/04/the-haskell-heap/). You can read them after this article, since I try to give an intuition regarding how it works without building on top of it, but you should read them at some point if you care about how GHC executes your code (say, if you care about performance, for example).

The thing you have to keep in mind though, is that basically anything creates a thunk in Haskell, by default. Consider the following code:

``` haskell
let x = 4 + 3 in [x, x+1, x+2]
```

Until you require the evaluation of the whole list, a thunk is created for the list (actually, there'll be one for each `(:)` constructor), another for `x` (and one for the `+` there, unless it's computed by GHC at compile-time), and two others for `x+1` and `x+2`. But when the evaluation of the whole list is required, including the actual value of each element, all these guys will get evaluated and the list will thus be evaluated to _normal form_.

Laziness happens at each "level", meaning you can evaluate what constructor was used (`Just` or `Nothing`, for example), then force the evaluation of what constructor was used for the data stored inside the `Just`, and so on. Getting top performance from your Haskell code generally means tailoring the evaluation of your data to the specific way it's exploited. If you read _a lot_ of data from a file and process, say, each line separately, not reading the whole file at once in memory is a good idea. A good implementation would read the data and produce a list of lines "on-demand". Then the function processing each line would take that list as an argument and consume it as it is being produced, doing the processing with O(1) space usage.

# Strictness to the rescue

Laziness is nice, and comes in very handy in some situations. But in others, it doesn't, it causes the creation of an awful lot of thunks, stacking up until you run out of memory. Instead, we would like to evaluate them as we go although the value they represent isn't actually needed immediately. One very simple example of this is the classical `sum` function.

``` haskell
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs
```

Evaluating `sum [1..5]`:

``` haskell
  sum [1..5]
= sum (1:2:3:4:5:[])         -- rewriting for clarity
= 1 + sum (2:3:4:5:[])       -- matched the second pattern
= 1 + 2 + sum (3:4:5[])      -- same
= ...
= 1 + 2 + 3 + 4 + 5 + sum [] -- matched the second pattern for 5:[]
= 1 + 2 + 3 + 4 + 5 + 0      -- matched the first pattern for []
= 15                         -- all the addition thunks are evaluated now
                             -- could be annoying with millions of elements
```

Now, notice how additions are piling up without actually being computed. That's because `sum` is building up a few thunks as it traverses the list. Obviously, we want to compute the result as we traverse the list. Maybe we could use an auxiliary function with an accumulator, right?

``` haskell
sum :: Num a => [a] -> a
sum xs = go xs 0
  where go []     acc = acc
        go (x:xs) acc = go xs (acc + x)
        -- `acc` represents "the sum so far"
```

Let's see how `sum [1..5]` behaves now.

``` haskell
  sum [1..5]
= go [1..5] 0
= go [2..5] (0+1)         -- go's second clause, x=1
= go [3..5] (0+1+2)       -- same, x=2
= go [4,5]  (0+1+2+3)     -- same, x=3
= go [5]    (0+1+2+3+4)   -- same, x=4
= go []     (0+1+2+3+4+5) -- same, x=5
= 0+1+2+3+4+5             -- go's first clause
= 15                      -- additions all evaluated at the end
```

Additions piling up again! How can we avoid this? 

## Bang Patterns

One very easy way is to use a strictness annotation for function arguments, through _!_ (pronounced _bang_). It requires a GHC extension (not a dangerous one, don't worry), called BangPatterns. You may activate it by giving GHC/GHCi the `-XBangPatterns` argument, or by writing `{-# LANGUAGE BangPatterns #-}` at the very top of your file (preferred method). Here is our `sum` function, fixed.

``` haskell
{-# LANGUAGE BangPatterns #-}

sum :: Num a => [a] -> a
sum xs = go xs 0
  where go []     !acc = acc
        go (x:xs) !acc = go xs (acc + x)
```

Notice the two `!`'s right before `acc`. What's happening is that these bangs ask that whenever one of these patterns is matched, the runtime should force the evaluation of the second argument, `acc`, before computing the RHS. However, **it doesn't evaluate to _normal form_ but "only" to _weak head normal form_, whatever the type of the argument is**. It is however enough for accumulators like numbers. But if we had written `sum !xs` instead of `sum xs`, it would only force the evaluation of the first cons-constructor (`(:)`) of the list, seeing `unevaluated-head : unevaluated-tail-of-the-list`. The same goes for values of type `Maybe Int` where it would evaluate whether the value is `Nothing` or `Just unevaluated-content`. And now, `sum`'s behavior:

``` haskell
  sum [1..5]
= go [1..5] 0  -- sum's definition
= go [2..5] 1  -- go's second clause, addition not piling up because 'acc' is evaluated
= go [3..5] 3  -- same
= go [4,5]  6  -- same
= go [5]    10 -- same
= go []     15 -- same
= 15           -- go's first clause
```

Hah, much better! That's about all there is to the bang pattern though. It comes in very handy when you don't want a lot of thunks to pile up on arguments that have "simple types", i.e where _weak head normal form_ also is _normal form_, or where just forcing the constructor is really what you want and is enough, but the former is a much common criterion. For your custom data types or more complex structures, there's something similar but much more appropriate.

## Strict fields in data types

Somehow dual to the need for strictness in accumulators mentionned above, sometimes we also want to evaluate values before we put them as some field in a data type. Imagine you're doing a bunch of matrix/vector operations. As a matter of fact, that's precisely what I had to do when I implemented the backpropagation learning algorithm in [my neural networks library](http://github.com/alpmestan/hnn). We do not want for the operations to pile up before being evaluated there! That would mean that thousands and thousands of arithmetic operations would have to be performed pretty much in one shot at the end to compute the new matrices, the output of the neural network on all input vectors and what not. That's just something we can't decently work with. So we somehow have to force the evaluation of all our matrix and vector coefficients right away, but throwing a lot of bangs at our code will be cumbersome, inelegant and we would have that feeling that there must be a better way to do it. Well, we would be right! And, to be honest, we will be throwing a bunch of `!`'s at our code there too, but differently. 

Keeping our matrix example, but simplifying it a bit, we will write a `2x2` matrix data type that will be strict in all its fields. No language extension needed this time though!

``` haskell
data Matrix2 a = M2 !a !a !a !a -- the four coefficients of our 2x2 matrix
-- here the coefficients can be of an arbitrary type, but this would
-- work too, only restricting us to Double coefficients:
data Matrix2 = M2 !Double !Double !Double !Double
```

That's it! I will let you check that the fields indeed get evaluated when we create a value of type `Matrix2` by creating a matrix whose coefficients are all equal to `product [1..]`.

``` active haskell
data Matrix2 = M2 !Double !Double !Double !Double deriving Show

main = do
  print (M2 1 2 3 4)
  -- this won't terminate
  let x = product [1..] in print (M2 x x x x)
```

You can see that here, all fields are marked with a `!`, but you can specify some strict fields and some non-strict ones, by just putting a `!` right before the fields you want to be strict. 

### Exercises

Here's some food for thought, with the answers right after the block of questions, in the "Show more" thingy.

- For example, you may want a hand-rolled `Pair` data type whose first component is strict and whose second component isn't. How would you do it? 
- Have you ever wondered what exactly made strict and lazy ByteStrings respectively strict and lazy? The same question holds for Text too. The lazy variant for both uses "chunks". Can you tell what's going on using our intuition so far and the hadoocks for [Data.ByteString.Lazy](http://hackage.haskell.org/packages/archive/bytestring/0.10.2.0/doc/html/Data-ByteString-Lazy.html) and  [Data.Text.Lazy](http://hackage.haskell.org/packages/archive/text/0.11.3.1/doc/html/Data-Text-Lazy.html)? How does this bring O(1) space when used accordingly?


@@@
The custon `Pair` type is pretty simple.
``` haskell
data Pair a b = Pair !a b
```

Regarding `Text` and `ByteString`, since they use a similar technique for implementing the lazy variant on top of the strict one, we will just study `Text`. So here's the idefinition of [Data.Text.Lazy.Text](http://hackage.haskell.org/packages/archive/text/0.11.3.1/doc/html/src/Data-Text-Lazy-Internal.html#Text):

``` haskell
data Text = Empty
          | Chunk {-# UNPACK #-} !T.Text Text
```

Let's skip the `{-# UNPACK #-}` pragma for now, as it is the topic of the next section and isn't our concern here. The `T.Text` field there refers to [Data.Text.Internal.Text](http://hackage.haskell.org/packages/archive/text/0.11.3.1/doc/html/Data-Text-Internal.html), a data type representing "A space efficient, packed, unboxed Unicode text type" -- this is what your usual strict Text type is. The second field refers to the type we're defining itself. 

Hmm, that rings a bell! That's quite close to a list, with elements of type T.Text but "strictified". That means that whenever we add another "bucket" of text to this list, it'll get evaluated (when the creation of the list will actually happen in memory). So we're operating on a list of strict texts. 

Taking a closer look, we see that the second field of the `Chunk` constructor isn't strict though. That means that we don't necessarily require the evaluation of the whole list whenever we just access a Chunk. The remaining elements are provided on-demand, as needed, `Chunk` by `Chunk`. 

That provides a partial answer to the O(1) space question: we can bring the content of a file (for example) chunk by chunk in memory. The answer is completed by the fact that if you manage to have the garbage collector free the `Chunk`s after they're used, you'll only have a handful of `Chunk`s in memory at the same time. That's however far from trivial to achieve, and is addressed by streaming libraries such as [pipes](http://hackage.haskell.org/package/pipes), [conduit](http://hackage.haskell.org/package/conduit) or [io-streams](http://hackage.haskell.org/package/io-streams).
@@@

## {-# UNPACK #-} and unboxed strict fields

Unpacking strict fields in data types is one of the key techniques for writing efficient Haskell code. But before introducing it, let's state the problem it solves.

Like in many programming languages, by default, GHC doesn't just store values. In particular, we have seen that laziness is implemented by storing a pointer to an unevaluated expression until the actual result is required, at which point we evaluate the expression and replace the pointer to the expression by the result. That requires some overhead on almost every single value you create. 

![Memory layout for boxed values](http://hackage.haskell.org/trac/ghc/raw-attachment/wiki/Commentary/Rts/Storage/HeapObjects/heap-object.png)

On the contrary, an _unboxed_ value is represented in memory by... just the value itself directly, no indirection is necessary, no pointer to a heap value (thus forbidding any laziness). 

For example, `Int` is the type of boxed ints. On the other hand, `Int#` is the type of unboxed ints, represented by a raw machine int, like you have them in C. The same goes for `Double#`, `Float#`, `Addr#` (that's just `void*`), etc. They all end with #, and most of the standard types are built on top of these unboxed types. One important note: these types cannot be defined in Haskell; they are somehow hardcoded in the compiler. The interested reader can head to [GHC.Prim's documentation](http://www.haskell.org/ghc/docs/latest/html/libraries/ghc-prim-0.3.0.0/GHC-Prim.html) to learn more about them and how they are generated by GHC.

So we happen to have access to the raw, low-level operations through this `GHC.Prim` module. For example, the addition for `Int#` defined there will most likely compile down to just a single addition assembly instruction. 

But what does this have to do with unpacking? Well, remember the matrices from earlier? What if I wanted to store my 4 `Double`s right in the matrix, instead of having 4 pointers to dereference? That's basically storing 4 values of type `Double#`. Well, that's precisely what the `{-# UNPACK #-}` pragma lets us do in the following code. Note that an unpacked field necessarily has to be strict, so GHC will reject an `{-# UNPACK #-}` pragma sitting right before a non-strict field.

``` haskell
data Mat2 = M2 {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
```

This is equivalent to:

``` haskell
data Mat2 = M2 Double#
               Double#
               Double#
               Double#
```

*Note*: you can't directly type `Int#`, `Double#`, etc unless you switch on the `MagicHash` language extension (either by passing `-XMagicHash` to GHC, or by writing `{-# LANGUAGE MagicHash #-}` at the top of your haskell module.

*Another note*: if you pass GHC the `-funbox-strict-fields` option (or write `{-# OPTIONS_GHC -funbox-strict-fields #-}` at the top of your module), it will try to unbox all the fields you have marked as strict (with a `!`), silently failing for those it can't unbox. If you mark a non-unpackable field with an `{-# UNPACK #-}` pragma, it will however warn you that it couldn't unpack it.

One last thing about `{-# UNPACK #-}`. When can we apply it? That's quite simple: the type you want to unpack inside the bigger one must have a single constructor (e.g, not `data Maybe a = Nothing | Just a`, which has two constructors `Nothing` and `Just`), must not be a type variable (e.g `data Foo a = Foo {-# UNPACK #-} !a` won't work, we must know what `a` is) and must be marked strict (`data Foo = Foo {-# UNPACK #-} Int` won't work but `data Foo = Foo {-# UNPACK #-} !Int` will). 

### Exercise

Fortunately, and that shouldn't be too surprising to you, GHC handles very well product types. Please consider the following snippet.

``` haskell
data StrictIntPair = SIP {-# UNPACK #-} !Int 
                         {-# UNPACK #-} !Int

data TwoSIPs = TwoSIPs {-# UNPACK #-} !StrictIntPair 
                       {-# UNPACK #-} !StrictIntPair

f :: TwoSIPs -> TwoSIPs
f (TwoSIPs (SIP a b) (SIP c d)) = TwoSIPs (SIP x x) (SIP x x)
  where x = a+b+c+d
```

How will `TwoSIPs` actually be represented? Don't pay too much attention to the `f` there, I only use it to witness the transformations GHC does.

@@@

`TwoSIPs` will be equivalent to the following data type.

``` haskell
data TwoSIPs = TwoSIPs Int# Int# Int# Int#
```

We can be sure of this by just looking at the generated Core (if you don't know what that is or how you can look at it, until there's a better starting point you probably should start [here](http://stackoverflow.com/questions/6121146/reading-ghc-core)).

``` haskell
f :: TwoSIPs -> TwoSIPs
f =
  \ (ds :: TwoSIPs) ->
    case ds of _ { TwoSIPs rb rb1 rb2 rb3 ->
    let {
      a :: Int#
      a = +# (+# (+# rb rb1) rb2) rb3 } in
    TwoSIPs a a a a
    }
```

First, we can see `TwoSIPs`' final layout on the 4th line: `case ds of _ { TwoSIPs rb rb1 rb2 rb3 -> ... }`. Just four fields. These fields happen to be `Int#`s, as witnessed by the `a` value, whose type is `Int#`, and it happens to be computed using primitive additions, and that's correctly typed only if all the operands (`rb`, `rb1`, `rb2` and `rb3`) are of type `Int#` too.

So, how come `TwoSIPs` ends up storing just four `Int#`s?

- If we take a look at `StrictIntPair`, well okay, the plan is to end up storing two `Int#`s there. Nothing complicated, GHC will gladly unpack these ints in `StrictIntPair` instead of having pointers to where they are stored, far away from their parent StrictIntPair. It will also unbox them, our strict unpacked `Int`s  may as well just be machine integers, that is to say `Int#`s. So we end up with something like `data StrictIntPair = SIP Int# Int#`. So far, so good!
- Now, let's consider `TwoSIPs`. We ask for the `StrictIntPair`s to be strict fields of `TwoSIPs`. In addition to this, we would like it to be unpacked in the memory layout of `TwoSIPs`. `StrictIntPair` being a single-constructor data type with no type variable involved, and since it's marked as strict, GHC can unpack it, therefore getting rid of the `SIP` constructor. Remember, we can unpack single-constructor data types only, that's for a reason: we know which constructor was used (since there is only one) so we don't loose any information on our way to `data TwoSIPs = TwoSIPs Int# Int# Int# Int#`. We can as well then just store those two `Int#`s per `StrictIntPair` right in `TwoSIPs`, thus the final representation.

If there's one thing to remember from this example, it's the way GHC took two `StrictIntPair` fields to transform them to just four `Int#`s.

@@@

You can learn more about unpacking in [the dedicated section of the GHC manual](http://www.haskell.org/ghc/docs/latest/html/users_guide/pragmas.html#unpack-pragma).

## Other ways to force the evaluation

There are some other ways to control the evaluation of your data. For example, if you ever need to evaluate a value before passing it to some function, maybe you're just after `($!)`, which is function application (just like `($)`), but evaluating the argument to WHNF before passing it to the function.

I have deliberately not mentionned `seq` in this article, because it seems to be a consensus that throwing `seq`s around in our code isn't good practice: using strict (optionally unboxed) fields and bang patterns appears to cover pretty much all cases. However, most of these techniques rely on `seq`, which is just a primop (like the operations on unboxed types, it's not definable in Haskell, thus baked into the GHC runtime system) and its action is to evaluate its first argument to WHNF and return the second argument. This sounds innocent but it can lead to broken guarantees about our code, when used badly, as you can read [here](http://stackoverflow.com/questions/12687392/why-is-seq-bad).