---
tags:
    - hlox
date: 2025-03-10
title: hlox—how did we get here?
---

i mentioned in [an update](/posts/an_update.html) that i wanted to started writing a devlog-y thing while i work through bob nystrom's ['crafting interpreters'](https://craftinginterpreters.com/). i'm now on chapter 24/30, so we have a lot to catch up on. in this post, i'll talk about working through the first half of the book.

my general approach has been to honour the semantics of the target language without making the exact same design decisions as the author, instead opting for my impression of the idiomatic way to do things in haskell. so, for instance, i use parser combinators instead of recursive descent to perform the parsing.

<section>

## why haskell?

because i like haskell. it's fun!! there's a huge wealth of cool features within the haskell ecosystem which i've barely scratched the surface of. this project has given me the opportunity to learn a bunch of new things. before getting into those, though, here's a sample of the language to familiarise the unfamiliar reader:

```lox
fun fib(n) {
  if (n <= 1) { return n; }
  return fib(n - 1) + fib(n - 2);
}

for (var i = 1; i < 25; i = i + 1) {
  print(fib(i));
}
```

there are a few properties we can gather with reasonable confidence immediately from the sample. the language is (in addition to boring):

- imperative
- strict
- dynamically typed

i'd grant that the last property isn't a given seeing that the types for this sample could be made to be trivially derivable. in fact, one idea i have for after i'm finished working through the book is adding a simple type system and checker.

in any case, it might seem like an awkward choice to use haskell here given that it lives on the opposite side of the language space as the target language, being a strongly-typed, functional, lazy, (mostly) pure language. seemingly, the largest ramification here is that lox is impure by default and haskell pure by default.

the consequence here is that actual execution of lox code must take place in IO in some form. which leads us to:

## monad transformers, mtl-style effects

prior to starting this project, i'd never worked with monad transformers apart from in very limited contexts. i've had a chance to really get my hands dirty with them during itjj, which has been a lot of fun.

the signature of the main entry point to the tree-walk interpreter looks like:

```haskell
evalExpr :: (MonadState Env m, MonadError LoxError m, MonadIO m) => Expr -> m Value
```

in addition to IO, we also need some notion of an environment (more on this later) to store variables and their values in, and a notion of exception so that we can fail gracefully.

### properties

there are two immediate properties of mtl-style effects that i want to highlight here:

**(a) composition**

we can create functions that require a subset of the effects that the full evaluation takes place in and freely call them from that environment, e.g.,<label for="1"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="1"
       class="margin-toggle sidenote-number"/>
<span class="sidenote">i think having the function argument was a weird design choice. i could've just return `m Bool`, which a callee could've called `fmap f` on if they really wanted to. i've changed this in the VM implementation, which i think is nicer.</span>

```haskell
expectBool :: (MonadError LoxError m) => (Bool -> a) -> SourcePos -> Value -> m a
expectBool f _ (TBool b) = return $ f b
expectBool _ l v = throwError . exprError l $ TypeError "bool" v

evalExpr (Located l e) =
  case e of
    {- ... -}
    BinOp op e1 e2 -> do
      v1 <- evalExpr e1
      case op of
        -- manual short circuit
        And -> do
          b1 <- expectBool id (location e1) v1 -- here
          if b1
            then
              TBool <$> (evalExpr e2 >>= expectBool id (location e2)) -- and here
            else pure (TBool False)
```

the effects also have no knowledge of each other. in this way, the effects can be said to _compose_.
<label for="2"
       class="margin-toggle sidenote-number">
</label>
<input type="checkbox"
       id="2"
       class="margin-toggle sidenote-number"/>
<span class="sidenote">it might be slightly misleading to say the effects have _no_ knowledge of each-other, because sometimes changing the ordering of the monad stack that the effects end up executing in will change both semantics and performance characteristics.</span>

one alternative to mtl-style is defining a concrete monad stack, something like:

```haskell
type Evaluation = ExceptT LoxError (StateT Env IO)

evalExpr :: Expr -> Evaluation Expr
expectBool :: Value -> Evaluation Bool
```

which, admittedly, is much less verbose. but we immediately lose out on this composition, and now `expectBool` is forced to take place in `Evaluation` even though it requires only a small piece of the functionality of the full monad.

**(b) polymorphism**

`evalExpr` is polymorphic in its effects. this is what i mean when i say "some notion of \[an environment|exception\]" above. we could theoretically write our own instance of MonadState (like, say, one that records changes in state as they occur) for use in testing/debugging.

maybe it goes without saying, but returning to the alternative of concrete monad stacks, we also lose out here too.

### my thoughts (on mtl)

overall, i have found this style of handling effects to be reasonably ergonomic and have continued using it extensively in the virtual machine side of the implementation in the second half of the book.

i think it's really cool to be able to see at a glance which effects each function needs. with that said, mtl-style effects are not the only way to accomplish this. namely, there are several so-called "algebraic effects" libraries in haskell which tackle the same problem from different angles (my understanding is most are implemented using the Free monad or delimited continuations as primitives). another extension that i hope to implement after finishing the book is rewriting the VM in one of these effect systems.

</section>

<section>

## environments

really the only other interesting design choice that came about when reading the first half of the book was how to handle environments. the book defines environments like:

```java
class Environment {
  final Environment enclosing;
  private final Map<String, Object> values = new HashMap<>();
  // methods...
}
```

this translates fairly naturally to the following haskell definition:

```haskell
type Scope = Map String Value

newtype Env = Env [Scope]
```

functions on `Env`s are straightforward:

```haskell
newScope :: Env -> Env
newScope (Env scopes) = Env $ Map.empty : scopes

tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (_ : xs) = Just xs

enclosing :: Env -> Maybe Env
enclosing (Env scopes) = Env <$> tail' scopes

find :: String -> Env -> Maybe Value
find i (Env scopes) = asum $ Map.lookup i <$> scopes

-- this is a bit sussy
modifyScope :: (Scope -> Scope) -> Env -> Env
modifyScope f (Env (scope : scopes)) = Env $ f scope : scopes
modifyScope _ (Env []) = Env []

declare :: String -> Value -> Env -> Env
declare s v = modifyScope (Map.insert s v)

assign :: String -> Value -> Env -> Maybe Env
assign _ _ (Env []) = Nothing
assign s v env@(Env (scope : scopes)) =
  (M.lookup s scope $> declare s v env) -- in current scope
    <|> (attach scope <$> assign s v (Env scopes)) -- keep current scope intact
```

### closures

it's a reasonably nice module, but it fails to maintain consistent semantics with the language of the book. consider:

```lox
fun counter() {
  var n = 0;
  fun count() {
    n = n + 1;
    return n;
  }
  return count;
}

var count = counter();
print(count());
print(count());
```

any reasonable semantics for lox would print 1 and then 2, or else disallow this syntactic construction entirely. the book's solution for this is to attach to each function AST node its calling environment, and switch to that environment when beginning the execution of its body. this works because Java classes are pointers and so mutation is shared among all environments which share the same instances.

if we translate this solution directly, we end up not affecting the world outside of the function's environment, because things are immutable in haskell land (and besides, the environment that `counter` originally executed in was garbage-ified the moment it returned control to the global scope, along with the original `n`).

to emulate the book's solution in haskell, then, the most straightforward way is to redefine `Scope`'s values to be `IORef Value`s instead of plain `Value`s. for whatever reason, this really irked me and i thought for a long time about a way to maintain the purity of the environment structure. ultimately, the solutions i came up with were hacks and unsatisfactory.

but thinking back on it now, it makes perfect sense that the environment can't reasonably be pure. we're executing in an impure language where mutation can happen anywhere. so past a certain point of complexity it doesn't make sense to model with pure functions, and besides, i crossed that threshold in the monadic function which actually does the execution.

### resolution

another program that poses a problem with the above model:

```lox
var a = 1;
{
  fun bar() {
    print(a);
  }

  bar();
  var a = 2;
  bar();
}
```

which should be printed in the second call to `bar`: 1 or 2? or should it be a syntax error? the book says 1, which i think is fair in keeping the semantics of closures consistent. i also think syntax error is an acceptable answer, but with the `find` implementation i defined above of:

```haskell
find i (Env scopes) = asum $ Map.lookup i <$> scopes
```

we're going to print 2.

the solution to this is to determine statically the locations that identifiers point to (more specifically, how many `Scope`s at the front of an `Env` we need to disregard before looking up the variable). i had a go at implementing this but ran into a few really annoying bugs, at which point, i decided:

</section>

<section>

## i'm going to bury my head in the sand

if anything, the exercise of implementing closures made me question why we'd ever want a language feature with these semantics. reasoning about the execution of a program becomes so much harder when a call to a function can have such nonlocal implications. it makes me appreciate Rust's static ban on such behaviour while preserving a reasonable imperative model.

but really i'm just coping. it was a hard problem and i didn't particularly feel like forging on.

since these semantics issues were only going to get worse with the implementation of classes and instances, i decided to skip ahead and begin the second half of the book, where we reimplement the language using a compile-to-bytecode-and-then-virtual-machinise<sup>TM</sup> architecture.

## conclusion

so that brings us up to the second half of the book. i've omitted a huge number of my thoughts from the first half of the book for the sake of brevity, but i might end up coming back to make changes if there's some take i desperately want to put out into the world.

one final thing:

the author mentions that the following program runs in jlox on his machine in 72 seconds.

```lox
fun fib(n) {
  if (n < 2) { return n;}
  return fib(n - 1) + fib(n - 2);
}

var before = clock();
print(fib(40));
var after = clock();
print(after - before);
```

on my fairly well-equipped work laptop, this runs in 25 and a half minutes in my tree-walk implementation! i haven't done any profiling but i suspect some of this extra overhead is due to the monad stack that we execute much of the code in, and also in variable lookups, which take exponentially more time as we recurse. it'll be fun to see the speedup i get from the new architecture, and how it compares to the book's implementation in C.

there's a fair bit more to write about before we catch up to where i am in the book, though, so i'll sign off here. thanks

</section>
