# Pear core

## Recursion schemes cheat-sheet

### Definitions

**Recursion** *consumes* a recursive data structure to produce a simpler (often non recursive) one.

**Corecursion** *produces* a recursive data structure, starting from a simpler (often non recursive) one.

**Algebra** tears down, ie *folds*, one layer of a recursive data structure to produce a simple value (`F[A] => A`), sometimes named F-Algebra.

**Coalgebra** builds up, ie *unfolds*, a simple value into one layer of a recursive data structure (`A => F[A]`), sometimes named F-Coalgebra.

### Schemes

There are three kinds of schemes:

* folds : tear down a recursive data structure and collapse it to a simpler one (but can be used to build another recursive data structure to), it works by first "descending" to the bottom of the structure and then applying an algebra while going back up the structure. It's therefore a bottom-up traversal. The simplest instance of a fold (and the one that is used 99% of the time) is `cata`(morphism). 

* unfolds : build up a recursive data structure from a simple value (that can sometime be another recursive data structure), it first applies the coalgebra to that value to produce the root of the resulting recursive structure, and then continues top-down. The simplest instance of an unfold is `ana`(morphism).

* refolds : combine a unfold followed by a fold. It allows to go from `A` to `B` through a recursive `F[_]` by combining a coalgebra `A => F[A]` and an algebra `F[B] => B`. The cool point is that it doesn't have to build the full `F[A]` structure to do so, only the branches one after another. The simplest refold is `hylo`(morphism). As a matter of fact, in matryohska `cata` and `ana` are implemented in terms of `hylo`, using a no-op coalgebra (resp. algebra).

### (Co)AlgebraM, hyloM, etc...

There can be various generalisations of (co)algebra and of related schemes. When a (co)algebra or scheme has the `M` suffix, it means that its result is wrapped in a monad. So `AlgebraM = F[A] => M[A]` and `CoalgebraM = A => M[F[A]]`. Each step of M-suffixed scheme will flatMap (bind) on the choosen monad. This can be used to:

* short-circuit the traversal: if you use the `X \/ ?` monad and return a `-\/` at some point, the traversal will stop there
* accumulate some values in a `State` (like it is done in `pear.form.decorate` and `pear.form.evaluate`).
