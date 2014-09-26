- Start Date: 2014/09/24
- RFC PR: (leave this empty)
- Rust Issue: (leave this empty)

# Summary

This RFC extends the type system with higher-kinded types allowing
programmers to abstract over type constructors.

This addition makes it possible to write generic code that was either
impossible or very ugly to encode before (as @aturon
demonstrates [here](https://github.com/rust-lang/rfcs/blob/master/active/0059-associated-items.md)).

# Motivation

This feature enables one to write generic code that operates on fully unapplied 
or partially applied type constructors. This feature is absent from most "main stream"
languages but is present in Scala and Haskell, and has an equivalent in C++'s
template template arguments. Allowing programmers to write code that abstracts over
collections, containers, smart pointers, and enabling advanced generic programming
techniques combined with features like associated types, multidispatch, and where
clauses.

# Detailed design

### High Level Design

We enrich type system is enriched with the notion of "kinds".

As an aside:
*unforunately this word has already been used in Rust, but I am not sure what
to use in its place since related words like sorts, kinds, and types already
have established meanings in the literature. I will try to refer to them as
higher kinds throughout.*

For those unfamiliar with kinds they though of as type constructor
polymorphism (or typelevel functions if you are so inclined). Higher kinds 
allows the programmer to take partially applied type constructors as 
type argument.

Theoretically kinds are just another level of classification. 
Just as we attribute every value a type, we now attribute every 
type a kind. This allows the programmer to specify what *kind*
of type they want to accept instead of treating all types
uniformly as we do in type systems with no kinds.

The kind used in a type system is a decision up that must be made by the
designers of the type system. Languages like Haskell (without extensions 
like DataKinds) and Scala have very simple kind systems. For comparsion 
there are languages like Omega that have a much richer notion of kinds.

These so called "simple kinds" are built on two kinds `*`
the kind of types, and `->` the kind given to type constructors.

We introduce the Kind of Rust types to be:
```
    kind ::= *
          | kind -> kind
          | '*
```

This grammar is purely for demonstration and we provide no way for the user to
directly annotate kinds with this syntax.

Here are a few examples:
```rust
struct Simple; // has kind '*'

struct Vec<T> { ... } // has the kind * -> *

struct HashMap<K, V> { ... } // has the kind * -> * -> *
```

The design used by Haskell and Scala would be sufficient if it was not for the
fact that Rust also allows for types to be parametrized by lifetimes. Since
these are treated by Rust distinctly at the type it seems prudent to consider them
distinct at the kind level as well.

For example the adaptor iterator `Map<'a, T>` would have the kind: `'* -> * -> *`.

Now each Item that declares a type implicitly assigns a kind to it in the
fashion we demonstrated in the above examples.

The second piece of the design is enabling us to write higher kinded type
parameters and to pass partially applied types as type arguments.

We allow for one to express that a type is higher kinded with the use of
`_`s as placeholders for plain type arguments, and `'_` for lifetime
arguments. 

If we consider a generic function that
takes a higher kinded type argument:

```rust
fn generic<G<_>, A>(arg: G<A>) -> G<A> { arg }
```
We can see here that G must be something of the kind: `* -> *`. We support
parital application for ease of use as well.

In the above situation G could be substitued by: `Vec` or `HashMap<int, _>`
both of which will have the kind `* -> *`.

Due to parametricity we do not know anything about G<_> and the ability by
itself is not very useful.

We must also be able to talk about the higher kinded types in traits to be
able to recover information about the type G.

Other languages with higher kinds and traits (typeclasses) support this
behavior already. There are a few quirks though. In both Haskell and Scala
it is possible for our dispatch type to be only higher kinded

For example in Functor:

```haskell
class Functor f where
  map :: (a -> b) -> f a -> f b
```

When instance selection happens we simply unify at the call site and pick the
instance based on `f`, so for a `[a]` map f xs we unify and solve that f is
[]. Then we look for an instance on `f`, and pick

instance Functor ([]) where
  map f [] = []
  map f (x:xs) = xs

In Scala everything happens via `implicit` selection solely on type:

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

and elsewhere they inject the operations on to the type using extension methods:

```scala
final class FunctorOps[F[_],A] ... { ... }
```

Rust traits require the Self type is fully applied since trait implementations
are resolved based on receiver type. For example if we invoke `map` on some
`Iterator` like `iter.map(f)` the `impl` is selected based on `iter` (and soon
any other input argument as specified in the Associated Types RFC).

We can easily support implementation of traits like `Functor` and friends with
this style:

```rust
trait Functor<F<_>, A> where Self == F<A> {
    fn fmap<B>(&self, f: |A| -> B) -> F<B>
}
```

```
impl Functor<Vec, A> for Vec<A> {
    fn fmap<B>(&self, f: |A| -> B) -> Vec<B> {
        ... // for now could be f.iter().map(f).collect()
    }
}
```

Obviously this change allows for us to implement traditional functional
programming abstractions like (Functor, Monad, ect) or Scala style overloaded
collections, but most importantly it opens up new kinds of abstractions in
Rust code.

One thing that comes to mind is that it now becomes possible to build data
structures that abstract over their ownership model.

For example we can create a list that abstracts over how it owns its tail list.
If we use Box we get a singly owned list. If we use Rc to get a list that
allows for structural sharing of sublists, or Arc for thread-safe sharing of
sublists.

```
trait SmartPtr<Ptr<_>, Val> where Self == Ptr<Val> {
  fn new(v: V) -> Ptr<Val>;
}

struct FlexList<C<_>, A> where C<A>: SmartPtr<C, A> {
  head: A,
  tail: C<A>
}

impl<C<_>, A> for FlexList<C, A> where C<A>: SmartPtr<C, A> {
  fn new(..) -> FlexList<C, A>
}
```

# Drawbacks

This change that it complicates the type system, and adds more surface
area to the type checker.

# Alternatives

Barring the out right rejection of this proposal there are not many alternatives.
The alternative choices that could be made have to do with syntax, and interplay
with traits.

The obvious syntax changes would be to fully infer the kinds of type variables
instead of making them explicit.

With regards to traits it may be possible to implement support for trait
resolution on only higher kinded type argumenta. This is mostly a nicety and is
open for suggestions and debate.

We could also omit parital application from the design and instead require users
to do: 
```rust
type IntMap<A> = HashMap<int, A>;
```

# Unresolved questions

Syntax around higher kinded types and traits. As discussed above we currently
opt for a backwards compatible but conservative solution to the combination of
type classes.

# References
AssociatedTypes: https://github.com/rust-lang/rfcs/blob/master/active/0059-associated-items.md
UFCS: https://github.com/rust-lang/rfcs/pull/132
ScalaHKT: https://lirias.kuleuven.be/bitstream/123456789/186940/4/tcpoly.pdf
WhereClauses: https://github.com/nikomatsakis/rfcs/blob/where-clauses/active/0000-where.md
WhereClausesPR: https://github.com/rust-lang/rfcs/pull/135
AssociatedTypesPR: https://github.com/rust-lang/rfcs/pull/195
FunctorEncoding: https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/Functor.scala#L16
FunctorSyntax: https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/syntax/FunctorSyntax.scala#L5
