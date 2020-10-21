# monoids-in-the-categoy-of-endofunctors

Abstract nonsense in Haskell: category theory, recursion schemes, dependent data types, etc.
The current version only contains category theory.

## Category theory morality
Each morality level re-defines the typeclasses of the level before it in a more general way.
This generality comes at the cost of the typeclasses being more difficult to understand and use, which is why the excessively general, incomprehensible, and virtually unusable stuff is called 'evil'.

### Good
The 'good' alignment contains more-or-less standard Haskell that any Haskeller should be able to understand.
It works exclusively in the category of types.

### Neutral
The 'neutral' alignment is more abstract, and allows working with categories other than Type.
This generalization lets us speak of functors which are not endofunctors, the category of constraints or equality, etc..

In this case a category is defined by a hom, and its objects are any value of the type the hom operates on.
(This is perhaps a confusing wording; as an example, the category defined by `->` has *all* types as objects.)

Here's some stuff we can talk about in neutral but not good:
* Categories, monoidal categories, and enriched categories.
* Functors which are not endofunctors (such as `:-`).
* Natural transformations.
* Monoids in categories other than Type.

### Evil
The 'evil' alignment generalizes categories to be defined both by a hom *and* a constraint restricting its objects.
This broadens what we are allowed to do a *lot*, but Haskell is *really* not meant to be used in this way,
making actually *doing* these things and using the resultant APIs extremely difficult.

Here are some new things we can describe in evil:
* `Set` can now finally get a functor instance: it is an endofunctor in the category of ordered types.
* Monads are directly defined as monoid objects in the monoidal category of endofunctors, rather than as their own special typeclass. (Monad is still available as a convenient alias.)
