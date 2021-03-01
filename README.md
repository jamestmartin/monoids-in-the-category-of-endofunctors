# Monoids in the Category of Endofunctors
This is a **toy** library for studying the field of [Abstract Nonsense](https://en.wikipedia.org/wiki/Abstract_nonsense) through Haskell.
Writing this sort of code makes for a fun puzzle, but I want to be *absolutely clear*
that I would *never* do anything even remotely close to this in a serious codebase.
If you want to see code more representative of me as a programming practitioner,
please refer to pretty much any project other than this one.

This library currently includes:
* Category theory (`Category`)
* Recursion schemes (`Category.Functor.Foldable`)
* Dependent types, type-level programming, and codata (`Data`)
* Dependent quantifiers (implemented with the help of a typeclass; `Quantifier`)

For further information, build and read the Haddock.

## Content warning
This library is an abuse of GHC Haskell and an abuse of common sense.
Do not attempt to view this library if you are faint of heart.
