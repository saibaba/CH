Introduction
============

The Curry-Howard correspondence says that we can use the type checker of a programming language as a proof checker. Any time we want to prove a logical proposition, we:

    * Translate the logical proposition to the corresponding type in our programming language - type corresponds to proposition
    * Create a value in our programming language that has the given type - if a value could be created, the type is inhabitated and each value is a proof of the prosition
    * Cse the type-checker to verify that our value has the specified type - what type checker does internally is equivalent to proof steps

If we find a value of the given type then that completes the proof of our original proposition.

CH is limited to "intuitionistic logic" which is just like the "regular" logic you learned in school but without the law of the excluded middle or double negation elimination:

* Not an axiom: P ⇔ ¬¬P (but P ⇒ ¬¬P is fine)
* Not an axiom: P ∨ ¬P

Note 1
======

A note on "False => False" is true:
∀(n ∈ Natural): even(n) ⇒ odd(n+1)
So, if n is odd, say 5,  even(5) is false and odd(5+1) is false. We still want above to be true as a theorem, so we let False => False to be true.

Note 2
======

The haskellforall wiki talks about type constructors to create new types (propositions) from existing ones (via various connectors like implies, and, or...). 
The equivalent way to do is to use "forall": For example:

data T = forall x. Con t | Nil

vs.

data T a = Con a | Nil


CH in reverse
=============

```
const :: forall a b. a -> b -> a
const x y = x
```

Since a-> b-> a is inhabitated by cost, a -> b -> a must be a theorem!

Problem with ⊥
==============
This makes every type inhabitated (by ⊥) i.e., everything is a theorem - an inconsistent logic ! Hence we have to work with a subset of haskell.


References
==========

* https://www.olivierverdier.com/posts/2015/10/09/type-logic-haskell/ (In type logic, there is no truth tables. A proposition is not true or false. As a proposition is simply a type, there may be many inhabitants which “prove” that the proposition is true.)
* https://en.wikibooks.org/wiki/Haskell/The_Curry%E2%80%93Howard_isomorphism
* http://www.haskellforall.com/2017/02/the-curry-howard-correspondence-between.html
* https://stackoverflow.com/questions/10753073/whats-the-theoretical-basis-for-existential-types/10753957#10753957
* https://stackoverflow.com/questions/28545545/
* https://gist.github.com/CMCDragonkai/b203769c588caddf8cb051529339635c
