{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}


{- Notes 
 - See note about inconsistency in Haskell type system: https://en.wikibooks.org/wiki/Haskell/The_Curry%E2%80%93Howard_isomorphism#The_problem_with_.E2.8A.A5
 -}

{- In this file, Prop means all propositions, true or false -}

-- see https://en.wikibooks.org/wiki/Haskell/The_Curry%E2%80%93Howard_isomorphism#Falsity
data Void -- no constructor, hence you cannot create any value, so uninhabited type
-- a better naming for Void would be 
data False  -- False, the uninhabited type

-- To prove ∀(a ∈ Prop): a ⇒ a
---- Translate into type: forall a. a -> a
---- Create a value of that type
--
id1 :: forall a . a -> a
id1 x = x

-- This or any of the tests are not needed, just that haskell can compile above instance for the type is enough proof

test1 = do
  putStrLn "Test-1 proposition  ∀(a ∈ Prop): a ⇒ a"
  print $ id1 20

-- To prove ∀(a ∈ Prop): a ⇒ a ⇒ a
---- Translate into type: forall a . a -> a -> a

---- Create a value of that type
first :: forall a . a -> a -> a
first x y = x

---- A second value / proof
second :: forall a . a -> a -> a
second x y = y

test2 = do
  putStrLn "Test-2 proposition ∀(a ∈ Prop): a ⇒ a ⇒ a"
  print $ first "first arg" "second arg"
  print $ second "first arg" "second arg"

-- To prove ∀(a ∈ Prop): ∀(b ∈ Prop): ∀(c ∈ Prop): (b ⇒ c) ⇒ (a ⇒ b) ⇒ (a ⇒ c)
---- Translate into type: forall a . forall b . forall c . (b->c)->(a->b)->(a->c)
---- Create value of that type
comp f g = \x -> f $ g x
---- proofs and programs are equivalent to one another: whenever we prove a logical proposition we get a potentially useful function for free.

test3 = do
  putStrLn "Test-3 proposition ∀(b ∈ Prop): ∀(c ∈ Prop): (b ⇒ c) ⇒ (a ⇒ b) ⇒ (a ⇒ c)"
  print $ comp (\x -> x + 1) (\x -> x * 10) 3

-- Circular reasoning is possible in Haskell : ∀(a ∈ Prop): a
---- Create a type: a
---- create a value of that type
example :: forall a . a
example = let x = x in x

test4 = do  -- do not run, goes into infinite loop
  putStrLn "Test-4 proposition  ∀(a ∈ Prop): a"
  example

-- just to simplify notation, instead of pair, to call with more natural name '&' for logical 'and'
type a :&: b = (a,b)
-- likewise for logical 'or'
type a :+: b = Either a b

-- To prove ∀(a ∈ Prop): ∀(b ∈ Prop): (a ∧ b) ⇒ a
---- Translate into type: forall a b . (a, b) -> a
---- Create a value of that type
myfst :: forall a b . a :&: b -> a
myfst (x, y) = x
---- this proves ∀(a ∈ Prop): ∀(b ∈ Prop): (a ∧ b) ⇒ b
mysnd :: forall a b . a :&: b -> b
mysnd (x, y) = y

test5 = do
  putStrLn "Test-5 a proposition ∀(a ∈ Prop): ∀(b ∈ Prop): (a ∧ b) ⇒ a"
  print $ myfst ("one", 1)
  putStrLn "Test-5 b proposition ∀(a ∈ Prop): ∀(b ∈ Prop): (a ∧ b) ⇒ b"
  print $ mysnd ("one", 1)


-- To prove ∀(a ∈ Prop): ∀(b ∈ Prop): (a ∧ (a ⇒ b)) ⇒ b
---- Translate into type: forall a . forall b . (a, a->b) -> b
---- Create a value of that type

act1 :: forall a . forall b . (a, a->b) -> b
act1 (v, f) = f v
test6 = do
  putStrLn "Test - 6 proposition ∀(a ∈ Prop): ∀(b ∈ Prop): (a ∧ (a ⇒ b)) ⇒ b"
  print $ act1 (1, \x -> x + 1)




-- To prove ∀(a ∈ Prop): ∀(b ∈ Prop): ∀(c ∈ Prop): ((a ∧ b) ⇒ c) ⇒ (a ⇒ b ⇒ c)
---- Translate into type: forall a . forall b . forall c . ( (a, b) -> c ) -> (a -> b -> c)
---- It is a function that takes function of type (a, b) -> c and returns a function of type a->b->c
---- i.e., takes a function that takes a pair in the input and returns an output and converts into
---- a function that takes first of pair, then second of pair and returns a function that returns
---- the same output ----- currying!!!

---- Create a value of that type
mycurry:: forall a . forall b . forall c . ( (a, b) -> c ) -> (a -> b -> c)
-- mycurry = \f -> \x -> \y -> f (x, y)
mycurry f x y = f (x, y)

adder :: (Integer, Integer) -> Integer
adder (x, y) = x + y

test7 = do
  putStrLn "Test -7 currying proposition: ∀(a ∈ Prop): ∀(b ∈ Prop): ∀(c ∈ Prop): ((a ∧ b) ⇒ c) ⇒ (a ⇒ b ⇒ c)"
  print $ ( (mycurry adder) 98 37)

-- To prove ∀(a ∈ Prop): ∀(b ∈ Prop): b ⇒ a ∨ b
---- Translate into type: forall a b . a -> OneOrOther a b
data OneOrOther = forall a . Show a => One a | forall b . Show b => Other b
instance Show OneOrOther where
  show (One a) = show a
  show (Other b) = show b
---- Create a value of that type
l :: forall a . (Show a) => a -> OneOrOther
l v = One v

test8 = do
  putStrLn "Test - 8 proposition  ∀(a ∈ Prop): ∀(b ∈ Prop): b ⇒ a ∨ b"
  print $ l "s"

-- To prove ∀(a ∈ Prop): a ∨ a ⇒ a
---- Translate into type: <see OneOrOther above>
---- Create a value of that type
b :: forall a . (Show a) => Either a a -> a
b (Left x) = x
b (Right x) = x

test9 = do
  putStrLn "Test -9 proposition  ∀(a ∈ Prop): a ∨ a ⇒ a"
  print $ b (Left "s")

-- To prove ∀(a ∈ Prop): ∀(b ∈ Prop): a ⇒ b ⇒ a"
---- meaning 'if we assume a is true, and further assume b is true, then a must be true'
---- Translate into type: a->b->a
---- Show that type is inhabitated, i.e., we can create an instance, i.e., for functions, we create a function implementation
myconst :: forall a b . a -> b -> a
myconst x y = x

test10 = do
  putStrLn "Test - 10 proposition ∀(a ∈ Prop): ∀(b ∈ Prop): a ⇒ b ⇒ a"
  print $ myconst 20 "string"

-- Not 
---- If a is a theorem (inhabitated type) then Not(a) is not a theorem (uninhabitated type). So,
---- type Not a = forall a. a -> False
---- would work as you cannot create an instance of a function that returns False (since you cannot create an instance of False type, function implementation do not have a way of returning a value of False type
---- Like wise, if a is not a theorem (uninhabitated type), then Not(a) is a theorem (inhabitated type).
---- The function id:: False->False would fit the bill!

type Not a = a -> False



-- Now complicated stuff

-- Law of excluded middles (every proposition must be true or false)
---- Or ∀(a ∈ Prop): a ∨ not(a)
---- exclmdl :: forall a . Either a (a -> False) Or more readable one:
exclmdl :: forall a . Either a (Not a)
---- It is not possible to create an instance of exclmdl, so, we use a bottom and assume it exists
exclmdl = exclmdl

-- To prove (∀(b ∈ Prop): b ∨ not(b)) ⇒ (∀(a ∈ Prop): not(not(a)) ⇒ a)
---- Above is "if we assume that all propositions are either true or false, then if a proposition is not false it must be true"
---- (See https://en.wikibooks.org/wiki/Haskell/The_Curry%E2%80%93Howard_isomorphism#Axiomatic_logic)
---- Translate into type: forall b .  ( Either b or (b->Void) ) -> ( forall a . ((a->Void) -> Void) -> a )
---- Create a value of that type (You need rankNType because forall is not for the whole expr but indpendently to each a and b
--  doubleneg ::  ( forall b .   Either b (b->Void) ) -> ( forall a . ((a->Void) -> Void) -> a )

-- (Either Void A) and A are isomorphic, becuase you cannot create instance of Void, Either Void A is always Right A
-- and the isomorphic transformation is just strip off Right


main = do
  test1
  test2
  test3
  --test4
  test5
  test6
  test7
  test8
  test9
  test10

