import Prelude hiding (fmap)
import Control.Applicative hiding ((<*>), liftA2)
import Control.Monad hiding ((>>=))

{- Part 0
 - 0.1 What is the Functor function, with its type?
 -  The functor function is 'fmap', its type is (a -> b) -> f a -> f b
 - 0.2 What are the applicative functions, what are their types?
 -  The applicative functions are (<*>) (referred to as app), and 'pure'.
 -  Their respective types are f (a -> b) -> f a -> f b,
 -  and a -> f a
 - 0.3 What are the Monad functions, what are their types?
 -  The monadic functions are (>>=), and 'return'.
 -  Their respective types are m a -> (a -> m b) -> m b,
 -  and a -> m a
 - Part 1
 - 1.1 Write fmap in terms of the applicative functions <*> and pure.
 -  See below:
 -} 

fmap :: Applicative f => (a -> b) -> f a -> f b
fmap f a = (pure f) <*> a

{-
 - Main.fmap (+1) (Just 1) -> Just 2
 - Main.fmap (+1) Nothing -> Nothing
 -
 - 2.2 Explain how your solution works.
 -  fmap defined above works by lifting our function, g, into the Applicative space
 -  by using 'pure'. With our Functor wrapped function, we can use (<*>) to apply
 -  the function to another functor wrapped value contained in x.
 -
 - Part 2
 - 2.1 Write (<*>) in terms of 'liftA2'
 -  See below:
 -}

(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<*>) f a = liftA2 id f a

{- (Just (+1)) Main.(<*>) (Just 1) -> Just 2
 - (Just (+1)) Main.(<*>) Nothing -> Nothing
 -
 - 2.2 Write liftA2 in terms of (<*>)
 -  See below: 
 -}

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = (fmap f a) <*> b

{- Main.liftA2 (+) (Just 1) (Just 2) -> Just 3
 - Main.liftA2 (+) (Just 1) Nothing -> Nothing
 -
 - 2.3 Explain how both solutions work.
 -  In writing (<*>) in terms of liftA2, we take advantage of the fact that id
 -  is fully polymorphic. So we use liftA2 to lift id into the Applicative space
 -  and compose it with our applicative function, f (a -> b). We apply this composed
 -  function on our Applicative wrapped f a to get an f b.
 -  
 -  In writing liftA2 in terms of <*> we create a curried applicative function
 -  by using fmap on our function f and the first Applicaive wrapped argument, f a.
 -  We then get an Applicative function back that using <*>, we may apply on the second
 -  Applicative wrapped argument, f b, to get f c. 
 -
 - Part 3
 - 3.1 Write join :: Monad m => m (m a) -> m a, using any monadic, applicative,
 -  or functor functions.
 -  See below:
 -}

join :: Monad m => m (m a) -> m a
join a = a >>= id

{- join (Just (Just 1)) -> Just 1
 - join (Just Nothing) -> Nothing
 -
 - 3.2 Write (>>=) using join (without using (>>=))
 - See below:
 -}

(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) a f = join (fmap f a)

{- 3.3 Explain how both solutions work
 -  To perform join, we can use the fact that (>>=) will apply a funciton to the
 -  interior of the monad, returning the transformed payload. If we use id, we
 -  won't transform the interior, but we will have extracted it from the outer Monadic
 -  wrapper, which is still typechecks, since the interior is wrapped in the same Monad.
 -
 -  To write (>>=) in terms of join, we fmap our function f. This has the effect of
 -  applying our function (a -> m b) on our (m a), which will normally return an m b,
 -  but as the return type of fmap, we wrap it once more in the Monadic space, giving us
 -  m m b. We can use join to finally return m b. 
 -}
