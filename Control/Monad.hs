{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- The "Control.Monad" module provides the 'Functor', 'Monad' and
-- 'MonadPlus' classes, together with some useful operations on monads.

module Control.Monad (
    -- * Functor and monad classes

      Functor(fmap)
    , Monad((>>=), (>>), return, fail)

    , MonadPlus (   -- class context: Monad
          mzero     -- :: (MonadPlus m) => m a
        , mplus     -- :: (MonadPlus m) => m a -> m a -> m a
        )
    -- * Functions

    -- ** Naming conventions
    -- $naming

    -- ** Basic @Monad@ functions

    , mapM          -- :: (Monad m) => (a -> m b) -> [a] -> m [b]
    , mapM_         -- :: (Monad m) => (a -> m b) -> [a] -> m ()
    , forM          -- :: (Monad m) => [a] -> (a -> m b) -> m [b]
    , forM_         -- :: (Monad m) => [a] -> (a -> m b) -> m ()
    , sequence      -- :: (Monad m) => [m a] -> m [a]
    , sequence_     -- :: (Monad m) => [m a] -> m ()
    , (=<<)         -- :: (Monad m) => (a -> m b) -> m a -> m b
    , (>=>)         -- :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
    , (<=<)         -- :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
    , forever       -- :: (Monad m) => m a -> m b
    , void          -- :: Functor f => f a -> f ()

    -- ** Generalisations of list functions

    , join          -- :: (Monad m) => m (m a) -> m a
    , msum          -- :: (MonadPlus m) => [m a] -> m a
    , filterM       -- :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
    , mapAndUnzipM  -- :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
    , zipWithM      -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
    , zipWithM_     -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
    , foldM         -- :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
    , foldM_        -- :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
    , replicateM    -- :: (Monad m) => Int -> m a -> m [a]
    , replicateM_   -- :: (Monad m) => Int -> m a -> m ()

    -- ** Conditional execution of monadic expressions

    , guard         -- :: (MonadPlus m) => Bool -> m ()
    , when          -- :: (Monad m) => Bool -> m () -> m ()
    , unless        -- :: (Monad m) => Bool -> m () -> m ()

    -- ** Monadic lifting operators

    , liftM         -- :: (Monad m) => (a -> b) -> (m a -> m b)
    , liftM2        -- :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
    , liftM3        -- :: ...
    , liftM4        -- :: ...
    , liftM5        -- :: ...

    , ap            -- :: (Monad m) => m (a -> b) -> m a -> m b

  ) where

import "base" Data.Functor
import qualified GHC.Base as B
import "this" Data.List
import GHC.Base ( Int, Bool(..), error, Maybe(..), (.), String, id, flip, IO )

infixl 1  >>, >>=

{- | The 'Monad' class defines the basic operations over a /monad/,
a concept from a branch of mathematics known as /category theory/.
From the perspective of a Haskell programmer, however, it is best to
think of a monad as an /abstract datatype/ of actions.
Haskell's @do@ expressions provide a convenient syntax for writing
monadic expressions.

Instances of 'Monad' should satisfy the following laws:

* @'return' a '>>=' k  =  k a@
* @m '>>=' 'return'  =  m@
* @m '>>=' (\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@

NB: This 'Monad'-class does not have the post-Haskell2010 @Applicative@ class as its superclass.
-}
class Monad m where
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b

    -- | Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    (>>)        :: forall a b. m a -> m b -> m b
    m >> k = m >>= \_ -> k
    {-# INLINE (>>) #-}

    -- | Inject a value into the monadic type.
    return      :: a -> m a

    -- | Fail with a message.  This operation is not part of the
    -- mathematical definition of a monad, but is invoked on pattern-match
    -- failure in a @do@ expression.
    fail        :: String -> m a
    fail s      = error s

-- | Monads that also support choice and failure.
class Monad m => MonadPlus m where
   -- | the identity of 'mplus'.  It should also satisfy the equations
   --
   -- > mzero >>= f  =  mzero
   -- > v >> mzero   =  mzero
   --
   mzero :: m a

   -- | an associative operation
   mplus :: m a -> m a -> m a

instance Monad IO where
    return = B.return
    (>>=) = (B.>>=)
    (>>)  = (B.>>)

-- See Note: [List comprehensions and inlining]
instance Monad []  where
    {-# INLINE (>>=) #-}
    xs >>= f             = [y | x <- xs, y <- f x]
    -- {-# INLINE (>>) #-}
    -- (>>) = (*>)
    {-# INLINE return #-}
    return x            = [x]
    {-# INLINE fail #-}
    fail _              = []

instance MonadPlus [] where
    mzero = []
    mplus = (++)


instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    (Just _) >>  k      = k
    Nothing  >>  _      = Nothing

    return              = Just
    fail _              = Nothing

instance MonadPlus Maybe where
   mzero = Nothing

   Nothing `mplus` ys  = ys
   xs      `mplus` _ys = xs

-- -----------------------------------------------------------------------------
-- Functions mandated by the Prelude

-- | @'guard' b@ is @'return' ()@ if @b@ is 'True',
-- and 'mzero' if @b@ is 'False'.
guard           :: (MonadPlus m) => Bool -> m ()
guard True      =  return ()
guard False     =  mzero


-- | This generalizes the list-based 'concat' function.
msum        :: MonadPlus m => [m a] -> m a
{-# INLINE msum #-}
msum        =  foldr mplus mzero

infixr 1 =<<

-- | Same as '>>=', but with the arguments interchanged.
{-# SPECIALISE (=<<) :: (a -> [b]) -> [a] -> [b] #-}
(=<<)           :: Monad m => (a -> m b) -> m a -> m b
f =<< x         = x >>= f

infixr 1 <=<, >=>

-- | Left-to-right Kleisli composition of monads.
(>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g     = \x -> f x >>= g

-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped
(<=<)       :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<)       = flip (>=>)


-- | @'mapM' f@ is equivalent to @'sequence' . 'map' f@.
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
{-# INLINE mapM #-}
mapM f as = foldr k (return []) as
            where
              k a r = f a >>= \x ->
                      r   >>= \xs ->
                      return (x:xs)
                      -- do { x <- f a; xs <- r; return (x:xs) }

-- | @'mapM_' f@ is equivalent to @'sequence_' . 'map' f@.
mapM_           :: Monad m => (a -> m b) -> [a] -> m ()
{-# INLINE mapM_ #-}
mapM_ f as      =  sequence_ (map f as)

-- | Evaluate each action in the sequence from left to right,
-- and collect the results.
sequence :: Monad m => [m a] -> m [a]
{-# INLINE sequence #-}
sequence = mapM id

-- | Evaluate each action in the sequence from left to right,
-- and ignore the results.
sequence_        :: Monad m => [m a] -> m ()
{-# INLINE sequence_ #-}
sequence_ ms     =  foldr (>>) (return ()) ms


-- | 'forM' is 'mapM' with its arguments flipped
forM            :: Monad m => [a] -> (a -> m b) -> m [b]
{-# INLINE forM #-}
forM            = flip mapM

-- | 'forM_' is 'mapM_' with its arguments flipped
forM_           :: Monad m => [a] -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_           = flip mapM_

-- | @'forever' act@ repeats the action infinitely.
forever     :: (Monad m) => m a -> m b
{-# INLINE forever #-}
forever a   = let a' = a >> a' in a'
-- Use explicit sharing here, as it is prevents a space leak regardless of
-- optimizations.

-- | The 'join' function is the conventional monad join operator. It is used to
-- remove one level of monadic structure, projecting its bound argument into the
-- outer level.
join              :: (Monad m) => m (m a) -> m a
join x            =  x >>= id

{- | In many situations, the 'liftM' operations can be replaced by uses of
'ap', which promotes function application.

>       return f `ap` x1 `ap` ... `ap` xn

is equivalent to

>       liftMn f x1 x2 ... xn

-}
ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap                =  liftM2 id

-- | This generalizes the list-based 'filter' function.
filterM          :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ []     =  return []
filterM p (x:xs) =
   p x           >>= \flg ->
   filterM p xs  >>= \ys ->
   return (if flg then x:ys else ys)

{- | The 'foldM' function is analogous to 'foldl', except that its result is
encapsulated in a monad. Note that 'foldM' works from left-to-right over
the list arguments. This could be an issue where @('>>')@ and the `folded
function' are not commutative.


>       foldM f a1 [x1, x2, ..., xm]

@==@

>       do
>         a2 <- f a1 x1
>         a3 <- f a2 x2
>         ...
>         f am xm

If right-to-left evaluation is required, the input list should be reversed.
-}

foldM             :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
{-# INLINEABLE foldM #-}
{-# SPECIALISE foldM :: (a -> b -> IO a) -> a -> [b] -> IO a #-}
{-# SPECIALISE foldM :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a #-}
foldM _ a []      =  return a
foldM f a (x:xs)  =  f a x >>= \fax -> foldM f fax xs

-- | Like 'foldM', but discards the result.
foldM_            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
{-# INLINEABLE foldM_ #-}
{-# SPECIALISE foldM_ :: (a -> b -> IO a) -> a -> [b] -> IO () #-}
{-# SPECIALISE foldM_ :: (a -> b -> Maybe a) -> a -> [b] -> Maybe () #-}
foldM_ f a xs     = foldM f a xs >> return ()




-- | Conditional execution of monadic expressions. For example,
--
-- > when debug (putStr "Debugging\n")
--
-- will output the string @Debugging\\n@ if the Boolean value @debug@
-- is 'True', and otherwise do nothing.
when              :: (Monad m) => Bool -> m () -> m ()
{-# INLINEABLE when #-}
{-# SPECIALISE when :: Bool -> IO () -> IO () #-}
{-# SPECIALISE when :: Bool -> Maybe () -> Maybe () #-}
when p s          =  if p then s else return ()

-- | The reverse of 'when'.
unless            :: (Monad m) => Bool -> m () -> m ()
{-# INLINEABLE unless #-}
{-# SPECIALISE unless :: Bool -> IO () -> IO () #-}
{-# SPECIALISE unless :: Bool -> Maybe () -> Maybe () #-}
unless p s        =  if p then return () else s

-- | Promote a function to a monad.
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1 = m1 >>= \x1 -> return (f x1)

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right.  For example,
--
-- >    liftM2 (+) [0,1] [0,2] = [0,2,1,3]
-- >    liftM2 (+) (Just 1) Nothing = Nothing
--
liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = m1 >>= \x1 ->
                 m2 >>= \x2 ->
                 return (f x1 x2)
                 -- do { x1 <- m1; x2 <- m2; return (f x1 x2) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM3  :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f m1 m2 m3 =
    m1 >>= \x1 ->
    m2 >>= \x2 ->
    m3 >>= \x3 ->
    return (f x1 x2 x3)
    -- do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM4  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM4 f m1 m2 m3 m4 =
    m1 >>= \x1 ->
    m2 >>= \x2 ->
    m3 >>= \x3 ->
    m4 >>= \x4 ->
    return (f x1 x2 x3 x4)
    -- do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM5  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
liftM5 f m1 m2 m3 m4 m5 =
    m1 >>= \x1 ->
    m2 >>= \x2 ->
    m3 >>= \x3 ->
    m4 >>= \x4 ->
    m5 >>= \x5 ->
    return (f x1 x2 x3 x4 x5)
    -- do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }


-- | The 'mapAndUnzipM' function maps its first argument over a list, returning
-- the result as a pair of lists. This function is mainly used with complicated
-- data structures or a state-transforming monad.
mapAndUnzipM      :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
{-# INLINE mapAndUnzipM #-}
mapAndUnzipM f xs =  sequence (map f xs) >>= return . unzip

-- | The 'zipWithM' function generalizes 'zipWith' to arbitrary monads.
zipWithM          :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
{-# INLINE zipWithM #-}
zipWithM f xs ys  =  sequence (zipWith f xs ys)

-- | 'zipWithM_' is the extension of 'zipWithM' which ignores the final result.
zipWithM_         :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f xs ys =  sequence_ (zipWith f xs ys)

-- | @'replicateM' n act@ performs the action @n@ times,
-- gathering the results.
replicateM        :: (Monad m) => Int -> m a -> m [a]
{-# INLINEABLE replicateM #-}
{-# SPECIALISE replicateM :: Int -> IO a -> IO [a] #-}
{-# SPECIALISE replicateM :: Int -> Maybe a -> Maybe [a] #-}
replicateM n x    = sequence (replicate n x)

-- | Like 'replicateM', but discards the result.
replicateM_       :: (Monad m) => Int -> m a -> m ()
{-# INLINEABLE replicateM_ #-}
{-# SPECIALISE replicateM_ :: Int -> IO a -> IO () #-}
{-# SPECIALISE replicateM_ :: Int -> Maybe a -> Maybe () #-}
replicateM_ n x   = sequence_ (replicate n x)

{- $naming

The functions in this library use the following naming conventions:

* A postfix \'@M@\' always stands for a function in the Kleisli category:
  The monad type constructor @m@ is added to function results
  (modulo currying) and nowhere else.  So, for example,

>  filter  ::              (a ->   Bool) -> [a] ->   [a]
>  filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

* A postfix \'@_@\' changes the result type from @(m a)@ to @(m ())@.
  Thus, for example:

>  sequence  :: Monad m => [m a] -> m [a]
>  sequence_ :: Monad m => [m a] -> m ()

* A prefix \'@m@\' generalizes an existing function to a monadic form.
  Thus, for example:

>  sum  :: Num a       => [a]   -> a
>  msum :: MonadPlus m => [m a] -> m a

-}
