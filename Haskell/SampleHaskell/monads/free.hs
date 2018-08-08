{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Free
import Control.Monad.Free.TH

data Toy b next =
    Output b next
  | Bell next
  | Done
  deriving (Functor)
  
{-
instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done
-}

{-
output :: a -> Free (Toy a) ()
-- output x = Free (Output x (Pure ()))
output x = liftF (Output x ())

bell :: Free (Toy a) ()
-- bell = Free (Bell (Pure ()))
bell     = liftF (Bell     ())

done :: Free (Toy a) r
-- done = Free Done
done     = liftF  Done
-}

makeFree ''Toy

subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: Free (Toy Char) r
program = do
    subroutine
    bell
    done

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) =
    "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) =
    "bell\n" ++ showProgram x
showProgram (Free Done) =
    "done\n"
showProgram (Pure r) =
    "return " ++ show r ++ "\n"

pretty :: (Show a, Show r) => Free (Toy a) r -> IO ()
pretty = putStr . showProgram

interpret :: (Show b) => Free (Toy b) r -> IO ()
interpret (Free (Output b x)) = print b  >> interpret x
interpret (Free (Bell     x)) = print "bell" >> interpret x
interpret (Free  Done       ) = return ()
interpret (Pure r) = return ()

{-
*Main> putStr (showProgram program)
output 'A'
bell
done

*Main> pretty (output 'A')
output 'A'
return ()

*Main> pretty (return 'A' >>= output)
output 'A'
return ()

*Main> pretty (output 'A' >>= return)
output 'A'
return ()

*Main> pretty ((output 'A' >> done) >> output 'C')
output 'A'
done

*Main> pretty (output 'A' >> (done >> output 'C'))
output 'A'
done

*Main> interpret program
'A'
"bell"
-}