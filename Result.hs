module Result
(
    Result(Failure, Pass),
) where

import Error
import Bytecode

-- Result type used in several parts of the compiler. It is parameterized by
-- 3 types:
--   * state represents the current state of a compiler pass (parsing or code 
--     generation)
--   * errs represents information about one or several errors
--   * res represents the result of a successful compilation
-- Both constructors carry the state so that in some cases, the pass can
-- continue without a result. This allows to detect several errors at once.

data Result errs res state = Failure errs state | Pass res state

-- Class a type must implement to represent an error when used with Result.
-- append take 2 error descriptors, and return an object describing both errors
class Errors e where
    append :: e -> e -> e

-- Class a type must implement to represent a result when used with Result
-- combine take 2 results, and return the result of their combination
class ResultType t where
    combine :: t -> t -> t
    new :: t

-- Allow to continue the current pass using monadic bind on the Result type.
-- in each constructor, the state is available so we can continue the pass.
-- If either of the operands result in a Failure, this results in a Failure
-- that describes the error, both errors, if both fail.
-- If both of the operands succeed, then this results in a result combining
-- the results of the operands.

instance (Errors errs, ResultType res) => Monad (Result errs res) where
    Failure errs st >>= next =
        let cont = next st in
        case cont of
            Failure e s -> Failure (append errs e) s
            Pass _ s -> Failure errs s

    Pass instrs st >>= next =
        let cont = next st in
        case cont of
            Failure errs s -> cont
            Pass i s -> Pass (combine instrs i) s

    return st = Pass new st

-- Allow use of the Result type with most-used compiler types

instance Errors [e] where
    append = (++)

instance ResultType [t] where
    combine = (++)
    new = []
