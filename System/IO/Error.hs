module System.IO.Error (
      -- * I\/O errors
    IOError,                    -- = IOException

    userError,                  -- :: String  -> IOError

    mkIOError,                  -- :: IOErrorType -> String -> Maybe Handle
                                --    -> Maybe FilePath -> IOError

    annotateIOError,            -- :: IOError -> String -> Maybe Handle
                                --    -> Maybe FilePath -> IOError

    -- ** Classifying I\/O errors
    isAlreadyExistsError,       -- :: IOError -> Bool
    isDoesNotExistError,
    isAlreadyInUseError,
    isFullError, 
    isEOFError,
    isIllegalOperation, 
    isPermissionError,
    isUserError,

    -- ** Attributes of I\/O errors
    ioeGetErrorString,          -- :: IOError -> String
    ioeGetHandle,               -- :: IOError -> Maybe Handle
    ioeGetFileName,             -- :: IOError -> Maybe FilePath

    -- * Types of I\/O error
    IOErrorType,                -- abstract

    alreadyExistsErrorType,     -- :: IOErrorType
    doesNotExistErrorType,
    alreadyInUseErrorType,
    fullErrorType,
    eofErrorType,
    illegalOperationErrorType, 
    permissionErrorType,
    userErrorType
  ) where

import "base" System.IO.Error hiding (IOError)
import qualified "base" System.IO.Error as Base
import Prelude hiding (IOError)

-- | Errors of type 'IOError' are used by the 'IO' monad.  This is an
-- abstract type; the module "System.IO.Error" provides functions to
-- interrogate and construct values of type 'IOError'.
type IOError = Base.IOError
