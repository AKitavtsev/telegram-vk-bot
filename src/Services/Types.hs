module Services.Types
  ( Priority(..)
  ) where

--
data Priority
      -- | Debug messages
  = DEBUG
      -- | Notable information that requires no immediate action.
  | INFO
      -- | Something is probably wrong, and we should investigate.
  | WARN
      -- | Something is wrong and immediate action is required.
  | ERROR
  deriving (Eq, Ord, Show)
