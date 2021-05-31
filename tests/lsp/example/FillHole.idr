
import Data.Maybe
import Data.List

data NoParam
  = NPCons1
  | NPCons2 Nat

noParam : NoParam
noParam = ?test1

-- SCENARIO
-- WHEN: Code action on ?test1
-- THEN: Code action contains:
--       - 'Fill Hole ~ NPCons1'
--       - 'Fill Hole ~ (NDCons2 ?test1_1)'

data WithParam a
  = WPCons1
  | WPCons2 a Nat a

withParam : WithParam Nat
withParam = ?test2

-- SCENARIO
-- WHEN: Code action on ?test2
-- THEN: Code action contains:
--       - 'Fill Hole ~ WPCons1'
--       - 'Fill Hole ~ (WPCons2 ?test2_1 ?test2_2 ?test2_3)

similarNames : Nat
similarNames = ?fromM (Just 0)

-- SCENARIO
-- WHEN: Code action on ?fromM
-- THEN: Code action contains:
--       - 'Fill Hole ~ fromMaybe'

record Record a where
  constructor MkRecord
  field1 : a
  field2 : String

someRecord : Record Int
someRecord = ?test3

-- SCENARIO
-- WHEN: Code action on ?test3
-- THEN: Code action result contains:
--       - 'Fill Hole ~ (MkRecord ?test3_1 ?test3_2)
