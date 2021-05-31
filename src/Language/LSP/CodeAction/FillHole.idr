module Language.LSP.CodeAction.FillHole

import Core.Context
import Core.Core
import Core.Env
import Core.Value
import Core.Metadata
import Core.UnifyState
import Language.JSON
import Language.LSP.CodeAction
import Language.LSP.Message
import Libraries.Data.PosMap
import Server.Configuration
import Server.Response
import Server.Utils
import Server.Log
import Idris.Syntax
import Idris.REPL.Opts
import Data.String

import Data.Nat
import Libraries.Data.NameMap

-- CodeAction for filling holes, not necessarily type correct constructions.
-- This code action does three things:
-- * Tries to determine the type of the hole and offers valid constructors with new holes for parameters.
--   Poor man's hole refining.
-- * Tries to find similar names from the context.
--   In many cases I find myself writing out the hole name somewhat similar to final function I want to
--   call later on, in the given hole.
-- * Tries to find similar names which have the same prefix of the given hole.
--   Same as above.


||| Tries to resolve a type where Like Nat, Maybe
||| It ignores the type parameters
resolveTypeTerm
  :  Ref Ctxt Defs
  => Ref LSPConf LSPConfiguration
  => {vars : _}
  -> Term vars
  -> Core (Maybe Name)
resolveTypeTerm (Ref fc x name)     = Just <$> toFullNames name
resolveTypeTerm (Bind fc x b scope) = resolveTypeTerm scope -- ignores explicit/implicit parameters: {a} -> DataType
resolveTypeTerm (App fc fn arg)     = resolveTypeTerm fn    -- ignores type parameters: DataType a
resolveTypeTerm other = do
  logString Debug "resolveTypeTerm: \{show other}"
  pure Nothing

constructors : Ref LSPConf LSPConfiguration => Ref Ctxt Defs => Name -> Core (Maybe (List (Name, Nat)))
constructors n = do
  defs <- get Ctxt
  Just gdef <- lookupDefExact !(toFullNames n) defs.gamma
    | Nothing => do
        pure Nothing
  let (TCon tag arity parampos detpos flags mutwith datacons detagabbleBy) = gdef
    | _ => do
        pure Nothing
  logString Debug "fillHole: constructors datacons: \{show datacons}" 
  -- Possible constructor informations, is something is not a constructor (for some reason)
  -- it is marked as Nothing
  mConsInfo
    <- traverse
        (\consName => do
          rConName <- toFullNames consName
          gConDef <- lookupCtxtExact rConName defs.gamma
          case gConDef of
            Nothing => pure Nothing
            Just gd => pure $ case gd.definition of
              DCon tag arity newtypeArg =>
                let arity' = minus (minus arity (length gd.inferrable)) (length gd.eraseArgs)
                in Just (rConName, arity')
              other => Nothing)
        datacons
  -- If all are constructors we are OK
  pure $ sequence mConsInfo

generateHoleName : Ref Ctxt Defs => String -> Nat -> Core (String, Nat)
generateHoleName holeName k = do
  defs <- get Ctxt
  let n = holeName ++ "_" ++ show k
  case !(lookupCtxtName (UN n) defs.gamma) of
    [] => pure (n, k+1)
    _ => generateHoleName holeName (k + 1)

generateHoleNames : Ref Ctxt Defs => String -> Nat -> (Nat, List String) -> Core (List String)
generateHoleNames baseName 0     (next, res) = pure (reverse res)
generateHoleNames baseName (S k) (next, res) = do
  (holeName, next') <- generateHoleName baseName next
  generateHoleNames baseName k (next',("?" ++ holeName) :: res)

renderConstructor : Ref Ctxt Defs => Name -> (Name, Nat) -> Core (Maybe String)
renderConstructor holeName (fullName, arity) = do
  let shortName = show $ dropAllNS fullName
  case arity of
    0 => pure $ Just shortName
    n => do
      let Just holeName' = userNameRoot holeName
          | Nothing => pure Nothing
      newHoles <- generateHoleNames holeName' arity (1,[])
      pure $ Just $ "(" ++ shortName ++ " " ++ unwords newHoles ++ ")"

fcToRange : FC -> Maybe Range
fcToRange (MkFC x (sline,scol) (eline, ecol))
  = Just (MkRange (MkPosition sline scol)
                  (MkPosition eline ecol))
fcToRange (MkVirtualFC x (sline,scol) (eline, ecol))
  = Just (MkRange (MkPosition sline scol)
                  (MkPosition eline ecol))
fcToRange EmptyFC = Nothing

fillHoleWith : FC -> CodeActionParams -> String -> CodeAction
fillHoleWith holeLoc params constructorString =
  MkCodeAction
    { title       = "Fill Hole ~ \{strSubstr 0 50 constructorString}"
    , kind        = Just RefactorRewrite
    , diagnostics = Just []
    , isPreferred = Just False -- not a quickfix
    , disabled    = Nothing
    , edit        = Just $ MkWorkspaceEdit
        { changes           = Just (singleton params.textDocument.uri
            [ MkTextEdit range constructorString ])
        , documentChanges   = Nothing
        , changeAnnotations = Nothing
        }
    , command     = Nothing
    , data_       = Nothing
    }
  where
    range : Range
    range = fromMaybe params.range $ fcToRange holeLoc 

isHole : Defs -> Name -> Core Bool
isHole defs n = do
  Just def <- lookupCtxtExact n (gamma defs)
    | Nothing => do pure False
  pure $ case definition def of
    Hole _ _ => True
    _        => False

||| Keeps the name with same preffix as the hole, ignoring namespaces.
namesWithPrefix : Ref Ctxt Defs => Name -> Core (List String)
namesWithPrefix holeName = do
  let Just shortName = userNameRoot holeName
      | Nothing => pure []
  defs <- get Ctxt
  pure
    $ mapMaybe (\n =>
            case userNameRoot n of
              Nothing => Nothing
              Just sn => if (shortName /= sn && isPrefixOf shortName sn)
                then Just sn
                else Nothing)
    $ keys
    $ namesResolvedAs defs.gamma

export
filleHoleWithConstructor
  :  Ref LSPConf LSPConfiguration
  => Ref MD Metadata
  => Ref Ctxt Defs
  => Ref UST UState
  => Ref Syn SyntaxInfo
  => Ref ROpts REPLOpts
  => CodeActionParams -> Core (List CodeAction)
filleHoleWithConstructor params = do
  let True = params.range.start.line == params.range.end.line
    | _ => pure []

  [] <- searchCache params.range FillHole
    | actions => do logString Debug "fillHole: found cached action"
                    pure actions

  meta <- get MD
  let line = params.range.start.line
  let col = params.range.start.character
  let Just (loc, name) = findPointInTreeLoc (line, col) (nameLocMap meta)
    | Nothing =>
        do logString Debug $
             "fillHole: couldn't find name in tree for position (\{show line},\{show col})"
           pure []

  -- The name is a hole
  defs <- get Ctxt
  let True = !(isHole defs name) -- should only work on holes
    | _ => do logString Debug $ "fillHole: \(show name) is not a hole"
              pure []

  -- Find the type information at this hole
  [(n,(x, global))] <- lookupCtxtName name (gamma defs)
    | _ => do
      logString Debug "fillHole: Non-unique context entries for \{show name}"
      pure []
  
  -- Try to find some constructors
  renderedConstructorStrings <- 
    -- If the type is a datatype find its definition
    case !(resolveTypeTerm global.type) of
      Nothing => pure []
      Just dataTypeName =>
        -- Find its constructors
        case !(constructors dataTypeName) of
          Nothing => pure []
          Just [] => pure []
          Just cns => do
            -- Render the (Constructor ?field1 ?field2) like fields
            someConstructorStrings <- traverse (renderConstructor name) cns
            case the (Maybe (List String)) (sequence someConstructorStrings) of
              Nothing => pure []
              Just cs => pure cs

  -- Find similar names  
  similarNames <- getSimilarNames name
  prefixNames <- namesWithPrefix name

  -- Limit the results
  cfg <- get LSPConf
  let limit = cfg.searchLimit

  -- Render code actions that inject constructors or similar names
  let fillerStrings = nub (renderedConstructorStrings ++ take limit prefixNames ++ take limit similarNames)
  let fillers = map (fillHoleWith global.location params) fillerStrings

  -- Cache the fillers
  modify LSPConf (record { cachedActions $= insert (cast loc, FillHole, fillers) })

  pure fillers
