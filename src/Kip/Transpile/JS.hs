{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | JavaScript-ish transpiler for Kip AST.
module Kip.Transpile.JS
  ( transpileProgram
  , transpileStmts
  , transpileStmt
  , transpileExp
  ) where

import Data.Char (isLetter)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T

import Kip.AST

-- | Transpile a list of statements into a JS-like program.
transpileProgram :: [Stmt Ann] -> Text
transpileProgram stmts =
  T.intercalate "\n\n"
    (jsPrelude : map transpileStmt stmts)

-- | Transpile a list of statements (no prelude).
transpileStmts :: [Stmt Ann] -> Text
transpileStmts = T.intercalate "\n\n" . map transpileStmt

-- | Transpile a single statement into JS-like code.
transpileStmt :: Stmt Ann -> Text
transpileStmt stmt =
  case stmt of
    Defn name _ exp' ->
      "const " <> toJsIdent name <> " = " <> transpileExp exp' <> ";"
    Function name args _ clauses _ ->
      renderFunction name args clauses
    PrimFunc name args _ _ ->
      "// primitive function " <> identText name <> "(" <> renderArgNames args <> ")"
    Load name ->
      "// load " <> identText name
    NewType name _ ctors ->
      renderNewType name ctors
    PrimType name ->
      "// primitive type " <> identText name
    ExpStmt exp' ->
      transpileExp exp' <> ";"

-- | Transpile an expression into a JS-like expression string.
transpileExp :: Exp Ann -> Text
transpileExp exp' =
  case exp' of
    Var {varName} ->
      toJsIdent varName
    StrLit {lit} ->
      renderString lit
    IntLit {intVal} ->
      T.pack (show intVal)
    App {fn, args} ->
      renderCall fn args
    Bind {bindName, bindExp} ->
      renderIife
        [ "const " <> toJsIdent bindName <> " = " <> transpileExp bindExp <> ";"
        , "return " <> toJsIdent bindName <> ";"
        ]
    Seq {first, second} ->
      renderIife
        (renderExpAsStmt first ++ ["return " <> transpileExp second <> ";"])
    Match {scrutinee, clauses} ->
      renderMatch scrutinee clauses
    Let {body} ->
      transpileExp body

renderFunction :: Identifier -> [Arg Ann] -> [Clause Ann] -> Text
renderFunction name args clauses =
  let argsText = renderArgNames args
      bodyLines =
        case clauses of
          [Clause PWildcard body] ->
            ["return " <> transpileExp body <> ";"]
          _ ->
            let arg0 = case args of
                         [] -> "__arg0"
                         ((argName, _) : _) -> toJsIdent argName
            in ("const __scrut = " <> arg0 <> ";")
               : renderClauseChain "__scrut" clauses
  in
    T.unlines
      [ "function " <> toJsIdent name <> "(" <> argsText <> ") {"
      , indent 2 (T.unlines bodyLines)
      , "}"
      ]

renderClauseChain :: Text -> [Clause Ann] -> [Text]
renderClauseChain scrutinee clauses =
  case clauses of
    [] -> ["throw new Error(\"No matching clause\");"]
    [Clause PWildcard body] ->
      ["return " <> transpileExp body <> ";"]
    (Clause pat body) : rest ->
      let (cond, binds) = renderPatMatch scrutinee pat
          header =
            if T.null cond
              then "if (true) {"
              else "if (" <> cond <> ") {"
          bodyLines =
            binds ++ ["return " <> transpileExp body <> ";"]
      in
        [ header
        , indent 2 (T.unlines bodyLines)
        , "}"
        ] ++
        case rest of
          [] -> []
          _ ->
            [ "else {"
            , indent 2 (T.unlines (renderClauseChain scrutinee rest))
            , "}"
            ]

renderMatch :: Exp Ann -> [Clause Ann] -> Text
renderMatch scrutinee clauses =
  renderIife $
    ("const __scrut = " <> transpileExp scrutinee <> ";")
      : renderMatchClauses "__scrut" clauses

renderMatchClauses :: Text -> [Clause Ann] -> [Text]
renderMatchClauses scrutinee clauses =
  case clauses of
    [] -> ["throw new Error(\"No match\");"]
    [Clause PWildcard body] ->
      ["return " <> transpileExp body <> ";"]
    (Clause pat body) : rest ->
      let (cond, binds) = renderPatMatch scrutinee pat
          header =
            if T.null cond
              then "if (true) {"
              else "if (" <> cond <> ") {"
          bodyLines =
            binds ++ ["return " <> transpileExp body <> ";"]
      in
        [ header
        , indent 2 (T.unlines bodyLines)
        , "}"
        ] ++
        case rest of
          [] ->
            ["throw new Error(\"No match\");"]
          _ ->
            [ "else {"
            , indent 2 (T.unlines (renderMatchClauses scrutinee rest))
            , "}"
            ]

renderPatMatch :: Text -> Pat Ann -> (Text, [Text])
renderPatMatch _ PWildcard = ("", [])
renderPatMatch scrutinee (PCtor ctor vars) =
  let cond = "__kipMatch(" <> scrutinee <> ", " <> renderString (identText ctor) <> ")"
      binds =
        case vars of
          [] -> []
          _ ->
            [ "const [" <> T.intercalate ", " (map (toJsIdent . fst) vars) <> "] = "
                <> scrutinee <> ".args || [];"]
  in (cond, binds)

renderNewType :: Identifier -> [Ctor Ann] -> Text
renderNewType name ctors =
  let ctorLines =
        [ "const " <> toJsIdent ctorName <> " = (...args) => ({ tag: "
            <> renderString (identText ctorName) <> ", args });"
        | (ctorName, _) <- ctors
        ]
      ctorSig =
        T.intercalate " | "
          [ identText ctorName <> "(" <> T.replicate (length args) "_" <> ")"
          | (ctorName, args) <- ctors
          ]
  in
    T.unlines $
      ("/* type " <> identText name <> " = " <> ctorSig <> " */")
        : ctorLines

renderCall :: Exp Ann -> [Exp Ann] -> Text
renderCall fn args =
  let fnText =
        case fn of
          Var {} -> transpileExp fn
          _ -> "(" <> transpileExp fn <> ")"
  in fnText <> "(" <> T.intercalate ", " (map transpileExp args) <> ")"

renderExpAsStmt :: Exp Ann -> [Text]
renderExpAsStmt exp' =
  case exp' of
    Bind {bindName, bindExp} ->
      ["const " <> toJsIdent bindName <> " = " <> transpileExp bindExp <> ";"]
    _ ->
      [transpileExp exp' <> ";"]

renderIife :: [Text] -> Text
renderIife lines' =
  T.unlines
    [ "(() => {"
    , indent 2 (T.unlines lines')
    , "})()"
    ]

renderArgNames :: [Arg Ann] -> Text
renderArgNames args =
  T.intercalate ", " (map (toJsIdent . fst) args)

identText :: Identifier -> Text
identText (_, name) = name

toJsIdent :: Identifier -> Text
toJsIdent ident =
  let raw = baseIdent ident
      sanitized = T.map replaceDash raw
      prefixed =
        case T.uncons sanitized of
          Nothing -> "_"
          Just (c, _) ->
            if isIdentStart c then sanitized else "_" <> sanitized
      safe =
        if prefixed `elem` jsReserved
          then "_" <> prefixed
          else prefixed
  in safe
  where
    baseIdent (_, name) = T.filter (/= ' ') name
    replaceDash c = if c == '-' then '_' else c
    isIdentStart c = isLetter c || c == '_' || c == '$'

jsReserved :: [Text]
jsReserved =
  [ "break", "case", "catch", "class", "const", "continue", "debugger"
  , "default", "delete", "do", "else", "export", "extends", "false"
  , "finally", "for", "function", "if", "import", "in", "instanceof"
  , "new", "null", "return", "super", "switch", "this", "throw"
  , "true", "try", "typeof", "var", "void", "while", "with", "yield"
  , "let", "enum", "await", "implements", "interface", "package"
  , "private", "protected", "public", "static", "undefined"
  ]

renderString :: Text -> Text
renderString txt =
  "\"" <> T.concatMap escapeChar txt <> "\""

escapeChar :: Char -> Text
escapeChar c =
  case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> T.singleton c

indent :: Int -> Text -> Text
indent n =
  T.unlines . map (T.replicate n " " <>) . filter (not . T.null) . T.lines

jsPrelude :: Text
jsPrelude =
  T.unlines
    [ "// Kip → JavaScript (readability-focused output)"
    , "const __kipMatch = (value, tag) => value && value.tag === tag;"
    ]
