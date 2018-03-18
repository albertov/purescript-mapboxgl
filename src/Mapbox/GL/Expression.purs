module Mapbox.GL.Expression where

import Prelude

import Control.Alt ((<|>))
import Data.Array (concatMap, fromFoldable, uncons, unsnoc)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError(ForeignError), F, fail, toForeign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Mapbox.Common (class ToValue, Value, pairs, toValue)

data ExpType = StringT | NumberT | BooleanT

instance encodeExpType :: Encode ExpType where
  encode StringT  = toForeign "string"
  encode NumberT  = toForeign "number"
  encode BooleanT = toForeign "boolean"

instance decodeExpType :: Decode ExpType where
  decode s = decode s >>= \s' -> case s' of
    "string" -> pure StringT
    "number" -> pure NumberT
    "boolean" -> pure BooleanT
    o -> fail (ForeignError ("Invalid expression type: " <> o))




data ArrayCheck = ArrayCheck ExpType (Maybe Int)

lit :: forall a. ToValue a => a -> Expr
lit = Const <<< toValue

data Expr
  = Const      Value -- This node does not exist in the official AST but we need
                     -- it to inject constants into Exprs
  | ArrayExp   Expr (Maybe ArrayCheck)
  | BooleanExp Expr (Array Expr)
  | LiteralExp (Either (StrMap Value) (Array Value))
  | NumberExp   Expr (Array Expr)
  | ObjectExp   Expr (Array Expr)
  | StringExp   Expr (Array Expr)
  | ToBoolean   Expr
  | ToColor     Expr (Array Expr)
  | ToNumber    Expr (Array Expr)
  | ToString    Expr
  | TypeOf      Expr
  | GeometryType
  | Id
  | Properties
  | At            Int Expr
  | Get           String (Maybe Expr)
  | Has           String (Maybe Expr)
  | Length        Expr
  | Not           Expr
  | NotEq         Expr Expr
  | LessThan      Expr Expr
  | LessThanEq    Expr Expr
  | Equal         Expr Expr
  | GreaterThan   Expr Expr
  | GreaterThanEq Expr Expr
  | All           Expr Expr (Array Expr)
  | Any           Expr Expr (Array Expr)
  | In            Expr Expr (Array Expr) --Not defined in the spec but found in the wild
  | Case          (Array (Tuple Expr Expr)) Expr
  | Coalesce      Expr Expr (Array Expr)
  | Match         Expr (Array (Tuple (NonEmpty Array Expr) Expr)) Expr
  | Interpolate   InterpolationType Expr (Array (Tuple Number Expr))
  | Step          Expr Expr (Array (Tuple Number Expr))
  | Let           (Array (Tuple String Expr)) Expr
  | Var           String
  | Concat        Expr Expr (Array Expr)
  | Downcase      Expr
  | Upcase        Expr
  | Rgb           Expr Expr Expr
  | Rgba          Expr Expr Expr Expr
  | ToRgba        Expr
  | Minus         Expr Expr
  | Mult          Expr Expr
  | Div           Expr Expr
  | Mod           Expr Expr
  | Exp           Expr Expr
  | Plus          Expr Expr
  | Acos          Expr
  | Asin          Expr
  | Atan          Expr
  | Cos           Expr
  | E
  | Ln            Expr
  | Ln2           Expr
  | Log10         Expr
  | Log2          Expr
  | Max           Expr Expr (Array Expr)
  | Min           Expr Expr (Array Expr)
  | Pi
  | Sin           Expr
  | Sqrt          Expr
  | Tan           Expr
  | Zoom
  | HeatmapDensity


instance encodeExpr :: Encode Expr where
  encode (Const v) = encode v
  encode (ArrayExp e Nothing) = unOp "array" e
  encode (ArrayExp e (Just (ArrayCheck t Nothing))) = toForeign
    [ toForeign "array", encode e, encode t]
  encode (ArrayExp e (Just (ArrayCheck t (Just l)))) = toForeign
    [ toForeign "array", encode e, encode t, encode l]
  encode (BooleanExp e es) = unOpArgs "boolean" e es
  encode (LiteralExp (Left v)) = toForeign [toForeign "literal", encode v]
  encode (LiteralExp (Right v)) = toForeign [toForeign "literal", encode v]
  encode (NumberExp e es) = unOpArgs "number" e es
  encode (ObjectExp e es) = unOpArgs "object" e es
  encode (StringExp e es) = unOpArgs "string" e es
  encode (ToBoolean e) = unOp "to-boolean" e
  encode (ToColor e es) = unOpArgs "to-color" e es
  encode (ToNumber e es) = unOpArgs "to-number" e es
  encode (ToString e) = unOp "to-string" e
  encode (TypeOf e) = unOp "typeof" e
  encode GeometryType = constant "geometry-type"
  encode Id = constant "id"
  encode Properties = constant "properties"
  encode (At i e) = toForeign [toForeign "at", toForeign i, encode e]
  encode (Get s Nothing) = toForeign ["get", s]
  encode (Get s (Just e)) = toForeign [toForeign "get", toForeign s, encode e]
  encode (Has s Nothing) = toForeign ["has", s]
  encode (Has s (Just e)) = toForeign [toForeign "has", toForeign s, encode e]
  encode (Length e) = unOp "length" e
  encode (Not e) = unOp "!" e
  encode (NotEq a b) = binOp "!=" a b
  encode (LessThan a b) = binOp "<" a b
  encode (LessThanEq a b) = binOp "<=" a b
  encode (Equal a b) = binOp "==" a b
  encode (GreaterThan a b) = binOp ">" a b
  encode (GreaterThanEq a b) = binOp ">=" a b
  encode (All a b xs) = binOpArgs "all" a b xs
  encode (Any a b xs) = binOpArgs "any" a b xs
  encode (In a b xs) = binOpArgs "in" a b xs
  encode (Case cs d) = toForeign  ([toForeign "case"] <> concatMap (\(Tuple c v) -> [encode c, encode v]) cs <> [encode d])
  encode (Coalesce a b xs) = toForeign  ([toForeign "coalesce", encode a, encode b] <> map encode xs)
  encode (Match i cs d) = toForeign  ([toForeign "match", encode i] <> concatMap encodeMatchCase cs <> [encode d])
    where encodeMatchCase (Tuple (NonEmpty x []) v) = [encode x, encode v]
          encodeMatchCase (Tuple labels v) = [encode (fromFoldable labels) , encode v]
  encode (Interpolate t e stops) =
    toForeign (   [toForeign "interpolate", encode t, encode e]
               <> concatMap (\(Tuple a b) -> [encode a, encode b]) stops)
  encode (Step i d steps) = toForeign  ([toForeign "step", encode i] <> concatMap (\(Tuple i o) -> [toForeign i, encode o]) steps <> [encode d])
  encode (Let xs e) = toForeign  ([toForeign "tag"] <> concatMap (\(Tuple k v) -> [toForeign k, encode v]) xs <> [encode e])
  encode (Var s) = toForeign  ["var", s]
  encode (Concat a b xs) = binOpArgs "concat" a b xs
  encode (Downcase a) = unOp "downcase" a
  encode (Upcase a) = unOp "upcase" a
  encode (Rgb r g b) = toForeign  [toForeign "rgb", encode r, encode g, encode b]
  encode (Rgba r g b a) = toForeign  [toForeign "rgba", encode r, encode g, encode b, encode a]
  encode (ToRgba v) = unOp "to-rgba" v
  encode (Minus a b) = binOp "-" a b
  encode (Mult a b) = binOp "*" a b
  encode (Div a b) = binOp "/" a b
  encode (Mod a b) = binOp "%" a b
  encode (Exp a b) = binOp "^" a b
  encode (Plus a b) = binOp "+" a b
  encode (Acos x) = unOp "acos" x
  encode (Asin x) = unOp "asin" x
  encode (Atan x) = unOp "atan" x
  encode (Cos x) = unOp "cos" x
  encode E = constant "e"
  encode (Ln x) = unOp "ln" x
  encode (Ln2 x) = unOp "ln2" x
  encode (Log10 x) = unOp "log10" x
  encode (Log2 x) = unOp "log2" x
  encode (Max a b xs) = binOpArgs "max" a b xs
  encode (Min a b xs) = binOpArgs "min" a b xs
  encode Pi = constant "pi"
  encode (Sin x) = unOp "sin" x
  encode (Sqrt x) = unOp "sqrt" x
  encode (Tan x) =  unOp "tan" x
  encode Zoom = constant "zoom"
  encode HeatmapDensity = constant "heatmap-density"

instance decodeExpr :: Decode Expr where
  decode v = decoded <|> Const <$> decode v
    where
    decoded = decode v >>= \a ->
      case uncons a of
        Just {head,tail} -> do
          tag <- decode head
          go tag tail
        Nothing ->
          fail (ForeignError "empty Expr")

    go :: String -> Array Foreign -> F Expr
    go "array" [e] = ArrayExp <$> decode e <*> pure Nothing
    go "array" [ty, e] = ArrayExp <$> decode e <*> (Just <$> (ArrayCheck <$> decode ty <*> pure Nothing))
    go "array" [ty, l, e] =
      ArrayExp <$> decode e <*> (Just <$> (ArrayCheck <$> decode ty <*> (Just <$> decode l)))
    go "boolean" xs = decodeUnOpArgs "boolean" BooleanExp xs
    go "literal" [x] = LiteralExp <$> (Left <$> decode x <|> Right <$> decode x)
    go "number" xs = decodeUnOpArgs "number" NumberExp xs
    go "object" xs = decodeUnOpArgs "object" ObjectExp xs
    go "string" xs = decodeUnOpArgs "string" StringExp xs
    go "to-boolean" xs = decodeUnOp "to-boolean" ToBoolean xs
    go "to-color" xs = decodeUnOpArgs "to-color" ToColor xs
    go "to-number" xs = decodeUnOpArgs "to-number" ToNumber xs
    go "to-string" xs = decodeUnOp "to-string" ToString xs
    go "typeof" xs = decodeUnOp "typeof" TypeOf xs
    go "geometry-type" xs = decodeConstant "geometry-type" GeometryType xs
    go "id" xs = decodeConstant "id" Id xs
    go "properties" xs = decodeConstant "properties" Properties xs
    go "at" [pos, arr] = At <$> decode pos <*> decode arr
    go "get" [prop] = Get <$> decode prop <*> pure Nothing
    go "get" [prop, ob] = Get <$> decode prop <*> (Just <$> decode ob)
    go "has" [prop] = Has <$> decode prop <*> pure Nothing
    go "has" [prop, ob] = Has <$> decode prop <*> (Just <$> decode ob)
    go "length" xs = decodeUnOp "length" Length xs
    go "!" xs = decodeUnOp "!" Not xs
    go "!=" xs = decodeBinOp "!=" NotEq xs
    go "<" xs = decodeBinOp "<" LessThan xs
    go "<=" xs = decodeBinOp "<=" LessThanEq xs
    go "==" xs = decodeBinOp "==" Equal xs
    go ">" xs = decodeBinOp ">" GreaterThan xs
    go ">=" xs = decodeBinOp ">=" GreaterThanEq xs
    go "all" xs = decodeBinOpArgs "all" All xs
    go "any" xs = decodeBinOpArgs "any" Any xs
    go "in" xs = decodeBinOpArgs "in" In xs
    go "case" xs
      | Just {init,last} <- unsnoc xs
      , Just cases <- pairs init
      = Case <$> traverse decodeTuple cases <*> decode last
    go "coalesce" xs = decodeBinOpArgs "coalesce" Coalesce xs
    go "match" xs
      | Just {head:input,tail} <- uncons xs
      , Just {init,last:default} <- unsnoc tail
      , Just labelVals <- pairs init
      = Match <$> decode input
              <*> traverse (\(Tuple ls v) -> Tuple <$> decodeLabel ls <*> decode v) labelVals
              <*> decode default
      where
      decodeLabel ls = decodeLabelArray ls <|> decodeLabelSingle ls
      decodeLabelArray ls = decode ls >>= \ls' -> case uncons ls' of
        Just {head,tail} -> pure (NonEmpty head tail)
        Nothing          -> fail (ForeignError "Expected at least on item in label array for match")
      decodeLabelSingle ls = NonEmpty <$> decode ls <*> pure []
    go "interpolate" xs
      | Just {head:t, tail:tail} <- uncons xs
      , Just {head:e, tail:stops'} <- uncons tail
      , Just stops <- pairs stops'
      = Interpolate <$> decode t
                    <*> decode e
                    <*> traverse (\(Tuple i o) -> Tuple <$> decode i <*> decode o) stops
    go "step" xs
      | Just {head:input,tail} <- uncons xs
      , Just {head:out0,tail:tail'} <- uncons tail
      , Just steps <- pairs tail'
      = Step <$> decode input <*> decode out0 <*> traverse decodeTuple steps
    go "let" xs
      | Just {init,last} <- unsnoc xs
      , Just bindings <- pairs init
      = Let <$> traverse decodeTuple bindings <*> decode last
    go "var" [name] = Var <$> decode name
    go "concat" xs = decodeBinOpArgs "concat" Concat xs
    go "donwcase" xs = decodeUnOp "downcase" Downcase xs
    go "upcase" xs = decodeUnOp "upcase" Upcase xs
    go "rgb" [r,g,b] = Rgb <$> decode r <*> decode g <*> decode b
    go "rgba" [r,g,b,a] = Rgba <$> decode r <*> decode g <*> decode b <*> decode a
    go "to-rgba" xs = decodeUnOp "to-rgba" ToRgba xs
    go "-" xs = decodeBinOp "-" Minus xs
    go "*" xs = decodeBinOp "*" Mult xs
    go "/" xs = decodeBinOp "/" Div xs
    go "%" xs = decodeBinOp "%" Mod xs
    go "^" xs = decodeBinOp "^" Exp xs
    go "+" xs = decodeBinOp "+" Plus xs
    go "acos" xs = decodeUnOp "acos" Acos xs
    go "asin" xs = decodeUnOp "asin" Asin xs
    go "atan" xs = decodeUnOp "atan" Atan xs
    go "cos" xs = decodeUnOp "cos" Cos xs
    go "e" xs = decodeConstant "e" E xs
    go "ln" xs = decodeUnOp "ln" Ln xs
    go "ln2" xs = decodeUnOp "ln2" Ln2 xs
    go "log10" xs = decodeUnOp "log10" Log10 xs
    go "log2" xs = decodeUnOp "log2" Log2 xs
    go "max" xs = decodeBinOpArgs "max" Max xs
    go "min" xs = decodeBinOpArgs "min" Min xs
    go "pi" xs = decodeConstant "pi" Pi xs
    go "sin" xs = decodeUnOp "sin" Sin xs
    go "sqrt" xs = decodeUnOp "sqrt" Sqrt xs
    go "tan" xs = decodeUnOp "tan" Tan xs
    go "zoom" xs = decodeConstant "zoom" Zoom xs
    go "heatmap-density" xs = decodeConstant "heatmap-density" HeatmapDensity xs
    
    go unknown _   = fail (ForeignError ("Unknown operand or invalid arguments: " <> unknown))

decodeTuple :: forall a b. Decode a => Decode b => Tuple Foreign Foreign -> F (Tuple a b)
decodeTuple (Tuple c v) = Tuple <$> decode c <*> decode v

constant :: String -> Foreign
constant c = toForeign [c]

decodeConstant :: String -> Expr -> Array Foreign -> F Expr
decodeConstant _   c [] = pure c
decodeConstant tag _ _  = fail (ForeignError ("expected no args for " <> tag))

unOp :: String -> Expr -> Foreign
unOp op e = toForeign [toForeign op, encode e]

decodeUnOp :: String -> (Expr -> Expr) -> Array Foreign -> F Expr
decodeUnOp _   f [x] = f <$> decode x
decodeUnOp tag _ _   = fail (ForeignError ("expected one arg for " <> tag))

unOpArgs :: String -> Expr -> Array Expr -> Foreign
unOpArgs op a xs = toForeign ([toForeign op, encode a] <> map encode xs)

decodeUnOpArgs :: String -> (Expr -> Array Expr -> Expr) -> Array Foreign -> F Expr
decodeUnOpArgs _ f xs
  | Just {head,tail} <- uncons xs = f <$> decode head <*> traverse decode tail
decodeUnOpArgs tag _ _ =
  fail (ForeignError ("expected at least one arg for " <> tag))


binOp :: String -> Expr -> Expr -> Foreign
binOp op a b = toForeign  [toForeign op, encode a, encode b]

decodeBinOp :: String -> (Expr -> Expr -> Expr) -> Array Foreign -> F Expr
decodeBinOp _   f [a,b] = f <$> decode a <*> decode b
decodeBinOp tag _ _   = fail (ForeignError ("expected two args for " <> tag))


binOpArgs :: String -> Expr -> Expr -> Array Expr -> Foreign
binOpArgs op a b xs = toForeign ([toForeign op, encode a, encode b] <> map encode xs)

decodeBinOpArgs :: String -> (Expr -> Expr -> Array Expr -> Expr) -> Array Foreign -> F Expr
decodeBinOpArgs _ f xs
  | Just {head:a, tail} <- uncons xs
  , Just {head:b, tail:rest} <- uncons tail
  = f <$> decode a <*> decode b <*> traverse decode rest
decodeBinOpArgs tag _ _ =
  fail (ForeignError ("expected at least two args for " <> tag))

data InterpolationType
  = Linear
  | Exponential Number
  | CubicBezier
    { x1 :: Number
    , y1 :: Number
    , x2 :: Number
    , y2 :: Number
    }

instance encodeInterpolationType :: Encode InterpolationType where
  encode Linear = toForeign ["linear"]
  encode (Exponential v) = toForeign [toForeign "exponential", toForeign v]
  encode (CubicBezier {x1,y1,x2,y2}) = toForeign [toForeign "cubic-bezier", toForeign x1, toForeign y1, toForeign x2, toForeign y2]
instance decodeInterpolationType :: Decode InterpolationType where
  decode o' = decode o' >>= \o -> case uncons o of
    Just {head, tail} -> do
      tag <- decode head
      case tag of
        "linear"
          | [] <- tail -> pure Linear
          | otherwise  -> fail (ForeignError "expected no arguments to linear interpolation")
        "exponential"
          | [base] <- tail -> Exponential <$> decode base
          | otherwise      -> fail (ForeignError "expected base argument to exponential interpolation")
        "cubic-bezier" -> do
            args <- traverse decode tail
            case args of
              [x1,y1,x2,y2] -> pure (CubicBezier {x1,y1,x2,y2})
              _             -> fail (ForeignError "expected 4 arguments to cubic-bezier interpolation")
        unknown -> fail (ForeignError ("unknown interpolation type: " <> unknown))
    _ -> fail (ForeignError "empty interpolate Expr")