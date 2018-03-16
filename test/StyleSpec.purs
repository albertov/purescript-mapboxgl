module StyleSpec where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (ForeignError)
import Data.Foreign.Class (decode)
import Data.Foreign.JSON (decodeJSONWith)
import Data.List.NonEmpty (NonEmptyList)
import Data.Traversable (traverse_)
import Mapbox.GL (Style)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile, readdir)
import Node.Path (FilePath, sep)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Runner (RunnerEffects)

basePath :: FilePath
basePath = "test/data/styles"

spec :: Spec (RunnerEffects (exception :: EXCEPTION, fs :: FS)) Unit
spec = describe "json" do
  it "can deserialize mapbox styles" do
    fs <- liftEff (readdir basePath)
    traverse_ checkFile (map ((<>) (basePath <> sep)) fs)

checkFile :: forall t8.
  String
  -> Aff
       ( fs :: FS
       , exception :: EXCEPTION
       | t8
       )
       Unit
checkFile fname = do
  f <- liftEff (readTextFile UTF8 fname)
  let em :: Either (NonEmptyList ForeignError) Style
      em = runExcept (decodeJSONWith decode f)
  case em of
    Right _ -> pure unit
    Left e ->
      fail ("Could not deserialize " <> fname <> ": " <> show e)
