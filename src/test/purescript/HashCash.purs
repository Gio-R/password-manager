module Test.HashCash where

import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (hex, fromArrayBuffer, toArrayBuffer)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.Unit (Unit)
import Effect.Aff (Aff, invincible)
import Functions.HashCash (verifyChallenge, computeHashCash)
import SRP (hashFuncSHA256)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import TestUtilities (makeTestableOnBrowser)

hashCashSpec :: SpecT Aff Unit Identity Unit
hashCashSpec =
  describe "HashCash" do
    let testReceipt = "test receipt"
    it testReceipt do
      let hexString = hex $ "EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C9C256576"
      challenge    <- fromArrayBuffer <$> (hashFuncSHA256 $ (toArrayBuffer hexString) : Nil)
      verification <- verifyChallenge hashFuncSHA256 challenge 20 hexString
      makeTestableOnBrowser testReceipt verification shouldEqual true
    let computeReceipt = "compute receipt"
    it computeReceipt $ invincible $ do
      let cost = 3
      let challenge = hex $ "EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C9C256576"
      receipt <- computeHashCash hashFuncSHA256 challenge cost
      verification <- verifyChallenge hashFuncSHA256 challenge cost receipt
      makeTestableOnBrowser computeReceipt verification shouldEqual true
                  
      