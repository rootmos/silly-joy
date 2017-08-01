module Spec where

import Test.Tasty
import Test.Tasty.Hspec

import Lib

spec_parser =
    describe "parse" $ do
        it "should parse empty string " $ do
            parse "" `shouldBe` Right []

        it "should parse: foo" $ do
            parse "foo" `shouldBe` Right [Word "foo"]

        it "should parse:  foo" $ do
            parse " foo" `shouldBe` Right [Word "foo"]

        it "should parse: foo  (trailing space)" $ do
            parse "foo " `shouldBe` Right [Word "foo"]

        it "should parse: foo bar" $ do
            parse "foo bar" `shouldBe` Right [Word "foo", Word "bar"]

        it "should parse: []" $ do
            parse "[]" `shouldBe` Right [Quoted []]

        it "should parse:  []" $ do
            parse " []" `shouldBe` Right [Quoted []]

        it "should parse: []  (trailing space)" $ do
            parse "[] " `shouldBe` Right [Quoted []]

        it "should parse: [ ]" $ do
            parse "[ ]" `shouldBe` Right [Quoted []]

        it "should parse: [foo]" $ do
            parse "[foo]" `shouldBe` Right [Quoted [Word "foo"]]

        it "should parse: [foo] bar" $ do
            parse "[foo] bar" `shouldBe` Right [Quoted [Word "foo"], Word "bar"]

        it "should parse: [ foo bar ]" $ do
            parse "[ foo bar ]" `shouldBe` Right
                [Quoted [Word "foo", Word "bar"]]

        it "should parse: [[foo]]" $ do
            parse "[[foo]]" `shouldBe` Right
                [Quoted [Quoted [Word "foo"]]]

        it "should parse: [[foo] bar]" $ do
            parse "[[foo] bar]" `shouldBe` Right
                [Quoted [Quoted [Word "foo"], Word "bar"]]
