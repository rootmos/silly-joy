module Spec where

import Test.Tasty
import Test.Tasty.Hspec
import Control.Exception

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

        it "should parse: 1" $ do parse "1" `shouldBe` Right [Number 1]
        it "should parse: 0" $ do parse "0" `shouldBe` Right [Number 0]
        it "should parse: -7" $ do parse "-7" `shouldBe` Right [Number (-7)]

        it "should parse: +" $ do parse "+" `shouldBe` Right [Word "+"]
        it "should parse: -" $ do parse "-" `shouldBe` Right [Word "-"]
        it "should parse: =" $ do parse "=" `shouldBe` Right [Word "="]
        it "should parse: !=" $ do parse "!=" `shouldBe` Right [Word "!="]
        it "should parse: <" $ do parse "<" `shouldBe` Right [Word "<"]
        it "should parse: >" $ do parse ">" `shouldBe` Right [Word ">"]
        it "should parse: <=" $ do parse "<=" `shouldBe` Right [Word "<="]
        it "should parse: >=" $ do parse ">=" `shouldBe` Right [Word ">="]

        it "should parse: foo bar" $ do
            parse "foo bar" `shouldBe` Right
                [Word "foo", Word "bar"]

        it "should parse: []" $ do
            parse "[]" `shouldBe` Right [Quoted []]

        it "should parse:  []" $ do
            parse " []" `shouldBe` Right [Quoted []]

        it "should parse: []  (trailing space)" $ do
            parse "[] " `shouldBe` Right [Quoted []]

        it "should parse: [ ]" $ do
            parse "[ ]" `shouldBe` Right [Quoted []]

        it "should parse: [foo]" $ do
            parse "[foo]" `shouldBe` Right
                [Quoted [Word "foo"]]

        it "should parse: [foo] bar" $ do
            parse "[foo] bar" `shouldBe` Right
                [Quoted [Word "foo"], Word "bar"]

        it "should parse: [ foo bar ]" $ do
            parse "[ foo bar ]" `shouldBe` Right
                [Quoted [Word "foo", Word "bar"]]

        it "should parse: [[foo]]" $ do
            parse "[[foo]]" `shouldBe` Right
                [Quoted [Quoted [Word "foo"]]]

        it "should parse: [[foo] bar]" $ do
            parse "[[foo] bar]" `shouldBe` Right
                [Quoted [Quoted [Word "foo"], Word "bar"]]

spec_simulate =
    describe "simulate" $ do
        it "should simulate empty string" $ do
            let (State { stack = st }) = simulateUnsafe "" []
            st `shouldBe` []
        it "should simulate: 1" $ do
            let (State { stack = st }) = simulateUnsafe "1" []
            st `shouldBe` [I 1]
        it "should simulate: 1 pop" $ do
            let (State { stack = st }) = simulateUnsafe "1 pop" []
            st `shouldBe` []
        it "should simulate: 2 dup" $ do
            let (State { stack = st }) = simulateUnsafe "2 dup" []
            st `shouldBe` [I 2, I 2]
        it "should simulate: pop" $ do
            evaluate (simulateUnsafe "pop" [])
                `shouldThrow` (== PoppingEmptyStack)
        it "should simulate: foo" $ do
            evaluate (simulateUnsafe "foo" [])
                `shouldThrow` (== Undefined "foo")
        it "should simulate: 1 2" $ do
            let (State { stack = st }) = simulateUnsafe "1 2" []
            st `shouldBe` [I 2, I 1]
        it "should simulate: 1 2 +" $ do
            let (State { stack = st }) = simulateUnsafe "1 2 +" []
            st `shouldBe` [I 3]
        it "should simulate: 1 2 -" $ do
            let (State { stack = st }) = simulateUnsafe "1 2 -" []
            st `shouldBe` [I (-1)]
        it "should simulate: 1 +" $ do
            evaluate (simulateUnsafe "1 +" [])
                `shouldThrow` (== PoppingEmptyStack)
        it "should simulate: 7 print" $ do
            let (State { stack = st }) = simulateUnsafe "7 print" [expect "7"]
            st `shouldBe` []
        it "should simulate: [foo]" $ do
            let (State { stack = st }) = simulateUnsafe "[foo]" []
            length st `shouldBe` 1
        it "should simulate: 7 [dup] i" $ do
            let (State { stack = st }) = simulateUnsafe "7 [dup] i" []
            st `shouldBe` [I 7, I 7]
        it "should simulate: [foo] +" $ do
            evaluate (simulateUnsafe "[foo] +" [])
                `shouldThrow` (== TypeMismatch)
        it "should simulate: [foo] 1 +" $ do
            evaluate (simulateUnsafe "[foo] 1 +" [])
                `shouldThrow` (== TypeMismatch)
        it "should simulate: 1 i" $ do
            evaluate (simulateUnsafe "1 i" [])
                `shouldThrow` (== TypeMismatch)
        it "should simulate: [1] 2 dip +" $ do
            let (State { stack = st }) = simulateUnsafe "[1] 2 dip +" []
            st `shouldBe` [I 3]
        it "should simulate: 1 2 dip" $ do
            evaluate (simulateUnsafe "1 2 dip" [])
                `shouldThrow` (== TypeMismatch)
        it "should simulate: 1 [foo] dip" $ do
            evaluate (simulateUnsafe "1 [foo] dip" [])
                `shouldThrow` (== TypeMismatch)

        it "should simulate: 1 2 <" $ do
            stack (simulateUnsafe "1 2 <" []) `shouldBe` [B True]
        it "should simulate: 2 2 <" $ do
            stack (simulateUnsafe "2 2 <" []) `shouldBe` [B False]

        it "should simulate: 2 1 >" $ do
            stack (simulateUnsafe "2 1 >" []) `shouldBe` [B True]
        it "should simulate: 2 2 >" $ do
            stack (simulateUnsafe "2 2 >" []) `shouldBe` [B False]

        it "should simulate: 3 1 >=" $ do
            stack (simulateUnsafe "3 1 >=" []) `shouldBe` [B True]
        it "should simulate: 1 3 >=" $ do
            stack (simulateUnsafe "1 3 >=" []) `shouldBe` [B False]
        it "should simulate: 2 2 >=" $ do
            stack (simulateUnsafe "2 2 >=" []) `shouldBe` [B True]

        it "should simulate: 3 1 <=" $ do
            stack (simulateUnsafe "3 1 <=" []) `shouldBe` [B False]
        it "should simulate: 1 3 <=" $ do
            stack (simulateUnsafe "1 3 <=" []) `shouldBe` [B True]
        it "should simulate: 2 2 <=" $ do
            stack (simulateUnsafe "2 2 <=" []) `shouldBe` [B True]

        it "should simulate: 1 2 =" $ do
            stack (simulateUnsafe "1 2 =" []) `shouldBe` [B False]
        it "should simulate: 2 2 =" $ do
            stack (simulateUnsafe "2 2 =" []) `shouldBe` [B True]

        it "should simulate: 1 2 !=" $ do
            stack (simulateUnsafe "1 2 !=" []) `shouldBe` [B True]
        it "should simulate: 2 2 !=" $ do
            stack (simulateUnsafe "2 2 !=" []) `shouldBe` [B False]

        it "should simulate: [1 1 =] [7] [8] ifte" $ do
            stack (simulateUnsafe "[1 1 =] [7] [8] ifte" []) `shouldBe` [I 7]
        it "should simulate: [1 2 =] [7] [8] ifte" $ do
            stack (simulateUnsafe "[1 2 =] [7] [8] ifte" []) `shouldBe` [I 8]
        it "should simulate: 1 1 [=] [7] [8] ifte" $ do
            stack (simulateUnsafe "1 1 [=] [7] [8] ifte" []) `shouldBe` [I 7]
        it "should simulate: 1 2 [=] [7] [8] ifte" $ do
            stack (simulateUnsafe "1 2 [=] [7] [8] ifte" []) `shouldBe` [I 8]

        it "should simulate: 1 2 swap" $ do
            stack (simulateUnsafe "1 2 swap" []) `shouldBe` [I 1, I 2]

        it "should simulate: [1 2] i" $ do
            stack (simulateUnsafe "[1 2] i" []) `shouldBe` [I 2, I 1]

        it "should simulate: [1] [2] concat i" $ do
            stack (simulateUnsafe "[1] [2] concat i" []) `shouldBe` [I 2, I 1]

        it "should simulate: [1] [2] b" $ do
            stack (simulateUnsafe "[1] [2] b" []) `shouldBe` [I 2, I 1]

        it "should simulate: [1] [i] cons i" $ do
            stack (simulateUnsafe "[1] [i] cons i" []) `shouldBe` [I 1]
