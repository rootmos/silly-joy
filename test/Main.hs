module Main where

import Test.Hspec
import Control.Exception

import Parser
import Meaning
import Runner

main :: IO ()
main = hspec $ do
  spec_parser
  spec_simulate

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

        it "should parse: \"foo\"" $ do
            parse "\"foo\"" `shouldBe` Right [Str "foo"]

        it "should parse: \"foo \\\"bar\\\"\"" $ do
            parse "\"foo \\\"bar\\\"\"" `shouldBe` Right [Str "foo \"bar\""]

        it "should parse: 1" $ do parse "1" `shouldBe` Right [Number 1]
        it "should parse: 0" $ do parse "0" `shouldBe` Right [Number 0]
        it "should parse: -7" $ do parse "-7" `shouldBe` Right [Number (-7)]

        it "should parse: +" $ do parse "+" `shouldBe` Right [Word "+"]
        it "should parse: -" $ do parse "-" `shouldBe` Right [Word "-"]
        it "should parse: *" $ do parse "*" `shouldBe` Right [Word "*"]
        it "should parse: /" $ do parse "/" `shouldBe` Right [Word "/"]
        it "should parse: %" $ do parse "%" `shouldBe` Right [Word "%"]
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

        it "should parse: x := 7; x" $ do
            parse "x := 7; x" `shouldBe` Right
                [Binding "x" [Number 7], Word "x"]

        it "should parse: x := 7" $ do
            parse "x := 7" `shouldBe` Right
                [Binding "x" [Number 7]]

        it "should parse: x := 7;" $ do
            parse "x := 7;" `shouldBe` Right
                [Binding "x" [Number 7]]

        it "should parse: x;" $ do
            parse "x;" `shouldBe` Right [Word "x"]

        it "should parse: x; y" $ do
            parse "x; y" `shouldBe` Right [Word "x", Word "y"]

        it "should parse: x ;y" $ do
            parse "x ;y" `shouldBe` Right [Word "x", Word "y"]

        it "should parse: x ; y" $ do
            parse "x ; y" `shouldBe` Right [Word "x", Word "y"]

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
        it "should simulate: 3 4 *" $ do
            let (State { stack = st }) = simulateUnsafe "3 4 *" []
            st `shouldBe` [I 12]
        it "should simulate: 1 +" $ do
            evaluate (simulateUnsafe "1 +" [])
                `shouldThrow` (== PoppingEmptyStack)

        it "should simulate: 7 2 /" $ do
            stack (simulateUnsafe "7 2 /" []) `shouldBe` [I 3]
        it "should simulate: 7 2 %" $ do
            stack (simulateUnsafe "7 2 %" []) `shouldBe` [I 1]
        it "should simulate: 7 2 div" $ do
            stack (simulateUnsafe "7 2 div" []) `shouldBe` [I 1, I 3]

        it "should simulate: 7 print" $ do
            let (State { stack = st }) = simulateUnsafe "7 print" [expect "7"]
            st `shouldBe` []
        it "should simulate: [foo]" $ do
            let (State { stack = st }) = simulateUnsafe "[foo]" []
            length st `shouldBe` 1
        it "should simulate: 7 [dup] i" $ do
            stack (simulateUnsafe "7 [dup] i" []) `shouldBe` [I 7, I 7]
        it "should simulate: [[7]] i i" $ do
            stack (simulateUnsafe "[[7]] i i" []) `shouldBe` [I 7]
        it "should simulate: [[7 8] dup] i dip i" $ do
            stack (simulateUnsafe "[[7 8] dup] i dip i" [])
                `shouldBe` [I 8, I 7, I 8, I 7]
        it "should simulate: [[7 8] dup] i b" $ do
            stack (simulateUnsafe "[[7 8] dup] i b" [])
                `shouldBe` [I 8, I 7, I 8, I 7]
        it "should simulate: [foo] +" $ do
            evaluate (simulateUnsafe "[foo] +" [])
                `shouldThrow` (== TypeMismatch)
        it "should simulate: [foo] 1 +" $ do
            evaluate (simulateUnsafe "[foo] 1 +" [])
                `shouldThrow` (== TypeMismatch)
        it "should simulate: 1 i" $ do
            evaluate (simulateUnsafe "1 i" [])
                `shouldThrow` (== TypeMismatch)

        it "should simulate: 2 3 4 [+] dip" $ do
            stack (simulateUnsafe "2 3 4 [+] dip" []) `shouldBe` [I 4, I 5]

        it "should simulate: [foo] dip" $ do
            evaluate (simulateUnsafe "[foo] dip" [])
                `shouldThrow` (== PoppingEmptyStack)
        it "should simulate: [foo] 1 dip" $ do
            evaluate (simulateUnsafe "[foo] 1 dip" [])
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

        it "should simulate: true" $ do
            stack (simulateUnsafe "true" []) `shouldBe` [B True]
        it "should simulate: false" $ do
            stack (simulateUnsafe "false" []) `shouldBe` [B False]

        it "should simulate: false false or" $ do
            stack (simulateUnsafe "false false or" []) `shouldBe` [B False]
        it "should simulate: true false or" $ do
            stack (simulateUnsafe "true false or" []) `shouldBe` [B True]
        it "should simulate: false true or" $ do
            stack (simulateUnsafe "false true or" []) `shouldBe` [B True]
        it "should simulate: true true or" $ do
            stack (simulateUnsafe "true true or" []) `shouldBe` [B True]

        it "should simulate: false false and" $ do
            stack (simulateUnsafe "false false and" []) `shouldBe` [B False]
        it "should simulate: true false and" $ do
            stack (simulateUnsafe "true false and" []) `shouldBe` [B False]
        it "should simulate: false true and" $ do
            stack (simulateUnsafe "false true and" []) `shouldBe` [B False]
        it "should simulate: true true and" $ do
            stack (simulateUnsafe "true true and" []) `shouldBe` [B True]

        it "should simulate: false not" $ do
            stack (simulateUnsafe "false not" []) `shouldBe` [B True]
        it "should simulate: true not" $ do
            stack (simulateUnsafe "true not" []) `shouldBe` [B False]

        it "should simulate: [1 1 =] [7] [8] ifte" $ do
            stack (simulateUnsafe "[1 1 =] [7] [8] ifte" []) `shouldBe` [I 7]
        it "should simulate: [1 2 =] [7] [8] ifte" $ do
            stack (simulateUnsafe "[1 2 =] [7] [8] ifte" []) `shouldBe` [I 8]
        it "should simulate: 1 1 [=] [7] [8] ifte" $ do
            stack (simulateUnsafe "1 1 [=] [7] [8] ifte" [])
                `shouldBe` [I 7, I 1, I 1]
        it "should simulate: 1 2 [=] [7] [8] ifte" $ do
            stack (simulateUnsafe "1 2 [=] [7] [8] ifte" [])
                `shouldBe` [I 8, I 2, I 1]

        it "should simulate: 0 null" $ do
            stack (simulateUnsafe "0 null" []) `shouldBe` [B True]
        it "should simulate: 1 null" $ do
            stack (simulateUnsafe "1 null" []) `shouldBe` [B False]

        it "should simulate: 0 succ" $ do
            stack (simulateUnsafe "0 succ" []) `shouldBe` [I 1]
        it "should simulate: 1 pred" $ do
            stack (simulateUnsafe "1 pred" []) `shouldBe` [I 0]

        it "should simulate: 1 2 swap" $ do
            stack (simulateUnsafe "1 2 swap" []) `shouldBe` [I 1, I 2]

        it "should simulate: [1 2] i" $ do
            stack (simulateUnsafe "[1 2] i" []) `shouldBe` [I 2, I 1]

        it "should simulate: 1 2 [+] I" $ do
            stack (simulateUnsafe "1 2 [+] I" []) `shouldBe` [I 3, I 2, I 1]

        it "should simulate: 1 2 3 [pop + 7 swap] I" $ do
            stack (simulateUnsafe "1 2 3 [pop + 7 swap] I" [])
                `shouldBe` [I 3, I 3, I 2, I 1]

        it "should simulate: [1] [2] concat i" $ do
            stack (simulateUnsafe "[1] [2] concat i" []) `shouldBe` [I 2, I 1]

        it "should simulate: [1] [2] b" $ do
            stack (simulateUnsafe "[1] [2] b" []) `shouldBe` [I 2, I 1]


        it "should simulate: 1 [2] cons i" $ do
            stack (simulateUnsafe "1 [2] cons i" []) `shouldBe` [I 2, I 1]


        it "should simulate: [1] first" $ do
            stack (simulateUnsafe "[1] first" []) `shouldBe` [I 1]

        it "should simulate: [1 2] first" $ do
            stack (simulateUnsafe "[1 2] first" []) `shouldBe` [I 1]

        it "should simulate: [] first" $ do
            evaluate (simulateUnsafe "[] first" [])
                `shouldThrow` (== EmptyAggregate)

        it "should simulate: 1 [] cons [] cons first first" $ do
            stack (simulateUnsafe "1 [] cons [] cons first first" [])
                `shouldBe` [I 1]

        it "should simulate: 1 2 [+ 4] first" $ do
            stack (simulateUnsafe "1 2 [+ 4] first" []) `shouldBe` [I 3]


        it "should simulate: [] size" $ do
            stack (simulateUnsafe "[] size" []) `shouldBe` [I 0]

        it "should simulate: [1 2] size" $ do
            stack (simulateUnsafe "[1 2] size" []) `shouldBe` [I 2]


        it "should simulate: [] rest" $ do
            evaluate (simulateUnsafe "[] rest" [])
                `shouldThrow` (== EmptyAggregate)

        it "should simulate: [1] rest i" $ do
            stack (simulateUnsafe "[1] rest i" []) `shouldBe` []

        it "should simulate: [1 2] rest i" $ do
            stack (simulateUnsafe "[1 2] rest i" []) `shouldBe` [I 2]

        it "should simulate: [1 2 3] rest i" $ do
            stack (simulateUnsafe "[1 2 3] rest i" []) `shouldBe` [I 3, I 2]


        it "should simulate: [] uncons" $ do
            evaluate (simulateUnsafe "[] uncons" [])
                `shouldThrow` (== EmptyAggregate)

        it "should simulate: [1] uncons size" $ do
            stack (simulateUnsafe "[1] uncons size" [])
                `shouldBe` [I 0, I 1]

        it "should simulate: [1] uncons i" $ do
            stack (simulateUnsafe "[1] uncons i" [])
                `shouldBe` [I 1]

        it "should simulate: [1 2 3] uncons size" $ do
            stack (simulateUnsafe "[1 2 3] uncons size" [])
                `shouldBe` [I 2, I 1]

        it "should simulate: [1 2 3] uncons i" $ do
            stack (simulateUnsafe "[1 2 3] uncons i" [])
                `shouldBe` [I 3, I 2, I 1]

        it "should simulate: 1 [2] cons uncons i" $ do
            stack (simulateUnsafe "1 [2] cons uncons i" [])
                `shouldBe` [I 2, I 1]

        it "should simulate: 1 2 [+ 4] uncons i" $ do
            stack (simulateUnsafe "1 2 [+ 4] uncons i" [])
                `shouldBe` [I 4, I 3]

        it "should simulate factorial example" $ do
            stack (flip simulateUnsafe [] $
                "5 [ [pop 0 =] [pop pop 1]" ++
                "[ [dup 1 -] dip dup i * ] ifte ] dup i") `shouldBe` [I 120]

        it "should simulate: \"foo\"" $ do
            stack (simulateUnsafe "\"foo\"" []) `shouldBe` [S "foo"]

        it "should simulate: \"foo\" strlen" $ do
            stack (simulateUnsafe "\"foo\" strlen" []) `shouldBe` [I 3]

        it "should simulate: 1 strlen" $ do
            evaluate (simulateUnsafe "1 strlen" [])
                `shouldThrow` (== TypeMismatch)

        it "should simulate: \"foo\" \"bar\" strcat" $ do
            stack (simulateUnsafe "\"foo\" \"bar\" strcat" [])
                `shouldBe` [S "foobar"]

        it "should simulate: [+] \"plus\" bind 1 2 plus" $ do
            stack (simulateUnsafe "[+] \"plus\" bind 1 2 plus" [])
                `shouldBe` [I 3]

        it "should simulate: [1] \"i\" bind i" $ do
            stack (simulateUnsafe "[1] \"i\" bind i" [])
                `shouldBe` [I 1]

        it "should simulate: [2] \"bind\" bind bind" $ do
            stack (simulateUnsafe "[2] \"bind\" bind bind" [])
                `shouldBe` [I 2]

        it "should simulate: [[1] \"y\" bind y] I y" $ do
            evaluate (simulateUnsafe "[[1] \"y\" bind y] I y" [])
                `shouldThrow` (== Undefined "y")


        it "should simulate: plus := +; 1 2 plus" $ do
            stack (simulateUnsafe "plus := +; 1 2 plus" [])
                `shouldBe` [I 3]

        it "should simulate: i := 1; i" $ do
            stack (simulateUnsafe "i := 1; i" [])
                `shouldBe` [I 1]

        it "should simulate: bind := 2; bind" $ do
            stack (simulateUnsafe "bind := 2; bind" [])
                `shouldBe` [I 2]

        it "should simulate: [y := 1; y] I y" $ do
            evaluate (simulateUnsafe "[y := 1; y] I y" [])
                `shouldThrow` (== Undefined "y")

        it "should simulate: [x := 1] i x" $ do
            stack (simulateUnsafe "[x := 1] i x" [])
                `shouldBe` [I 1]

        it "should simulate: [x := 1; 2] i x" $ do
            stack (simulateUnsafe "[x := 1; 2] i x" [])
                `shouldBe` [I 1, I 2]

        it "should simulate: read_int" $ do
            stack (simulateUnsafe "read_int" [send "7"]) `shouldBe` [I 7]

        it "should simulate: read_int" $ do
            evaluate (simulateUnsafe "read_int" [send "foo"])
                `shouldThrow` (== UnparseableAsNumber "foo")

        it "should simulate: read_line" $ do
            stack (simulateUnsafe "read_line" [send "foo"])
                `shouldBe` [S "foo"]

        it "should simulate: 1 2 3 rolldown" $ do
            stack (simulateUnsafe "1 2 3 rolldown" [])
                `shouldBe` (reverse [I 2, I 3, I 1])

        it "should simulate: 1 2 3 rollup" $ do
            stack (simulateUnsafe "1 2 3 rollup" [])
                `shouldBe` (reverse [I 3, I 1, I 2])

        it "should simulate: 1 2 3 rotate" $ do
            stack (simulateUnsafe "1 2 3 rotate" [])
                `shouldBe` (reverse [I 3, I 2, I 1])

        it "should simulate: [1 2] x rolldown i" $ do
            stack (simulateUnsafe "[1 2] x rolldown i" [])
                `shouldBe` [I 2, I 1, I 2, I 1]

        it "should simulate: 0 [7] [] primrec" $ do
            stack (simulateUnsafe "0 [7] [] primrec" [])
                `shouldBe` [I 7]

        it "should simulate: 5 [1] [*] primrec" $ do
            stack (simulateUnsafe "5 [1] [*] primrec" [])
                `shouldBe` [I 120]

        it "should simulate: 10 [0] [+] primrec" $ do
            stack (simulateUnsafe "10 [0] [+] primrec" [])
                `shouldBe` [I 55]

        it "should simulate: 0 [null] [succ] [dup pred] [*] linrec" $ do
            stack (simulateUnsafe "0 [null] [succ] [dup pred] [*] linrec" [])
                `shouldBe` [I 1]

        it "should simulate: 5 [null] [succ] [dup pred] [*] linrec" $ do
            stack (simulateUnsafe "5 [null] [succ] [dup pred] [*] linrec" [])
                `shouldBe` [I 120]

        it "should simulate: [1] 0 times" $ do
            stack (simulateUnsafe "[1] 0 times" [])
                `shouldBe` []

        it "should simulate: [1] 5 times" $ do
            stack (simulateUnsafe "[1] 5 times" [])
                `shouldBe` [I 1, I 1, I 1, I 1, I 1]

        it "should simulate: clear" $ do
            stack (simulateUnsafe "clear" []) `shouldBe` []
        it "should simulate: 1 2 clear" $ do
            stack (simulateUnsafe "1 2 clear" []) `shouldBe` []

        it "should simulate: [1 2 3] [1 +] map i" $ do
            stack (simulateUnsafe "[1 2 3] [1 +] map i" [])
                `shouldBe` [I 4, I 3, I 2]

        it "should simulate: 1 [] cons [1 +] map i" $ do
            stack (simulateUnsafe "1 [] cons [1 +] map i" [])
                `shouldBe` [I 2]

        it "should simulate: [1 2 3 4] [2 % null] filter i" $ do
            stack (simulateUnsafe "[1 2 3 4] [2 % null] filter i" [])
                `shouldBe` [I 4, I 2]

        it "should simulate: 1 [2 3 4] cons [2 % null] filter i" $ do
            stack (simulateUnsafe "1 [2 3 4] cons [2 % null] filter i" [])
                `shouldBe` [I 4, I 2]
