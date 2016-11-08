import           Shunty
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tokenize function" $ do
    it "takes an empty string returns an empty list" $ do
      tokenize "" `shouldBe` []

    it "splits operators and digits" $ do
      tokenize "1+2*3-4/5" `shouldBe`
        [Value 1, Op Plus, Value 2, Op Multiply, Value 3, Op Minus, Value 4, Op Divide, Value 5]

    it "ignores whitespace" $ do
      tokenize "1    *               3" `shouldBe` [Value 1, Op Multiply, Value 3]

    it "accepts brackets as tokens" $ do
      tokenize "(1 + 3)" `shouldBe` [Bracket Open, Value 1, Op Plus, Value 3, Bracket Close]

    it "pulls multidigit numbers" $ do
      tokenize "1235" `shouldBe` [Value 1235]

  describe "Shunting Yard implementation" $ do
    it "correctly stops" $ do
      shuntingYard [] [] [Value 1, Value 2, Op Plus] `shouldBe` [Value 1, Value 2, Op Plus]
      shuntingYard [] [Op Plus] [Value 1, Value 2] `shouldBe` [Value 1, Value 2, Op Plus]

    it "adds any operator to an empty stack" $ do
      shuntingYard [Op Plus] [] [] `shouldBe` shuntingYard [] [Op Plus] []
      shuntingYard [Op Multiply] [] [] `shouldBe` shuntingYard [] [Op Multiply] []
      shuntingYard [Op Minus] [] [] `shouldBe` shuntingYard [] [Op Minus] []
      shuntingYard [Op Divide] [] [] `shouldBe` shuntingYard [] [Op Divide] []

    it "outputs higher precedence operators" $ do
      shuntingYard [Op Plus] [Op Multiply] [] `shouldBe` shuntingYard [] [Op Plus] [Op Multiply]

  describe "Parse Function" $ do
    it "parses a value" $ do
      parse "123" `shouldBe` [Value 123]

    it "parses a minimal example" $ do
      parse "1 + 2" `shouldBe` [Value 1, Value 2, Op Plus]

    it "parses no bracket examples" $ do
      parse "1 * 2 + 3" `shouldBe` [Value 1, Value 2 , Op Multiply, Value 3, Op Plus]
      parse "1 + 2 * 3" `shouldBe` [Value 1, Value 2, Value 3, Op Multiply, Op Plus]

    it "parses simple bracket examples" $ do
      parse "1 * (2 + 3)" `shouldBe` [Value 1, Value 2, Value 3, Op Plus, Op Multiply]

    it "parses a complex example" $ do
      -- A * (B + C * D) + E becomes A B C D * + * E +
      parse "1 * (2 + 3 * 4) + 5" `shouldBe` [Value 1, Value 2, Value 3, Value 4, Op Multiply, Op Plus, Op Multiply, Value 5, Op Plus]
