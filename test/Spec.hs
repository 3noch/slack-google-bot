import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Test.SmallCheck.Series.Instances ()
import Test.Hspec
import Test.Hspec.SmallCheck (property)
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (testSpec)

import AppLib.GoogleCseApi (ResultItem(..))
import AppLib.SlackBot (botResponder)
import qualified AppLib.SearchFormatting as Fmt

main = defaultMain =<< tests


tests = testSpec "tests" $ do
  describe "bot responder" $ do
    let responder = botResponder (liftIO . return)

    it "responds to messages starting with 'google'" $ do
      responder "google" `shouldReturn` Just ""
      responder "google test" `shouldReturn` Just "test"
      responder "google google" `shouldReturn` Just "google"
      responder "google Something a bit more *#$@$ Complex"
        `shouldReturn` Just "Something a bit more *#$@$ Complex"

    it "doesn't respond to messages that don't start with 'google'" $ do
      responder "" `shouldReturn` Nothing
      responder "googl" `shouldReturn` Nothing
      responder "googlE" `shouldReturn` Nothing
      responder "GOOGLE" `shouldReturn` Nothing
      responder "oogle test" `shouldReturn` Nothing

  describe "formatter" $ do
    it "can embolden things" $
      property $ \x -> Fmt.bold x == "*" <> x <> "*"

    it "can format a single result item" $
      Fmt.formatResultItem ResultItem {title="title", link="http://link.com", snippet="snip"}
        `shouldBe` "*title*\nsnip\nhttp://link.com\n"
