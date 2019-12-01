import qualified App.Template.Parse.Lexer as Lexer
import qualified App.Template.Parse.Parser as Parser
import qualified App.Template.Parse.Lifter as Lifter
import qualified Lib.Utils.String as S

main = do
  input <- readFile "demo.template"
  let rawLines = lines input
  let trimmedLines = map S.trim rawLines
  let tokens = Lexer.tokenize trimmedLines
  let tree = Parser.parse tokens
  let output = Lifter.lift tree
  putStrLn $ show output

