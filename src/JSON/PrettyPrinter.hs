module JSON.PrettyPrinter
    ( prettyJValue
    , green
    , red
    )
where
import           Text.PrettyPrint.Leijen        ( text
                                                , indent
                                                , double
                                                , integer
                                                , (<>)
                                                , Doc
                                                , linebreak
                                                , vsep
                                                )
import           Data.Map                       ( toList )
import           JSON.Parsing.AST
import           System.Console.ANSI

green :: Show a => a -> IO ()
green msg = do
    setSGR [SetColor Foreground Vivid Green]
    print msg
    setSGR [Reset]

red :: Show a => a -> IO ()
red msg = do
    setSGR [SetColor Foreground Vivid Red]
    print msg
    setSGR [Reset]

prettyEntry :: (String, JValue) -> Doc
prettyEntry (key, value) =
    indent 2 $ text key <> text ": " <> prettyJValue value

prettyJValue :: JValue -> Doc
prettyJValue (JObject entries) =
    text "Object ["
        <> linebreak
        <> vsep (map (indent 2 . prettyEntry) $ toList entries)
        <> linebreak
        <> text "]"


prettyJValue (JList values) =
    text "Array ["
        <> linebreak
        <> vsep (map (indent 2 . prettyJValue) values)
        <> linebreak
        <> text "]"

prettyJValue (JInt    int  ) = text "Int " <> integer int
prettyJValue (JFloat  float) = text "Float " <> double float
prettyJValue (JString str  ) = text "String " <> text str
