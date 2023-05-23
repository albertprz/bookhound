module Bookhound.Parser (Parser, ParseResult, ParseError(..), runParser, errorParser,
               andThen, exactly, isMatch, check, anyOf, allOf, char,
               withTransform, withError, withErrorN, withErrorFrom, except) where

import           Bookhound.Utils.Foldable (findJust)
import           Control.Applicative      (liftA2)
import           Data.Either              (fromRight)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text, pack, uncons, unpack)

type Input = Text

data Parser a
  = P
      { parse     :: Input -> ParseResult a
      , transform :: forall b. Maybe (Parser b -> Parser b)
      , errors    :: Set (Int, ParseError)
      }

data ParseResult a
  = Result Input a
  | Error ParseError
  deriving (Eq)

data ParseError
  = UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | UnexpectedString String
  | NoMatch String
  | ErrorAt String
  deriving (Eq, Ord)


instance Show a => Show (ParseResult a) where
  show (Result i a) = "Pending: " <> " >" <> unpack i <> "< "
                                  <> "\n\nResult: \n" <> show a
  show (Error err)  = show err

instance Show ParseError where
  show UnexpectedEof        = "Unexpected end of stream"
  show (ExpectedEof i)      = "Expected end of stream, but got "
                               <> ">" <> unpack i <> "<"
  show (UnexpectedChar c)   = "Unexpected char: "
                               <> "[" <> show c <> "]"
  show (UnexpectedString s) = "Unexpected string: "
                               <> "[" <> s <> "]"
  show (NoMatch s)          = "Did not match condition: " <> s
  show (ErrorAt s)          = "Error at " <> s


instance Functor ParseResult where
  fmap f (Result i a) = Result i (f a)
  fmap _ (Error pe)   = Error pe


instance Functor Parser where
  fmap f (P p t e) = applyTransformError t e $
    mkParser (fmap f . p)

instance Applicative Parser where
  pure a      = mkParser (`Result` a)
  liftA2 f (P p t e) mb@(P _ t' e') =
    applyTransformsErrors [t, t'] [e, e'] $ mkParser (\x ->
      case p x of
        Result i a -> parse ((f a) <$> mb) i
        Error pe   -> Error pe
    )

instance Monad Parser where
  (>>=) (P p t e) f = applyTransform t $ mkParser (\x ->
      case p x of
        Error pe   -> Error pe
        Result i a -> parse (applyError (e <> e') p2) i
          where
            p2@(P _ _ e') = f a
    )

runParser :: Parser a -> Input -> Either [ParseError] a
runParser p@(P _ _ e) i = toEither $ parse (exactly p) i
  where
    toEither = \case
      Result _ a -> Right a
      Error pe   -> Left $ (snd <$> reverse (Set.toList e)) <> [pe]

errorParser :: ParseError -> Parser a
errorParser = mkParser . const . Error

andThen :: Parser String -> Parser a -> Parser a
andThen p1 p2@(P _ t e) = applyTransformError t e $
  P (\i -> parse p2 $ fromRight i $ pack <$> runParser p1 i) t e

char :: Parser Char
char = mkParser $
   maybe (Error UnexpectedEof) (\(ch, rest) -> Result rest ch) . uncons


exactly :: Parser a -> Parser a
exactly (P p t e) = applyTransformError t e $
  mkParser (\x ->
    case p x of
      result@(Result i _) | i == mempty -> result
      Result i _                        -> Error $ ExpectedEof i
      err@(Error _)                     -> err
    )

anyOf :: [Parser a] -> Parser a
anyOf ps = anyOfHelper ps Nothing mempty

allOf :: [Parser a] -> Parser a
allOf ps = allOfHelper ps Nothing mempty


anyOfHelper :: [Parser a]
            -> (forall b. Maybe (Parser b -> Parser b))
            -> Set (Int, ParseError)
            -> Parser a
anyOfHelper [] _ _  = errorParser $ NoMatch "anyOf"
anyOfHelper [p] _ _ = p
anyOfHelper ((P p t e) : rest) t' e' =
  applyTransformsErrors [t, t'] [e, e'] $
    mkParser (\x ->
      case p x of
        result@(Result _ _) -> result
        Error _             -> parse (anyOfHelper rest t e) x
      )



allOfHelper :: [Parser a]
            -> (forall b. Maybe (Parser b -> Parser b))
            -> Set (Int, ParseError)
            -> Parser a
allOfHelper [] _ _ = errorParser $ NoMatch "allOf"
allOfHelper [p] _ _ = p
allOfHelper ((P p t e) : rest) t' e' =
  applyTransformsErrors [t, t'] [e, e'] $
    mkParser (\x ->
      case p x of
        Result _ _    -> parse (allOfHelper rest t e) x
        err@(Error _) -> err
      )



isMatch :: (Char -> Char -> Bool) -> Parser Char -> Char -> Parser Char
isMatch cond parser c1 =
  do c2 <- parser
     if cond c1 c2
       then pure c2
       else errorParser $ UnexpectedChar c2

check :: String -> (a -> Bool) -> Parser a -> Parser a
check condName cond parser =
  do c2 <- parser
     if cond c2
       then pure c2
       else  errorParser $ NoMatch condName


except :: Parser a -> Parser a -> Parser a
except (P p t e) (P p' _ _) = applyTransformError t e $ mkParser (
  \x -> case p' x of
    Result _ _ -> Error $ NoMatch "except"
    Error _    -> p x
  )

withError :: String -> Parser a -> Parser a
withError = withErrorN 0

withErrorN :: Int -> String -> Parser a -> Parser a
withErrorN n str = applyError . Set.singleton $ (n, ErrorAt str)

withErrorFrom :: (a -> String) -> Parser a -> Parser a
withErrorFrom = withErrorNFrom 0

withErrorNFrom :: Int -> (a -> String) -> Parser a -> Parser a
withErrorNFrom n errFn p =
  do value <- p
     withErrorN n (errFn value) $ pure $ value

withTransform :: (forall b. Parser b -> Parser b) -> Parser a -> Parser a
withTransform t = applyTransform $ Just t



applyTransformsErrors :: (forall b. [Maybe (Parser b -> Parser b)])
                      -> [Set (Int, ParseError)]
                      -> Parser a
                      -> Parser a
applyTransformsErrors ts es =
  applyTransformError (findJust ts) (mconcat es)


applyTransformError :: (forall b. Maybe (Parser b -> Parser b))
                    -> Set (Int, ParseError)
                    -> Parser a
                    -> Parser a
applyTransformError t e = applyTransform t . applyError e



applyTransform :: (forall b. Maybe (Parser b -> Parser b)) -> Parser a -> Parser a
applyTransform f p =  maybe p (\f' -> (f' p) {transform = f}) f

applyError :: Set (Int, ParseError) -> Parser a -> Parser a
applyError e p@(P _ _ e') = p {errors = e <> e'}

mkParser :: (Input -> ParseResult a) -> Parser a
mkParser p = P {parse = p, transform = Nothing, errors = Set.empty}
