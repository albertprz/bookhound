module Bookhound.Parser (Parser(parse), ParseResult(..), ParseError(..), mkParser, runParser, throwError, andThen, exactly, eof , lookAhead , notFollowedBy, both, choice, anyOf, allOf, anyChar, satisfy, withTransform, withError, withErrorN, except) where

import           Bookhound.Utils.Foldable  (findJust)
import           Control.Applicative       (Alternative (..), liftA2)
import           Control.Monad             (MonadPlus)
import           Control.Monad.Error.Class (MonadError (..))
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text, pack, unpack)
import qualified Data.Text                 as Text

data Parser a
  = P
      { parse     :: Input -> ParseResult a
      , transform :: forall b. Maybe (Parser b -> Parser b)
      , errors    :: Set (Int, ParseError)
      }

instance Functor Parser where
  fmap f (P p t e) = applyTransformError t e $
    mkParser (fmap f . p)

instance Applicative Parser where
  pure a      = mkParser (`Result` a)
  liftA2 f (P p t e) mb@(P _ t' e') =
    applyTransformsErrors [t, t'] [e, e'] $ mkParser \x ->
      case p x of
        Result i a -> parse (f a <$> mb) i
        Error pe   -> Error pe

instance Monad Parser where
  (>>=) (P p t e) f =
    applyTransformError t e $ mkParser \x ->
      case p x of
        Result i a -> parse (f a) i
        Error pe   -> Error pe

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty

instance Alternative Parser where
  (<|>) (P p t e) (P p' t' e') =
    applyTransformsErrors [ t, t' ] [ e, e' ] $
      mkParser
        \x -> case p x of
          Error _ -> p' x
          result  -> result
  empty = mkParser \i ->
    if Text.null i then
      Error UnexpectedEof
    else
      Error $ ExpectedEof i

instance MonadPlus Parser

instance MonadError ParseError Parser where
  throwError = mkParser . const . Error
  catchError p errFn = mkParser
        \x -> case parse p x of
          Error err -> parse (errFn err) x
          result    -> result

anyChar :: Parser Char
anyChar = mkParser $
   maybe (Error UnexpectedEof) (uncurry $ flip Result) . Text.uncons

runParser :: Parser a -> Input -> Either [ParseError] a
runParser p@(P _ _ e) i = toEither $ parse (exactly p) i
  where
    toEither = \case
      Result _ a -> Right a
      Error pe   -> Left $ filter hasPriorityError [pe]
        <> (snd <$> reverse (Set.toList e))
        <> filter (not . hasPriorityError) [pe]

andThen :: Parser String -> Parser a -> Parser a
andThen p1 p2@(P _ t e) = applyTransformError t e $
  mkParser (\i -> parse p2 $ either (const i) pack (runParser p1 i))

exactly :: Parser a -> Parser a
exactly p = p <* eof

eof :: Parser ()
eof = mkParser
  \i ->
    if i == mempty then
      Result i ()
    else
      Error $ ExpectedEof i

lookAhead :: Parser a -> Parser a
lookAhead (P p t e) =
  applyTransformError t e $
    mkParser
      \x -> case p x of
        Result _ a -> Result x a
        err        -> err

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (P p t e) =
  applyTransformError t e $
    mkParser
      \x -> case p x of
        Result _ _ -> Error UnexpectedEof
        _          -> Result x ()

choice :: Foldable f => f (Parser a) -> Parser a
choice = anyOf

anyOf :: Foldable f => f (Parser a) -> Parser a
anyOf = foldl (<|>) empty

allOf :: Foldable f => f (Parser a) -> Parser a
allOf = foldl both (pure undefined)

both :: Parser a -> Parser a -> Parser a
both (P p t e) (P p' t' e') =
  applyTransformsErrors [ t, t' ] [ e, e' ] $
    mkParser
      \x -> case p x of
        Result _ _ -> p' x
        err        -> err

except :: Parser a -> Parser a -> Parser a
except (P p t e) (P p' _ _) = applyTransformError t e $ mkParser (
  \x -> case p' x of
    Result _ _ -> Error $ NoMatch "except"
    Error _    -> p x
  )

satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy cond ma = do
  c2 <- ma
  if cond c2 then
    pure c2
  else
    empty

withError :: String -> Parser a -> Parser a
withError = withErrorN 0

withErrorN :: Int -> String -> Parser a -> Parser a
withErrorN n str = applyError . Set.singleton $ (n, ErrorAt str)


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


hasPriorityError :: ParseError -> Bool
hasPriorityError (ErrorAt _) = True
hasPriorityError _           = False

data ParseResult a
  = Result Input a
  | Error ParseError
  deriving (Eq)

instance Functor ParseResult where
  fmap f (Result i a) = Result i (f a)
  fmap _ (Error pe)   = Error pe


instance Show a => Show (ParseResult a) where
  show (Result i a) = "Pending: " <> " >" <> unpack i <> "< "
                                  <> "\n\nResult: \n" <> show a
  show (Error err)  = show err

data ParseError
  = UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | UnexpectedString String
  | NoMatch String
  | ErrorAt String
  deriving (Eq, Ord)

instance Show ParseError where
  show UnexpectedEof        = "Unexpected end of stream"
  show (ExpectedEof i)      = "Expected end of stream, but got "
                               <> ">" <> unpack i <> "<"
  show (UnexpectedChar c)   = "Unexpected anyChar: "
                               <> "[" <> show c <> "]"
  show (UnexpectedString s) = "Unexpected string: "
                               <> "[" <> s <> "]"
  show (NoMatch s)          = "Did not match condition: " <> s
  show (ErrorAt s)          = "Error at " <> s

type Input = Text
