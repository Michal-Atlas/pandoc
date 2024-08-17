module Text.Pandoc.Writers.Gemtext (writeGemtext)
 where
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Options (WriterOptions (..), def)
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Writers.RST
import Data.Either
import Text.Pandoc.Class
import qualified Data.Map as M
import Debug.Trace

inlineToGem Space = T.pack " "
inlineToGem (Str s) = s
inlineToGem SoftBreak = T.pack "\n"
inlineToGem e = error $ show e

inlinesToText :: [Inline] -> Text
inlinesToText = query inlineToGem

blockToGem :: PandocMonad m => Block -> m Text
blockToGem (Para n) = return $ inlinesToText n <> T.pack "\n\n"
blockToGem (Header lvl l inlines) = return $ T.pack "\n" <> T.pack (replicate lvl '#')
  <> T.pack " " <>
  inlinesToText inlines
  <> T.pack "\n\n"
blockToGem (CodeBlock l body) = return $ T.pack "```\n" <> body <> T.pack "\n```\n"
blockToGem t@(Table _ _ _ _ _ _) = do
  rstt <- writeRST def (Pandoc (Meta M.empty) [t])
  return $ T.pack "```\n" <> rstt <> T.pack "```\n"
blockToGem e = error $ show e

writeGemtext :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeGemtext opts (Pandoc meta blocks) = do
  gemBs <- mapM blockToGem blocks
  return $ foldl (<>) (T.pack "") gemBs
