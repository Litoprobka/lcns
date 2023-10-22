module Lcns.TH (makeGettersFor) where

import Language.Haskell.TH
import Optics ((.~))
import Optics.TH
import Relude

makeGettersFor :: [String] -> Name -> DecsQ
makeGettersFor fields =
  makeFieldLabelsWith $
    fieldLabelsRulesFor noPrefix & generateUpdateableOptics .~ False
 where
  noPrefix = fields <&> \x -> (x, x)