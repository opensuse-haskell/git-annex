module Annex.Wanted where

import Annex.Common

wantGet :: LiveUpdate -> Bool -> Maybe Key -> AssociatedFile -> Annex Bool
