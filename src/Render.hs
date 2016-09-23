module Render
       where

class ToString a where
  renderString :: a -> String

renderStringE :: (ToString a, ToString b) => Either a b -> String
renderStringE (Left x)  = renderString x
renderStringE (Right x) = renderString x

instance ToString a => ToString [a] where
  renderString =
    concat . map renderString
