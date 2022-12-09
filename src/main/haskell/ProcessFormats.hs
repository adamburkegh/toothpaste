module ProcessFormats where

import PetriNet
import ToString
import Text.XML.Light
import Data.Set (Set, toList, fromList)
import Data.Typeable

element :: String -> [Attr] -> [Content] -> Element
element elemName attrs elements = 
    Element (unqual elemName) attrs elements Nothing

idAttr :: String -> [Attr]
idAttr valStr = [Attr (unqual "id") valStr]

nameNode :: (Show a, Typeable a) => a -> Element
nameNode x = element "name" [] 
                [Elem (element "text" [] 
                             [Text (CData CDataText (toString x) Nothing)] ) ] 

propertyNode :: String -> String -> Content
propertyNode key contentText = 
    Elem (element "property" [Attr (unqual "key") key] 
            [Text (CData CDataText contentText Nothing)]  )


class (Show a) => XMLOutput a where
    toXML :: a -> String -> Element

instance (Show a, Ord a, Typeable a) => XMLOutput (Transition a) where
    toXML (Transition x nodeId) idStr = 
        element "transition" (idAttr nodeId) [Elem (nameNode x) ] 

instance (Show a, Ord a, Typeable a) => XMLOutput (Place a) where
    toXML (Place x nodeId) idStr = 
        element "place" (idAttr nodeId) [Elem (nameNode x)] 

instance (Show a, Ord a, Typeable a) => XMLOutput (Edge a) where
    toXML (ToPlace (Transition tr trId) (Place pl plId) ) idStr = 
            element "arc" 
                    [Attr (unqual "source") trId, 
                        Attr (unqual "target") plId,
                        Attr (unqual "id") idStr] 
                    [] 
    toXML (ToTransition (Place pl plId) (Transition tr trId) ) idStr = 
            element "arc" 
                    [Attr (unqual "source") plId, 
                        Attr (unqual "target") trId,
                        Attr (unqual "id") idStr] 
                    [] 

spnToolAttrs = [Attr (unqual "tool") "StochasticPetriNet",
                Attr (unqual "version") "0.2" ] 

instance (Show a, Ord a, Typeable a) => XMLOutput (WTransition a) where
    toXML (WTransition x nodeId w sil) idStr = 
        element "transition" (idAttr nodeId) 
                [Elem (nameNode x), 
                 Elem $ element "toolspecific"
                         spnToolAttrs
                         [propertyNode "distributionType" "IMMEDIATE",
                          propertyNode "weight" (show w),
                          propertyNode "invisible" (show sil),
                          propertyNode "priority" "0" ] ] 
-- TODO missing visible property


instance (Show a, Ord a, Typeable a) => XMLOutput (WEdge a) where
    toXML (WToPlace (WTransition tr trId w sil) (Place pl plId) ) idStr = 
            element "arc" 
                    [Attr (unqual "source") trId, 
                        Attr (unqual "target") plId,
                        Attr (unqual "id") idStr] 
                    [] 
    toXML (WToTransition (Place pl plId) (WTransition tr trId w sil) ) idStr = 
            element "arc" 
                    [Attr (unqual "source") plId, 
                        Attr (unqual "target") trId,
                        Attr (unqual "id") idStr] 
                    [] 


setToElemList :: (XMLOutput a) => Set a -> String -> [Element]
setToElemList x prefix = 
        zipWith 
            (\ idn el -> toXML el (prefix ++ show idn) ) 
            [1 .. length x] 
            (toList x)

pageElement :: [Element] -> Element
pageElement x = element "page" [Attr (unqual "id") "pg1"] (map Elem x) 

petriNetToXML :: (Show a, Ord a, Typeable a) => PetriNet a -> String -> Element
petriNetToXML (places,trans,edges) name = 
                element "pnml" 
                    [Attr (unqual "xmlns") 
                            "http://www.pnml.org/version-2009/grammar/pnml"]
                    [Elem $ element "net" 
                        (idAttr name)
                        [Elem $ pageElement 
                            (setToElemList trans "t"
                            ++ setToElemList places "p" 
                            ++ setToElemList edges "e" ) ]
                    ]

petriNetToString net name = ppElement $ petriNetToXML net name

weightedNetToXML :: WeightedNet -> String -> Element
weightedNetToXML net name = 
                element "pnml" 
                    [Attr (unqual "xmlns") 
                            "http://www.pnml.org/version-2009/grammar/pnml"]
                    [Elem $ element "net" 
                        (idAttr name)
                        [Elem $ element "toolspecific" spnToolAttrs [],
                         Elem $ pageElement 
                            (setToElemList (wntransitions net) "t"
                            ++ setToElemList (wnplaces net) "p" 
                            ++ setToElemList (wnedges net) "e" ) ]
                    ]

weightedNetToString :: WeightedNet -> String -> String
weightedNetToString net name = ppElement $ weightedNetToXML net name


-- basic demonstration

pI = Place "I" "pI"
pO = Place "O" "pO"

ta = Transition "a" "ta1"
itoa = ToTransition pI ta
atoi = ToPlace ta pO

ts = fromList [ta]
ps = fromList [pI :: Place [Char], pO :: Place [Char]]
es = fromList[itoa,atoi]
net = (ps,ts,es)

out = showElement $ toXML itoa "arc1"
out2 = ppElement $ petriNetToXML net "pnet1"

