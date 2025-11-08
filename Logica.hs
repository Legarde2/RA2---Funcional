module Logica where

import qualified Data.Map as Map
import Data.Time (UTCTime)
import Inventario 

type ResultadoOperacao = (Inventario, LogEntry)

-- Função para adicionar um novo item
addItem :: UTCTime        
        -> String          
        -> String          
        -> Int           
        -> String          
        -> Inventario      
        -> Either String ResultadoOperacao 
addItem time newID newNome newQtde newCat inv =
    if Map.member newID inv
    then Left "Erro: Item com este ID já existe."
    else
        let newItem = Item 
                { itemID = newID
                , nome = newNome
                , quantidade = newQtde
                , categoria = newCat
                }
            newInv = Map.insert newID newItem inv
            logEntry = LogEntry
                { timestamp = time
                , acao = Add
                , detalhes = "Adicionado item ID: " ++ newID
                , status = Sucesso
                }
        in Right (newInv, logEntry)