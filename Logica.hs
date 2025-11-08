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

-- Função pura para remover um item
removerItem :: UTCTime         
            -> String         
            -> Int             
            -> Inventario      
            -> Either String ResultadoOperacao
removerItem time itemID qtdeRemover inv =
    case Map.lookup itemID inv of
        -- Caso 1: Item não encontrado
        Nothing -> Left "Erro: Item não encontrado."

        -- Caso 2: Item encontrado
        Just item ->
            if (quantidade item) < qtdeRemover
            then Left "Erro: Estoque insuficiente."
            else
                let novaQtde = (quantidade item) - qtdeRemover
                    itemAtualizado = item { quantidade = novaQtde }
                    newInv = Map.insert itemID itemAtualizado inv
                    logEntry = LogEntry
                        { timestamp = time
                        , acao = Remove
                        , detalhes = "Removido " ++ show qtdeRemover ++ " unidades do item ID: " ++ itemID ++ ". Nova qtde: " ++ show novaQtde
                        , status = Sucesso
                        }
                in Right (newInv, logEntry)