module Logica where

import qualified Data.Map as Map
import Data.Time (UTCTime)
import Inventario

type ResultadoOperacao = (Inventario, LogEntry)

-- Função para adicionar um novo item
additem :: UTCTime
        -> String
        -> String
        -> Int
        -> String
        -> Inventario
        -> Either String ResultadoOperacao
additem time newID newNome newQtde newCat inv =
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
removeltem :: UTCTime
            -> String
            -> Int
            -> Inventario
            -> Either String ResultadoOperacao
removeltem time itemID qtdeRemover inv =
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


-- Função pura para atualizar a quantidade de um item
updateQty :: UTCTime
                    -> String
                    -> Int
                    -> Inventario
                    -> Either String ResultadoOperacao
updateQty time itemID novaQtde inv =
    if novaQtde < 0
    then Left "Erro: Quantidade não pode ser negativa."
    else
        case Map.lookup itemID inv of
            Nothing -> Left "Erro: Item não encontrado para atualizar."
            Just item ->
                let qtdeAntiga = quantidade item
                    itemAtualizado = item { quantidade = novaQtde }
                    newInv = Map.insert itemID itemAtualizado inv
                    logEntry = LogEntry
                        { timestamp = time
                        , acao = Update
                        , detalhes = "Quantidade do item ID: " ++ itemID ++ " atualizada de " ++ show qtdeAntiga ++ " para " ++ show novaQtde
                        , status = Sucesso
                        }
                in Right (newInv, logEntry)
                
                


-- Tipos para a função de popular
type ListaItens = [(String, String, Int, String)]
type ResultadoPopular = (Inventario, [LogEntry]) -- Retorna os logs

-- Função auxiliar para adicionar itens em sequência
addSequencial :: UTCTime -> (String, String, Int, String) -> Either String ResultadoPopular -> Either String ResultadoPopular
addSequencial _ _ (Left err) = Left err -- Se já falhou, continua falhando
addSequencial time (id, nome, qtde, cat) (Right (inv, logs)) =
    case additem time id nome qtde cat inv of
        Left err -> Left err -- caso falho
        Right (novoInv, novoLog) -> Right (novoInv, novoLog : logs) -- caso sucesso

-- Função pura para popular o inventário
popularItens :: UTCTime -> ListaItens -> Inventario -> Either String ResultadoPopular
popularItens time lista invInicial =
    foldl (flip (addSequencial time)) (Right (invInicial, [])) lista