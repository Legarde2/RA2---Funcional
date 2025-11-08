module Relatorios where

import Inventario (LogEntry(..), StatusLog(..), acao, detalhes, status)
import Data.List (sortOn, groupBy, group, sort)
import Data.Time (UTCTime)

-- Função 1 Filtrar logs de erro
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro todosOsLogs = filter ehFalha todosOsLogs
  where
    ehFalha :: LogEntry -> Bool
    ehFalha log = case status log of
        Falha _ -> True
        Sucesso -> False
