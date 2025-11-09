-- Módulos do Projeto
import Inventario
import Logica

-- Módulos de Sistema
import qualified Data.Map as Map
import Data.Time (getCurrentTime, UTCTime)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)

-- Constantes para os nomes dos arquivos
invFile :: FilePath
invFile = "Inventario.dat"

logFile :: FilePath
logFile = "Auditoria.log"

-- carregar o inventário do disco
loadInventario :: IO Inventario
loadInventario = (do
    putStrLn $ "Carregando " ++ invFile ++ "..."
    conteudo <- readFile invFile
    case readMaybe conteudo of
        Just inv -> do
            putStrLn "Inventário carregado com sucesso."
            return inv
        Nothing -> do
            putStrLn $ "Erro: " ++ invFile ++ " corrompido. Iniciando com inventário vazio."
            return Map.empty
    ) `catch` handleReadError Map.empty

-- carregar os logs do disco
loadLogs :: IO [LogEntry]
loadLogs = (do
    putStrLn $ "Carregando " ++ logFile ++ "..."
    conteudo <- readFile logFile
    let logs = map read (lines conteudo)
    putStrLn "Logs carregados com sucesso."
    return logs
    ) `catch` handleReadError []

-- Handler para catch
handleReadError :: a -> IOException -> IO a
handleReadError defaultVal e = do
    putStrLn $ "Aviso: Arquivo não encontrado ou permissão negada. Iniciando com dados vazios."
    return defaultVal

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    inv <- loadInventario
    logs <- loadLogs
    
    putStrLn "\nSistema de Gerenciamento de Inventário "
    putStrLn "Comandos: add, remove, update, report, sair"
