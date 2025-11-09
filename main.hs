-- Módulos do projeto
import Inventario
import Logica

-- Módulos de Sistema
import qualified Data.Map as Map
import Data.Time (getCurrentTime, UTCTime)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)
import System.Exit (exitSuccess) 

-- Constantes para os nomes dos arquivos
invFile :: FilePath
invFile = "Inventario.dat"
logFile :: FilePath
logFile = "Auditoria.log"


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

loadLogs :: IO [LogEntry]
loadLogs = (do
    putStrLn $ "Carregando " ++ logFile ++ "..."
    conteudo <- readFile logFile
    let logs = map read (lines conteudo)
    putStrLn "Logs carregados com sucesso."
    return logs
    ) `catch` handleReadError []

handleReadError :: a -> IOException -> IO a
handleReadError defaultVal e = do
    putStrLn $ "Aviso: Arquivo não encontrado ou permissão negada. Iniciando com dados vazios."
    return defaultVal

-- Salva o inventário no disco
saveInventario :: Inventario -> IO ()
saveInventario inv = do
    writeFile invFile (show inv)
    putStrLn "Estado do inventário salvo em Inventario.dat."

saveLog :: LogEntry -> IO ()
saveLog logEntry = do
    appendFile logFile (show logEntry ++ "\n")
    putStrLn "Operação registrada em Auditoria.log."

mainLoop :: Inventario -> [LogEntry] -> IO ()
mainLoop inv logs = do
    putStr "\nComando (add, remove, update, report, sair): "
    comando <- getLine
    
    time <- getCurrentTime
    
    processCommand comando time inv logs

processCommand :: String -> UTCTime -> Inventario -> [LogEntry] -> IO ()
processCommand comando time inv logs = do
    case comando of
        "add" -> do
            putStr "ID do item: "
            id <- getLine
            putStr "Nome do item: "
            nome <- getLine
            putStr "Quantidade inicial: "
            qtdeStr <- getLine
            putStr "Categoria: "
            cat <- getLine

            case readMaybe qtdeStr of
                Nothing -> do
                    putStrLn "Erro: Quantidade deve ser um número."
                    mainLoop inv logs
                Just qtde -> do
                    let resultado = addItem time id nome qtde cat inv
                    handleResultado resultado inv logs

        "remove" -> do
            putStr "ID do item: "
            id <- getLine
            putStr "Quantidade a remover: "
            qtdeStr <- getLine

            case readMaybe qtdeStr of
                Nothing -> do
                    putStrLn "Erro: Quantidade deve ser um número."
                    mainLoop inv logs
                Just qtde -> do
                    let resultado = removerItem time id qtde inv
                    handleResultado resultado inv logs

        "update" -> do
            putStr "ID do item: "
            id <- getLine
            putStr "Nova quantidade total: "
            qtdeStr <- getLine

            case readMaybe qtdeStr of
                Nothing -> do
                    putStrLn "Erro: Quantidade deve ser um número."
                    mainLoop inv logs
                Just qtde -> do
                    let resultado = atualizarQuantidade time id qtde inv
                    handleResultado resultado inv logs
        
        "report" -> do
            mainLoop inv logs

        "sair" -> do
            putStrLn "Encerrando..."
            exitSuccess

        _ -> do
            putStrLn "Erro: Comando inválido."
            mainLoop inv logs

handleResultado :: Either String ResultadoOperacao -> Inventario -> [LogEntry] -> IO ()
handleResultado (Left erroMsg) inv logs = do
    -- Caso 1: Falha na lógica 
    putStrLn $ "Falha na operação: " ++ erroMsg
    
    time <- getCurrentTime
    let logFalha = LogEntry {
        timestamp = time,
        acao = QueryFail, 
        detalhes = erroMsg,
        status = Falha erroMsg
    }
    
    -- Salva apenas o log
    saveLog logFalha
    
    -- Continua o loop com o inventário antigo
    mainLoop inv (logFalha : logs)

handleResultado (Right (novoInv, logSucesso)) _ logs = do
    -- Caso 2: Sucesso na operação
    putStrLn "Operação bem-sucedida."
    
    -- Salva o novo inventário
    saveInventario novoInv
    saveLog logSucesso
    
    mainLoop novoInv (logSucesso : logs)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    inv <- loadInventario
    logs <- loadLogs
    
    putStrLn "\nSistema de Gerenciamento de Inventário "
    putStrLn "Comandos: add, remove, update, report, sair"
    
    mainLoop inv logs
