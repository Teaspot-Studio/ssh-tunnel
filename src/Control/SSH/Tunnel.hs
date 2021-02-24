{-# LANGUAGE RecordWildCards #-}
module Control.SSH.Tunnel(
    SshTunnelConfig(..)
  , openSshTunnel
  , addFingerprints
  , SshTunnel
  , makeSshTunnel
  , makeSshTunnelSimple
  , closeSshTunnel
  , saveSshTunnel
  , loadSshTunnel
  ) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Managed
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import GHC.Generics
import Network.HTTP.Client as C
import Turtle
import Prelude hiding (FilePath)

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Configuration of SSH tunnel
data SshTunnelConfig = SshTunnelConfig {
  sshTunnelKey        :: Text -- ^ Path to ssh pem file
, sshTunnelPort       :: Int -- ^ Port that is used for local ssh proxy
, sshTunnelRemotePort :: Int -- ^ Port that is used on remote machine for vpn manager
, sshTunnelRemoteUser :: Text -- ^ Name of SSH user for tunneling
, sshTunnelRemoteNode :: Text -- ^ Host of SSH tunnel
, sshTunnelTempFolder :: Text -- ^ Place where we can place our temp files
} deriving (Generic, Show, Read)


-- | Open SSH tunnel and return settings for connection manager
--
-- Tunnel is created with:
-- @
-- ssh -f -N -M -S <master-socket> -i <pemfile> -L <localport>:127.0.0.1:<remoteport> <user>@<host>
-- @
--
-- Note: that the tunnel is created in background (-f) without a shell on remote host (-N)
-- and (-M -S <master-socket>) defines special socket that is used to terminate the tunnel.
--
-- How to use in your code:
-- @
-- import Control.Monad.Managed
-- import Control.SSH.Tunnel
-- import Network.HTTP.Client
-- import Network.HTTP.Client.TLS
--
-- with (openSshTunnel config tlsManagerSettings) $ \settings -> do
--   manager <- newManager settings
--   -- do things with manager
--   -- as soon you live the scope, tunnel will be down
-- @
openSshTunnel :: MonadManaged m => SshTunnelConfig -- ^ Configuration of connection
  -> ManagerSettings -- ^ Your manager settings that would be extended with proxy information
  -> m ManagerSettings -- ^ Extended client manager settings, tunnel termination action is handled by 'MonadManaged'
openSshTunnel cfg settings = fmap fst $ openSshTunnel' cfg settings

-- | Internal version that also returns path to master socket
openSshTunnel' :: MonadManaged m => SshTunnelConfig -- ^ Configuration of connection
  -> ManagerSettings -- ^ Your manager settings that would be extended with proxy information
  -> m (ManagerSettings, Text) -- ^ Extended client manager settings, tunnel termination action is handled by 'MonadManaged'
openSshTunnel' cfg mngSettings = using $ do
  -- open ssh tunnel
  masterSocketName <- genTempName (sshTunnelTempFolder cfg) "ssh_tunnel_master"
  isExists <- testfile $ Turtle.fromText masterSocketName
  when isExists $ rm $ Turtle.fromText masterSocketName
  res <- liftIO $ shell (sshCommand masterSocketName) empty
  case res of 
    ExitSuccess -> return ()
    excode -> fail $ "Failed to open SSH tunnel: " <> show excode

  -- configure proxy manager
  let proxySettings = useProxy $ C.Proxy "127.0.0.1" (sshTunnelPort cfg)
  let mngSettings' = managerSetProxy proxySettings mngSettings

  managed $ \k -> do
    r <- k (mngSettings', masterSocketName)
    ExitSuccess <- shell (closeTunnelCmd masterSocketName) empty
    return r
  where
    -- We open ssh in detached mode with master socket to be able to close the tunnel
    sshCommand :: Text -> Text
    sshCommand masterSocketName = "ssh -f -N -M "
      <> "-S \"" <> masterSocketName <> "\" "
      <> "-i \"" <> sshTunnelKey cfg <> "\" "
      <> "-L "
      <> showl (sshTunnelPort cfg) <> ":127.0.0.1:" <> showl (sshTunnelRemotePort cfg) <> " "
      <> sshTunnelRemoteUser cfg <> "@" <> sshTunnelRemoteNode cfg

-- Send exit command over master socket
closeTunnelCmd :: Text -> Text
closeTunnelCmd masterSocketName = "ssh "
  <> "-S \"" <> masterSocketName <> "\" "
  <> " -O exit 127.0.0.1"

-- | Generation of temporary name for a file
genTempName :: MonadIO m => Text -> Text -> m Text
genTempName folder template = do
  uuid <- liftIO UUID.nextRandom
  return $ folder <> "/" <> template <> "-" <> UUID.toText uuid

-- | Handy way to print into text
showl :: Show a => a -> Text
showl = pack . show

-- | SSH tunnel ID that can be used to send commands to it
data SshTunnel = SshTunnel {
  sshTunnelCloseMutex :: MVar ()
, sshTunnelSocketName :: Text
}

-- | Make a SSH tunnel, same as 'openSshTunnel', but handles all 'Managed' monad
-- stuff internally. As soon as 'SshTunnel' value is garbage collected, internal
-- ssh tunnel will be closed. Also you can use 'closeSshTunnel' to manually free
-- resources.
makeSshTunnel :: MonadIO m => SshTunnelConfig -- ^ Configuration of connection
  -> ManagerSettings -- ^ Your manager settings that would be extended with proxy information
  -> m (ManagerSettings, SshTunnel)  -- ^ Extended client manager settings and id that can be used to shut down the tunnel
makeSshTunnel cfg settings = do
  mvar <- liftIO newEmptyMVar
  settVar <- liftIO newEmptyMVar
  _ <- liftIO . forkIO $ with (openSshTunnel' cfg settings) $ \tunnelArgs -> do
    putMVar settVar tunnelArgs
    takeMVar mvar
  (settings', socketName) <- liftIO $ takeMVar settVar
  return (settings', SshTunnel mvar socketName)

-- | Helper, when you don't need manager in 'makeSshTunnelSimple'
makeSshTunnelSimple :: MonadIO m => SshTunnelConfig -> m SshTunnel
makeSshTunnelSimple cfg = fmap snd $ makeSshTunnel cfg defaultManagerSettings

-- | Closes given ssh tunnel, see 'makeSshTunnel'
closeSshTunnel :: MonadIO m => SshTunnel -> m ()
closeSshTunnel = liftIO . flip putMVar () . sshTunnelCloseMutex

-- | Write down info about tunnel into file to be able to close it from other
-- haskell program.
saveSshTunnel :: (MonadFail m, MonadIO m) => FilePath -> SshTunnel -> m ()
saveSshTunnel path SshTunnel{..} = do
  path' <- case Turtle.toText path of
    Left e -> fail $ "saveSshTunnel: cannot convert path to Text " ++ show e
    Right path' -> return $ unpack path'
  liftIO $ T.writeFile path' sshTunnelSocketName

-- | Read saved tunnel from file that was written by 'saveSshTunnel'
loadSshTunnel :: MonadIO m => FilePath -> m SshTunnel
loadSshTunnel path = liftIO $ do
  path' <- case Turtle.toText path of
    Left e -> fail $ "saveSshTunnel: cannot convert path to Text " ++ show e
    Right path' -> return $ unpack path'
  name <- T.readFile path'
  mvar <- liftIO newEmptyMVar
  _ <- liftIO . forkIO $ do
    takeMVar mvar
    ExitSuccess <- shell (closeTunnelCmd name) empty
    return ()
  return $ SshTunnel mvar name

-- | Read remote server fingerprints that can be added to known_hosts
getFingerprints :: MonadIO m => SshTunnelConfig -> m (ExitCode, Text)
getFingerprints SshTunnelConfig{..} = shellStrict ("ssh-keyscan -t rsa " <> sshTunnelRemoteNode) empty

-- | Add server fingerprints to given path
addFingerprints :: MonadIO m => SshTunnelConfig -> FilePath -> m ()
addFingerprints cfg path = liftIO $ do 
  (res, fingers) <- getFingerprints cfg 
  case res of 
    ExitSuccess -> return ()
    _ -> fail $ "Failed to read fingerprints: " <> show res
  cnt <- readTextFile path 
  writeTextFile path $ cnt <> fingers