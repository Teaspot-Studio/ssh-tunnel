ssh-tunnel
==========

Small library that allows to create SSH tunnel (SOCKS proxy) from Haskell and proxy
[http-client](http://hackage.haskell.org/package/http-client).

Tunnel is created with:
```
ssh -f -N -M -S <master-socket> -i <pemfile> -L <localport>:127.0.0.1:<remoteport> <user>@<host>
```

Note: that the tunnel is created in background (-f) without a shell on remote host (-N)
and (-M -S <master-socket>) defines special socket that is used to terminate the tunnel.

How to use in your code:
```haskell
import Control.Monad.Managed
import Control.SSH.Tunnel
import Network.HTTP.Client
import Network.HTTP.Client.TLS

with (openSshTunnel config tlsManagerSettings) $ \settings -> do
 manager <- newManager settings
 -- do things with manager
 -- as soon you live the scope, tunnel will be down
```

Also, you can find `addFingerprints` function useful for adding fingerprints to
known hosts on first connect. 
