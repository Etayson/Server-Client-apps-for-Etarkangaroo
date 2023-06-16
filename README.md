# Server-Client-apps-for-Etarkangaroo (windows only)
## Server

To manage workfiles on server side use EtarkangarooServer.  
Server app sets search key, search ranges, size of DP and other things for client and merge work files to single work file.  
The use of a server app allows you to work more flexibly with working files.  
```
Usage:
-port       [optional] port on which clients are listening, default 8000
-checkdp    [optional] this flag alow check every DP from clients
-ht         [optional] hashtable size 2^, default value 25
-dp         [required] number of trailing zeros distinguished point
-wi         [optional] timer interval (in seconds) for autosaving ht/kangaroos on client side, can be overwritten by client app, default 7200
-beginrange [required] range start from
-endrange   [required] end range
-pub        [required] set single uncompressed/compressed pubkey for searching
Example:
EtarkangarooServer.exe -checkdp -dp 16 -wi 180 -beginrange 80000000000000000000 -endrange FFFFFFFFFFFFFFFFFFFF -pub 037E1238F7B1CE757DF94FAA9A2EB261BF0AEB9F84DBF81212104E78931C2A19DC
```
## Client
Require Etarkangaroo in the same folder https://github.com/Etayson/Etarkangaroo  
```
Usage:
-wi       [optional] timer interval for autosaving ht/kangaroos, without setting, the value from the server is used
-min      [optional] the minimum size (in MB) of a working file to send to the server
-name     [optional] name of client
-pool     [required] IP adress of server:port, default 127.0.0.1:8000
-grid     [Etarkangaroo settings]
-d        [Etarkangaroo settings]
-wmerge   [Etarkangaroo settings]
Example:
EtarkangarooClient.exe -pool 127.0.0.1:8000 -d 0 -grid 44,64 -wi 300 -wmerge -min 10
```
Purebasic v.5.31 required for compilation
