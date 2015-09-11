# udon_client
udon_client handles the connections and command execuation of Udon.

## usage
### start client
```erlang
udon_client_app:start([{localhost, {"localhost", 6380, 10, 20}}])
```

### run command
```erlang
udon_storage:execuate(Command, {Bucket, Key}, Args)
```