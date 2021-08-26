watch-file path:
     watchexec -r -w src -w {{path}} 'gyro build run -- {{path}}'

watch-repl:
    watchexec -r -w src 'gyro build run'
