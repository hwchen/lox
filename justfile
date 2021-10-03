watch-file-debug path:
     watchexec -r -w src -w {{path}} 'gyro build run -- {{path}}'

watch-repl-debug:
    watchexec -r -w src 'gyro build run'

watch-file path:
     watchexec -r -w src -w {{path}} 'gyro build -Drelease-fast=true run -- {{path}}'

watch-repl:
    watchexec -r -w src 'gyro build run'
