REM cargo test performance counter helper
SET TEMP_PROMPT=%PROMPT%
SET PROMPT=[$T]$_$$$S
cargo clean > tempout 2> temperr
CLS
cargo test > tempout 2> temperr
DEL tempout temperr
SET PROMPT=%TEMP_PROMPT%