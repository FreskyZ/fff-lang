REM cargo test performance counter helper
echo time
cargo clean > tempout 2> temperr
cargo build > tempout 2> temperr
del tempout
del temperr