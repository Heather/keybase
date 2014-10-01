@echo off

echo ---------------------------
echo   Dependencies check
echo ---------------------------

cabal install --only-dependencies

echo ---------------------------
echo   Configure
echo ---------------------------

cabal configure

echo ---------------------------
echo   Build
echo ---------------------------

cabal build

pause