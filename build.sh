set -e

stack test

# This doesn't work with antivirus on
# stack build --copy-bins --local-bin-path bin

# workaround
bp=`stack path --local-install-root`
sleep 3
rm -f hbin/toothpaste.exe
cp $bp/bin/toothpaste.exe hbin

./gradlew test distZip


