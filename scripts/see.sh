
f=$1

./gradlew pnmlToDot --args var/${f}.pnml

scripts/rundot.sh var/${f}.dot

echo ${f}_dot.png

start var/${f}_dot.png

