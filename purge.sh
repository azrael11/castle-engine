# Remove compiler garbage, backups and etc.

find -type d -name 'backup' -prune -exec rm -rf {} \;
find -type d -name 'lib' -prune -exec rm -rf {} \;
find . -name '*.dbg' -delete
rm -rf castle-engine-output
rm link.res

# Remove compiled executables

rm JoystickDetector
rm CastleInternalJoystickDatabaseConverter
rm *.exe
