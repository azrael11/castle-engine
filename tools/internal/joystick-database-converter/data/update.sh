mv gamecontrollerdb.txt gamecontrollerdb.old
wget https://raw.githubusercontent.com/gabomdq/SDL_GameControllerDB/master/gamecontrollerdb.txt
if [ $? -eq 0 ]
then
  echo "Download successfull"
  rm gamecontrollerdb.old
else
  echo "Download failed"
  rm gamecontrollerdb.txt
  mv gamecontrollerdb.old gamecontrollerdb.txt
fi