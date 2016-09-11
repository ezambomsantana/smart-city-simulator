echo "Start collectl"

collectl -scj  -P -f/home/eduardo/ &

echo "Start SCSimulator"

cd /home/eduardo/Software/sim2/mock-simulators/smart-city/src/

strace -t -o teste_all -f make smart_city_run CMD_LINE_OPT="--batch"

echo "Finish collectl"

ps -ef | grep collectl | grep -v grep | awk '{print $2}' | xargs kill
