## Run the simulator without the log monitor
make smart_city_run CMD_LINE_OPT="--batch"

## Get the system calls executed in the simulator 
strace -t -o teste_all -f make smart_city_run CMD_LINE_OPT="--batch"


## Get the summary of the system calls executed in the simulator
strace -c -o teste_all -f make smart_city_run CMD_LINE_OPT="--batch"

## Generate the Erlang traces

fprof:profile({file, "arq_origem.trace"}).

fprof:analyse([totals, {dest, "arq_dest.analysis"}]).

## Generate the KCachegrind formated file
./erlgrind arq_dest.analysis
