# Run the simulator without the log monitor
make smart_city_run CMD_LINE_OPT="--batch"

# Get the system calls executed in the simulator 
strace -t -o teste_all -f make smart_city_run CMD_LINE_OPT="--batch"


# Get the summary of the system calls executed in the simulator
strace -c -o teste_all -f make smart_city_run CMD_LINE_OPT="--batch"
