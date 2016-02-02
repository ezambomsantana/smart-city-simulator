# Smart City Simulator

To run the simulator is necessary to install Sim-Diasca, a general-purpose, large-scale, discrete-event simulator.

To install Sim-Diasca is necessary to install the Erlang Virtual Machine. It can run in a Windows environment, but it runs better in Linux, mainly in Fedora based systems. You can find the Sim-Diasca in https://github.com/softlab-ntua/bencherl/tree/master/app/sim-diasca.

## Installing Sim-Diasca in Linux

To install Sim-Diasca in Linux follow these steps:

1) Install Erlang VM running the script install-erlang.sh that is in the /common/conf folder of the Sim-Diasca files.

2) Compile Sim-Diasca running the command make run in the root of Sim-Diasca folder.

3) Copy the files of the src folder of this repository in the folder /mock-simulators/smart-city.

4) Compile the sorces of the Smart City simulator running the command make run.

5) To run a simulation run the command make smart_city_run




