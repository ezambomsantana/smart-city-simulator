# Smart City Simulator (SCSimulator)

SCSimulator is a large-scale, Smart City simulator with the aim of simulate many Smart City complex scenario such as traffic management, waste management, and smart grids. This simulator is built on top of Sim-Diasca, a general-purpose, large-scale, discrete-event simulator. 

To install Sim-Diasca is necessary to install the Erlang Virtual Machine. It can run in a Windows environment, but it runs better in Linux, mainly in Fedora based systems. You can find Sim-Diasca in https://github.com/softlab-ntua/bencherl/tree/master/app/sim-diasca.

## Installing Sim-Diasca and SCSimulator on Linux

To install Sim-Diasca on Linux follow these steps:

1) Install Erlang VM running the script install-erlang.sh that is in the /common/conf folder of the Sim-Diasca files.

2) Compile Sim-Diasca running the command make all in the root of Sim-Diasca folder.

3) Copy the files of the src folder of this repository in the folder /mock-simulators/smart-city.

4) Compile the SCSimulator sources running the command make all.

5) To run a example simulation run the command make smart_city_run

## Installing the Visualization Tool

To install the SCSimulator visualization tool, it is required to install the JDK (version 7+) and the Apache Tomcat web server (version 7+). This is a web-based application written in Java, the application access the Google Maps API and read Sim-Diasca files with the positions and values generated in the simulation.

## Manual



## Videos

Video with the simulator installation


Video with the simulator running (in Portuguese) - https://www.youtube.com/watch?v=iDcinyRxmTQ


