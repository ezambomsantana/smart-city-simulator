# Smart City Simulator (SCSimulator)

SCSimulator is a large-scale, Smart City simulator with the aim of simulate many Smart City complex scenario such as traffic management, waste management, and smart grids. This simulator is built on top of Sim-Diasca, a general-purpose, large-scale, discrete-event simulator. 

To install Sim-Diasca is necessary to install the Erlang Virtual Machine. It can be executed in a Windows environment, but it runs better on Linux, mainly on Fedora based systems. You can find and download Sim-Diasca in https://github.com/softlab-ntua/bencherl/tree/master/app/sim-diasca.

Video with the simulator running (in Portuguese) - https://www.youtube.com/watch?v=iDcinyRxmTQ

SCSimulator Documentation - https://github.com/ezambomsantana/smart-city-simulator/wiki

## Installing Sim-Diasca and SCSimulator on Linux

To install Sim-Diasca on Linux follow these steps:

1) Install Erlang VM running the script install-erlang.sh that is in the /common/conf folder of the Sim-Diasca files.

2) Compile Sim-Diasca running the command make all in the root of Sim-Diasca folder.

3) Copy the files in the src folder of this repository in the folder /mock-simulators/smart-city.

4) Compile the SCSimulator sources running the command make all.

5) To run a example simulation run the command make smart_city_run

## Installing the Visualization Tool (Monitor)

To install the SCSimulator visualization tool, it is required to install the JDK (version 7+) and the Apache Tomcat web server (version 7+). This is a web-based application written in Java, the application access the Google Maps API and read Sim-Diasca files with the positions and values generated in the simulation.

1) Get the code in this repository.

2) Get the Apache Tomcat Server (http://tomcat.apache.org/)

3) Install Apache Maven to build the project.

4) Run the command mvn clean install to generate the WAR file.

5) Get the WAR file and put in the webapps folder of the Tomcat Server.

6) Start Tomcat

7) Access in any browser the address http://localhost:8080/monitor/simulation.jsp

## Next Developments

1) Include the information of how many cars are in each street. - OK

2) Implement a OSM (Open Street Maps) parser to the creation of the city graph. - OK

3) Integrate the simulation with the visualization tool of MatSIM. - OK

4) Implement the bus lines of São Paulo

5) Make each node of the city graph be an actor

6) RUn the simulator in containers - OK

http://www.sumo.dlr.de/userdoc/Simulation/Basic_Definition.html
