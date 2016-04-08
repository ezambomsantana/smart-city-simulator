# SCSimulator - An Open-Source, Scalable Smart City Simulator

SCSimulator is an open-source, scalable Smart City simulator with the aim of simulate many Smart City complex scenario such as traffic management, waste management, and smart grids. This simulator is built on top of Sim-Diasca, a general-purpose, large-scale, discrete-event simulator. 

The Sim-Diasca programming model is easy to understand, it is required to implement an Erlang class (using Erlang WOOPER extension) to each actor that will be used in the simulation.Figure 1 presents the class diagram of the current version of SCSimulator.


Figura 1 - Current version of SCSimulator.

The list bellow describes the actors included in each version of the simulation.

### Version 1.1

The classes of version 1.1 are:

#### BusStop

#### BusTerminal

#### Bus

### Version 1.0

The classes of version 1.0 are:

#### Car

The class Car has the aim to simulate the behaviour of a car moving in the city. It has the atributes name, route, position, and speed. Route is the id of the path between two points, the origin and the destination of the car, position is the current lat/long posistion of the car, and speed is it current speed.

The main methods of this class are: move, to chage the position of the vehicle, going to a foward position in the route, calculeted by its speed; wait_traffic_light, to send a signal to the car wait the traffic light become green to start moving again; and is_to_move to verify if the car is allowed to move.

#### Route

#### Building

The class Building represents points in the city that has two main functions: generate vehicles and use city resources such as water, eletricity, and gas. Its atributes are a name, the type of the building (house, company, hospital) and its position.
