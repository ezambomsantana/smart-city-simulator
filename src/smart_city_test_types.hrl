% Copyright (C) 2008-2014 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% List of common types defined for Soda-Test.



% Duration, in minutes:
%
% Note: a better approach would be to specify a duration in minutes (of virtual
% time), and to convert it at simulation time into a number of ticks (depending
% on the simulation frequency).
%


-type sensor_lat() :: float().
-type sensor_long() :: float().
-type sensor_value() :: float().
-type sensor_data_interval() :: float().
-type car_lat() :: float().
-type car_long() :: float(). 
-type car_speed() :: float().
-type car_position() :: float().
-type statio_lat() :: float(). 
-type station_long() :: float().
-type route() :: [integer()].
-type car_index() :: integer().



