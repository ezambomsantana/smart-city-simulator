<html>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<head>
<title>SCSimulator</title>
<meta name="viewport" content="initial-scale=1.0, user-scalable=no">
<meta charset="utf-8">
<style>
html, body, #map-canvas {
	height: 100%;
	margin: 0px;
	padding: 0px
}
</style>
<script src="https://maps.googleapis.com/maps/api/js?v=3.exp"></script>
<script type="text/javascript" src="http://code.jquery.com/jquery-1.10.1.min.js"></script>
<script>
	  

	var map;
	var cars = [];
	var buildings = [];
	var terminals = [];
	var semaphores = [];
	var buses = [];
	var sensors = [];
	var stops = [];
	
    function initialize() {

  	  var myLatlng = new google.maps.LatLng(-23.547225, -46.636808);
	  var mapOptions = {
	    zoom: 14,
	    center: myLatlng
	  }
	  map = new google.maps.Map(document.getElementById('map-canvas'), mapOptions);
	  var text = '';

		create_points();
		window.setInterval(refresh_map, 1000);
	  }
    
    

    
	function refresh_map() {
		create_points();
	}
		
	function create_points() {
		
	    
	    for (var j = 0; j < semaphores.length; j++) {
	    	semaphores[j].setMap(null);
	    }
    	semaphores = [];
	    
	    $.getJSON('/monitor/SemaphoreServlet', function(data) {  
	        data.forEach(function(x, i) {
	            var stat = data[i];
	            mark_semaphore(map, stat.lat1, stat.lon1, stat.color1, stat.lat2, stat.lon2, stat.color2);
	        });
	    });
	    
	    $.getJSON('/monitor/MapServlet', function(data) {  
	        data.forEach(function(x, i) {
	        	var stat = data[i];
	        	
	        	if (i < cars.length) {
		        	if (cars[i].getPosition().lat().toFixed(4) != stat.lat.toFixed(4)) {
		        		var m = mark(map, stat.lat, stat.lon, stat.speed);
		        		cars[i].setMap(null);
		        		cars[i] = null;
		        		cars[i] = m;
		        	} else if (cars[i].getPosition().lng().toFixed(4) != stat.lon.toFixed(4)) {
		        		var m = mark(map, stat.lat, stat.lon, stat.speed);
		        		cars[i].setMap(null);
		        		cars[i] = m;
		        	}	            
	        	} else {
	        		var m = mark(map, stat.lat, stat.lon, stat.speed);
	            	cars.push(m);
	        	}
	            
	        });
	    });
	    
	    $.getJSON('/monitor/BusServlet', function(data) {  
	        data.forEach(function(x, i) {
	            var stat = data[i];
	        	if (i < buses.length) {
		        	if (buses[i].getPosition().lat().toFixed(4) != stat.lat.toFixed(4)) {
		        		var m = mark_bus(map, stat.lat, stat.lon, stat.speed);
		        		buses[i].setMap(null);
		        		buses[i] = null;
		        		buses[i] = m;
		        	} else if (buses[i].getPosition().lng().toFixed(4) != stat.lon.toFixed(4)) {
		        		var m = mark_bus(map, stat.lat, stat.lon, stat.speed);
		        		buses[i].setMap(null);
		        		buses[i] = null;
		        		buses[i] = m;
		        	}	            
	        	} else {
	        		var m = mark_bus(map, stat.lat, stat.lon, stat.speed);
	            	buses.push(m);
	        	}
	        	
	        	
	            
	        });
	    });
	    
	    $.getJSON('/monitor/SensorServlet', function(data) {  
	        data.forEach(function(x, i) {
	            var stat = data[i];
	            if (i < sensors.length) {
	            	sensors[i].setLabel(stat.value);
	            } else {
	            	mark_sensor(map, stat.lat, stat.lon, stat.value);
	            }	            
	        });
	    });
	    
	    $.getJSON('/monitor/BuildingSensor', function(data) {  
	        data.forEach(function(x, i) {
	            var stat = data[i];
	            if (i < buildings.length) {
	            	buildings[i].setLabel(stat.value);
	            } else {
	            	mark_building(map, stat.lat, stat.lon, stat.water);
	            }	 	            
	        });
	    });

	    $.getJSON('/monitor/TerminalServlet', function(data) {  
	        data.forEach(function(x, i) {
	            var stat = data[i];
	            if (i < terminals.length) {
	            	terminals[i].setLabel("");
	            } else {
	            	mark_terminal(map, stat.lat, stat.lon, "");
	            }
	            
	        });
	    });
	    
	    $.getJSON('/monitor/BusStopServlet', function(data) {  
	        data.forEach(function(x, i) {
	            var stat = data[i];
	            if (i < terminals.length) {
	            	stops[i].setLabel("");
	            } else {
	            	mark_terminal(map, stat.lat, stat.lon, stat.numPassanger);
	            }
	            
	        });
	    });	    
		
	}

    function mark(map, lat, lon, text) {
    	  var myLatlng = new google.maps.LatLng(lat, lon);
    	  var marker = new google.maps.Marker({
  	      position: myLatlng,
  	      icon: 'http://maps.google.com/mapfiles/kml/pal4/icon31.png',
  	      map: map,
  	      title: text
  	 	 });
    	  return marker;
    	
      }

      function mark_sensor(map, lat, lon, text) {
    	  var myLatlng = new google.maps.LatLng(lat, lon);
    	  var marker = new google.maps.Marker({
  	      position: myLatlng,
  	      map: map,
  	      title: text,
  	      icon: 'http://maps.google.com/mapfiles/kml/pal4/icon40.png'
  	  });
    	sensors.push(marker);
      }

      function mark_building(map, lat, lon, text) {
    	  var myLatlng = new google.maps.LatLng(lat, lon);
    	  var marker = new google.maps.Marker({
  	      position: myLatlng,
  	      map: map,
  	      title: text,
  	      icon: 'http://maps.google.com/mapfiles/kml/pal3/icon31.png'
  	  });
    	  
    	  buildings.push(marker);
      }
      
      function mark_terminal(map, lat, lon, text) {
    	  var myLatlng = new google.maps.LatLng(lat, lon);
    	  var marker = new google.maps.Marker({
  	      position: myLatlng,
  	      map: map,
  	      title: text,
  	      icon: 'https://google-maps-icons.googlecode.com/files/bus-tour.png'
  	  });
    	terminals.push(marker);
      }      
   
      
      function mark_bus(map, lat, lon, text) {
    	  var myLatlng = new google.maps.LatLng(lat, lon);
    	  var marker = new google.maps.Marker({
  	      position: myLatlng,
  	      map: map,
  	      title: text,
  	      icon: 'https://google-maps-icons.googlecode.com/files/bus.png'
  	 		 });
    	return marker;
      }  
      
      
      
      function mark_semaphore(map, lat, lon, text, lat2, lon2, text2) {
    	  var myLatlng = new google.maps.LatLng(lat, lon);
    	  var myLatlng2 = new google.maps.LatLng(lat2, lon2);
    	  if (text == 'red') {
		      var marker = new google.maps.Marker({
		  	      position: myLatlng,
		  	      map: map,
		  	      title: text,
		  	      icon: 'http://labs.google.com/ridefinder/images/mm_20_red.png'
		  	  });
    		  var marker2 = new google.maps.Marker({
		  	      position: myLatlng2,
		  	      map: map,
		  	      title: text2,
		  	      icon: 'http://labs.google.com/ridefinder/images/mm_20_green.png'
		  	  });
    	  } else {
    		  var marker = new google.maps.Marker({
		  	      position: myLatlng,
		  	      map: map,
		  	      title: text,
		  	      icon: 'http://labs.google.com/ridefinder/images/mm_20_green.png'
		  	  });
		      var marker2 = new google.maps.Marker({
		  	      position: myLatlng2,
		  	      map: map,
		  	      title: text2,
		  	      icon: 'http://labs.google.com/ridefinder/images/mm_20_red.png'
		  	  });
    	  }
    	semaphores.push(marker);
    	semaphores.push(marker2);
      }
    
    	google.maps.event.addDomListener(window, 'load', initialize);

    </script>
</head>
<body>
	<div id="map-canvas"></div>


	<table>
		<tr>
			<td>Nome</td>
			<td>Logintude</td>
			<td>Latitude</td>
		</tr>

		<c:forEach var="bus" items="${lista}">
			<tr>
				<td><c:out value="${bus.letters}"></c:out></td>
				<td><c:out value="${bus.lat}"></c:out></td>
				<td><c:out value="${bus.lon}"></c:out></td>

			</tr>
		</c:forEach>
	</table>

</body>
</html>