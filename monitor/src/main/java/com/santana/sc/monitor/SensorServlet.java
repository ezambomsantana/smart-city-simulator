package com.santana.sc.monitor;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.google.gson.Gson;

import entities.Building;
import entities.Car;
import entities.Sensor;

/**
 * Servlet implementation class SensorServlet
 */
public class SensorServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public SensorServlet() {
        super();
        // TODO Auto-generated constructor stub
    }

	private static final List<Sensor> listSensor = new ArrayList<Sensor>();	
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		listSensor.clear();

		
		Files.walk(Paths.get("/home/eduardo/sc-monitor/locations/sensors")).forEach(filePath -> {
			if (Files.isRegularFile(filePath)) {
				try {
					File fXmlFile = filePath.toFile();
					DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
					DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
					Document doc = dBuilder.parse(fXmlFile);

					doc.getDocumentElement().normalize();
					NodeList nList = doc.getElementsByTagName("locations");

					

					for (int temp = 0; temp < nList.getLength(); temp++) {

						Node nNode = nList.item(temp);

						System.out.println("\nCurrent Element :" + nNode.getNodeName());

						if (nNode.getNodeType() == Node.ELEMENT_NODE) {

							Element eElement = (Element) nNode;

							String value = eElement.getElementsByTagName("value").item(0).getTextContent();
							float lat = Float.parseFloat(eElement.getElementsByTagName("lat").item(0).getTextContent());
							float lon = Float.parseFloat(eElement.getElementsByTagName("long").item(0).getTextContent());
							Sensor sensor = new Sensor();
							sensor.setLat(lat);
							sensor.setLon(lon);
							sensor.setValue(value);
							listSensor.add(sensor);
							
						}
					}
				} catch (Exception e) {
					e.printStackTrace();
				}

			}
		});

		PrintWriter out = response.getWriter();
		String json = new Gson().toJson(listSensor);
		  response.setContentType("application/json");
		  response.setCharacterEncoding("UTF-8");
		  out.write(json);
		
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

}
