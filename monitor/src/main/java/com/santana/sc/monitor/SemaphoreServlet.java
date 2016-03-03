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
import entities.Semaphore;

/**
 * Servlet implementation class BuildingSensor
 */
public class SemaphoreServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public SemaphoreServlet() {
        super();
        // TODO Auto-generated constructor stub
    }
    
	private static final List<Semaphore> listSemaphore = new ArrayList<Semaphore>();

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		listSemaphore.clear();

		Files.walk(Paths.get("/home/eduardo/sc-monitor/locations/semaphores")).forEach(filePath -> {
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

						if (nNode.getNodeType() == Node.ELEMENT_NODE) {

							Element eElement = (Element) nNode;

							String color1 = eElement.getElementsByTagName("state1").item(0).getTextContent();
							float lat1 = Float.parseFloat(eElement.getElementsByTagName("lat1").item(0).getTextContent());
							float lon1 = Float
									.parseFloat(eElement.getElementsByTagName("long1").item(0).getTextContent());
							Semaphore semaphore = new Semaphore();
							semaphore.setLat1(lat1);
							semaphore.setLon1(lon1);
							semaphore.setColor1(color1);
							
							String color2 = eElement.getElementsByTagName("state2").item(0).getTextContent();
							float lat2 = Float.parseFloat(eElement.getElementsByTagName("lat2").item(0).getTextContent());
							float lon2 = Float
									.parseFloat(eElement.getElementsByTagName("long2").item(0).getTextContent());
							semaphore.setLat2(lat2);
							semaphore.setLon2(lon2);
							semaphore.setColor2(color2);
							
							listSemaphore.add(semaphore);

						}
					}
				} catch (Exception e) {
					e.printStackTrace();
				}

			}
		});
		PrintWriter out = response.getWriter();
		String json = new Gson().toJson(listSemaphore);
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
