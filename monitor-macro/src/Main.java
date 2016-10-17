package main;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JFrame;

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.FRLayout;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.UndirectedSparseGraph;
import edu.uci.ics.jung.visualization.VisualizationViewer;

public class Main {

	public static void main(String[] args) throws InterruptedException, IOException {

		JFrame jf = new JFrame();
		
		int [][] matriz = {
				{0,1,0,0,0,0,0,0,0,0,0},
				{1,0,1,1,0,0,0,0,0,0,0},
				{0,1,0,0,0,0,1,0,0,0,0},
				{0,1,0,0,1,0,0,0,0,0,0},
				{0,0,0,1,0,0,1,0,0,0,1},
				{0,0,0,0,0,0,0,0,0,0,0},
				{0,0,1,0,1,0,0,1,1,1,0},
				{0,0,0,0,0,0,1,0,1,0,0},
				{0,0,0,0,0,0,1,1,0,1,0},
				{0,0,0,0,0,0,1,1,0,0,1},
				{0,0,0,0,1,0,0,0,0,1,0}
		};

		Graph g = new UndirectedSparseGraph();
		for (int i = 0; i < matriz.length; i++) {
			g.addVertex(i);
		}

		for (int i = 0; i < matriz.length; i++) {
			for (int j = i; j < matriz[i].length; j++) {
				if (matriz[i][j] != 0) {
					String nome = i + " -> " + j;
					g.addEdge(nome, i, j);
				}
			}
		}

		VisualizationViewer vv = new VisualizationViewer(new FRLayout(g));
		jf.getContentPane().add(vv);
		jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		FileReader fileReader = new FileReader("/home/eduardo/log_sc_simulator.log");
		BufferedReader reader = new BufferedReader(fileReader);

		List<String> linhas = new ArrayList<String>();
		String data = null;
		while ((data = reader.readLine()) != null) {
			linhas.add(data);
		}

		String [] valuesVertex = new String[matriz.length];
		
		for (int i = 0 ; i < valuesVertex.length; i++) {
			valuesVertex[i] = "0";
		}
		

		jf.pack();
		jf.setVisible(true);
		
		for (int f = 0; f < linhas.size(); f++) {
			
			String linha = linhas.get(f);
			if (linha.contains("city")) {
				System.out.println("Novo tick: " + linha);
			} else if (linha.contains("vertex")) {	
				
				String [] values = linha.split(",");
				
				
				String vertex = values[1];
				

				int index = Integer.parseInt(vertex.substring(1)) - 1;
								
				String value = values[3];				
				valuesVertex[index] = value;

				vv.repaint();

				vv.getRenderContext().setVertexLabelTransformer(new Transformer<Integer, String>() {
					@Override
					public String transform(Integer e) {					
						return "r" + String.valueOf(e) + ":" + valuesVertex[e];
					}
				});

	
			} else if (linha.contains("car")) {
				System.out.println(linha);
			}			

			Thread.sleep(100);

		}

	}

}
