package entities;

public class Building {

	private String name;
	private float lat;
	private float lon;
	private String energy;
	private String water;
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public float getLat() {
		return lat;
	}
	public void setLat(float lat) {
		this.lat = lat;
	}
	public float getLon() {
		return lon;
	}
	public void setLon(float lon) {
		this.lon = lon;
	}
	public String getEnergy() {
		return energy;
	}
	public void setEnergy(String energy) {
		this.energy = energy;
	}
	public String getWater() {
		return water;
	}
	public void setWater(String water) {
		this.water = water;
	}

}
