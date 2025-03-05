/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2012  Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.defaulttype;

import java.awt.Color;

import capsis.kernel.GScene;
import jeeb.lib.defaulttype.TreeWithCrownProfile;
import jeeb.lib.util.Vertex3d;

/**
 * A simple description of a tree mainly for drawing. A NumberableTree may be
 * turned into several individual avatars for individuas spatialized level
 * viewing. This avatar has a regular crown description (symetric).
 * 
 * @author F. de Coligny - February 2012
 */
public class TreeAvatar implements TreeWithCrownProfile, SceneConnected {

	private int id;
	private String name;
	private double x; // m
	private double y; // m
	private double z; // m
	private int age;
	private double dbh; // cm
	private double height; // m
	private Color trunkColor;
	private double crownBaseHeight; // m
	private double crownRadius; // m
	private Color crownColor;
	private int crownType; // CONIC, SPHERIC
	private double[][] crownProfile;
	private Vertex3d relativeMin; // m
	private Vertex3d relativeMax; // m
	private Tree realTree;

	/**
	 * Default constructor.
	 */
	public TreeAvatar() {
	}

	public TreeAvatar(int id, String name, double x, double y, double z, double dbh, double height, Color trunkColor) {
		this.id = id;
		this.name = name;
		this.x = x;
		this.y = y;
		this.z = z;
		this.dbh = dbh;
		this.height = height;
		this.trunkColor = trunkColor;
	}

	public void setCrown(double crownBaseHeight, double crownRadius, Color crownColor, double[][] crownProfile) {
		this.crownBaseHeight = crownBaseHeight;
		this.crownRadius = crownRadius;
		this.crownColor = crownColor;
		this.crownProfile = crownProfile;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public double getX() {
		return x;
	}

	public void setX(double x) {
		this.x = x;
	}

	public double getY() {
		return y;
	}

	public void setY(double y) {
		this.y = y;
	}

	public double getZ() {
		return z;
	}

	public void setZ(double z) {
		this.z = z;
	}

	public int getAge() {
		return age;
	}

	public void setAge(int age) {
		this.age = age;
	}

	public double getDbh() {
		return dbh;
	}

	public void setDbh(double dbh) {
		this.dbh = dbh;
	}

	public double getHeight() {
		return height;
	}

	public void setHeight(double height) {
		this.height = height;
	}

	public Color getTrunkColor() {
		return trunkColor;
	}

	public void setTrunkColor(Color trunkColor) {
		this.trunkColor = trunkColor;
	}

	public double getCrownBaseHeight() {
		return crownBaseHeight;
	}

	public void setCrownBaseHeight(double crownBaseHeight) {
		this.crownBaseHeight = crownBaseHeight;
	}

	public double getCrownRadius() {
		return crownRadius;
	}

	public void setCrownRadius(double crownRadius) {
		this.crownRadius = crownRadius;
	}

	public Color getCrownColor() {
		return crownColor;
	}

	public void setCrownColor(Color crownColor) {
		this.crownColor = crownColor;
	}

	public int getCrownType() {
		return crownType;
	}

	public void setCrownType(int crownType) {
		this.crownType = crownType;
	}

	public double[][] getCrownProfile() {
		return crownProfile;
	}

	public void setCrownProfile(double[][] crownProfile) {
		this.crownProfile = crownProfile;
	}

	public Vertex3d getRelativeMin() {
		return relativeMin;
	}

	public void setRelativeMin(Vertex3d relativeMin) {
		this.relativeMin = relativeMin;
	}

	public Vertex3d getRelativeMax() {
		return relativeMax;
	}

	public void setRelativeMax(Vertex3d relativeMax) {
		this.relativeMax = relativeMax;
	}

	public Tree getRealTree() {
		return realTree;
	}

	public void setRealTree(Tree realTree) {
		this.realTree = realTree;
	}

	// fc-7.6.2018
	@Override
	public GScene getScene() {
		return realTree == null ? null : realTree.getScene();
	}

	public String toString() {
		String rt = "";
		if (realTree != null)
			rt = " for realTree: " + realTree.toString();

		return "TreeAvatar" + rt + " x,y,z: " + x + "," + y + "," + z;
	}

}
