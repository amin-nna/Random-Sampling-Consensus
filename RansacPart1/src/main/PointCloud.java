/**
 @author  Amina Anna Mahamane 300227147
 @version 1.0
 @since   2023-02-01
 */
package main;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class PointCloud implements Iterable<Point3D>{
    private List<Point3D> listPointCloud = new ArrayList<Point3D>();


    /**
     * An empty constructor that constructs an empty point cloud
     */
    PointCloud(){
        listPointCloud = new ArrayList<Point3D>();
    }


    /**
     * A constructor from a xyz file
     * @param filename
     */
    PointCloud(String filename){
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            int i=0;
            while ((line = br.readLine()) != null) {
                String[] values = line.split("\t");
                if (i>0 ) {
                    addPoint(new Point3D(Double.parseDouble(values[0]),Double.parseDouble(values[1]),Double.parseDouble(values[2])));
                    //System.out.println("On a les coordonnées : ("+values[0]+ ","+values[1]+ ","+values[2] + ")") ;
                }
                i+=1;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

    }


    /**
     * A addPoint method that adds a point to the point cloud
     * @param pt
     */
    public void addPoint(Point3D pt) {
        listPointCloud.add(pt);
    }


    /**
     * A getPoint method that returns a random point from the cloud
     * @return
     */
    public Point3D getPoint(){
        int min = 0; // Minimum value of range
        int max = listPointCloud.size()-1; // Maximum value of range
        // Print the min and max
        //System.out.println("Random value in int from "+ min + " to " + max + ":");
        // Generate random int value from min to max
        int random_number = (int)Math.floor(Math.random() * (max - min + 1) + min);
        // Printing the generated random numbers
        //System.out.println(random_number);

        return listPointCloud.get(random_number);
    }


    /**
     * A save method that saves the point cloud into a xyz file
     * @param filename
     */
    public void save(String filename) {
        FileWriter writer;
        try {
            writer = new FileWriter(filename);
            writer.append("X");
            writer.append("\t");
            writer.append("Y");
            writer.append("\t");
            writer.append("Z");
            writer.append("\n");
            for (Point3D point: listPointCloud){
                String texte = point.getX()+"\t"+point.getY()+"\t"+point.getZ()+"\n";

                writer.append(texte);
            }
            writer.flush();
            writer.close();

        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }


    /**
     * An iterator method that returns an iterator to the points in the cloud.
     * @return
     */
    @Override
    public Iterator<Point3D> iterator() {
        return new PCIterator();
    }

    /**
     * @author amina_anna
     *
     */
    private class PCIterator implements Iterator<Point3D> {
        private int index = 0;

        @Override
        public boolean hasNext() {
            return index+1 < listPointCloud.size();
        }

        @Override
        public Point3D next() {
            if (hasNext())
            {
                index++;
                Point3D value = listPointCloud.get(index);
                return value;
            }
            System.out.println("No more positions available");
            return null;
        }

        @Override
        public void remove()
        {
            if (listPointCloud.size()==0) {
                System.out.println("Removals are not supported");
            }
            //We remove the actual point
            listPointCloud.remove(index);
            //We return to the precedent
            --index;
        }};



}