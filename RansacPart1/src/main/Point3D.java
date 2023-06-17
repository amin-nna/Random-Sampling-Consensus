/**
 @author  Amina Anna Mahamane 300227147
 @version 1.0
 @since   2023-02-01
 */
package main;


public class Point3D {

    private double x;
    private double y;
    private double z;

    /**
     * @param a
     * @param o
     * @param h
     */
    public Point3D(double a, double o, double h){
        x= a;
        y= o;
        z= h;
    }


    /**
     * @return
     *      The value of the x coordinate
     */
    public double getX() {
        return x;
    }

    /**
     * @return
     *      The value of the y coordinate
     */
    public double getY() {
        return y;
    }

    /**
     * @return
     *      The value of the z coordinate
     */
    public double getZ() {
        return z;
    }

}
