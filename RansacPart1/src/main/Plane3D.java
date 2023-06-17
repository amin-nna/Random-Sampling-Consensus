/**
 @author  Amina Anna Mahamane 300227147
 @version 1.0
 @since   2023-02-01
 */
package main;

public class Plane3D {
    private double aX;
    private double bY;
    private double cZ;
    private double dC;

    /**
     * @param p1
     * @param p2
     * @param p3
     */
    public Plane3D(Point3D p1, Point3D p2, Point3D p3) {
        double a1 = p2.getX() - p1.getX();
        double b1 = p2.getY() - p1.getY();
        double c1 = p2.getZ() - p1.getZ();
        double a2 = p3.getX() - p1.getX();
        double b2 = p3.getY() - p1.getY();
        double c2 = p3.getZ() - p1.getZ();
        aX = b1 * c2 - b2 * c1;
        bY = a2 * c1 - a1 * c2;
        cZ = a1 * b2 - b1 * a2;
        dC = (- aX * p1.getX() - bY * p1.getY() - cZ * p1.getZ());

    }

    /**
     * @param a
     * @param b
     * @param c
     * @param d
     */
    public Plane3D(double a, double b, double c, double d) {
        aX = a;
        bY = b;
        cZ = c;
        dC = d;

    }

    /**
     * @param pt
     * @return
     *      The orthogonal distance of th point to the plane
     */
    public double getDistance(Point3D pt) {
        double d = Math.abs((aX * pt.getX() + bY *
                pt.getY() + cZ * pt.getZ() + dC));
        double e = (float)Math.sqrt(aX * aX + bY *
                bY + cZ * cZ);
        return d/e;

    }

    /**
     * @return
     *      The equation of the plane
     */
    public String toString() {
        return "equation of plane is " + aX + " x + " + bY + " y + " + cZ + " z + " + dC + " = 0";
    }

}
