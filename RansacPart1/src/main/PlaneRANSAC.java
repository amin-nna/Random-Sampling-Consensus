/**
 * As we should repeat 2 to 5 until we are confident to have found the dominant plane with a confidence of 99%
 * The value of percentage of points on plane that gives us a computation of numberOfIterations between 2 and 5
 * is between 0.82 <= percentageOfPointsOnPlane <= 0.95.
 * With percentageOfPointsOnPlane=0.85 we are having 4 iterations.
 *
 *  @author Amina Anna Mahamane 300227147
 *  @version 1.0
 *  @since 2023-02-01
 *
 */
package main;
import java.util.Iterator;

public class PlaneRANSAC {
    private double epsilon;
    private Plane3D plane;
    private PointCloud nuage;

    public PlaneRANSAC() {

    }

    /**
     * @param pc
     */
    public PlaneRANSAC(PointCloud pc) {
        nuage = pc;
    }

    /**
     * @param eps
     */
    public void setEps(double eps) {
        epsilon = eps;
    }

    /**
     * @return
     *      The value of epsilon
     */
    public double getEps() {return epsilon;}


    /**
     * A method that returns the estimated number of iterations required to obtain a certain level
     * of confidence to identify a plane made of a certain percentage of points
     *
     * @param confidence
     * @param percentageOfPointsOnPlane
     * @return
     *      The number of iterations we have to do to make sure we have found a dominant plane
     */
    public int getNumberOfIterations(double confidence, double percentageOfPointsOnPlane) {
        int intNumberOfIterations = (int) (Math.log(1-confidence)/ Math.log(1-(Math.pow(percentageOfPointsOnPlane, 3))));
        System.out.println("Epsilon : " + epsilon+" Confidence : " + confidence+" Percentage of points on plane : " + percentageOfPointsOnPlane+" Number of iterations : " + intNumberOfIterations);

        return intNumberOfIterations;}


    /**
     * A run method that runs the RANSAC algorithm for identifying the dominant plane of the point cloud
     * (only one plane)
     * filename being the xyz file that will contain the points of the dominant plane. This method will
     * also remove the plane points from the point cloud
     *
     * @param numberOfIterations
     * @param filename
     */
    public void run(int numberOfIterations,String filename) {

        //Best support is set to 0
        int indice = 0;
        Plane3D actualPlane;
        nuage = new PointCloud(filename);
        Iterator<Point3D> iter = nuage.iterator();
        Point3D point;


        while (numberOfIterations > 0) {
            //Best number of points
            int count = 0;

            //Random plane with 3 points
            actualPlane = new Plane3D(nuage.getPoint(), nuage.getPoint(), nuage.getPoint());

            while(iter.hasNext()) {
                point = iter.next();
                if (actualPlane.getDistance(point)<getEps()) {
                    //This point is within the wanted distance to the plane
                    count++;
                }
            }


            if ( count > indice ) {
                //Update best plane so far
                indice = count;
                plane = actualPlane;
            }

            numberOfIterations--;
        }


        iter = nuage.iterator();
        int countable = 0;
        while(iter.hasNext()) {
            point = iter.next();
            countable++;
            if (plane.getDistance(point)>=getEps()) {
                iter.remove();
                countable--;
            }
        }

        nuage.save(filename.substring(0,filename.indexOf('.'))+"_p1"+".xyz");
        System.out.println("We found "+countable+" points.");


    }

    public static void main(String args[]) {
        //findClusters("PointCloud1.csv",0.9,10);

        String nomDuFichier = "PointCloud1.xyz";

        try {
            nomDuFichier = args[0];

        }
        catch(ArrayIndexOutOfBoundsException e) {
            System.out.println("Le programme va tourner avec le fichier par défaut");
        }
        System.out.println("Nom du fichier : "+ nomDuFichier);

        PlaneRANSAC Ransac = new PlaneRANSAC();
        Ransac.setEps(0.9);

        for ( int repeat = 1  ;repeat <4 ; repeat++) {

            Ransac.run(Ransac.getNumberOfIterations(0.99,0.85),nomDuFichier);

            Ransac.nuage.save(nomDuFichier.substring(0,nomDuFichier.indexOf('.'))+"_p"+repeat+".xyz");
        }


    }

}


