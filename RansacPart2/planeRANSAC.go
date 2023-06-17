package main

import (
	"bufio"
	"fmt"
	"math"
	"math/rand"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"
)

// Point3D represents a point in 3D space
type Point3D struct {
	X, Y, Z float64
}

// Plane3D represents a plane in 3D space
type Plane3D struct {
	A, B, C, D float64
}

// Plane3DwSupport represents a plane in 3D space along with its support points
type Plane3DwSupport struct {
	Plane       Plane3D
	SupportSize int
}

// ReadXYZ reads an XYZ file and returns a slice of Point3D
func ReadXYZ(filename string) ([]Point3D, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer func(file *os.File) {
		err := file.Close()
		if err != nil {
			fmt.Printf("Error reading %s: %s\n", filename, err.Error())
		}
	}(file)

	var points []Point3D
	scanner := bufio.NewScanner(file)
	if !scanner.Scan() { // skip the first line
		return nil, fmt.Errorf("failed to read the first line")
	}
	for scanner.Scan() {
		line := scanner.Text()
		values := strings.Fields(line)
		if len(values) != 3 {
			return nil, fmt.Errorf("invalid file format: expected 3 values per line")
		}
		x, err := strconv.ParseFloat(values[0], 64)
		if err != nil {
			return nil, err
		}
		y, err := strconv.ParseFloat(values[1], 64)
		if err != nil {
			return nil, err
		}
		z, err := strconv.ParseFloat(values[2], 64)
		if err != nil {
			return nil, err
		}
		points = append(points, Point3D{x, y, z})
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return points, nil
}

// SaveXYZ saves a slice of Point3D into an XYZ file
func SaveXYZ(filename string, points []Point3D) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer func(file *os.File) {
		err := file.Close()
		if err != nil {
			fmt.Printf("Error reading %s: %s\n", filename, err.Error())
		}
	}(file)

	for _, p := range points {
		_, err := fmt.Fprintf(file, "%f %f %f\n", p.X, p.Y, p.Z)
		if err != nil {
			return err
		}
	}

	return nil
}

// GetDistance computes the distance between points p1 and p2
func (p1 *Point3D) GetDistance(p2 *Point3D) float64 {
	dx := p1.X - p2.X
	dy := p1.Y - p2.Y
	dz := p1.Z - p2.Z
	return math.Sqrt(dx*dx + dy*dy + dz*dz)
}

// GetPlane computes the plane defined by a set of 3 points
func GetPlane(points [3]Point3D) Plane3D {
	p1 := points[0]
	p2 := points[1]
	p3 := points[2]

	// compute the plane equation coefficients
	a := (p2.Y-p1.Y)*(p3.Z-p1.Z) - (p2.Z-p1.Z)*(p3.Y-p1.Y)
	b := (p2.Z-p1.Z)*(p3.X-p1.X) - (p2.X-p1.X)*(p3.Z-p1.Z)
	c := (p2.X-p1.X)*(p3.Y-p1.Y) - (p2.Y-p1.Y)*(p3.X-p1.X)
	d := -(a*p1.X + b*p1.Y + c*p1.Z)

	return Plane3D{a, b, c, d}
}

// GetNumberOfIterations computes the number of required RANSAC iterations
func GetNumberOfIterations(confidence float64, percentageOfPointsOnPlane float64) int {
	pow := math.Pow(percentageOfPointsOnPlane, 3)
	return int(math.Log(1-confidence) / math.Log(1-pow))
}

// GetSupport computes the support of a plane in a set of points
func GetSupport(plane Plane3D, points []Point3D, eps float64) Plane3DwSupport {
	var support []Point3D

	// iterate over all points and add those that are within the distance threshold
	for _, point := range points {
		dist := distanceToPlane(plane, point)
		if dist <= eps {
			support = append(support, point)
		}
	}

	return Plane3DwSupport{plane, len(support)}
}

// GetSupportingPoints extracts the points that supports the given plane
// and returns them as a slice of points
func GetSupportingPoints(plane Plane3D, points []Point3D, eps float64) []Point3D {
	var support []Point3D

	// iterate over all points and add those that are within the distance threshold
	for _, point := range points {
		dist := distanceToPlane(plane, point)
		if dist <= eps {
			support = append(support, point)
		}
	}
	return support
}

// Returns the distance between a point and a plane.
func distanceToPlane(plane Plane3D, point Point3D) float64 {
	return math.Abs(plane.A*point.X+plane.B*point.Y+plane.C*point.Z+plane.D) / math.Sqrt(plane.A*plane.A+plane.B*plane.B+plane.C*plane.C)
}

// RemovePlane Removes all points in the slice that belong to the given plane within a tolerance epsilon.
func RemovePlane(plane Plane3D, points []Point3D, eps float64) []Point3D {
	var result []Point3D
	for _, point := range points {
		if distanceToPlane(plane, point) > eps {
			result = append(result, point)
		}
	}
	fmt.Printf("We have  %d points remaining after belonging to the dominant plane have been removed\n", len(result))
	return result
}

func randomPointGenerator(pointCloud []Point3D) <-chan Point3D {
	output := make(chan Point3D)
	var wg sync.WaitGroup
	rand.NewSource(time.Now().UnixNano()) // seed the random number generator
	for i := 0; i < 30; i++ {
		index := rand.Intn(len(pointCloud)) // generate a random index for the slice
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			output <- pointCloud[i]
		}(index)
	}
	go func() {
		wg.Wait()
		close(output)
	}()
	return output
}

func tripletGenerator(input <-chan Point3D) <-chan [3]Point3D {
	output := make(chan [3]Point3D)
	var wg sync.WaitGroup
	var triplet [3]Point3D
	var i int

	// Add the number of triplets to the WaitGroup
	wg.Add(1)

	// Process points from the input channel
	go func() {
		defer wg.Done()
		for point := range input {
			triplet[i] = point
			i++
			if i == 3 {
				output <- triplet
				i = 0
			}
		}
		// If there are fewer than 3 points left, send a partially filled triplet
		if i > 0 && i < 3 {
			output <- triplet
		}
		close(output)
	}()

	// Wait for the triplet processing goroutine to finish
	go func() {
		wg.Wait()
	}()

	return output
}

func takeN(input <-chan [3]Point3D, N int) <-chan [3]Point3D {
	output := make(chan [3]Point3D)
	var wg sync.WaitGroup
	wg.Add(N)
	for i := 0; i < N; i++ {
		go func() {
			defer wg.Done()
			triplet := <-input
			output <- triplet
		}()
	}
	go func() {
		wg.Wait()
		close(output)
	}()
	return output
}

func planeEstimator(input <-chan [3]Point3D) <-chan Plane3D {
	output := make(chan Plane3D)
	var wg sync.WaitGroup
	for triplet := range input {
		wg.Add(1)
		go func(t [3]Point3D) {
			defer wg.Done()
			// Compute the plane defined by the three points in the triplet
			plane := GetPlane(t)
			output <- plane
		}(triplet)
	}
	go func() {
		wg.Wait()
		close(output)
	}()
	return output
}

func supportingPointFinder(planeEstimatorChan <-chan Plane3D, pointCloud []Point3D, eps float64) <-chan Plane3DwSupport {
	output := make(chan Plane3DwSupport)
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		for plane := range planeEstimatorChan {
			wg.Add(1)
			go func(plane Plane3D) {
				defer wg.Done()
				output <- GetSupport(plane, pointCloud, eps)
			}(plane)
		}
	}()
	go func() {
		wg.Wait()
		close(output)
	}()
	return output
}

func fanIn(inputs <-chan Plane3DwSupport) <-chan Plane3DwSupport {
	output := make(chan Plane3DwSupport)
	go func() {
		for plane := range inputs {
			output <- plane
		}
		close(output)
	}()
	return output
}

func dominantPlaneIdentifier(input <-chan Plane3DwSupport, support *Plane3DwSupport) {
	for plane := range input {
		if plane.SupportSize > support.SupportSize {
			*support = plane
		}
	}
}

func rename(filename string, add string) string {
	// Find the index of the dot in the filename
	dotIndex := strings.LastIndex(filename, ".")
	if dotIndex == -1 {
		// No dot found in the filename, return the original filename
		return filename
	}
	// Extract the substring before the dot
	return filename[:dotIndex] + add
}

func main() {
	// Check if input arguments are valid
	if len(os.Args) != 5 {

		fmt.Println(len(os.Args))
		for j := 0; j < len(os.Args); j++ {
			fmt.Println(os.Args[j], j)
		}

		fmt.Println("Usage: go planeRANSAC filename confidence percentage eps")
		fmt.Println("Try this: go run planeRansac.go PointCloud1.xyz 0.99 .85 0.9")

		return
	}

	// Read input file and create slice of Point3D
	filename := os.Args[1]
	pointCloud, err := ReadXYZ(filename)
	if err != nil {
		fmt.Printf("Error reading %s: %s\n", filename, err.Error())
		return
	}

	startTime := time.Now()

	// Create bestSupport variable
	var bestSupport Plane3DwSupport

	// Get number of iterations
	confidence, err4 := strconv.ParseFloat(os.Args[2], 64)
	if err4 != nil {
		fmt.Printf("Error parsing confidence: %s\n", err4.Error())
		return
	}
	percentage, err5 := strconv.ParseFloat(os.Args[3], 64)
	if err5 != nil {
		fmt.Printf("Error parsing percentage: %s\n", err5.Error())
		return
	}
	eps, err6 := strconv.ParseFloat(os.Args[4], 64)
	if err6 != nil {
		fmt.Printf("Error parsing eps: %s\n", err6.Error())
		return
	}
	numIterations := GetNumberOfIterations(confidence, percentage)
	fmt.Printf("Number of points %d\n", len(pointCloud))

	// Pipeline stages
	fmt.Printf("Starting pipeline\n")
	randPointChan := randomPointGenerator(pointCloud)
	fmt.Printf("Stage %d\n", 1)
	tripletChan := tripletGenerator(randPointChan)
	fmt.Printf("Stage %d\n", 2)
	takeNChan := takeN(tripletChan, numIterations)
	fmt.Printf("Stage %d\n", 3)
	planeEstimatorChan := planeEstimator(takeNChan)
	fmt.Printf("Stage %d\n", 4)
	supportingPointFinderChan := supportingPointFinder(planeEstimatorChan, pointCloud, eps)
	fmt.Printf("Stage %d\n", 5)
	fanInChan := fanIn(supportingPointFinderChan)
	fmt.Printf("Stage %d\n", 6)
	dominantPlaneIdentifier(fanInChan, &bestSupport)
	fmt.Printf("Stage %d \n", 7)

	fmt.Printf("We have  %d points in the dominant plane\n", bestSupport.SupportSize)
	err1 := SaveXYZ(rename(filename, "_p3.xyz"), GetSupportingPoints(bestSupport.Plane, pointCloud, eps))
	if err != nil {
		fmt.Printf("Error saving %s: %s\n", rename(filename, "_p3.xyz"), err1.Error())
		return
	}
	err2 := SaveXYZ(rename(filename, "_p0.xyz"), RemovePlane(bestSupport.Plane, pointCloud, eps))
	if err2 != nil {
		fmt.Printf("Error saving %s: %s\n", rename(filename, "_p0.xyz"), err2.Error())
		return
	}
	endTime := time.Now()
	elapsedTime := endTime.Sub(startTime).Milliseconds()
	fmt.Printf("Number of threads %d \n", numIterations)
	fmt.Printf("Finished in %d milliseconds", elapsedTime)
}
