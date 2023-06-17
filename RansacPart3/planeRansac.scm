#lang scheme

;;Amina Anna Mahamane Ousmane 300227147

; Reads a file of 3D point cloud and returns a list of points
; fileIn: string - the path of the file to read
; return: list - a list of 3D points, each point represented as a list of three numbers
(define (readXYZ fileIn)
  (let ((sL (map (lambda (s) (string-split s)) (cdr (file->lines fileIn))))) 
    (let ((points (map (lambda (L) (map string->number L)) sL))
          (count (length sL)))
      (begin
        (display "In the initial point cloud, we have ")
        (display count)
        (display " points.\n")
        points))))

; Generates a random point from a list of 3D points
; Ps: list - a list of 3D points, each point represented as a list of three numbers
; return: list - a random 3D point from the input list
(define (random-point Ps)
  (list-ref Ps (random (length Ps))))

; Computes the plane equation that goes through three 3D points
; P1, P2, P3: list - three 3D points, each point represented as a list of three numbers
; return: list - the coefficients of the plane equation in the form of a list [a, b, c, d], where ax + by + cz + d = 0 is the equation of the plane
(define (plane P1 P2 P3)
  (let* ((a (- (* (- (caddr P2) (caddr P1)) (- (cadr P3) (cadr P1)))
               (* (- (caddr P3) (caddr P1)) (- (cadr P2) (cadr P1)))))
         (b (- (* (- (caddr P3) (caddr P1)) (- (car P2) (car P1)))
               (* (- (car P3) (car P1)) (- (cadr P2) (cadr P1)))))
         (c (- (* (- (car P3) (car P1)) (- (cadr P2) (cadr P1)))
               (* (- (cadr P3) (cadr P1)) (- (car P2) (car P1)))))
         (d (- (+ (* a (car P1)) (* b (cadr P1)) (* c (caddr P1)))
               (* 1.0 0.0 0.0))))
    (list a b c d)))

; count: Count the number of elements in a list that satisfy a given predicate
; lst - the list to be counted
; pred - the predicate to test against each element of the list
; Returns the number of elements in the list that satisfy the given predicate.
(define (count lst pred)
  (cond ((null? lst) 0)                ; base case: empty list, return 0
        ((pred (car lst))             ; if the predicate is true for the first element
         (+ 1 (count (cdr lst) pred))) ; add 1 to the count and continue with the rest of the list
        (else (count (cdr lst) pred)))) ; otherwise, continue with the rest of the list

; within-epsilon-distance?: Check if a given point is within epsilon distance from a plane
; point - a point represented as a list of three numbers (x y z)
; plane - a plane represented as a list of four numbers (a b c d)
; eps - the maximum distance allowed between the point and plane
; Returns true if the point is within epsilon distance from the plane, false otherwise.
(define (within-epsilon-distance? point plane eps)
  (<= (/ (abs (+ (* (car plane) (car point))     ; calculate the distance between the point and plane
                  (* (cadr plane) (cadr point))
                  (* (caddr plane) (caddr point))
                  (cadddr plane)))
         (sqrt (+ (* (car plane) (car plane))     ; calculate the length of the plane's normal vector
                   (* (cadr plane) (cadr plane))
                   (* (caddr plane) (caddr plane)))))
      eps))                                      ; check if the distance is within the given epsilon

; support: Find the number of points within epsilon distance of a plane
; plane - a plane represented as a list of four numbers (a b c d)
; points - a list of points represented as lists of three numbers each (x y z)
; eps - the maximum distance allowed between a point and the plane for it to be considered "supporting"
; Returns a pair of the form (count . plane), where count is the number of points in the list that are within epsilon
; distance of the plane, and plane is the original plane.
(define (support plane points eps)
  (let ((count (count points (lambda (p) (within-epsilon-distance? p plane eps))))) ; count the number of points within epsilon distance
    (cons count plane)))                                                         ; return a pair of the count and the plane


; dominant-plane: Uses RANSAC algorithm to find the dominant plane in a point cloud.
; Ps - a list of points represented as lists of three numbers each (x y z)
; confidence - a number between 0 and 1 representing the desired level of confidence in the result
; percentage - a number between 0 and 1 representing the expected percentage of inliers in the point cloud
; eps - the maximum distance allowed between a point and the plane for it to be considered "supporting"
; Returns the dominant plane in the point cloud as a list of four numbers (a b c d), where ax + by + cz + d = 0.
(define (dominant-plane Ps confidence percentage eps)
  (let* ((n (ransac-number-of-iterations confidence percentage))
         (planes (for/list ([i (in-range n)])
                   (let* ((P1 (random-point Ps))
                          (P2 (random-point Ps))
                          (P3 (random-point Ps))
                          (plane (plane P1 P2 P3)))
                     (support plane Ps eps)))))
    (let ((dominant (sort planes (lambda (a b) (> (car a) (car b)))))) 
      (car dominant))))


; ransac-number-of-iterations: Calculates the number of iterations required by RANSAC algorithm.
; confidence - a number between 0 and 1 representing the desired level of confidence in the result
; percentage - a number between 0 and 1 representing the expected percentage of inliers in the point cloud
; Returns the number of iterations required by RANSAC algorithm as an exact integer.
(define (ransac-number-of-iterations confidence percentage)
  (let ((inlier-probability (expt (- 1.0 percentage) 3)))
    (inexact->exact (ceiling (/ (log (- 1.0 confidence)) (log (- 1.0 inlier-probability)))))))

; writePointCloudToFile: Writes a point cloud to a file.
; filename - the name of the file to write to
; points - a list of points represented as lists of three numbers each (x y z)
(define (writePointCloudToFile filename points)
  (with-output-to-file filename
    (lambda ()
      (display (string-append "Total points in the dominant plane: " (number->string (length points)) "\n"))
      (for-each (lambda (p)
                  (display (string-append (number->string (car p)) " " 
                                          (number->string (cadr p)) " " 
                                          (number->string (caddr p)) "\n")))
                points))))


; Calculates the distance between a point and a plane in 3D space
; point - a list of three numbers representing the (x, y, z) coordinates of a point
; plane - a pair representing teh number of supporting points and the equation of the plane in the form (ax + by + cz + d = 0)
; Returns the distance between the point and the plane
(define (point-plane-distance point plane)
  (/ (abs (+ (* (car (cdr plane)) (car point))
              (* (cadr (cdr plane)) (cadr point))
              (* (caddr (cdr plane)) (caddr point))
              (cadddr  (cdr plane))))
     (sqrt (+ (* (car (cdr plane)) (car (cdr plane)))
               (* (cadr (cdr plane)) (cadr (cdr plane)))
               (* (caddr (cdr plane)) (caddr (cdr plane)))))))

; Removes a suffix from a string if it exists
; suffix - the suffix to remove from the string
; string - the string to remove the suffix from
; Returns the string with the suffix removed, or the original string if the suffix is not found
(define (string-remove-suffix suffix string)
  (let ((suffix-length (string-length suffix))
        (string-length (string-length string)))
    (if (and (>= string-length suffix-length)
             (string=? (substring string (- string-length suffix-length)) suffix))
        (substring string 0 (- string-length suffix-length))
        string)))

; planeRANSAC: Applies the RANSAC algorithm to find the dominant plane in a point cloud and returns the plane and its inliers.
; filename - the name of the file containing the point cloud
; confidence - the desired confidence level (between 0 and 1) for finding the dominant plane
; percentage - the percentage of inliers in the point cloud that are expected to lie on the dominant plane
; eps - the maximum distance allowed between a point and the dominant plane for it to be considered an inlier
; Returns a pair of the form (dominant . inliers), where dominant is the equation of the dominant plane (represented as a list of four numbers)
; and inliers is a list of the points that lie on the dominant plane (represented as lists of three numbers).
(define (planeRANSAC filename confidence percentage eps)
  ; read in the point cloud from file
  (let ((Ps (readXYZ filename)))
    ; determine the number of iterations required
    (let ((k (ransac-number-of-iterations confidence percentage)))
      ; find the dominant plane
      (let ((dominant (dominant-plane Ps confidence percentage eps)))
        ; filter points lying on the dominant plane
        (begin
        (display "In the dominant plane, we have ")
        (display ( car dominant ))
        (display " points \n")
        dominant)
        (let ((inliers (filter (lambda (p) (<= (point-plane-distance p dominant) eps)) Ps)))
          ; write inliers to file
          (let ((inliers-filename (string-append (string-remove-suffix ".xyz" filename) "_inliers.xyz")))
            (writePointCloudToFile inliers-filename inliers))
          ; return dominant plane and number of inliers
          dominant)))))

;;Call
(planeRANSAC "Point_Cloud_1_No_Road_Reduced.xyz" 0.99 0.60 0.8)

