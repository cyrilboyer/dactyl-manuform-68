(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))


(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 5)
(def ncols 6)

(def α (/ π 12))                        ; curvature of the columns
(def β (/ π 36))                        ; curvature of the rows
(def centerrow (- nrows 3))             ; controls front-back tilt
(def centercol 3)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 10))            ; or, change this for more precise tenting control
(def column-style
  (if (> nrows 5) :orthographic :standard))  ; options include :standard, :orthographic, and :fixed
; (def column-style :fixed)

(defn column-offset [column] (cond
  (= column 2) [0 2.82 -4.5]
  (>= column 4) [0 -12 5.64]            ; original [0 -5.8 5.64]
  :else [0 0 0]))

(def thumb-offsets [6 -3 -6])

(def keyboard-z-offset 15)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1.0)                  ; original= 0.5

(def wall-z-offset -10)                 ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 8)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 3)                  ; wall thickness parameter; originally 5

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;;   http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.3) ;; Was 14.1, then 14.25
(def keyswitch-width 14.4)

(def sa-profile-key-height 12.7)

;; Split plate thickness into 2 to be able to have a thick case but a think "plate"
(def plate-thickness-case 4)
(def plate-thickness 1.2)
(def mount-width (+ keyswitch-width 4.4))
(def mount-height (+ keyswitch-height 3))

(def single-plate
  (let [
        top-wall (union (->> (cube (/ keyswitch-width 3) 1.5 plate-thickness)
                      (translate [(/ keyswitch-width 3)
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (- plate-thickness-case (/ plate-thickness 2))]))
                      (->> (cube (/ keyswitch-width 3) 1 plate-thickness)
                      (translate [(- (/ keyswitch-width 2) (/ keyswitch-width 2))
                                  (+ 1 (/ keyswitch-height 2))
                                  (- plate-thickness-case (/ plate-thickness 2))]))
                      (->> (cube (/ keyswitch-width 3) 1.5 plate-thickness)
                      (translate [(- 0 (/ keyswitch-width 3))
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (- plate-thickness-case (/ plate-thickness 2))])))
        bottom-wall (->> (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (- plate-thickness-case (/ plate-thickness 2))])
                      (mirror [1 0 0])
                      (mirror [0 1 0]))
        left-wall (->> (cube 2.2 (+ keyswitch-height 3) plate-thickness)
                       (translate [(+ (/ 2.2 2) (/ keyswitch-width 2))
                                   0
                                   (- plate-thickness-case (/ plate-thickness 2))]))
        pcb (->> (cube 15 16 1.6)
                       (translate [0
                                   0
                                   -3])
                       (color [0/255 220/255 220/255 1]))
        top-wall-clip (union 
                            ;; left leg
                            (->> (cube (/ keyswitch-width 2.5) 1.5 7.6)
                              (translate [(/ keyswitch-width 2.5)
                                          (+ 2.42 (/ keyswitch-height 2))
                                          -1]))
                            ;; right leg
                            (->> (cube (/ keyswitch-width 2.5) 1.5 7.6)
                              (translate [(- 0 (/ keyswitch-width 2.5))
                                          (+ 2.42 (/ keyswitch-height 2))
                                          -1]))
                            ;; bottom support
                             (->> (cube (+ keyswitch-width 2.88) 1.5 2.5)
                              (translate [0
                                          (+ 1.61 (/ keyswitch-height 2))
                                          -3.4]))
                            ;; top joint
                            (->> (rotate -0.62 [1, 0, 0] (cube (+ 3 keyswitch-width) 2.1 plate-thickness))
                              (translate [0
                                    (+ 1.96 (/ keyswitch-height 2))
                                    (- 3.5 (/ plate-thickness 2))]))
                            ;; bottom clip
                            (->> (difference
                              (cube (+ 1.22 keyswitch-width) 1 1)
                              (translate [0 -2.6 0] (rotate -0.74 [1, 0, 0] (cube (+ 8 keyswitch-width) 5 3))))
                              (translate [0
                                          7.53
                                          -4.25])
                            )
                            ;; left clip
                            (->> (difference
                              (cube 1.8 2 1.2)
                              (translate [0 -2.6 0] (rotate -0.74 [1, 0, 0] (cube (+ 0 keyswitch-width) 5 2.5))))
                              (translate [-3.05
                                          8.03
                                          -8.04])
                              (rotate -1.5708 [0, 1, 0])
                            )
                            ;; right clip
                            (->> (difference
                              (cube 1.8 2 1.2)
                              (translate [0 -2.6 0] (rotate -0.74 [1, 0, 0] (cube (+ 0 keyswitch-width) 5 2.5))))
                              (translate [3.05
                                          8.03
                                          -8.04])
                              (rotate 1.5708 [0, 1, 0])
                            )
        )
    ]
    (union 
            top-wall
            bottom-wall
            left-wall
            (->> left-wall
                (mirror [1 0 0])
                (mirror [0 1 0]))
            ;; pcb
            top-wall-clip
            (->> top-wall-clip
                (mirror [1 0 0])
                (mirror [0 1 0]))
    )))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 (/ sa-double-length 2)
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 28 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))
(def column-base-angle (* β (- centercol 2)))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [0 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0])
                                )]
    (->> (case column-style
          :orthographic placed-shape-ortho
          :fixed        placed-shape-fixed
                        placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
    (fn [angle obj] (rotate angle [1 0 0] obj))
    (fn [angle obj] (rotate angle [0 1 0] obj))
    column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))


(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [1 2 3 4] column)
                         (not= row lastrow))]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [1 2 3 4] column)
                         (not= row lastrow))]
           (->> (sa-cap (if (= column 5) 1 1))
                (key-place column row)))))

; (pr (rotate-around-y π [10 0 1]))
; (pr (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0]))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 3.5)
(def post-size 0.1)
(def post-size-big 0.8)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness-case)])))
(def web-post-big (->> (cube post-size-big post-size-big web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness-case)])))

(def post-adj (/ post-size 2))
(def post-adj-big (- 0 (/ post-size-big 2)))
;; (def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
;; (def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
;; (def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
;; (def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-tr-thin (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl-thin (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl-thin (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br-thin (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
;; (def web-post-tr-big (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
;; (def web-post-tl-big (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
;; (def web-post-bl-big (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
;; (def web-post-br-big (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj-big) (- (/ mount-height 2) post-adj-big) 0] web-post-big))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj-big) (- (/ mount-height 2) post-adj-big) 0] web-post-big))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj-big) (+ (/ mount-height -2) post-adj-big) 0] web-post-big))
(def web-post-br (translate [(- (/ mount-width 2) post-adj-big) (+ (/ mount-height -2) post-adj-big) 0] web-post-big))
(def web-post-tr-big (translate [(- (/ mount-width 2) post-adj-big) (- (/ mount-height 2) post-adj-big) 0] web-post-big))
(def web-post-tl-big (translate [(+ (/ mount-width -2) post-adj-big) (- (/ mount-height 2) post-adj-big) 0] web-post-big))
(def web-post-bl-big (translate [(+ (/ mount-width -2) post-adj-big) (+ (/ mount-height -2) post-adj-big) 0] web-post-big))
(def web-post-br-big (translate [(- (/ mount-width 2) post-adj-big) (+ (/ mount-height -2) post-adj-big) 0] web-post-big))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
  (apply union
        (triangle-hulls
             (key-place 1 cornerrow web-post-br)
             (key-place 2 lastrow web-post-tl)
             (key-place 2 cornerrow web-post-bl)
             (key-place 2 lastrow web-post-tr)
             (key-place 2 cornerrow web-post-br)
             (key-place 3 cornerrow web-post-bl))
      (triangle-hulls
             (key-place 3 lastrow web-post-tr)
             (key-place 3 lastrow web-post-br)
             (key-place 3 lastrow web-post-tr)
             (key-place 4 cornerrow web-post-bl))
      (triangle-hulls
             (key-place 4 3 web-post-br)
             (key-place 4 4 web-post-br)
             (key-place 5 cornerrow web-post-br))
      (triangle-hulls
             (key-place 4 4 web-post-tl)
             (key-place 4 4 web-post-bl)
             (key-place 3 4 web-post-br))
      (triangle-hulls
             (key-place 4 lastrow web-post-tl)
             (key-place 4 lastrow web-post-tr)
             (key-place 4 cornerrow web-post-bl)
             (key-place 4 cornerrow web-post-br)
             (key-place 4 3 web-post-br)
             (key-place 4 4 web-post-tr)
             (key-place 5 cornerrow web-post-bl))
      (triangle-hulls
             (key-place 4 4 web-post-tl)
             (key-place 4 4 web-post-bl)
             (key-place 3 4 web-post-br))
      (triangle-hulls
             (key-place 4 4 web-post-tl)
             (key-place 4 3 web-post-bl)
             (key-place 3 4 web-post-br))
      (triangle-hulls
             (key-place 3 4 web-post-tr)
             (key-place 4 3 web-post-bl)
             (key-place 3 3 web-post-br))
      (triangle-hulls
             (key-place 3 4 web-post-tr)
             (key-place 3 4 web-post-tl)
             (key-place 3 3 web-post-br)
             (key-place 3 3 web-post-bl))
      (triangle-hulls
             (key-place 3 4 web-post-tl)
             (key-place 3 4 web-post-bl)
             (key-place 3 3 web-post-bl)
             (key-place 2 4 web-post-br)
             (key-place 2 4 web-post-tr))
      (triangle-hulls
              (key-place 1 4 web-post-tr)
              (key-place 2 4 web-post-tl)
              (key-place 2 4 web-post-bl))
      (triangle-hulls
              (key-place 1 4 web-post-tr)
              (key-place 1 4 web-post-br)
              (key-place 2 4 web-post-bl))
      (triangle-hulls
              (key-place 1 4 web-post-tr)
              (key-place 2 4 web-post-tl)
              (key-place 1 3 web-post-br))
      (triangle-hulls
              (key-place 1 3 web-post-bl)
              (key-place 1 3 web-post-br)
              (key-place 1 4 web-post-tl)
              (key-place 1 4 web-post-tr))
      (triangle-hulls
             (key-place 4 4 web-post-bl)
             (key-place 3 4 web-post-bl)
             (key-place 3 4 web-post-br))
      (triangle-hulls
             (key-place 1 4 web-post-tl)
             (key-place 1 3 web-post-bl)
             (key-place 0 3 web-post-br))
         (concat
          ;; Row connections
          (for [column (range 0 (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl-big)
             (key-place column row web-post-tr-big)
             (key-place (inc column) row web-post-bl-big)
             (key-place column row web-post-br-big)
            ;;  (key-place (inc column) row (translate [0 0 -1] web-post-tl))
            ;;  (key-place column row (translate [0 0 -1] web-post-tr))
            ;;  (key-place (inc column) row (translate [0 0 -1] web-post-bl))
            ;;  (key-place column row (translate [0 0 -1] web-post-br))
            ))

          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br-big)
             (key-place column (inc row) web-post-tr-big)
             (key-place (inc column) row web-post-bl-big)
             (key-place (inc column) (inc row) web-post-tl-big)
            ;;  (key-place column row (translate [0 0 -1] web-post-br))
            ;;  (key-place column (inc row) (translate [0 0 -1] web-post-tr))
            ;;  (key-place (inc column) row (translate [0 0 -1] web-post-bl))
            ;;  (key-place (inc column) (inc row) (translate [0 0 -1] web-post-tl))
             ))
        )
))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
         thumb-offsets))
; (pr thumborigin)

(defn thumb-tr-place [shape]
  (->> shape
      ;  (rotate (deg2rad  10) [1 0 0])
      ;  (rotate (deg2rad -23) [0 1 0])
      ;  (rotate (deg2rad  -3) [0 0 1])
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-32 -18 20])
       ))
(defn thumb-tl-place [shape]
  (->> shape
      ;  (rotate (deg2rad  10) [1 0 0])
      ;  (rotate (deg2rad -23) [0 1 0])
      ;  (rotate (deg2rad  -3) [0 0 1])
       (rotate (deg2rad  8) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-55 -17 12])))
(defn thumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  16) [1 0 0])
       (rotate (deg2rad -34) [0 1 0])
       (rotate (deg2rad  48) [0 0 1])
       (translate thumborigin)
       (translate [-48 -42 2.5])
       ))
(defn thumb-ml-place [shape]
  (->> shape
       (rotate (deg2rad   -2) [1 0 0])
       (rotate (deg2rad -34) [0 1 0])
       (rotate (deg2rad  40) [0 0 1])
       (translate thumborigin)
       (translate [-74 -27 1.5])))
(defn thumb-br-place [shape]
  (->> shape
       (rotate (deg2rad 10) [1 0 0])
       (rotate (deg2rad -33) [0 1 0])
       (rotate (deg2rad  54) [0 0 1])
       (translate thumborigin)
       (translate [-60.8 -57.3 -11.3])
       ))
(defn thumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad  -4) [1 0 0])
       (rotate (deg2rad -35) [0 1 0])
       (rotate (deg2rad  52) [0 0 1])
       (translate thumborigin)
       (translate [-79.3 -45.3 -9.5])
       ))

(defn thumb-1x-layout [shape]
  (union
   (thumb-mr-place shape)
   (thumb-ml-place shape)
   (thumb-br-place shape)
   (thumb-bl-place shape)))

(defn thumb-15x-layout [shape]
  (union
   (thumb-tr-place shape)
   (thumb-tl-place shape)))

(def larger-plate
  (let [plate-height (/ (- sa-double-length mount-height) 3)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness-case (/ web-thickness 2))]))
        ]
    (union top-plate (mirror [0 1 0] top-plate))))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))


(def thumb
  (union
   (thumb-1x-layout single-plate)
   (thumb-15x-layout single-plate)
   (thumb-15x-layout larger-plate)
   ))

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  1.15) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  1.15) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -1.15) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -1.15) post-adj) 0] web-post))

(def thumb-connectors
  (union
      (triangle-hulls    ; top two
             (thumb-tl-place thumb-post-tr)
             (thumb-tl-place thumb-post-br)
             (thumb-tr-place thumb-post-tl)
             (thumb-tr-place thumb-post-bl))
      (triangle-hulls    ; bottom two on the right
             (thumb-br-place web-post-tr)
             (thumb-br-place web-post-br)
             (thumb-mr-place web-post-tl)
             (thumb-mr-place web-post-bl))
      (triangle-hulls    ; bottom two on the left
             (thumb-bl-place web-post-tr-thin)
             (thumb-bl-place web-post-br-thin)
             (thumb-ml-place web-post-tl-thin)
             (thumb-ml-place web-post-bl-thin))
      (triangle-hulls    ; centers of the bottom four
             (thumb-br-place web-post-tl)
             (thumb-bl-place web-post-bl)
             (thumb-br-place web-post-tr)
             (thumb-bl-place web-post-br)
             (thumb-mr-place web-post-tl)
             (thumb-ml-place web-post-bl)
      )
      ;; (triangle-hulls    ; centers of the bottom four
      ;;        (thumb-mr-place web-post-tl)
      ;;        (thumb-ml-place web-post-bl)
      ;;        (thumb-mr-place web-post-tr)
      ;;        (thumb-tl-place web-post-bl)
      ;;        (thumb-tl-place web-post-br)
      ;;        (thumb-ml-place web-post-br)
      ;; )
      (triangle-hulls    ; centers of the bottom four
             (thumb-ml-place web-post-br-thin)
             (thumb-ml-place web-post-bl)
             (thumb-mr-place web-post-tl)
      )
      (triangle-hulls    ; centers of the bottom four
             (thumb-mr-place web-post-tr)
             (thumb-mr-place web-post-tl)
             (thumb-ml-place web-post-br-thin)
             (thumb-tl-place web-post-bl)
             (thumb-tl-place web-post-br)
      )
      (triangle-hulls    ; top two to the middle two, starting on the left
             (thumb-tl-place thumb-post-tl)
             (thumb-ml-place web-post-tr)
             (thumb-tl-place (translate [0.5 0 0] thumb-post-bl)))
      (triangle-hulls    ; top two to the middle two, starting on the left
             (thumb-tl-place web-post-bl)
             (thumb-tl-place thumb-post-br)
             (thumb-mr-place web-post-tr)
             (thumb-tr-place thumb-post-bl)
             (thumb-mr-place web-post-br)
             (thumb-tr-place thumb-post-br))
      (triangle-hulls
             (thumb-tr-place thumb-post-br)
             (key-place 1 4 web-post-bl)
             (thumb-tr-place thumb-post-tr)
             (key-place 1 4 web-post-tl))
      (triangle-hulls
             (thumb-tr-place thumb-post-tl)
             (key-place 0 3 web-post-bl)
             (thumb-tr-place thumb-post-tr))
      (triangle-hulls
             (key-place 0 3 web-post-br)
             (key-place 1 4 web-post-tl)
             (key-place 0 3 web-post-bl)
             (thumb-tr-place thumb-post-tr))
      (triangle-hulls
             (key-place 0 3 web-post-bl)
             (thumb-tr-place thumb-post-tl)
             (thumb-tl-place thumb-post-tr))
      (triangle-hulls
             (key-place 0 3 web-post-bl)
             (thumb-tl-place thumb-post-tl)
             (thumb-tl-place thumb-post-tr))
  ))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def left-wall-x-offset 10)
(def left-wall-z-offset  3)

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]) )

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))


(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
    (hull
      (place1 post1)
      (place1 (translate (wall-locate1 dx1 dy1) post1))
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place1 (translate (wall-locate3 dx1 dy1) post1))
      (place2 post2)
      (place2 (translate (wall-locate1 dx2 dy2) post2))
      (place2 (translate (wall-locate2 dx2 dy2) post2))
      (place2 (translate (wall-locate3 dx2 dy2) post2)))
    (bottom-hull
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place1 (translate (wall-locate3 dx1 dy1) post1))
      (place2 (translate (wall-locate2 dx2 dy2) post2))
      (place2 (translate (wall-locate3 dx2 dy2) post2)))
      )
  )

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

(def case-walls
  (union
   ; back wall
   (key-wall-brace 0 0 0 1 web-post-tl 0       0 0 1 web-post-tr)
   (key-wall-brace 1 0 0 1 web-post-tl 1       0 0 1 web-post-tr)
   (key-wall-brace 2 0 0 0.704 web-post-tl 2       0 0 0.704 web-post-tr)
   (key-wall-brace 3 0 0 1 web-post-tl 3       0 0 1 web-post-tr)
   (key-wall-brace 4 0 0 1 web-post-tl 4       0 0 1 web-post-tr)
   (key-wall-brace 5 0 0 1 web-post-tl 5       0 0 1 web-post-tr)

  ;;  (for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
  (key-wall-brace 1 0 0 1 web-post-tl (dec 1) 0 0 1 web-post-tr)
  (key-wall-brace 2 0 0 0.704 web-post-tl (dec 2) 0 0 1 web-post-tr)
  (key-wall-brace 3 0 0 1 web-post-tl (dec 3) 0 0 0.704 web-post-tr)
  (key-wall-brace 4 0 0 1 web-post-tl (dec 4) 0 0 1 web-post-tr)
  (key-wall-brace 5 0 0 1 web-post-tl (dec 5) 0 0 1 web-post-tr)

  ;;  (for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
   ; right wall
   (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y       1 0 web-post-br))
   (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr))
   (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 1 0 web-post-br)
   ; left wall
   (for [y (range 0 lastrow)] (union (wall-brace (partial left-key-place y 1)       -1 0 web-post (partial left-key-place y -1) -1 0 web-post)
                                     (hull (key-place 0 y web-post-tl)
                                           (key-place 0 y web-post-bl)
                                           (left-key-place y  1 web-post)
                                           (left-key-place y -1 web-post))))
   (for [y (range 1 lastrow)] (union (wall-brace (partial left-key-place (dec y) -1) -1 0 web-post (partial left-key-place y  1) -1 0 web-post)
                                     (hull (key-place 0 y       web-post-tl)
                                           (key-place 0 (dec y) web-post-bl)
                                           (left-key-place y        1 web-post)
                                           (left-key-place (dec y) -1 web-post))))
   (wall-brace (partial key-place 0 0) 0 1 web-post-tl (partial left-key-place 0 1) 0 1 web-post)
   (wall-brace (partial left-key-place 0 1) 0 1 web-post (partial left-key-place 0 1) -1 0 web-post)
   ; front wall
   (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
   (key-wall-brace 1 lastrow 0.6 -1.25 web-post-bl 1 lastrow 0.5 -1 web-post-br)
   (key-wall-brace 1 lastrow 0.5 -1 web-post-br 2 lastrow 1 -1.25 web-post-bl)
   (key-wall-brace 2 lastrow   1 -1.25 web-post-bl 2 lastrow 0.5 -2 web-post-br)
   (key-wall-brace 2 lastrow 0.5 -2 web-post-br 3 lastrow 1 -2 web-post-bl)
   (key-wall-brace 4 lastrow 0.5 -1 web-post-br 5 cornerrow 0 -1 web-post-br)
   (key-wall-brace 4 lastrow 0.5 -1 web-post-br 4 lastrow 1 -1 web-post-bl)
   (key-wall-brace 4 lastrow 1 -1 web-post-bl 3 4 1 -2 web-post-bl)
   
  ;;  (key-wall-brace 3 4 0.5 -1 web-post-br 4 4 1 -1 web-post-bl)
  ;;  (key-wall-brace 4 cornerrow 0 -1 web-post-bl 4       cornerrow 0 -1 web-post-br)
;;    (key-wall-brace 5 cornerrow 0 -1 web-post-bl 5       cornerrow 0 -1 web-post-br)
  ;;  (key-wall-brace 5 cornerrow 0 -1 web-post-bl (dec 5) cornerrow 0 -1 web-post-br)
   ; thumb walls
   (wall-brace thumb-mr-place  0 -1 web-post-br thumb-tr-place  0 -1 thumb-post-br)
   (wall-brace thumb-mr-place  0 -1 web-post-br thumb-mr-place  0 -1 web-post-bl)
   (wall-brace thumb-br-place  0 -1 web-post-br thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-ml-place -0.3  1 web-post-tr thumb-ml-place  0  1 web-post-tl)
   (wall-brace thumb-bl-place  0  1 web-post-tr thumb-bl-place  0  1 web-post-tl)
   (wall-brace thumb-br-place -1  0 web-post-tl thumb-br-place -1  0 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place -1  0 web-post-bl)
   ; thumb corners
   (wall-brace thumb-br-place -1  0 web-post-bl thumb-br-place  0 -1 web-post-bl)
   (wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place  0  1 web-post-tl)
   ; thumb tweeners
   (wall-brace thumb-mr-place  0 -1 web-post-bl thumb-br-place  0 -1 web-post-br)
   (wall-brace thumb-ml-place  0  1 web-post-tl thumb-bl-place  0  1 web-post-tr)
   (wall-brace thumb-bl-place -1  0 web-post-bl thumb-br-place -1  0 web-post-tl)
   (wall-brace thumb-tr-place  0 -1 thumb-post-br (partial key-place 1 lastrow)  0.6 -1.25 web-post-bl)
   ; clunky bit on the top left thumb connection  (normal connectors don't work well)
   (bottom-hull
     (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
     (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr)))
   (hull
     (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
     (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
     (thumb-tl-place thumb-post-tl))
   (hull
     (left-key-place cornerrow -1 web-post)
     (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
     (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
     (thumb-tl-place thumb-post-tl))

   (hull
     (left-key-place cornerrow -1 web-post)
     (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
     (key-place 0 cornerrow web-post-bl)
     (key-place 0 cornerrow (translate (wall-locate1 0 2) web-post-bl))
     (thumb-tl-place thumb-post-tl))
     
   (hull
     (thumb-ml-place web-post-tr)
     (thumb-ml-place (translate (wall-locate1 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
     (thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
     (thumb-tl-place thumb-post-tl))
  )
)


(def rj9-start  (map + [0 -3  0] (key-position 0 0 (map + (wall-locate3 0 1) [0 (/ mount-height  2) 0]))))
(def rj9-position  [(first rj9-start) (second rj9-start) 11])
(def rj9-cube   (cube 14.78 13 22.38))
(def rj9-space  (translate rj9-position rj9-cube))
(def rj9-holder (translate rj9-position
                  (difference rj9-cube
                              (union (translate [0 2 0] (cube 10.78  9 18.38))
                                     (translate [0 0 5] (cube 10.78 13  5))))))

(def usb-holder-position (key-position 1 0 (map + (wall-locate2 0 1) [0 (/ mount-height 2) 0])))
(def usb-holder-size [6.5 10.0 13.6])
(def usb-holder-thickness 4)
(def usb-holder
    (->> (cube (+ (first usb-holder-size) usb-holder-thickness) (second usb-holder-size) (+ (last usb-holder-size) usb-holder-thickness))
         (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))
(def usb-holder-hole
    (->> (apply cube usb-holder-size)
         (translate [(first usb-holder-position) (second usb-holder-position) (/ (+ (last usb-holder-size) usb-holder-thickness) 2)])))

(defn screw-insert-shape [bottom-radius top-radius height]
   (union (cylinder [bottom-radius top-radius] height)
          (translate [0 0 (/ height 2)] (sphere top-radius))))

(defn screw-insert [column row bottom-radius top-radius height]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                       (if shift-down  (key-position column row (map - (wall-locate2  0 -1) [0 (/ mount-height 2) 0]))
                        (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                                       (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))
        ]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate [(first position) (second position) (/ height 2)])
    )))

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union (screw-insert -0.9 -0.5         bottom-radius top-radius height)
         (screw-insert 0 (- lastrow 0.4)   bottom-radius top-radius height)
         (screw-insert 0.5 4.3  bottom-radius top-radius height)
         (screw-insert 2 -0.4         bottom-radius top-radius height)
         (screw-insert 3.6 4.3   bottom-radius top-radius height)
         (screw-insert 4.9 -0.2   bottom-radius top-radius height)
         (screw-insert 4.9 3.2   bottom-radius top-radius height)
         ))
(def screw-insert-height 3.8)
(def screw-insert-bottom-radius (/ 5.31 2))
(def screw-insert-top-radius (/ 5.1 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.6) (+ screw-insert-top-radius 1.6) (+ screw-insert-height 1.5)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))

(def bottom-plate-height 4)

;; ; Hole Depth Y: 4.4
;; (def screw-insert-height 4)

;; ; Hole Diameter C: 4.1-4.4
;; (def screw-insert-bottom-radius (/ 4.4 2))
;; (def screw-insert-top-radius (/ 4.4 2))
;; (def screw-insert-holes (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height true))

;; ; Wall Thickness W:\t1.65
;; (def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1.5) true))

;; (def screw-radius 1.8)
;; (def screw-insert-screw-holes (screw-insert-all-shapes screw-radius screw-radius bottom-plate-height false))
;; (def screw-insert-screw-countersink
;;     (let [  screw-head-radius       3.2
;;             screw-head-height       1.8
;;             screw-countersink-depth 0.6]
;;         (union
;;             (screw-insert-all-shapes screw-head-radius screw-head-radius screw-countersink-depth false)
;;             (translate [0 0 (- screw-countersink-depth 0.01)]
;;                 (screw-insert-all-shapes screw-head-radius screw-radius (+ screw-head-height 0.02) false)))))

(def model-right (difference
                   (union
                    key-holes
                    connectors
                    thumb
                    thumb-connectors
                    (difference (union case-walls
                                       screw-insert-outers)
                                       ; teensy-holder)
                                       ; usb-holder)
                                ; rj9-space
                                ; usb-holder-hole
                                (translate [0 0 -0.01] screw-insert-holes))
                    ; rj9-holder
                    ; wire-posts
                    ; thumbcaps
                    ; caps
                    )
                   (translate [0 0 -20] (cube 350 350 40))
                  ))

(spit "things/test-single-plate.scad"
      (write-scad single-plate))

(spit "things/right-cyril.scad"
      (write-scad model-right))

(spit "things/left-cyril.scad"
      (write-scad (mirror [-1 0 0] model-right)))

(spit "things/right-cyril-test.scad"
      (write-scad
                   (union
                    key-holes
                    connectors
                    thumb
                    thumb-connectors
                    case-walls
                    thumbcaps
                    caps
                    ; teensy-holder
                    ; rj9-holder
                    ; usb-holder-hole
                    ; usb-holder-hole
                    ; ; teensy-holder-hole
                    ;             screw-insert-outers
                    ;             teensy-screw-insert-holes
                    ;             teensy-screw-insert-outers
                    ;             usb-cutout
                    ;             rj9-space
                                ; wire-posts
                  )))

(def plate-right
    (let [  fill            (cube 20 20 1)
            key-holes-fill  (apply union
                                (for [column columns
                                    row rows
                                    :when (or (.contains [1 2 3 4] column)
                                            (not= row lastrow))]
                                    (key-place column row fill)))
            thumb-fill      (union
                                (thumb-tr-place fill)
                                (thumb-tl-place fill)
                                (thumb-mr-place fill)
                                (thumb-ml-place fill)
                                (thumb-br-place fill)
                                (thumb-bl-place fill))]
    (difference
        (translate [0 0 0]
            (extrude-linear {:height bottom-plate-height}
                (projection true
                    (union
                        model-right
                        key-holes-fill
                        thumb-fill))))
        screw-insert-screw-holes)))

(spit "things/right-plate-cyril.scad"
      (write-scad
         (difference plate-right (
           translate [0 0 0] (
            union model-right
            screw-insert-outers
            (translate [0 0 0] (scale [1.01 1.01 1] model-right))
            (translate [0 0 -3] screw-insert-screw-holes)
            (translate [0 0 -6.5] screw-insert-holes)
)))))

(spit "things/left-plate-cyril.scad"
      (write-scad (mirror [-1 0 0]
         (difference plate-right (
           translate [0 0 0] (
            union model-right
            screw-insert-outers
            (translate [0 0 0] (scale [1.01 1.01 1] model-right))
            (translate [0 0 -3] screw-insert-screw-holes)
            (translate [0 0 -6.5] screw-insert-holes)
))))))

(spit "things/test.scad"
      (write-scad
         (difference usb-holder usb-holder-hole)))



(defn -main [dum] 1)  ; dummy to make it easier to batch
