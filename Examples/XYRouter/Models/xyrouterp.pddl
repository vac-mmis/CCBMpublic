(define (problem xyrouter)
    (:domain xyrouter)
    (:objects
	11 12 13 14 15
	21 22 23 24 25
	31 32 33 34 35
	41 42 43 44 45
	51 52 53 54 55 - position
	0.7 2.1 3.5 4.9 6.3 - coordinate)
    (:init
      :duration (exponential 0.1)
        ;;; static things
	(connected 11 12)
	(connected 11 21)
	(connected 12 11)
	(connected 12 13)
	(connected 12 22)
	(connected 13 12)
	(connected 13 14)
	(connected 13 23)
	(connected 14 13)
	(connected 14 15)
	(connected 14 24)
	(connected 15 14)
	(connected 15 25)
	(connected 21 11)
	(connected 21 22)
	(connected 21 31)
	(connected 22 12)
	(connected 22 21)
	(connected 22 23)
	(connected 22 32)
	(connected 23 13)
	(connected 23 22)
	(connected 23 24)
	(connected 23 33)
	(connected 24 14)
	(connected 24 23)
	(connected 24 25)
	(connected 24 34)
	(connected 25 15)
	(connected 25 24)
	(connected 25 35)
	(connected 31 21)
	(connected 31 32)
	(connected 31 41)
	(connected 32 22)
	(connected 32 31)
	(connected 32 33)
	(connected 32 42)
	(connected 33 23)
	(connected 33 32)
	(connected 33 34)
	(connected 33 43)
	(connected 34 24)
	(connected 34 33)
	(connected 34 35)
	(connected 34 44)
	(connected 35 25)
	(connected 35 34)
	(connected 35 45)
	(connected 41 31)
	(connected 41 42)
	(connected 41 51)
	(connected 42 32)
	(connected 42 41)
	(connected 42 43)
	(connected 42 52)
	(connected 43 33)
	(connected 43 42)
	(connected 43 44)
	(connected 43 53)
	(connected 44 34)
	(connected 44 43)
	(connected 44 45)
	(connected 44 54)
	(connected 45 35)
	(connected 45 44)
	(connected 45 55)
	(connected 51 41)
	(connected 51 52)
	(connected 52 42)
	(connected 52 51)
	(connected 52 53)
	(connected 53 43)
	(connected 53 52)
	(connected 53 54)
	(connected 54 44)
	(connected 54 53)
	(connected 54 55)
	(connected 55 45)
	(connected 55 54)
	(= (x-pos 11) 0.7) (= (y-pos 11) 0.7)
	(= (x-pos 12) 0.7) (= (y-pos 12) 2.1)
	(= (x-pos 13) 0.7) (= (y-pos 13) 3.5)
	(= (x-pos 14) 0.7) (= (y-pos 14) 4.9)
	(= (x-pos 15) 0.7) (= (y-pos 15) 6.3)
	(= (x-pos 21) 2.1) (= (y-pos 21) 0.7)
	(= (x-pos 22) 2.1) (= (y-pos 22) 2.1)
	(= (x-pos 23) 2.1) (= (y-pos 23) 3.5)
	(= (x-pos 24) 2.1) (= (y-pos 24) 4.9)
	(= (x-pos 25) 2.1) (= (y-pos 25) 6.3)
	(= (x-pos 31) 3.5) (= (y-pos 31) 0.7)
	(= (x-pos 32) 3.5) (= (y-pos 32) 2.1)
	(= (x-pos 33) 3.5) (= (y-pos 33) 3.5)
	(= (x-pos 34) 3.5) (= (y-pos 34) 4.9)
	(= (x-pos 35) 3.5) (= (y-pos 35) 6.3)
	(= (x-pos 41) 4.9) (= (y-pos 41) 0.7)
	(= (x-pos 42) 4.9) (= (y-pos 42) 2.1)
	(= (x-pos 43) 4.9) (= (y-pos 43) 3.5)
	(= (x-pos 44) 4.9) (= (y-pos 44) 4.9)
	(= (x-pos 45) 4.9) (= (y-pos 45) 6.3)
	(= (x-pos 51) 6.3) (= (y-pos 51) 0.7)
	(= (x-pos 52) 6.3) (= (y-pos 52) 2.1)
	(= (x-pos 53) 6.3) (= (y-pos 53) 3.5)
	(= (x-pos 54) 6.3) (= (y-pos 54) 4.9)
	(= (x-pos 55) 6.3) (= (y-pos 55) 6.3)
	(= joe 122)
	;;; dynamic stuff
        (at 21))
    (:goal (at 55)))
