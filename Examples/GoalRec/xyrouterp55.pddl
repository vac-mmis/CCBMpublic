(define (problem xyrouter55)
    (:domain xyrouter)
    (:objects
	11 12 13 14 15
	21 22 23 24 25
	31 32 33 34 35
	41 42 43 44 45
	51 52 53 54 55 - position)
    (:init
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
	;;; dynamic stuff
     (at 21))
 (:goal (at 55)))
