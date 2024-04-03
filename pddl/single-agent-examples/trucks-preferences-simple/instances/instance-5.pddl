(define (problem truck-5)
(:domain Trucks-SimplePreferences)
(:objects
	truck1 - truck
	package1 - package
	package2 - package
	package3 - package
	package4 - package
	package5 - package
	package6 - package
	package7 - package
	l1 - location
	l2 - location
	l3 - location
	t0 - time
	t1 - time
	t2 - time
	t3 - time
	t4 - time
	t5 - time
	t6 - time
	t7 - time
	t8 - time
	t9 - time
	t10 - time
	t11 - time
	t12 - time
	t13 - time
	t14 - time
	a1 - truckarea
	a2 - truckarea)

(:init
	(at truck1 l2)
	(free a1 truck1)
	(free a2 truck1)
	(closer a1 a2)
	(at package1 l1)
	(at package2 l1)
	(at package3 l2)
	(at package4 l2)
	(at package5 l3)
	(at package6 l3)
	(at package7 l2)
	(connected l1 l2)
	(connected l1 l3)
	(connected l2 l1)
	(connected l2 l3)
	(connected l3 l1)
	(connected l3 l2)
	(time-now t0)
	(le t1 t1)
	(le t1 t2)
	(le t1 t3)
	(le t1 t4)
	(le t1 t5)
	(le t1 t6)
	(le t1 t7)
	(le t1 t8)
	(le t1 t9)
	(le t1 t10)
	(le t1 t11)
	(le t1 t12)
	(le t1 t13)
	(le t1 t14)
	(le t2 t2)
	(le t2 t3)
	(le t2 t4)
	(le t2 t5)
	(le t2 t6)
	(le t2 t7)
	(le t2 t8)
	(le t2 t9)
	(le t2 t10)
	(le t2 t11)
	(le t2 t12)
	(le t2 t13)
	(le t2 t14)
	(le t3 t3)
	(le t3 t4)
	(le t3 t5)
	(le t3 t6)
	(le t3 t7)
	(le t3 t8)
	(le t3 t9)
	(le t3 t10)
	(le t3 t11)
	(le t3 t12)
	(le t3 t13)
	(le t3 t14)
	(le t4 t4)
	(le t4 t5)
	(le t4 t6)
	(le t4 t7)
	(le t4 t8)
	(le t4 t9)
	(le t4 t10)
	(le t4 t11)
	(le t4 t12)
	(le t4 t13)
	(le t4 t14)
	(le t5 t5)
	(le t5 t6)
	(le t5 t7)
	(le t5 t8)
	(le t5 t9)
	(le t5 t10)
	(le t5 t11)
	(le t5 t12)
	(le t5 t13)
	(le t5 t14)
	(le t6 t6)
	(le t6 t7)
	(le t6 t8)
	(le t6 t9)
	(le t6 t10)
	(le t6 t11)
	(le t6 t12)
	(le t6 t13)
	(le t6 t14)
	(le t7 t7)
	(le t7 t8)
	(le t7 t9)
	(le t7 t10)
	(le t7 t11)
	(le t7 t12)
	(le t7 t13)
	(le t7 t14)
	(le t8 t8)
	(le t8 t9)
	(le t8 t10)
	(le t8 t11)
	(le t8 t12)
	(le t8 t13)
	(le t8 t14)
	(le t9 t9)
	(le t9 t10)
	(le t9 t11)
	(le t9 t12)
	(le t9 t13)
	(le t9 t14)
	(le t10 t10)
	(le t10 t11)
	(le t10 t12)
	(le t10 t13)
	(le t10 t14)
	(le t11 t11)
	(le t11 t12)
	(le t11 t13)
	(le t11 t14)
	(le t12 t12)
	(le t12 t13)
	(le t12 t14)
	(le t13 t13)
	(le t13 t14)
	(le t14 t14)
	(next t0 t1)
	(next t1 t2)
	(next t2 t3)
	(next t3 t4)
	(next t4 t5)
	(next t5 t6)
	(next t6 t7)
	(next t7 t8)
	(next t8 t9)
	(next t9 t10)
	(next t10 t11)
	(next t11 t12)
	(next t12 t13)
	(next t13 t14))

(:goal (and 

	(at-destination package1 l3)


	(at-destination package2 l2)

	(at-destination package3 l1)
	(preference p1A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t4))))
	(preference p2A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t5))))
	(preference p3A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t6))))
	(preference p4A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t7))))
	(preference p5A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t8))))
	(preference p6A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t9))))
	(preference p7A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t10))))
	(preference p8A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t11))))
	(preference p9A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t12))))
	(preference p10A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t13))))
	(preference p11A (exists (?t - time)
		 (and (delivered package3 l1 ?t) (le ?t t14))))

	(at-destination package4 l3)

	(at-destination package5 l2)
	(preference p1B (exists (?t - time)
		 (and (delivered package5 l2 ?t) (le ?t t6))))
	(preference p2B (exists (?t - time)
		 (and (delivered package5 l2 ?t) (le ?t t7))))
	(preference p3B (exists (?t - time)
		 (and (delivered package5 l2 ?t) (le ?t t8))))
	(preference p4B (exists (?t - time)
		 (and (delivered package5 l2 ?t) (le ?t t9))))
	(preference p5B (exists (?t - time)
		 (and (delivered package5 l2 ?t) (le ?t t10))))
	(preference p6B (exists (?t - time)
		 (and (delivered package5 l2 ?t) (le ?t t11))))
	(preference p7B (exists (?t - time)
		 (and (delivered package5 l2 ?t) (le ?t t12))))
	(preference p8B (exists (?t - time)
		 (and (delivered package5 l2 ?t) (le ?t t13))))
	(preference p9B (exists (?t - time)
		 (and (delivered package5 l2 ?t) (le ?t t14))))

	(at-destination package6 l2)


	(at-destination package7 l1)
))

(:metric minimize (+ (* 1 (is-violated p1A))
		     (* 1 (is-violated p1B))
		     (* 2 (is-violated p2A))
		     (* 2 (is-violated p2B))
		     (* 3 (is-violated p3A))
		     (* 3 (is-violated p3B))
		     (* 4 (is-violated p4A))
		     (* 4 (is-violated p4B))
		     (* 5 (is-violated p5A))
		     (* 5 (is-violated p5B))
		     (* 6 (is-violated p6A))
		     (* 6 (is-violated p6B))
		     (* 7 (is-violated p7A))
		     (* 7 (is-violated p7B))
		     (* 8 (is-violated p8A))
		     (* 8 (is-violated p8B))
		     (* 9 (is-violated p9A))
		     (* 9 (is-violated p9B))
		     (* 10 (is-violated p10A))
		     (* 11 (is-violated p11A))))

)
