(define (problem depotprob1818) (:domain Depot)
(:objects
	depot0 depot1 depot2 depot3 depot4 depot5 depot6 depot7 depot8 depot9 - Depot
	distributor0 distributor1 distributor2 distributor3 distributor4 distributor5 distributor6 distributor7 distributor8 distributor9 - Distributor
	truck0 truck1 truck2 truck3 truck4 truck5 - Truck
	pallet0 pallet1 pallet2 pallet3 pallet4 pallet5 pallet6 pallet7 pallet8 pallet9 pallet10 pallet11 pallet12 pallet13 pallet14 pallet15 pallet16 pallet17 pallet18 pallet19 pallet20 pallet21 pallet22 pallet23 pallet24 pallet25 pallet26 pallet27 pallet28 pallet29 - Pallet
	crate0 crate1 crate2 crate3 crate4 crate5 crate6 crate7 crate8 crate9 crate10 crate11 crate12 crate13 crate14 crate15 crate16 crate17 crate18 crate19 - Crate
	hoist0 hoist1 hoist2 hoist3 hoist4 hoist5 hoist6 hoist7 hoist8 hoist9 hoist10 hoist11 hoist12 hoist13 hoist14 hoist15 hoist16 hoist17 hoist18 hoist19 hoist20 hoist21 hoist22 hoist23 hoist24 hoist25 hoist26 hoist27 hoist28 hoist29 - Hoist)
(:init
	(at pallet0 depot0)
	(clear pallet0)
	(at pallet1 depot1)
	(clear pallet1)
	(at pallet2 depot2)
	(clear pallet2)
	(at pallet3 depot3)
	(clear pallet3)
	(at pallet4 depot4)
	(clear crate12)
	(at pallet5 depot5)
	(clear pallet5)
	(at pallet6 depot6)
	(clear crate11)
	(at pallet7 depot7)
	(clear crate16)
	(at pallet8 depot8)
	(clear pallet8)
	(at pallerate - surface)

(:predicates (at ?x - locatable ?y - place)
             (on ?x - crate ?y - surface)
             (in ?x t9 depot9)
	(clear crate19)
	(at pallet10 distributor0)
	(clear crate18)
	(at pallet11 distributor1)
	(clear crate1)
	(at pallet12 distributor2)
	(clear pallet12)
	(at pallet13 distributor3)
	(clear crate9)
	(at pallet14 distributor4)
	(clear crate10)
	(at pallet15 distributor5)
	(clear pallet15)
	(at pallet16 distributor6)
	(clear crate4)
	(at pallet17 distributor7)
	(clear crate2)
	(at pallet18 distributor8)
	(clear pallet18)
	(at pallet19 distributor9)
	(clear crate5)
	(at pallet20 depot6)
	(clear pallet20)
	(at pallet21 distributor9)
	(clear crate6)
	(at pallet22 depot7)
	(clear pallet22)
	(at pallet23 distributor1)
	(clear pallet23)
	(at pallet24 distributor4)
	(clear pallet24)
	(at pallet25 distributor9)
	(clear crate14)
	(at pallet26 distributor7)
	(clear pallet26)
	(at pallet27 distributor4)
	(clear crate17)
	(at pallet28 distributor3)
	(clear crate15)
	(at pallet29 depot7)
	(clear crate8)
	(at truck0 depot6)
	(at truck1 distributor9)
	(at truck2 depot1)
	(at truck3 distributor3)
	(at truck4 distributor5)
	(at truck5 depot3)
	(at hoist0 depot0)
	(available hoist0)
	(at hoist1 depot1)
	(available hoist1)
	(at hoist2 depot2)
	(available hoist2)
	(at hoist3 depot3)
	(available hoist3)
	(at hoist4 depot4)
	(available hoist4)
	(at hoist5 depot5)
	(available hoist5)
	(at hoist6 depot6)
	(available hoist6)
	(at hoist7 depot7)
	(available hoist7)
	(at hoist8 depot8)
	(available hoist8)
	(at hoist9 depot9)
	(available hoist9)
	(at hoist10 distributor0)
	(available hoist10)
	(at hoist11 distributor1)
	(available hoist11)
	(at hoist12 distributor2)
	(available hoist12)
	(at hoist13 distributor3)
	(available hoist13)
	(at hoist14 distributor4)
	(available hoist14)
	(at hoist15 distributor5)
	(available hoist15)
	(at hoist16 distributor6)
	(available hoist16)
	(at hoist17 distributor7)
	(available hoist17)
	(at hoist18 distributor8)
	(available hoist18)
	(at hoist19 distributor9)
	(available hoist19)
	(at hoist20 depot5)
	(available hoist20)
	(at hoist21 depot1)
	(available hoist21)
	(at hoist22 distributor7)
	(available hoist22)
	(at hoist23 distributor2)
	(available hoist23)
	(at hoist24 distributor4)
	(available hoist24)
	(at hoist25 depot6)
	(available hoist25)
	(at hoist26 depot5)
	(available hoist26)
	(at hoist27 distributor1)
	(available hoist27)
	(at hoist28 distributor9)
	(available hoist28)
	(at hoist29 depot9)
	(available hoist29)
	(at crate0 distributor9)
	(on crate0 pallet25)
	(at crate1 distributor1)
	(on crate1 pallet11)
	(at crate2 distributor7)
	(on crate2 pallet17)
	(at crate3 distributor3)
	(on crate3 pallet13)
	(at crate4 distributor6)
	(on crate4 pallet16)
	(at crate5 distributor9)
	(on crate5 pallet19)
	(at crate6 distributor9)
	(on crate6 pallet21)
	(at crate7 distributor3)
	(on crate7 pallet28)
	(at crate8 depot7)
	(on crate8 pallet29)
	(at crate9 distributor3)
	(on crate9 crate3)
	(at crate10 distributor4)
	(on crate10 pallet14)
	(at crate11 depot6)
	(on crate11 pallet6)
	(at crate12 depot4)
	(on crate12 pallet4)
	(at crate13 distributor3)
	(on crate13 crate7)
	(at crate14 distributor9)
	(on crate14 crate0)
	(at crate15 distributor3)
	(on crate15 crate13)
	(at crate16 depot7)
	(on crate16 pallet7)
	(at crate17 distributor4)
	(on crate17 pallet27)
	(at crate18 distributor0)
	(on crate18 pallet10)
	(at crate19 depot9)
	(on crate19 pallet9)
)

(:goal (and
		(on crate0 pallet11)
		(on crate1 pallet22)
		(on crate2 pallet12)
		(on crate3 pallet26)
		(on crate4 crate17)
		(on crate5 crate18)
		(on crate6 pallet7)
		(on crate7 pallet15)
		(on crate8 pallet1)
		(on crate9 crate11)
		(on crate10 crate19)
		(on crate11 pallet23)
		(on crate12 crate4)
		(on crate15 pallet9)
		(on crate16 pallet8)
		(on crate17 pallet18)
		(on crate18 pallet25)
		(on crate19 pallet5)
	)
))
