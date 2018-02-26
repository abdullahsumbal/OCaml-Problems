		let even (n : int) : bool = n mod 2 = 0




			let rec create_list (l : int list) acc =
			match l with
			| [] -> acc
			| h::t -> create_list t acc
			

			create_list [1,2,3,4,5,6] []
			