get_layout <-
function(sg,layout.type="fr1"){

	layout_type=layout.type
<<<<<<< HEAD
	if(layout_type=="fr1"){
		sg$layout = layout.fruchterman.reingold(sg, weights = (abs(E(sg)$weight)))
	}else{
		if(layout_type=="fr2"){
=======
	if(layout_type=="fr2"){
		sg$layout = layout.fruchterman.reingold(sg, weights = (abs(E(sg)$weight)))
	}else{
		if(layout_type=="fr1"){
>>>>>>> 26f914bd644e88f4e9429a4ff8ec70a2f3e910f7
			sg$layout = layout.fruchterman.reingold(sg, weights = (1-abs(E(sg)$weight)))
		}else{
			if(layout_type=="fr"){
				
<<<<<<< HEAD
                sg$layout = layout_with_fr(sg)
               
=======
				sg$layout = layout_with_fr(sg)
>>>>>>> 26f914bd644e88f4e9429a4ff8ec70a2f3e910f7
			}else{
				if(layout_type=="lgl"){
					
					sg$layout = layout.lgl(sg)
				}else{
					if(layout_type=="kk"){
					
						sg$layout = layout_with_kk(sg)
					}else{
						
							if(layout_type=="drl"){
					
								sg$layout = layout_with_drl(sg)
							}else{
								
								if(layout_type=="tree"){
					
									sg$layout = layout_as_tree(sg)
								}else{
									
									if(layout_type=="circle"){
					
										sg$layout = layout_in_circle(sg)
									}else{
										
											if(layout_type=="sphere"){
					
												sg$layout = layout_on_sphere(sg)
											}else{
												
													if(layout_type=="nicely"){
					
														sg$layout = layout_nicely(sg)
													}else{
														
																if(layout_type=="graphopt"){
					
																	sg$layout = layout_with_graphopt(sg)
																}else{
																	
																				if(layout_type=="randomly"){
					
																					sg$layout = layout_randomly(sg)
																				}
																}
													}
											}
									}
								}
							}
					}
					
					
				}
					
			}
			
		}
	}

	return(sg$layout)
}
