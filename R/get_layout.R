get_layout <-
function(sg,layout.type="fr1"){

	layout_type=layout.type
	if(layout_type=="fr2"){
		sg$layout = layout.fruchterman.reingold(sg, weights = (abs(E(sg)$weight)))
	}else{
		if(layout_type=="fr1"){
			sg$layout = layout.fruchterman.reingold(sg, weights = (1-abs(E(sg)$weight)))
		}else{
			if(layout_type=="fr"){
				
				sg$layout = layout_with_fr(sg)
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
