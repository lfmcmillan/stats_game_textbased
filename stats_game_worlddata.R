generate_user_world <- function(user_name) {

    while (file.exists(paste0(user_name,"_world.Rdata"))) {
        recreate_yesno <- menu(c("Yes","No"),title="World already exists for this username. Recreate?")
        if (recreate_yesno == 2) return()
    }

    streams <- generate_stream_params(sample(1:2,1))

    crop_values <- read.csv("crops.csv",header=TRUE)
    row.names(crop_values) <- crop_values$name
    crop_names <- c("sunflower","purple bean","speckled bean","orange cabbage",
                        "red broccoli","pebble melon","blue spinach","black tomato",
                        "purple seaweed","fronded seaweed","pink nut","purple potato")
    ncrops <- length(crop_names)
    crops <- data.frame(name=crop_names,
                        preferred_site=rep(NA,ncrops),
                        preferred_water=rep(NA,ncrops),
                        preferred_season=rep(NA,ncrops),
                        protein=generate_crop_params(crop_names,crop_values,"protein")
    )
    # vitC=generate_vitC(crop_names),
    # vitA=generate_vitA(crop_names),
    # non_sugar_carb=generate_non_sugar_carb(crop_names),
    # sugar=generate_sugar(crop_names),
    # b12=generate_b12(crop_names),
    # calcium=generate_calcium(crop_names),
    # iron=generate_iron(crop_names),
    # fibre=generate_fibre(crop_names),
    # potassium=generate_potassium(crop_na),
    # magnesium=generate_magnesium(crop_na),
    # sodium=generate_sodium(crop_na),
    # phosphorus=generate_phosphorus(crop_na)

    list(streams=streams, crops=crops)
}

generate_stream_params <- function(most_contaminated_idx) {
    mean1 <- runif(1, 3, 8)
    mean2 <- runif(1, 3, 8) + sample(5:10,1)
    means <- c(mean1, mean2)

    if (most_contaminated_idx == 2) means <- sort(means, decreasing=FALSE)

    params <- list(distribution="normal",
                   names=c("Stream A","Stream B"),
                   means=means,
                   SDs=runif(2,1,5),
                   sizes=c(24,24))
}

generate_crop_params <- function(crop_names, crop_values, value_type) {
    ncrops <- length(crop_names)
    output <- rep(NA,ncrops)

    for (i in 1:ncrops)
    {
        min_val <- crop_values[crop_names[i],paste0(value_type,"_min")]
        max_val <- crop_values[crop_names[i],paste0(value_type,"_max")]
        output[i] <- runif(1,min=min_val,max=max_val)
    }

    output
}

find_any_word <- function(patterns, x) {
    any(sapply(patterns, function(pattern) {
        (length(grep(pattern,x)) > 0)
    }))
}