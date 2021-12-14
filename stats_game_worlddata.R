generate_user_world <- function(user_name) {

    while (file.exists(paste0(user_name,"_world.Rdata"))) {
        recreate_yesno <- menu(c("Yes","No"),title="World already exists for this username. Recreate?")
        if (recreate_yesno == 2) return()
    }

    streams <- generate_stream_params(sample(1:2,1))

    weather <- list(rainfall=generate_weather_data("rainfall"),
                    max_temp=generate_weather_data("max_temp"),
                    min_temp=generate_weather_data("min_temp"))

    shelter_materials <- generate_shelter_material_params(sample(1:2,1))

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

    list(streams=streams, crops=crops, weather=weather)
}

generate_stream_params <- function(most_contaminated_idx) {
    mean1 <- runif(1, 3, 8)
    mean2 <- runif(1, 3, 8) + sample(2:4,1)
    means <- c(mean1, mean2)

    if (most_contaminated_idx == 1) {
        means <- sort(means, decreasing=TRUE)
    }

    params <- list(distribution="normal",
                   names=c("Stream A","Stream B"),
                   means=means,
                   SDs=runif(2,3,5),
                   sizes=c(12,12))
}

generate_weather_data <- function(type) {
    nyears <- 4

    year <- rep(2017:2020,each=12)
    month_num <- rep(1:12, times=nyears)
    month_name <- rep(month.abb, times=nyears)
    switch(tolower(type),
           "max_temp"={
               # basic_seq <- seq(0,1,length=12)
               # full_seq <- rep(basic_seq, times=nyears)
               # randomness <- runif(12*nyears,-0.25,0.25) +
               #     rep(seq(from=0,by=0.1,length.out=nyears), each=12)
               # sinusoidal <- sin((full_seq+0.12)*2*pi) + randomness
               # values <- sinusoidal*(35-10)/2+20
               max_temp <- read.csv("monthly_max_temp_hong_kong.csv", header=TRUE)
               subset_years <- sample_years(max_temp, nyears)
               value <- max_temp$Temperature[max_temp$Year %in% subset_years]
               ylab <- "Temperature (degrees C)"
               plot_title <- "Maximum Temperature"
               question_name <- "maximum monthly temperature"
               direction <- "highest"
           },
           "min_temp"={
               min_temp <- read.csv("monthly_min_temp_hong_kong.csv", header=TRUE)
               subset_years <- sample_years(min_temp, nyears)
               value <- min_temp$Temperature[min_temp$Year %in% subset_years]
               ylab <- "Temperature (degrees C)"
               plot_title <- "Minimum Temperature"
               question_name <- "minimum monthly temperature"
               direction <- "lowest"
           },
           "rainfall"={
               rain <- read.csv("monthly_rainfall_hong_kong_longformat.csv", header=TRUE)
               subset_years <- sample_years(rain, nyears)
               value <- rain$Rainfall[rain$Year %in% subset_years]
               ylab <- "Total Rainfall (mm)"
               plot_title <- "Rainfall"
               question_name <- "total monthly rainfall"
               direction <- "highest"
           },
           "windspeed"={

           })

    df <- data.frame(year=year, month_num=month_num, month_name=month_name, value=value)
    output <- list(type=type, ylab=ylab, plot_title=plot_title,
                   question_name=question_name, direction=direction, df=df)

    output
}

sample_years <- function(series, nyears) {
    years <- unique(series$Year)
    total_years <- length(years)
    if (total_years %% 10 == 0) {
        year_mat <- matrix(years, ncol=10)
    } else if (total_years %% 7 == 0) {
        year_mat <- matrix(years, ncol=7)
    } else {
        browser()
    }
    year_row_idx <- sample(1:nrow(year_mat),1)
    year_row <- year_mat[year_row_idx,]
    subset_years <- sample(year_row, nyears)
}

generate_shelter_material_params <- function(most_effective_idx) {
    mean1 <- runif(1, 3, 8)
    mean2 <- runif(1, 3, 8) + sample(2:4,1)
    means <- c(mean1, mean2)

    if (most_effective_idx == 2) {
        means <- sort(means, decreasing=TRUE)
    }

    params <- list(distribution="normal",
                   names=c("Palm","Pacific Etang"),
                   means=means,
                   SDs=runif(2,3,5),
                   sizes=c(12,12))
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

