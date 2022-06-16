switch(as.character(progress$level),
       "1" = {
           level_topic <<- "basic_stats"
           samples <- generate_samples(world$seed, 2, world$streams)
           generated_questions <<- generate_sample_question_set(samples, "lowest")
           plot_details <<- prepare_summary_stat_barplots(samples)
       },
       "2" = {
           level_topic <<- "basic_stats"
           generated_questions <<- generate_variable_type_question_set(level=2)
           plot_details <<- list()
       },
       "3" = {
           level_topic <<- "basic_stats"
           generated_questions <<- generate_weather_questions(world$monthly_weather)
           plot_details <<- prepare_weather_timeseries(world$monthly_weather)
       },
       "4" = {
           level_topic <<- "basic_stats"
           generated_questions <<- generate_histogram_question_set(world$daily_weather$windspeed)
           plot_details <<- prepare_histogram(world$daily_weather$windspeed)
       },
       "5" = {
           level_topic <<- "basic_stats"
           samples <- generate_samples(1, 2, world$shelter_materials)
           generated_questions <<- generate_boxplot_question_set(samples)
           plot_details <<- prepare_boxplot(samples, "Total rain leaked (mm)")
       },
       "6" = {
           level_topic <<- "basic_stats"
           generated_questions <<- generate_scatterplot_question_set(world$trauma_assessments, linear=TRUE)
           plot_details <<- prepare_scatterplot(world$trauma_assessments)
       },
       "7" = {
           level_topic <<- "basic_stats"
           samples <- generate_samples(1, 2, world$fruit_rot)
           generated_questions <<- generate_dotplot_question_set(samples,
                                                                 direction = sample(c("lowest","highest"),1))
           plot_details <<- prepare_dotplot(samples, "Days from picking till first rot")
       },
       "8" = {
           level_topic <<- "basic_stats"
           generated_questions <<- generate_two_histograms_question_set(world$daily_weather$windspeed_comparison)
           plot_details <<- prepare_two_histograms(world$daily_weather$windspeed_comparison)
       })