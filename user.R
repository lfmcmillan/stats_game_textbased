observeNewUserButton <- observe({
    if (iv$is_valid()) {
        if (file.exists(paste0(input$username,".Rdata"))) {
            output$userMessage <- renderText({"Sorry, that username already exists. Please enter a different one."})
        } else {
            progress$level <- 1

            ## Note use of <<- means world object is updated at session level
            world <<- generate_user_world(input$username)
            save(progress,world,file=paste0(input$username,".Rdata"))

            show("introPage")
            hide("userPage")
        }
    } else {
        iv$enable() # Start showing validation feedback
    }
})
bindEvent(observeNewUserButton, input$newUserButton)

observeIntroNextButton <- observe({
    progress$done_intro <- TRUE
    progress$level <- first_level
    save(progress, world, file=paste0(input$username,".Rdata"))

    show("levelPage")
    hide("introPage")
})
bindEvent(observeIntroNextButton, input$introNextButton)

observeLoadUserButton <- observe({
    if (iv$is_valid()) {
        userfilename <- paste0(input$username,".Rdata")
        if (!file.exists(userfilename)) {
            output$userMessage <- renderText({"Sorry, no user found with that username. Please try a different username or create a new one:"})
        } else {
            load(userfilename)

            show("progressPage")
            hide("userPage")
        }
    } else {
        iv$enable() # Start showing validation feedback
    }
})
bindEvent(observeLoadUserButton, input$loadUserButton)