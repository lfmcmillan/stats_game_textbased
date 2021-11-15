game <- function() {
    user_response <- menu(c("New user","Load user"),title="Welcome to Stats Island v1.0. What would you like to do?")
    if (user_response == 1) {
        create_user()
    } else if (user_response == 2) {
        load_user()
    }
}

create_user <- function() {
    user_name <- readline(prompt="Please enter the username you want:")

    while (file.exists(paste0(user_name,".Rdata"))) {
        user_name <- readline(prompt="Sorry, that username already exists. Please enter a different one:")
    }

    cat(paste("Thanks! Creating new user",user_name,"now.\n"))
    level <- 0
    save(level,file=paste0(user_name,".Rdata"))

    intro(user_name)
}

intro <- function(user_name) {
    cat("This is Stats Island. You and 100 other people crash landed near the island and swam to shore.\n")
    cat("You discovered that the island is deserted, but there is a hut that has some scientific equipment and historical records.\n")
    cat("It appears that this island was once a research outpost.\n")
    cat("A crate washes ashore containing some luggage, including a laptop and a portable solar panel charger.\n")
    cat("You can use the hut to perform scientific experiments, and analyse data on the laptop.\n")
    cat("The existing data, and any new experiments, will help you work out how to survive on the island.\n")
    cat("None of the other survivors knows anything about statistics, but they will help as much as they can.\n")

    done_intro <- TRUE
    save(done_intro,level=1, file=paste0(user_name,".Rdata"))
}