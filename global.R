plot_any_type <- function(plot_details) {
    switch(plot_details$plot_type,
           "bar"=barplot(plot_details$y, names.arg=plot_details$x,
                         main=plot_details$title, ylab=plot_details$ylab, col="darkblue"),
           "box"=boxplot(plot_details$y, names=plot_details$x, col="lightblue", ylab=plot_details$ylab),
           "dot"=stripchart(values ~ sample_names,
                            data=data.frame(values=plot_details$y,sample_names=plot_details$x),
                            xlab=plot_details$xlab, pch=1, cex=1.3, method="stack"),
           "scatter"=plot(plot_details$x, plot_details$y,
                          xlab=plot_details$xlab, ylab=plot_details$ylab),
           "hist"=hist(plot_details$x, xlab=plot_details$xlab, main=plot_details$title, col="lightblue"),
           "weather_timeseries"={
               colours <- palette("Dark2")
               plot(plot_details$x[[1]], plot_details$y[[1]],
                    type="l", ylim=plot_details$ylim, col=colours[1], lwd=2,
                    xlab=plot_details$xlab, xaxt='n', ylab=plot_details$ylab,
                    main=plot_details$title)
               axis(side=1, at=plot_details$axis_at, labels=plot_details$axis_labels)
               legend("topleft",legend=plot_details$legend_text,
                      col=colours[seq_along(plot_details$x)], lwd=2)
               for (i in 2:length(plot_details$x)) {
                   lines(plot_details$x[[i]], plot_details$y[[i]],
                         col=colours[i], lwd=2)
               }
           }
    )
}
