# Start

# Thesis title: The Role of International Commitments in Combating the Illicit Distribution of Cocaine.
# Author: Raquel Baeta

# Set the working directory
setwd("~/Desktop/working-sessions")

# Load packages
library(reshape2)
library(wesanderson)

# Dot matrix plot
df <- data.frame(ctry = c("AUT", "BEL", "DNK", "FIN", "DEU", "GRC", "IRL", "LUX", "NLD", "NOR", "PRT", "ESP", "SWE", "CHE"),
                 UN_1961 = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                 UN_1971 = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                 UN_1988 = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

# Reduce the size of plotting symbols for better spacing
symbol_size <- 3.5

df_matrix <- as.matrix(df[, -1])  # only the data
newdata <- reshape2::melt(df_matrix)
newdata  # reshaped

ctry_labs <- df$ctry
treaty_labs <- c("UN (1961)", "UN (1971)", "UN (1988)")

# Adjust margins and spacing
par(mar=c(2, 2, 2, 2) + 0.1, xpd=TRUE)

# Specify the path to save the plot in the working directory as a PNG
pdf("dot_matrix.pdf", width=6, height=6) # Adjust figure dimensions

# Get the GrandBudapest2 palette
grandbudapest2 <- wes_palette("GrandBudapest2")

with(newdata,
     plot(as.numeric(Var2), rev(Var1),
          pch=c(1, 19)[value+1],
          col=grandbudapest2[value+1], # color dots based on value
          cex=symbol_size,
          axes=FALSE,
          xlab="",
          ylab="")
 )

mtext(rev(ctry_labs), side=2,line=1 , at=1:length(ctry_labs), las=1)
mtext(treaty_labs , side=3,line=2 , at=1:length(treaty_labs))

# Add a title to your plot at the bottom
mtext("State Commitment to United Nations Conventions", side=1,line=2)

# Save the plot in the working directory
dev.off()

# End
