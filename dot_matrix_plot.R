#
# Thesis title: The Role of International Commitments in Combating the Illicit Distribution of Cocaine.
# Author: Raquel Baeta
# Data Source: data

# Packages to install and load
install.packages("reshape2")

library(reshape2) # restructure and aggregate

# Plot title: A dot matrix plot of state commitment to United Nations Treaties

# Step 1: Create a data frame state commitment data
df <- data.frame(ctry = c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Luxembourg",
                          "Netherlands", "Norway", "Portugal", "Spain", "Sweden", "Switzerland"),
                 UN_1961 = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                 UN_1971 = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                 UN_1988 = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

# Step 2: Reduce the size of plotting symbols for better spacing
symbol_size <- 3

# Step 3: Only extract the data from the data frame
df_matrix <- as.matrix(df[, -1])

# Step 4: Reshaped the data frame 
newdata <- reshape2::melt(df_matrix)
newdata  

# Step 5: Create column names
ctry_labs <- df$ctry
treaty_labs <- c("UN (1961)", "UN (1971)", "UN (1988)")

# Step 5: Adjust margins and spacing
par(mar=c(2, 2, 2, 6), xpd=TRUE)  # Increased right margin for more space
with(newdata,
     plot(as.numeric(Var2), rev(Var1),
          pch=c(1, 19)[value+1],
          cex=symbol_size,
          axes=FALSE,
          xlab="",
          ylab="")
)
mtext(rev(ctry_labs), side=2, line=1, at=1:length(ctry_labs), las=1)
mtext(treaty_labs, side=3, line=2, at=1:length(treaty_labs))

# Step 6: Specify the path to save the plot as a PNG image in the working directory
png("dot_matrix.png", width=1500, height=800) 
with(newdata,
     plot(as.numeric(Var2), rev(Var1),
          pch=c(1, 19)[value+1],
          cex=symbol_size,
          axes=FALSE,
          xlab="",
          ylab="")
)
mtext(rev(ctry_labs), side=2, line=1, at=1:length(ctry_labs), las=1)
mtext(treaty_labs, side=3, line=2, at=1:length(treaty_labs))

# End
