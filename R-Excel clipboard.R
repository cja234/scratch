x <- "hello world"
#writeClipboard(x)
writeClipboard(as.character(x))

# assign the contents of the clipboard to the vector x
x <- readClipboard()
x

# use the scan function to copy a column of numbers from Excel to R
# x will contain the numbers from Excel as numbers, not as quoted strings
# Note that scan only works with columns of numbers. 
# To copy a row from Excel, first transpose the row in Excel, 
# then copy the result as a column.
x <- scan()
x

# copy a table x to the clipboard in such a way that 
# it can be pasted into Excel preserving the table structure
write.table(x, "clipboard", sep="\t", row.names=FALSE)
