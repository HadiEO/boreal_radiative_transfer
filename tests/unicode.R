legend("topleft", xpd = TRUE,
       legend = c(expression(LAI[eff]), expression(paste(italic(i[0]), " = 1 - ", italic(cgf(theta[sun])))), expression(paste(italic(cgf(theta[view])), " = 1 - ECC      ")), "CI", expression(italic(omega[L])), expression(italic(rho[g]))),
       col = cols, pch = c(rep(1,4), 2, 2),
       lty = c(rep(1, 4), 2, 2),  lwd = rep(1,6), ncol = 2, cex = 1.05, bty = "n")  # remove inset and x.intersp

# Unicode
# https://unicode-table.com/en/#03B8
# theta: U+03B8; italic: U+1D703     
# omega: U+03C9
# rho: U+03C1



# expression(LAI[eff]) 
expression(paste(italic(i[0]), " = 1 - ", italic(cgf(theta[sun])))) 
expression(paste(italic(cgf(theta[view])), " = 1 - ECC      ")) 
"CI" 
expression(italic(omega[L]))
expression(italic(rho[g]))


italic(cgf(theta[view])


       
# windows(family='Times') #name for Times in default windows gr device lib
# windows(family = "Times New Roman")
windows()
plot(1:15, 1:15, type="n")
# text(5,9, expression(italic(β)~":"~italic(α))) #1                     # This doesn't look like the proper greek symbol
text(5,7, paste("\u03B8", "," , "\u03C9", ",", "\u03C1", sep=" "), font=3) #2 # This is italicize but how to have subscript? font=3 makes the italic
# text(5,5, expression(italic(theta)~", "~italic(omega)~", "~italic(rho))) #3               # This doesn't work to italicize
# text(5,3, expression(italic("\u1D6F3")~", "~italic("\u03C9")~", "~italic("\u03C1"))) #4    # This doesn't look like the proper greek symbol
# text(5,1.5, expression(italic("\u03B2")~plain(":")~italic("\u03B1"))) #5 # This doesn't look like the proper greek symbol
# text(5,10, expression("\u03B8"), font=3) #2                           # This doesnt work
# text(5,8, paste("\u03B8", "," , "\u03C9", ",", "\u03C1", sep=" "), font=4) #2 # This is italicize but how to have subscript? Font=4 makes bold italic
text(5,8, paste("\u1D6F3", "," , "\u03C9", ",", "\u03C1", sep=" ")) #2 # This is italicize but how to have subscript? font=3 makes the italic




