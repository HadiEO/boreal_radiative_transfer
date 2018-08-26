
# Read testNilson result --------------------------------------------------

testNilson <- read_csv2("results/testNilson_notSpeciesSpecific_res.csv")

# Read gaps ---------------------------------------------------------------  

gaps <- read_csv2("results/gaps.csv") 

# Merge gaps and testNilson  ------------------------------------------------

gaps.testNilson <- left_join(gaps, testNilson, by = c("ID" = "Forest_ID"))



# Plot gap fractions --------------------------------------------------------------------
ring.centers <- c(7, 23, 38, 53, 68)
x11()
for(i in 1:nrow(gaps.testNilson)) {
  # Measured gap fractions
  with(gaps.testNilson[i,], 
       plot(ring.centers,
       c(gaps1, gaps2, gaps3, gaps4, gaps5),
       ylim = c(0,0.6)))
  # Modelled gap fractions
  with(gaps.testNilson[i,], 
       points(ring.centers,
            c(GAPS1_estimate, GAPS2_estimate, GAPS3_estimate, GAPS4_estimate, GAPS5_estimate),
            pch = 19))
  locator(1)
}


# Plot PAI_estimate vs LAIeff ---------------------------------------------

x11()
ggplot(gaps.testNilson, aes(x = LAIeff, y = GI_estimate, col = Dominant_species, shape = Dominant_species)) +
  geom_point() + geom_abline(slope = 1, intercept = 0) + 
  scale_x_continuous(limits = c(0,6)) + 
  scale_y_continuous(limits = c(0,6))


# Write to disk -----------------------------------------------------------

write.csv2(gaps.testNilson, "results/gaps_and_testNilson.csv")

