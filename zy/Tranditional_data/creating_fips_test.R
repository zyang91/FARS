# Example dataset
data <- data.frame(
  state_code = c(1, 1, 2, 2, 10),
  county_code = c(101, 102, 101, 102, 103)
)

# Create a unique code for each county
data$unique_county_code <- sprintf("%02d%03d", data$state_code, data$county_code)

# View the result
print(data)
