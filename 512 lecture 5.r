library('tidyverse')

vax_data = read_csv(
     "covid19_vaccinations_in_the_united_states.csv", 
     na = 'N/A',
     col_types = cols()
 )

vax_data %>% head


vax_data %>% colnames

colnames(vax_data)

p = ggplot(vax_data, aes(x= `Percent of 18+ Pop with 1+ Doses by State of Residence`))
p = p + geom_histogram()
p


