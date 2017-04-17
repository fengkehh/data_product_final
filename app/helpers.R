# Data processing code
# Data ingress
if (!file.exists('./data/annual_all_2016.zip')) {
    download.file(url = 'http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/annual_all_2016.zip', destfile = './data/annual_all_2016.zip')
}

if (!file.exists('./data/annual_all_2016.csv')) {
    unzip('./data/annual_all_2016.zip')
}

if (!file.exists('./data/states_and_counties.csv')) {
    download.file(url = 'https://aqs.epa.gov/aqsweb/documents/codetables/states_and_counties.csv', destfile = './data/states_and_counties.csv')    
}

data_2016 <- read.csv('./data/annual_all_2016.csv')

data_states <- read.csv('./data/states_and_counties.csv')

# Conversion
data_2016$Parameter.Code <- factor(data_2016$Parameter.Code)

# Extract Criteria Pollutants
Selected.Code <- c(42101, 12128, 42602, 44201, 88101, 42401)
ind <- match(Selected.Code, data_2016$Parameter.Code)
Parameter.Name <- data_2016$Parameter.Name[ind]

# List for pollutant selection box in shiny
criteriaList <- setNames(as.list(Selected.Code), Parameter.Name)

# Extract criteria measurement data
ind <- (data_2016$Parameter.Code %in% Selected.Code) & !(data_2016$State.Code %in% c(80, 'CC'))

data_criteria <- data_2016[ind, ]

# Geolocation Matching
# Aggregate to get rid of county data and reduce dimension
data_states <- with(data_states, aggregate(list(State.Code = State.Code), 
                                           by = list(State.Name = State.Name, 
                                                     State.Abb = State.Abbreviation), 
                                           FUN = function(x) x[1]))

# indices for the reference state code without Mexico and Canada (already excluded in criteria data)
ref_ind <- !(data_states$State.Code %in% c(80, 'CC'))

data_states <- data_states[ref_ind, ]

data_states$State.Code <- factor(as.numeric(as.character(data_states$State.Code)))