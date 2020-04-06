fread('./data/raw/runoff_day/6335020_Q_Day.Cmd.txt')

raw_path <- './data/raw/runoff_day/'
fnames <- list.files(raw_path)
n_station <- length(fnames)
id_length <- 7
runoff_day_raw <- data.table()
id_sname <- runoff_stations[, .(id, sname)]

file_count <- 1

temp_dt <- fread(paste0(raw_path, fnames[file_count]))
station_id <- substr(fnames[file_count], 1, id_length)
temp_dt <- cbind(id = factor(station_id), temp_dt)
