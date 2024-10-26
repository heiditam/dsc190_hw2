# EDA
video_data = read.table("videodata.txt", header=TRUE)
video_multiple = read.table("videoMultiple.txt", header=TRUE)

video_data[video_data == 99] <- NA
video_multiple[video_multiple == 99] <- NA

summary(video_data)
summary(video_multiple)

