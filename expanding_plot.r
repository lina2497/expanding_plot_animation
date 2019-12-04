#load packages
pacman::p_load(tidyverse,
               tweenr,
               viridis,
               magick,
               gifski)

#function to wrap tween from tweenr
tween_wrap<-function(x){
  unlist(tween(c(0,x),10))
}


#define data
farm <- tibble(
  cats = c(tween_wrap(1),
           rep(1, 40)),
  cows = c(rep(0, 10),
           tween_wrap(4),
           rep(4, 30)),
  dogs = c(rep(0, 20),
           tween_wrap(7),
           rep(7, 20)),
  horses = c(rep(0, 30),
             tween_wrap(15),
             rep(15, 10)),
  sheep = c(rep(0, 40),
            tween_wrap(40))
)%>%
  mutate(frame=1:50)%>%
  gather(key = species,
         value= number,
         -frame)

#specify number of frames
n_frames<-max(farm$frame)

# generate temp .png images
temp_dir <- tempdir()
img_frames <-
  file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))


#generate plots
message(paste("Generating", n_frames, "temporary .png images..."))
for (i in seq_len(n_frames)) {
  
  
  
  if(i<=10) {
    df<-filter(farm, species == "cats")
  }
  if (between(i, 11, 20)) {
    df<-filter(farm, species %in% c("cats", "cows"))
  }
  if (between(i, 21, 30)) {
    df<-filter(farm, species %in% c("cats", "cows", "dogs"))
  }
  if (between(i, 31, 40)) {
    df<-filter(farm, species %in% c("cats", "cows", "dogs", "horses"))
  }
  if (between(i, 41, 50)) {
    df<-filter(farm, species %in% c("cats", "cows", "dogs", "horses", "sheep"))
  }
  
  
  
  message(paste(" - image", i, "of", n_frames))
  ggsave(plot = filter(df, frame==i)%>%
           ggplot(aes(x=species,y=number, fill=species))+
           geom_col()+
           scale_fill_viridis(discrete = T),
         filename = img_frames[i])
  
}

#specify duration of gif and file name
duration=3
file="expanding plot.gif"

#render gif
gifski::gifski(img_frames,delay = duration / n_frames, gif_file = file)

