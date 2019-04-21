#!/bin/sh

# make directories if needed
mkdir -p $SCRATCH/politeness_and_coordination/data/videos-raw/
mkdir -p $SCRATCH/politeness_and_coordination/data/videos-archive/
mkdir -p $SCRATCH/politeness_and_coordination/data/aux-raw/

# download and unzip conversation folder
curl -L [conversation_url] > $SCRATCH/politeness_and_coordination/data/videos-raw/convo.zip
unzip $SCRATCH/politeness_and_coordination/data/videos-raw/convo.zip
mv $SCRATCH/politeness_and_coordination/data/videos-raw/convo.zip $SCRATCH/politeness_and_coordination/data/videos-archive/convo.zip

# download and unzip Tweety folder
curl -L [tweety_url] > $SCRATCH/politeness_and_coordination/data/videos-raw/cartoon.zip
unzip $SCRATCH/politeness_and_coordination/data/videos-raw/cartoon.zip
mv $SCRATCH/politeness_and_coordination/data/videos-raw/cartoon.zip $SCRATCH/politeness_and_coordination/data/videos-archive/cartoon.zip

# download and unzip interlocutor-leading map folder
curl -L [map_interlocutor_url] > $SCRATCH/politeness_and_coordination/data/videos-raw/map-interlocutor_leading.zip
unzip $SCRATCH/politeness_and_coordination/data/videos-raw/map-interlocutor_leading.zip
mv $SCRATCH/politeness_and_coordination/data/videos-raw/map-interlocutor_leading.zip $SCRATCH/politeness_and_coordination/data/videos-archive/map-interlocutor_leading.zip

# download and unzip participant-leading map folder
curl -L [map_participant_url] > $SCRATCH/politeness_and_coordination/data/videos-raw/map-participant_leading.zip
unzip $SCRATCH/politeness_and_coordination/data/videos-raw/map-participant_leading.zip
mv $SCRATCH/politeness_and_coordination/data/videos-raw/map-participant_leading.zip $SCRATCH/politeness_and_coordination/data/videos-archive/map-participant_leading.zip

# download roleplay folder
curl -L [role_url] > $SCRATCH/politeness_and_coordination/data/videos-raw/roleplay.zip
unzip $SCRATCH/politeness_and_coordination/data/videos-raw/roleplay.zip
mv $SCRATCH/politeness_and_coordination/data/videos-raw/roleplay.zip $SCRATCH/politeness_and_coordination/data/videos-archive/roleplay.zip

# download auxiliary data files
wget -i $SCRATCH/politeness_and_coordination/scripts/01-data_acquisition/data_file_urls.txt -P $SCRATCH/politeness_and_coordination/data/aux-raw
for i in $SCRATCH/politeness_and_coordination/data/aux-raw/*?raw=1
  do mv "$i" "${i/?raw=1/}"
done
