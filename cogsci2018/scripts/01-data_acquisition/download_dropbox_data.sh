#!/bin/sh

# wget-dropbox_data_files.sh

# download all videos
wget -i $SCRATCH/politeness_and_coordination/scripts/01-data_acquisition/video_file_urls.txt -P $SCRATCH/politeness_and_coordination/data/videos-raw

# download auxiliary data files
wget -i $SCRATCH/politeness_and_coordination/scripts/01-data_acquisition/data_file_urls.txt -P $SCRATCH/politeness_and_coordination/data/aux-raw

# rename videos
for i in $SCRATCH/politeness_and_coordination/data/videos-raw/*.mov?raw=1
  do mv "$i" "${i/.mov?raw=1/.mov}"
done

# rename auxiliary data files
for i in $SCRATCH/politeness_and_coordination/data/aux-raw/*?raw=1
  do mv "$i" "${i/?raw=1/}"
done
