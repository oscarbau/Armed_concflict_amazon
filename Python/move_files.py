## Move all files in directory
## by: Oscar Bautista
## date: 05/10/2017
import os, glob, shutil

src_path = raw_input(r"Source root directory: ")
print""
dest = raw_input(r'Destination directory: ')

if not os.path.exists(dest):
        os.makedirs(dest)
  
for root, dirs, files in os.walk(src_path):
    for file in files:
        name=os.path.join(root,file)
        sink=os.path.join(dest,file)
        #shutil.move(name, sink)
        print (file+ " moved from "+root+ " to "+ dest)
        print ""

