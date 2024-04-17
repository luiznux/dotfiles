import os
import subprocess

default_dir_tree ="""~/{AUR,Torrents,Mangas,Books,Isos,Calibre-Library,Videos,Music,Downloads,
                      Pictures/Screenshots,Documents,Desktop,sandbox, projects/luiznux,.vim,
                      .config/{i3,polybar,ranger,rofi,alacritty,scripts,picom}}"""

def create_directory_tree ():
    if os.path.exists(default_dir_tree):
        print("Default directory tree already exists, skipping mkdir")

    else:
        subprocess.call('mkdir -vp ')
