# For some reason, fonts won't work, 
# this will fix them.
xset fp+ ~/misc/fonts

# Load settings for urxvt
xrdb -merge .Xresources

# Start i3
exec i3
