## dotfiles
Here are all my dotfiles, feel free to use them!

To install dotfiles, please make sure to install GNU Stow and use it like so:
`stow folder_name -t "home/user"`

where `folder_name` is the name of the folder you want to get the dots and `user` is your username

the only exception here is the folder `power_management` which contains `systemd` entries. In order to use these, you need to copy them in `/etc/systemd/system/` and enable them through `systemctl`.
