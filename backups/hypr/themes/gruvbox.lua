hl.on("hyprland.start", function () 
  hl.exec_cmd("pkill swaybg; swaybg -m fill -i $(shuf -e -n1 $HOME/Obrazy/wallpapers/gruvbox/*) &")
end)

colors = {
  active = "rgb(98971a)",
  inactive = "rgb(504945)",
}

return colors
