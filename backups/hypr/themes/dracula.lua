hl.on("hyprland.start", function () 
  hl.exec_cmd("pkill swaybg; swaybg -m fill -i $(shuf -e -n1 $HOME/Obrazy/wallpapers/dracula/*) &")
end)

colors = {
  active = "rgb(5AF78E)",
  inactive = "rgb(4D4D4D)",
}

return colors
