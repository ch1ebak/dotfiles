hl.on("hyprland.start", function () 
  hl.exec_cmd("pkill swaybg; swaybg -m fill -i $(shuf -e -n1 $HOME/Obrazy/wallpapers/everforest/*) &")
end)

colors = {
  active = "rgb(a7c080)",
  inactive = "rgb(272e33)",
}

return colors
