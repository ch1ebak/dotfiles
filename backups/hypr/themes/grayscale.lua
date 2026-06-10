hl.on("hyprland.start", function () 
  hl.exec_cmd("pkill swaybg; swaybg -m fill -i $(shuf -e -n1 $HOME/Obrazy/wallpapers/grayscale/*) &")
end)

colors = {
  active = "rgb(cccccc)",
  inactive = "rgb(727272)",
}

return colors
