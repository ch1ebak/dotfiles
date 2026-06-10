hl.on("hyprland.start", function () 
  hl.exec_cmd("pkill swaybg; swaybg -m fill -i $(shuf -e -n1 $HOME/Obrazy/wallpapers/tokyonight/*) &")
end)

colors = {
  active = "rgb(7aa2f7)",
  inactive = "rgb(414868)",
}

return colors
