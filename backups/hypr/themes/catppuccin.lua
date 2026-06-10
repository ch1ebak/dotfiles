hl.on("hyprland.start", function () 
  hl.exec_cmd("pkill swaybg; swaybg -m fill -i $(shuf -e -n1 $HOME/Obrazy/wallpapers/catppuccin/*) &")
end)

colors = {
  active = "rgb(cba6f7)",
  inactive = "rgb(45475a)",
}

return colors
