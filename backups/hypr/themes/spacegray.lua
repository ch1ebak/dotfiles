hl.on("hyprland.start", function () 
  hl.exec_cmd("pkill swaybg; swaybg -m fill -i $(shuf -e -n1 $HOME/Obrazy/wallpapers/spacegray/*) &")
end)

colors = {
  active = "rgb(b04b57)",
  inactive = "rgb(4c4f56)",
}

return colors
