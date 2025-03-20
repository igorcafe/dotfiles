package.path = mp.command_native({"expand-path", "~~/script-modules/?.lua;"})..package.path
local list = require "scroll-list"

list.header = "Video options"
list.wrap = true
list.list = {}

list.keybinds = {
   {'DOWN', 'scroll_down', function() list:scroll_down() end, {repeatable = true}},
   {'UP', 'scroll_up', function() list:scroll_up() end, {repeatable = true}},
   {'PGDWN', 'move_pagedown', function() list:move_pagedown() end, {}},
   {'PGUP', 'move_pageup', function() list:move_pageup() end, {}},
   {'HOME', 'move_begin', function() list:move_begin() end, {}},
   {'END', 'move_end', function() list:move_end() end, {}},
   {'ESC', 'close_browser', function() list:close() end, {}},
   {'ENTER', 'choose', function() choose() end, {}}
}

local video_options = {
   {label = "1080p 60fps h264 mp4", id = "299"},
   {label = " 720p 60fps h264 mp4", id = "298"},
   {label = "1080p 30fps h264 mp4", id = "137"},
   {label = " 720p 30fps h264 mp4", id = "136"},
   {label = " 480p 30fps h264 mp4", id = "135"},
   {label = " 360p 30fps h264 mp4", id = "134"},
   {label = " 720p 30fps h264 [live]", id = "95"},
}

for i = 1, #video_options do
   local item = {}
   item.ass = video_options[i].label
   list.list[i] = item
end

list:update()

function choose()
   local pos = mp.get_property_number("time-pos")
   mp.command("stop")
   mp.msg.info("current position is", pos)
   mp.msg.info("changing quality to", video_options[list.selected].id, video_options[list.selected].label)
   list:close()

   mp.osd_message(list.list[list.selected].ass)
   mp.set_property("ytdl-raw-options", "format=((" .. video_options[list.selected].id .. ")+(bestaudio))/best")

   local should_seek = true

   local function on_seekable(_, seekable)
      if seekable and should_seek then
         should_seek = false
         mp.set_property_number("time-pos", pos)
         mp.unobserve_property(on_seekable)
      end
   end

   mp.observe_property("seekable", "bool", on_seekable)
   mp.commandv("loadfile", mp.get_property("path"), "replace")
end

mp.add_key_binding("Ctrl+TAB", function() list:toggle() end)
