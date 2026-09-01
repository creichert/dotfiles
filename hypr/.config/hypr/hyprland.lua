-- https://wiki.hypr.land/Configuring/

-- Required packages:
-- - uwsm
-- - kitty
-- - hyprpaper
-- - hyprsunset
-- - hyprshot
-- - hyprpicker
-- - wofi
-- - mako
-- - cliphist
-- - playerctl

-- Force gpu to onboard graphics / igpu.
-- UWSM supplies AQ_DRM_DEVICES=/dev/dri/card1 from
-- ~/.config/uwsm/env-hyprland.
--
-- card1: igpu
-- card0: nvidia (1 HDMI, 3 DP)
-- hl.env("AQ_DRM_DEVICES", "/dev/dri/card1")
-- hl.env("AQ_DRM_DEVICES", "/dev/dri/card1:/dev/dri/card2")

-- Force apps to use wayland.
hl.env("ELECTRON_OZONE_PLATFORM_HINT", "auto")
hl.env("HYPRSHOT_DIR", "~/downloads/screenshots")
hl.env("GTK_THEME", "adw-gtk3-dark")


----------------
---- MONITORS ----
----------------

-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
local isLaptop = os.getenv("HOSTNAME") == "laptop"

if isLaptop then
    hl.monitor({ output = "eDP-1", mode = "2560x1440@60", position = "0x0", scale = 1.66666 })
else
    hl.monitor({ output = "DP-1", mode = "3440x1440@144", position = "0x0", scale = 1 })
    hl.monitor({ output = "HDMI-A-1", mode = "1920x1080@144", position = "3440x0", scale = 1, transform = 3 })
    -- improves display but much harder on igpu. leave for testing
    -- hl.monitor({ output = "HDMI-A-1", mode = "3840x2160@60", position = "3440x0", scale = 2, transform = 3 })
    hl.workspace_rule({ workspace = "1", monitor = "DP-1", default = true })
    hl.workspace_rule({ workspace = "2", monitor = "HDMI-A-1", default = true })
end


---------------------
---- MY PROGRAMS ----
---------------------

-- See https://wiki.hypr.land/Configuring/Basics/Variables/
local terminal = "kitty"
local menu = "pkill wofi || wofi --show drun --term=kitty --define=drun-print_desktop_file=true"


-------------------
---- AUTOSTART ----
-------------------

-- See https://wiki.hypr.land/Configuring/Basics/Autostart/
hl.on("hyprland.start", function()
    -- open editor and term for cfg editing
    hl.exec_cmd("uwsm app emacs", { workspace = "special:cfg silent" })
    hl.exec_cmd("uwsm app " .. terminal, { workspace = "special:cfg silent" })

    -- clipboard
    os.remove(os.getenv("HOME") .. "/.cache/cliphist/db")
    hl.exec_cmd("uwsm app -s b -t service -- wl-paste -t text --watch cliphist store")
    hl.exec_cmd("uwsm app -s b -t service -- wl-paste -t image --watch cliphist store")

    -- background services
    hl.exec_cmd("uwsm app -s b -t service hyprpaper")
    hl.exec_cmd("uwsm app -s b -t service hyprsunset")
    hl.exec_cmd("uwsm app -s b -t service hypridle")
    hl.exec_cmd("uwsm app -s b -t service waybar")

    -- hyprpolkitagent is a polkit authentication daemon. It is required for GUI
    -- applications to be able to request elevated privileges.
    --
    -- hl.exec_cmd("systemctl --user start hyprpolkitagent")()
end)


-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Environment-variables/
hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")


-----------------------
---- LOOK AND FEEL ----
-----------------------

-- Refer to https://wiki.hypr.land/Configuring/Basics/Variables/
hl.config({
    general = {
        gaps_in = 5,
        gaps_out = 20,
        border_size = 2,
        -- See the variable types documentation for color syntax.
        col = {
            active_border = { colors = { "rgba(32ccffee)", "rgba(00ff99ee)" }, angle = 45 },
            inactive_border = "rgba(595959aa)",
        },
        -- Set to true to enable resizing windows by clicking and dragging on borders and gaps.
        resize_on_border = false,
        -- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Tearing/ before enabling this.
        allow_tearing = false,
        layout = "dwindle",
    },

    cursor = {
        inactive_timeout = 5,
        default_monitor = 1,
        no_hardware_cursors = true,
        -- If true, will not warp the cursor in many cases (focusing, keybinds, etc).
        -- no_warps = false,
    },

    decoration = {
        rounding = 10,
        rounding_power = 2,
        -- Change transparency of focused and unfocused windows.
        active_opacity = 0.95,
        inactive_opacity = 0.9,
        shadow = {
            enabled = not isLaptop,
            range = 4,
            render_power = 3,
            color = "rgba(1a1a1aee)",
        },
        blur = {
            enabled = true,
            -- size = 3,
            passes = 1,
            vibrancy = 0.1696,
            new_optimizations = true,
            -- xray = true,
            -- special = true,
            -- popups = false,
        },
    },

    animations = {
        enabled = true,
    },
})

-- Default animations, see https://wiki.hypr.land/Configuring/Advanced-and-Cool/Animations/ for more.
hl.curve("easeOutQuint", { type = "bezier", points = { { 0.23, 1 }, { 0.32, 1 } } })
hl.curve("easeInOutCubic", { type = "bezier", points = { { 0.65, 0.05 }, { 0.36, 1 } } })
hl.curve("linear", { type = "bezier", points = { { 0, 0 }, { 1, 1 } } })
hl.curve("almostLinear", { type = "bezier", points = { { 0.5, 0.5 }, { 0.75, 1.0 } } })
hl.curve("quick", { type = "bezier", points = { { 0.15, 0 }, { 0.1, 1 } } })

hl.animation({ leaf = "global", enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "border", enabled = true, speed = 5.39, bezier = "easeOutQuint" })
hl.animation({ leaf = "windows", enabled = true, speed = 4.79, bezier = "easeOutQuint" })
hl.animation({ leaf = "windowsIn", enabled = true, speed = 4.1, bezier = "easeOutQuint", style = "popin 87%" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 1.49, bezier = "linear", style = "popin 87%" })
hl.animation({ leaf = "fadeIn", enabled = true, speed = 1.73, bezier = "almostLinear" })
hl.animation({ leaf = "fadeOut", enabled = true, speed = 1.46, bezier = "almostLinear" })
hl.animation({ leaf = "fade", enabled = true, speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers", enabled = true, speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn", enabled = true, speed = 4, bezier = "easeOutQuint", style = "fade" })
hl.animation({ leaf = "layersOut", enabled = true, speed = 1.5, bezier = "linear", style = "fade" })
hl.animation({ leaf = "fadeLayersIn", enabled = true, speed = 1.79, bezier = "almostLinear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true, speed = 1.39, bezier = "almostLinear" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesIn", enabled = true, speed = 1.21, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesOut", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })


-- See https://wiki.hypr.land/Configuring/Layouts/Dwindle-Layout/ for more.
hl.config({
    dwindle = {
        -- force_split = 0,
        preserve_split = true,
    },
})


hl.config({
    misc = {
        disable_hyprland_logo = true,
        disable_splash_rendering = true,
        mouse_move_enables_dpms = true,
        key_press_enables_dpms = true,
        disable_autoreload = true,
        focus_on_activate = true,
        -- enable_anr_dialog = false,
        -- anr_missed_pings = 5,
        on_focus_under_fullscreen = 1,
        exit_window_retains_fullscreen = true,
    },

    xwayland = {
        enabled = true,
    },
})


---------------
---- INPUT ----
---------------

-- See https://wiki.hypr.land/Configuring/Basics/Variables/
hl.config({
    input = {
        kb_layout = "us",
        -- kb_variant = "",
        -- kb_model = "",
        kb_options = "ctrl:nocaps",
        -- kb_rules = "",
        sensitivity = 1.0,
        repeat_rate = 120,
        repeat_delay = 250,
        follow_mouse = 1,
        touchpad = {
            natural_scroll = false,
        },
    },
})

-- Logitech trackball per-device config.
hl.device({
    name = "logitech-usb-receiver-mouse",
    -- Hold right click to scroll w/ trackball.
    scroll_method = "on_button_down",
    scroll_button = 273,
})


---------------------
---- KEYBINDINGS ----
---------------------

local mainMod = "SUPER" -- Sets "Windows" key as main modifier.

-- Launch apps, manipulate session.
hl.bind(mainMod .. " + P", hl.dsp.exec_cmd("uwsm app -- $(" .. menu .. ")"))
hl.bind(mainMod .. " + SHIFT + P", hl.dsp.exec_cmd("killall wofi || uuctl wofi"))
hl.bind(mainMod .. " + Tab", hl.dsp.window.cycle_next())
hl.bind(mainMod .. " + SHIFT + Tab", hl.dsp.window.cycle_next({ next = false }))
hl.bind(mainMod .. " + F1", hl.dsp.exec_cmd("uwsm app emacs"))
hl.bind(mainMod .. " + W", hl.dsp.focus({ monitor = "DP-1" }))
hl.bind(mainMod .. " + E", hl.dsp.focus({ monitor = "HDMI-A-1" }))
hl.bind(mainMod .. " + Q", hl.dsp.exec_cmd("hyprctl reload"))
-- hl.bind(mainMod .. " + SHIFT + Q", hl.dsp.exit())
hl.bind(mainMod .. " + SHIFT + Q", hl.dsp.exec_cmd("uwsm stop"))
hl.bind(mainMod .. " + N", hl.dsp.exec_cmd("uwsm app " .. terminal))
hl.bind(mainMod .. " + escape", hl.dsp.exec_cmd("makoctl dismiss -a"))
hl.bind(mainMod .. " + CTRL + escape", hl.dsp.exec_cmd("makoctl restore"))

-- Move windows around.
hl.bind(mainMod .. " + SHIFT + H", hl.dsp.window.move({ direction = "left" }))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.window.move({ direction = "right" }))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.window.move({ direction = "up" }))
hl.bind(mainMod .. " + SHIFT + J", hl.dsp.window.move({ direction = "down" }))

-- Resize windows in a submap.
hl.bind(mainMod .. " + SHIFT + R", hl.dsp.submap("resize"))
hl.define_submap("resize", function()
    hl.bind("right", hl.dsp.window.resize({ x = 10, y = 0, relative = true }), { repeating = true })
    hl.bind("left", hl.dsp.window.resize({ x = -10, y = 0, relative = true }), { repeating = true })
    hl.bind("up", hl.dsp.window.resize({ x = 0, y = -10, relative = true }), { repeating = true })
    hl.bind("down", hl.dsp.window.resize({ x = 0, y = 10, relative = true }), { repeating = true })
    hl.bind("escape", hl.dsp.submap("reset"))
end)

-- Special workspaces (scratchpads).
hl.bind(mainMod .. " + L", hl.dsp.workspace.toggle_special("cfg"))
hl.bind(mainMod .. " + R", hl.dsp.workspace.toggle_special("db"))
hl.bind(mainMod .. " + K", hl.dsp.workspace.toggle_special("terms"))

hl.bind(mainMod .. " + space", hl.dsp.window.fullscreen({ mode = "maximized" }))
hl.bind(mainMod .. " + SHIFT + space", hl.dsp.exec_cmd("hyprctl keyword general:layout dwindle"))

hl.bind(mainMod .. " + C", hl.dsp.window.close())
hl.bind(mainMod .. " + T", hl.dsp.window.float())
-- hl.bind(mainMod .. " + P", hl.dsp.window.pseudo())
hl.bind(mainMod .. " + J", hl.dsp.layout("togglesplit"))

-- Move focus with mainMod + arrow keys.
hl.bind(mainMod .. " + left", hl.dsp.focus({ direction = "left" }))
hl.bind(mainMod .. " + right", hl.dsp.focus({ direction = "right" }))
hl.bind(mainMod .. " + up", hl.dsp.focus({ direction = "up" }))
hl.bind(mainMod .. " + down", hl.dsp.focus({ direction = "down" }))

-- Clipboard manual selection.
hl.bind(mainMod .. " + SHIFT + V", hl.dsp.exec_cmd("pkill wofi || cliphist list | wofi --show dmenu | cliphist decode | wl-copy"))

-- Screenshots.
hl.bind(mainMod .. " + SHIFT + S", hl.dsp.exec_cmd("hyprshot -s -m region -o ~/downloads/screenshots/"))
hl.bind("Print", hl.dsp.exec_cmd("hyprshot -s -m window -o ~/downloads/screenshots/"))
hl.bind(mainMod .. " + SHIFT + C", hl.dsp.exec_cmd("hyprpicker --autocopy"))

-- Switch to workspace on the same monitor. Default workspace switching changes
-- screens if that workspace is displayed on another monitor.
hl.bind(mainMod .. " + 1", hl.dsp.focus({ workspace = "1", on_current_monitor = true }))
hl.bind(mainMod .. " + 2", hl.dsp.focus({ workspace = "2", on_current_monitor = true }))
hl.bind(mainMod .. " + 3", hl.dsp.focus({ workspace = "3", on_current_monitor = true }))
hl.bind(mainMod .. " + 4", hl.dsp.focus({ workspace = "4", on_current_monitor = true }))
hl.bind(mainMod .. " + 5", hl.dsp.focus({ workspace = "5", on_current_monitor = true }))
hl.bind(mainMod .. " + 6", hl.dsp.focus({ workspace = "6", on_current_monitor = true }))
hl.bind(mainMod .. " + 7", hl.dsp.focus({ workspace = "7", on_current_monitor = true }))
hl.bind(mainMod .. " + 8", hl.dsp.focus({ workspace = "8", on_current_monitor = true }))
hl.bind(mainMod .. " + 9", hl.dsp.focus({ workspace = "9", on_current_monitor = true }))
hl.bind(mainMod .. " + 0", hl.dsp.focus({ workspace = "10", on_current_monitor = true }))

-- Move active window to a workspace with mainMod + SHIFT + [0-9].
hl.bind(mainMod .. " + SHIFT + 1", hl.dsp.window.move({ workspace = "1", follow = false }))
hl.bind(mainMod .. " + SHIFT + 2", hl.dsp.window.move({ workspace = "2", follow = false }))
hl.bind(mainMod .. " + SHIFT + 3", hl.dsp.window.move({ workspace = "3", follow = false }))
hl.bind(mainMod .. " + SHIFT + 4", hl.dsp.window.move({ workspace = "4", follow = false }))
hl.bind(mainMod .. " + SHIFT + 5", hl.dsp.window.move({ workspace = "5", follow = false }))
hl.bind(mainMod .. " + SHIFT + 6", hl.dsp.window.move({ workspace = "6", follow = false }))
hl.bind(mainMod .. " + SHIFT + 7", hl.dsp.window.move({ workspace = "7", follow = false }))
hl.bind(mainMod .. " + SHIFT + 8", hl.dsp.window.move({ workspace = "8", follow = false }))
hl.bind(mainMod .. " + SHIFT + 9", hl.dsp.window.move({ workspace = "9", follow = false }))
hl.bind(mainMod .. " + SHIFT + 0", hl.dsp.window.move({ workspace = "10", follow = false }))
-- mod+shift+equal -> move window to terms workspace.
hl.bind(mainMod .. " + SHIFT + equal", hl.dsp.window.move({ workspace = "special:terms", follow = false }))

-- Scroll through existing workspaces with mainMod + scroll.
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up", hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging.
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + SHIFT + mouse:272", hl.dsp.window.resize(), { mouse = true })

-- Laptop multimedia keys for volume and LCD brightness.
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 3%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 3%-"), { locked = true, repeating = true })
hl.bind("XF86AudioMute", hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), { locked = true, repeating = true })
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl s 5%+"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl s 5%-"), { locked = true, repeating = true })

-- Requires playerctl.
hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"), { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
-- hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"), { locked = true })


--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

-- See https://wiki.hypr.land/Configuring/Basics/Window-Rules/
-- and https://wiki.hypr.land/Configuring/Basics/Workspace-Rules/

hl.window_rule({
    name = "maximized-red-border",
    match = { fullscreen = true },
    border_color = "rgba(FF0050FF)",
})

-- hl.window_rule({
--     name = "chromium-opacity",
--     match = { class = "(?i)^(chromium|emacs)$" },
--     opacity = "0.95 0.95"
-- })


-- Float file dialog popups and FreeCAD utility windows.
local centeredRules = {
    { name = "windowrule-4", title = "^(.* file.*)$", float = true },
    { name = "windowrule-5", title = "^(.* File.*)$", float = true, size = "(monitor_w*0.5) (monitor_h*0.5)" },
    { name = "windowrule-6", title = "^(.*Task Manager.*)$", float = true },
    { name = "windowrule-7", title = "^(Network Connections)$", float = true },
    { name = "windowrule-8", title = "^(FreeCAD)$", float = true },
    { name = "windowrule-9", title = "^(.*Import file.*)$", size = "(monitor_w*0.5) (monitor_h*0.5)" },
    { name = "windowrule-10", title = "^(.*Export file.*)$", size = "(monitor_w*0.5) (monitor_h*0.5)" },
    { name = "windowrule-11", title = "^(.*Save FreeCAD Document.*)$", size = "(monitor_w*0.5) (monitor_h*0.5)" },
    { name = "windowrule-12", title = "^(.*Open document.*)$", size = "(monitor_w*0.5) (monitor_h*0.5)" },
    { name = "windowrule-13", title = "^(.*Addon Manager.*)$", float = true, size = "(monitor_w*0.75) (monitor_h*0.75)" },
    { name = "windowrule-14", title = "^(.*Expression editor.*)$", float = true, size = "(monitor_w*0.5) (monitor_h*0.05)" },
}

for _, rule in ipairs(centeredRules) do
    hl.window_rule({
        name = rule.name,
        match = { title = rule.title },
        float = rule.float,
        size = rule.size,
        center = true,
    })
end


-- Fix some dragging issues with XWayland.
hl.window_rule({
    name = "xwayland-drag-fix",
    match = {
        class = "^$",
        title = "^$",
        xwayland = true,
        float = true,
        fullscreen = false,
        pin = false,
    },
    no_focus = true,
})

-- Inhibit idle on fullscreen apps.
-- hl.window_rule({ name = "idle-inhibit-fullscreen", match = { class = ".*" }, idle_inhibit = "fullscreen" })

-- Keep window focused
-- hl.window_rule({ name = "stay-focused-chromium-menu", match = { class = "Chromium", initial_title = "menu window" }, stay_focused = true })
