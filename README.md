# x11_crosshair

A small X11 overlay that draws a click-through crosshair on top of all windows. It provides a lightweight visible pointer overlay useful in situations where the native cursor is invisible, hard to see, or when you want a persistent crosshair for presentations, demos, accessibility, or debugging.

[![x11_crosshair screenshot](https://raw.githubusercontent.com/plops/x11_crosshair/main/screenshot.webp)](https://github.com/plops/x11_crosshair/blob/main/screenshot.webp)


## Features
- Click-through fullscreen overlay
- Thin crosshair with a 1px stippled black line for visibility on light backgrounds
- Attempts to stay on top by raising the override-redirect window each frame

## Common use cases
- Restore a visible targeting aid if the native cursor is missing or invisible
- Presentation or screen-capture overlays to highlight pointer location
- Accessibility assistance for users who need a more visible pointer
- Debugging remote-desktop or compositor issues where the cursor isn't shown

## Dependencies
- X11 development headers (libx11-dev)
- Xfixes development headers (libxfixes-dev)
- pkg-config, cmake, a C++17 compiler

On Debian/Ubuntu:
```
sudo apt install build-essential cmake pkg-config libx11-dev libxfixes-dev
```

## Build
```
mkdir -p build
cd build
cmake ..
make
```

## Run
From the build directory:
```
./x11_crosshair
```
Run it in background if desired:
```
./x11_crosshair &
```

To stop it, kill the process (e.g. `pkill x11_crosshair`).

## What this program accomplishes and how it works

- What it accomplishes
  - Displays a visible crosshair overlay that follows the mouse pointer even when the native cursor is missing or invisible.
  - The overlay is click-through so it does not intercept mouse events; applications continue to receive input normally.

- How it uses the X server (brief)
  - Opens the X display with XOpenDisplay and queries the default screen/root window.
  - Tries to select a 32-bit TrueColor visual (when available) and creates a colormap for correct colors.
  - Creates an override-redirect, fullscreen InputOutput window so the window manager does not decorate or manage it.
  - Uses the XFIXES extension to:
    - Set the ShapeInput region to an empty region (no input area) so the window is click-through.
    - Set the ShapeBounding region to two thin rectangles (horizontal + vertical) that define the visible crosshair area — only those pixels are drawn, the rest is transparent.
  - Uses XQueryPointer (synchronous) each loop to get the current pointer coordinates.
  - Draws the thick white crosshair using XFillRectangles and draws 1px stippled/dashed black lines using a separate GC to improve contrast.
  - Calls XMapRaised/XRaiseWindow and XFlush to ensure the overlay is visible and stays on top.

- Why the overlay is click-through
  - ShapeInput is set to an empty region via XFixesSetWindowShapeRegion. With no input region, the X server forwards input to underlying windows as if the overlay wasn't there.

## Explanation of the sleeps and timing choices

- sleep when XQueryPointer fails (50 ms)
  - If XQueryPointer returns false (rare, e.g., if the server temporarily cannot report pointer position), the code sleeps for 50 ms before retrying. This avoids tight retry loops that would waste CPU and logs while waiting for the X server to respond.

- per-frame sleep (10 ms)
  - After updating the shape and drawing each frame the loop sleeps for 10 ms. This:
    - Limits CPU usage so the overlay does not spin at 100% CPU.
    - Gives a responsive update rate (~100 FPS upper bound) which is well beyond typical perceptual needs for pointer-tracking while keeping motion smooth.
    - Balances responsiveness vs. resource use; adjust this value if you prefer lower CPU or different responsiveness (e.g., 16 ms for ~60 FPS).

- Additional notes on X calls
  - Many Xlib calls are buffered client-side; XFlush is used to send queued requests to the server promptly so changes (map/raise/draw) take effect quickly.
  - XQueryPointer is a synchronous round-trip to the X server to get the current pointer position; it is the most direct, portable way to track the pointer without additional event grabs.

## Notes / Troubleshooting
- Running under tiling window managers (dwm, i3, etc.) is supported; the overlay is created as override-redirect and is raised each frame. If another client still appears above it, try switching focus away from that client or ensure the compositor isn't re-stacking windows.
- If the overlay is invisible or behaves oddly on some hardware/compositors, try using a different visual (the code attempts a 32-bit TrueColor visual when available) or disabling compositor effects as a diagnostic step.
- This tool is intended as a simple, user-space overlay. It does not modify drivers or fix low-level input subsystems; use it when a visible pointer overlay is the desired, pragmatic solution.
