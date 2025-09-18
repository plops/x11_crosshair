# x11_crosshair

This small X11 overlay draws a click-through crosshair on top of all windows. It was generated as a practical workaround (via an AI) for a recurring issue where the mouse cursor disappeared on an AMD laptop — instead of restarting X11 each time, this tool provides an overlay cursor that remains visible.

> Note: this project does not fix the underlying driver/hardware issue. It is intended as a lightweight workaround to avoid restarting the X server when the cursor vanishes.

## Features
- Click-through fullscreen overlay
- Thin crosshair with a 1px stippled black line for visibility on light backgrounds
- Attempts to stay on top by raising the override-redirect window each frame

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

## Notes / Troubleshooting
- This overlay is a workaround — if your hardware/driver causes the cursor to disappear repeatedly, consider investigating driver updates or switching to a different driver.
- Running under tiling window managers (dwm, i3, etc.) is supported; the overlay is created as override-redirect and is raised each frame. If another client still appears above it, try switching focus away from that client or ensure the compositor isn't re-stacking windows.
- If the overlay is invisible or behaves oddly on some hardware/compositors, try using a different visual (the code attempts a 32-bit TrueColor visual when available) or disabling compositor effects as a diagnostic step.
