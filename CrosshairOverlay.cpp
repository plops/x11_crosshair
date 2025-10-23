// no preamble
// implementation
#include "CrosshairOverlay.h"
#include <algorithm>
void CrosshairOverlay::run() {
  auto thickness{3};
  auto rects{std::array<XRectangle, 2>()};
  auto event{XEvent()};
  auto last_x{-1};
  auto last_y{-1};
  while (true) {
    XNextEvent(display, &event);
    if (MotionNotify == event.type) {
      const auto root_x{event.xmotion.x_root};
      const auto root_y{event.xmotion.y_root};
      if (last_x == root_x && last_y == root_y) {
        continue;
      }
      last_x = root_x;
      last_y = root_y;
      {
        rects[0].x = 0;
        rects[0].y = std::max(0, (root_y - thickness) / 2);
        rects[0].width = DisplayWidth(display, screen);
        rects[0].height = thickness;
        rects[1].x = std::max(0, (root_x - thickness) / 2);
        rects[1].y = 0;
        rects[1].width = thickness;
        rects[1].height = DisplayHeight(display, screen);
        {
          auto bounding{
              XFixesCreateRegion(display, rects.data(), rects.size())};
          XFixesSetWindowShapeRegion(display, window, ShapeBounding, 0, 0,
                                     bounding);
          XFixesDestroyRegion(display, bounding);
        }
        XClearWindow(display, window);
        XFillRectangles(display, window, gc, rects.data(), rects.size());
        XDrawLine(display, window, black_gc, 0, root_y,
                  DisplayWidth(display, screen) - 1, root_y);
        XDrawLine(display, window, black_gc, root_x, 0, root_x,
                  DisplayHeight(display, screen) - 1);
        XRaiseWindow(display, window);
        XFlush(display);
        std::this_thread::sleep_for(std::chrono::milliseconds(16));
      }
    }
  }
}
CrosshairOverlay::CrosshairOverlay() {
  display = XOpenDisplay(nullptr);
  screen = DefaultScreen(display);
  root = RootWindow(display, screen);
  auto *visual{DefaultVisual(display, screen)};
  auto depth{DefaultDepth(display, screen)};
  auto vinfo{XVisualInfo()};
  if (XMatchVisualInfo(display, screen, 32, TrueColor, &vinfo)) {
    visual = vinfo.visual;
    depth = vinfo.depth;
  }
  auto attrs{XSetWindowAttributes{
      .background_pixmap = None,
      .border_pixel = 0,
      .override_redirect = True,
      .colormap = XCreateColormap(display, root, visual, AllocNone)}};
  auto attr_mask{CWColormap | CWBorderPixel | CWBackPixmap |
                 CWOverrideRedirect};
  window = XCreateWindow(display, root, 0, 0, DisplayWidth(display, screen),
                         DisplayHeight(display, screen), 0, depth, InputOutput,
                         visual, attr_mask, &attrs);
  {
    auto inputRegion{XFixesCreateRegion(display, nullptr, 0)};
    XFixesSetWindowShapeRegion(display, window, ShapeInput, 0, 0, inputRegion);
    XFixesDestroyRegion(display, inputRegion);
  }
  XSelectInput(display, root, PointerMotionMask);
  XMapRaised(display, window);
  XFlush(display);
  gc = XCreateGC(display, window, 0, nullptr);
  XSetForeground(display, gc, WhitePixel(display, screen));
  black_gc = XCreateGC(display, window, 0, nullptr);
  XSetForeground(display, black_gc, BlackPixel(display, screen));
  XSetLineAttributes(display, black_gc, 1, LineOnOffDash, CapButt, JoinMiter);
  auto dashes{std::array<const char, 2>({1, 1})};
  XSetDashes(display, black_gc, 0, dashes.data(), dashes.size());
}
CrosshairOverlay::~CrosshairOverlay() {
  if (!display) {
    return;
  }
  if (gc) {
    XFreeGC(display, gc);
  }
  if (black_gc) {
    XFreeGC(display, black_gc);
  }
  if (window) {
    XDestroyWindow(display, window);
  }
  XCloseDisplay(display);
}
Display *CrosshairOverlay::getDisplay() { return display; }
void CrosshairOverlay::setDisplay(Display *display) { this->display = display; }
const int &CrosshairOverlay::getScreen() { return screen; }
void CrosshairOverlay::setScreen(int screen) { this->screen = screen; }
const Window &CrosshairOverlay::getRoot() { return root; }
void CrosshairOverlay::setRoot(Window root) { this->root = root; }
const Window &CrosshairOverlay::getWindow() { return window; }
void CrosshairOverlay::setWindow(Window window) { this->window = window; }
const GC &CrosshairOverlay::getGc() { return gc; }
void CrosshairOverlay::setGc(GC gc) { this->gc = gc; }
const GC &CrosshairOverlay::getBlackGc() { return black_gc; }
void CrosshairOverlay::setBlackGc(GC black_gc) { this->black_gc = black_gc; }