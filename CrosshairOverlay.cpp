// no preamble
// implementation
#include "CrosshairOverlay.h"
void CrosshairOverlay::run() {
  while (true) {
    auto root_x{0};
    auto root_y{0};
    auto win_x{0};
    auto win_y{0};
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
}
CrosshairOverlay::~CrosshairOverlay() {}
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