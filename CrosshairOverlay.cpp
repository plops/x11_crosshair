// no preamble
// implementation
#include "CrosshairOverlay.h"
CrosshairOverlay::CrosshairOverlay() {}
CrosshairOverlay::~CrosshairOverlay() {}
Display *CrosshairOverlay::getDisplay() { return display; }
void CrosshairOverlay::setDisplay(Display *display) { this->display = display; }
const int &CrosshairOverlay::getScreen() { return screen; }
void CrosshairOverlay::setScreen(int screen) { this->screen = screen; }