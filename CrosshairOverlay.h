#ifndef CROSSHAIROVERLAY_H
#define CROSSHAIROVERLAY_H

// header 
class CrosshairOverlay  {
        public:
         CrosshairOverlay ()       ;   
         ~CrosshairOverlay ()       ;   
        
        Display* getDisplay ()       ;   
        void setDisplay (Display* display)       ;   
        
        const int& getScreen ()       ;   
        void setScreen (int screen)       ;   
        private:
        Display* display {nullptr};
        int screen {0};
};

#endif /* !CROSSHAIROVERLAY_H */