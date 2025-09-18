cmake_minimum_required(VERSION 3.10)
project(x11_crosshair VERSION 1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find X11
find_package(X11 REQUIRED)

# Use pkg-config to find Xfixes
find_package(PkgConfig REQUIRED)
pkg_check_modules(XFIXES REQUIRED xfixes)

# Create executable from main.cpp
add_executable(x11_crosshair main.cpp)

# Include directories from found packages
target_include_directories(x11_crosshair PRIVATE
    ${X11_INCLUDE_DIR}
    ${XFIXES_INCLUDE_DIRS}
)

# Link required libraries
target_link_libraries(x11_crosshair PRIVATE
    ${X11_LIBRARIES}
    ${XFIXES_LIBRARIES}
)

# Add some useful compiler flags
target_compile_options(x11_crosshair PRIVATE -Wall -Wextra -Wpedantic)

# If pkg-config reported library dirs, ensure the linker can find them
if(XFIXES_LIBRARY_DIRS)
    link_directories(${XFIXES_LIBRARY_DIRS})
endif()




