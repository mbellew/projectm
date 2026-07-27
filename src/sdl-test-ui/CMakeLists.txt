if(NOT ENABLE_SDL_UI OR ENABLE_EMSCRIPTEN OR CMAKE_SYSTEM_NAME STREQUAL Android)
    return()
endif()

# Camera capture for the video-history texture. Enabled by default on macOS
# (AVFoundation) and Linux (V4L2); other platforms have no backend yet and fall
# back to a build without camera capture.
set(_video_capture_default OFF)
if(APPLE OR CMAKE_SYSTEM_NAME STREQUAL Linux)
    set(_video_capture_default ON)
endif()
option(ENABLE_VIDEO_CAPTURE "Enable camera capture in the SDL test UI." ${_video_capture_default})

# Per-platform capture backend. _have_video_capture tracks whether a real backend
# is compiled in (drives the PROJECTM_VIDEO_CAPTURE_ENABLED define below).
set(_video_capture_sources videoCapture.hpp)
set(_have_video_capture OFF)
if(ENABLE_VIDEO_CAPTURE AND APPLE)
    list(APPEND _video_capture_sources videoCapture_macos.mm)
    set(_have_video_capture ON)
elseif(ENABLE_VIDEO_CAPTURE AND CMAKE_SYSTEM_NAME STREQUAL Linux)
    list(APPEND _video_capture_sources videoCapture_linux.cpp)
    set(_have_video_capture ON)
endif()

# Luxonis OAK (depthai-core) depth camera for depth-enhanced foreground masking.
# Off by default. When on, we locate an *installed* depthai-core via
# find_package(depthai). depthai-core uses the Hunter package manager, which
# requires HunterGate to run before project() — so it CANNOT be consumed via
# FetchContent/add_subdirectory. Build & install it standalone instead, e.g.:
#
#   git clone --recursive -b v2.30.0 https://github.com/luxonis/depthai-core
#   cmake -S depthai-core -B build-depthai -DBUILD_SHARED_LIBS=ON \
#         -DDEPTHAI_OPENCV_SUPPORT=OFF -DCMAKE_INSTALL_PREFIX=$HOME/.local/depthai-core \
#         -DCMAKE_POLICY_VERSION_MINIMUM=3.5
#   cmake --build build-depthai --target install --parallel
#
# then configure this project with
#   -DENABLE_LUXONIS=ON -DCMAKE_PREFIX_PATH=$HOME/.local/depthai-core
# (or -Ddepthai_DIR=$HOME/.local/depthai-core/lib/cmake/depthai). If depthai
# isn't found we still build a no-op stub so the app links unchanged.
option(ENABLE_LUXONIS "Enable Luxonis OAK depth-camera capture in the SDL test UI." OFF)

set(_luxonis_found OFF)
if(ENABLE_LUXONIS)
    find_package(depthai CONFIG QUIET)
    if(depthai_FOUND)
        set(_luxonis_found ON)
        message(STATUS "Luxonis: using installed depthai-core (${depthai_DIR}).")
    else()
        message(WARNING "ENABLE_LUXONIS is on but depthai-core was not found. "
                "Build/install it standalone and pass -DCMAKE_PREFIX_PATH to its "
                "install prefix (see src/sdl-test-ui/CMakeLists.txt). Building the no-op stub.")
    endif()
endif()

if(_luxonis_found)
    list(APPEND _video_capture_sources depthCapture.hpp depthCapture_oak.cpp)
else()
    list(APPEND _video_capture_sources depthCapture.hpp depthCapture_stub.cpp)
endif()

# ONNX Runtime person-segmentation masking. Off by default. Uses an installed
# onnxruntime (the prebuilt osx-arm64 package ships a CMake config + the CoreML
# execution provider for ANE/GPU). Configure with:
#   -DENABLE_ONNX_SEG=ON -DCMAKE_PREFIX_PATH=$HOME/.local/onnxruntime
# A real vs stub backend is chosen so the app always links.
option(ENABLE_ONNX_SEG "Enable ONNX person-segmentation masking in the SDL test UI." OFF)

set(_onnx_found OFF)
if(ENABLE_ONNX_SEG)
    # The prebuilt onnxruntime ships a CMake config, but its imported target
    # points at a non-existent include/onnxruntime subdir (headers live in
    # include/). Locate headers + lib directly instead (searches CMAKE_PREFIX_PATH).
    find_path(ONNXRUNTIME_INCLUDE_DIR onnxruntime_cxx_api.h)
    find_library(ONNXRUNTIME_LIBRARY onnxruntime)
    if(ONNXRUNTIME_INCLUDE_DIR AND ONNXRUNTIME_LIBRARY)
        set(_onnx_found ON)
        message(STATUS "ONNX seg: using onnxruntime (${ONNXRUNTIME_LIBRARY}).")
    else()
        message(WARNING "ENABLE_ONNX_SEG is on but onnxruntime was not found. "
                "Install the prebuilt package and pass -DCMAKE_PREFIX_PATH to it. "
                "Building the no-op stub.")
    endif()
endif()

if(_onnx_found)
    list(APPEND _video_capture_sources onnxCommon.hpp onnxCommon.cpp)
    list(APPEND _video_capture_sources segMask.hpp segMask.cpp)
    list(APPEND _video_capture_sources poseTracker.hpp poseTracker.cpp)
    list(APPEND _video_capture_sources nudeNet.hpp nudeNet.cpp)
else()
    list(APPEND _video_capture_sources segMask.hpp segMask_stub.cpp)
    list(APPEND _video_capture_sources poseTracker.hpp poseTracker_stub.cpp)
    list(APPEND _video_capture_sources nudeNet.hpp nudeNet_stub.cpp)
endif()

add_executable(projectM-Test-UI
        ConfigFile.cpp
        ConfigFile.h
        audioCapture.cpp
        audioCapture.hpp
        loopback.cpp
        loopback.hpp
        pmSDL.cpp
        pmSDL.hpp
        poseTouchBridge.cpp
        poseTouchBridge.hpp
        projectM_SDL_main.cpp
        screenshot.cpp
        screenshot.hpp
        setup.cpp
        setup.hpp
        ${_video_capture_sources}
        )

if(_luxonis_found)
    target_compile_definitions(projectM-Test-UI PRIVATE PROJECTM_LUXONIS_ENABLED=1)
    # depthai-core exposes depthai::core (and depthai::opencv); we only need core.
    target_link_libraries(projectM-Test-UI PRIVATE depthai::core)
endif()

if(_onnx_found)
    target_compile_definitions(projectM-Test-UI PRIVATE PROJECTM_ONNX_ENABLED=1)
    target_include_directories(projectM-Test-UI PRIVATE ${ONNXRUNTIME_INCLUDE_DIR})
    target_link_libraries(projectM-Test-UI PRIVATE ${ONNXRUNTIME_LIBRARY})
endif()

if(APPLE)
    # Embed Info.plist into the Mach-O (__TEXT,__info_plist) so a bare CLI / LaunchAgent launch has
    # a TCC identity (bundle id) and the camera/microphone consent prompts carry usage strings.
    # Without NSCameraUsageDescription macOS kills the process the moment it opens the camera.
    set(_info_plist "${CMAKE_CURRENT_SOURCE_DIR}/Info.plist")
    target_link_options(projectM-Test-UI PRIVATE
            "-Wl,-sectcreate,__TEXT,__info_plist,${_info_plist}")
    set_target_properties(projectM-Test-UI PROPERTIES LINK_DEPENDS "${_info_plist}")
endif()

if(_have_video_capture)
    target_compile_definitions(projectM-Test-UI PRIVATE PROJECTM_VIDEO_CAPTURE_ENABLED=1)
endif()

if(ENABLE_VIDEO_CAPTURE AND APPLE)
    set_source_files_properties(videoCapture_macos.mm PROPERTIES
            COMPILE_FLAGS "-fobjc-arc"
            )
    # stb_image: the idle stand-in (setIdleStandIn) decodes its PNG via the vendored stb decoder,
    # compiled unconditionally in pmSDL.cpp -- so link it on the macOS video-capture path too
    # (previously only the Linux branch linked it, so macOS failed to link).
    find_package(Threads REQUIRED)
    target_link_libraries(projectM-Test-UI
            PRIVATE
            Threads::Threads
            stb_image
            "-framework AVFoundation"
            "-framework CoreMedia"
            "-framework CoreVideo"
            "-framework Foundation"
            )
elseif(ENABLE_VIDEO_CAPTURE AND CMAKE_SYSTEM_NAME STREQUAL Linux)
    # The V4L2 backend runs capture on a std::thread and decodes MJPEG frames via
    # the vendored stb_image (same decoder the Renderer uses).
    find_package(Threads REQUIRED)
    target_link_libraries(projectM-Test-UI PRIVATE Threads::Threads stb_image)
endif()

# screenshot.cpp compiles the vendored stb_image_write (header-only) itself, so it needs the
# stb include dir but not the object library -- which is only linked on the video-capture path.
target_include_directories(projectM-Test-UI
        PRIVATE
        $<TARGET_PROPERTY:stb_image,INTERFACE_INCLUDE_DIRECTORIES>
        )

target_link_libraries(projectM-Test-UI
        PRIVATE
        libprojectM::playlist
        GLM::GLM
        SDL2::SDL2
        SDL2::SDL2main
        ${CMAKE_DL_LIBS}
        glad
        )

if(MSVC)
    if(CMAKE_VERSION VERSION_GREATER_EQUAL "3.16")
        set_target_properties(projectM-Test-UI
                PROPERTIES
                VS_DPI_AWARE "PerMonitor"
                )
    else()
        message(AUTHOR_WARNING
                "You're using a CMake version less than 3.16 with Visual Studio.\n"
                "The resulting projectMSDL executable will not be DPI-aware and possibly render at a "
                "lower-than-expected resolution on high-DPI displays.")
    endif()

    add_custom_command(TARGET projectM-Test-UI POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_FILE:libprojectM::projectM>
            $<TARGET_FILE:libprojectM::playlist>
            $<TARGET_FILE_DIR:projectM-Test-UI>
            )
endif()
