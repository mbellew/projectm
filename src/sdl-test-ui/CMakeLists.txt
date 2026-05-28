if(NOT ENABLE_SDL_UI OR ENABLE_EMSCRIPTEN OR CMAKE_SYSTEM_NAME STREQUAL Android)
    return()
endif()

# Camera capture for the video-history texture. Enabled on macOS by default; other
# platforms get a no-op stub via the CMake conditional below.
set(_video_capture_default OFF)
if(APPLE)
    set(_video_capture_default ON)
endif()
option(ENABLE_VIDEO_CAPTURE "Enable camera capture in the SDL test UI." ${_video_capture_default})

set(_video_capture_sources videoCapture.hpp)
if(ENABLE_VIDEO_CAPTURE AND APPLE)
    list(APPEND _video_capture_sources videoCapture_macos.mm)
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
        projectM_SDL_main.cpp
        setup.cpp
        setup.hpp
        ${_video_capture_sources}
        )

if(ENABLE_VIDEO_CAPTURE AND APPLE)
    target_compile_definitions(projectM-Test-UI PRIVATE PROJECTM_VIDEO_CAPTURE_ENABLED=1)
    set_source_files_properties(videoCapture_macos.mm PROPERTIES
            COMPILE_FLAGS "-fobjc-arc"
            )
    target_link_libraries(projectM-Test-UI
            PRIVATE
            "-framework AVFoundation"
            "-framework CoreMedia"
            "-framework CoreVideo"
            "-framework Foundation"
            )
endif()

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
