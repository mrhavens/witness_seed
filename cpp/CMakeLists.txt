
### 3. CMakeLists.txt: `/cpp/CMakeLists.txt`

```cmake
cmake_minimum_required(VERSION 3.15)
project(WitnessSeed CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find or fetch dependencies
include(FetchContent)

# Crow
FetchContent_Declare(
    crow
    GIT_REPOSITORY https://github.com/CrowCpp/Crow.git
    GIT_TAG master
)
FetchContent_MakeAvailable(crow)

# cpp-httplib
FetchContent_Declare(
    httplib
    GIT_REPOSITORY https://github.com/yhirose/cpp-httplib.git
    GIT_TAG master
)
FetchContent_MakeAvailable(httplib)

# nlohmann/json
FetchContent_Declare(
    json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.2
)
FetchContent_MakeAvailable(json)

# stduuid
FetchContent_Declare(
    uuid
    GIT_REPOSITORY https://github.com/mariusbancila/stduuid.git
    GIT_TAG master
)
FetchContent_MakeAvailable(uuid)

# Add executable
add_executable(witness_seed witness_seed.cpp)

# Link libraries
target_link_libraries(witness_seed PRIVATE
    Crow::Crow
    httplib
    nlohmann_json::nlohmann_json
    stduuid
)

# Include directories
target_include_directories(witness_seed PRIVATE
    ${crow_SOURCE_DIR}/include
    ${httplib_SOURCE_DIR}
    ${json_SOURCE_DIR}/include
    ${uuid_SOURCE_DIR}/include
)