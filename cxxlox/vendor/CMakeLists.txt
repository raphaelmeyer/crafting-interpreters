include(FetchContent)

FetchContent_Declare(
  doctest
  GIT_REPOSITORY  https://github.com/doctest/doctest.git
  GIT_TAG         v2.5.1
)

FetchContent_MakeAvailable(doctest)
