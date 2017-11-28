cmake_minimum_required(VERSION 2.8.3)
project(knowledge_msgs)

find_package(catkin REQUIRED COMPONENTS
  std_msgs
  message_generation
  geometry_msgs
)

add_service_files(FILES LogStartAction.srv LogFinishAction.srv GetFixedKitchenObjects.srv)

generate_messages(
  DEPENDENCIES
  std_msgs
  geometry_msgs
)

catkin_package(
        CATKIN_DEPENDS message_runtime std_msgs geometry_msgs
)

include_directories(
  ${catkin_INCLUDE_DIRS}
)