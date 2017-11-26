cmake_minimum_required(VERSION 2.8.3)
project(vision_msgs)

find_package(catkin REQUIRED COMPONENTS
  geometry_msgs
  std_msgs
  message_generation
)

add_message_files(FILES ObjectInformation.msg)
add_service_files(FILES GetObjectInfo.srv)

generate_messages(
  DEPENDENCIES
  std_msgs
  geometry_msgs
)

catkin_package(
        CATKIN_DEPENDS message_runtime geometry_msgs std_msgs 
)

include_directories(
  ${catkin_INCLUDE_DIRS}
)

