cmake_minimum_required(VERSION 2.8.3)
project(Object_Detection)

find_package(catkin REQUIRED COMPONENTS
  geometry_msgs
  std_msgs
  message_generation
)

add_message_files(FILES Object_Detection.msg)
add_service_files(FILES Poke_Object.srv)

generate_messages(
  DEPENDENCIES
  std_msgs
  geometry_msgs
)

catkin_package(
	CATKIN_DEPENDS message_runtime geometry_msgs std_msgs 
)