cmake_minimum_required(VERSION 2.8.3)
project(knowledge)

add_definitions(-std=c++11)

find_package(catkin REQUIRED COMPONENTS 
	roscpp
	knowrob_common
	knowrob_srdl
	object_detection
	json_prolog
	)


catkin_package(
CATKIN_DEPENDS 
	knowrob_common 
	knowrob_srdl
	roscpp
	object_detection 
	json_prolog
)

include_directories(
  include ${catkin_INCLUDE_DIRS}
)


add_executable(poke_service_node src/poke_service_node.cpp)
target_link_libraries(poke_service_node ${catkin_LIBRARIES})

add_executable(test_client src/test_client.cpp)
target_link_libraries(test_client ${catkin_LIBRARIES})

