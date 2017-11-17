#include "ros/ros.h"
#include "object_detection/PokeObject.h"
#include <string>
#include <iostream>

int main(int argc, char **argv)
{
   ros::init(argc, argv, "test_client");
   
     ros::NodeHandle n;
     ros::ServiceClient client = n.serviceClient<object_detection::PokeObject>("/poke_service_node/calculate_poke_position");

     object_detection::PokeObject srv;
     srv.request.detection.type = "";
     srv.request.detection.position.x = 0;
     srv.request.detection.position.y = 0;
     srv.request.detection.position.z = 0;

     if (client.call(srv))
     {
       ROS_INFO("x: %g", srv.response.poke_position.x);
       ROS_INFO("y: %g", srv.response.poke_position.y);
       ROS_INFO("z: %g", srv.response.poke_position.z);
     }
     else
     {
       ROS_ERROR("Failed to call service!");
       return 1;
     }
     return 0;
}