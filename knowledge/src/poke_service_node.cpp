#include "ros/ros.h"
#include "object_detection/PokeObject.h"
#include "json_prolog/prolog.h"
#include <string>
#include <sstream>
  
using namespace json_prolog;

bool calculate_poke_position(object_detection::PokeObject::Request  &req, object_detection::PokeObject::Response &res)
{
	std::stringstream ss;
	ss << "calculate_poke_position(" << req.detection.position.x << "," 
	                                 << req.detection.position.y << "," 
	                                 << req.detection.position.z << ",RX,RY,RZ)";
	std::string query = ss.str();

    Prolog pl;
  	PrologBindings bdg = pl.once(query);

    if(&bdg != NULL)
    {
      res.poke_position.x = std::stod(bdg["RX"].toString());
      res.poke_position.y = std::stod(bdg["RY"].toString());
      res.poke_position.z = std::stod(bdg["RZ"].toString());

      return true;
    }
    else
    {
      return false;
    }
}
   
int main(int argc, char **argv)
{
    ros::init(argc, argv, "poke_service_node");
    ros::NodeHandle nh("~");
   
    ros::ServiceServer service = nh.advertiseService("calculate_poke_position", calculate_poke_position);
    ROS_INFO("Ready to calc.");
    ros::spin();
   
    return 0;
}