#include <glib.h>

#define MAXBUFSIZE 65536 // Max UDP Packet size is 64 Kbyte
#define CONTROL_SOCKET_PATH "/tmp/pwzd.c.tener.sock"
#define MULTICAST_ADDR "224.0.0.213"
#define MULTICAST_PORT 6969

struct daemon_opts {
  char * control_socket_path; // -l
  char * multicast_addr; // -m
  char * config_file; // -c 
  int verbose; // -v, -q
  int port; // -p
  int multicast_loop; // -o
  int hello_interval; // -i
  int die_time; // -d
};

enum command {
  FULLINFO, // complete info 
  INFO,     // info on specific topic
  ADD,      // add new interest
  DEL,      // del old interest
  QUIT      // make daemon quit
};

struct client_opts {
  char * control_socket_path; // -l
  char * add_del_arg; // argument for -a / -d / -i
  enum command cmd;
};

typedef struct daemon_opts daemon_opts;
typedef struct client_opts client_opts;


struct daemon_data {
  GHashTable * my_interests; // własne zainteresowania
  GHashTable * other_interests; // zainteresowania innych
};

typedef struct daemon_data daemon_data;


struct host_info
{
  time_t last_seen;
  GHashTable * interests;
};
typedef struct host_info host_info;
