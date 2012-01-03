
struct {
  struct nlmsghdr		nlmsg_info;
  struct rtmsg		rtmsg_info;
  char			buffer[2048];
} netlink_req;

int fd;
struct sockaddr_nl local;
struct sockaddr_nl peer;
struct msghdr msg_info;
struct iovec iov_info;
char read_buffer[8192];

struct nlmsghdr *nlmsg_ptr;
char *read_ptr;
int nlmsg_len;
struct rtmsg *rtmsg_ptr;
struct rtattr *rtattr_ptr;
int rtmsg_len;
//char temp_buffer[8192];
int rtn;

typedef struct
{
  char type;
  char family;
  void * dest;
  unsigned char dest_mask;
  struct rtmsg * rtmsg_ptr;
} rttable_entry;


typedef union
{
  unsigned char ip8[16];
  unsigned int ip32[4];
  unsigned long long ip64[2];
} ipv6_u ;

typedef union
{
  unsigned char ip8[4];
  unsigned int ip32;
} ipv4_u ;

typedef struct
{
  ipv6_u addr;
  int mask;
} ipv6_masked;
