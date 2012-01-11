struct netlink_req_s {
  struct nlmsghdr  nlmsg_info;
  struct rtmsg     rtmsg_info;
  char             buffer[2048];
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
int rtn;

typedef struct
{
  char type;
  char family;
  void * dest;
  char gateway[128];
  unsigned char dest_mask;
  struct rtmsg * rtmsg_ptr;
  int rtmsg_len;
} rttable_entry;
