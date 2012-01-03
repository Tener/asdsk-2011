#include <bits/sockaddr.h>
#include <stdio.h>
#include <asm/types.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <sys/socket.h>
#include <unistd.h>
#include <arpa/inet.h>

#include <strings.h>
#include <string.h>
#include <stdlib.h>

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

void prepare_rttable_entry( struct rtmsg * rtmsg_ptr, rttable_entry * entry )
{
  entry->dest = NULL;
  entry->dest_mask = 0;
  entry->rtmsg_ptr = rtmsg_ptr;
  entry->type = rtmsg_ptr->rtm_type;
  entry->family = rtmsg_ptr->rtm_family;

  struct rtattr * rtattr_ptr = (struct rtattr *) RTM_RTA(rtmsg_ptr);
  int rtmsg_len = RTM_PAYLOAD(nlmsg_ptr);
  for(;RTA_OK(rtattr_ptr, rtmsg_len); rtattr_ptr = RTA_NEXT(rtattr_ptr, rtmsg_len)) {
    switch(rtattr_ptr->rta_type)
      {
      case RTA_DST:
        {
          int addr_size = rtmsg_ptr->rtm_family == AF_INET ? 4 : 16;
          entry->dest = malloc(addr_size);
          memcpy(entry->dest, RTA_DATA(rtattr_ptr), addr_size);
          entry->dest_mask = rtmsg_ptr->rtm_dst_len;
        }
      }
  }

}

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

ipv6_u masked_ipv6( ipv6_u a, int mask )
{
  unsigned long long foo(unsigned long long v, int masklen)
  {
    unsigned long long masking = -1;
    masking = masking << (64-masklen);
    return (v & masking);
  }

  if (mask > 64)
    {
      mask -= 64;
      a.ip64[1] = foo(a.ip64[1], mask);
    }
  else
    {
      a.ip64[1] = 0;
      a.ip64[0] = foo(a.ip64[0], mask);
    }
  return a;
}

int subsumes_ipv6( ipv6_masked a, ipv6_masked b )
{
  if (a.mask > b.mask)
    {
      return 0;
    }

  ipv6_u am = masked_ipv6( a.addr, a.mask );
  ipv6_u bm = masked_ipv6( b.addr, a.mask );

  return ((am.ip64[0] == bm.ip64[0]) && (am.ip64[1] == bm.ip64[1])) ? 1 : 0;
}

ipv6_u ipv4_to_ipv6( ipv4_u a )
{
  ipv6_u b;
  b.ip32[0] = a.ip32;
  b.ip32[1] = 0;
  b.ip32[2] = 0;
  b.ip32[3] = 0;
  return b;
}

int compare_ipv6( ipv6_u a, ipv6_u b )
{
  if (a.ip64[0] < b.ip64[0])
    {
      return -1;
    }
  else if (a.ip64[0] > b.ip64[0])
    {
      return 1;
    }
  else
    {
      if (a.ip64[1] < b.ip64[1])
        {
          return -1;
        }
      else if (a.ip64[1] > b.ip64[1])
        {
          return 1;
        }
      else
        {
          return 0;
        }
    }
}

int compare_ipv4( ipv4_u a, ipv4_u b )
{
  return compare_ipv6(ipv4_to_ipv6(a), ipv4_to_ipv6(b));
}


/**
 * Send a request to NETLINK to send the routing table
 */
void send_route_read_request(unsigned char fam)
{
  bzero(&local, sizeof(local));
  local.nl_family = AF_NETLINK;
  local.nl_pad = 0;
  local.nl_pid = getpid();
  local.nl_groups = 0;
  if(bind(fd, (struct sockaddr*) &local, sizeof(local)) < 0) {
    printf("Error in sock bind\n");
    exit(1);
  }


  bzero(&peer, sizeof(peer));
  peer.nl_family = AF_NETLINK;
  peer.nl_pad = 0;
  peer.nl_pid = 0;
  peer.nl_groups = 0;

  bzero(&msg_info, sizeof(msg_info));
  msg_info.msg_name = (void *) &peer;
  msg_info.msg_namelen = sizeof(peer);

  bzero(&netlink_req, sizeof(netlink_req));

  netlink_req.nlmsg_info.nlmsg_len = NLMSG_LENGTH(sizeof(struct rtmsg));
  netlink_req.nlmsg_info.nlmsg_flags = NLM_F_REQUEST | NLM_F_DUMP;
  netlink_req.nlmsg_info.nlmsg_type = RTM_GETROUTE;

  netlink_req.rtmsg_info.rtm_family = fam; // IPv6: AF_INET6
  netlink_req.rtmsg_info.rtm_table = RT_TABLE_MAIN; // TODO: extra tables

  iov_info.iov_base = (void *) &netlink_req.nlmsg_info;
  iov_info.iov_len = netlink_req.nlmsg_info.nlmsg_len;
  msg_info.msg_iov = &iov_info;
  msg_info.msg_iovlen = 1;

  rtn = sendmsg(fd, &msg_info, 0);
  if(rtn < 0) {
    printf("Error in sendmsg\n");
    exit(1);
  }
}

/**
 * Read the output sent by NETLINK for the read route table request given
 * in function send_route_read_request()
 */
void read_route_request_results()
{
  bzero(read_buffer, 8192);

  read_ptr = read_buffer;
  nlmsg_len = 0;
  while(1) {
    rtn = recv(fd, read_ptr, 4096, 0);
    if(rtn < 0) {
      printf("Error in recv\n");
      exit(1);
    }
    nlmsg_ptr = (struct nlmsghdr *) read_ptr;

    if(nlmsg_ptr->nlmsg_type == NLMSG_DONE) {
      break;
    }

    read_ptr = read_ptr + rtn;
    nlmsg_len = nlmsg_len + rtn;
  }
}

void show_route_table(enum rt_class_t t)
{
  switch(t)
    {
    case RT_TABLE_UNSPEC:   printf("Table=RT_TABLE_UNSPEC"); break;
    case RT_TABLE_COMPAT:   printf("Table=RT_TABLE_COMPAT"); break;
    case RT_TABLE_DEFAULT:  printf("Table=RT_TABLE_DEFAULT"); break;
    case RT_TABLE_MAIN:     printf("Table=RT_TABLE_MAIN"); break;
    case RT_TABLE_LOCAL:    printf("Table=RT_TABLE_LOCAL"); break;
    case RT_TABLE_MAX:      printf("Table=RT_TABLE_MAX"); break;
    default:
      printf("Table=***UNKNOWN CONSTANT***");
    };

  printf("\t");

}

void show_route_type(unsigned char t)
{
  switch(t)
    {
    case RTN_UNSPEC:      printf("Type=RTN_UNSPEC     "); break; 
    case RTN_UNICAST:	  printf("Type=RTN_UNICAST    "); break; 
    case RTN_LOCAL:	  printf("Type=RTN_LOCAL      "); break; 
    case RTN_BROADCAST:	  printf("Type=RTN_BROADCAST  "); break; 
    case RTN_ANYCAST:	  printf("Type=RTN_ANYCAST    "); break; 
    case RTN_MULTICAST:	  printf("Type=RTN_MULTICAST  "); break; 
    case RTN_BLACKHOLE:	  printf("Type=RTN_BLACKHOLE  "); break; 
    case RTN_UNREACHABLE: printf("Type=RTN_UNREACHABLE"); break; 
    case RTN_PROHIBIT:	  printf("Type=RTN_PROHIBIT   "); break; 
    case RTN_THROW:	  printf("Type=RTN_THROW      "); break; 
    case RTN_NAT:	  printf("Type=RTN_NAT	      "); break; 
    case RTN_XRESOLVE:	  printf("Type=RTN_XRESOLVE   "); break; 
    default:
      printf("Type=***UNKNOWN CONSTANT***");
    };

  printf("\t");

}

void show_route_scope(unsigned char t)
{
  switch(t)
    {
    case RT_SCOPE_UNIVERSE: printf("Scope=RT_SCOPE_UNIVERSE"); break; 
    case RT_SCOPE_SITE:	    printf("Scope=RT_SCOPE_SITE    "); break; 
    case RT_SCOPE_LINK:	    printf("Scope=RT_SCOPE_LINK    "); break; 
    case RT_SCOPE_HOST:	    printf("Scope=RT_SCOPE_HOST    "); break; 
    case RT_SCOPE_NOWHERE:  printf("Scope=RT_SCOPE_NOWHERE "); break; 
    default:
      printf("Scope=***UNKNOWN CONSTANT***");
    }

  printf("\t");
}

void show_route_protocol(unsigned char t)
{
  switch(t)
    {
    case RTPROT_UNSPEC:   printf("Protocol=RTPROT_UNSPEC  "); break; 
    case RTPROT_REDIRECT: printf("Protocol=RTPROT_REDIRECT"); break; 
    case RTPROT_KERNEL:	  printf("Protocol=RTPROT_KERNEL  "); break; 
    case RTPROT_BOOT:	  printf("Protocol=RTPROT_BOOT	  "); break; 
    case RTPROT_STATIC:	  printf("Protocol=RTPROT_STATIC  "); break; 
    case RTPROT_GATED:	  printf("Protocol=RTPROT_GATED	  "); break; 
    case RTPROT_RA:	  printf("Protocol=RTPROT_RA	  "); break; 
    case RTPROT_MRT:	  printf("Protocol=RTPROT_MRT	  "); break; 
    case RTPROT_ZEBRA:	  printf("Protocol=RTPROT_ZEBRA	  "); break; 
    case RTPROT_BIRD:	  printf("Protocol=RTPROT_BIRD	  "); break; 
    case RTPROT_DNROUTED: printf("Protocol=RTPROT_DNROUTED"); break; 
    case RTPROT_XORP:	  printf("Protocol=RTPROT_XORP	  "); break; 
    case RTPROT_NTK:	  printf("Protocol=RTPROT_NTK	  "); break; 
    case RTPROT_DHCP:	  printf("Protocol=RTPROT_DHCP	  "); break; 
      
    default:
      printf("Protocol=***UNKNOWN CONSTANT***");
    }

  printf("\t");
}

const char * attr_to_name(unsigned char t)
{
  switch(t)
    {
    case RTA_UNSPEC:    return "RTA_UNSPEC"   ; 
    case RTA_DST:	return "RTA_DST"      ; 
    case RTA_SRC:	return "RTA_SRC"      ; 
    case RTA_IIF:	return "RTA_IIF"      ; 
    case RTA_OIF:	return "RTA_OIF"      ; 
    case RTA_GATEWAY:	return "RTA_GATEWAY"  ; 
    case RTA_PRIORITY:	return "RTA_PRIORITY" ; 
    case RTA_PREFSRC:	return "RTA_PREFSRC"  ; 
    case RTA_METRICS:	return "RTA_METRICS"  ; 
    case RTA_MULTIPATH: return "RTA_MULTIPATH"; 
    case RTA_PROTOINFO: return "RTA_PROTOINFO"; 
    case RTA_FLOW:	return "RTA_FLOW"     ;  
    case RTA_CACHEINFO: return "RTA_CACHEINFO"; 
    case RTA_SESSION:	return "RTA_SESSION"  ; 
    case RTA_MP_ALGO:	return "RTA_MP_ALGO"  ; 
    case RTA_TABLE:	return "RTA_TABLE"    ; 
    case RTA_MARK:	return "RTA_MARK"     ; 
    }
  return "**UNKOWN ATTRIBUTE**";
}

// short versions
const char * attr_to_name_short(unsigned char t)
{
  switch(t)
    {
    case RTA_UNSPEC:	return "UNSPEC"	  ; 
    case RTA_DST:	return "DST"	  ; 
    case RTA_SRC:	return "SRC"	  ; 
    case RTA_IIF:	return "IIF"	  ; 
    case RTA_OIF:	return "OIF"	  ; 
    case RTA_GATEWAY:	return "gw"  ; 
    case RTA_PRIORITY:	return "prio" ; 
    case RTA_PREFSRC:	return "prefsrc"  ; 
    case RTA_METRICS:	return "METRICS"  ; 
    case RTA_MULTIPATH: return "MULTIPATH"; 
    case RTA_FLOW:	return "FLOW"	  ;  
    case RTA_CACHEINFO: return "CACHEINFO"; 
    case RTA_TABLE:	return "table"    ; 
    case RTA_MARK:	return "MARK"     ; 
    case RTA_PROTOINFO: return "PROTOINFO*"; 
    case RTA_SESSION:	return "SESSION*"  ; 
    case RTA_MP_ALGO:	return "MP_ALGO*"  ; 
    }
  return "**UNKOWN ATTRIBUTE**";
}


void show_route_table_short(enum rt_class_t t)
{
  printf(" ");

  switch(t)
    {
    case RT_TABLE_UNSPEC:   printf("UNSPEC"); break;
    case RT_TABLE_COMPAT:   printf("COMPAT"); break;
    case RT_TABLE_DEFAULT:  printf("DEFAULT"); break;
    case RT_TABLE_MAIN:	    printf("MAIN"); break;
    case RT_TABLE_LOCAL:    printf("LOCAL"); break;
    case RT_TABLE_MAX:	    printf("MAX"); break;
    default:
      printf("Table=***UNKNOWN CONSTANT***");
    };

}
void show_route_type_short(unsigned char t)
{
  switch(t)
    {
    case RTN_UNSPEC:      printf(" "); break; 
    case RTN_UNICAST:	  printf("U"); break; 
    case RTN_LOCAL:	  printf("L"); break; 
    case RTN_BROADCAST:	  printf("B"); break; 
    case RTN_ANYCAST:	  printf("A"); break; 
    case RTN_MULTICAST:	  printf("M"); break; 
    case RTN_BLACKHOLE:	  printf("h"); break; 
    case RTN_UNREACHABLE: printf("r"); break; 
    case RTN_PROHIBIT:	  printf("P"); break; 
    case RTN_THROW:	  printf("T"); break; 
    case RTN_NAT:	  printf("N"); break; 
    case RTN_XRESOLVE:	  printf("X"); break; 
    default:
      printf(" Type=***UNKNOWN CONSTANT*** ");
    };
}

void show_route_scope_short(unsigned char t)
{
  switch(t)
    {
    case RT_SCOPE_UNIVERSE: printf("U"); break; 
    case RT_SCOPE_SITE:	    printf("S"); break; 
    case RT_SCOPE_LINK:	    printf("L"); break; 
    case RT_SCOPE_HOST:	    printf("H"); break; 
    case RT_SCOPE_NOWHERE:  printf("N"); break; 
    default:
      printf(" Scope=***UNKNOWN CONSTANT*** ");
    }

}

void show_route_protocol_short(unsigned char t)
{
  switch(t)
    {
    case RTPROT_UNSPEC:   printf(" "); break; 
    case RTPROT_REDIRECT: printf("R"); break; 
    case RTPROT_KERNEL:	  printf("K"); break; 
    case RTPROT_BOOT:	  printf("B"); break; 
    case RTPROT_STATIC:	  printf("S"); break; 
    case RTPROT_GATED:	  printf("G"); break; 
    case RTPROT_RA:	  printf("A"); break; 
    case RTPROT_MRT:	  printf("M"); break; 
    case RTPROT_ZEBRA:	  printf("Z"); break; 
    case RTPROT_BIRD:	  printf("I"); break; 
    case RTPROT_DNROUTED: printf("o"); break; 
    case RTPROT_XORP:	  printf("X"); break; 
    case RTPROT_NTK:	  printf("N"); break; 
    case RTPROT_DHCP:	  printf("D"); break; 
      
    default:
      printf(" Protocol=***UNKNOWN CONSTANT*** ");
    }

}

void print(){ };

int count_messages(){ 
  int i = 0;
  int nlmsg_len_c=nlmsg_len;
  struct nlmsghdr * nlmsg_ptr = (struct nlmsghdr *) read_buffer;
  for(; NLMSG_OK(nlmsg_ptr, nlmsg_len_c); nlmsg_ptr = NLMSG_NEXT(nlmsg_ptr, nlmsg_len_c)) {
    i++;
  }

  return i; 
}

/**
 * Extract each route table entry and print
 */
int process_and_print()
{
  char dst_str[128];
  char gw_str[128];
  char ifc_str[128];

  printf("\n");
  printf("/--- Protocol: [B]oot, [K]ernel, [S]tatic, [R]edirect, [G]ated,\n");
  printf("|    RDISC/ND R[A], [M]erit MRT, [Z]ebra, B[I]RD, R[o]uted, [X]ORP,\n");
  printf("|    [N]etsukuku, [D]HCP\n");
  printf("|/-- Type: [U]nicast, [B]roadcast, [M]ulticast, [A]nycast,\n");
  printf("||   [L]ocal, black[h]ole, un[r]eachable, [P]rohibit, [T]hrow, [N]at,\n");
  printf("||   [X]resolve\n");
  printf("||/- Scope: [U]niverse, [S]ite, [L]ocal, [H]ost, [N]owhere\n");

  int count = count_messages();
  int i = 0;
  //  struct rtmsg * rtmsg_array = malloc(count * sizeof(struct rtmsg));
  rttable_entry * rtentry_array = malloc(count * sizeof(rttable_entry)); 

  nlmsg_ptr = (struct nlmsghdr *) read_buffer;
  for(; NLMSG_OK(nlmsg_ptr, nlmsg_len); nlmsg_ptr = NLMSG_NEXT(nlmsg_ptr, nlmsg_len)) {
    rtmsg_ptr = (struct rtmsg *) NLMSG_DATA(nlmsg_ptr);
    prepare_rttable_entry( rtmsg_ptr, &(rtentry_array[i]) );
    i++;
  }

  //
  int rtentry_type_eq(char t)
  {
    switch(t)
      {
      case RTN_UNICAST:
      case RTN_LOCAL:
      case RTN_ANYCAST:
        return -1;
      default:
        return t;
      }
  }

  int entry_comparer( const void * e1_, const void * e2_ )
  {
    rttable_entry * e1 = (rttable_entry *)e1_;
    rttable_entry * e2 = (rttable_entry *)e2_;

    int t1, t2;
    t1 = rtentry_type_eq(e1->type);
    t2 = rtentry_type_eq(e2->type);

    if (t1 < t2)
      {
        return -1;
      }

    if (t1 > t2)
      {
        return 1;
      }

    // equal
    
    if (e1->family < e2->family)
        return -1;

    if (e1->family > e2->family)
        return 1;

    
    switch (e1->family)
      {
      case AF_INET:
        //        return compare_ipv4( *e1->dest, e2->dest );
      case AF_INET6:
        return compare_ipv6( *((ipv6_u*)e1->dest), 
                             *((ipv6_u*)e2->dest) );
      }

    return 0;
  }

  qsort( rtentry_array, count, sizeof(rttable_entry), entry_comparer );

  for(i=0; i<count;i++)
    {
      show_route_type_short(rtentry_array[i].rtmsg_ptr->rtm_type);
      show_route_scope_short(rtentry_array[i].rtmsg_ptr->rtm_scope);
      show_route_protocol_short(rtentry_array[i].rtmsg_ptr->rtm_protocol);
    }

  return 0;

  //
  nlmsg_ptr = (struct nlmsghdr *) read_buffer;
  for(; NLMSG_OK(nlmsg_ptr, nlmsg_len); nlmsg_ptr = NLMSG_NEXT(nlmsg_ptr, nlmsg_len)) {

    rtmsg_ptr = (struct rtmsg *) NLMSG_DATA(nlmsg_ptr);
    show_route_type_short(rtmsg_ptr->rtm_type);
    show_route_scope_short(rtmsg_ptr->rtm_scope);
    show_route_protocol_short(rtmsg_ptr->rtm_protocol);

    printf(" ");

    bzero(dst_str, 128);
    bzero(gw_str, 128);
    bzero(ifc_str, 128);

    rtattr_ptr = (struct rtattr *) RTM_RTA(rtmsg_ptr);
    rtmsg_len = RTM_PAYLOAD(nlmsg_ptr);
    for(;RTA_OK(rtattr_ptr, rtmsg_len); rtattr_ptr = RTA_NEXT(rtattr_ptr, rtmsg_len)) {
      const char * attr_name = attr_to_name_short(rtattr_ptr->rta_type);

      switch(rtattr_ptr->rta_type) {
      case RTA_DST:
      case RTA_SRC:
      case RTA_GATEWAY:
      case RTA_PREFSRC:
        {
          char buf[128];
          inet_ntop(rtmsg_ptr->rtm_family, RTA_DATA(rtattr_ptr), buf, 128);
          printf("%s %s", attr_name, buf);
        }
        break;

      case RTA_IIF:
      case RTA_OIF:
        {
          printf("%s %d", attr_name, *((int *) RTA_DATA(rtattr_ptr)));
        }
        break;

      case RTA_TABLE:     printf("%s", attr_name), show_route_table_short(rtmsg_ptr->rtm_table); break;

      case RTA_UNSPEC:    
      case RTA_PRIORITY:  
      case RTA_METRICS:   
      case RTA_MULTIPATH: 
      case RTA_FLOW:      
      case RTA_CACHEINFO: 
      case RTA_MARK:
      case RTA_PROTOINFO: 
      case RTA_SESSION:   
      case RTA_MP_ALGO:   
        printf("%s", attr_name);
        break;

      default:
        printf("Unkown attribute");
        break;

      }
      printf(" -- ");
    }

    printf(" Mask=%d ", rtmsg_ptr->rtm_dst_len);

    if(strlen(dst_str) == 0) {
      printf("default");
    } else {
      printf("%s/%d", dst_str, rtmsg_ptr->rtm_dst_len);
    }

    if(strlen(gw_str) != 0) {
      printf(" via %s", gw_str);
    }

    if(strlen(ifc_str) != 0) {
      printf(" dev %s", ifc_str);
    }
    printf("\n");
  }
}

int main(int argc, char *argv[])
{
  // open NETLINK socket
  fd = socket(AF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
  if(fd < 0) {
    printf("Error in sock open\n");
    exit(1);
  }

  send_route_read_request(AF_INET6);
  read_route_request_results();
  process_and_print();

  send_route_read_request(AF_INET);
  read_route_request_results();
  process_and_print();

  close(fd);

  exit(0);
}
