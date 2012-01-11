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

#include <boost/asio.hpp>
#include <algorithm>
#include <vector>
#include <iostream>

// using namespace std;
// #include <boost/asio/ip/address.hpp>

#include "types.h"
#include "helpers.h"



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

/*
 helpers
*/

int count;
rttable_entry * rtentry_array = NULL;


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

/**
 * Extract each route table entry and print
 */

class Entry
{
public:
  boost::asio::ip::address addr;
  int indent;
  rttable_entry inner;
  int mask;

  Entry(){ indent = 0; mask = 128; }

  bool subsumes( Entry other )
  {
    if ( addr.is_v6() != other.addr.is_v6() )
      {
        // jeden adres jest v6, drugi v4
        // zwracamy zawsze false
        return false;
      }

    if ( addr.is_v6() )
      {
        boost::asio::ip::address_v6::bytes_type a1 = addr.to_v6().to_bytes();
        boost::asio::ip::address_v6::bytes_type a2 = other.addr.to_v6().to_bytes();
        int mask_left = mask;
        for(unsigned int i = 0; i < a1.size(); i++)
          {
            int tmp = (1 << std::max(0,std::min(mask_left,8)))-1;
            unsigned char apply_mask = tmp;
            mask_left -= 8;
            a1[i] &= apply_mask;
            a2[i] &= apply_mask;
          }
        bool val = boost::asio::ip::address_v6(a1) == boost::asio::ip::address_v6(a2);
        return val;
      }
    else
      {
        boost::asio::ip::address_v4::bytes_type a1 = addr.to_v4().to_bytes();
        boost::asio::ip::address_v4::bytes_type a2 = other.addr.to_v4().to_bytes();
        int mask_left = mask;
        for(unsigned int i = 0; i < a1.size(); i++)
          {
            int tmp = (1 << std::max(0,std::min(mask_left,8)))-1;
            unsigned char apply_mask = tmp;
            mask_left -= 8;
            a1[i] &= apply_mask;
            a2[i] &= apply_mask;
          }
        bool val = boost::asio::ip::address_v4(a1) == boost::asio::ip::address_v4(a2);
        return val;
      }

    return false;
  }

  void print()
  {
        show_route_protocol_short(inner.rtmsg_ptr->rtm_protocol);
        show_route_type_short(inner.rtmsg_ptr->rtm_type);
        show_route_scope_short(inner.rtmsg_ptr->rtm_scope);

        for(int i=0; i<indent;i++) printf(" ");
        printf(" %s/%d%s%s ===> ", addr.to_string().c_str(), inner.dest_mask, 
               strlen(inner.gateway) ? " via " : "", inner.gateway);

        rtattr_ptr = (struct rtattr *) RTM_RTA(inner.rtmsg_ptr);
        rtmsg_len = inner.rtmsg_len;

        for(; RTA_OK(rtattr_ptr, rtmsg_len); rtattr_ptr = RTA_NEXT(rtattr_ptr, rtmsg_len)) {
            const char * attr_name = attr_to_name_short(rtattr_ptr->rta_type);

            switch(rtattr_ptr->rta_type) {
              // te parametry już przetworzyliśmy
            case RTA_DST:
            case RTA_GATEWAY:
              break;

            case RTA_SRC:
            case RTA_PREFSRC:
            {
                char buf[128];
                inet_ntop(rtmsg_ptr->rtm_family, RTA_DATA(rtattr_ptr), buf, 128);
                printf("%s %s ", attr_name, buf);
            }
            break;

            case RTA_IIF:
            case RTA_OIF:
            {
                printf("%s %d ", attr_name, *((int *) RTA_DATA(rtattr_ptr)));
            }
            break;

            case RTA_TABLE:
              printf("%s", attr_name);
              show_route_table_short((enum rt_class_t)rtmsg_ptr->rtm_table);
              printf(" ");
                break;

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
                printf("%s ", attr_name);
                break;

            default:
                printf("Unkown attribute ");
                break;

            }
        }

        printf("\n");
  }
};

bool Entry_oper(const Entry & a, const Entry & b)
{
  return a.addr < b.addr;
}

int process_and_print()
{
    std::vector<Entry> ev;

    unsigned int nlmsg_len_c = nlmsg_len;

    count = count_messages();
    int i = 0;
    rtentry_array = (rttable_entry *)malloc(count * sizeof(rttable_entry));

    // preparujemy wiadomości
    nlmsg_ptr = (struct nlmsghdr *) read_buffer;
    for(; NLMSG_OK(nlmsg_ptr, nlmsg_len_c); nlmsg_ptr = NLMSG_NEXT(nlmsg_ptr, nlmsg_len_c)) {
        Entry e;
        rtmsg_ptr = (struct rtmsg *) NLMSG_DATA(nlmsg_ptr);
        prepare_rttable_entry( rtmsg_ptr, &(rtentry_array[i]) );

        e.inner = rtentry_array[i];
        if (e.inner.dest)
          {
            char buf[128];
            inet_ntop(e.inner.family, e.inner.dest, buf, 128);
            e.addr = boost::asio::ip::address::from_string(buf);
          }
        ev.push_back(e);

        i++;
    }

    // sortujemy
    sort(ev.begin(), ev.end(), Entry_oper);

    // dodajemy wcięcia
    for(unsigned int i=0; i<ev.size(); i++)
      {
        int max_indent = 0;
        
        for(unsigned int j=0; j<i; j++)
          {
            if ((ev[j].subsumes(ev[i]))) // && (ev[j] != ev[i]))
              {
#define max(a,b) a > b ? a : b
                max_indent = max(max_indent, ev[j].indent+1);
#undef max 
              }
          }
        ev[i].indent = max_indent;
      }
    
    // wypisujemy
    for(unsigned int i=0; i<ev.size(); i++)
      {
        ev[i].print();
      }

    return 0;
}

int main(int argc, char *argv[])
{
    printf("/--- Protocol: [B]oot, [K]ernel, [S]tatic, [R]edirect, [G]ated,\n");
    printf("|    RDISC/ND R[A], [M]erit MRT, [Z]ebra, B[I]RD, R[o]uted, [X]ORP,\n");
    printf("|    [N]etsukuku, [D]HCP\n");
    printf("|/-- Type: [U]nicast, [B]roadcast, [M]ulticast, [A]nycast,\n");
    printf("||   [L]ocal, black[h]ole, un[r]eachable, [P]rohibit, [T]hrow, [N]at,\n");
    printf("||   [X]resolve\n");
    printf("||/- Scope: [U]niverse, [S]ite, [L]ocal, [H]ost, [N]owhere\n");

    // otwieramy gniazdo NETLINK
    fd = socket(AF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
    if(fd < 0) {
        printf("Error in sock open\n");
        exit(1);
    }

    // najpierw IPv6
    send_route_read_request(AF_INET6);
    read_route_request_results();
    process_and_print();

    // potem IPv4
    send_route_read_request(AF_INET);
    read_route_request_results();
    process_and_print();

    close(fd);

    exit(0);
}
