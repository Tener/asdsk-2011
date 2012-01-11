int count_messages()
{ 
  int i = 0;
  int nlmsg_len_c=nlmsg_len;
  struct nlmsghdr * nlmsg_ptr = (struct nlmsghdr *) read_buffer;
  for(; NLMSG_OK(nlmsg_ptr, nlmsg_len_c); nlmsg_ptr = NLMSG_NEXT(nlmsg_ptr, nlmsg_len_c)) {
    i++;
  }

  return i; 
}

void prepare_rttable_entry( struct rtmsg * rtmsg_ptr, rttable_entry * entry )
{
  entry->dest = NULL;
  entry->dest_mask = 0;
  entry->rtmsg_ptr = rtmsg_ptr;
  entry->type = rtmsg_ptr->rtm_type;
  entry->family = rtmsg_ptr->rtm_family;
  entry->gateway[0] = 0;

  struct rtattr * rtattr_ptr = (struct rtattr *) RTM_RTA(rtmsg_ptr);
  int rtmsg_len = RTM_PAYLOAD(nlmsg_ptr);
  entry->rtmsg_len = rtmsg_len;

  for(;RTA_OK(rtattr_ptr, rtmsg_len); rtattr_ptr = RTA_NEXT(rtattr_ptr, rtmsg_len)) {
    switch(rtattr_ptr->rta_type)
      {
      case RTA_DST:
        {
          int addr_size = rtmsg_ptr->rtm_family == AF_INET ? 4 : 16;
          entry->dest = malloc(addr_size);
          memcpy(entry->dest, RTA_DATA(rtattr_ptr), addr_size);
        }
        break;
      case RTA_GATEWAY:
        {
          inet_ntop(rtmsg_ptr->rtm_family, RTA_DATA(rtattr_ptr), entry->gateway, 128);
        }
        break;
      }
  }  
  entry->dest_mask = rtmsg_ptr->rtm_dst_len;
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

