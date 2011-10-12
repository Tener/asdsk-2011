#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <assert.h>
#include <ctype.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

#include "pwz.h"

void print_help()
{
  printf(
"  USAGE:                           \n"
"  pwz // control program options   \n" 
"                                   \n" 
"  -l /path/to/control/socket       \n" 
"  -i             - informacje      \n" 
"  -a FOO         - dodaj FOO       \n" 
"  -d FOO         - usuń FOO        \n" 
"  -q             - zakończ demona  \n" 
         );
}

int main(int argc, char ** argv)
{
  client_opts opts;
  opts.control_socket_path = CONTROL_SOCKET_PATH;
  opts.command_arg = NULL;
  opts.cmd = FULLINFO;

  int c;
  while ((c = getopt (argc, argv, "hl:a:d:i:q")) != -1)
    {
      switch (c)
        {
        case 'h':
          {
            print_help();
            exit(0);
          }
        case 'l': opts.control_socket_path = optarg; break; 
        case 'i': opts.cmd = INFO; opts.command_arg = optarg; break; 
        case 'a': opts.cmd = ADD;  opts.command_arg = optarg; break; 
        case 'd': opts.cmd = DEL;  opts.command_arg = optarg; break; 
        case 'q': opts.cmd = QUIT; break;
        case '?': 
        default:
          printf("\n");
          print_help();
          exit(1);
          break;
        }
    }

  //
  int sockfd;
  struct sockaddr_un servaddr; /* Struct for the server socket address. */
  
  sockfd = socket( AF_LOCAL, SOCK_STREAM, 0 ); /* Create the client's endpoint. */
  bzero( &servaddr, sizeof( servaddr ) ); /* Zero all fields of servaddr. */
  servaddr.sun_family = AF_LOCAL; /* Socket type is local (Unix Domain). */
  strcpy( servaddr.sun_path, CONTROL_SOCKET_PATH ); /* Define the name of this socket. */
  connect(sockfd, (struct sockaddr *) &servaddr, sizeof(servaddr)); /* Connect the client's and the server's endpoint. */


  void sends(char * arg)
  {
    assert(arg != NULL);
    send(sockfd, arg, strlen(arg), 0);
  }

  void read_till_end()
  {
       char buffer[BUFSIZ];
       ssize_t bytes;
       while ((bytes = read(sockfd, buffer, BUFSIZ)) > 0)
         {
           write(1, buffer, strlen(buffer));
           fflush(stdout);
         }
       close(sockfd);
  }

  switch(opts.cmd)
    {
    case FULLINFO: 
      sends("f"); 
      sends("\n");
      break;
    case INFO: 
      sends("i"); 
      sends(opts.command_arg); 
      sends("\n");
      break;
    case ADD:
      sends("a"); 
      sends(opts.command_arg); 
      sends("\n");
      break;
    case DEL:
      sends("d"); 
      sends(opts.command_arg); 
      sends("\n");
      break;
    case QUIT:
      sends("q"); 
      sends("\n");
      return 0;
    }

  read_till_end();

  return 0;
}


int main_()
{
   int sock, status, socklen;
   char buffer[MAXBUFSIZE];
   struct sockaddr_in saddr;
   struct in_addr iaddr;
   unsigned char ttl = 3;
   unsigned char one = 1;

   // set content of struct saddr and imreq to zero
   memset(&saddr, 0, sizeof(struct sockaddr_in));
   memset(&iaddr, 0, sizeof(struct in_addr));

   // open a UDP socket
   sock = socket(PF_INET, SOCK_DGRAM, 0);
   if ( sock < 0 )
     perror("Error creating socket"), exit(1);

   saddr.sin_family = PF_INET;
   saddr.sin_port = htons(0); // Use the first free port
   saddr.sin_addr.s_addr = htonl(INADDR_ANY); // bind socket to any interface
   status = bind(sock, (struct sockaddr *)&saddr, sizeof(struct sockaddr_in));

   if ( status < 0 )
     perror("Error binding socket to interface"), exit(1);

   iaddr.s_addr = INADDR_ANY; // use DEFAULT interface

   // Set the outgoing interface to DEFAULT
   setsockopt(sock, IPPROTO_IP, IP_MULTICAST_IF, &iaddr,
	      sizeof(struct in_addr));

   // Set multicast packet TTL to 3; default TTL is 1
   setsockopt(sock, IPPROTO_IP, IP_MULTICAST_TTL, &ttl,
	      sizeof(unsigned char));

   // send multicast traffic to myself too
   status = setsockopt(sock, IPPROTO_IP, IP_MULTICAST_LOOP,
		       &one, sizeof(unsigned char));

   // set destination multicast address
   saddr.sin_family = PF_INET;
   saddr.sin_addr.s_addr = inet_addr(MULTICAST_ADDR);
   saddr.sin_port = htons(MULTICAST_PORT);

   // put some data in buffer
   strcpy(buffer, "Hello world from client\n");

   socklen = sizeof(struct sockaddr_in);
   // receive packet from socket
   status = sendto(sock, buffer, strlen(buffer), 0,
		     (struct sockaddr *)&saddr, socklen);

   // shutdown socket
   shutdown(sock, SHUT_RDWR);
   // close socket
   close(sock);

   return 0;
}
