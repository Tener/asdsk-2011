#include <ctype.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>
     
// #include <json/json.h>
#include "pwz.h"

/*

  pwz // control program options

  -l /path/to/control/socket
  -i             - informacje
  -a FOO         - dodaj FOO  
  -d FOO         - usuń FOO
  -q             - zakończ demona

 */

void udp_daemon(daemon_opts opts);

void print_help()
{
  printf(
"USAGE: pwzd OPTIONS                          \n" 
"                                             \n"
"  -c /path/to/config/file                    \n" 
"  -l /path/to/control/socket                 \n" 
"  -v             - verbose, default is quiet \n" 
"  -q             - quiet (default)           \n" 
"  -i 30          - hello interval (see spec.)\n" 
"  -d 6           - die time (in mult. of hello interval)\n" 
"  -p 6969        - port used                 \n" 
"  -m 224.0.0.213 - multicast address         \n" 
"  -h             - print help (this)         \n" 
"                                             \n" );

}
// int main_(int argc, char ** argv)
// {
//   client_opts opts;
//   opts.control_socket_path = CONTROL_SOCKET_PATH;
//   opts.add_del_arg = NULL;
//   opts.cmd = FULLINFO;
//   
//   return 0;
// }

int main(int argc, char ** argv)
{
  daemon_opts opts;
  opts.control_socket_path = CONTROL_SOCKET_PATH;
  opts.verbose = 0;
  opts.port = MULTICAST_PORT;
  opts.multicast_addr = MULTICAST_ADDR;
  opts.config_file = NULL;
  opts.multicast_loop = 1;
  opts.hello_interval = 6;
  opts.die_time = 5;
  
  //opterr = 0;
  int c;
  while ((c = getopt (argc, argv, "i:d:ho:vql:p:m:c:")) != -1)
    {
      switch (c)
        {
        case 'h':
          {
            print_help();
            exit(0);
          }


        case 'o': opts.multicast_loop = atoi(optarg); break;
        case 'v': opts.verbose = 1; break; 
        case 'q': opts.verbose = 0; break; 
        case 'l': opts.control_socket_path = optarg; break; 
        case 'd': opts.die_time = atoi(optarg); break;
        case 'i': opts.hello_interval = atoi(optarg); break;
        case 'p': 
          {
            char * endptr;
            opts.port = strtol(optarg, &endptr, 10);
            if (!(optarg[0] && !endptr[0]))
              {
                fprintf(stderr, "Invalid port number: '%s'\n", optarg);
                exit(2);
              }
            break; 
          }
        case 'm': 
          {
            struct in_addr inp;
            if (!(inet_aton(optarg, &inp)))
              {
                fprintf(stderr, "Invalid multicast address: '%s'\n", optarg);
                exit(2);
              }
            else
              {
                opts.multicast_addr = optarg;
              }
            break; 
          }
        case 'c': opts.config_file = optarg; break; 
        case '?': 
          exit(1);
          break;
        }
    }

  // dump flags
  if (opts.verbose)
    {
      printf("Running daemon with following options:\n");
      printf("    config file path: %s\n", opts.config_file);
      printf("    control socket path: %s\n", opts.control_socket_path);
      printf("    verbose: %s\n", opts.verbose ? "true" : "false");
      printf("    port: %d\n", opts.port);
      printf("    multicast address: %s\n", opts.multicast_addr);
      printf("    multicast loop: %s\n", opts.multicast_loop ? "true" : "false");
    }

  udp_daemon(opts);

  return 0;
}

void udp_daemon(daemon_opts opts)
{
   daemon_data data;
   data.my_interests = g_hash_table_new(NULL,g_str_equal);
   data.other_interests = g_hash_table_new(NULL,g_str_equal); // Key = *char, Value = *host_info

   /* 
      Functions operating on interests.
    */


   void del_interest(GHashTable * ints, char * in)
   {
     printf("Deleting interest '%s'\n", in);
     g_hash_table_remove(ints, in);
   }

   void add_interest(GHashTable * ints, char * in)
   {
     // to make sure there are no duplicates
     del_interest(ints, in);
     printf("Adding interest '%s'\n", in);
     g_hash_table_insert(ints, in, in);
   }

   ////

   

   int can_continue = 1;

   int sock, cli_sock;
   int status;

   char buffer[MAXBUFSIZE];
   struct sockaddr_in saddr;
   struct ip_mreq imreq;

   // UDP socket initialization
   {
     // set content of struct saddr and imreq to zero
     memset(&saddr, 0, sizeof(struct sockaddr_in));
     memset(&imreq, 0, sizeof(struct ip_mreq));
     
     // open a UDP socket
     sock = socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);
     if ( sock < 0 )
       perror("Error creating socket"), exit(1);
     
     saddr.sin_family = PF_INET;
     saddr.sin_port = htons(opts.port); // listen on port
     saddr.sin_addr.s_addr = htonl(INADDR_ANY); // bind socket to any interface
     status = bind(sock, (struct sockaddr *)&saddr, sizeof(struct sockaddr_in));
     
     if ( status < 0 )
       perror("Error binding socket to interface"), exit(1);
     
     imreq.imr_multiaddr.s_addr = inet_addr(opts.multicast_addr);
     imreq.imr_interface.s_addr = INADDR_ANY; // use DEFAULT interface
     
     // JOIN multicast group on default interface
     status = setsockopt(sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, 
                         (const void *)&imreq, sizeof(struct ip_mreq));
   }

   // CLI socket initialization
   {
     /* Remove old socket if it's still there */
     unlink(opts.control_socket_path);

     /* configure socket parameters */
     struct sockaddr_un servaddr; 
     cli_sock = socket( AF_LOCAL, SOCK_STREAM, 0 ); /* Create the server's endpoint */
     bzero( &servaddr, sizeof( servaddr ) ); /* Zero all fields of servaddr. */
     servaddr.sun_family = AF_LOCAL; /* Socket type is local (Unix Domain). */
     strcpy( servaddr.sun_path,opts.control_socket_path); /* Define the name of this socket. */

     /* create the file for socket and register it as a socket */
     bind(cli_sock, (struct sockaddr*) &servaddr, sizeof(servaddr));

     const int LISTENQ = 20;
     listen(cli_sock, LISTENQ);
   }
     
   // set IP_MULTICAST_LOOP
   if (opts.multicast_loop)
   {
     unsigned char one = 1;
     setsockopt(sock, IPPROTO_IP, IP_MULTICAST_LOOP,
                &one, sizeof(unsigned char));
   }

   

   // timer
   struct timeval tv;
   void reset_timeout()
   {
     tv.tv_sec = opts.hello_interval;
     tv.tv_usec = 0;
   }
   reset_timeout();


   void do_receive()
   {
     printf("Received packet.\n");
     socklen_t socklen = sizeof(struct sockaddr_in);
     // receive packet from socket
     ssize_t payload_len = recvfrom(sock, buffer, MAXBUFSIZE, 0, 
                                    (struct sockaddr *)&saddr, &socklen);
     char addrbuf[INET_ADDRSTRLEN];
     inet_ntop(AF_INET, &(saddr.sin_addr), addrbuf, INET_ADDRSTRLEN);
     printf("SENDER: %s\n"
            "PAYLOAD: '%s'\n", addrbuf, /* saddr.sin_addr.s_addr, */ buffer);
   }

   // serve cli client in blocking fashion.
   void do_cli()
   {
     struct sockaddr_un cliaddr;
     socklen_t clilen;

     static char * buffer = NULL;
     static size_t bufflen = 1024;
     if (!buffer)
       buffer = malloc(bufflen);

     // helper functions

     void send_current()
     {
       fprintf(connfile_w, "Current set of interests:\n");
       printf("Current set of interests:\n");
       void aux(void * key, void * value, void * user_data)
       {
         fprintf(connfile_w, "   %s\n", (char*)key);
         printf("   %s\n", (char*)key);
       }

       g_hash_table_foreach(data.my_interests, aux, NULL);
     }

     void show_interest_info(char * in)
     {
       GList * interested = NULL;
       void one_host(void * key, void * value, void * user_data)
       {
         char * host = (char*) key;
         host_info * hi = (host_info*) value;
         if (g_hash_table_lookup_extended(hi->interests, in, NULL, NULL))
           {
             gint mycmp(gconstpointer a, gconstpointer b){ return g_strcmp0(a,b); }
             interested = g_list_insert_sorted(interested, host, mycmp);
           }
       }

       g_hash_table_foreach(data.other_interests, 
                            one_host,
                            NULL);
       
       
       void print_one(void * data, void * userdata)
       {
         fprintf(connfile_w, "%s\n", (char*)data);
       }
       
       g_list_foreach(interested, print_one, NULL);
       g_list_free(interested);
     }
     
     void show_full_interest_info()
     {
       time_t now = time(NULL);

       void one_host(void * key, void * value, void * user_data)
       {
         char * host = (char*) key;
         host_info * hi = (host_info*) value;

         time_t diff = now-(hi->last_seen);

         fprintf(connfile_w, "%s  %d:%d\n", host, diff / 60, diff % 60);
         
         void one_interest(void * key, void * value, void * user_data)
         {
           fprintf(connfile_w, "   %s\n", (char*)key);
         }

         g_hash_table_foreach(hi->interests, one_interest, NULL);         
       }

       g_hash_table_foreach(data.other_interests, 
                            one_host,
                            NULL);
     }

     // do stuff

     printf("Received control command.\n");
     int connfd = accept(cli_sock, ( struct sockaddr * ) &cliaddr, &clilen );
     FILE * connfile_r = fdopen(connfd, "r");
     FILE * connfile_w = fdopen(dup(connfd), "w");
     
     ssize_t count = getline(&buffer, &bufflen, connfile_r);
     if (count < 1)
       {
         printf("CLI error.\n");
         return;
       }

     switch (buffer[0])
       {
       case 'Q':
       case 'q': printf("Received quit command. Bye, bye...\n"); can_continue = 0; break;
       case 'a': printf("Received add command.\n"); add_interest(data.my_interests, buffer+1); send_current(); break;
       case 'd': printf("Received del command.\n"); del_interest(data.my_interests, buffer+1); send_current(); break;
       case 'i': printf("Received info command.\n"); show_interest_info(buffer+1); break;
       case 'I':
       case 'f': printf("Received full info command.\n"); show_full_interest_info(); break;
       default:
         printf("No command. What? %s\n", buffer);
         break;
       }
     
//     fflush(connfile_w);
//     shutdown(fileno(connfile_r), SHUT_RDWR);
     fclose(connfile_w);
     fclose(connfile_r);
     //    close(connfd); /* close connected socket */         
   }
   
   void do_send()
   {
     printf("Sending packet.\n");

     struct sockaddr_in saddr;
     socklen_t socklen = sizeof(struct sockaddr_in);

     // set destination multicast address
     saddr.sin_family = PF_INET;
     saddr.sin_addr.s_addr = inet_addr(opts.multicast_addr);
     saddr.sin_port = htons(opts.port);
     
     char buffer[128];
     // put some data in buffer
     strcpy(buffer, "Hello world from server\n");

     // send packet
     sendto(sock, buffer, strlen(buffer), 0,
            (struct sockaddr *)&saddr, socklen);

   }

   void cleanup()
   {
     // shutdown socket
     shutdown(sock, SHUT_RDWR);
     // close udp socket
     close(sock);
     // close cli socket
     shutdown(cli_sock, SHUT_RDWR);
     close(cli_sock);
   }

   atexit(cleanup);

   while(can_continue)
     {
       fd_set readfs;
       FD_ZERO(&readfs);
       FD_SET(sock, &readfs);
       FD_SET(cli_sock, &readfs);

       select(cli_sock+1, &readfs, NULL, NULL, &tv);

       int cont = 0;

       if (FD_ISSET(sock, &readfs))
         {
           do_receive();
           cont = 1;
         }

       if (FD_ISSET(cli_sock, &readfs))
         {
           do_cli();
           cont = 1;
         }

       if (!cont)
         {
           reset_timeout();
           do_send();
         }
     }
   
}

