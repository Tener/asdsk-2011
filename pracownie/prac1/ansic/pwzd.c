// Author: Krzysztof Skrzętnicki
// LICENSE: MIT

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdint.h>
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

/*

  pwz // control program options

  -l /path/to/control/socket
  -i             - informacje
  -a FOO         - dodaj FOO
  -d FOO         - usuń FOO
  -q             - zakończ demona

 */

int udp_daemon(daemon_opts opts);

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
  opts.hello_interval = 30;
  opts.die_time = 6;

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

  return udp_daemon(opts);
}

int udp_daemon(daemon_opts opts)
{
   void destroy_host_info(host_info * hi)
   {
     if (!hi)
       return;
     if (hi->interests)
       g_hash_table_destroy(hi->interests);
     free(hi);
   }

   void destroy_host_info_(gpointer data)
   {
     destroy_host_info(data);
   }

   daemon_data data;
   data.my_interests = g_hash_table_new_full(g_str_hash,g_str_equal,free,NULL);
   data.other_interests = g_hash_table_new_full(g_str_hash,g_str_equal,free,destroy_host_info_); // Key = *char, Value = *host_info

   /*
      Functions operating on interests.
    */

   void del_interest(GHashTable * ints, char * in)
   {
     if (opts.verbose) printf("Deleting interest '%s'\n", in);
     g_hash_table_remove(ints, in);
   }

   void add_interest(GHashTable * ints, char * in)
   {
     char * keyvalue = strdup(in);
     if (opts.verbose) printf("Adding interest %p='%s'\n", keyvalue, keyvalue);
     g_hash_table_replace(ints, keyvalue, keyvalue);
   }

   void remove_outdated_entries()
   {
     time_t now = time(NULL);
     gboolean aux(void * key, void * value, void * user_data)
     {
       char * host = key;
       host_info * hi = value;
       int remove = ((now - hi->last_seen) > (opts.hello_interval * opts.die_time));

       if (remove && opts.verbose)
         {
           printf("Remove outdated entry for host %s.\n", host);
         }

       return remove;
     }
     g_hash_table_foreach_remove(data.other_interests, aux, NULL);
   }

   /*
      Fill my interest table
   */

   if (opts.config_file)
   {
     printf("Loading config file.\n");

     FILE * f = fopen(opts.config_file,"r");
     if (!f)
       {
         fprintf(stderr, "Could not open config file: %s\n", opts.config_file);
         return 1;
       }

     GString * config = g_string_new(NULL);
     while(!feof(f))
       {
         char c = fgetc(f);
         if (c == EOF)
           break;

         config = g_string_append_c(config, c);
         if (!c)
           {
             add_interest(data.my_interests, config->str);
             config = g_string_erase(config, 0, -1);
           }
       }
     fclose(f);

     // trailing item
     if (strlen(config->str))
       {
             add_interest(data.my_interests, config->str);
       }
     g_string_free(config, TRUE);
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
     if (opts.verbose) printf("Received packet.\n");
     socklen_t socklen = sizeof(struct sockaddr_in);
     // receive packet from socket
     ssize_t payload_len = recvfrom(sock, buffer, MAXBUFSIZE, 0,
                                    (struct sockaddr *)&saddr, &socklen);
     char addrbuf[INET_ADDRSTRLEN];
     inet_ntop(AF_INET, &(saddr.sin_addr), addrbuf, INET_ADDRSTRLEN);

     if (opts.verbose)
       {
         printf("SENDER: %s\n"
                "PAYLOAD: '%s'\n",
                addrbuf, buffer);
       }

     if (payload_len < 4)
       {
         if (opts.verbose) printf("Not even 4 bytes received.\n");
         return;
       }

     uint32_t declared_count = ntohl(*((uint32_t *) buffer));
     uint32_t actual_count = 0;

     host_info hi;
     hi.last_seen = time(NULL);
     hi.interests = g_hash_table_new_full(g_str_hash,g_str_equal,free,NULL);

     char * ptr = buffer+4;
     ssize_t payload_left = payload_len - 4;
     char * auxbuf = malloc(payload_left);
     bzero(auxbuf, payload_left);

     void cleanup()
     {
       free(auxbuf);
     }

     while(payload_left > 0)
       {
         char * found = memchr(ptr, 0, payload_left);
         if (!found)
           { // error. packet is malformed and does not end with '\0'.
             if (opts.verbose) printf("Error: packet is malformed: does not end with NULL byte.\n");
             cleanup();
             return;
           }

         // insert found argument
         add_interest(hi.interests, ptr);
         // move pointer, update count etc.
         payload_left -= (found-ptr)+1; // == strlen(ptr)+1
         ptr = found+1; // +1, to move beyond '\0'
         actual_count++;
       }

     // check if actual_count == declared_count
     if (actual_count != declared_count)
       {
         if (opts.verbose) printf("Error: packet is malformed: actual number of interests (%d) different from declared one (%d).\n", actual_count, declared_count);
             cleanup();
             return;
       }

     // check if the interest list isn't actually empty. if yes, this is a "goodbye packet". handle it.
     if (!actual_count)
       {
         if (opts.verbose) printf("Goodbye packet received.\n");
         g_hash_table_remove(data.other_interests, addrbuf);
       }
     else // all is ok, allocate memory for new host info, copy it, insert into hash table
       {
         host_info * hi_tmp = malloc(sizeof(host_info));
         *hi_tmp = hi;
         g_hash_table_replace(data.other_interests, strdup(addrbuf), hi_tmp);         
       }

     cleanup();
   }

   // serve cli client in blocking fashion.
   void do_cli()
   {
     gint mycmp(gconstpointer a, gconstpointer b){ return g_strcmp0(a,b); }

     FILE * connfile_r = NULL;
     FILE * connfile_w = NULL;
     struct sockaddr_un cliaddr;
     socklen_t clilen;

     static char * buffer = NULL;
     static size_t bufflen = 1024;
     if (!buffer)
       buffer = malloc(bufflen);

     // helper functions

     // "Current set of interests.."
     void send_current()
     {
       GList * interested = NULL;
       fprintf(connfile_w, "Current set of interests:\n");
       void insert_sorted(void * key, void * value, void * user_data)
       {
         interested = g_list_insert_sorted(interested, (char*)key, mycmp);
       }
       g_hash_table_foreach(data.my_interests, insert_sorted, NULL);

       void print_one(void * data, void * userdata)
       {
         fprintf(connfile_w, "   %s\n", (char*)data);
       }

       g_list_foreach(interested, print_one, NULL);
       g_list_free(interested);
     }

     // info on specific topic
     void show_interest_info(char * in)
     {
       GList * interested = NULL;
       void one_host(void * key, void * value, void * user_data)
       {
         char * host = (char*) key;
         host_info * hi = (host_info*) value;
         if (g_hash_table_lookup_extended(hi->interests, in, NULL, NULL))
           {
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

     // info about all hosts
     void show_full_interest_info()
     {
       time_t now = time(NULL);

       void one_host(void * key, void * value, void * user_data)
       {
         GList * interested = NULL;
         char * host = (char*) key;
         host_info * hi = (host_info*) value;

         time_t diff = now-(hi->last_seen);

         fprintf(connfile_w, "%s  %d:%02d\n", host, (int)(diff / 60), (int)(diff % 60));

         void one_interest(void * key, void * value, void * user_data)
         {
           interested = g_list_insert_sorted(interested, key, mycmp);
         }

         void print_one(void * data, void * userdata)
         {
           fprintf(connfile_w, "   %s\n", (char*)data);
         }

         g_hash_table_foreach(hi->interests, one_interest, NULL);
         g_list_foreach(interested, print_one, NULL);
         g_list_free(interested);
       }

       g_hash_table_foreach(data.other_interests,
                            one_host,
                            NULL);
     }

     // do stuff
     if (opts.verbose) printf("Received control command.\n");
     int connfd = accept(cli_sock, ( struct sockaddr * ) &cliaddr, &clilen );
     connfile_r = fdopen(connfd, "r");
     connfile_w = fdopen(dup(connfd), "w");

     ssize_t count = getline(&buffer, &bufflen, connfile_r);
     if (count < 1)
       {
         printf("CLI error.\n");
         return;
       }

     char pick_argument_buffer()
     {
       char c0 = buffer[0];
       size_t len = strlen(buffer);
       memmove(buffer, buffer+1, len-2);
       buffer[len-2] = 0;
       return c0;
     }

     switch (pick_argument_buffer())
       {
       case 'Q':
       case 'q': if (opts.verbose) printf("Received quit command. Bye, bye...\n"); can_continue = 0; break;
       case 'a': if (opts.verbose) printf("Received add command [%s].\n", buffer); add_interest(data.my_interests, buffer); send_current(); break;
       case 'd': if (opts.verbose) printf("Received del command [%s].\n", buffer); del_interest(data.my_interests, buffer); send_current(); break;
       case 'i': if (opts.verbose) printf("Received info command [%s].\n", buffer); show_interest_info(buffer); break;
       case 'I':
       case 'f': if (opts.verbose) printf("Received full info command [%s].\n", buffer); show_full_interest_info(); break;
       default:
         if (opts.verbose) printf("No command. What? >>>%s<<<\n", buffer);
         break;
       }

     fclose(connfile_w);
     fclose(connfile_r);
   }

   void do_send()
   {
     if (opts.verbose) printf("Sending packet.\n");

     struct sockaddr_in saddr;
     socklen_t socklen = sizeof(struct sockaddr_in);

     // set destination multicast address
     saddr.sin_family = PF_INET;
     saddr.sin_addr.s_addr = inet_addr(opts.multicast_addr);
     saddr.sin_port = htons(opts.port);

     // create packet
     uint32_t count = 0;
     GString * msg = g_string_new(NULL);
     void append(void * key, void * val, void * user_data)
     {
       msg = g_string_append(msg, (char*)val);
       msg = g_string_append_c(msg, '\0');
       count++;
     }
     g_hash_table_foreach(data.my_interests, append, NULL);

     ssize_t msglen = 4 + msg->len;
     char * buffer = malloc(msglen);
     *((uint32_t *) buffer) = htonl(count);

     memcpy(buffer+4, msg->str, msg->len);

     // send packet
     sendto(sock, buffer, msglen, 0,
            (struct sockaddr *)&saddr, socklen);
     free(buffer);
     g_string_free(msg, TRUE);
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

   // at start send single information
   do_send();

   while(can_continue)
     {
       fd_set readfs;
       FD_ZERO(&readfs);
       FD_SET(sock, &readfs);
       FD_SET(cli_sock, &readfs);

       select(cli_sock+1, &readfs, NULL, NULL, &tv);

       // remove outdated entries
       remove_outdated_entries();

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

   // remove our interests
   g_hash_table_remove_all(data.my_interests);
   do_send();

   return 0;
}
