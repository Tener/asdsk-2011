#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include "pwz.h"


int main()
{
  //signal(SIGPIPE, SIG_IGN);

       printf( "Write freely. Terminate with Ctrl-D or Ctrl-C.\n" );
       int pid;

       int sockfd;
       struct sockaddr_un servaddr; /* Struct for the server socket address. */

       sockfd = socket( AF_LOCAL, SOCK_STREAM, 0 ); /* Create the client's endpoint. */
       bzero( &servaddr, sizeof( servaddr ) ); /* Zero all fields of servaddr. */
       servaddr.sun_family = AF_LOCAL; /* Socket type is local (Unix Domain). */
       strcpy( servaddr.sun_path, CONTROL_SOCKET_PATH ); /* Define the name of this socket. */
       connect(sockfd, (struct sockaddr *) &servaddr, sizeof(servaddr)); /* Connect the client's and the server's endpoint. */


       ssize_t sz = 2048;
       char * buf = malloc(sz);
       getline(&buf, &sz, stdin);
       write(sockfd, &buf, strlen(buf));
       send(sockfd, buf, strlen(buf)+1, 0);

       // while( !feof( stdin ) ) {
       //        send = getchar();
       //        write( sockfd, &send , sizeof( char ) );
       // }

       // #define BUFSIZ 1024
       
       char buffer[BUFSIZ];
       ssize_t bytes;
       while ((bytes = read(sockfd, buffer, BUFSIZ)) > 0)
         {
           printf("foo\n");
           write(1, buffer, strlen(buffer));
           printf("%s\n", buffer);
           fflush(stdout);
         }

       close( sockfd );
       
       return 0;
}
