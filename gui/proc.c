#include <stdio.h>
#include <unistd.h>

int main() {
   // printf() displays the string inside quotation
   int i = 0;
   while(1){
     fflush(stdout);
     printf("%d", i);
     i++;
     sleep(1);
   }
   return 0;
}
