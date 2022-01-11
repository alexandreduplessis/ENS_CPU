#include <stdio.h>
#include <stdlib.h>

void read_file(char* filename, int addr_size, int word_size, unsigned long long int* array) {
    FILE* fp;
    fp = fopen(filename, "r");
    if (fp == NULL) {
        perror("Error while opening ROM\n");
        exit(1);
    }
    int num_chars = ((1 << addr_size) * word_size + 7) / 8;
    unsigned long long int current = 0;
    int num_read_bits = 0;
    int read_index = 0;
    for (int i = 0; i < num_chars; i++) {
        current = (current << 8) | fgetc(fp);
        num_read_bits += 8;
        while (num_read_bits >= word_size) {
            unsigned long long int mask = (num_read_bits == 64) ? 0 : (1 << num_read_bits);
            num_read_bits -= word_size;
            mask -= 1 << num_read_bits;
            array[read_index] = (current & mask) >> num_read_bits;
            read_index++;
        }
    }
}

int rom_id = 1;
int main(int argc, char** argv) {
unsigned long long int a = 0;
unsigned long long int b = 0;
unsigned long long int s = 0;
while (1) {
printf("a = ?\n"); scanf("%llu", &a);
printf("b = ?\n"); scanf("%llu", &b);
s=a&b
;s=a&b
;printf("s = %llu\n", s);
}
return 0;
}

