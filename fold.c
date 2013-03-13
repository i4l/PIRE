#include <stdlib.h>
#include <stdio.h>

int main () {
  
  int *arr = malloc(8*sizeof(int));
  int *res = malloc(8*sizeof(int));
  
  int i;
  for(i = 0; i < 8; i++) {
    arr[i] = i;
  } 

  for( i = 8/2; i != 0; i /= 2) {
    int j;
    for(j=0; j < 8; j++) {
      if(j % 2 == 0)
        res[j] = arr [j] + arr[j+1]; // 0 2 4 6
      if(j % 4 == 0)
        res[j] = res[j] +  res[j+2]; // 0 4
      if(j % 8 == 0)
        res[j] = res[j] + res[j+4]; // 0
      }
  }

  for(i = 0; i < 8; i++) {
    printf("%i\n",res[i]);
  }

  return 0;

}
