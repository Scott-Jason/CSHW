#include <iostream>
#include <vector>
using namespace std;

/*
  This program is written in C++, I used while loop because I wanted my code to keep executing until the low index i met the high index j. A do while loop would have been fine too.
*/


bool binarySearch(vector<int> arr, int key) {
    int i = 0;
    int j = arr.size() - 1;

    while (i <= j) {
        int middle = i + (j - i) / 2;
        if (arr[middle] == key) {
            return true;
        }else if (arr[middle] < key) {
            i = middle + 1;
        }else {
            j = middle - 1;
        }
    }
    return false; 
}

int main() {
  vector<int> test = {0,1,2,3,4,5,6,7,8,9};
  cout << binarySearch(test, 4) << endl;
  cout << binarySearch(test, 11) << endl;
  cout << binarySearch(test, -3) << endl;
  cout << binarySearch(test, 2) << endl;
  return 0;
}
