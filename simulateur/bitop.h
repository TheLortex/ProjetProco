#include <bitset>
#include <climits>
#include <iostream>
#include <string>

using namespace std;

typedef unsigned long long int ull;

template<int taille>
ull bininput() {
  string str;
  cin >> str;
  ull n = 0;

  for(int i=0;i<str.size();i++) {
    n = 2*n + (str[i] == '1' ? 1 : 0);
  }
  return n;
}

template<int taille>
void binoutput(ull val) {
  bitset<taille> res(val);
  cout << res << endl;
}


inline ull maskbit(const int ind, const int taille) {
  return (ULLONG_MAX)^(((1ULL << taille) - 1) << ind);
}

inline ull select(ull data,  const int taille,const int ind) {
  const ull masque = ((1ULL << taille) - 1) << ind; 
  return ((data & masque) >> ind);
}

inline ull ramread(ull* ram, const unsigned char ws, ull ra) { // Taille < 64
  const char wpc = 64/ws;
  
  ull position = ra/wpc; 
  char index = ra%wpc;
  
  return (ram[position] & (((1ULL << ws) - 1) << index)) >> index;
}


inline void ramwrite(ull* ram, const unsigned char ws, bool write_enable, ull wa, ull data) {
  const char wpc = 64/ws;
  ull position = wa/wpc;
  char index = wa%wpc;
  if(write_enable) {
    ram[position] &= maskbit(index, ws);
    ram[position] |= (data << index);
  }
}
