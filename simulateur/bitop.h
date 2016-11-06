#include <bitset>
#include <climits>
#include <iostream>
#include <string>
#include <cstdint>
#include <fstream>
#include <cstring>

using namespace std;

typedef uint64_t ull;

template<int taille>
ull bininput() {
  string str;
  cin >> str;
  ull n = 0;



  for(int i=0;i<min(taille,(int)str.size());i++) {
    n += (str[i] == '1' ? 1 : 0)*(1ULL << i);
  }

  if ((int) str.size() >= taille)
    return n;
  else
    return n << (taille - (int)str.size());
}

template<int taille>
void binoutput(ull val) {
  bitset<taille> res(val);
  for(int i=taille-1; i>=0;i--) {
    cout << res[i];
  }
  cout << endl;
}

ull* readromfile(string addr) {
  ifstream entree(addr, fstream::binary);
  entree.seekg (0, entree.end);
  int size = entree.tellg();
  entree.seekg (0, entree.beg);

  if (size <= 0)
    return 0;

    cout << size;

  char* buf = new char[size];
  ull* buffout = new ull[size/8];

  entree.read(buf, size);
  entree.close();

  memcpy (buffout, buf, size);

  return buffout;
}


inline ull maskbit(const int ind, const int taille) {
  return (ULLONG_MAX)^(((1ULL << taille) - 1) << ind);
}

inline ull select(ull data,  const int taille,const int ind) {
  const ull masque = ((1ULL << taille) - 1) << ind;
  return ((data & masque) >> ind);
}

inline ull read(ull* ram, const unsigned char ws, ull ra) { // Taille < 64
  const char wpc = 64/ws;

  ull position = ra/wpc;
  char index = ws*(ra%wpc);

  return (ram[position] & (((1ULL << ws) - 1) << index)) >> index;
}


inline void ramwrite(ull* ram, const unsigned char ws, bool write_enable, ull wa, ull data) {
  const char wpc = 64/ws;
  ull position = wa/wpc;
  char index = ws* (wa%wpc);
  if(write_enable) {
    ram[position] &= maskbit(index, ws);
    ram[position] |= (data << index);
  }
}
