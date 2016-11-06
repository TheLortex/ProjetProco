#include <bitset>
#include <climits>
#include <iostream>
#include <string>
#include <cstdint>
#include <fstream>
#include <cstring>

using namespace std;


/*
  L'ensemble des données est stocké dans des entiers 64 bits non signés.
*/
typedef uint64_t ull;

/*
bininput: renvoie un entier 64 bits dont le i-ème bit de poids faible correspond au i-ème bit dans la chaîne d'entrée.
*/
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


/*
binoutput(val): affiche l'entier val sous forme binaire dans la même convention que pour bininput.
*/
template<int taille>
void binoutput(ull val) {
  bitset<taille> res(val);
  for(int i=0; i<taille;i++) {
    cout << res[i];
  }
  cout << endl;
}

/*
  Place le contenu d'un fichier donné dans la ROM qui servira à l'exécution du programme.
*/
ull* readromfile(string addr) {
  ifstream entree(addr, fstream::binary);
  entree.seekg (0, entree.end);
  int size = entree.tellg();
  entree.seekg (0, entree.beg);

  if (size <= 0)
    return 0;

  char* buf = new char[size];
  ull* buffout = new ull[size/8];

  entree.read(buf, size);
  entree.close();

  memcpy (buffout, buf, size);

  return buffout;
}

/*
  maskbit(ind, taille) crée l'entier binaire suivant:
  1111111111110000000000001111111111
  ^           ^          ^         ^
  |           |          |         |
  63      ind+taille    ind        0
*/
inline ull maskbit(const int ind, const int taille) {
  return (ULLONG_MAX)^(((1ULL << taille) - 1) << ind);
}

/*
  select(data, taille, ind) sélectionne dans l'entier 'data' 'taille' bits à partir de l'indice 'ind'
*/
inline ull select(ull data,  const int taille,const int ind) {
  const ull masque = ((1ULL << taille) - 1) << ind;
  return ((data & masque) >> ind);
}

/*
  read(ram, ws, ra) lit dans la ram (ou la rom) le mot stocké à l'adresse 'ra'.
  On suppose que les mots sont de taille inférieure à 64 bits.
*/
inline ull read(ull* ram, const unsigned char ws, ull ra) {
  const char wpc = 64/ws;

  ull position = ra/wpc;
  char index = ws*(ra%wpc);

  return (ram[position] & (((1ULL << ws) - 1) << index)) >> index;
}

/*
  ramwrite(ram, ws, write_enable, wa, data) écrit le mot 'data' si 'write_enable' est vrai dans le tableau 'ram' à l'adresse 'wa'
*/
inline void ramwrite(ull* ram, const unsigned char ws, bool write_enable, ull wa, ull data) {
  const char wpc = 64/ws;
  ull position = wa/wpc;
  char index = ws* (wa%wpc);
  if(write_enable) {
    ram[position] &= maskbit(index, ws);
    ram[position] |= (data << index);
  }
}
