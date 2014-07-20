#include <iostream>
#include <algorithm>
#include <vector>
using namespace std;

class DisjointSet {
    int *pp; 
    int sz;

public:
    DisjointSet(): pp(0), sz(0) {}

    DisjointSet(int n): pp(0), sz(0) { assign(n); }

    ~DisjointSet() { clear(); }

    void clear(){ sz = 0; delete[] pp; pp = NULL; }

    void assign(int n) { clear(); pp = new int[n]; sz = n; }

    void make_set(int x) { pp[x] = x; }

    void union_set(int x, int y) { pp[find_set(x)] = find_set(y); }

    int find_set(int x){ 
        return x == pp[x] ? x : (pp[x] = find_set(pp[x]));
    }
};

int N;
vector<pair<int, pair<int,int> > > E;
DisjointSet DS;

int main() {
    int x, y, z;
    E.clear();
    cin >> N;
    DS.assign(N);
    for (int i = 0; i < N; i++)
        DS.make_set(i);
    while(cin >> x >> y >> z) {
        --x; --y;
        E.push_back(make_pair(z, make_pair(x, y))); 
    }
    sort(E.begin(), E.end());
    int i = 0, k = N;
    for ( ;i < E.size(); i++) {
        int u = E[i].second.first, v = E[i].second.second;
        if (DS.find_set(u) != DS.find_set(v)) {
            if (k > 4) {
                k--;
                DS.union_set(u, v);
            } else {
                break;
            }
        }
    }

    cout << E[i].first << endl;
    return 0;
}

