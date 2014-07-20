#include <cstdio>
#include <iostream>
#include <algorithm>
#include <vector>
#include <unordered_map>
using namespace std;

const int MAXN = 200000;
int pa[MAXN];

int N, B;
unordered_map<int,int> id;

void init() {
    for (int i = 0; i < N; i++)
        pa[i] = i;
}

int getpa(int x) {
    return x == pa[x] ? x : (pa[x] = getpa(pa[x]));
}

void join(int x, int y) {
    pa[getpa(x)] = getpa(y);
}

int check(int x, int y) {
    if (id.find(y) != id.end()) {
        int px = getpa(id[x]), py = getpa(id[y]);
        if (px != py) {
            join(px, py);
            return 1;
        }
    }
    return 0;
}

int merge(int x) {
    int s = 0;
    for (int i = 0; i < B; i++) {
        s += check(x, x^(1<<i));
    }
    for (int i = 0; i < B; i++) {
        for (int j = i+1; j < B; j++) {
            s += check(x, x^(1<<i)^(1<<j));
        }
    }
    return s;
}

int main() {
    cin >> N >> B;
    int bb;
    int k = N;
    init();
    id.clear();
    for (int i = 0; i < N; i++) {
        int x = 0;
        for (int j = 0; j < B; j++) {
            scanf("%d", &bb);
            x |= bb<<j;
        }
        if (id.find(x) != id.end()) {
            k--;
        } else {
            id[x] = i;
            k -= merge(x);
        }
    }
    cout << k << endl;
    return 0;
}

