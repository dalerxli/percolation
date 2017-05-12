#include <fstream>
#include <iostream>
#include <vector>
#include <random>
#include <unordered_set>
#include <functional>
using namespace std;


random_device rnd;

#define VISITED 3
#define FREE 0
#define OCCUPIED 1


int ones = 0;
int N = 100;
void simple_fill2d(vector<vector<char>>& material, double density) {
	unsigned int max = rnd.max();
	for(auto& d1 : material) 
	for(auto& d0t : d1) {
		d0t = rnd() < (max * density) ;
		ones += d0t;
	}

}

void simple_fill3d(vector<vector<vector<char>>>& material, double density) {
	
	for(auto& d2 : material)
		simple_fill2d(d2, density);
	
} 

struct point {
	int x,y,z;
};
struct point_hash {
	size_t operator() (const point& a) const {
		return (a.x + a.y * N + a.z * N);
	}
};
struct point_equal {
	bool operator() (const point& a, const point& b) const {
		return a.x == b.x && a.y == b.y && a.z == b.z;
	}
};
inline void nexts(vector<vector<vector<char>>> &material,unordered_set<point, point_hash, point_equal>& next, const point &p) {
	vector<char> dd {-1, 1};
	
	for(int dx : dd) 
		if(p.x+dx < material[p.z][p.y].size() && p.x+dx > 0 && material[p.z][p.y][p.x+dx] == OCCUPIED) next.insert(point {p.x + dx, p.y, p.z } ); 
	for(int dy : dd) 
		if(p.y+dy < material[p.z].size()      && p.y+dy > 0 && material[p.z][p.y+dy][p.x] == OCCUPIED) next.insert(point {p.x, p.y + dy, p.z }); 	
	for(int dz : dd) 
		if(p.z+dz < material.size()           && p.z+dz > 0 && material[p.z+dz][p.y][p.x] == OCCUPIED) next.insert(point {p.x, p.y, p.z + dz } ); 
} 

bool bfs(vector<vector<vector<char>>> &material) {
	
	unordered_set<point, point_hash, point_equal> next, next2;
	//initial - nx = 0, nx,ny - all surface

	for(int y = 0;y  < material[0].size(); y++) {
		for(int x = 0; x < material[y].size(); x++) {
			next.insert(point {x,y,0});
		}
	}
	
	while(next.size() > 0) {

		//cout << next.size() << endl;
		for(const point &p: next) {
			material[p.z][p.y][p.x] = VISITED;
			if(p.z == material.size() - 1) {
				return true;
			}
		}
		for(const point &p: next) {
			nexts(material, next2, p);
			
		}
		next = std::move(next2);
		next2.clear();
	
	}
	return false;
}

int main(int argc, char** argv) {

	
	double occ = 0.2;
	int total = 100;
	
	double occ_begin = 0.28, occ_end = 0.33, occ_step = 0.01;
	if(argc > 1) {
		N = atoi(argv[1]);
	}
	if(argc > 2) {
		total = atoi(argv[2]);
	}
	if(argc > 3) {
		occ_begin = atof(argv[3]);
	}
	if(argc > 4) {
		occ_end = atof(argv[4]);
	}
	if(argc > 5) {
		occ_step = atof(argv[5]);
	}
	int Nx = N, Ny = N, Nz = N;
		
	vector<vector<vector<char>>> mat;
	mat.resize(Nz);
	for(auto& d2 : mat) {
		d2.resize(Ny);
		for(auto& d1 : d2) d1.resize(Nx);
	}
	
	ofstream fres("result.txt");
	
	for(occ = occ_begin; occ < occ_end; occ += occ_step) {
		int success = 0;
		for(int i = 0;i < total; i++) {
			
			simple_fill3d(mat,occ);
			success += bfs(mat);
			
			
		}	
		cout << int((occ - occ_begin) / (occ_end - occ_begin) * 100) <<'%' << endl;
		fres << occ << '\t' << double(success)/total << endl;
	}
	
	
	return 0;
}
