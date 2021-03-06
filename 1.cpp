#include <fstream>
#include <iostream>
#include <vector>
#include <random>
#include <unordered_set>
#include <unordered_map>
#include <functional>
#include <armadillo>
#include <Eigen/Sparse>
#include <cstdlib>
#include <cstring>
using namespace std;


random_device rnd;

#define VISITED -3
#define FREE -2
#define OCCUPIED -1


int ones = 0;
int N = 100;

pair<int,int> random_dxdy() {
	int dx = (rnd() % 3 ) - 1,
	    dy = (rnd() % 3 ) - 1;
	if( dx != 0 && dy != 0) {
		int r = rnd() % 2;
		dx *= r;
		dy *= (1-r);
	}
	return make_pair(dx, dy);
}







template<typename T> double simple_fill2d(vector<vector<T>>& material, double density) {
	unsigned int max = rnd.max();
	int _ones = 0;
	for(auto& d1 : material) 
	for(auto& d0t : d1) {
		d0t = (rnd() < (max * density)) ? OCCUPIED : FREE ;
		ones += (d0t == OCCUPIED);
		_ones += (d0t == OCCUPIED);
	}
	return density;
//	cout << _ones << ' ';
}

double _fill2d(vector<vector<int>>& material, int N, int E, bool alternative = false) {
	//if(E == 1) return simple_fill2d(material, double(N)/material.size()/material[0].size());
		
	int Y = material.size(), X = material[0].size();
	for(int y = 0; y < Y; y++) for(int x = 0; x < X; x++) material[x][y] = FREE;
	int x,y;
	int dx, dy;
	if(E == 1) {
		for(int i = 0; i < N; i++) {
			x = rnd() % X, y = rnd() % Y;
			material[x][y] = OCCUPIED;
		}
	}
	else for(int i =0 ; i < N; i++) {
		x = rnd() % X, y = rnd() % Y;
		if(alternative) {
			auto p = random_dxdy();
			for(int j = 0; j < E; j++) {
				int dx = j*p.first, dy = j*p.second;
				if(x + dx < X && x + dx >= 0 && y + dy < Y && y + dy >= 0) material[y+dy][x+dx] = OCCUPIED;
			}
		}
		else {
			do {
				auto p = random_dxdy();
				dx = p.first * (E-1), dy = p.second * (E-1);
			} while(x + dx >= X || x + dx < 0 || y + dy >= Y || y + dy < 0  );
			dx /= E-1;
			dy /= E-1;
			//cout << dx << ' ' << dy << endl;
			for(int j = 0; j < E; j++) {
				material[y + j*dy][x + j*dx] = OCCUPIED;
			}
		}
		
	}
	double real_occ = 0;
	for(const auto& d1 : material) for(auto d0 : d1 ) real_occ += (d0 == OCCUPIED);
	return real_occ / material.size() / material[0].size();
}
double fill2d(vector<vector<int>>& material, double occ, int E, bool alternative = false) {
	int N = material.size() * material[0].size() * (occ / E);
	//cout << material.size() << ' ' << material[0].size() << ' ' << occ << ' ' << E << endl;
	//cout << N << endl;
	return _fill2d(material, N, E, alternative);
}


void simple_fill3d(vector<vector<vector<char>>>& material, double density) {
	
	for(auto& d2 : material)
		simple_fill2d(d2, density);
	
} 

struct point2d { 
	int x,y;
	bool operator== (const point2d& b) const {
		return x == b.x && y == b.y;
	}
	bool operator!= (const point2d& b) const {
		return x != b.x || y != b.y;
	}
};

struct point2d_hash {
	size_t operator() (const point2d& a) const {
		return a.x + a.y * N;
	}
};

struct point2d_equal {
	bool operator() (const point2d& a, const point2d& b) const {
		return a.x == b.x && a.y == b.y;
	}

};


inline void nexts2d(vector<vector<int>> &material, unordered_set<point2d, point2d_hash, point2d_equal>& next, const point2d &p) {
	vector<char> dd{-1,1};
	
	for(int dx : dd) 
		if(p.x+dx < material[p.y].size() && p.x+dx > 0 && material[p.y][p.x+dx] == OCCUPIED) next.insert(point2d {p.x + dx, p.y } );

 	for(int dy : dd) 
		if(p.y+dy < material.size() && p.y+dy > 0 && material[p.y + dy][p.x] == OCCUPIED) next.insert(point2d {p.x , p.y + dy } ); 
}


bool bfs2d(vector<vector<int>> &material) {
	
	unordered_set<point2d, point2d_hash, point2d_equal> next, next2;
	//initial - ny = 0, nx - all surface

	
	for(int x = 0; x < material[0].size(); x++)  if(material[0][x] == OCCUPIED) next.insert(point2d {x,0});

	
	while(next.size() > 0) {

		//cout << next.size() << endl;
		for(const point2d &p: next) {
			material[p.y][p.x] = VISITED;
			if(p.y == material.size() - 1) {
				return true;
			}
		}
		for(const point2d &p: next) {
			nexts2d(material, next2, p);
			
		}
		next = std::move(next2);
		next2.clear();
	
	}
	return false;
}

char fun(char a) {
	if (a == FREE) return '.';
	if (a == OCCUPIED) return 'O';
	if (a == VISITED) return 'V';
}
void printmat(const vector<vector<int>> &mat, ostream& L) {
	for(const auto& d1: mat) {
		for(const auto& d0: d1) {
			if(d0 < 0) L << fun(d0);
			else L << int(d0);
			L << '\t';
		}
		L << endl;
	}
	L << endl << endl;
}
ofstream L("log.txt");
long long step = 0;
inline void wave_nexts2d(vector<vector<int>> &material, unordered_set<point2d, point2d_hash, point2d_equal>& next, const point2d &p) {
	vector<char> dd{-1,1};
	for(char dx : dd) 
		if(p.x+dx < material[p.y].size() && p.x+dx >= 0 && material[p.y][p.x+dx] == OCCUPIED) {
			next.insert(point2d {p.x + dx, p.y } );
		}
 	for(char dy : dd) 
		if(p.y+dy < material.size() && p.y+dy >= 0 && material[p.y + dy][p.x] == OCCUPIED) {
			next.insert(point2d {p.x , p.y + dy } ); 
		}
}

bool wave_forward2d(vector<vector<int>>& material) {
	unordered_set<point2d, point2d_hash, point2d_equal> next, next2;
	
	for(int x = 0; x < material[0].size(); x++) {
		if(material[0][x] == OCCUPIED) next.insert(point2d{x,0});
	}
	bool achieved = false;
	step = 1;	
	while(next.size() > 0) {
		for(const point2d &p: next) {
			material[p.y][p.x] = step;
			if(p.y == material.size() - 1) achieved = true;
		}
		
		for(const point2d &p : next) {
			wave_nexts2d(material, next2, p);
		}
		next = std::move(next2);
		next2.clear();
		++step;
	}
	return achieved;
}
bool has_next_lower(vector<vector<int>>& mat, const point2d &p, const point2d &from) {
	vector<int> dd {-1,1};
	for(int dx : dd) {
		if(point2d{p.x+dx,p.y} != from && p.x + dx < mat[p.y].size() && p.x + dx >= 0 && mat[p.y][p.x+dx] > 0 && mat[p.y][p.x + dx] < mat[p.y][p.x]) return true;
	}
	for(int dy : dd) {
		if(point2d{p.x, p.y + dy} != from && p.y + dy  < mat.size() && p.y + dy >= 0 && mat[p.y+dy][p.x] > 0 && mat[p.y + dy][p.x] < mat[p.y][p.x] ) return true;
	}
	return false;
}
inline void back_nexts2d(vector<vector<int>> &material, vector<vector<int>> &vis, unordered_set<point2d, point2d_hash, point2d_equal>& next, const point2d &p) {
	vector<char> dd{-1,1};
	for(char dx : dd) 
		if(p.x+dx < material[p.y].size() && p.x+dx >= 0 && vis[p.y][p.x+dx] != VISITED && material[p.y][p.x+dx] > 0 && (material[p.y][p.x+dx] < material[p.y][p.x] ||  has_next_lower(material, point2d{p.x+dx,p.y},p) )) {
			next.insert(point2d {p.x + dx, p.y } );
		}
 	for(char dy : dd) 
		if(p.y+dy < material.size() && p.y+dy >= 0 && material[p.y + dy][p.x] > 0 && vis[p.y+dy][p.x] != VISITED && (material[p.y + dy][p.x]  < material[p.y][p.x] ||  has_next_lower(material, point2d{p.x,p.y+dy},p) )) {
			next.insert(point2d {p.x , p.y + dy } ); 
		}
}


void wave_backward2d(vector<vector<int>>& material) {
	unordered_set<point2d, point2d_hash, point2d_equal> next, next2;
	int Yborder = material.size() - 1; 
	for(int x = 0; x < material.rbegin()->size(); ++x) {
		if(material[Yborder][x] > 0) next.insert(point2d { x, Yborder });
	}
	auto vis = material;
	while(next.size() > 0) {
		for(const point2d &p: next) {
			vis[p.y][p.x] = VISITED;	
		}
		
		for(const point2d &p : next) {
			back_nexts2d(material,vis, next2, p);
		}
		next = std::move(next2);
		next2.clear();
		++step;

	}
	material = vis;
}

typedef unordered_map<point2d, unordered_map<point2d, double, point2d_hash, point2d_equal>, point2d_hash, point2d_equal> hash_graph2d;
//weights - ___conductance___, not resistance!!!
void add_edge2d(hash_graph2d &graph, const point2d &a, const point2d &b, double w) {
	if(graph[a].count(b)) {
		w += graph[a][b];
	}
	graph[a][b] = graph[b][a] = w;
}
inline void wave_nexts_mkgraph2d(vector<vector<int>> &material, unordered_set<point2d, point2d_hash, point2d_equal>& next, const point2d &p, hash_graph2d &graph) {
	vector<char> dd{-1,1};
	for(char dx : dd) 
		if(p.x+dx < material[p.y].size() && p.x+dx >= 0 && (material[p.y][p.x+dx] == OCCUPIED || material[p.y][p.x+dx] == VISITED)) {
			point2d np { p.x + dx, p.y};
			if(material[np.y][np.x] != VISITED) next.insert(np);
			add_edge2d(graph, p, np, 0.5);
		}
 	for(char dy : dd) 
		if(p.y+dy < material.size() && p.y+dy >= 0 && (material[p.y + dy][p.x] == OCCUPIED || material[p.y+dy][p.x] == VISITED) ) {
			point2d np { p.x, p.y + dy};
			if(material[np.y][np.x] != VISITED) next.insert(np);
			add_edge2d(graph, p, np, 0.5); 
		}
}
hash_graph2d make_graph2d(vector<vector<int>>& material) {
	hash_graph2d graph;
	unordered_set<point2d, point2d_hash, point2d_equal> next, next2;
	int Yborder = material.size() - 1;

	point2d start_point{-1,-1};
	point2d end_point{material.size(), material.size()};

	
	

	for(auto & d1 : material) for(auto & d0 : d1) {
		if(d0 == VISITED) d0 = OCCUPIED;
		else d0 = FREE;
	}
	

	//link start_point with first_layer
	for(int x = 0; x < material[0].size(); ++x) {
		if(material[0][x] == OCCUPIED) {
			auto p = point2d{x,0};
			next.insert(p);
			add_edge2d(graph, start_point, p, 1e9);
		}
	}
	
	
	while(next.size() > 0) {
	

		for(const point2d &p: next) {
			material[p.y][p.x] = VISITED;
		}
		
		for(const point2d &p : next) {
			wave_nexts_mkgraph2d(material, next2, p, graph);
		}
		
		//link last layer with end_point
		for(const point2d &p : next) {
			if(p.y == Yborder) {
				add_edge2d(graph, end_point, p, 1e9);
			}
		}
		next = std::move(next2);
		next2.clear();
		++step;
	}
	
	
	return graph;
}

hash_graph2d relax_graph2d(const hash_graph2d & _graph, const point2d & start_point, const point2d & end_point) {
	//relax graph
	unordered_set<point2d, point2d_hash, point2d_equal> next;
	auto graph = _graph;
	bool have_smth_to_relax = true;
	while(have_smth_to_relax) {
		for(const auto &p : graph) {
			if(p.second.size() == 2 && p.first != start_point && p.first != end_point) {
				next.insert(p.first);
			}
		}
		
		have_smth_to_relax = next.size() > 0;

		for(const auto &p : next) {
			if(graph[p].size() == 2) {
				auto it = graph[p].begin();
				auto p1 = it->first;
				double w1 = it->second;
				++it;
				auto p2 = it->first;
				double w2 = it->second;
				graph[p1].erase(p);
				graph[p2].erase(p);
				graph.erase(p);
				add_edge2d(graph, p1, p2, 1/(1/w1 + 1/w2));
			}
		}
		next.clear();	
	}
	return graph;
}
typedef unordered_map<point2d, int, point2d_hash, point2d_equal> point2d_namer;

pair<vector<vector<double>>,point2d_namer>  laplacian_matrix2d(const hash_graph2d & bad_graph) {
	point2d_namer names;
	unordered_map<int, point2d> rev_names;
	vector<unordered_map<int,double>> graph;
	int i = 0;
	for(const auto& p : bad_graph) {
		names[p.first] = i;
		rev_names[i] = p.first;		
		++i;
	}
	int M = i;

	graph.resize(M);
	for(const auto& p : bad_graph) {
		unordered_map<int, double> &es = graph[names[p.first]];
		for(const auto& vs : p.second) {
			es[names[vs.first]] = vs.second;
		}
	}

	vector<vector<double>> matrix(M,vector<double>(M,0));

		
	for(int i = 0; i < M; i++) {
		for(int j = 0; j < M; j++) {
			if ( i == j ) {
				for(const auto &e : graph[i]) matrix[i][i] += e.second;
			}
			else {
				if( graph[i].count(j)) matrix[i][j] = -graph[i][j];
			}
		}

	}
	return make_pair(matrix,names);
}
using namespace Eigen;
pair<vector<Triplet<double>>, point2d_namer> laplacian_matrix2d_Eigen(const hash_graph2d & bad_graph) {
	point2d_namer names;
	unordered_map<int, point2d> rev_names;
	vector<unordered_map<int,double>> graph;
	int i = 0;
	for(const auto& p : bad_graph) {
		names[p.first] = i;
		rev_names[i] = p.first;		
		++i;
	}
	int M = i;

	graph.resize(M);
	for(const auto& p : bad_graph) {
		unordered_map<int, double> &es = graph[names[p.first]];
		for(const auto& vs : p.second) {
			es[names[vs.first]] = vs.second;
		}
	}
	vector<Triplet<double>> cr;
	for(int i = 0; i < M; i++) {
		double mii = 0;
		for(const auto &e : graph[i]) mii += e.second;
		cr.push_back(Triplet<double>(i,i,mii));
	}
	
	for(int i = 0; i < M; i++) {
		for(auto p : graph[i]) {
			cr.push_back(Triplet<double>(i,p.first, -p.second));
		}
	}
	return make_pair(cr, names);
}

double resistance(vector<vector<double>> _K, int from, int to, bool sparse = false) {
	int M = _K.size();
	arma::mat K1(M, M),K2(M, M);
	for(int i = 0; i < M; i++) {
		for(int j = 0; j < M; j++) {
			K1.at(i,j) = _K[i][j];
			K2.at(i,j) = _K[i][j];
		}
	}
	//cout <<M << ' ' << from << ' ' << to << endl;
	K1.shed_row(from);
	K1.shed_col(to);
	K2.shed_row(max(from,to));
	K2.shed_row(min(from,to));
	K2.shed_col(max(from,to));
	K2.shed_col(min(from,to));
	return abs(arma::det(K2) / arma::det(K1));
}

double resistance_Eigen(const vector<Triplet<double>> &K, int M, int from, int to) {
	using namespace Eigen;
	SparseMatrix<double, ColMajor> K1(M-1,M-1), K2(M-2,M-2);
	SparseLU<SparseMatrix<double, ColMajor> > S1, S2;

	//1
	vector<Triplet<double>> coef1;
	for(const auto & t : K) {
		if(t.col() != to && t.row() != from) {
			
			coef1.push_back(Triplet<double>(t.row() - (t.row() > from), t.col() - (t.col() > to), t.value()));
		}
	}
	//for(auto t : coef1) {
	//	cout << t.row() <<'\t' << t.col() << '\t' << t.value() << endl;
	//}
	K1.setFromTriplets(coef1.begin(), coef1.end());
	//cout << K1 << endl;
	S1.analyzePattern(K1); S1.factorize(K1);
	//cout << S1.lastErrorMessage() << endl;
	//cout << "d1" << endl;
	double d1 = S1.logAbsDeterminant();
	//2
	vector<Triplet<double>> coef2;
	for(const auto & t: K) {
		if(t.col() != to && t.row() != from && t.col() != from && t.row() != to) {
			int nrow = t.row() - (t.row() > from) - (t.row() > to);
			int ncol = t.col() - (t.col() > from) - (t.col() > to);
			coef2.push_back(Triplet<double>(nrow, ncol, t.value()));
		}
	}
	K2.setFromTriplets(coef2.begin(), coef2.end());
	S2.analyzePattern(K2); S2.factorize(K2);
	//cout << "d2" << endl;
	double d2 = S2.logAbsDeterminant();
	//cout <<"dets: "<< d1 << '\t' << d2 << endl;
	return exp(d2-d1);
}

struct point3d {
	int x,y,z;
};
struct point3d_hash {
	size_t operator() (const point3d& a) const {
		return (a.x + a.y * N + a.z * N);
	}
};
struct point3d_equal {
	bool operator() (const point3d& a, const point3d& b) const {
		return a.x == b.x && a.y == b.y && a.z == b.z;
	}
};

inline void nexts3d(vector<vector<vector<char>>> &material,unordered_set<point3d, point3d_hash, point3d_equal>& next, const point3d &p) {
	vector<char> dd {-1, 1};
	
	for(int dx : dd) 
		if(p.x+dx < material[p.z][p.y].size() && p.x+dx >= 0 && material[p.z][p.y][p.x+dx] == OCCUPIED) next.insert(point3d {p.x + dx, p.y, p.z } ); 
	for(int dy : dd) 
		if(p.y+dy < material[p.z].size()      && p.y+dy >= 0 && material[p.z][p.y+dy][p.x] == OCCUPIED) next.insert(point3d {p.x, p.y + dy, p.z }); 	
	for(int dz : dd) 
		if(p.z+dz < material.size()           && p.z+dz >= 0 && material[p.z+dz][p.y][p.x] == OCCUPIED) next.insert(point3d {p.x, p.y, p.z + dz } ); 
} 

bool bfs3d(vector<vector<vector<char>>> &material) {
	
	unordered_set<point3d, point3d_hash, point3d_equal> next, next2;
	//initial - nx = 0, nx,ny - all surface

	for(int y = 0;y  < material[0].size(); y++) {
		for(int x = 0; x < material[y].size(); x++) {
			next.insert(point3d {x,y,0});
		}
	}
	
	while(next.size() > 0) {

		//cout << next.size() << endl;
		for(const point3d &p: next) {
			material[p.z][p.y][p.x] = VISITED;
			if(p.z == material.size() - 1) {
				return true;
			}
		}
		for(const point3d &p: next) {
			nexts3d(material, next2, p);
			
		}
		next = std::move(next2);
		next2.clear();
	
	}
	return false;
}

int main2(int argc, char** argv) {
	
	int N = 200;
	int repeat = 10000;
	double step = 0.01;
	double start = 0.3;
	double finish = 0.8;
	string filename = "log2d.txt";
	if(argc > 1) {
		N = atoi(argv[1]);
	}
	if(argc > 2) {
		repeat = atoi(argv[2]);
	}
	if(argc > 3) {
		start = atof(argv[3]);
	}
	if(argc > 4) {
		finish = atof(argv[4]);
	}
	if(argc > 5) {
		step   = atof(argv[5]);
	}
	if(argc > 6) {
		filename = argv[6];
	}
	ofstream F(filename);
	vector<vector<int>>mat;
	mat.resize(N);
	for(auto & d1 :mat) d1.resize(N);
	for(double occ = start; occ < finish; occ += step) {
		int success = 0;
		for(int i = 0; i < repeat; i++) {
			simple_fill2d(mat, occ);
			success += bfs2d(mat);
		}
		F << occ << '\t' << double(success)/repeat << endl;
		cout << occ << '\t' << double(success)/repeat << endl;
	}


}
#define RESISTANCE 1
#define PERCOLATION 0
int main(int argc, char**argv) {
	int total = 10;
	int E = 1;
	int repeat = 100;
	double start = 0.6;
	double finish = 0.7;
	double step = 0.01;
	string filename = "logAR";
	int mode = RESISTANCE;	
	if(argc > 1) {
		N = atoi(argv[1]);
	}
	if(argc > 2) {
		repeat = atoi(argv[2]);
	}
	if(argc > 3) {
		start = atof(argv[3]);
	}
	if(argc > 4) {
		finish = atof(argv[4]);
	}
	if(argc > 5) {
		step = atof(argv[5]);
	}
	if(argc > 6) {
		E = atoi(argv[6]);
	}
	if(argc > 7) {
		mode = atoi(argv[7]);
	}
	if(argc > 8) {
		filename = argv[8];
	}
	//if(argc > 3) {
	//	E = atoi(argv[2]);
	//}
	cout << mode << endl;
	ofstream L("log0.txt");
	int Nx = N, Ny = N;
	vector<vector<int>> mat;
	mat.resize(Ny);
	for(auto& d1: mat) d1.resize(Nx);
	int fails = 0;
	if(mode == PERCOLATION) {
		filename = "logPerc";
	}
	string fullname = filename + to_string(E) + "_" + to_string(N) + "_"+ to_string(repeat) + "_" +  to_string(start) + "_" + to_string(finish) + ".txt";
	
	ofstream F(fullname);
	for(double occ = start; occ <= finish; occ += step) {
		if(mode == RESISTANCE) {
			double Caverage = 0;
			int off_stat = 0;
			double occ_average = 0;
			for(int i = 0; i < repeat; i++) {
				//L << start << ' ' << finish << ' ' << occ << endl;
				double real_occ = fill2d(mat, occ, E, true);
				occ_average+= real_occ;
				auto mat2 = mat;
				double R;
				if(!wave_forward2d(mat)) {
					Caverage += 0;
					//continue;
				}
				else {
				
					//printmat(mat, L);

					wave_backward2d(mat);

					//printmat(mat, L);

					hash_graph2d graph = make_graph2d(mat);
					point2d start_point { -1, -1};
					point2d end_point {mat.size(), mat.size() };
					auto relaxed_graph = relax_graph2d(graph,start_point, end_point);
					auto result = laplacian_matrix2d_Eigen(relaxed_graph);
					//auto result = laplacian_matrix2d(relaxed_graph);
					//cout << graph.size() << endl;
					//cout << relaxed_graph.size() << endl;
					if(result.second.size() == 2) {
						R = 1/result.first[0].value();
					}
					//else R = resistance(result.first, result.second[start_point], result.second[end_point]);
					else R = resistance_Eigen(result.first, relaxed_graph.size(), result.second[start_point], result.second[end_point]);
					double C = 1/R;
					if(C <= 1 && C > 0) 
					Caverage += C;
					else off_stat++;
				}	
				//cout << "R = " << R << endl;
			}
			Caverage /= (repeat-off_stat);
			F << occ << '\t' << Caverage << '\t' << occ_average/repeat << endl;
			double progress = (occ - start)/(finish - start)*100;// + step/(finish - start)*i/repeat*100;
			cout << progress << '%' << '\t' << occ << '\t' << Caverage <<'\t' << occ_average/repeat<<  endl;
		}
		else {
			double success = 0;
			double occ_average = 0;
			for(int i = 0;i < repeat; i++) {
				occ_average += fill2d(mat, occ, E, true);
				success += bfs2d(mat);
			}
			F << occ << '\t' << success/repeat << '\t' << occ_average/repeat << endl;
			double progress = (occ - start)/(finish - start)*100;// + step/(finish - start)*i/repeat*100;
			cout << progress << '%' << '\t' << occ << '\t' << success/repeat <<'\t' << occ_average/repeat<<  endl;
		}
	}
}


int main1(int argc, char** argv) {

	
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
			success += bfs3d(mat);
			
			
		}	
		cout << int((occ - occ_begin) / (occ_end - occ_begin) * 100) <<'%' << endl;
		fres << occ << '\t' << double(success)/total << endl;
	}
	
	
	return 0;
}
