#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <set>
#include <unordered_set>
#include <cmath>
#include <random>


using namespace std;

random_device rnd;
const double EPS = 1E-9;
#define det(a,b,c,d)  (a*d-b*c) 
struct pt {
double x, y;
pt(double x = 0, double y = 0) : x(x), y(y) {}
bool operator< (const pt& p) const {
	return x < p.x - EPS || abs(x - p.x) < EPS && y < p.y - EPS;
}
bool operator== (const pt& p) const {
	return abs(x-p.x) < EPS && abs(y- p.y) < EPS; 
}
bool operator!= (const pt& p) const {
	return abs(x - p.x) <EPS || abs(y- p.y) < EPS;
}
};
#define point pt 
struct line {
double a, b, c;

line() {}
	line (pt p, pt q) {
		a = p.y - q.y;
		b = q.x - p.x;
		c = - a * p.x - b * p.y;
		norm();
	}

	void norm() {
		double z = sqrt (a*a + b*b);
		if (abs(z) > EPS)
			a /= z,  b /= z,  c /= z;
	}

	double dist (pt p) const {
		return a * p.x + b * p.y + c;
	}
	};

	inline bool betw (double l, double r, double x) {
	return min(l,r) <= x + EPS && x <= max(l,r) + EPS;
}

struct seg {
	pt p, q;
	int id;
	seg(pt p = pt(), pt q = pt(), int id = 0):p(p),q(q),id(id) {}
	double get_y (double x) const {
		if (abs (p.x - q.x) < EPS)  return min(p.y,q.y);
		return p.y + (q.y - p.y) * (x - p.x) / (q.x - p.x);
	}
};

ostream& operator<<(ostream& os, const pt& p) {
	os << '{' << p.x << ", " << p.y << '}';
}
ostream& operator<<(ostream& os, const seg& s) {
	os << s.p << ' ' << s.q;
} 
inline bool intersect1d (double l1, double r1, double l2, double r2) {
	if (l1 > r1)  swap (l1, r1);
	if (l2 > r2)  swap (l2, r2);
	return max (l1, l2) <= min (r1, r2) + EPS;
}

inline int vec (const pt & a, const pt & b, const pt & c) {
	double s = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
	return abs(s)<EPS ? 0 : s>0 ? +1 : -1;
}

bool intersect_ext (pt a, pt b, pt c, pt d, pt & left, pt & right) {
	if (! intersect1d (a.x, b.x, c.x, d.x) || ! intersect1d (a.y, b.y, c.y, d.y))
		return false;
	line m (a, b);
	line n (c, d);
	double zn = det (m.a, m.b, n.a, n.b);
	if (abs (zn) < EPS) {
		if (abs (m.dist (c)) > EPS || abs (n.dist (a)) > EPS)
			return false;
		if (b < a)  swap (a, b);
		if (d < c)  swap (c, d);
		left = max (a, c);
		right = min (b, d);
		return true;
	}
	else {
		left.x = right.x = - det (m.c, m.b, n.c, n.b) / zn;
		left.y = right.y = - det (m.a, m.c, n.a, n.c) / zn;
		return betw (a.x, b.x, left.x)
			&& betw (a.y, b.y, left.y)
			&& betw (c.x, d.x, left.x)
			&& betw (c.y, d.y, left.y);
	}
}
bool intersect_ext(const seg &a, const seg& b, pt & left, pt & right) {
	return intersect_ext(a.p, a.q, b.p, b.q, left, right);
}
bool intersect (const seg & a, const seg & b) {
	return intersect1d (a.p.x, a.q.x, b.p.x, b.q.x)
		&& intersect1d (a.p.y, a.q.y, b.p.y, b.q.y)
		&& vec (a.p, a.q, b.p) * vec (a.p, a.q, b.q) <= 0
		&& vec (b.p, b.q, a.p) * vec (b.p, b.q, a.q) <= 0;
}


bool operator< (const seg & a, const seg & b) {
	double x = max (min (a.p.x , a.q.x), min (b.p.x, b.q.x));
	return a.get_y(x) < b.get_y(x) - EPS;
}


struct event {
	double x;
	int tp, id, id2;

	event() { }
	event (double x, int tp, int id, int id2 = 0)
		: x(x), tp(tp), id(id), id2(id2)
	{ }

	bool operator< (const event & e) const {
		if (abs (x - e.x) > EPS)  return x < e.x;
		return tp > e.tp;
	}
	bool operator> (const event & e) const {
		if (abs (x - e.x) > EPS) return x > e.x;
		return tp < e.tp;
	}
};

ostream& operator<<(ostream& os, const event & e) {
	return os << "x: " << e.x << " tp: " << e.tp << " id: " << e.id << " id2: " << e.id2;
}

set<seg> s;
priority_queue<event, vector<event>, greater<event>> events;
vector<unordered_set<int>> edges;
vector< set<seg>::iterator > where;
vector<seg> a;

inline set<seg>::iterator prev (set<seg>::iterator it) {
	return it == s.begin() ? s.end() : --it;
}

inline set<seg>::iterator next (set<seg>::iterator it) {
	return ++it;
}

bool check_intersection(const seg &a, const seg &b)  {
	//cout << "check_int" << endl;
	//cout << "\t" << a << endl;
	//cout << "\t" << b << endl;
	if( intersect(a,b) ) {
		cout << "\t got!" << endl;
		pt left, right;
		bool check = intersect_ext(a.p,a.q,b.p,b.q,left,right);
		if(!check) return false;
		if(edges[a.id].count(b.id)) {
			return true;
		}
		edges[a.id].insert(b.id);
		edges[b.id].insert(a.id);
		if(left == a.p || left == a.q ||  left == b.p || left == b.q || right == a.p || right == a.q || right == a.p || right == b.p || right == b.q) return true;
		if(left == right) {
			//cout << "double intersection" << endl;
			//cout << '\t' << a.id << ' ' << b.id << ' ' << left.x <<  endl;
			events.push(event (left.x, 0, a.id, b.id ));
		}
		else {
			cout << "double intersection" << endl;
			//events.insert(event (left.x, 1, a.id, b.id));
			//events.insert(event (right.x, 2, a.id, b.id));
			point pt_ev = max(left,right);
			const seg& seg_ev = min(a,b);
			events.push(event(pt_ev.x, -1,seg_ev.id)); 
		}
		return true;
	}
	return false;
}

void sort_ids(int& id1, int& id2) {
	auto it1 = where[id1];
	it1++;
	//cout << "sort_ids" << endl;
	//if(it1 != s.end() && it1 != s.begin()) {
	//	cout <<'\t'<< *where[id1] << ' ' << id1 << endl;
	//	cout <<'\t'<< *where[id2] << ' ' << id2 << endl;
	//}
	if(it1 != where[id2]) {
		//cout << '\t'<<"swapped" << endl;
		swap(id1, id2);
	}
}


void erase_check(int id) {
	set<seg>::iterator
		nxt = next (where[id]),
		prv = prev (where[id]);
	if(nxt != s.end() && prv != s.end()) check_intersection(*prv, *nxt);
	s.erase (where[id]);
}
priority_queue<seg> vert;
void test_intersections(const event& e) {
	//cout << "entered inter" <<endl;
	//cout <<e << endl;
	if(e.tp == 0) {
		//intersection
		cout << "intersection" <<s.size() << ' ' << events.size() << ' ' << endl;
		int id1 = e.id, id2 = e.id2;
		//if(e.tp == 0) {
			//usual intersection
			sort_ids(id1,id2);
			/*if(1) {
				for(const auto &c:s) {
					cout <<'\t'<< c << endl;
				}
				cout << '\t' << "-------------------" << endl;
				cout << '\t' << a[id1] << endl;
				cout << '\t' << a[id2] << endl;
			}
			*/
			double midx = (events.top().x + e.x)/2;
			seg seg1(point{midx, where[id1]->get_y(midx)},where[id1]->q, id1);
			seg seg2(point{midx, where[id2]->get_y(midx)},where[id2]->q, id2); 
			
			cout << '\t' << "before" << endl;	
			s.erase(where[id1]);
			s.erase(where[id2]);
			a[id1] = seg1;
			a[id2] = seg2;
			where[id1] = s.insert(seg1).first;
			where[id2] = s.insert(seg2).first;
			cout << '\t' << "after" << endl;
			//cout << "after\n";
			/*if(1) {
				for(const auto &c:s) {
					cout <<'\t'<< c << endl;
				}
				cout << '\t' << "-------------------" << endl;
				cout << '\t' << a[id1] << endl;
				cout << '\t' << a[id2] << endl;
			}*/
			
			//cout << "\tw " << *where[id2] << endl;
			//cout << "\tw " << *where[id1] << endl;
			auto 	prv = prev(where[id2]),
				nxt = next(where[id1]);
			if(prv != s.end() ) {
				//cout <<"prv "<< *prv << endl;
				check_intersection(*prv, *where[id2]);
				check_intersection(*prv, *where[id1]);
			}
			if(nxt != s.end() ) {
				//cout <<"nxt "<< *nxt << endl;
				check_intersection(*where[id1], *nxt);
				check_intersection(*where[id2], *prv);
			}

			cout << '\t' << "after after" << endl;	
			//}
		}
		else if(e.tp == +1) {
			//insert new
			int id = e.id;
			if(abs(a[id].p.x - a[id].q.x) < EPS) {
				//vertical segments
				cout << "vertical shit" << endl;
				vert.push(a[id]);
				double currx = a[id].p.x;
				int nid = id;	
				while(true) {
					event ne = events.top();
					if(ne.tp == +1 && abs(a[ne.id].q.x - currx) < EPS && abs(a[ne.id].p.x) < EPS) {
						events.pop();
						vert.push(a[ne.id]);
					}	
				} 
				auto cur_s = s.begin();
				while(!vert.empty()) {
					auto vs = vert.top();
					vert.pop();
					if(!vert.empty()) {
						auto next_vs = vert.top();
						if(intersect(vs, next_vs)) {
							edges[vs.id].insert(next_vs.id);
							edges[next_vs.id].insert(next_vs.id);
						}
						
					}
					
					while(cur_s != s.end() && cur_s->get_y(currx) <  min(vs.p.y, vs.q.y)) cur_s++;
					while(cur_s != s.end() && intersect(*cur_s, vs)) {
						edges[vs.id].insert(cur_s->id);
						edges[cur_s->id].insert(vs.id);
					}
					if(cur_s != s.end()) cur_s--;
				}
			}
			else {
				cout << "insert" << endl;
				//cout << "trying to insert" << endl;
				set<seg>::iterator nxt = s.lower_bound(a[id]), prv = prev(nxt);
				while(nxt != s.end() &&	check_intersection(*nxt, a[id])) nxt = next(nxt);
				while(prv != s.end() && check_intersection(*prv, a[id])) prv = prev(prv);
				where[id] = s.insert(nxt, a[id]);
			}
		}
		else {
			erase_check(e.id);
		}
}
void generate_wires(pt lt, pt rd, int N, double size) {
	random_device rnd;
	double rnd_max = rnd.max();
	a.clear();
	size /= 2;
	//cout << "GEN " <<endl;
	//cout << "lt " << lt << "rd " << rd << endl;
	for(int i = 0; i < N; i++) {
		double x = (rnd()/rnd_max)*(rd.x-lt.x) + lt.x;
		double y = (rnd()/rnd_max)*(rd.y-lt.y) + lt.y;
		//cout << x << '\t' << y << endl;
		double angle = (rnd()/rnd_max)*2*3.1415926;
		a.push_back(seg(pt(x - size*cos(angle), y - size*sin(angle)), pt(x+size*cos(angle), y + size*sin(angle)), i));
	}
	//cout << "-----------------" << endl;
}
bool bfs(const vector<unordered_set<int>> & edges, const unordered_set<int> & start, const unordered_set<int> & end) {
	
	unordered_set<int> next(start), next2;
	//initial - nx = 0, nx,ny - all surface
	vector<int> visited(edges.size(), 0);
	
	while(next.size() > 0) {
		//cout << next.size() << endl;
		for(int v: next) {
			visited[v] = true;
			if(end.count(v)) {
				return true;
			}
		}
		for(int v: next) {
			for(int nv : edges[v]) 
				if(!visited[nv]) next2.insert(nv);
			
		}
		next = std::move(next2);
		next2.clear();
	}
	return false;
}


void solve () {
	edges.resize(a.size());
	//vector<event> e;
	//cout << "resized" << endl;
	s.clear();
	where.resize (a.size());
	for (int i=0; i<a.size(); ++i) {
		//e.push_back (event (min (a[i].p.x, a[i].q.x), +1, i));
		//e.push_back (event (max (a[i].p.x, a[i].q.x), -1, i));
		events.push(event (min (a[i].p.x, a[i].q.x), +1, i));
		if(abs(a[i].p.x - a[i].q.x) > EPS) 
			events.push(event (max (a[i].p.x, a[i].q.x), -1, i));
		//cout << i << endl;
	}
	//sort (e.begin(), e.end());
 	//cout << "events added" << endl;
	/*auto events2 = events;
	while(!events2.empty()) {
		auto e = events2.top();
		events2.pop();
		cout << e << endl;
	}*/
	while(events.size() > 0) {
		auto e = events.top();
		events.pop();
		test_intersections(e);
		/*if (e[i].tp == +1) {
			set<seg>	::iterator
				nxt = s.lower_bound (a[id]),
				prv = prev (nxt);
			if (nxt != s.end() && intersect (*nxt, a[id]))
				return make_pair (nxt->id, id);
			if (prv != s.end() && intersect (*prv, a[id]))
				return make_pair (prv->id, id);
			where[id] = s.insert (nxt, a[id]);
			
		}
		else {
			set<seg>::iterator
				nxt = next (where[id]),
				prv = prev (where[id]);
			if (nxt != s.end() && prv != s.end() && intersect (*nxt, *prv))
				return make_pair (prv->id, nxt->id);
			s.erase (where[id]);
		}*/
	}
}

unordered_set<int> get_all_ints(const seg & sg) {
	unordered_set<int> result;
	for(int i = 0; i < a.size(); ++i) {
		if(intersect(a[i], sg)) result.insert(i);
	}
	return result;
}


int main(int argc, char** argv) {
	int N = 100000;
	int R = 1;
	if(argc > 1) N = atoi(argv[1]);
	if(argc > 2) R = atoi(argv[2]);
	ifstream fin("input.txt");
	//int N;
	//fin >> N;
	//a.resize(N);
	//seg seg1, seg2;
	//fin >> seg1.p.x >> seg1.p.y >> seg1.q.x >> seg1.q.y;
	//fin >> seg2.p.x >> seg2.p.y >> seg2.q.x >> seg2.q.y;
	//cout << "PRECHECK "<<intersect(seg1, seg2) << endl;
	//seg1.id = 0;
	//seg2.id = 1;
	//a.push_back(seg1);
	//a.push_back(seg2);
	//for(int i = 0; i < N; i++) {
	//	fin >> a[i].p.x >> a[i].p.y >> a[i].q.x >> a[i].q.y;
	//	a[i].id = i;
	//}
	//cout << a.size() << endl;	
	double probability = 0,links_density = 0;
	for(int i = 0; i < R;i++) {
	generate_wires(pt(0,0),pt(100,100), N, 2);
	auto left_boundary = get_all_ints(seg(pt(0, -10), pt(0, 110), -1));
	auto right_boundary = get_all_ints(seg(pt(100, -10), pt(100, 110), -1));
	/*cout << "WIRES" << endl;
	for(int i = 0; i < a.size(); i++) {
		cout << a[i] << endl;
	}
	cout << "---------------" << endl;*/
	solve();
	for(const auto & ve : edges) links_density += ve.size();
	probability += bfs(edges, left_boundary, right_boundary);
	/*for(int i = 0; i < edges.size(); i++) {
		cout << i << ": ";
		for(const auto &ve : edges[i]) cout << ve << '\t';
		cout << endl;
	}*/
	}
	cout << (probability / R) << endl;
	cout << (links_density / R / N) << endl;
}
