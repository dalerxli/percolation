#include <fstream>
#include <iostream>
#include <vector>
#include <priority_queue>

using namespace std;


const double EPS = 1E-9;
 
struct pt {
	double x, y;
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
	seg(pt p, pt q, int id):p(p),q(q),id(id) {}
	double get_y (double x) const {
		if (abs (p.x - q.x) < EPS)  return p.y;
		return p.y + (q.y - p.y) * (x - p.x) / (q.x - p.x);
	}
};
 
 
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
	if (! intersect_1d (a.x, b.x, c.x, d.x) || ! intersect_1d (a.y, b.y, c.y, d.y))
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
};
 
set<seg> s;
priority_queue<event> events;
vector<vector<int>> edges;
vector < set<seg>::iterator > where;
vector<seg> a;

inline set<seg>::iterator prev (set<seg>::iterator it) {
	return it == s.begin() ? s.end() : --it;
}
 
inline set<seg>::iterator next (set<seg>::iterator it) {
	return ++it;
}

void check_intersection(const seg &a, const seg &b)  {
	if( intersect(a,b) ) {
		pt left, right;
		bool check = intersect_ext(a.p,a.q,b.p,b.q,left,right);
		if(!check) return;
		edges[a.id].insert(b.id);
		edges[b.id].insert(a.id);
		if(left == a.p || left == a.q ||  left == b.p || left == b.q || right == a.p || right == a.q || right == a.p || right == b.p || right == b.q) return;
		if(left == right) {
			events.insert(event (left.x, 0, a.id, b.id ));
		}
		else {
			events.insert(event (left.x, 1, a.id, b.id));
			events.insert(event (right.x, 2, a.id, b.id));
		}
	}
}

void sort_ids(int& id1, int& id2) {
	auto it1 = where[id1];
	it1++;
	if(it1 != where[id2]) {
		swap(id1, id2);
	}
}


void test_intersections(const event& e, const vector<seg> &segs) {
	if(e.tp >= 0) {
		//intersection
		int id1 = e.id, id2 = e.id2;
		if(e.tp == 0) {
			//usual intersection
			sort_ids(id1,id2);
			auto 	prv = prev(where[id1]),
				nxt = next(where[id2]);
			check_intersection(*prv, *where[id2]);
			check_intersection(*where[id1], *nxt);
			int midx = (events.top().x + e.x)/2;
			seg seg1(point(midx, where[id1]->get_y(midx)),*where[id1].q, id);
			seg seg2(point(midx, where[id2]->get_y(midx)),*where[id2].q, id); 
			segs.erase(where[id1]);
			segs.erase(where[id2]);
			a[id1] = seg1;
			a[id2] = seg2;
			where[id1] = segs.insert(seg1);
			where[id2] = segs.insert(seg2);
		}
		if(e.tp == 1) {
			//left of double intersection
			//TODO : union if double intesection IMMEDIATELY!!!

		}
	}
	else if(e.tp == -1) {
		//insert new 
		int id = e.id;
		set<seg>::iterator nxt = s.lower_bound(segs[id]), prv = prev(nxt);
		if(nxt != s.end()) check_intersection(*nxt, segs[id]);
		if(prv != s.end()) check_intersection(*nxt, segs[id]);
		where[id] = s.insert(nxt, segs[id]);
	}
	else {
		int id = e.id;
		set<seg>::iterator
				nxt = next (where[id]),
				prv = prev (where[id]);
		if(nxt != s.end() && prv != s.end() check_intersection(*prv, *nxt);
		s.erase (where[id]);
	}
}
 
vector<vector<int>> solve () {
	int n = (int) a.size();
	edges.resize(a.size());
	//vector<event> e;
	for (int i=0; i<n; ++i) {
		//e.push_back (event (min (a[i].p.x, a[i].q.x), +1, i));
		//e.push_back (event (max (a[i].p.x, a[i].q.x), -1, i));
		e.push(event (min (a[i].p.x, a[i].q.x), +1, i));
		e.push(event (max (a[i].p.x, a[i].q.x), -1, i));
		where[id] = s.insert(nxt, a[id])
	}
	//sort (e.begin(), e.end());
 
	s.clear();
	where.resize (a.size());
	for (size_t i=0; i<e.size(); ++i) {
		int id = e[i].id;
		if (e[i].tp == +1) {
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
		}
	}
 
	return make_pair (-1, -1);
}

vector<vector<int>> edges;
