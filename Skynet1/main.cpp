#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <limits>
#include <set>
#include <queue>
#include <map>

#define INF 1000000000
#define UNDEFINED -1

using namespace std;

class Graph {
public:
	Graph(int n) {
		for (int i = 0; i < n; ++i) {
			_links.push_back(std::set<int>());
		}
	}

	void setGateway(int i) {
		_gateways.insert(i);
	}

	bool linked(int n1, int n2) {
		return _links[n1].find(n2) != _links[n1].end();
	}

	void link(int n1, int n2) {
		_links[n1].insert(n2);
		_links[n2].insert(n1);
	}

	void unlink(int n1, int n2) {
		_links[n1].erase(n2);
		_links[n2].erase(n1);
	}

	std::set<int>& links(int n) {
		return _links[n];
	}

	// Returns all nodes, including unlinked nodes
	std::vector<int> nodes() {
		std::vector<int> nodes;
		for (int i = 0; i < _links.size(); ++i) {
			nodes.push_back(i);
		}

		return nodes;
	}

	// Returns only nodes connected to n
	std::set<int> neighbours(int n) {
		std::set<int> neighbours;
		addNeighbours(neighbours, n);

		return neighbours;
	}

	std::set<int>& gateways() {
		return _gateways;
	}

private:
	void addNeighbours(std::set<int>& neighbours, int curr) {
		if (neighbours.find(curr) == neighbours.end()) {
			neighbours.insert(curr);
			for (int n : _links[curr]) {
				addNeighbours(neighbours, n);
			}
		}
	}

	std::vector<std::set<int> > _links;
	std::set<int> _gateways;
};

int nodeWithMinDist(std::set<int> q, std::vector<int> dst) {
	int min_dst = INF;
	int min = 0;
	for (int i = 0; i < dst.size(); ++i) {
		if ((dst[i] < min_dst) && (q.find(i) != q.end())) {
			min_dst = dst[i];
			min = i;
		}
	}

	return min;
}

class DijkstraResult {
public:
	DijkstraResult(std::vector<int> distances, std::vector<int> prevs)
		: _distances(distances), _prevs(prevs)
	{}

	std::vector<int> _distances;
	std::vector<int> _prevs;
};

DijkstraResult dijkstra(Graph& g, int source) {
	std::set<int> q;
	std::vector<int> dist;
	std::vector<int> prev;

	for (int n : g.neighbours(source)) {
		q.insert(n);
	}

	for (int n : g.nodes()) {
		dist.push_back(INF);
		prev.push_back(UNDEFINED);
	}

	dist[source] = 0;

	while (!q.empty()) {
		int u = nodeWithMinDist(q, dist);
		q.erase(u);

		for (int v : g.links(u)) {
			int alt = dist[u] + 1;
			if (alt < dist[v]) {
				dist[v] = alt;
				prev[v] = u;
			}
		}
	}

	return DijkstraResult(dist, prev);
}

bool gatewayIsReachable(DijkstraResult& dr, int gateway) {
	return (dr._distances[gateway] != UNDEFINED);
}

int closestGateway(Graph& g, DijkstraResult& dr) {
	int min_dist = INF;
	int closest_gateway = UNDEFINED;
	for (int gateway : g.gateways()) {
		if ((dr._distances[gateway] < min_dist) && gatewayIsReachable(dr, gateway)) {
			closest_gateway = gateway;
			min_dist = dr._distances[gateway];
		}
	}

	return closest_gateway;
}

int closestLink(Graph& g, DijkstraResult& dr, int gateway, int start) {
	int closestLink = gateway;

	while (dr._prevs[closestLink] != start) {
		closestLink = dr._prevs[closestLink];
	}

	return closestLink;
}

int main()
{
	int N; // the total number of nodes in the level, including the gateways
	int L; // the number of links
	int E; // the number of exit gateways
	cin >> N >> L >> E; cin.ignore();

	cerr << N << " " << L << " " << E << endl;

	Graph g(N);

	for (int i = 0; i < L; ++i) {
		int N1; // N1 and N2 defines a link between these nodes
		int N2;
		cin >> N1 >> N2; cin.ignore();

		cerr << N1 << " " << N2 << endl;

		g.link(N1, N2);
	}
	for (int i = 0; i < E; ++i) {
		int EI; // the index of a gateway node
		cin >> EI; cin.ignore();

		cerr << EI << endl;

		g.setGateway(EI);
	}

	std::vector<int> moves_log;

	// game loop
	while (1) {
		int SI; // The index of the node on which the Skynet agent is positioned this turn
		cin >> SI; cin.ignore();

		moves_log.push_back(SI);
		for (int move : moves_log) {
			cerr << move << endl;
		}

		DijkstraResult dr = dijkstra(g, SI);
		int cg = closestGateway(g, dr);

		int cl = closestLink(g, dr, cg, SI);

		g.unlink(SI, cl);

		cout << SI << " " << cl << endl;
		// Example: 0 1 are the indices of the nodes you wish to sever the link between
		//cout << "0 1" << endl;
	}
}