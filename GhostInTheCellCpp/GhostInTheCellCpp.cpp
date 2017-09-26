#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

enum ActionType {
	MOVE,
	WAIT
};

typedef int8_t Player;

class Factory {

public:
	uint8_t _id;
	Player _player;
	uint16_t _cyborgs;
	uint8_t _production;

	Factory (uint8_t id, Player player, uint16_t cyborgs, uint8_t production)
		: _id{ id }, _player{ player }, _cyborgs{ cyborgs }, _production{ production }
	{}
};

class Troop {

public:
	uint8_t _id;
	Player _player;
	uint8_t _src;
	uint8_t _dst;
	uint16_t _cyborgs;
	uint8_t _turns;

	Troop(uint8_t id, Player player, uint8_t src, uint8_t dst, uint16_t cyborgs, uint8_t turns)
		: _id{ id }, _player{ player }, _src{ src }, _dst{ dst }, _cyborgs{ cyborgs }, _turns{ turns }
	{}
};

struct Turn {
	std::vector<Factory*> _factories;
	std::vector<Troop*> _troops;
};

class Graph {
public:
	Graph(uint8_t elements)
		: _elements{elements} {
		_graph = new int8_t[elements * elements];
		
		for (int i = 0; i < elements; ++i) {
			for (int j = 0; j < elements; ++j) {
				_graph[i * elements + j] = -1;
			}
		}
	}

	~Graph() {
		delete _graph;
	}

	int8_t get(int x, int y) {
		return _graph[x * _elements + y];
	}

	void set(int x, int y, int8_t value) {
		_graph[x * _elements + y] = value;
	}

private:
	int8_t *_graph;
	uint8_t _elements;
};

struct Action {
	ActionType _type;
	uint8_t _src;
	uint8_t _dst;
	uint16_t _cyborgs;
};

class ActionsPool {
public:
	ActionsPool(int size)
		: _size{ size } {
		_pool = new Action[size];
		_free = new bool[size];
	}

	~ActionsPool() {
		delete _pool;
		delete _free;
	}

	Action* newAction() {
		for (int i = 0; i < _size; ++i) {
			if (*_free == true) {
				return &(_pool[i]);
			}
		}

		throw new std::runtime_error("Out of memory");
	}

	void deleteAction(Action *action) {
		_free[(action - _pool) / sizeof(Action)] = true;
	}

private:
	Action *_pool;
	size_t _size;
	bool *_free;
};

std::vector<Factory*> playerFactories(std::vector<Factory*>& factories, Player player) {
	std::vector<Factory*> playerFactories;

	for (size_t i = 0; i < factories.size(); ++i) {
		if (factories[i]->_player == player) {
			playerFactories.push_back(factories[i]);
		}
	}

	return playerFactories;
}

std::vector<Factory*> neighbourEnemyFactories(int8_t src, Graph &graph, std::vector<Factory*>& factories, Player player) {
	std::vector<Factory*> dstFactories;

	for (size_t i = 0; i < factories.size(); ++i) {
		if (graph.get(src, i) > 0) {
			dstFactories.push_back(factories[i]);
		}
	}

	return dstFactories;
}

Action* nextAction(Action *prevAction, Graph &graph, std::vector<Factory*>& factories, Player player, ActionsPool &actionsPool) {
	if (prevAction == NULL) {
		Action *action = actionsPool.newAction();
		action->_cyborgs = 0;
		action->_dst = 0;
		action->_src = 0;
		action->_type = WAIT;
		return action;
	}
	
	int8_t src = prevAction->_src;
	int8_t dst = prevAction->_dst;
	int16_t cyborgs = prevAction->_cyborgs + 1;
	std::vector<Factory*> srcFactories = playerFactories(factories, player);
	while (src < srcFactories.size()) {
		std::vector<Factory*> dstFactories = neighbourEnemyFactories(src, graph, factories, player);
		while (dst < dstFactories.size()) {
			while (cyborgs < factories[src]->_cyborgs) {
				Action *action = actionsPool.newAction();
				action->_cyborgs = cyborgs;
				action->_dst = dst;
				action->_src = src;
				action->_type = MOVE;
				return action;
			}
			cyborgs = 0;
			++dst;
		}
		dst = 0;
		++src;
	}
	
	return NULL;
}

Factory* fightOuterBattle(Factory* factory, std::vector<Troop*> &troops, Player player) {
	// TODO Continue ... 
}

int main()
{
	int factoryCount; // the number of factories
	std::cin >> factoryCount; std::cin.ignore();
	int linkCount; // the number of links between factories

	Graph graph(factoryCount);

	std::cin >> linkCount; std::cin.ignore();
	for (int i = 0; i < linkCount; i++) {
		int factory1;
		int factory2;
		int distance;
		std::cin >> factory1 >> factory2 >> distance; std::cin.ignore();

		graph.set(factory1, factory2, distance);
		graph.set(factory2, factory1, distance);
	}

	std::vector<Factory> factories;
	std::vector<Troop> troops;

	// game loop
	while (1) {
		int entityCount; // the number of entities (e.g. factories and troops)
		std::cin >> entityCount; std::cin.ignore();
		for (int i = 0; i < entityCount; i++) {
			int entityId;
			std::string entityType;
			int arg1;
			int arg2;
			int arg3;
			int arg4;
			int arg5;
			std::cin >> entityId >> entityType >> arg1 >> arg2 >> arg3 >> arg4 >> arg5; std::cin.ignore();

			if (entityType == "FACTORY") {
				Factory factory(entityId, arg1, arg2, arg3);
				factories.push_back(factory);
			}
			else if (entityType == "TROOP") {
				Troop troop(entityId, arg1, arg2, arg3, arg4, arg5);
				troops.push_back(troop);
			}
		}

		// Write an action using cout. DON'T FORGET THE "<< endl"
		// To debug: cerr << "Debug messages..." << endl;


		// Any valid action, such as "WAIT" or "MOVE source destination cyborgs"
		std::cout << "WAIT" << std::endl;
	}
}