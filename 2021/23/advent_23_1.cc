#include <iostream>
#include <queue>
#include <cstdint>
#include <cstdlib>
#include <vector>
#include <unordered_set>
#include <cassert>

/*
 * We encode all the possible places as follows, with 15 hex digits:

#############
#89.A.B.C.DE#
###1#3#5#7###
  #0#2#4#6#
  #########

* So we can encode the state with a 32-bit number
*
* The id numbers of each amphipod are the final positions of their rooms.
*
*/

static const uint64_t pods_move_energy[4] = {1, 10, 100, 1000};

static constexpr bool in_a_room(uint32_t pos) { return pos < 8; }
static constexpr uint32_t room_id(uint32_t pos) { return pos / 2; }
static constexpr uint32_t hall_left(uint32_t room) { return 9 + room; }
static constexpr uint32_t hall_right(uint32_t room) { return hall_left(room) + 1; }

static int dist[15][15];

constexpr int pos_dist(uint32_t s, uint32_t t)
{
  const int coordinates[15][2] = {
    {2, 0}, {2, 1}, {4, 0}, {4, 1}, {6, 0}, {6, 1}, {8, 0}, {8, 1}, // rooms
    {0, 2}, {1, 2}, {3, 2}, {5, 2}, {7, 2}, {9, 2}, {10, 2}
  };

  if (s == t) {
    return 0;
  }
  if (s > t) {
    return pos_dist(t,s);
  }

  if (in_a_room(s) && in_a_room(t) && room_id(s) == room_id(t)) {
    return 1;
  }

  int moves = 0;
  int hall_s_pos = coordinates[s][0], hall_t_pos = coordinates[t][0];
  if (in_a_room(s)) {
    moves += 2 - (s % 2);
  }
  if (in_a_room(t)) {
    moves += 2 - (t % 2);
  }
  return moves + abs(hall_t_pos - hall_s_pos);
}

static_assert(pos_dist(1,6) == 9);
static_assert(pos_dist(1,9) == 2);

void init_dist()
{
  for (size_t i = 0; i < 15; ++i) {
    for (size_t j = 0; j < 15; ++j) {
      dist[i][j] = pos_dist(i,j);
    }
  }
}

struct State
{
  uint32_t amphipod_positions[8];

  State(uint32_t state_code);
  uint32_t encode() const;
  std::vector<uint32_t> move_amphipod(size_t pod) const;
  uint64_t estimate_energy() const;
};

State::State(uint32_t state_code)
{
  for (size_t i = 0; i < 8; ++i) {
    amphipod_positions[i] = state_code & 0xf;
    state_code >>= 4;
  }
}

uint32_t State::encode() const
{
  uint32_t s = 0;
  for (size_t i = 0; i < 8; ++i) {
    s |= (amphipod_positions[i] << (i * 4));
  }
  return s;
}

/* possible moves:
 *   1. if an amphipod is in a room, and can go outside, then it
 *   can go to the hallway
 *   2. if an amphipod is in the hallway, it can only go to its
 *   room if possible
 */

std::vector<uint32_t> State::move_amphipod(size_t pod) const
{
  std::vector<uint32_t> all_pos;

  bool occupied[15];
  std::fill(occupied, occupied + 15, false);

  for (size_t i = 0; i < 8; ++i) {
    occupied[amphipod_positions[i]] = true;
  }

  uint32_t pos = amphipod_positions[pod];
  if (in_a_room(pos)) {
    if (pos % 2 == 0) {
      if (occupied[pos + 1]) {
	return all_pos;
      }
    }
    uint32_t room = room_id(pos);

    for (uint32_t p = hall_left(room); p >= 8; p--) {
      if (!occupied[p]) {
	all_pos.push_back(p);
      } else {
	break;
      }
    }
    for (uint32_t p = hall_right(room); p <= 0xe; p++) {
      if (!occupied[p]) {
	all_pos.push_back(p);
      } else {
	break;
      }
    }
    return all_pos;
  } else {
    uint32_t dest_room = pod / 2;
    uint32_t room_pos = dest_room * 2;
    for (size_t i = 0; i < 8; ++i) {
      if (i / 2 != dest_room &&
	  (amphipod_positions[i] == room_pos || amphipod_positions[i] == room_pos + 1)) {
	// occupied by other amphipods
	return all_pos;
      }
    }

    for (size_t i = 0; i < 8; ++i) {
      uint32_t p = amphipod_positions[i];
      if (pos < hall_left(dest_room) && p > pos && p <= hall_left(dest_room)) {
	return all_pos;
      }
      if (pos > hall_right(dest_room) && p < pos && p >= hall_right(dest_room)) {
	return all_pos;
      }
    }

    // we can move now, check the position to move
    if (occupied[room_pos]) {
      all_pos.push_back(room_pos + 1);
      return all_pos;
    } else {
      all_pos.push_back(room_pos);
      return all_pos;
    }
  }
}

uint64_t State::estimate_energy() const
{
  uint64_t energy = 0;
  for (size_t i = 0; i < 8; ++i) {
    uint32_t p = amphipod_positions[i];
    uint32_t dest_room = i / 2;
    if (in_a_room(p) && room_id(p) == dest_room) {
      continue;
    }
    int moves;
    if (amphipod_positions[i ^ 1] == dest_room * 2 || i % 2 == 1) {
      // already have one in the room, or pod_id is odd number (then
      // pod_id - 1 will go first)
      moves = dist[p][dest_room * 2 + 1];
    } else {
      moves = dist[p][dest_room * 2];
    }
    energy += pods_move_energy[i / 2] * moves;
  }
  return energy;
}

struct Moving_State
{
  uint32_t state_code;
  uint64_t total_energy;
  uint64_t estimated_total_energy;
};

bool operator<(const Moving_State &s, const Moving_State &t)
{
  return s.estimated_total_energy > t.estimated_total_energy;
}

constexpr bool is_final_state(uint32_t s)
{
  uint32_t apods = s & 0xff;
  if (!(apods == 0x01 || apods == 0x10)) {
    return false;
  }
  uint32_t bpods = (s >> 8) & 0xff;
  if (!(bpods == 0x23 || bpods == 0x32)) {
    return false;
  }
  uint32_t cpods = (s >> 16) & 0xff;
  if (!(cpods == 0x45 || cpods == 0x54)) {
    return false;
  }
  uint32_t dpods = (s >> 24) & 0xff;
  if (!(dpods == 0x67 || dpods == 0x76)) {
    return false;
  }
  return true;
}

uint64_t least_energy(uint32_t init_state)
{
  State s(init_state);

  Moving_State init;
  init.state_code = init_state;
  init.total_energy = 0;
  init.estimated_total_energy = s.estimate_energy();

  std::priority_queue<Moving_State> search_queue;
  std::unordered_set<uint32_t> searched_states;
  search_queue.push(init);

  while (!search_queue.empty()) {
    const Moving_State top_state = search_queue.top();
    if (searched_states.contains(top_state.state_code)) {
      search_queue.pop();
      continue;
    }
    searched_states.insert(top_state.state_code);
    if (is_final_state(top_state.state_code)) {
      return top_state.total_energy;
    }
    const State current_state(top_state.state_code);
    for (size_t pod_id = 0; pod_id < 8; ++pod_id) {
      std::vector<uint32_t> dest_pos = current_state.move_amphipod(pod_id);
      if (dest_pos.size() > 0) {
	uint32_t cur_pos = current_state.amphipod_positions[pod_id];
	State next_state = current_state;
	for (uint32_t next_pos: dest_pos) {
	  int distance = dist[cur_pos][next_pos];
	  uint64_t e = pods_move_energy[pod_id / 2] * distance;

	  Moving_State next_moving_state;
	  next_state.amphipod_positions[pod_id] = next_pos;
	  next_moving_state.state_code = next_state.encode();
	  next_moving_state.total_energy = top_state.total_energy + e;
	  next_moving_state.estimated_total_energy =
	    next_moving_state.total_energy + next_state.estimate_energy();
	  search_queue.push(next_moving_state);
	}
      }
    }
    search_queue.pop();
  }

  return ~(0ULL);
}

int main()
{
  init_dist();
  std::cout << least_energy(0x76543210) << std::endl; // should be 0
  std::cout << least_energy(0x72435160) << std::endl; // example, should be 12521
  std::cout << least_energy(0x05261347) << std::endl; // my input
}
