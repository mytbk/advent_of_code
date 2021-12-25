#include <iostream>
#include <queue>
#include <cstdint>
#include <cstdlib>
#include <vector>
#include <unordered_set>
#include <cassert>

static const int NUM_PODS = 16;
/*
 * Same as part 1, but 2 more places in each rooms, so 23 places in total.
 * Using 5 bits to encode a position, 80 bits in total to encode all the amphipods
 *
 * The id numbers of each amphipod are the final positions of their rooms.
 *
 */

static const uint64_t pods_move_energy[4] = {1, 10, 100, 1000};

using pos_t = uint8_t;
using room_t = uint32_t;
using uint128_t = __uint128_t;

namespace std {
  template <>
  struct hash<uint128_t> {
    std::size_t operator()(uint128_t const &s) const noexcept
    {
      std::size_t h1 = std::hash<uint64_t>()(uint64_t(s));
      std::size_t h2 = std::hash<uint64_t>()(uint64_t(s >> 64));
      return h1 ^ (h2 << 1);
    }
  };
}

static constexpr bool in_a_room(pos_t pos) { return pos < 16; }
static constexpr room_t room_id(pos_t pos) { return pos / 4; }
static constexpr pos_t hall_left(room_t room) { return 17 + room; }
static constexpr pos_t hall_right(room_t room) { return hall_left(room) + 1; }

const pos_t HALL_MIN = 16;
const pos_t HALL_MAX = 22;

static int dist[23][23];

constexpr int pos_dist(pos_t s, pos_t t)
{
  const int room_x[4] = {2, 4, 6, 8};
  const int hall_x[7] = {0, 1, 3, 5, 7, 9, 10};

  if (s == t) {
    return 0;
  }

  if (in_a_room(s) && in_a_room(t) && room_id(s) == room_id(t)) {
    return abs(s - t);
  }

  int moves = 0;
  int hall_s_pos, hall_t_pos;
  if (in_a_room(s)) {
    moves += 4 - (s % 4);
    hall_s_pos = room_x[room_id(s)];
  } else {
    hall_s_pos = hall_x[s - 16];
  }
    
  if (in_a_room(t)) {
    moves += 4 - (t % 4);
    hall_t_pos = room_x[room_id(t)];
  } else {
    hall_t_pos = hall_x[t - 16];
  }
  
  return moves + abs(hall_t_pos - hall_s_pos);
}

void init_dist()
{
  for (size_t i = 0; i < 23; ++i) {
    for (size_t j = 0; j < 23; ++j) {
      dist[i][j] = pos_dist(i,j);
    }
  }
}

struct State
{
  uint8_t amphipod_positions[NUM_PODS];

  State(uint128_t state_code);
  uint128_t encode() const;
  std::vector<pos_t> move_amphipod(size_t pod) const;
  uint64_t estimate_energy() const;
};

State::State(uint128_t state_code)
{
  for (size_t i = 0; i < NUM_PODS; ++i) {
    amphipod_positions[i] = state_code & 0xff;
    state_code >>= 8;
  }
}

uint128_t State::encode() const
{
  uint128_t s = 0;
  for (size_t i = 0; i < NUM_PODS; ++i) {
    s |= ((uint128_t)amphipod_positions[i] << (i * 8));
  }
  return s;
}

/* possible moves:
 *   1. if an amphipod is in a room, and can go outside, then it
 *   can go to the hallway
 *   2. if an amphipod is in the hallway, it can only go to its
 *   room if possible
 */

std::vector<pos_t> State::move_amphipod(size_t pod) const
{
  std::vector<uint8_t> all_pos;

  bool occupied[23];
  std::fill(occupied, occupied + 23, false);

  for (size_t i = 0; i < NUM_PODS; ++i) {
    occupied[amphipod_positions[i]] = true;
  }

  pos_t pos = amphipod_positions[pod];
  if (in_a_room(pos)) {
    for (pos_t up = pos + 1; (up % 4) != 0; ++up) {
      // check if upside of pos is occupied
      if (occupied[up]) {
	return all_pos;
      }
    }
    room_t room = room_id(pos);

    for (pos_t p = hall_left(room); p >= HALL_MIN; p--) {
      if (!occupied[p]) {
	all_pos.push_back(p);
      } else {
	break;
      }
    }
    for (pos_t p = hall_right(room); p <= HALL_MAX; p++) {
      if (!occupied[p]) {
	all_pos.push_back(p);
      } else {
	break;
      }
    }
    return all_pos;
  } else {
    room_t dest_room = pod / 4;
    const pos_t room_pos_min = dest_room * 4;
    const pos_t room_pos_max = room_pos_min + 4 - 1;
    for (size_t i = 0; i < NUM_PODS; ++i) {
      if (i / 4 != dest_room &&
	  (amphipod_positions[i] >= room_pos_min && amphipod_positions[i] <= room_pos_max)) {
	// occupied by other amphipods
	return all_pos;
      }
    }

    for (size_t i = 0; i < NUM_PODS; ++i) {
      uint32_t p = amphipod_positions[i];
      if (pos < hall_left(dest_room) && p > pos && p <= hall_left(dest_room)) {
	return all_pos;
      }
      if (pos > hall_right(dest_room) && p < pos && p >= hall_right(dest_room)) {
	return all_pos;
      }
    }

    // we can move now, check the position to move
    for (pos_t p = room_pos_min; p <= room_pos_max; ++p) {
      if (!occupied[p]) {
	all_pos.push_back(p);
	return all_pos;
      }
    }
    assert(false);
  }
}

uint64_t State::estimate_energy() const
{
  uint64_t energy = 0;

  // to simplify the calculation, we first sort the position of each
  // amphipod class, and let amphipod POD go to place POD, the result
  // will still be the same
  pos_t sorted_positions[16];
  std::copy(amphipod_positions, amphipod_positions + 16, sorted_positions);

  for (size_t i = 0; i < 4; ++i) {
    // if the position is in the dest room, it should be sorted first
    auto sort_func = [i](pos_t a, pos_t b) {
      if (in_a_room(a) && in_a_room(b) && room_id(a) == i && room_id(b) == i) {
	// both a and b are in the dest room
	return a < b;
      }
      // otherwise, first check who's in the dest room
      if (in_a_room(a) && room_id(a) == i) {
	return true;
      }
      if (in_a_room(b) && room_id(b) == i) {
	return false;
      }
      // if both not in the dest room, it doesn't matter
      return false;
    };
    std::sort(sorted_positions + i * 4, sorted_positions + i * 4 + 4, sort_func);
  }

  for (size_t i = 0; i < NUM_PODS; ++i) {
    int moves = dist[sorted_positions[i]][i];
    energy += pods_move_energy[i / 4] * moves;
  }
  return energy;
}

struct Moving_State
{
  uint128_t state_code;
  uint64_t total_energy;
  uint64_t estimated_total_energy;
};

bool operator<(const Moving_State &s, const Moving_State &t)
{
  if (s.estimated_total_energy > t.estimated_total_energy) {
    return true;
  }
  if (s.estimated_total_energy == t.estimated_total_energy) {
    return s.total_energy > t.total_energy;
  }
  return false;
}

constexpr bool is_final_state(uint128_t s)
{
  for (size_t i = 0; i < NUM_PODS; ++i) {
    pos_t pos = (s >> (i * 8)) & 0xff;
    if ((pos / 4) != (i / 4)) {
      return false;
    }
  }
  return true;
}

uint64_t least_energy(uint128_t init_state)
{
  State s(init_state);

  Moving_State init;
  init.state_code = init_state;
  init.total_energy = 0;
  init.estimated_total_energy = s.estimate_energy();

  std::priority_queue<Moving_State> search_queue;
  std::unordered_set<uint128_t> searched_states;
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
    for (size_t pod_id = 0; pod_id < NUM_PODS; ++pod_id) {
      std::vector<pos_t> dest_pos = current_state.move_amphipod(pod_id);
      if (dest_pos.size() > 0) {
	pos_t cur_pos = current_state.amphipod_positions[pod_id];
	State next_state = current_state;
	for (pos_t next_pos: dest_pos) {
	  int distance = dist[cur_pos][next_pos];
	  uint64_t e = pods_move_energy[pod_id / 4] * distance;

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

constexpr uint128_t amphipod_state(pos_t pos[16])
{
  uint128_t st = 0;
  for (size_t i = 0; i < 16; ++i) {
    st |= (uint128_t)pos[i] << (i * 8);
  }
  return st;
}

int main()
{
  init_dist();

  pos_t state_0[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  std::cout << least_energy(amphipod_state(state_0)) << std::endl; // should be 0

  pos_t state_1[16] = {0,9,12,14,3,5,10,11,6,7,8,13,1,2,4,15};
  //pos_t state_1_1[16] = {0,9,12,14,3,5,10,11,6,7,8,13,1,2,4,22}; // state_1 + 3000
  //pos_t state_1_2[16] = {0,9,12,16,3,5,10,11,6,7,8,13,1,2,4,22}; // state_1 + 3010
  //pos_t state_1_3[16] = {0,9,12,16,3,5,10,21,6,7,8,13,1,2,4,22}; // state_1 + 3050
  //pos_t state_1_4[16] = {0,9,12,16,3,5,20,21,6,7,8,13,1,2,4,22}; // state_1 + 3080
  //pos_t state_1_5[16] = {0,17,12,16,3,5,20,21,6,7,8,13,1,2,4,22}; // state_1 + 3088
  //pos_t state_1_7[16] = {0,17,12,16,3,5,20,21,10,9,8,13,1,2,4,22}; // state_1 + 4288
  std::cout << least_energy(amphipod_state(state_1)) << std::endl; // example, should be 44169

  /*
#############
#...........#
###B#B#D#A###
  #D#C#B#A#
  #D#B#A#C#
  #D#C#A#C#
  #########
  */
  pos_t my_input[16] = {8,9,14,15,3,5,7,10,4,6,12,13,0,1,2,11};
  std::cout << least_energy(amphipod_state(my_input)) << std::endl;
}
