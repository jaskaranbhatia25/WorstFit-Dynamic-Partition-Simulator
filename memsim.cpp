// -------------------------------------------------------------------------------------
// this is the only file you need to edit
// -------------------------------------------------------------------------------------
//
// (c) 2022, Pavol Federl, pfederl@ucalgary.ca
// Do not distribute this file.

#include "memsim.h"
#include <cassert>
#include <iostream>
#include <unordered_map>
#include <list>
#include <set>
#include <iterator>
#include <math.h>

// I recommend you implement the simulator as a class. This is only a suggestion.
// If you decide not to use this class, please remove it before submitting it.


struct Partition {
  int tag;
  int64_t size, addr;

  Partition(){
  }
};

typedef std::list<Partition>::iterator PartitionRef;
typedef std::set<PartitionRef>::iterator Tree;
struct scmp {
  bool operator()(const PartitionRef & c1, const PartitionRef & c2) const {
    if (c1->size == c2->size){
      return c1->addr < c2->addr;
    }
    else{
      return c1->size > c2->size;
    }
  }
};


struct Simulator {
  std::list<Partition> all_blocks;
  std::set<PartitionRef,scmp> free_blocks;
  std::unordered_map<long, std::vector<PartitionRef>> tagged_blocks;
  int64_t page_size;
  int64_t currentpages_request;
  PartitionRef partition_ref;

  Simulator(int64_t page_size)
  {
    this->all_blocks = std::list<Partition>();
    this->tagged_blocks=std::unordered_map<long, std::vector<PartitionRef>>();
    this->currentpages_request=0;
    this->page_size=page_size;
  }
  void allocate(int tag, int64_t size)
  {
    int booleanchecking = 0;
    if (all_blocks.empty()) {
      int pagestobeadded ;
      pagestobeadded= ceil((double)size / page_size);
      currentpages_request = currentpages_request + pagestobeadded;
      Partition latest;
      latest.tag = -1;
      latest.size = pagestobeadded * page_size;
      latest.addr = 0;
      all_blocks.push_back(latest);
      free_blocks.insert(all_blocks.begin());
    }
    if (! free_blocks.empty()) {
      PartitionRef iterating = (*free_blocks.begin());
      if (iterating->size >= size) {
        partition_ref = iterating;
        booleanchecking = 1;
        free_blocks.erase(free_blocks.begin());
      }     
    }
    if (! booleanchecking) {
      if (std::prev(all_blocks.end())->tag == -1) {
        int64_t pagestobeadded = ceil((double)(size - std::prev(all_blocks.end())->size) / page_size);
        currentpages_request = currentpages_request + pagestobeadded;
        int64_t available_size = std::prev(all_blocks.end())->size + (pagestobeadded * page_size);
        std::set<PartitionRef>::iterator itertor = free_blocks.find(std::prev(all_blocks.end()));
        if (itertor != free_blocks.end()) {
          free_blocks.erase(itertor);
        }
        std::prev(all_blocks.end())->size = size;
        std::prev(all_blocks.end())->tag = tag;
        available_size = available_size - size;
        tagged_blocks[tag].push_back(std::prev(all_blocks.end()));
        if (available_size > 0) {
          Partition latest;
          latest.tag = -1;
          latest.addr = std::prev(all_blocks.end())->addr + std::prev(all_blocks.end())->size;
          latest.size = available_size;
          all_blocks.push_back(latest);
          free_blocks.insert(std::prev(all_blocks.end()));
        }
      }
      else {
        int64_t pagestobeadded = ceil((double)size / page_size);
        currentpages_request = currentpages_request + pagestobeadded;
        Partition latest;
        latest.tag = tag;
        latest.addr = std::prev(all_blocks.end())->addr + std::prev(all_blocks.end())->size;
        latest.size = size;
        int64_t available_size = (pagestobeadded * page_size) - size;
        all_blocks.push_back(latest);
        tagged_blocks[tag].push_back(std::prev(all_blocks.end()));
        if (available_size > 0) {
          Partition jsb;
          jsb.tag = -1;
          jsb.addr = latest.addr + size;
          jsb.size = available_size;
          all_blocks.push_back(jsb);
          free_blocks.insert(std::prev(all_blocks.end()));
        }
      }
    }
    else {
      int64_t available_size = partition_ref->size;
      partition_ref->size = size;
      partition_ref->tag = tag;
      available_size = available_size - size;
      tagged_blocks[tag].push_back(partition_ref);
      if (available_size > 0) {
        Partition latest;
        latest.tag = -1;
        latest.addr = partition_ref->addr + partition_ref->size;
        latest.size = available_size;
        all_blocks.insert(std::next(partition_ref), latest);
        free_blocks.insert(std::next(partition_ref));
      }
    }
  }
  void deallocate(int tag)
  {
    PartitionRef current;
    PartitionRef previous;
    PartitionRef next;
    for(int i = 0; i< (int)tagged_blocks[tag].size();i++){
      current = tagged_blocks[tag].at(i);
      current->tag = -1;
      if(current==all_blocks.begin() && current==std::prev(all_blocks.end())){
        free_blocks.insert(current);
        continue;
      }   
      if(current!=all_blocks.begin()) previous =  std::prev(current);
      if(current!=std::prev(all_blocks.end())) next = std::next(current);
      if(current==all_blocks.begin() && next->tag==-1){
        Tree jass = free_blocks.find(next);
        free_blocks.erase(jass);
        current->size+=next->size;
        all_blocks.erase(next);
      }
      else if(current== std::prev(all_blocks.end()) && previous->tag==-1){
        Tree jass = free_blocks.find(previous);
        free_blocks.erase(jass);
        current->size+=previous->size;
        current->addr = previous->addr+current->size;
        all_blocks.erase(previous);
      }
      else if(current!=all_blocks.begin() && current != std::prev(all_blocks.end())){
        if(previous->tag==-1 && next->tag!=-1){
          Tree jass = free_blocks.find(previous);
          free_blocks.erase(jass);
          current->size+=previous->size;
          current->addr = previous->addr;
          all_blocks.erase(previous);
        }else if( previous->tag!=-1 && next->tag==-1){
          Tree jass = free_blocks.find(next);
          free_blocks.erase(jass);
          current->size+=next->size;
          all_blocks.erase(next);
        }else if( previous->tag==-1 && next->tag==-1){
          Tree jass1 = free_blocks.find(previous);
          Tree jass2 = free_blocks.find(next);
          free_blocks.erase(jass1);
          free_blocks.erase(jass2);
          current->size+=previous->size;
          current->size+=next->size;
          current->addr = previous->addr;
          all_blocks.erase(previous);
          all_blocks.erase(next);
        }
      } 
      free_blocks.insert(current);
    }
    
    tagged_blocks.erase(tag);

  }
  
  MemSimResult getStats()
  {
    MemSimResult result;

    if (free_blocks.empty()) {
      result.max_free_partition_size = 0;
      result.max_free_partition_address = 0;
    }else {
      result.max_free_partition_size = (*free_blocks.begin())->size;
      result.max_free_partition_address = (*free_blocks.begin())->addr;
    }
    result.n_pages_requested= currentpages_request;
    return result;
  }
  void check_consistency()
  {

  }
};
MemSimResult mem_sim(int64_t page_size, const std::vector<Request> & requests)
{
  Simulator sim(page_size);
  for (const auto & req : requests) {
    if (req.tag < 0) {
      sim.deallocate(-req.tag);
    } else {
      sim.allocate(req.tag, req.size);
    }
    sim.check_consistency();
  }
  return sim.getStats();
}
