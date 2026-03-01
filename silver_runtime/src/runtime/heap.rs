use super::object::Object;
use super::value::RtValue;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Handle {
    index: u32,
    generation: u32,
}

impl Handle {
    pub fn index(self) -> u32 {
        self.index
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HeapError {
    InvalidHandle,
}

#[derive(Debug)]
struct Entry {
    object: Object,
    marked: bool,
}

#[derive(Debug, Default)]
pub struct Heap {
    entries: Vec<Option<Entry>>,
    generations: Vec<u32>,
    free_list: Vec<u32>,
}

impl Heap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc(&mut self, object: Object) -> Handle {
        if let Some(index) = self.free_list.pop() {
            let index_usize = index as usize;
            let generation = self.generations[index_usize];
            self.entries[index_usize] = Some(Entry {
                object,
                marked: false,
            });
            return Handle { index, generation };
        }

        let index = self.entries.len() as u32;
        self.entries.push(Some(Entry {
            object,
            marked: false,
        }));
        self.generations.push(0);
        Handle {
            index,
            generation: 0,
        }
    }

    pub fn get(&self, handle: Handle) -> Result<&Object, HeapError> {
        let index = handle.index as usize;
        let Some(generation) = self.generations.get(index) else {
            return Err(HeapError::InvalidHandle);
        };
        if *generation != handle.generation {
            return Err(HeapError::InvalidHandle);
        }
        let Some(Some(entry)) = self.entries.get(index) else {
            return Err(HeapError::InvalidHandle);
        };
        Ok(&entry.object)
    }

    pub fn get_mut(&mut self, handle: Handle) -> Result<&mut Object, HeapError> {
        let index = handle.index as usize;
        let Some(generation) = self.generations.get(index) else {
            return Err(HeapError::InvalidHandle);
        };
        if *generation != handle.generation {
            return Err(HeapError::InvalidHandle);
        }
        let Some(Some(entry)) = self.entries.get_mut(index) else {
            return Err(HeapError::InvalidHandle);
        };
        Ok(&mut entry.object)
    }

    pub fn free(&mut self, handle: Handle) -> Result<Object, HeapError> {
        let index = handle.index as usize;
        let Some(generation) = self.generations.get_mut(index) else {
            return Err(HeapError::InvalidHandle);
        };
        if *generation != handle.generation {
            return Err(HeapError::InvalidHandle);
        }

        let Some(slot) = self.entries.get_mut(index) else {
            return Err(HeapError::InvalidHandle);
        };

        let Some(entry) = slot.take() else {
            return Err(HeapError::InvalidHandle);
        };

        // Invalidate all outstanding handles for this index.
        *generation = generation.wrapping_add(1);
        self.free_list.push(handle.index);
        Ok(entry.object)
    }

    pub fn collect_garbage(&mut self, roots: &[RtValue]) -> usize {
        // Clear marks
        for slot in self.entries.iter_mut() {
            if let Some(entry) = slot.as_mut() {
                entry.marked = false;
            }
        }

        // Mark phase
        let mut stack: Vec<Handle> = Vec::new();
        for root in roots {
            root.push_handles(&mut stack);
        }

        while let Some(handle) = stack.pop() {
            let index = handle.index as usize;
            let Some(generation) = self.generations.get(index) else {
                continue;
            };
            if *generation != handle.generation {
                continue;
            }

            let Some(Some(entry)) = self.entries.get_mut(index) else {
                continue;
            };

            if entry.marked {
                continue;
            }
            entry.marked = true;

            entry.object.push_handles(&mut stack);
        }

        // Sweep
        let mut freed = 0usize;
        for index in 0..self.entries.len() {
            let should_free = self.entries[index]
                .as_ref()
                .is_some_and(|entry| !entry.marked);
            if should_free {
                let _ = self.entries[index].take();
                self.generations[index] = self.generations[index].wrapping_add(1);
                self.free_list.push(index as u32);
                freed += 1;
            }
        }

        freed
    }

    pub fn allocated_count(&self) -> usize {
        self.entries.iter().filter(|e| e.is_some()).count()
    }
}
