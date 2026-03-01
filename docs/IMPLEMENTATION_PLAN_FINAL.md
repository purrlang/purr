# COMPREHENSIVE IMPLEMENTATION PLAN: PURR COMPILER TO MESSAGE BROKER

## Executive Summary

This plan documents the remaining work to complete the Purr programming language compiler and build a working message broker demo. Current status: M1-M11 complete + M12 parser/IR done; M12 semantic validation and M13+ remain.

**Total Scope**: 24 milestones + optional persistence layer
- **Critical Path** (M1-M24): ~20-25 additional implementation sessions
- **With Persistence** (M1-M27): ~28-35 additional implementation sessions
- **Highest Risk**: M16 (deterministic scheduler), M21 (event loop), M24 (delivery guarantees)
- **Blocking Issue**: M12 semantic validation must complete before M13+

---

## Phase Current Status

### Completed (M1-M11)
- ✅ M1-M5: Core language (lexer, parser, sema, IR, codegen)
- ✅ M6 GATE 1A: 5 scalar validation programs ready
- ✅ M7-M9: Structs, enums, containers with full C runtime
- ✅ M9 GATE 1B: 5 ADT validation programs ready
- ✅ M10.5: Bench infrastructure with runtime instrumentation
- ✅ M11: FFI (extern C functions with type mapping)

### In Progress
- 🔄 M12: Namespaces
  - ✅ Lexer: namespace, use keywords
  - ✅ Parser: namespace declarations, use statements with aliases
  - ✅ AST: namespace_name, uses list in program
  - ✅ IR: Pass-through to codegen
  - ⏳ **BLOCKING**: Semantic validation (namespace existence, name resolution, collision detection)

### Not Started (M13-M27)
- ❌ M13: Namespace enforcement & multi-file compilation
- ❌ M14: Message type definitions
- ❌ M15: Actor state fields & handler implementation
- ❌ M16: Deterministic scheduler & spawn/send runtime
- ❌ M17: Request/response channels
- ❌ M18: Lambda expressions
- ❌ M19 GATE 2: Actor validation programs
- ❌ M20-M27: Networking, broker, persistence

---

## Detailed Milestone Breakdown

### BLOCKING: M12 Semantic Validation (1-2 sessions)

**Purpose**: Enable multi-file compilation by validating namespace declarations and resolving qualified names.

**Completed Components**:
- Lexer recognizes `namespace` and `use` keywords
- Parser handles `namespace <name.hierarchy>` and `use <name>` / `use <name> = <alias>`
- AST stores namespace_name and uses list
- IR passes through namespace info to codegen

**Remaining Work**:
1. **Semantic Validation**:
   - Validate `namespace` is first non-comment token in file
   - Check that imported namespaces exist (require mapping of all known namespaces)
   - Detect alias collisions (two uses with same alias in one file)
   - Detect final-segment collisions (two imports ending in same name without explicit aliases)
   - For now: assume all namespace imports resolve (single-file compilation doesn't validate)

2. **Name Resolution**:
   - For now: qualified names only (e.g., `math.add(x, y)`)
   - Update symbol table to track qualified symbols
   - Enforce: unqualified access to imported symbols is a compile error

3. **Implementation**:
   - Add `Sema.validate_namespace` function
   - Modify `Sema.check_program` to call namespace validation before other checks
   - Update symbol table to accept qualified names (e.g., `"math.add"`)
   - Modify `Sema.lookup_function` to require qualification for imported symbols
   - Modify error reporting to show clear messages for qualification errors

**Testing**:
- `namespace_math.pu`: math module with `fn add(x: i32, y: i32) i32`
- `namespace_main.pu`: main module, `use math = example.math`, calls `example.add(x, y)`
- Compile separately, then together (multi-file compilation)
- Verify error: unqualified `add(x, y)` rejected

**Estimated Complexity**: Small (1-2 sessions)
- Straightforward validation logic
- No new AST/IR changes needed
- Most complexity is in error messages

**Deliverables**:
- `Sema.validate_namespace` function
- Updated symbol table with qualified name support
- Compiler error messages for all invalid cases (Section 8.7 of spec)
- Passing multi-file compilation tests

---

### M13: Multi-File Compilation (1 session)

**Purpose**: Driver support for compiling multiple `.pu` files into a single executable.

**Completed Components**:
- Namespace semantic validation (M12)

**Remaining Work**:
1. **Driver Changes**:
   - Modify `Driver.compile` to accept multiple input files
   - Parse command line: `purrc [--test|--bench] file1.pu file2.pu ... -o output`
   - For each input file:
     - Parse to AST
     - Build global namespace map
     - Validate semantic constraints
   - Generate single IR from all files' ASTs
   - Generate single C file

2. **Namespace Merging**:
   - Symbols from all files in same namespace are visible to each other
   - Symbols from different namespaces require qualification
   - Build global symbol table merging all namespaces

3. **Codegen**:
   - Single C file with all structs, enums, functions
   - Namespace info as C comments for organization
   - Single main() calls appropriate entry point

**Testing**:
- Namespace example programs compile and run correctly
- `namespace_main.pu` + `namespace_math.pu` + `namespace_utils.pu`

**Estimated Complexity**: Small (1 session)
- Most infrastructure already in place
- Main challenge is symbol table merging

**Deliverables**:
- Updated driver accepting multiple files
- Global namespace/symbol table
- Working multi-file compilation

---

### M14: Message Type Definitions (1 session)

**Purpose**: Implement `message` declarations as distinct types from structs.

**Completed Components**:
- AST: `message_def` type with name and fields
- Parser: `parse_message` function exists
- Lexer: `message` keyword recognized

**Remaining Work**:
1. **Semantic Analysis**:
   - Register message types in symbol table (distinct from structs)
   - Validate message fields contain only value types (no function types, no mailboxes)
   - Prevent sending struct types (compile error)
   - Allow message literals: `Subscribe { topic: "orders", sender: broker }`

2. **IR Representation**:
   - Messages as value types (like structs in C)
   - Add message field to program_ir

3. **Codegen**:
   - Generate C struct for each message
   - No special handling needed (same as structs)

4. **Runtime**:
   - Message type tags for dispatch (simple integers)
   - Message constructor helpers (if needed)

**Testing**:
- Define messages: `Subscribe`, `Publish`, `Deliver`
- Create message instances in code
- Type checking: reject sending struct as message

**Estimated Complexity**: Small (1 session)
- Mostly straightforward registration and validation
- Codegen identical to structs

**Deliverables**:
- Message type checking
- Message literal support
- Type errors for invalid message usage
- Message codegen (C structs)

---

### M15: Actor State and Handlers (2 sessions)

**Purpose**: Implement actor state fields and message handlers.

**Completed Components**:
- AST: `actor_def` with state_fields and handlers
- Parser: actor and handler syntax parsed
- Lexer: `actor`, `on`, `state` keywords

**Remaining Work**:

**Session 1: Semantic Analysis**:
- Register actor types in symbol table
- For each actor:
  - Validate all state fields are value types
  - Validate handlers correspond to defined messages
  - Track which messages an actor handles
  - Validate handler parameters match message fields
  - Enforce: state fields only accessible in handlers/private functions
  - Enforce: no cross-actor state access

**Session 2: IR & Codegen**:
- IR: Lower actor handlers to functions: `__actor_<name>_<message>(actor_instance, message)`
- Generate actor C struct with state fields
- Generate handler dispatch function
- Codegen: emit actor structs, handler functions

**IR Generation**:
```ocaml
(* For each handler in an actor: *)
Actor handler "Subscribe" becomes:
__actor_Broker_Subscribe(broker_instance: Broker, msg: Subscribe)
```

**C Codegen**:
```c
struct Broker {
    map* topics;  /* state field */
};

void __actor_Broker_Subscribe(Broker* self, Subscribe msg) {
    /* handler body */
}
```

**Testing**:
- Actor with multiple state fields and handlers
- Type checking: reject invalid state field types
- Handler dispatch: correct message delivered to handler

**Estimated Complexity**: Medium (2 sessions)
- State field handling straightforward
- Handler registration and validation clear
- IR/codegen generation requires careful function naming

**Deliverables**:
- Actor type registration
- Handler parameter validation
- State field access control (runtime checks in codegen)
- IR generation for handlers
- C codegen for actor structs and handlers

---

### M16: Deterministic Scheduler & Message Passing (3 sessions)

**Purpose**: Implement `spawn`, `send`, mailboxes, and deterministic message processing.

**Risk Level**: HIGH (most complex runtime component)

**Completed Components**:
- AST: `Spawn` and `Send` expressions
- Parser: `spawn ActorType { field: value }` and `send target MessageType { fields }`
- Lexer: `spawn`, `send` keywords

**Session 1: Mailbox Runtime**:
1. **C Runtime Mailbox Implementation**:
   ```c
   typedef struct {
       void** messages;          /* Queue of messages */
       int64_t capacity;         /* Fixed capacity */
       int64_t read_pos;         /* Reader position */
       int64_t write_pos;        /* Writer position */
       int64_t count;            /* Messages in queue */
   } PurrMailbox;

   /* Lock-free MPSC operations */
   PurrMailbox* purr_mailbox_create(int64_t capacity);
   bool purr_mailbox_send(PurrMailbox* box, void* message);
   void* purr_mailbox_recv(PurrMailbox* box);
   int64_t purr_mailbox_size(PurrMailbox* box);
   ```

2. **Actor Instance Runtime**:
   - Actor instance struct with:
     - State fields
     - Mailbox pointer
     - Last processed message
   - Actor creation: allocate, initialize state, attach mailbox

3. **Message Encoding/Decoding**:
   - Message type tag (enum)
   - Message data (union of all message types)
   - Serialization for queue storage

**Session 2: Deterministic Scheduler**:
1. **Scheduler Algorithm**:
   - Maintain list of active actors
   - Round-robin per actor:
     - Dequeue one message from mailbox
     - Dispatch to appropriate handler
     - Execute handler to completion
     - Re-queue actor if more messages
   - Continue until all queues empty

2. **C Runtime Scheduler**:
   ```c
   typedef struct {
       Actor** actors;           /* Active actor instances */
       int64_t actor_count;
       int64_t current_actor;    /* Round-robin position */
   } PurrScheduler;

   void purr_scheduler_init();
   void purr_scheduler_spawn_actor(Actor* actor);
   void purr_scheduler_step();  /* Process one message */
   void purr_scheduler_run();   /* Run to completion */
   void purr_scheduler_reset(); /* For tests */
   ```

3. **Testing Runtime Functions**:
   - `spawn_test(actor_init)`: spawn actor on test scheduler
   - `send_test(mailbox, message)`: enqueue message
   - `drain()`: run scheduler to completion
   - `expect_output(string)`: check output captured

**Session 3: Codegen & Integration**:
1. **Codegen Changes**:
   - `spawn ActorName {...}` → `purr_scheduler_spawn_actor(actor_instance)`
   - `send mailbox MessageType {...}` → `purr_mailbox_send(mailbox, message_data)`
   - Handler dispatch: generated dispatch function

2. **Main Program**:
   - If program has actors: replace main() with scheduler setup
   - Find Main actor if it exists
   - Initialize scheduler
   - Spawn Main actor with `on start()` handler
   - Run scheduler

**Example Codegen** (conceptual):
```c
Broker* broker = malloc(sizeof(Broker));
broker->topics = map_new();
broker->mailbox = purr_mailbox_create(100);
purr_scheduler_spawn_actor((Actor*)broker);

void send_Subscribe(Broker* broker, Subscribe msg) {
    void* msg_data = malloc(sizeof(WrappedMessage));
    *((WrappedMessage*)msg_data) = {
        .type = MESSAGE_SUBSCRIBE,
        .data.subscribe = msg
    };
    purr_mailbox_send(broker->mailbox, msg_data);
}

void __dispatch_Broker(Broker* self, WrappedMessage* msg) {
    switch(msg->type) {
        case MESSAGE_SUBSCRIBE:
            __actor_Broker_Subscribe(self, msg->data.subscribe);
            break;
    }
}
```

**Testing**:
- Simple actor: spawn, send message, verify handler executes
- Multiple actors: send between them, verify message order preserved
- Test runtime: `spawn_test`, `send_test`, `drain`, `expect_output`

**Estimated Complexity**: Large (3 sessions)
- Round-robin scheduler is straightforward
- Message encoding/decoding complex
- Integration with existing codegen requires careful changes
- Testing infrastructure adds surface area

**Deliverables**:
- Mailbox C runtime (lock-free MPSC queue)
- Deterministic scheduler algorithm + C runtime
- Actor spawning and dispatch
- Message encoding/decoding
- Test runtime (spawn_test, send_test, drain)
- Codegen for spawn/send/dispatch
- Working actor examples

---

### M17: Request/Response Channels (1-2 sessions)

**Purpose**: Implement `reply<T>` single-use reply channels.

**Completed Components**:
- AST: `Reply` type constructor
- Mailbox infrastructure (M16)

**Remaining Work**:
1. **Reply Type Implementation**:
   - `reply<T>` is a single-use mailbox with capacity 1
   - Can receive exactly one message
   - After read, attempting to read again yields nil

2. **Request/Response Pattern**:
   - Message with `reply<ResponseType>` field
   - Handler writes response to reply channel
   - Caller waits/polls for response

3. **Codegen**:
   - `reply<T>` → `PurrMailbox*` (capacity 1) in C
   - Request message includes reply mailbox
   - Sender blocks/polls until response arrives

4. **Test Support**:
   - `send_request_test`: send and wait for reply
   - `reply_test`: send response via reply channel

**Testing**:
- Request/response pattern: ask one actor for data, get reply
- Multiple round-trip exchanges

**Estimated Complexity**: Small-Medium (1-2 sessions)
- Mostly specialized mailbox handling
- Limited new infrastructure

**Deliverables**:
- Reply channel type
- Request/response message patterns
- Codegen for reply handling
- Test examples

---

### M18: Lambda Expressions (1 session)

**Purpose**: Implement `(params): Type => expr` syntax for closures (no captures).

**Completed Components**:
- AST: Lambda expression type exists
- Parser: Lambda syntax parsing exists
- Lexer: `=>` token recognized

**Remaining Work**:
1. **Semantic Analysis**:
   - Type inference for lambda parameters and return type
   - Function type: `(i32, i32): i32`
   - Validate lambda body returns correct type
   - Enforce: no captures (lambda is just a function pointer)

2. **IR**:
   - Generate function for each lambda
   - Lambda literal → function pointer (address of generated function)

3. **Codegen**:
   - Generate C function for lambda
   - Lambda literal → function pointer

4. **Built-in Support**:
   - `list_each(list, lambda)`: call lambda for each element
   - `map_each(map, lambda)`: call lambda for each key-value pair

**Example**:
```purr
var f = (x: i32): i32 => (x * 2)
list_each(my_list, (elem: i32): void => print_i32(elem))
```

**Testing**:
- Lambda literals as values
- Lambdas passed to higher-order functions
- Lambda execution correctness

**Estimated Complexity**: Small (1 session)
- Function pointer codegen straightforward
- Main complexity in type checking

**Deliverables**:
- Lambda type inference
- Lambda codegen (function pointers)
- Integration with list_each, map_each
- Working lambda examples

---

### M19 GATE 2: Actor Validation Programs (2-3 sessions)

**Purpose**: Validate actor system works via LLM-generated test programs.

**Prerequisite**: M16 deterministic scheduler complete

**Programs to Generate** (5 actor programs):

1. **Ping-Pong** (M16-M18):
   - Two actors exchange messages N times
   - Each counts rounds
   - Verify both reach expected count

2. **Fan-Out/Fan-In** (M16-M17):
   - Producer spawns N workers
   - Producer sends requests to all workers
   - Workers reply with results
   - Producer aggregates replies

3. **Pipeline** (M16):
   - Chain of 3 actors
   - Message flows through pipeline
   - Each actor transforms and forwards
   - Verify output at end

4. **Supervisor** (M16):
   - Supervisor spawns child actor
   - On failure message, restarts child
   - Tracks restart count

5. **Load Balancer** (M16):
   - Load balancer actor maintains worker pool
   - Round-robin distributes incoming requests
   - Workers process and return results
   - Balancer forwards results back

**Testing**:
- Generate programs using LLM
- Compile and run on deterministic scheduler
- Verify correctness via `test` declarations with `spawn_test`/`send_test`/`drain`

**Scoring**:
- **4/5+ correct**: Actor system is working, proceed to networking
- **2-3/5**: Debug which actor patterns fail, fix language
- **<2/5**: Rethink actor model, may need redesign

**Bench Baseline**:
- Run all 5 programs with bench runner
- Record `message_count`, `scheduler_steps` baseline
- Use for future regression detection

**Estimated Complexity**: Medium (2-3 sessions)
- Depends entirely on actor system (M16-M18) correctness
- LLM generation + manual validation

**Deliverables**:
- 5 working actor programs
- Bench baseline recorded
- Diagnostic data if any programs fail

---

### M20: TCP Server (Blocking IO) (1-2 sessions)

**Purpose**: Implement blocking TCP server using actors.

**Prerequisites**: M16 deterministic scheduler, M11 FFI

**Remaining Work**:
1. **POSIX Extern Wrappers**:
   ```purr
   extern fn socket(domain: i32, type: i32, protocol: i32) i32
   extern fn bind(fd: i32, addr: bytes, len: i64) i32
   extern fn listen(fd: i32, backlog: i32) i32
   extern fn accept(fd: i32, addr: bytes, len: bytes) i32
   extern fn read(fd: i32, buf: bytes, count: i64) i64
   extern fn write(fd: i32, buf: bytes, count: i64) i64
   extern fn close(fd: i32) i32
   ```

2. **Echo Server Actor**:
   - Listener: accepts connections, spawns handler per connection
   - Handler: reads from socket, writes back
   - Use FFI calls within actors

3. **Blocking Single-Threaded Model**:
   - Main actor spawns listener
   - Listener blocks on accept()
   - Handler blocks on read/write
   - Scheduler only runs one actor at a time (deterministic)

**Example**:
```purr
actor Listener {
    on start(addr: string, port: i32) {
        var fd = socket(AF_INET, SOCK_STREAM, 0)
        var server_addr = bind_sockaddr(addr, port)
        bind(fd, server_addr, size_of(server_addr))
        listen(fd, 10)

        for true {
            var client_fd = accept(fd, nil, nil)
            spawn ClientHandler { fd: client_fd }
        }
    }
}
```

**Testing**:
- Compile and run echo server
- Connect with `nc` (netcat)
- Send text, verify echo response

**Estimated Complexity**: Small-Medium (1-2 sessions)
- POSIX wrappers straightforward
- Server logic simple
- Main complexity: integrating IO with actor system

**Deliverables**:
- POSIX extern declarations
- Echo server actor implementation
- Working echo server executable
- Netcat test verification

---

### M21: Event Loop Scheduler (2-3 sessions)

**Purpose**: Implement `epoll`/`kqueue` event loop for production scheduling.

**Risk Level**: HIGH (complex runtime)

**Prerequisites**: M20 TCP server, M16 deterministic scheduler

**Remaining Work**:
1. **Session 1: Event Loop Core**:
   - Platform detection (Linux → epoll, macOS → kqueue)
   - Integrate event loop into scheduler
   - Track file descriptors with interest flags (readable, writable, error)

2. **Session 2: Actor Yielding**:
   - Actors yield on IO (blocking call fails with "would block")
   - Scheduler removes actor from ready queue
   - Scheduler polls for IO readiness
   - Re-queue actor when IO ready
   - Non-blocking sockets throughout

3. **Session 3: Dual Scheduler Modes**:
   - `purr_scheduler_mode(DETERMINISTIC | EVENT_LOOP)`
   - Tests use DETERMINISTIC (reproducible)
   - Production uses EVENT_LOOP
   - Same actor code works in both modes

**C Runtime Pseudocode**:
```c
#ifdef __linux__
    int epfd = epoll_create1(0);
    struct epoll_event events[MAX_FDS];
#else
    int kqfd = kqueue();
    struct kevent events[MAX_FDS];
#endif

void purr_scheduler_step_event_loop() {
    int n = epoll_wait(epfd, events, MAX_FDS, timeout);
    for(int i = 0; i < n; i++) {
        Actor* actor = events[i].data.ptr;
        actor->ready = true;
    }

    /* Process all ready actors */
    for each actor in ready_queue {
        execute_one_message(actor);
    }
}
```

**Testing**:
- Echo server handles multiple concurrent connections
- Benchmark: message throughput deterministic vs event-loop

**Estimated Complexity**: Large (2-3 sessions)
- Event loop integration complex
- Platform-specific code (Linux/macOS)
- Yielding logic requires careful actor state tracking
- Non-blocking socket handling new

**Deliverables**:
- Event loop implementation (epoll/kqueue)
- Actor yielding on IO
- Dual scheduler modes
- Multi-client echo server
- Benchmark comparison

---

### M22: Wire Protocol (1 session)

**Purpose**: Implement binary protocol for message broker.

**Prerequisites**: M11 FFI, M14 messages

**Protocol Design**:
```
Command:
    1 byte: type (0=PUBLISH, 1=SUBSCRIBE, 2=ACK)

PUBLISH:
    type: 0
    topic_len: 2 bytes
    topic: bytes
    payload_len: 4 bytes
    payload: bytes

SUBSCRIBE:
    type: 1
    id: 4 bytes
    topic_len: 2 bytes
    topic: bytes

DELIVER (from broker):
    type: 2
    id: 4 bytes
    topic_len: 2 bytes
    topic: bytes
    payload_len: 4 bytes
    payload: bytes
```

**Remaining Work**:
1. **Protocol Messages**:
   ```purr
   message Publish {
       string topic
       bytes payload
   }

   message Subscribe {
       i32 id
       string topic
   }

   message Deliver {
       i32 id
       string topic
       bytes payload
   }
   ```

2. **Encode/Decode Functions**:
   - `encode_publish(msg: Publish): bytes`
   - `decode_publish(buf: bytes): Publish`
   - Similar for Subscribe, Deliver
   - Use `purr.std.bytes` for bit manipulation

3. **Parser for Incoming Data**:
   - TCP handler reads bytes
   - Parses command type
   - Dispatches to appropriate message handler

**Testing**:
- Hand-craft binary messages
- Parse and verify round-trip
- Encode/decode correctness

**Estimated Complexity**: Small (1 session)
- Binary format design straightforward
- Encode/decode using bytes utilities
- No new language features

**Deliverables**:
- Protocol message types
- Encode/decode functions
- Tested round-trip correctness

---

### M23: Topic Routing (1-2 sessions)

**Purpose**: Implement broker actor with topic-based message routing.

**Prerequisites**: M16 spawn/send, M22 wire protocol, M21 event loop (optional but helpful)

**Remaining Work**:
1. **Broker Actor**:
   ```purr
   actor Broker {
       state map<string, list<mailbox<Subscriber>>> topics

       on Subscribe(msg) {
           var subs = map_get(self.topics, msg.topic)
           var updated = list_append(subs, msg.sender)
           self.topics = map_set(self.topics, msg.topic, updated)
       }

       on Publish(msg) {
           var subs = map_get(self.topics, msg.topic)
           list_each(subs, (sub): void =>
               send sub Deliver { topic: msg.topic, payload: msg.payload })
       }
   }
   ```

2. **Subscriber Actor**:
   - Connects to TCP socket
   - Sends Subscribe message to broker
   - Listens for Deliver messages
   - Writes to socket

3. **TCP Handler Actor**:
   - Per-connection handler
   - Reads TCP data
   - Parses wire protocol
   - Sends/forwards messages to/from broker

4. **Scaling Test**:
   - Multiple subscribers to one topic
   - Multiple publishers
   - Verify fan-out distribution
   - Bench message distribution

**Testing**:
- 2 subscribers to "orders", 1 publisher
- Publish 100 messages
- Verify both subscribers receive all 100

**Bench**:
- 100 subscribers to topic
- 1000 messages published
- Measure `message_count` per message (should be linear)

**Estimated Complexity**: Medium (1-2 sessions)
- Broker logic straightforward
- Main complexity: TCP socket integration with actor system
- Scaling requires careful benchmark setup

**Deliverables**:
- Broker actor implementation
- Subscriber actor
- TCP handler integration
- Multi-subscriber test
- Scaling benchmark

---

### M24: Consumer Groups and ACK (2 sessions)

**Purpose**: Implement consumer groups for shared subscriptions and delivery guarantees.

**Prerequisites**: M23 topic routing, M17 reply channels

**Remaining Work**:
1. **Session 1: Consumer Groups**:
   - Track per-consumer delivery state: `map<consumer_id, message_offset>`
   - On Subscribe: add consumer to group instead of individual subscribers
   - On Publish: round-robin dispatch to ONE consumer per group
   - Each consumer in group gets distinct messages

2. **Session 2: ACK/NACK & Redelivery**:
   - Message IDs for tracking
   - ACK message: broker marks as delivered
   - NACK/timeout: message redelivered to next consumer in group
   - Timeout detection: timer per pending message
   - Requeue on timeout

**Consumer Group Messages**:
```purr
message Subscribe {
    i32 id
    string topic
    string group_id
    reply<SubscribeAck> reply
}

message SubscribeAck {
    bool success
}

message Ack {
    i32 message_id
}

message Nack {
    i32 message_id
}
```

**Broker State Expansion**:
```purr
actor Broker {
    state map<string, list<mailbox<Subscriber>>> topics
    state map<string, list<mailbox<Subscriber>>> consumer_groups
    state map<i32, (i32, Subscriber)> pending_messages  (* msg_id -> (offset, consumer) *)
    state map<string, i32> group_offsets  (* topic/group -> next_offset *)
}
```

**Testing**:
- 2 consumers in group "order-processors"
- Publish 100 messages
- Each consumer gets 50 (or load-balanced distribution)
- All messages delivered exactly once across group
- Timeout: simulate NACK by not ACKing, verify redelivery

**Bench**:
- 10 consumers in group
- 1000 messages
- Measure `message_count` (should be ~1000, not 10000)

**Estimated Complexity**: Large (2 sessions)
- Consumer group logic moderately complex
- Timeout handling adds state management
- Redelivery requires queue manipulation
- Testing delivery guarantees requires careful orchestration

**Deliverables**:
- Consumer group implementation
- ACK/NACK protocol
- Timeout-based redelivery
- Exactly-once delivery test
- Load balancing benchmark
- **Working message broker** (M1-M24 complete)

---

## Optional: Persistence Layer (M25-M27)

### M25: Append-Only Log (1-2 sessions)

**Purpose**: Persist messages to disk for durability.

- File-based append log: `broker.log`
- On Publish: append to log before routing
- On startup: replay log to restore state
- Messages remain in log after ACK (for archive)

**Deliverables**:
- Log writer (FFI file IO)
- Log parser
- Replay-on-startup logic
- Durability test

### M26: Log Compaction (1-2 sessions)

**Purpose**: Remove acknowledged messages, keep undelivered.

- Background compaction actor
- Scans log for ACK'd messages
- Writes compacted log
- Atomic swap (rename)
- Offsets survive compaction

### M27: LSM Storage (Stretch Goal, 3-4 sessions)

**Purpose**: Scalable metadata storage using log-structured merge trees.

- Sorted memtable + SSTable layers
- Compaction as actor
- Used for consumer group offsets
- High complexity, optional

---

## Summary: Milestone Dependencies

```
M1-M5 (core)
    ↓
M6 GATE 1A (validation)
    ↓
M7-M9 (type system)
    ↓
M10 GATE 1B (validation)
    ↓
M10.5 (bench)
    ↓
M11 (FFI)
    ↓
M12 (namespaces) ← BLOCKING
    ↓
M13 (multi-file)
    ↓
M14 (messages)
    ↓
M15 (actor state)
    ↓
M16 (scheduler) ← HIGH RISK
    ↓
M17 (reply channels)
    ↓
M18 (lambdas)
    ↓
M19 GATE 2 (validation)
    ↓
M20 (TCP blocking)
    ↓
M21 (event loop) ← HIGH RISK
    ↓
M22 (wire protocol)
    ↓
M23 (topic routing)
    ↓
M24 (consumer groups) ← WORKING BROKER
    ↓
M25-M27 (persistence, optional)
```

---

## Estimated Effort and Risk

| Milestone | Sessions | Risk | Blocking | Notes |
|-----------|----------|------|----------|-------|
| M12 sema  | 1-2      | Low  | YES      | Unblocks multi-file compilation |
| M13       | 1        | Low  | No       | Driver changes only |
| M14       | 1        | Low  | No       | Like structs, distinct type |
| M15       | 2        | Med  | No       | Handler dispatch is key complexity |
| M16       | 3        | HIGH | No       | Scheduler + mailbox + test runtime |
| M17       | 1-2      | Med  | No       | Specialized mailbox usage |
| M18       | 1        | Low  | No       | Function pointers |
| M19 GATE2 | 2-3      | —    | No       | Validation + benchmarking |
| M20       | 1-2      | Med  | No       | TCP blocking I/O |
| M21       | 2-3      | HIGH | No       | Event loop (epoll/kqueue) |
| M22       | 1        | Low  | No       | Protocol encoding |
| M23       | 1-2      | Med  | No       | Broker actor logic |
| M24       | 2        | HIGH | No       | Delivery guarantees |
| M25-M27   | 5-8      | —    | No       | Optional persistence |

**Total Sessions**: 20-25 to working broker (M1-M24)
**Critical Path**: M12 → M13 → M14-M15 → M16 → M19 → M21 → M22-M24

**Highest Risk Items**:
1. M16 (deterministic scheduler): complex runtime, message encoding
2. M21 (event loop): platform-specific, concurrent IO
3. M24 (delivery guarantees): timeout handling, exactly-once semantics

---

## Testing & Validation Strategy

### Per Milestone
1. **Semantic Validation**: Compile-time error checking
2. **Functionality Tests**: Example programs compile and run
3. **Integration Tests**: Multiple features work together
4. **Bench Baseline**: Record performance metrics

### Gates
- **M19 GATE 2**: 5 actor programs (ping-pong, fan-out, pipeline, supervisor, load-balancer)
- Scoring: 4+/5 pass → proceed; <4/5 → diagnose and fix

### Demo
- **M24**: Working message broker
  - 2 subscribers, 1 publisher
  - 100 messages delivered
  - Test: `purrc examples/broker_demo.pu && ./broker_demo.exe`
  - Verify output shows all messages delivered

---

## Commit Strategy

Commit at clear milestone boundaries:

```
commit "M12: Implement namespace semantic validation and multi-file support"
commit "M13: Add multi-file compilation to driver"
commit "M14: Implement message type definitions"
commit "M15: Implement actor state fields and handlers"
commit "M16.1: Implement mailbox runtime and actor spawning"
commit "M16.2: Implement deterministic scheduler"
commit "M16.3: Implement message dispatch and codegen"
commit "M17: Implement reply channels for request/response"
commit "M18: Implement lambda expressions"
commit "M19: GATE 2 actor validation (5 test programs)"
commit "M20: Implement blocking TCP server"
commit "M21: Implement event loop scheduler (epoll/kqueue)"
commit "M22: Implement wire protocol (publish/subscribe/ack)"
commit "M23: Implement broker actor with topic routing"
commit "M24: Implement consumer groups and delivery guarantees"
(optionally)
commit "M25: Implement append-only log persistence"
commit "M26: Implement log compaction"
commit "M27: Implement LSM storage for metadata"
```

---

## Critical Files for Implementation

### Phase M12-M13 (Namespaces & Multi-File)
- `compiler/purrc0/src/sema.ml` - Add namespace validation, qualified symbol lookup
- `compiler/purrc0/src/driver.ml` - Multi-file compilation, global symbol table

### Phase M14-M15 (Messages & Actor State)
- `compiler/purrc0/src/sema.ml` - Message type registration, handler validation
- `compiler/purrc0/src/ir.ml` - Handler IR generation
- `compiler/purrc0/src/codegen_c.ml` - Actor struct codegen, handler dispatch

### Phase M16-M18 (Scheduler & Concurrency)
- `compiler/purrc0/runtime/purr_runtime.h/c` - Mailbox, scheduler, test runtime
- `compiler/purrc0/src/ir.ml` - Spawn/send IR lowering
- `compiler/purrc0/src/codegen_c.ml` - Spawn/send codegen, main() actor setup
- `compiler/purrc0/src/sema.ml` - Spawn/send type checking

### Phase M20-M24 (Networking & Broker)
- `compiler/purrc0/runtime/purr_runtime.h/c` - Event loop, TCP integration
- `examples/broker.pu` - Working broker implementation
- `tools/build.py` - Test/bench infrastructure

---

## Next Actions

**Immediate**:
1. Complete M12 semantic validation (namespace name resolution)
2. Compile and test M1-M9 to verify current state
3. Run GATE 1A and GATE 1B validation programs

**First Session**:
1. Implement M12 semantic validation (1-2 sessions)
2. Add multi-file compilation to driver (M13)

**Critical Path**:
1. M14-M15: Messages & actors (get actor system basics working)
2. **M16: Deterministic scheduler** (highest complexity, highest risk)
3. M17-M18: Reply channels, lambdas
4. M19: GATE 2 validation (prove actor system works)
5. M20-M24: Networking and broker (deterministic → event loop → broker)

---

## Summary

This plan provides a clear roadmap from current state to working message broker. Each milestone has specific deliverables, clear dependencies, and estimated complexity. The staged approach ensures the language remains in a working state between sessions.
