# Battleship

A multiplayer implementation of Battleship in Haskell

## Development Status

**Milestone 1 Completed**: Core game logic implemented and tested

## Testing

Run tests using Cabal:

```bash
cabal test
```

## Development Plan

### Milestone 1: Core Game Logic
- Basic types (Board, Ships, Coordinates)
- Ship placement with validation
- Shooting mechanics
- Turn management
- Win condition checking
- Pure game logic, no UI or network

### Milestone 2: Text UI
- Console-based interface
- Local two-player game
- Board visualization

### Milestone 3: Network Foundation
- TCP server connections
- Message passing
- Game protocol design

### Milestone 4: Two-Player Network Game
- Network game management
- Turn synchronization
- Error handling

### Milestone 5: Full Game
- Concurrent game support
- Robust error handling
- Match making
