# Pokémon Battle Game in CLIPS

This is a turn-based Pokémon battle game implemented in the CLIPS programming language. Players select a Pokémon and battle against an enemy using moves with varying effects like attack, defense boosts, debuffs, and healing. The game demonstrates basic turn-based mechanics, dynamic game state management, and AI decision-making.

## Features

- **Pokémon Selection**: Choose your Pokémon from a predefined list or let the game select one randomly.
- **Moves**: Each Pokémon has unique moves with attributes like power, accuracy, and effects (e.g., heal, debuff, buff).
- **Dynamic Stats**: Tracks health, attack, defense, and speed during the battle.
- **AI Opponent**: The enemy Pokémon uses basic decision-making to select moves.
- **Battle Mechanics**: Implements damage calculation, effect application, and turn-based gameplay.

## Game Mechanics

### Pokémon Template
Defines the base stats and available moves for each Pokémon:
- `name`
- `health` and `max-health`
- `attack`
- `defense`
- `speed`
- `moves`

### Move Template
Defines properties of individual moves:
- `name`: The name of the move.
- `type`: The type (e.g., attack, heal, debuff, buff).
- `power`: Damage power (for attack moves).
- `accuracy`: Accuracy percentage.
- `effect`: Effects like healing or stat changes.
- `target`: Whether the move targets the self or the enemy.
- `max-pp` and `current-pp`: Tracks move usage.

### Dynamic Gameplay
- The `pokemon-state` template tracks real-time stats during the battle.
- AI chooses moves based on current health, player stats, and available moves.

## Setup and Execution

1. **Install CLIPS**: Ensure you have CLIPS installed on your system. You can download it from [CLIPS Downloads](http://www.clipsrules.net/).

2. **Load the Code**:
   - Save the `.clp` file to your local machine.
   - Open CLIPS and load the file:
     ```clips
     (load "pokemon-game.clp")
     ```

3. **Run the Game**:
   - Start the game by asserting the initial state:
     ```clips
     (assert (game-state (current-turn choose-player)))
     ```
   - Run the CLIPS engine:
     ```clips
     (run)
     ```

4. **Interact**:
   - Follow the on-screen prompts to choose your Pokémon, select moves, and battle the opponent.

## Rules and Turn Order

1. The player selects their Pokémon first, followed by the enemy's selection.
2. Turns are determined based on the speed stat of the Pokémon.
3. Each turn, the active player selects a move:
   - **Attack**: Deals damage based on the attacker's `attack` and the defender's `defense`.
   - **Buff/Debuff**: Increases or decreases stats like attack or defense.
   - **Heal**: Restores a percentage of the Pokémon's health.
4. The battle continues until one Pokémon's health reaches 0.

## Example Pokémon and Moves

### Pokémon
- **Pikachu**: Fast and agile, with moves like `Thunder-Shock` and `Quick-Attack`.
- **Charmander**: Balanced stats and moves like `Ember` and `Scratch`.
- **Bulbasaur**: Strong defensive stats with moves like `Leech-Seed` and `Growl`.
- **Squirtle**: High defense with moves like `Water-Gun` and `Recover`.
- **Eevee**: Versatile with moves like `Quick-Attack` and `Heal`.

### Moves
- **Thunder-Shock**: A basic electric attack with a chance to paralyze.
- **Recover**: Heals 50% of the Pokémon's max health.
- **Tail-Whip**: Reduces the opponent's defense.
- **Growl**: Reduces the opponent's attack power.

## Future Improvements

- Add more Pokémon and moves for greater variety.
- Implement type advantages (e.g., water > fire, fire > grass).
- Enhance AI decision-making for more strategic gameplay.
- Introduce multiplayer mode.

## License

This project is open-source and available under the MIT License.

## Contributing

Contributions are welcome! Feel free to fork this repository and submit a pull request with your improvements.

---

Enjoy your Pokémon battle adventure!
