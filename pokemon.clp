; Define the Pokémon template with additional stats
(deftemplate pokemon
   (slot name)
   (slot health)
   (slot max-health)
   (slot attack)
   (slot defense)
   (slot speed)
   (multislot buffs (default (create$)))
   (multislot moves))

; Define the move template with move types and PP
(deftemplate move
   (slot name)
   (slot type)
   (slot power (default 0))
   (slot accuracy)
   (slot effect (default none))
   (slot target (default enemy))
   (slot description (default "No description available."))
   (slot max-pp)
   (slot current-pp))

; Define the game state template
(deftemplate game-state
   (slot player-pokemon)
   (slot enemy-pokemon)
   (slot current-turn))

; Define the battle state template
(deftemplate battle-state
   (slot player-pokemon-state)
   (slot enemy-pokemon-state))

; Define the Pokémon state template to keep track of dynamic stats
(deftemplate pokemon-state
   (slot name)
   (slot health)
   (slot max-health)
   (slot attack)
   (slot defense)
   (slot speed)
   (multislot buffs)
   (multislot moves))

; Initialize the Pokémon and their moves with adjusted PP
(deffacts initialize
   ; Moves with adjusted PP for healing moves
   (move (name Thunder-Shock) (type attack) (power 40) (accuracy 100)
         (description "An electric attack that may paralyze the foe.")
         (max-pp 15) (current-pp 15))
   (move (name Quick-Attack) (type attack) (power 40) (accuracy 100)
         (description "An attack that always strikes first.")
         (max-pp 30) (current-pp 30))
   (move (name Tail-Whip) (type debuff) (accuracy 100) (effect decrease-defense) (target enemy)
         (description "Wags the tail to lower the foe's defense.")
         (max-pp 20) (current-pp 20))
   (move (name Growl) (type debuff) (accuracy 100) (effect decrease-attack) (target enemy)
         (description "Growls to reduce the foe's attack power.")
         (max-pp 40) (current-pp 40))
   (move (name Scratch) (type attack) (power 40) (accuracy 100)
         (description "Scratches the foe with sharp claws.")
         (max-pp 35) (current-pp 35))
   (move (name Ember) (type attack) (power 40) (accuracy 100)
         (description "A small flame attack that may burn the foe.")
         (max-pp 25) (current-pp 25))
   (move (name Harden) (type buff) (accuracy 100) (effect increase-defense) (target self)
         (description "Stiffens the body's muscles to raise defense.")
         (max-pp 30) (current-pp 30))
   (move (name Vine-Whip) (type attack) (power 45) (accuracy 100)
         (description "Strikes the foe with slender vines.")
         (max-pp 25) (current-pp 25))
   (move (name Leech-Seed) (type heal) (accuracy 90) (effect heal) (target self)
         (description "Plants a seed that restores HP over time.")
         (max-pp 5) (current-pp 5))
   (move (name Tackle) (type attack) (power 40) (accuracy 100)
         (description "A full-body charge attack.")
         (max-pp 35) (current-pp 35))
   (move (name Water-Gun) (type attack) (power 40) (accuracy 100)
         (description "Squirts water to attack the foe.")
         (max-pp 25) (current-pp 25))
   (move (name Bubble) (type attack) (power 40) (accuracy 100)
         (description "An attack using bubbles that may lower speed.")
         (max-pp 30) (current-pp 30))
   (move (name Recover) (type heal) (accuracy 100) (effect heal) (target self)
         (description "Restores the user's HP by 50%.")
         (max-pp 5) (current-pp 5))
   (move (name Heal) (type heal) (accuracy 100) (effect heal) (target self)
         (description "Heals the user's HP by a fixed amount.")
         (max-pp 3) (current-pp 3))
   ; Pokémon definitions
   (pokemon (name Pikachu) (health 100) (max-health 100) (attack 55) (defense 40) (speed 90)
      (moves Thunder-Shock Quick-Attack Tail-Whip Heal))
   (pokemon (name Charmander) (health 100) (max-health 100) (attack 52) (defense 43) (speed 65)
      (moves Scratch Ember Growl Recover))
   (pokemon (name Bulbasaur) (health 100) (max-health 100) (attack 49) (defense 49) (speed 45)
      (moves Vine-Whip Tackle Leech-Seed Growl))
   (pokemon (name Squirtle) (health 100) (max-health 100) (attack 48) (defense 65) (speed 43)
      (moves Water-Gun Tackle Tail-Whip Recover))
   (pokemon (name Eevee) (health 100) (max-health 100) (attack 55) (defense 50) (speed 55)
      (moves Quick-Attack Tail-Whip Heal Growl)))

; Define the global variable for Pokémon list
(defglobal ?*pokemon-list* = (create$ Pikachu Charmander Bulbasaur Squirtle Eevee))

; Function to calculate damage
(deffunction calculate-damage (?attacker-state ?defender-state ?move)
   (bind ?move-power (fact-slot-value ?move power))
   (bind ?attacker-attack (fact-slot-value ?attacker-state attack))
   (bind ?defender-defense (fact-slot-value ?defender-state defense))
   (bind ?damage (max 1 (round (/ (* ?attacker-attack ?move-power) (+ ?defender-defense 50)))))
   (return ?damage))

; Function to apply move effects with reduced healing amount
(deffunction apply-move-effect (?user-state ?move ?opponent-state)
   (bind ?effect (fact-slot-value ?move effect))
   (bind ?target (fact-slot-value ?move target))
   (if (eq ?target self)
      then
         (bind ?target-state ?user-state)
      else
         (bind ?target-state ?opponent-state))
   (if (eq ?effect increase-defense)
      then
         (bind ?defense (fact-slot-value ?target-state defense))
         (bind ?new-defense (+ ?defense 10))
         (modify ?target-state (defense ?new-defense))
         (printout t (fact-slot-value ?target-state name) "'s defense increased!" crlf))
   (if (eq ?effect decrease-attack)
      then
         (bind ?attack (fact-slot-value ?target-state attack))
         (bind ?new-attack (max 1 (- ?attack 10)))
         (modify ?target-state (attack ?new-attack))
         (printout t (fact-slot-value ?target-state name) "'s attack decreased!" crlf))
   (if (eq ?effect decrease-defense)
      then
         (bind ?defense (fact-slot-value ?target-state defense))
         (bind ?new-defense (max 1 (- ?defense 10)))
         (modify ?target-state (defense ?new-defense))
         (printout t (fact-slot-value ?target-state name) "'s defense decreased!" crlf))
   (if (eq ?effect heal)
      then
         (bind ?health (fact-slot-value ?target-state health))
         (bind ?max-health (fact-slot-value ?target-state max-health))
         (bind ?heal-amount (round (/ ?max-health 4))) ; Heal 25% of max health
         (bind ?new-health (min ?max-health (+ ?health ?heal-amount)))
         (modify ?target-state (health ?new-health))
         (printout t (fact-slot-value ?target-state name) " healed and now has " ?new-health " health!" crlf)))

(deffunction get-move-fact (?move-name)
   (bind ?result (find-all-facts ((?m move)) 
      (eq ?move-name (fact-slot-value ?m name))))
   (if (> (length$ ?result) 0)
      then
         (return (nth$ 1 ?result))  ; Return the first matching result
      else
         (return FALSE)))

; Start the game
(defrule start-game
   =>
   (assert (game-state (current-turn choose-player))))

; Choose player's Pokémon
(defrule choose-player-pokemon
   ?fact <- (game-state (current-turn choose-player))
   =>
   (printout t crlf "Choose your Pokémon:" crlf)
   (bind ?index 1)
   (foreach ?pokemon-name ?*pokemon-list*
      (printout t ?index ". " ?pokemon-name crlf)
      (bind ?index (+ ?index 1)))
   (printout t ?index ". Random" crlf)
   (printout t "Enter the number of your choice: ")
   (bind ?choice (read))
   (bind ?total-options (+ (length$ ?*pokemon-list*) 1)) ; Total options including Random
   (if (and (integerp ?choice) (>= ?choice 1) (<= ?choice ?total-options))
      then
         (if (eq ?choice ?total-options) ; Random choice
            then
               (bind ?list-length (length$ ?*pokemon-list*))
               (bind ?random-choice (random 1 (+ ?list-length 1)))
               (bind ?pokemon-name (nth$ ?random-choice ?*pokemon-list*))
            else
               (bind ?pokemon-name (nth$ ?choice ?*pokemon-list*)))
         (printout t "You chose " ?pokemon-name "." crlf)
         (retract ?fact)
         (assert (game-state (player-pokemon ?pokemon-name) (current-turn choose-enemy)))
      else
         (printout t "Invalid choice. Try again." crlf)
         (assert (game-state (current-turn choose-player)))))

; Choose enemy's Pokémon
(defrule choose-enemy-pokemon
   ?fact <- (game-state (player-pokemon ?player-pokemon) (current-turn choose-enemy))
   =>
   (printout t crlf "Choose enemy's Pokémon:" crlf)
   (bind ?index 1)
   (foreach ?pokemon-name ?*pokemon-list*
      (printout t ?index ". " ?pokemon-name crlf)
      (bind ?index (+ ?index 1)))
   (printout t ?index ". Random" crlf)
   (printout t "Enter the number of your choice: ")
   (bind ?choice (read))
   (bind ?total-options (+ (length$ ?*pokemon-list*) 1)) ; Total options including Random
   (if (and (integerp ?choice) (>= ?choice 1) (<= ?choice ?total-options))
      then
         (if (eq ?choice ?total-options) ; Random choice
            then
               (bind ?list-length (length$ ?*pokemon-list*))
               (bind ?random-choice (random 1 (+ ?list-length 1)))
               (bind ?enemy-pokemon (nth$ ?random-choice ?*pokemon-list*))
            else
               (bind ?enemy-pokemon (nth$ ?choice ?*pokemon-list*)))
         (if (eq ?enemy-pokemon ?player-pokemon)
            then
               (printout t "Cannot choose the same Pokémon as the player. Try again." crlf)
               (assert (game-state (player-pokemon ?player-pokemon) (current-turn choose-enemy)))
            else
               (printout t "Enemy chose " ?enemy-pokemon "." crlf)
               (retract ?fact)
               (assert (game-state (player-pokemon ?player-pokemon) (enemy-pokemon ?enemy-pokemon) (current-turn battle))))
      else
         (printout t "Invalid choice. Try again." crlf)
         (assert (game-state (player-pokemon ?player-pokemon) (current-turn choose-enemy)))))

; Initialize battle state
(defrule initialize-battle-state
   ?game <- (game-state (player-pokemon ?player-pokemon) (enemy-pokemon ?enemy-pokemon) (current-turn battle))
   (pokemon (name ?player-pokemon) (moves $?player-moves) (health ?player-health) (max-health ?player-max-health) (attack ?player-attack) (defense ?player-defense) (speed ?player-speed))
   (pokemon (name ?enemy-pokemon) (moves $?enemy-moves) (health ?enemy-health) (max-health ?enemy-max-health) (attack ?enemy-attack) (defense ?enemy-defense) (speed ?enemy-speed))
   =>
   ; Create move instances for player Pokémon
   (bind ?player-move-facts (create$))
   (foreach ?move-name $?player-moves
      (bind ?move-fact (get-move-fact ?move-name))
      (if ?move-fact
         then
            (bind ?pp (fact-slot-value ?move-fact max-pp))
            (bind ?new-move (duplicate ?move-fact (current-pp ?pp)))
            (bind ?player-move-facts (insert$ ?player-move-facts (+ 1 (length$ ?player-move-facts)) ?new-move))))
   ; Create move instances for enemy Pokémon
   (bind ?enemy-move-facts (create$))
   (foreach ?move-name $?enemy-moves
      (bind ?move-fact (get-move-fact ?move-name))
      (if ?move-fact
         then
            (bind ?pp (fact-slot-value ?move-fact max-pp))
            (bind ?new-move (duplicate ?move-fact (current-pp ?pp)))
            (bind ?enemy-move-facts (insert$ ?enemy-move-facts (+ 1 (length$ ?enemy-move-facts)) ?new-move))))
   ; Create pokemon-state facts with moves
   (assert (pokemon-state (name ?player-pokemon) (health ?player-health) (max-health ?player-max-health)
                          (attack ?player-attack) (defense ?player-defense) (speed ?player-speed)
                          (moves ?player-move-facts)))
   (assert (pokemon-state (name ?enemy-pokemon) (health ?enemy-health) (max-health ?enemy-max-health)
                          (attack ?enemy-attack) (defense ?enemy-defense) (speed ?enemy-speed)
                          (moves ?enemy-move-facts)))
   ; Determine who goes first based on speed
   (if (>= ?player-speed ?enemy-speed)
      then
         (modify ?game (current-turn player))
      else
         (modify ?game (current-turn enemy)))
   (printout t crlf "Battle begins between " ?player-pokemon " and " ?enemy-pokemon "!" crlf))

; Player's turn
(defrule player-turn
   ?game <- (game-state (current-turn player) (player-pokemon ?player-pokemon) (enemy-pokemon ?enemy-pokemon))
   ?player-state <- (pokemon-state (name ?player-pokemon) (health ?player-health) (moves $?player-moves))
   ?enemy-state <- (pokemon-state (name ?enemy-pokemon) (health ?enemy-health))
   =>
   (printout t crlf "Your turn. Choose a move:" crlf)
   (bind ?index 1)
   (bind ?available-moves (create$))
   (foreach ?move-fact $?player-moves
      (bind ?move-name (fact-slot-value ?move-fact name))
      (bind ?current-pp (fact-slot-value ?move-fact current-pp))
      (bind ?description (fact-slot-value ?move-fact description))
      (if (> ?current-pp 0)
         then
            (printout t ?index ". " ?move-name " (PP: " ?current-pp ") - " ?description crlf)
            (bind ?available-moves (insert$ ?available-moves (+ 1 (length$ ?available-moves)) ?move-fact))
            (bind ?index (+ ?index 1))
         else
            (printout t ?index ". " ?move-name " (No PP left)" crlf)
            (bind ?index (+ ?index 1))))
   (if (> (length$ ?available-moves) 0)
      then
         (printout t "Enter the number of your choice: ")
         (bind ?choice (read))
         (if (and (integerp ?choice) (>= ?choice 1) (< ?choice ?index))
            then
               (bind ?selected-move-fact (nth$ ?choice ?available-moves))
               (bind ?move-name (fact-slot-value ?selected-move-fact name))
               ; Decrease PP
               (bind ?current-pp (fact-slot-value ?selected-move-fact current-pp))
               (bind ?new-pp (- ?current-pp 1))
               (modify ?selected-move-fact (current-pp ?new-pp))
               ; Proceed with move execution
               (printout t "You used " ?move-name "!" crlf)
               (bind ?move-type (fact-slot-value ?selected-move-fact type))
               (if (eq ?move-type attack)
                  then
                     ; Damage calculation
                     (bind ?damage (calculate-damage ?player-state ?enemy-state ?selected-move-fact))
                     (bind ?new-enemy-health (max 0 (- ?enemy-health ?damage)))
                     (modify ?enemy-state (health ?new-enemy-health))
                     (printout t "Enemy's health is now " ?new-enemy-health "." crlf)
                     (if (<= ?new-enemy-health 0)
                        then
                           (printout t crlf "Enemy fainted! You win!" crlf)
                           (halt)))
               (if (or (eq ?move-type buff) (eq ?move-type debuff) (eq ?move-type heal))
                  then
                     (apply-move-effect ?player-state ?selected-move-fact ?enemy-state))
               (modify ?game (current-turn enemy))
            else
               (printout t "Invalid move. Try again." crlf)
               (assert (game-state (current-turn player) (player-pokemon ?player-pokemon) (enemy-pokemon ?enemy-pokemon))))
      else
         (printout t "No moves left with PP! You struggle and lose 10 HP." crlf)
         ; Apply struggle effect
         (bind ?new-health (max 0 (- ?player-health 10)))
         (modify ?player-state (health ?new-health))
         (if (<= ?new-health 0)
            then
               (printout t crlf "Your Pokémon fainted! You lose!" crlf)
               (halt))
         (modify ?game (current-turn enemy))))

; Enemy's turn with decision-making
(defrule enemy-turn
   ?game <- (game-state (current-turn enemy) (player-pokemon ?player-pokemon) (enemy-pokemon ?enemy-pokemon))
   ?enemy-state <- (pokemon-state (name ?enemy-pokemon) (health ?enemy-health) (max-health ?enemy-max-health) (attack ?enemy-attack) (moves $?enemy-moves))
   ?player-state <- (pokemon-state (name ?player-pokemon) (health ?player-health) (attack ?player-attack))
   =>
   (bind ?action "attack")
   (bind ?selected-move-fact FALSE)
   (bind ?available-moves (create$))
   ; Build a list of moves with PP > 0
   (foreach ?move-fact $?enemy-moves
      (bind ?current-pp (fact-slot-value ?move-fact current-pp))
      (if (> ?current-pp 0)
         then
            (bind ?available-moves (insert$ ?available-moves (+ 1 (length$ ?available-moves)) ?move-fact))))
   ; Check for healing if health is low
   (if (< ?enemy-health (/ ?enemy-max-health 2))
      then
         (foreach ?move-fact ?available-moves
            (if (not ?selected-move-fact)
               then
                  (if (eq (fact-slot-value ?move-fact type) heal)
                     then
                        (bind ?selected-move-fact ?move-fact)
                        (bind ?action "heal")))))
   ; Check for debuff if player's attack is high
   (if (and (eq ?action "attack") (> ?player-attack 50))
      then
         (foreach ?move-fact ?available-moves
            (if (not ?selected-move-fact)
               then
                  (if (eq (fact-slot-value ?move-fact type) debuff)
                     then
                        (bind ?selected-move-fact ?move-fact)
                        (bind ?action "debuff")))))
   ; Default to attack
   (if (not ?selected-move-fact)
      then
         (foreach ?move-fact ?available-moves
            (if (not ?selected-move-fact)
               then
                  (if (eq (fact-slot-value ?move-fact type) attack)
                     then
                        (bind ?selected-move-fact ?move-fact)))))
   ; Execute selected move
   (if ?selected-move-fact
      then
         ; Decrease PP
         (bind ?current-pp (fact-slot-value ?selected-move-fact current-pp))
         (bind ?new-pp (- ?current-pp 1))
         (modify ?selected-move-fact (current-pp ?new-pp))
         (bind ?move-name (fact-slot-value ?selected-move-fact name))
         (printout t crlf "Enemy used " ?move-name "!" crlf)
         (bind ?move-type (fact-slot-value ?selected-move-fact type))
         (if (eq ?action "attack")
            then
               ; Damage calculation
               (bind ?damage (calculate-damage ?enemy-state ?player-state ?selected-move-fact))
               (bind ?new-player-health (max 0 (- ?player-health ?damage)))
               (modify ?player-state (health ?new-player-health))
               (printout t "Your health is now " ?new-player-health "." crlf)
               (if (<= ?new-player-health 0)
                  then
                     (printout t crlf "Your Pokémon fainted! You lose!" crlf)
                     (halt)))
         (if (eq ?action "heal")
            then
               (apply-move-effect ?enemy-state ?selected-move-fact ?enemy-state))
         (if (eq ?action "debuff")
            then
               (apply-move-effect ?enemy-state ?selected-move-fact ?player-state))
         (modify ?game (current-turn player))
      else
         (printout t "Enemy has no moves left with PP! It struggles and loses 10 HP." crlf)
         ; Apply struggle effect to enemy
         (bind ?new-health (max 0 (- ?enemy-health 10)))
         (modify ?enemy-state (health ?new-health))
         (if (<= ?new-health 0)
            then
               (printout t crlf "Enemy fainted! You win!" crlf)
               (halt))
         (modify ?game (current-turn player))))
