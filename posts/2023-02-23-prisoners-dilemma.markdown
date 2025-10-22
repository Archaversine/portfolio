---
title: Simulating Prisoner’s Dilemma Strategies with Haskell
author: Adam Brohl
---

There are several different strategies that one could use to approach a 
prisoner’s dilemma-type situation, here are four of the many that will be explored:

1. **Always Cooperating**: Always choosing to cooperate with the opponent no matter what their past actions were.
2. **Always Defecting**: Always choosing to defect towards the opponent no matter what their past actions were.
3. **Grim Trigger**: Cooperating with the opponent until the opponent defects. Once the opponent defects, always defect towards the opponent.
4. **Tit for Tat**: Begin by cooperating with the opponent, after that copy the opponent’s last action.

To begin laying the groundwork for these situations, we need two main things:

1. **Strategy Type**: For the different strategies to be simulated
2. **Choice Type**: For the different possible choices each player can make.

Each of these are defined below:

```haskell
data Strategy = AlwaysCooperate
  | AlwaysDefect
  | GrimTrigger
  | TitForTat deriving Show

data Choice = Cooperate | Defect deriving (Eq, Show)
```

## Defining Strategies

The way a player makes a decision with a given strategy must be defined.
When making a decision, information of the opponent’s previous moves should 
also be taken into account, to allow for strategies like `GrimTrigger` and `TitForTat`.

Below is a function `getStrategyChoice` that returns a player’s choice utilizing
a specific strategy. This function takes in the player’s strategy and a list 
of the all the opponents previous moves (the first element is the most recent move made):

```haskell 
getStrategyChoice :: Strategy -> [Choice] -> Choice
getStrategyChoice AlwaysCooperate _ = Cooperate
getStrategyChoice AlwaysDefect    _ = Defect
getStrategyChoice GrimTrigger opponentOutcomes
    | Defect `elem` opponentOutcomes = Defect    -- If opponent has defected in the past
    | otherwise                      = Cooperate -- If the opponent has never defected
getStrategyChoice TitForTat opponentOutcomes
    | null opponentOutcomes = Cooperate             -- If opponent hasn't moved yet cooperate
    | otherwise             = head opponentOutcomes -- Play opponent's most recent move
```

## Calculating Rewards

In the prisoner’s dilemma, players get a higher reward if they both cooperate
than if they both defect. However, both players have an incentive to defect
since if one player cooperates and the other defects, the one that defects
gets a higher reward than if they both cooperated. The rewards for each outcome
can be shown with the following table:

|               | P2 Cooperates  | P2 Defects    |
|---------------|----------------|---------------|
| P1 Cooperates | P1: 10, P2: 10 | P1: 0, P2: 15 |
| P1 Defects    | P1: 15, P2:  0 | P1: 5, P2:  5 |

Below is a function `getOutcomeRewards` that takes in two `Choice` types from each
player and returns a tuple of rewards (Ex: (0, 15)) where the first value is
the payoff for player one and the second value is the payoff for player two:

```haskell 
getOutcomeRewards :: Choice -> Choice -> (Int, Int)
getOutcomeRewards Cooperate Cooperate = (10, 10)
getOutcomeRewards Cooperate Defect    = (0, 15)
getOutcomeRewards Defect    Cooperate = (15, 0)
getOutcomeRewards Defect    Defect    = (5, 5)
```

## Simulating Strategies

All the groundwork for strategies and choices has been finished, now we need a
way to simulate the payoffs each strategy gets. A function `simulateStrategies`
will be defined below that takes in two strategies, and a number for how many
iterations to run them against each other. This function will then return a
list of tuples where each tuple has the `Strategy` and payoffs of that strategy
in the simulation. For example, simulating two `AlwaysDefect` strategies against
each other for 10 iterations would yield the result `[(AlwaysDefect, 50), (AlwaysDefect, 50)]`.

```haskell 
simulateStrategies :: Strategy -> Strategy -> Int -> [(Strategy, Int)]
simulateStrategies s1 s2 n = simulateStrategies' s1 s2 n [] (0, 0)
  where simulateStrategies' s1 s2 0 outcomes (r1, r2) = [(s1, r1), (s2, r2)]
        simulateStrategies' s1 s2 n outcomes (r1, r2) = do
          let choice1 = getStrategyChoice s1 (map snd outcomes)
              choice2 = getStrategyChoice s2 (map fst outcomes)
              outcome = (choice1, choice2)
              rewards = getOutcomeRewards choice1 choice2

          simulateStrategies' s1 s2 (n - 1) (outcome : outcomes) (fst rewards + r1, snd rewards + r2)
```

## Comparing Strategies

Now that `simulateStrategies` exists, it is now possible to compare strategies
with each other. For example, to compare the `AlwaysCooperate` with the
`AlwaysDefect` strategy, `simulateStrategies AlwaysCooperate AlwaysDefect 100` will
return the payoffs for both strategies after 100 iterations.

Below is the code to test each strategy with each other
(there are duplicates, but for the sake of simplicity they are left in):

```haskell
-- Comparing AlwaysCooperate
print $ simulateStrategies AlwaysCooperate AlwaysCooperate 100
print $ simulateStrategies AlwaysCooperate AlwaysDefect 100
print $ simulateStrategies AlwaysCooperate GrimTrigger 100
print $ simulateStrategies AlwaysCooperate TitForTat 100

-- Comparing AlwaysDefect
print $ simulateStrategies AlwaysDefect AlwaysCooperate 100
print $ simulateStrategies AlwaysDefect AlwaysDefect 100
print $ simulateStrategies AlwaysDefect GrimTrigger 100
print $ simulateStrategies AlwaysDefect TitForTat 100

-- Comparing GrimTrigger
print $ simulateStrategies GrimTrigger AlwaysCooperate 100
print $ simulateStrategies GrimTrigger AlwaysDefect 100
print $ simulateStrategies GrimTrigger GrimTrigger 100
print $ simulateStrategies GrimTrigger TitForTat 100

-- Comparing TitForTat
print $ simulateStrategies TitForTat AlwaysCooperate 100
print $ simulateStrategies TitForTat AlwaysDefect 100
print $ simulateStrategies TitForTat GrimTrigger 100
print $ simulateStrategies TitForTat TitForTat 100
```

## Final Results

After comparing all the strategies with each other, we can see the payoffs
for each strategy against each strategy:
