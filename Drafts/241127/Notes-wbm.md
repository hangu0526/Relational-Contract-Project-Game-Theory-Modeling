This file has notes for this PD project
# <2024-11-02> Notes on Indentifying Strategies.

The goal is to create a data base where each row corresponds to a single player (with id) and the outcome from play in a PD.
The row contains the id of opponent, parameters for the PD (r,t,s,p in normalized form), number of rounds and following variables:

    PD-Type (0 or 1)  0=easy, 1=hard
    AC (0 or 1)  1 = person plays only C
    AD (0 of 1)  1= person plays only D
    TFT (0 or 1) 1= person is consistent with TFT
    G_T, T=0,1,2,3 (0 of 1) 1 = if play consistent  with grim, but D for T periods less left to play.

    G_0  represent grim for all periods.

One way to code this is write the code for each strategy to play the game.  Then for each game, use the strategy to predict how the player will play.  If the player ever deviates from the prediction, then code this as 0. If one makes it to the end of the game successfully prediction play, then one has 1.  

Thus for each player-round one will have a set of strategies with which the play is consistent.

The literature focuses upon how a player chooses a strategy, rather than how well a strategy does.  The new idea is
to compute the average payoffs for each strategy as follows.

For TFT, for each player-round that is consistent with TFT, take the payoff the player got and add this up and compute
the average payoff per round for each strategy.

Notice that there will be double counting.  For example, for some players their observed play will be consistent with both grim and TFT.  That will be fine.  The reason is that if player 1 is playing player 2, and and player one's strategy is consistent with both TFT and Grim, then both strategies would get the same payoff against player 2, and hence should count those payoffs for both grim and TFT.

The output would be a table for each of the case (length and game difficulty) illustrating how each strategy is doing.

# Details for code
## <2024-11-09>
1. Sort data by arrange(session, id, supergame, round)
This will insure that rows correspond to players in the game.
When round =1  then that is first play
When round != 1 that is prevous plan.

2. Add ocoop variable which the the play for oid - can be found by
matching oid with id, and session supergame and round.

3. Create variables AC AD TFT G0 G1 G1

The variable will give the current play for player id, given previous play.

#### <2024-11-13 15:57:50> 
From the analysis I have done, it is clear that TFT does not do well in the finite prisoner's dilemma.
What would be useful is to see how it does versus other persons. 

We need to histograms.

Histogram 1 shows:
A: The distribution of  payoffs by session-id and
B: The distribution of payoffs under the TFT strategy.

Histogram 2: Selects only the games in which the individual played TFT, but not G0, and also their oponents.
Show a histgram of the individuals who played TFT, and separately their opponents (on the same chart).

