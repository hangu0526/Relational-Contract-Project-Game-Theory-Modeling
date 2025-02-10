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


