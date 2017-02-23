# rlcourse-february-17-zhuoyu
rlcourse-february-17-zhuoyu created by GitHub Classroom

# Comparing n-step Q(sigma) with other n-step methods

In this exercise, I implement the SARSA(0), n-step SARSA, n-step Expected SARSA, n-step tree backup and n-step Q(sigma) on a grid game.
## The Environment

The example is simplified from Example 11.8 on http://artint.info/html/ArtInt_262.html. The original setting of the environment is available online. I tried the complex version of the game but it took too much time for the agent to learn it, so I modified the setting to simplify it. The following is the simplified version of it.
 

As shown in Figure 1, there are 25 grid locations the agent could be in, and in each episode the agent starts from P3. A prize is on the up-left corner, P0. When the agent lands on a prize, it receives a reward of 10 and the game stop. There are five monsters appear at the locations marked M. If the agent lands on these locations, it receives a reward of -10.
In this example, the state consists of two components: ⟨X,Y⟩, where X is the X-coordinate of the agent, Y is the Y-coordinate of the agent. There are thus 5×5 = 25 states. 
The agent has four actions: up, down, left, and right. These move the agent one step in the direction indicated by the name. If the agent crashes into an outside wall or one of the interior walls (the thick lines near the location R), it remains where was and receives a reward of -1.

## The experiments
The settings of the experiments are as follow:
Discount rate ϒ= 0.90;
Greedy policy ε=0.10;
Step size α=0.25 and 5;
n in the n-step methods n=2, 4, 6, 8;
Considered methods: SARSA(0), n-step SARSA, n-step Expected SARSA, n-step tree backup and n-step Q(sigma).

Results and some plots can be find in the word file.
The code is attached.
