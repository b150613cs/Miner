Miner
=====

Miner is a Minesweeper solver written in Common Lisp. It uses Bayes'
theorem to calculate estimates of the likehood of different squares
being mines. When testing it, I found it had about a 20% solve rate,
even though there is around a 30% chance it will lose on one of the
first two turns. It uses a library I wrote,
[Clamp](https://github.com/malisper/Clamp). Miner is dependent on the
clamp-experimental system in Clamp.

To play as a human, use the 'play-as-human' function. If you want to
watch the ai play, set the 'print-board-ai*' variable to T, and then
call the 'play-as-ai' function.