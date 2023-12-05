#!bin/zsh

# Download script for http://vintage-basic.net/games.html

rm -rf programs
mkdir programs

file_list=('aceyducey.bas' 'amazing.bas' 'animal.bas' 'awari.bas' 'bagels.bas' 'banner.bas' 'basketball.bas' 'batnum.bas' 'battle.bas' 'blackjack.bas' 'bombardment.bas' 'bombsaway.bas' 'bounce.bas' 'bowling.bas' 'boxing.bas' 'bug.bas' 'bullfight.bas' 'bullseye.bas' 'bunny.bas' 'buzzword.bas' 'calendar.bas' 'change.bas' 'checkers.bas' 'chemist.bas' 'chief.bas' 'chomp.bas' 'civilwar.bas' 'combat.bas' 'craps.bas' 'cube.bas' 'depthcharge.bas' 'diamond.bas' 'dice.bas' 'digits.bas' 'evenwins.bas' 'gameofevenwins.bas' 'flipflop.bas' 'ftball.bas' 'football.bas' 'furtrader.bas' 'golf.bas' 'gomoko.bas' 'guess.bas' 'gunner.bas' 'hammurabi.bas' 'hangman.bas' 'hello.bas' 'hexapawn.bas' hi-lo.bas 'highiq.bas' 'hockey.bas' 'horserace.bas' 'hurkle.bas' 'kinema.bas' 'king.bas' 'letter.bas' 'life.bas' 'lifefortwo.bas' 'litquiz.bas' 'love.bas' 'lunar.bas' 'lem.bas' 'rocket.bas' 'mastermind.bas' 'mathdice.bas' 'mugwump.bas' 'name.bas' 'nicomachus.bas' 'nim.bas' 'number.bas' 'onecheck.bas' 'orbit.bas' 'pizza.bas' 'poetry.bas' 'poker.bas' 'queen.bas' 'reverse.bas' 'rockscissors.bas' 'roulette.bas' 'russianroulette.bas' 'salvo.bas' 'sinewave.bas' 'slalom.bas' 'slots.bas' 'splat.bas' 'stars.bas' 'stockmarket.bas' 'superstartrek.bas' 'superstartrekins.bas' 'synonym.bas' 'target.bas' '3dplot.bas' 'qubit.bas' 'tictactoe1.bas' 'tictactoe2.bas' 'tower.bas' 'train.bas' 'trap.bas' '23matches.bas' 'war.bas' 'weekday.bas' 'word.bas')

for file in $file_list 
do
    curl "http://vintage-basic.net/bcg/$file" > "programs/$file"
done