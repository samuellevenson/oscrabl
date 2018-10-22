open Board

type player = {
  name: string;
  letters: tile list;
  score: int
}

type t = {
  board: board;
  bag: tile list;
  players: player list;
  current_player: int;
}

let init_bag = [
  {letter = "_"; value = 0};
  {letter = "_"; value = 0};
  {letter = "A"; value = 1};
  {letter = "A"; value = 1};
  {letter = "A"; value = 1};
  {letter = "A"; value = 1};
  {letter = "A"; value = 1};
  {letter = "A"; value = 1};
  {letter = "A"; value = 1};
  {letter = "A"; value = 1};
  {letter = "A"; value = 1};
  {letter = "B"; value = 3};
  {letter = "B"; value = 3};
  {letter = "C"; value = 3};
  {letter = "C"; value = 3};
  {letter = "D"; value = 2};
  {letter = "D"; value = 2};
  {letter = "D"; value = 2};
  {letter = "D"; value = 2};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "E"; value = 1};
  {letter = "F"; value = 4};
  {letter = "F"; value = 4};
  {letter = "G"; value = 2};
  {letter = "G"; value = 2};
  {letter = "G"; value = 2};
  {letter = "H"; value = 4};
  {letter = "H"; value = 4};
  {letter = "I"; value = 1};
  {letter = "I"; value = 1};
  {letter = "I"; value = 1};
  {letter = "I"; value = 1};
  {letter = "I"; value = 1};
  {letter = "I"; value = 1};
  {letter = "I"; value = 1};
  {letter = "I"; value = 1};
  {letter = "I"; value = 1};
  {letter = "J"; value = 8};
  {letter = "K"; value = 5};
  {letter = "L"; value = 1};
  {letter = "L"; value = 1};
  {letter = "L"; value = 1};
  {letter = "L"; value = 1};
  {letter = "M"; value = 3};
  {letter = "M"; value = 3};
  {letter = "N"; value = 1};
  {letter = "N"; value = 1};
  {letter = "N"; value = 1};
  {letter = "N"; value = 1};
  {letter = "N"; value = 1};
  {letter = "N"; value = 1};
  {letter = "O"; value = 1};
  {letter = "O"; value = 1};
  {letter = "O"; value = 1};
  {letter = "O"; value = 1};
  {letter = "O"; value = 1};
  {letter = "O"; value = 1};
  {letter = "O"; value = 1};
  {letter = "P"; value = 3};
  {letter = "P"; value = 3};
  {letter = "Q"; value = 10};
  {letter = "R"; value = 1};
  {letter = "R"; value = 1};
  {letter = "R"; value = 1};
  {letter = "R"; value = 1};
  {letter = "R"; value = 1};
  {letter = "R"; value = 1};
  {letter = "S"; value = 1};
  {letter = "S"; value = 1};
  {letter = "S"; value = 1};
  {letter = "S"; value = 1};
  {letter = "T"; value = 1};
  {letter = "T"; value = 1};
  {letter = "T"; value = 1};
  {letter = "T"; value = 1};
  {letter = "T"; value = 1};
  {letter = "T"; value = 1};
  {letter = "U"; value = 1};
  {letter = "U"; value = 1};
  {letter = "U"; value = 1};
  {letter = "U"; value = 1};
  {letter = "V"; value = 4};
  {letter = "V"; value = 4};
  {letter = "W"; value = 4};
  {letter = "W"; value = 4};
  {letter = "X"; value = 8};
  {letter = "Y"; value = 4};
  {letter = "Y"; value = 4};
  {letter = "Z"; value = 10};
]

let init_state = {
  board = emptyBoard;
  bag = init_bag;
  players = [];
  current_player = -1;
}
