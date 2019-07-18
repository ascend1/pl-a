# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # class method to choose the next piece
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  # class method to return the cheat piece
  def self.cheat_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end

  # Be aware: y-axis has positive values down below x-axis. This is critical
  # to make the 5-block blob piece correct!
  All_My_Pieces = All_Pieces +
                  [rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1,-1]]), # sp1
                   [[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]], # sp2 (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]],
                   rotations([[0, 0], [1, 0], [0, 1]])] # sp3

  # your enhancements here

end

class MyBoard < Board
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat_next = false
  end

  # rotates the current piece 180 degree
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  # cheat
  def cheat
    if !game_over? and @game.is_running? and @score >= 100 and !@cheat_next
      @cheat_next = true
      @score -= 100
    end
  end

  # gets the next piece
  def next_piece
    @current_block =
      if @cheat_next
        @cheat_next = false
        MyPiece.cheat_piece(self)
      else
        MyPiece.next_piece(self)
      end
    @current_pos = nil
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0...locations.size).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end

end
