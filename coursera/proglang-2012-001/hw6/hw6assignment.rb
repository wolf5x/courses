# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyTetris < Tetris
	# your enhancements here

	def set_board
		@canvas = TetrisCanvas.new
		# Only this line differs to the Tetris class
		@board = MyBoard.new(self)
		@canvas.place(@board.block_size * @board.num_rows + 3,
					  @board.block_size * @board.num_columns + 6, 24, 80)
		@board.draw
	end

	def key_bindings
		super
		@root.bind('c', proc {@board.cheat}) 
		@root.bind('u', proc {@board.rotate_180})
	end
end

class MyPiece < Piece
	# The constant All_My_Pieces should be declared here
	All_My_Pieces = All_Pieces + 
		[rotations([[0,0],[-1,0],[1,0],[0,1],[-1,1]]), # ==-
			[[[0,0],[-1,0],[1,0],[-2,0],[2,0]],
				[[0,0],[0,-1],[0,1],[0,-2],[0,2]]], # -----
			rotations([[0,0],[1,0],[0,1]])] # |_

	Cheat_Piece = [[[0,0]]]

	# your enhancements here

	def self.next_piece(board)
		MyPiece.new(All_My_Pieces.sample, board)
	end

	def self.cheat_piece(board)
		MyPiece.new(Cheat_Piece, board)
	end

end

class MyBoard < Board
	# your enhancements here

	def initialize (game)
		super(game)
		@cheated = false
		@current_block = MyPiece.next_piece(self)
	end

	def next_piece
		if @cheated
			@cheated = false
			@current_block = MyPiece.cheat_piece(self)
		else
			@current_block = MyPiece.next_piece(self)
		end
		@current_pos = nil
	end

	def cheat
		if !@cheated and @score >= 100
			@score -= 100
			@cheated = true
			draw
		end
	end

	def rotate_180
		if !game_over? and @game.is_running?
			@current_block.move(0, 0, -2)
		end
		draw
	end

	def store_current
		locations = @current_block.current_rotation
		displacement = @current_block.position
		(0..(locations.length-1)).each{|index| 
			current = locations[index];
			@grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
				@current_pos[index]
		}
		remove_filled
		@delay = [@delay - 2, 80].max
	end
end
