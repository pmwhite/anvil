module Point = struct
  module V2 = struct
    type t = 
      { x : int
      ; y : int
      ; z : int
      }
  end

  module V1 = struct
    type t = 
      { x : int
      ; y : int
      }
  end
end

module Player = struct
  module V3 = struct
    type t = 
      { position : Point.V2.t
      ; health : int
      ; name : string
      }
  end

  module V2 = struct
    type t = 
      { position : Point.V2.t
      ; health : int
      }
  end

  module V1 = struct
    type t = 
      { position : Point.V1.t
      ; health : int
      }
  end
end

module World = struct
  module V3 = struct
    type t = 
      { players : Player.V3.t list
      ; health : int
      }
  end

  module V2 = struct
    type t = 
      { players : Player.V2.t list
      ; health : int
      }
  end

  module V1 = struct
    type t = 
      { players : Player.V1.t list
      ; health : int
      }
  end
end