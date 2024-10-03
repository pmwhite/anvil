module Point = struct
  module V4 = struct
    type t =
      { w : int
      ; x : int
      ; y : int
      ; z : int
      }
  end

  module V3 = struct
    module V2 = struct
      type t =
        { w : int
        ; x : int
        ; y : int
        ; z : int
        }
    end

    module V1 = struct
      type t =
        { x : int
        ; y : int
        ; z : int
        }
    end

    type t =
      | V1 of V1.t
      | V2 of V2.t
  end

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
  module V5 = struct
    type t =
      { position : Point.V4.t
      ; health : int
      ; name : string
      }
  end

  module V4 = struct
    type t =
      { position : Point.V3.t
      ; health : int
      ; name : string
      }
  end

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

module Tree = struct
  module V2 = struct
    type t =
      | Birch
      | Oak
      | Elm
      | Maple
  end

  module V1 = struct
    type t =
      | Birch
      | Oak
      | Elm
  end
end

module Plant = struct
  module V2 = struct
    type t =
      | Tree of Tree.V2.t
      | Rock
  end

  module V1 = struct
    type t =
      | Tree of Tree.V1.t
      | Rock
  end
end

module World = struct
  module V5 = struct
    module V2 = struct
      type t =
        { players : Player.V5.t list
        ; health : int
        }
    end

    module V1 = struct
      type t =
        { players : Player.V4.t list
        ; health : int
        }
    end

    type t =
      | V1 of V1.t
      | V2 of V2.t
  end

  module V4 = struct
    type t =
      { players : Player.V4.t list
      ; health : int
      }
  end

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
