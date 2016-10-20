measure vlen :: Vector a -> Int

assume length :: x:Vector a -> { v:Int | v=vlen x}

assume (!) :: x:Vector a -> {v:Nat|v<vlen x} -> a
