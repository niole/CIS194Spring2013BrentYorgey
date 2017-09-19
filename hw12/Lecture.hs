instance Monad [] where
    return x = [x]
      xs >>= k = concat (map k xs)

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) = ma >>= \a -> sequence mas >>= \as -> return (a:as)
