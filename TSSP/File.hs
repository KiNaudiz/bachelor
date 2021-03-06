module File
where

import TSSP

writeWaveset :: Show a => Waveset a -> FilePath -> IO ()
writeWaveset set filename = do
        let dx = show $ wsetDx set
            dt = show $ wsetDt set
            x0 = show $ wsetX0 set
            ws = show $ wsetWaves set
            cb = dx ++ "\n" ++ dt ++ "\n" ++ x0 ++ "\n" ++ ws
        writeFile filename cb

readWaveset :: (Read a) => FilePath -> IO (Waveset a)
readWaveset filename = do
        str <- readFile filename
        let [dx',dt',x0',ws']   = lines str
            [dx,dt,x0]  = map read [dx',dt',x0']-- :: [a]
            ws          = read ws'-- :: [[a]]
        return $ Waveset ws dx dt x0

