module File
where

import CNTypes
import CN
import Vector

writeWaveset :: Show a => Waveset a -> FilePath -> IO ()
writeWaveset set filename = do
        let dx = show $ wsetDx set
            dt = show $ wsetDt set
            x0 = show $ wsetX0 set
            ws = show $ map fillVec $ wsetWaves set
            cb = dx ++ "\n" ++ dt ++ "\n" ++ x0 ++ "\n" ++ ws
        writeFile filename cb

readWaveset :: (Read a) => FilePath -> IO (Waveset a)
readWaveset filename = do
        str <- readFile filename
        let [dx',dt',x0',ws']   = lines str
            [dx,dt,x0]  = map read [dx',dt',x0']-- :: [a]
            ws          = read ws'-- :: [[a]]
        return $ Waveset (map vecList ws) dx dt x0

-- wanted format: t x Re(psi) Im(psi)
writeWavesetGnuplot :: (Show a) => Waveset a -> FilePath -> IO String
writeWavesetGnuplot filename = do
        -- let ws = show $ map
        -- return str
        undefined
