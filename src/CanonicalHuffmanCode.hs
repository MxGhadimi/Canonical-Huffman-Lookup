{-# LANGUAGE BangPatterns #-}

module CanonicalHuffmanCode where

import Data.List (sortOn, foldl', find)
import Data.Text as Text (unpack, pack)
import Data.Text.IO as TextIO (readFile, writeFile)
import Data.Map as Map (Map, insertWith, toList, lookup, union, singleton, empty, insert)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Bits (shiftL, (.|.), shiftR, setBit, testBit)
import qualified Data.ByteString as BString (ByteString, pack, writeFile, length, unpack, readFile, index, empty, reverse, cons, drop, take, foldl')
import System.IO (readFile) 
import Data.Word (Word8)
import Data.Array (Array, array, listArray, (!), bounds, elems)
import Data.Ord (Down(..))

-- !Types
type Code = BString.ByteString
type CanonicalMap = Map Char Code

-- ! Min Heap
-- nodes store a priority, value, and left/right subtrees
data MinHeap a = Empty | HNode Int a (MinHeap a) (MinHeap a)

insertNode :: Int -> a -> MinHeap a -> MinHeap a
insertNode priority value heap = merge (HNode priority value Empty Empty) heap

extractMin :: MinHeap a -> Maybe (a, MinHeap a)
extractMin Empty = Nothing
extractMin (HNode _ value left_child right_child) = Just (value, merge left_child right_child)

merge :: MinHeap a -> MinHeap a -> MinHeap a
merge Empty h2 = h2
merge h1 Empty = h1
merge h1@(HNode p1 v1 l1 r1) h2@(HNode p2 v2 l2 r2)
    | p1 <= p2  = HNode p1 v1 (merge r1 h2) l1
    | otherwise = HNode p2 v2 (merge r2 h1) l2

-- ! Huffman Tree
-- leafs store characters and their frequencies, internal nodes store frequency and left/right children
data HuffTree =
    Leaf { character :: !Char, frequency :: !Int }
  | Node { frequency :: !Int, lchild :: !HuffTree, rchild :: !HuffTree }
  deriving (Show, Eq)

emptyHeap :: MinHeap HuffTree
emptyHeap = Empty

-- again using min-heap
buildHuffmanTree :: Map Char Int -> HuffTree
buildHuffmanTree frequency_map =
    let leaves       = map (\(char, freq) -> Leaf char freq) (toList frequency_map)
        initial_heap = foldl (\heap leaf -> insertNode (frequency leaf) leaf heap) emptyHeap leaves
    in buildTree initial_heap
  where
    buildTree :: MinHeap HuffTree -> HuffTree
    buildTree heap =
        case extractMin heap of
            Nothing             -> error "heap is empty"
            Just (tree1, heap1) ->
                case extractMin heap1 of
                    Nothing             -> tree1 -- one tree left
                    Just (tree2, heap2) ->
                        let freq' = frequency tree1 + frequency tree2
                            tree' = Node freq' tree1 tree2 -- combine two trees
                        in buildTree (insertNode freq' tree' heap2)

generateCodes :: HuffTree -> Map Char BString.ByteString
generateCodes tree = go tree BString.empty
  where
    go (Leaf char _) path       = Map.singleton char (BString.reverse path)
    go (Node _ left right) path = Map.union (go left (BString.cons 0 path))
                                            (go right (BString.cons 1 path))

-- hmc to canonical hmc
-- first code of length L = (first code of length L-1 + count of codes of length L-1) << 1
toCanonicalHuffman :: Map Char BString.ByteString -> Map Char BString.ByteString
toCanonicalHuffman standardHMC_codes =
    let lengths      = Map.map BString.length standardHMC_codes
        sorted_chars = sortOn (\(char, len) -> (len, char)) (Map.toList lengths)
    in
    case sorted_chars of
        []            -> Map.empty
        [(char, _)] -> Map.singleton char (BString.pack [0])  -- just one '0' bit
        _             -> let (_, _, codeMap) = foldl' assignCode (0, -1, Map.empty) sorted_chars
                         in codeMap
    where
        assignCode (!prevCode, !prevLen, !codeMap) (char, len) =
            let code = if len == prevLen 
                    then prevCode + 1
                    else (prevCode + 1) `shiftL` (len - prevLen)
                binaryCode = toBinaryByteString code len
            in (code, len, Map.insert char binaryCode codeMap)
        
        toBinaryByteString :: Int -> Int -> BString.ByteString
        toBinaryByteString n len = BString.pack [if testBit n (len - i - 1) then 1 else 0 | i <- [0..len-1]]



-- ! Decode Tree
-- using trie tree
data DecodeTree  = 
    DNode (Maybe Char) DecodeTree DecodeTree
  | EmptyDNode

emptyDNode :: DecodeTree
emptyDNode = EmptyDNode

buildDecodeTree :: Map String Char -> DecodeTree
buildDecodeTree codes = foldl' insertCode emptyDNode (Map.toList codes)
  where
    insertCode tree (code, char) = insertCodePath tree code char
    
    insertCodePath :: DecodeTree -> String -> Char -> DecodeTree
    insertCodePath tree "" char = 
        case tree of
            DNode _ left_child right_child -> DNode (Just char) left_child right_child
            EmptyDNode -> DNode (Just char) EmptyDNode EmptyDNode
    
    insertCodePath tree (bit:bits) char =
        case tree of
            DNode m left_child right_child ->
                case bit of
                    '0' -> DNode m (insertCodePath left_child bits char) right_child
                    '1' -> DNode m left_child (insertCodePath right_child bits char)
                    _   -> error "Invalid bit character" -- shouldn't happen
            EmptyDNode ->
                case bit of
                    '0' -> DNode Nothing (insertCodePath EmptyDNode bits char) EmptyDNode
                    '1' -> DNode Nothing EmptyDNode (insertCodePath EmptyDNode bits char)
                    _   -> error "Invalid bit character"

decodeWithDTree :: DecodeTree -> String -> String
decodeWithDTree tree bits = go tree bits []
    where
        go current [] result = 
            case current of
                DNode (Just char) _ _ -> reverse (char:result)
                _ -> reverse result
        
        go current (b:bs) result =
            case current of
                DNode (Just char) _ _ -> go tree (b:bs) (char:result)
                
                DNode Nothing left_child right_child     ->
                    case b of
                        '0' -> go left_child bs result
                        '1' -> go right_child bs result
                        _   -> error "Invalid bit character"
                EmptyDNode                   -> error "Decoding failed - reached empty node"

-- ! Bit/ByteString conversion
bitsToByteString :: String -> BString.ByteString
bitsToByteString bit_string = 
    let bytes = packBits bit_string
    in BString.pack bytes
    where
        packBits :: String -> [Word8]
        packBits [] = []
        packBits bits = 
            let (byte_bits, rest) = splitAt 8 bits
                actual_bits       = if Prelude.length byte_bits < 8 then byte_bits ++ replicate (8 - Prelude.length byte_bits) '0' else byte_bits
                byte              = foldl' (\acc (i, bit) -> if bit == '1' then acc `setBit` (7 - i) else acc) (0 :: Word8) (zip [0..7] actual_bits)
            in byte : packBits rest

byteStringToBits ::BString.ByteString -> String
byteStringToBits bs = concatMap byteToBits (BString.unpack bs)
    where
        byteToBits w = [if testBit w (7-i) then '1' else '0' | i <- [0..7]]

stringToCode :: String -> BString.ByteString
stringToCode str = BString.pack (map charToBit str)
    where
        charToBit '0' = 0
        charToBit '1' = 1
        charToBit _   = error "Invalid bit character"

bitStringToInt :: String -> Int
bitStringToInt bits = foldl' (\acc bit -> (acc `shiftL` 1) + if bit == '1' then 1 else 0) 0 bits

intToBitString :: Int -> Int -> String
intToBitString value len = [if testBit value (len - i - 1) then '1' else '0' | i <- [0..len-1]]


extractBitChunk :: BString.ByteString -> Int -> Int -> (Int, Int)
extractBitChunk bs offset count =
    let available = BString.length bs * 8 - offset
        to_read   = min count available
        value     = extractBitsDirect bs offset to_read
    in (value, to_read)

extractBitsDirect :: BString.ByteString -> Int -> Int -> Int
extractBitsDirect bs startPos count =
    foldl' (\acc i -> 
        let bit_pos = startPos + i
            byte_index = bit_pos `div` 8
            bit_byte = 7 - (bit_pos `mod` 8)
            byte = BString.index bs byte_index
            bit = if testBit byte bit_byte then 1 else 0
        in (acc `shiftL` 1) .|. bit
    ) 0 [0..count-1]


-- ! Encoding/Decoding
hmcEncode :: FilePath -> IO ()
hmcEncode input_file = do
    text <- TextIO.readFile input_file
    let unpacked           = Text.unpack text
        frequency_map      = foldl' (\acc char -> Map.insertWith (+) char 1 acc) Map.empty unpacked
        huffman_tree       = buildHuffmanTree frequency_map
        codes              = generateCodes huffman_tree
        canonical_codes    = toCanonicalHuffman codes
        bit_string         = concatMap (\char ->     -- Convert to bit string using Byte string codes directly
            let code = fromMaybe (error $ "Character not in code map: " ++ show char) (Map.lookup char canonical_codes)
            in codeToBitString code) unpacked
    
        original_bit_len   = Prelude.length bit_string
        byte_string        = bitsToByteString bit_string
    
    BString.writeFile (getFileName input_file ++ "_encoded.hmc") byte_string
    
    let codes_ser = Map.toList $ Map.map codeToBitString canonical_codes
    TextIO.writeFile (getFileName input_file ++ "_encoded_codes.txt") (Text.pack $ show (codes_ser, original_bit_len))
    
    putStrLn $ "Input file: " ++ input_file
    putStrLn $ "Output file: " ++ getFileName input_file ++ "_encoded.hmc"
    where

        codeToBitString :: BString.ByteString -> String
        codeToBitString bs = map (\byte -> if byte == 0 then '0' else '1') (BString.unpack bs)

hmcDecode :: FilePath -> FilePath -> IO ()
hmcDecode encoded_file code_table_file = do
    byteString     <- BString.readFile encoded_file
    code_table_str <- System.IO.readFile code_table_file
    -- frequency map
    let (canonical_codes_list, original_bit_len) = read code_table_str :: ([(Char, String)], Int)
        canonical_codes   = Map.fromList [(char, pack code) | (char, code) <- canonical_codes_list] -- convert back to ByteString codes
        reverse_map       = Map.fromList [(unpack code, char) | (char, code) <- Map.toList canonical_codes]
        bit_string        = byteStringToBits byteString
        actual_bit_string = take original_bit_len bit_string
        decode_tree       = buildDecodeTree reverse_map
        decoded           = decodeWithDTree decode_tree actual_bit_string

    Prelude.writeFile (getFileName encoded_file ++ "_decoded.txt") decoded
    putStrLn $ "Decoded file: " ++ getFileName encoded_file ++ "_decoded.txt"


-- ! Decoding using (Decoding of Canonical Huan Codes with Lookp Tables By Yakov Nekritch) Method
type AdditionalTables = Map Int AdditionalTableData
type LookupTable = Array Int DecodeAction

data CanonicalCode = CanonicalCode {
    max_len       :: Int,
    first_codes   :: Array Int Int,
    first_i       :: Array Int Int,
    codes_len     :: Array Int Int,
    symbol_map    :: Array Int Char
}

data DecodeAction = 
      ShortCode [Char] Int
    | SameLength Int Int [Char]
    | AdditionalTable Int Int [Char]
    | LongCode Int Int [Char]
    deriving Show

data AdditionalTableData = AdditionalTableData {
    at_table_bits :: Int,
    at_symbols :: Array Int Char
} deriving (Show)

-- hmc to Canonical Code
buildCanonicalCode :: Map Char BString.ByteString -> CanonicalCode
buildCanonicalCode code_map = 
    let sorted_codes    = sortOn (\(_, code) -> (BString.length code, bsToInt code)) (Map.toList code_map) -- sort by code length, then by code value
        build_max_len         = maximum (map (BString.length . snd) sorted_codes)
        total_symbols   = Prelude.length sorted_codes
        first_code_list = [ (len, fst (head codesForLen)) | len <- [1..build_max_len], let codesForLen = [(bsToInt code, char) | (char, code) <- sorted_codes, BString.length code == len], not (null codesForLen) ]
        
        first_indices_list = go 1 0 []
            where
                go len idx acc
                    | len > build_max_len = reverse acc
                    | otherwise =
                        let count = Prelude.length [() | (_, code) <- sorted_codes, BString.length code == len]
                        in if count > 0
                            then go (len + 1) (idx + count) ((len, idx):acc)
                            else go (len + 1) idx ((len, idx):acc) -- -1 = no codes
        
        first_codes_array   = array (1, build_max_len) [(i, lookupDefault (-1) i first_code_list) | i <- [1..build_max_len]]
        first_indices_array = array (1, build_max_len) [(i, lookupDefault 0 i first_indices_list) | i <- [1..build_max_len]]
        code_lengths_array  = listArray (0, total_symbols - 1) [BString.length code | (_, code) <- sorted_codes]
        symbol_map_arr        = listArray (0, total_symbols - 1) [char | (char, _) <- sorted_codes]
        
    in CanonicalCode build_max_len first_codes_array first_indices_array code_lengths_array symbol_map_arr
    where
        lookupDefault def key pairs = fromMaybe def (Prelude.lookup key pairs)

        bsToInt :: BString.ByteString -> Int
        bsToInt bs = BString.foldl' (\acc byte -> (acc `shiftL` 1) + fromIntegral byte) 0 bs

extractCompleteCodewords :: CanonicalCode -> String -> ([Char], Int, String)
extractCompleteCodewords canon bits = go bits [] 0
    where
        go remaining acc total_used =
            case parseOneCodeword canon remaining of
                Just (symbol, len, rest) -> go rest (symbol:acc) (total_used + len)
                Nothing                  -> (reverse acc, total_used, remaining)

parseOneCodeword :: CanonicalCode -> String -> Maybe (Char, Int, String)
parseOneCodeword canon bits =
    findValidLength 1
    where
        maxCheck = min (max_len canon) (Prelude.length bits)
        
        findValidLength len
            | len > maxCheck = Nothing
            | otherwise =
                let code_bits = take len bits
                    code_val  = bitStringToInt code_bits
                in case decodeWithLength canon code_val len of
                    Just symbol -> Just (symbol, len, drop len bits)
                    Nothing     -> findValidLength (len + 1)

decodeWithLength :: CanonicalCode -> Int -> Int -> Maybe Char
decodeWithLength canon value len =
    if len < 1 || len > max_len canon 
    then Nothing
    else
        let first_code = first_codes canon ! len
        in if first_code == -1  -- doesn't exist
           then Nothing
           else
            let codeCount = countCodesOfLength canon len
                in if value < first_code || value >= first_code + codeCount
                   then Nothing
                   else
                    let first_index = first_i canon ! len
                        index = first_index + (value - first_code)
                    in if index >= 0 && index <= snd (bounds (symbol_map canon))
                       then Just (symbol_map canon ! index)
                       else Nothing

getPossibleLengths :: CanonicalCode -> String -> [Int]
getPossibleLengths canon prefix =
    let prefix_val = bitStringToInt prefix
        prefix_len = Prelude.length prefix
    in 
    [len | len <- [prefix_len..max_len canon], len <= max_len canon, isValidPrefix canon prefix_val prefix_len len]

isValidPrefix :: CanonicalCode -> Int -> Int -> Int -> Bool
isValidPrefix canon prefix_val prefix_len total_len =
    if total_len > max_len canon || total_len < prefix_len then
        False
    else
        let first_code = first_codes canon ! total_len
        in if first_code == -1  -- No codes of this length
           then False
           else
            let codeCount      = countCodesOfLength canon total_len
                lastCode       = first_code + codeCount - 1
                shifted_prefix = prefix_val `shiftL` (total_len - prefix_len)
                maxShifted     = shifted_prefix + ((1 `shiftL` (total_len - prefix_len)) - 1)
            in shifted_prefix  <= lastCode && maxShifted >= first_code

countCodesOfLength :: CanonicalCode -> Int -> Int
countCodesOfLength canon len =
    let next_index  = if len < max_len canon then first_i canon ! (len + 1) 
                      else snd (bounds (symbol_map canon)) + 1
        first_index = first_i canon ! len
    in next_index - first_index

buildAllAdditionalTables :: CanonicalCode -> AdditionalTables
buildAllAdditionalTables canon =
    let maxprefix_len = min 8 (max_len canon - 1)
        allprefixes   = generateAllPrefixes maxprefix_len
        
        scored_prefixes = map (\prefix -> 
            let possible_lens = getPossibleLengths canon prefix
                score         = calculatePrefix canon prefix possible_lens
            in (prefix, score)) allprefixes
        
        
        best_prefixes = take 20 $ map fst $ sortOn (Down . snd) $ filter ((> 0) . snd) scored_prefixes
    in foldl' (buildTableForPrefix canon) Map.empty best_prefixes
    where
        generateAllPrefixes :: Int -> [String]
        generateAllPrefixes generate_max_len = [intToBitString i len | len <- [1..generate_max_len], i <- [0..(1 `shiftL` len) - 1]]
        
        calculatePrefix :: CanonicalCode -> String -> [Int] -> Int
        calculatePrefix canon_calc prefix possible_lens =
            let prefix_len     = Prelude.length prefix
                max_extra_bits = maximum (map (\len -> len - prefix_len) possible_lens)
                valid_comp     = countValidCompletions canon_calc prefix
                total_comps    = 1 `shiftL` max_extra_bits
                coverage       = valid_comp * 100 `div` total_comps
            in
            if Prelude.length possible_lens > 1 && max_extra_bits >= 2 && max_extra_bits <= 6 then
                coverage + (Prelude.length possible_lens * 10)
            else
                0

        buildTableForPrefix :: CanonicalCode -> AdditionalTables -> String -> AdditionalTables
        buildTableForPrefix canon_calc tables prefix =
            let possible_lens = getPossibleLengths canon_calc prefix
            in
            if shouldBuildAdditionalTable prefix possible_lens then
                case buildAdditionalTable canon_calc prefix of
                    Just table -> Map.insert (bitStringToInt prefix) table tables
                    Nothing    -> tables
            else
                tables

buildAdditionalTable :: CanonicalCode -> String -> Maybe AdditionalTableData
buildAdditionalTable canon prefix =
    let prefix_val       = bitStringToInt prefix
        prefix_len       = Prelude.length prefix
        possible_lens    = getPossibleLengths canon prefix
        extra_bits       = maximum (map (\len -> len - prefix_len) possible_lens)
        table_size       = 1 `shiftL` extra_bits
        symbol_arr       = listArray (0, table_size - 1) [ lookupSymbol prefix_val prefix_len extra_bits completion_bits | completion_bits <- [0..table_size - 1] ]
        valid_symbol_num = Prelude.length (filter (/= '?') (elems symbol_arr))
    in

    if valid_symbol_num > 0 && valid_symbol_num >= table_size `div` 2 then
        Just $ AdditionalTableData extra_bits symbol_arr
    else
        Nothing
    where
        lookupSymbol :: Int -> Int -> Int -> Int -> Char
        lookupSymbol prefix_val prefix_len extra_bits completion_bits =
            let total_len = prefix_len + extra_bits
                full_val = (prefix_val `shiftL` extra_bits) .|. completion_bits
            in
            case decodeWithLength canon full_val total_len of
                Just symbol -> symbol
                Nothing     -> 
                    case find (\len -> 
                        let extra_bits_count = len - prefix_len
                        in extra_bits_count <= extra_bits &&  decodeWithLength canon ((prefix_val `shiftL` extra_bits_count) .|. completion_bits) len /= Nothing) (getPossibleLengths canon prefix) of
                            Just len -> 
                                let extra_bits_count   = len - prefix_len
                                    adjusted_comp = completion_bits `shiftR` (extra_bits - extra_bits_count)
                                in fromMaybe '?' (decodeWithLength canon ((prefix_val `shiftL` extra_bits_count) .|. adjusted_comp) len)
                            Nothing  -> '?'  -- No valid completion

-- lookup table decoding
analyzeBitString :: CanonicalCode -> AdditionalTables -> Int -> Int -> DecodeAction
analyzeBitString canon additional_t lFirst bit_val =
    let bit_str                         = intToBitString bit_val lFirst
        (symbols, bits_used, remaining) = extractCompleteCodewords canon bit_str
    in
    case remaining of
        "" -> ShortCode symbols bits_used
        prefix 
            | not (null symbols) -> ShortCode symbols bits_used
            | otherwise        -> handleIncompletePrefix canon additional_t lFirst bit_val prefix symbols

handleIncompletePrefix :: CanonicalCode -> AdditionalTables -> Int -> Int -> String -> [Char] -> DecodeAction
handleIncompletePrefix canon additional_t lFirst bit_val prefix symbols =
    let prefix_val = bitStringToInt prefix
        prefix_len = Prelude.length prefix
        possible_lens = getPossibleLengths canon prefix
    in
    if null possible_lens then
        ShortCode symbols (lFirst - 1) 
    else if Prelude.length possible_lens == 1 then
        let total_len   = head possible_lens
            extra_bits  = total_len - prefix_len
            first_index = getFirstIndexForPrefix canon prefix
        in SameLength extra_bits first_index symbols
    else
        case Map.lookup prefix_val additional_t of
            Just table_data -> 
                let extra_bits = at_table_bits table_data
                in AdditionalTable extra_bits prefix_val symbols
            Nothing         ->
                let extra_bits = max_len canon - prefix_len
                in LongCode extra_bits bit_val symbols

actionHandler :: CanonicalCode -> AdditionalTables -> DecodeAction -> BString.ByteString -> Int -> [Char] -> Int -> ([Char], Int)
actionHandler canon additional_t action bs offset result lFirst =
    case action of
        ShortCode symbols bitsUsed                     -> (foldl' (flip (:)) result symbols, bitsUsed)
        
        SameLength extra_bits first_index symbols      ->
            let (extra_vits_val, _) = extractBitChunk bs (offset + lFirst) extra_bits
                symbol_index = first_index + extra_vits_val
                symbol      = symbol_map canon ! symbol_index
                total_bits  = lFirst + extra_bits
            in (symbol : foldl' (flip (:)) result symbols, total_bits)
        
        AdditionalTable extra_bits table_index symbols ->
            case Map.lookup table_index additional_t of
                Just table_data ->
                    let (extra_vits_val, actual_read) = extractBitChunk bs (offset + lFirst) extra_bits
                        symbol                        = at_symbols table_data ! extra_vits_val
                        total_bits                    = lFirst + actual_read
                    in if symbol /= '?'
                       then (symbol : foldl' (flip (:)) result symbols, total_bits)
                       else handleFallback canon bs offset result lFirst symbols  -- invalid symbol
                Nothing         -> handleFallback canon bs offset result lFirst symbols  -- table not found
        
        LongCode extra_bits stored_table_bits symbols  ->
            let (long_bits_val, num_read) = extractBitChunk bs (offset + lFirst) extra_bits
                total_len     = findLengthByComparison canon (combineBits stored_table_bits lFirst long_bits_val extra_bits) 
                shiftedBits  = combineBits stored_table_bits lFirst long_bits_val extra_bits `shiftR` (max_len canon - total_len)
                firstCode    = first_codes canon ! total_len
                first_index     = first_i canon ! total_len
                index        = first_index + (shiftedBits - firstCode)
                symbol       = symbol_map canon ! index
                total_bits   = lFirst + num_read
            in (symbol : foldl' (flip (:)) result symbols, total_bits)
    where
        combineBits :: Int -> Int -> Int -> Int -> Int
        combineBits prefix _ bits' bits'Len = (prefix `shiftL` bits'Len) .|. bits'

        findLengthByComparison :: CanonicalCode -> Int -> Int
        findLengthByComparison canon_length bits =
            go (max_len canon_length)
            where
            go len
                | len <= 0 = 1
                | (bits `shiftR` (max_len canon_length - len)) >= (first_codes canon_length ! len) = len
                | otherwise = go (len - 1)

        -- ifAdditionalTable failures
        handleFallback :: CanonicalCode -> BString.ByteString -> Int -> [Char] -> Int -> [Char] -> ([Char], Int)
        handleFallback canon_fb bs_fb offset_fb result_fb lFirst_fb symbols =
            let fallback_bits     = min 8 (lFirst_fb - 1)
                (fallback_val, _) = extractBitChunk bs_fb offset_fb fallback_bits
                fallback_str      = intToBitString fallback_val fallback_bits
            in
            case parseOneCodeword canon_fb fallback_str of
                Just (symbol, len, _) -> (symbol : foldl' (flip (:)) result_fb symbols, len)
                Nothing               -> (foldl' (flip (:)) result_fb symbols, 1) -- one bit and continue

-- returns main & additional tables
buildLookupTable :: CanonicalCode -> Int -> (LookupTable, AdditionalTables)
buildLookupTable canon lFirst =
    let table_size        = 1 `shiftL` lFirst
        additionalTables = buildAllAdditionalTables canon
        mainTable        = array (0, table_size - 1) [ (i, analyzeBitString canon additionalTables lFirst i) | i <- [0..table_size - 1] ]
    in (mainTable, additionalTables)

hmcDecodeLookUp :: FilePath -> FilePath -> IO ()
hmcDecodeLookUp encoded_file code_table_file = do
    byte_string <- BString.readFile encoded_file
    code_table_str <- Text.unpack <$> TextIO.readFile code_table_file
    let (canonical_codes_list, original_bit_len) = read code_table_str :: ([(Char, String)], Int)
        code_lengths    = map (Prelude.length . snd) canonical_codes_list
        max_code_length = maximum code_lengths
        total_codes     = Prelude.length code_lengths
        lFirst          = max_code_length + 2
    
    putStrLn $ "Selected lFirst: " ++ show lFirst ++ " bits"
    
    let can_fit = filter (\(_, code) -> Prelude.length code <= lFirst) canonical_codes_list
    putStrLn $ "Codes that fit in lookup table: " ++ show (Prelude.length can_fit) ++ "/" ++ show total_codes
    
    let canonical_codes       = Map.fromList [(char, stringToCode code) | (char, code) <- canonical_codes_list]
        canon                 = buildCanonicalCode canonical_codes
        (table, additional_t) = buildLookupTable canon lFirst
        result                = decodeWithTable byte_string canon table additional_t lFirst original_bit_len
        output_file           = getFileName encoded_file ++ "_decoded_lookup.txt"
    
    putStrLn $ "Additional tables built: " ++ show (Map.size additional_t)
        
    Prelude.writeFile output_file result
    putStrLn $ "Decoded file: " ++ output_file
    putStrLn $ "Decoded text length: " ++ show (Prelude.length result)

decodeWithTable :: BString.ByteString -> CanonicalCode -> LookupTable -> AdditionalTables -> Int -> Int -> String
decodeWithTable bs canon table additional_t lFirst original_bit_len =
    go bs (0 :: Int) [] (0 :: Int)
  where
    total_bits = BString.length bs * 8
    
    go :: BString.ByteString -> Int -> [Char] -> Int -> String
    go !bs_g !bit_offset !result !iterations
        | bit_offset >= original_bit_len = reverse result
        | bit_offset >= total_bits = reverse result  -- Safety check
        | otherwise =
            let (current_bits, _) = extractBitChunk bs_g bit_offset lFirst
                action                    = table ! current_bits
                (result', bits_processed) = actionHandler canon additional_t action bs_g bit_offset result lFirst
                offset'                   = bit_offset + bits_processed
            in 
            if offset' > total_bits then
                reverse result
            else
                go bs_g offset' result' (iterations + 1)

--  should we build additional table for this prefix?
shouldBuildAdditionalTable :: String -> [Int] -> Bool
shouldBuildAdditionalTable prefix possible_lens =
    let prefix_len     = Prelude.length prefix
        max_extra_bits = maximum (map (\len -> len - prefix_len) possible_lens)
        min_extra_bits = minimum (map (\len -> len - prefix_len) possible_lens)
    in
    Prelude.length possible_lens > 1 && max_extra_bits <= 3

-- Get the first symbol index for codewords with a given prefix
getFirstIndexForPrefix :: CanonicalCode -> String -> Int
getFirstIndexForPrefix canon prefix =
    let prefix_val    = bitStringToInt prefix
        prefix_len    = Prelude.length prefix
        possible_lens = getPossibleLengths canon prefix
    in
    case possible_lens of
        [] -> -1  -- No valid lengths
        (total_len:_) ->  -- Use the first (shortest) possible length
            let first_code     = first_codes canon ! total_len
                first_index    = first_i canon ! total_len
                shifted_prefix = prefix_val `shiftL` (total_len - prefix_len)
            in 
            if first_code == -1 then -1
            else first_index + (shifted_prefix - first_code)

countValidCompletions :: CanonicalCode -> String -> Int
countValidCompletions canon prefix =
    let prefix_val = bitStringToInt prefix
        prefix_len = Prelude.length prefix
        possible_lens = getPossibleLengths canon prefix
    in
    sum [Prelude.length [() | completion_bits <- [0..(1 `shiftL` (total_len - prefix_len)) - 1], isValidCompletion canon prefix_val prefix_len total_len completion_bits] | total_len <- possible_lens ]
    where
        isValidCompletion :: CanonicalCode -> Int -> Int -> Int -> Int -> Bool
        isValidCompletion canon_comp prefix_val prefix_len total_len completion_bits =
            let full_code_val = (prefix_val `shiftL` (total_len - prefix_len)) .|. completion_bits
            in isJust (decodeWithLength canon_comp full_code_val total_len)


-- ! utils
getFileName :: FilePath -> FilePath
getFileName = takeWhile (/= '.')

viewBits :: FilePath -> IO ()
viewBits encoded_file = do
    bytes <- BString.readFile encoded_file
    let byte_list = BString.unpack bytes
    
    putStrLn "Complete bit sequence:"
    let all_bits = concatMap byteToBits byte_list
    putStrLn all_bits
    where        
        byteToBits :: Word8 -> String
        byteToBits w = [if testBit w (7-i) then '1' else '0' | i <- [0..7]]
