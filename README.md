# Canonical Huffman Coding in Haskell with Lookup Table Decoding

An educational implementation of Canonical Huffman Codes in Haskell featuring optimized decoding using lookup tables, based on "Decoding of Canonical Huffman Codes with Lookup Tables" by Yakov Nekritch.

## Features

- **Canonical Huffman Encoding**: Space-efficient code representation
- **Optimized Decoding**: Lookup tables for O(1) symbol retrieval
- **Bit-level Operations**: Efficient storage and retrieval
- **Educational Design**: Clear separation of algorithm phases

## 🛠 Implementation Highlights

### Three Decoding Strategies
1. **Standard Tree Traversal** (`hmcDecode`)
2. **Lookup Table Optimization** (`hmcDecodeLookUp`)
3. **Additional Tables** for ambiguous prefixes

### Time Complexity
- **Encoding**: $O(nL)$ where $n$ is text length, $L$ is average code length
- **Standard Decoding**: $O(nL)$ 
- **Lookup Table Decoding**: $O(n)$ with $O(1)$ average case per symbol

### Space Complexity
- **Canonical Codes**: $O(k)$ vs $O(kL)$ for standard Huffman codes
- **Lookup Tables**: $O(2^{\text{lFirst}})$

### Key Components
- Min-heap based tree construction
- Canonical code reassignment
- Bit-packing for compact storage
- Adaptive lookup table generation

## Usage
```haskell
-- Basic encoding
-- Creates: file_encoded.hmc (compressed binary)
--          file_encoded_codes.txt (code table)
hmcEncode "file.txt"

-- Standard decoding with Tree Traversal
-- Creates: file_encoded_decoded.txt
hmcDecode "file_encoded.hmc" "file_encoded_codes.txt"

-- Optimized decoding with lookup tables
-- Creates: file_encoded_decoded_lookup.txt
hmcDecodeLookUp "file_encoded.hmc" "file_encoded_codes.txt"

-- View the binary representation of encoded files
viewBits "file_encoded.hmc"
```
### example outputs in `examples/` directory
### Configuration  
**`lFirst` (Lookup Table Size): **
- Determines the size of the primary lookup table (2^lFirst entries)
- Auto-calculated using `max_code_length + 2` 
- adjust manually if needed (line 552)

**`best_prefixes` (Additional Tables)**
- Handles ambiguous prefixes that don't fit in the main lookup table
- Default: Top 20 prefixes by coverage score, where Scoring is Based on prefix utility and ambiguity reduction
- adjust manually if needed (line 382)

## Recommended Resources
- Original Paper [*"A Method for the Construction of Minimum-Redundancy Codes"*](https://doi.org/10.1109/JRPROC.1952.273898) by D. A. Huffman
- [*"Decoding of Canonical Huffman Codes with Lookup Tables"*](https://doi.org/10.1109/DCC.2000.838213) by Y. Nekritch
- [*Huffman Codes: An Information Theory Perspective*](https://www.youtube.com/watch?v=B3y0RsVCyrw) by Reducible

