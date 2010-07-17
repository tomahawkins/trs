module PIC.Output
  ( writeVerilogROM
  ) where

import System.IO
import Text.Printf

import PIC.Assemble
import PIC.Configuration
import Utils

-- | Writes a Verilog ROM module.  Includes a commented program listing.
--
-- > writeVerilogROM config program moduleName
writeVerilogROM :: String -> ProgramAbs -> IO ()
writeVerilogROM moduleName program@(ProgramAbs instrs) = writeFile (moduleName ++ ".v") file
  where
  file = "module " ++ moduleName ++ " ( input [" ++ show (aW - 1) ++ ":0] address , output reg [" ++ show (iW - 1) ++ ":0] instruction );\n" ++ caseStmt ++ "endmodule\n"
  caseStmt = "  always @*\n    case (address)  // synopsys full_case parallel_case\n" ++ addrCases ++ printf ("      %-" ++ show (4 + aW) ++ "s") "default" ++ " : instruction <= " ++ show iW ++ "'b101" ++ replicate (iW - 3) '0' ++ ";  //          GOTO 0\n    endcase\n"
  iW = instrWidth basicConfig
  aW = bitsRequired (instrMemoryDepth basicConfig)
  addrCases = concatMap render program2
  program1 = binMachineCode basicConfig program
  program2 = enum (zip program1 instrs)
  render (addr,(code,instr)) = printf "      %2d'b%s : instruction <= %d'b%s;  //  %6d  %s\n" aW (bin aW addr) iW code addr (show instr)

binMachineCode :: Configuration -> ProgramAbs -> [String]
binMachineCode config (ProgramAbs instrs) =
  if length instrs > instrDepth
    then error $ "Program exceeds instruction memory depth:  memory depth = " ++ show instrDepth ++ "  program length = " ++ show (length instrs)
    else map (instructionCode config) instrs ++ replicate (instrDepth - length instrs) (replicate (instrWidth config) '0')
  where
  instrDepth = instrMemoryDepth config

-- | Calculates the binary encoding of an instruction given a processor configuration.
instructionCode :: Configuration -> Instruction -> String
instructionCode config instr = case instr of
  ADDLW k      -> "111110" ++ bin kw k
  ADDWF f W    -> "000111" ++ "0" ++ bin fw f
  ADDWF f F    -> "000111" ++ "1" ++ bin fw f
  ANDLW k      -> "111001" ++ bin kw k
  ANDWF f W    -> "000101" ++ "0" ++ bin fw f
  ANDWF f F    -> "000101" ++ "1" ++ bin fw f
  ASSERT _ _   -> instructionCode config NOP
--BCF f b      -> "0100" ++ bin bw b ++ bin fwb f
  BSF f b      -> "0101" ++ bin bw b ++ bin fwb f
  BTFSC f b    -> "0110" ++ bin bw b ++ bin fwb f
  BTFSS f b    -> "0111" ++ bin bw b ++ bin fwb f
  CLRF f       -> "000001" ++ "1" ++ bin fw f
--CLRW         -> "000001" ++ "0" ++ bin fw 0
  COMF f W     -> "001001" ++ "0" ++ bin fw f
  COMF f F     -> "001001" ++ "1" ++ bin fw f
--DECF f W     -> "000011" ++ "0" ++ bin fw f
--DECF f F     -> "000011" ++ "1" ++ bin fw f
--DECFSZ f W   -> "001011" ++ "0" ++ bin fw f
--DECFSZ f F   -> "001011" ++ "1" ++ bin fw f
  GOTO k       -> "101" ++ bin (instrWidth' - 3) k
--INCF f W     -> "001010" ++ "0" ++ bin fw f
--INCF f F     -> "001010" ++ "1" ++ bin fw f
  INCFSZ f W   -> "001111" ++ "0" ++ bin fw f
  INCFSZ f F   -> "001111" ++ "1" ++ bin fw f
--IORLW k      -> "111000" ++ bin kw k
  IORWF f W    -> "000100" ++ "0" ++ bin fw f
  IORWF f F    -> "000100" ++ "1" ++ bin fw f
  MOVLW k      -> "110000" ++ bin kw k
  MOVF f W     -> "001000" ++ "0" ++ bin fw f
  MOVF f F     -> "001000" ++ "1" ++ bin fw f
  MOVWF f      -> "000000" ++ "1" ++ bin fw f
  NOP          -> fill instrWidth'
--RLF f W      -> "001101" ++ "0" ++ bin fw f
--RLF f F      -> "001101" ++ "1" ++ bin fw f
--RRF f W      -> "001100" ++ "0" ++ bin fw f
--RRF f F      -> "001100" ++ "1" ++ bin fw f
--SLEEP        -> "00000001100011" ++ fill (instrWidth' - 14)   -- XXX What if instrWidth < 14 bits?
--SUBLW k      -> "111100" ++ bin kw k
  SUBWF f W    -> "000010" ++ "0" ++ bin fw f
  SUBWF f F    -> "000010" ++ "1" ++ bin fw f
--SWAPF f W    -> "001110" ++ "0" ++ bin fw f
--SWAPF f F    -> "001110" ++ "1" ++ bin fw f
--XORLW k      -> "111010" ++ bin kw k
  XORWF f W    -> "000110" ++ "0" ++ bin fw f
  XORWF f F    -> "000110" ++ "1" ++ bin fw f

  where
  fill :: Int -> String
  fill n = replicate n '0'
  kw = instrWidth' - 6
  fw = kw - 1
  bw = bitsRequired (dataWidth config)
  fwb = instrWidth' - 4 - bw
  instrWidth' = instrWidth config

