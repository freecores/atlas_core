-- ########################################################
-- #         << ATLAS Project - Bootloader ROM >>         #
-- # **************************************************** #
-- #  Initialized with boot loader.                       #
-- # **************************************************** #
-- #  Last modified: 17.04.2014                           #
-- # **************************************************** #
-- #  by Stephan Nolting 4788, Hanover, Germany           #
-- ########################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.atlas_core_package.all;

entity BOOT_MEM is
	port	(
				-- Host Interface --
				CLK_I           : in  std_logic; -- global clock line
				I_ADR_I         : in  std_logic_vector(15 downto 0); -- instruction adr
				I_EN_I          : in  std_logic; -- IR update
				I_DAT_O         : out std_logic_vector(15 downto 0); -- instruction out
				D_EN_I          : in  std_logic; -- access enable
				D_RW_I          : in  std_logic; -- read/write
				D_ADR_I         : in  std_logic_vector(15 downto 0); -- data adr
				D_DAT_I         : in  std_logic_vector(15 downto 0); -- data in
				D_DAT_O         : out std_logic_vector(15 downto 0)  -- data out
			);
end BOOT_MEM;

architecture BOOT_MEM_STRUCTURE of BOOT_MEM is

	-- Internal constants(configuration --
	constant mem_size_c      : natural := 2048; -- 2kB
	constant log2_mem_size_c : natural := log2(mem_size_c/2); -- address width (word boundary!)

	-- Memory Type --
	type mem_file_t is array (0 to (mem_size_c/2)-1) of std_logic_vector(15 downto 0); -- word mem!

	-- MEMORY IMAGE (Bootloader Program) --
	------------------------------------------------------
	constant BOOT_MEM_FILE_C : mem_file_t :=
    (
        000000 => x"bc0e", -- B
        000001 => x"bc04", -- B
        000002 => x"bc03", -- B
        000003 => x"bc02", -- B
        000004 => x"bc01", -- B
        000005 => x"c000", -- LDIL
        000006 => x"cc00", -- LDIH
        000007 => x"ec8a", -- MCR
        000008 => x"cc19", -- LDIH
        000009 => x"ed0f", -- MCR
        000010 => x"c512", -- LDIL
        000011 => x"c907", -- LDIH
        000012 => x"be74", -- BL
        000013 => x"bc00", -- B
        000014 => x"ec11", -- MRC
        000015 => x"ec88", -- MCR
        000016 => x"ec8a", -- MCR
        000017 => x"c380", -- LDIL
        000018 => x"cff8", -- LDIH
        000019 => x"1c07", -- STSR
        000020 => x"2800", -- CLR
        000021 => x"ec08", -- MCR
        000022 => x"ec0b", -- MCR
        000023 => x"ec0d", -- MCR
        000024 => x"ec00", -- MRC
        000025 => x"ed88", -- MCR
        000026 => x"c002", -- LDIL
        000027 => x"ed8b", -- MCR
        000028 => x"c064", -- LDIL
        000029 => x"ed8d", -- MCR
        000030 => x"c901", -- LDIH
        000031 => x"ed2f", -- MCR
        000032 => x"ec17", -- MRC
        000033 => x"ec97", -- MRC
        000034 => x"c160", -- LDIL
        000035 => x"c909", -- LDIH
        000036 => x"c18f", -- LDIL
        000037 => x"0923", -- ADD
        000038 => x"29b3", -- CLR
        000039 => x"2a44", -- CLR
        000040 => x"100a", -- SUBS
        000041 => x"149b", -- SBCS
        000042 => x"9003", -- BMI
        000043 => x"0241", -- INC
        000044 => x"bdfc", -- B
        000045 => x"ed49", -- MCR
        000046 => x"ec22", -- MRC
        000047 => x"d406", -- SBR
        000048 => x"ed0a", -- MCR
        000049 => x"c522", -- LDIL
        000050 => x"c905", -- LDIH
        000051 => x"be4d", -- BL
        000052 => x"c118", -- LDIL
        000053 => x"c906", -- LDIH
        000054 => x"be4a", -- BL
        000055 => x"ee11", -- MRC
        000056 => x"be4c", -- BL
        000057 => x"c128", -- LDIL
        000058 => x"c906", -- LDIH
        000059 => x"be45", -- BL
        000060 => x"ee97", -- MRC
        000061 => x"ee17", -- MRC
        000062 => x"be46", -- BL
        000063 => x"0250", -- MOV
        000064 => x"be44", -- BL
        000065 => x"be40", -- BL
        000066 => x"ec27", -- MRC
        000067 => x"c083", -- LDIL
        000068 => x"2001", -- AND
        000069 => x"c330", -- LDIL
        000070 => x"0b60", -- ADD
        000071 => x"bc0f", -- B
        000072 => x"c546", -- LDIL
        000073 => x"c906", -- LDIH
        000074 => x"be36", -- BL
        000075 => x"c136", -- LDIL
        000076 => x"c907", -- LDIH
        000077 => x"be33", -- BL
        000078 => x"c17c", -- LDIL
        000079 => x"c907", -- LDIH
        000080 => x"be30", -- BL
        000081 => x"be32", -- BL
        000082 => x"0300", -- MOV
        000083 => x"0080", -- MOV
        000084 => x"be2e", -- BL
        000085 => x"be2c", -- BL
        000086 => x"c0b0", -- LDIL
        000087 => x"181e", -- CMP
        000088 => x"81f0", -- BEQ
        000089 => x"c0b1", -- LDIL
        000090 => x"181e", -- CMP
        000091 => x"8085", -- BEQ
        000092 => x"c0b2", -- LDIL
        000093 => x"181e", -- CMP
        000094 => x"8052", -- BEQ
        000095 => x"c0b3", -- LDIL
        000096 => x"181e", -- CMP
        000097 => x"8019", -- BEQ
        000098 => x"c0b4", -- LDIL
        000099 => x"181e", -- CMP
        000100 => x"8021", -- BEQ
        000101 => x"c292", -- LDIL
        000102 => x"ca83", -- LDIH
        000103 => x"c0f0", -- LDIL
        000104 => x"181e", -- CMP
        000105 => x"f705", -- RBAEQ
        000106 => x"c0e4", -- LDIL
        000107 => x"181e", -- CMP
        000108 => x"80e2", -- BEQ
        000109 => x"c2c6", -- LDIL
        000110 => x"ca85", -- LDIH
        000111 => x"c0f7", -- LDIL
        000112 => x"181e", -- CMP
        000113 => x"f705", -- RBAEQ
        000114 => x"c0f2", -- LDIL
        000115 => x"181e", -- CMP
        000116 => x"85da", -- BNE
        000117 => x"2800", -- CLR
        000118 => x"c080", -- LDIL
        000119 => x"cc80", -- LDIH
        000120 => x"ec99", -- MCR
        000121 => x"3400", -- GT
        000122 => x"c138", -- LDIL
        000123 => x"c906", -- LDIH
        000124 => x"be04", -- BL
        000125 => x"2800", -- CLR
        000126 => x"2100", -- STUB
        000127 => x"bca3", -- B
        000128 => x"bc98", -- B
        000129 => x"bc98", -- B
        000130 => x"bc98", -- B
        000131 => x"bc98", -- B
        000132 => x"bc9b", -- B
        000133 => x"c516", -- LDIL
        000134 => x"c906", -- LDIH
        000135 => x"be91", -- BL
        000136 => x"be99", -- BL
        000137 => x"edca", -- MCR
        000138 => x"be97", -- BL
        000139 => x"edc9", -- MCR
        000140 => x"c034", -- LDIL
        000141 => x"c805", -- LDIH
        000142 => x"3404", -- GTL
        000143 => x"be8a", -- BL
        000144 => x"be90", -- BL
        000145 => x"c47e", -- LDIL
        000146 => x"cc4a", -- LDIH
        000147 => x"180e", -- CMP
        000148 => x"8489", -- BNE
        000149 => x"be8b", -- BL
        000150 => x"3f64", -- SFT
        000151 => x"2066", -- STUB
        000152 => x"be88", -- BL
        000153 => x"20e6", -- STUB
        000154 => x"be86", -- BL
        000155 => x"2166", -- STUB
        000156 => x"be84", -- BL
        000157 => x"21e6", -- STUB
        000158 => x"be82", -- BL
        000159 => x"2266", -- STUB
        000160 => x"be80", -- BL
        000161 => x"22e6", -- STUB
        000162 => x"be7e", -- BL
        000163 => x"2366", -- STUB
        000164 => x"c280", -- LDIL
        000165 => x"ecda", -- MCR
        000166 => x"ec5e", -- MCR
        000167 => x"be79", -- BL
        000168 => x"7f5a", -- STR
        000169 => x"ec06", -- MRC
        000170 => x"2806", -- EOR
        000171 => x"ec0e", -- MCR
        000172 => x"2400", -- LDUB
        000173 => x"1858", -- CMP
        000174 => x"85f9", -- BNE
        000175 => x"bc56", -- B
        000176 => x"c100", -- LDIL
        000177 => x"be28", -- BL
        000178 => x"c47e", -- LDIL
        000179 => x"cc4a", -- LDIH
        000180 => x"180d", -- CMP
        000181 => x"8468", -- BNE
        000182 => x"c102", -- LDIL
        000183 => x"be22", -- BL
        000184 => x"2055", -- STUB
        000185 => x"c104", -- LDIL
        000186 => x"be1f", -- BL
        000187 => x"20d5", -- STUB
        000188 => x"c106", -- LDIL
        000189 => x"be1c", -- BL
        000190 => x"2155", -- STUB
        000191 => x"c108", -- LDIL
        000192 => x"be19", -- BL
        000193 => x"21d5", -- STUB
        000194 => x"c10a", -- LDIL
        000195 => x"be16", -- BL
        000196 => x"2255", -- STUB
        000197 => x"c10c", -- LDIL
        000198 => x"be13", -- BL
        000199 => x"22d5", -- STUB
        000200 => x"c10e", -- LDIL
        000201 => x"be10", -- BL
        000202 => x"2355", -- STUB
        000203 => x"c200", -- LDIL
        000204 => x"ecca", -- MCR
        000205 => x"ec4e", -- MCR
        000206 => x"c010", -- LDIL
        000207 => x"0940", -- ADD
        000208 => x"be09", -- BL
        000209 => x"7eca", -- STR
        000210 => x"ec06", -- MRC
        000211 => x"2805", -- EOR
        000212 => x"ec0e", -- MCR
        000213 => x"2400", -- LDUB
        000214 => x"1848", -- CMP
        000215 => x"85f7", -- BNE
        000216 => x"bc2d", -- B
        000217 => x"0370", -- MOV
        000218 => x"be42", -- BL
        000219 => x"3eb0", -- SFT
        000220 => x"0121", -- INC
        000221 => x"be3f", -- BL
        000222 => x"26d3", -- ORR
        000223 => x"3460", -- RET
        000224 => x"c150", -- LDIL
        000225 => x"c906", -- LDIH
        000226 => x"be36", -- BL
        000227 => x"be38", -- BL
        000228 => x"3c80", -- SFT
        000229 => x"be36", -- BL
        000230 => x"2490", -- ORR
        000231 => x"c47e", -- LDIL
        000232 => x"cc4a", -- LDIH
        000233 => x"1818", -- CMP
        000234 => x"8433", -- BNE
        000235 => x"be27", -- BL
        000236 => x"3c94", -- SFT
        000237 => x"2011", -- STUB
        000238 => x"be24", -- BL
        000239 => x"2091", -- STUB
        000240 => x"be22", -- BL
        000241 => x"2111", -- STUB
        000242 => x"be20", -- BL
        000243 => x"2191", -- STUB
        000244 => x"be1e", -- BL
        000245 => x"2211", -- STUB
        000246 => x"be1c", -- BL
        000247 => x"2291", -- STUB
        000248 => x"be1a", -- BL
        000249 => x"2311", -- STUB
        000250 => x"2ad5", -- CLR
        000251 => x"ecda", -- MCR
        000252 => x"ec5e", -- MCR
        000253 => x"be15", -- BL
        000254 => x"7cda", -- STR
        000255 => x"ec06", -- MRC
        000256 => x"2801", -- EOR
        000257 => x"ec0e", -- MCR
        000258 => x"2400", -- LDUB
        000259 => x"1858", -- CMP
        000260 => x"85f9", -- BNE
        000261 => x"ec11", -- MRC
        000262 => x"ec8a", -- MCR
        000263 => x"c174", -- LDIL
        000264 => x"c906", -- LDIH
        000265 => x"be0f", -- BL
        000266 => x"ec06", -- MRC
        000267 => x"2491", -- LDUB
        000268 => x"1809", -- CMP
        000269 => x"8015", -- BEQ
        000270 => x"c520", -- LDIL
        000271 => x"c907", -- LDIH
        000272 => x"be08", -- BL
        000273 => x"bcc9", -- B
        000274 => x"0370", -- MOV
        000275 => x"be08", -- BL
        000276 => x"3c80", -- SFT
        000277 => x"be06", -- BL
        000278 => x"2490", -- ORR
        000279 => x"3460", -- RET
        000280 => x"bcc5", -- B
        000281 => x"bcce", -- B
        000282 => x"bcd2", -- B
        000283 => x"bcd6", -- B
        000284 => x"bc6b", -- B
        000285 => x"bcba", -- B
        000286 => x"bd30", -- B
        000287 => x"bc69", -- B
        000288 => x"bcbc", -- B
        000289 => x"bcd5", -- B
        000290 => x"c164", -- LDIL
        000291 => x"c906", -- LDIH
        000292 => x"beb9", -- BL
        000293 => x"24aa", -- LDUBS
        000294 => x"8016", -- BEQ
        000295 => x"c0a2", -- LDIL
        000296 => x"bec4", -- BL
        000297 => x"24a2", -- LDUB
        000298 => x"be1e", -- BL
        000299 => x"24b3", -- LDUB
        000300 => x"be1c", -- BL
        000301 => x"24c4", -- LDUB
        000302 => x"be1a", -- BL
        000303 => x"24d5", -- LDUB
        000304 => x"be18", -- BL
        000305 => x"24e6", -- LDUB
        000306 => x"be16", -- BL
        000307 => x"c0a2", -- LDIL
        000308 => x"beb8", -- BL
        000309 => x"beb2", -- BL
        000310 => x"c534", -- LDIL
        000311 => x"c906", -- LDIH
        000312 => x"bea5", -- BL
        000313 => x"ee06", -- MRC
        000314 => x"bee1", -- BL
        000315 => x"beac", -- BL
        000316 => x"beab", -- BL
        000317 => x"c080", -- LDIL
        000318 => x"ccc0", -- LDIH
        000319 => x"1c01", -- STSR
        000320 => x"2800", -- CLR
        000321 => x"ed0f", -- MCR
        000322 => x"ec88", -- MCR
        000323 => x"ec8b", -- MCR
        000324 => x"ec8c", -- MCR
        000325 => x"ec8a", -- MCR
        000326 => x"ec89", -- MCR
        000327 => x"3400", -- GT
        000328 => x"0370", -- MOV
        000329 => x"3c90", -- SFT
        000330 => x"bea2", -- BL
        000331 => x"3c90", -- SFT
        000332 => x"bea0", -- BL
        000333 => x"3460", -- RET
        000334 => x"c508", -- LDIL
        000335 => x"c906", -- LDIH
        000336 => x"be8d", -- BL
        000337 => x"bea5", -- BL
        000338 => x"c134", -- LDIL
        000339 => x"c905", -- LDIH
        000340 => x"3424", -- GTL
        000341 => x"ecca", -- MCR
        000342 => x"be91", -- BL
        000343 => x"c280", -- LDIL
        000344 => x"c00f", -- LDIL
        000345 => x"2058", -- ANDS
        000346 => x"840a", -- BNE
        000347 => x"be8c", -- BL
        000348 => x"c0a4", -- LDIL
        000349 => x"be8f", -- BL
        000350 => x"0250", -- MOV
        000351 => x"bebc", -- BL
        000352 => x"c0ba", -- LDIL
        000353 => x"be8b", -- BL
        000354 => x"c0a0", -- LDIL
        000355 => x"be89", -- BL
        000356 => x"7a5a", -- LDR
        000357 => x"c0a0", -- LDIL
        000358 => x"be86", -- BL
        000359 => x"beb4", -- BL
        000360 => x"c00f", -- LDIL
        000361 => x"2058", -- ANDS
        000362 => x"8414", -- BNE
        000363 => x"c0a0", -- LDIL
        000364 => x"be80", -- BL
        000365 => x"be7f", -- BL
        000366 => x"c010", -- LDIL
        000367 => x"1250", -- SUB
        000368 => x"c470", -- LDIL
        000369 => x"2240", -- AND
        000370 => x"c12e", -- LDIL
        000371 => x"78c9", -- LDR
        000372 => x"3c90", -- SFT
        000373 => x"c880", -- LDIH
        000374 => x"c020", -- LDIL
        000375 => x"1818", -- CMP
        000376 => x"f8c2", -- MVHI
        000377 => x"be73", -- BL
        000378 => x"c08f", -- LDIL
        000379 => x"2014", -- AND
        000380 => x"3409", -- TEQ
        000381 => x"85f6", -- BNE
        000382 => x"ec20", -- MRC
        000383 => x"dc0f", -- STB
        000384 => x"b804", -- BTS
        000385 => x"c5fe", -- LDIL
        000386 => x"343d", -- TEQ
        000387 => x"85d5", -- BNE
        000388 => x"be6d", -- BL
        000389 => x"2800", -- CLR
        000390 => x"3400", -- GT
        000391 => x"bc54", -- B
        000392 => x"bc93", -- B
        000393 => x"c001", -- LDIL
        000394 => x"ed0c", -- MCR
        000395 => x"c050", -- LDIL
        000396 => x"c83f", -- LDIH
        000397 => x"ed0a", -- MCR
        000398 => x"c000", -- LDIL
        000399 => x"c801", -- LDIH
        000400 => x"bea9", -- BL
        000401 => x"c142", -- LDIL
        000402 => x"c906", -- LDIH
        000403 => x"be4a", -- BL
        000404 => x"c150", -- LDIL
        000405 => x"c906", -- LDIH
        000406 => x"be47", -- BL
        000407 => x"be5a", -- BL
        000408 => x"3c80", -- SFT
        000409 => x"be58", -- BL
        000410 => x"2410", -- ORR
        000411 => x"c4fe", -- LDIL
        000412 => x"ccca", -- LDIH
        000413 => x"1809", -- CMP
        000414 => x"8439", -- BNE
        000415 => x"c100", -- LDIL
        000416 => x"0290", -- MOV
        000417 => x"be2f", -- BL
        000418 => x"be4f", -- BL
        000419 => x"3c80", -- SFT
        000420 => x"be4d", -- BL
        000421 => x"2690", -- ORR
        000422 => x"3ed4", -- SFT
        000423 => x"2055", -- STUB
        000424 => x"c102", -- LDIL
        000425 => x"be27", -- BL
        000426 => x"be47", -- BL
        000427 => x"3c80", -- SFT
        000428 => x"be45", -- BL
        000429 => x"2690", -- ORR
        000430 => x"20d5", -- STUB
        000431 => x"c104", -- LDIL
        000432 => x"be20", -- BL
        000433 => x"c106", -- LDIL
        000434 => x"be3f", -- BL
        000435 => x"0180", -- MOV
        000436 => x"be8b", -- BL
        000437 => x"0121", -- INC
        000438 => x"c010", -- LDIL
        000439 => x"1828", -- CMP
        000440 => x"85fa", -- BNE
        000441 => x"2ad5", -- CLR
        000442 => x"be37", -- BL
        000443 => x"0180", -- MOV
        000444 => x"be83", -- BL
        000445 => x"0121", -- INC
        000446 => x"2400", -- LDUB
        000447 => x"02d1", -- INC
        000448 => x"1858", -- CMP
        000449 => x"85f9", -- BNE
        000450 => x"c001", -- LDIL
        000451 => x"ed0c", -- MCR
        000452 => x"c050", -- LDIL
        000453 => x"c83f", -- LDIH
        000454 => x"ed0a", -- MCR
        000455 => x"c00c", -- LDIL
        000456 => x"c801", -- LDIH
        000457 => x"be70", -- BL
        000458 => x"c174", -- LDIL
        000459 => x"c906", -- LDIH
        000460 => x"be11", -- BL
        000461 => x"c690", -- LDIL
        000462 => x"ca80", -- LDIH
        000463 => x"3450", -- GT
        000464 => x"0370", -- MOV
        000465 => x"3dd0", -- SFT
        000466 => x"be6d", -- BL
        000467 => x"0121", -- INC
        000468 => x"01d0", -- MOV
        000469 => x"be6a", -- BL
        000470 => x"3460", -- RET
        000471 => x"c504", -- LDIL
        000472 => x"c907", -- LDIH
        000473 => x"be04", -- BL
        000474 => x"bcba", -- B
        000475 => x"bc94", -- B
        000476 => x"bca5", -- B
        000477 => x"01f0", -- MOV
        000478 => x"7829", -- LDR
        000479 => x"c080", -- LDIL
        000480 => x"ccff", -- LDIH
        000481 => x"2081", -- AND
        000482 => x"3c98", -- SFTS
        000483 => x"8003", -- BEQ
        000484 => x"be08", -- BL
        000485 => x"bdf9", -- B
        000486 => x"3430", -- RET
        000487 => x"0170", -- MOV
        000488 => x"c08d", -- LDIL
        000489 => x"be03", -- BL
        000490 => x"c08a", -- LDIL
        000491 => x"03a0", -- MOV
        000492 => x"ec22", -- MRC
        000493 => x"dc05", -- STB
        000494 => x"b9fe", -- BTS
        000495 => x"ed18", -- MCR
        000496 => x"3470", -- RET
        000497 => x"ec20", -- MRC
        000498 => x"dc8f", -- STBI
        000499 => x"b9fe", -- BTS
        000500 => x"c800", -- LDIH
        000501 => x"3470", -- RET
        000502 => x"0170", -- MOV
        000503 => x"c200", -- LDIL
        000504 => x"c184", -- LDIL
        000505 => x"bff8", -- BL
        000506 => x"c0c6", -- LDIL
        000507 => x"1809", -- CMP
        000508 => x"9003", -- BMI
        000509 => x"c0a0", -- LDIL
        000510 => x"1001", -- SUB
        000511 => x"c0b0", -- LDIL
        000512 => x"1809", -- CMP
        000513 => x"91f8", -- BMI
        000514 => x"c0c6", -- LDIL
        000515 => x"1818", -- CMP
        000516 => x"91f5", -- BMI
        000517 => x"c0b9", -- LDIL
        000518 => x"1818", -- CMP
        000519 => x"a404", -- BLS
        000520 => x"c0c1", -- LDIL
        000521 => x"1809", -- CMP
        000522 => x"a1ef", -- BHI
        000523 => x"0080", -- MOV
        000524 => x"bfe0", -- BL
        000525 => x"c030", -- LDIL
        000526 => x"1090", -- SUB
        000527 => x"c009", -- LDIL
        000528 => x"1809", -- CMP
        000529 => x"a402", -- BLS
        000530 => x"0497", -- DEC
        000531 => x"3e42", -- SFT
        000532 => x"3e42", -- SFT
        000533 => x"3e42", -- SFT
        000534 => x"3e42", -- SFT
        000535 => x"2641", -- ORR
        000536 => x"05b9", -- DECS
        000537 => x"85e0", -- BNE
        000538 => x"3420", -- RET
        000539 => x"0370", -- MOV
        000540 => x"3d42", -- SFT
        000541 => x"3d22", -- SFT
        000542 => x"3d22", -- SFT
        000543 => x"3d22", -- SFT
        000544 => x"be0f", -- BL
        000545 => x"bfcb", -- BL
        000546 => x"3d40", -- SFT
        000547 => x"be0c", -- BL
        000548 => x"bfc8", -- BL
        000549 => x"3d45", -- SFT
        000550 => x"3d25", -- SFT
        000551 => x"3d25", -- SFT
        000552 => x"3d25", -- SFT
        000553 => x"be06", -- BL
        000554 => x"bfc2", -- BL
        000555 => x"0140", -- MOV
        000556 => x"be03", -- BL
        000557 => x"bfbf", -- BL
        000558 => x"3460", -- RET
        000559 => x"c08f", -- LDIL
        000560 => x"2121", -- AND
        000561 => x"c089", -- LDIL
        000562 => x"181a", -- CMP
        000563 => x"8803", -- BCS
        000564 => x"c0b0", -- LDIL
        000565 => x"bc02", -- B
        000566 => x"c0b7", -- LDIL
        000567 => x"0892", -- ADD
        000568 => x"3470", -- RET
        000569 => x"ed0b", -- MCR
        000570 => x"ec22", -- MRC
        000571 => x"dc03", -- STB
        000572 => x"b9fe", -- BTS
        000573 => x"ec23", -- MRC
        000574 => x"3470", -- RET
        000575 => x"00f0", -- MOV
        000576 => x"c050", -- LDIL
        000577 => x"c837", -- LDIH
        000578 => x"ed0a", -- MCR
        000579 => x"c001", -- LDIL
        000580 => x"ed0c", -- MCR
        000581 => x"c006", -- LDIL
        000582 => x"bff3", -- BL
        000583 => x"c050", -- LDIL
        000584 => x"c83f", -- LDIH
        000585 => x"ed0a", -- MCR
        000586 => x"c000", -- LDIL
        000587 => x"c805", -- LDIH
        000588 => x"bfed", -- BL
        000589 => x"dc01", -- STB
        000590 => x"b805", -- BTS
        000591 => x"c530", -- LDIL
        000592 => x"c907", -- LDIH
        000593 => x"bf8c", -- BL
        000594 => x"bc42", -- B
        000595 => x"c040", -- LDIL
        000596 => x"c83f", -- LDIH
        000597 => x"ed0a", -- MCR
        000598 => x"c001", -- LDIL
        000599 => x"ed0c", -- MCR
        000600 => x"3c20", -- SFT
        000601 => x"c802", -- LDIH
        000602 => x"bfdf", -- BL
        000603 => x"03a0", -- MOV
        000604 => x"cb80", -- LDIH
        000605 => x"3ff0", -- SFT
        000606 => x"0030", -- MOV
        000607 => x"c800", -- LDIH
        000608 => x"2407", -- ORR
        000609 => x"bfd8", -- BL
        000610 => x"2800", -- CLR
        000611 => x"ed0c", -- MCR
        000612 => x"c050", -- LDIL
        000613 => x"c83f", -- LDIH
        000614 => x"ed0a", -- MCR
        000615 => x"c001", -- LDIL
        000616 => x"ed0c", -- MCR
        000617 => x"c000", -- LDIL
        000618 => x"c805", -- LDIH
        000619 => x"bfce", -- BL
        000620 => x"dc00", -- STB
        000621 => x"b9fc", -- BTS
        000622 => x"3410", -- RET
        000623 => x"00f0", -- MOV
        000624 => x"c040", -- LDIL
        000625 => x"c83f", -- LDIH
        000626 => x"ed0a", -- MCR
        000627 => x"c001", -- LDIL
        000628 => x"ed0c", -- MCR
        000629 => x"3c20", -- SFT
        000630 => x"c803", -- LDIH
        000631 => x"bfc2", -- BL
        000632 => x"0020", -- MOV
        000633 => x"c800", -- LDIH
        000634 => x"3c00", -- SFT
        000635 => x"bfbe", -- BL
        000636 => x"29b3", -- CLR
        000637 => x"ed3c", -- MCR
        000638 => x"0180", -- MOV
        000639 => x"c980", -- LDIH
        000640 => x"3410", -- RET
        000641 => x"e5b0", -- CDP
        000642 => x"ec30", -- MRC
        000643 => x"dc06", -- STB
        000644 => x"b9fe", -- BTS
        000645 => x"c306", -- LDIL
        000646 => x"200e", -- ANDS
        000647 => x"840a", -- BNE
        000648 => x"ecb1", -- MRC
        000649 => x"ef32", -- MRC
        000650 => x"2800", -- CLR
        000651 => x"009a", -- INCS
        000652 => x"0f60", -- ADC
        000653 => x"ed99", -- MCR
        000654 => x"edea", -- MCR
        000655 => x"ef34", -- MRC
        000656 => x"3470", -- RET
        000657 => x"c542", -- LDIL
        000658 => x"c907", -- LDIH
        000659 => x"bf4a", -- BL
        000660 => x"c550", -- LDIL
        000661 => x"c907", -- LDIH
        000662 => x"bf47", -- BL
        000663 => x"bf5a", -- BL
        000664 => x"2800", -- CLR
        000665 => x"3400", -- GT
        000666 => x"0170", -- MOV
        000667 => x"bf56", -- BL
        000668 => x"c08d", -- LDIL
        000669 => x"1809", -- CMP
        000670 => x"f702", -- RBAEQ
        000671 => x"c088", -- LDIL
        000672 => x"1809", -- CMP
        000673 => x"802c", -- BEQ
        000674 => x"bdf9", -- B
        000675 => x"c516", -- LDIL
        000676 => x"c906", -- LDIH
        000677 => x"bf38", -- BL
        000678 => x"bf50", -- BL
        000679 => x"edca", -- MCR
        000680 => x"bf4e", -- BL
        000681 => x"edc9", -- MCR
        000682 => x"bff0", -- BL
        000683 => x"bf3c", -- BL
        000684 => x"c524", -- LDIL
        000685 => x"c906", -- LDIH
        000686 => x"bf2f", -- BL
        000687 => x"bf47", -- BL
        000688 => x"02c0", -- MOV
        000689 => x"bfe9", -- BL
        000690 => x"345d", -- TEQ
        000691 => x"801a", -- BEQ
        000692 => x"06d1", -- DEC
        000693 => x"bf32", -- BL
        000694 => x"bfcb", -- BL
        000695 => x"c540", -- LDIL
        000696 => x"c906", -- LDIH
        000697 => x"bf24", -- BL
        000698 => x"0260", -- MOV
        000699 => x"bf60", -- BL
        000700 => x"c320", -- LDIL
        000701 => x"c1ae", -- LDIL
        000702 => x"00e0", -- MOV
        000703 => x"bf2d", -- BL
        000704 => x"3cc0", -- SFT
        000705 => x"c880", -- LDIH
        000706 => x"181e", -- CMP
        000707 => x"f8c3", -- MVHI
        000708 => x"bf28", -- BL
        000709 => x"00c0", -- MOV
        000710 => x"c880", -- LDIH
        000711 => x"181e", -- CMP
        000712 => x"f8c3", -- MVHI
        000713 => x"bf23", -- BL
        000714 => x"eca0", -- MRC
        000715 => x"dc9f", -- STBI
        000716 => x"b9e6", -- BTS
        000717 => x"bf1a", -- BL
        000718 => x"c69c", -- LDIL
        000719 => x"ca80", -- LDIH
        000720 => x"3450", -- GT
        000721 => x"0d0a", -- .DW
        000722 => x"0d0a", -- .DW
        000723 => x"4174", -- .DW
        000724 => x"6c61", -- .DW
        000725 => x"732d", -- .DW
        000726 => x"324b", -- .DW
        000727 => x"2042", -- .DW
        000728 => x"6f6f", -- .DW
        000729 => x"746c", -- .DW
        000730 => x"6f61", -- .DW
        000731 => x"6465", -- .DW
        000732 => x"7220", -- .DW
        000733 => x"2d20", -- .DW
        000734 => x"5632", -- .DW
        000735 => x"3031", -- .DW
        000736 => x"3430", -- .DW
        000737 => x"3431", -- .DW
        000738 => x"370d", -- .DW
        000739 => x"0a62", -- .DW
        000740 => x"7920", -- .DW
        000741 => x"5374", -- .DW
        000742 => x"6570", -- .DW
        000743 => x"6861", -- .DW
        000744 => x"6e20", -- .DW
        000745 => x"4e6f", -- .DW
        000746 => x"6c74", -- .DW
        000747 => x"696e", -- .DW
        000748 => x"672c", -- .DW
        000749 => x"2073", -- .DW
        000750 => x"746e", -- .DW
        000751 => x"6f6c", -- .DW
        000752 => x"7469", -- .DW
        000753 => x"6e67", -- .DW
        000754 => x"4067", -- .DW
        000755 => x"6d61", -- .DW
        000756 => x"696c", -- .DW
        000757 => x"2e63", -- .DW
        000758 => x"6f6d", -- .DW
        000759 => x"0d0a", -- .DW
        000760 => x"7777", -- .DW
        000761 => x"772e", -- .DW
        000762 => x"6f70", -- .DW
        000763 => x"656e", -- .DW
        000764 => x"636f", -- .DW
        000765 => x"7265", -- .DW
        000766 => x"732e", -- .DW
        000767 => x"6f72", -- .DW
        000768 => x"672f", -- .DW
        000769 => x"7072", -- .DW
        000770 => x"6f6a", -- .DW
        000771 => x"6563", -- .DW
        000772 => x"742c", -- .DW
        000773 => x"6174", -- .DW
        000774 => x"6c61", -- .DW
        000775 => x"735f", -- .DW
        000776 => x"636f", -- .DW
        000777 => x"7265", -- .DW
        000778 => x"0d0a", -- .DW
        000779 => x"0000", -- .DW
        000780 => x"0d0a", -- .DW
        000781 => x"426f", -- .DW
        000782 => x"6f74", -- .DW
        000783 => x"2070", -- .DW
        000784 => x"6167", -- .DW
        000785 => x"653a", -- .DW
        000786 => x"2030", -- .DW
        000787 => x"7800", -- .DW
        000788 => x"0d0a", -- .DW
        000789 => x"436c", -- .DW
        000790 => x"6f63", -- .DW
        000791 => x"6b28", -- .DW
        000792 => x"487a", -- .DW
        000793 => x"293a", -- .DW
        000794 => x"2030", -- .DW
        000795 => x"7800", -- .DW
        000796 => x"426f", -- .DW
        000797 => x"6f74", -- .DW
        000798 => x"696e", -- .DW
        000799 => x"670d", -- .DW
        000800 => x"0a00", -- .DW
        000801 => x"4275", -- .DW
        000802 => x"726e", -- .DW
        000803 => x"2045", -- .DW
        000804 => x"4550", -- .DW
        000805 => x"524f", -- .DW
        000806 => x"4d0d", -- .DW
        000807 => x"0a00", -- .DW
        000808 => x"4177", -- .DW
        000809 => x"6169", -- .DW
        000810 => x"7469", -- .DW
        000811 => x"6e67", -- .DW
        000812 => x"2064", -- .DW
        000813 => x"6174", -- .DW
        000814 => x"612e", -- .DW
        000815 => x"2e2e", -- .DW
        000816 => x"0d0a", -- .DW
        000817 => x"0000", -- .DW
        000818 => x"5374", -- .DW
        000819 => x"6172", -- .DW
        000820 => x"7469", -- .DW
        000821 => x"6e67", -- .DW
        000822 => x"2069", -- .DW
        000823 => x"6d61", -- .DW
        000824 => x"6765", -- .DW
        000825 => x"2000", -- .DW
        000826 => x"446f", -- .DW
        000827 => x"776e", -- .DW
        000828 => x"6c6f", -- .DW
        000829 => x"6164", -- .DW
        000830 => x"2063", -- .DW
        000831 => x"6f6d", -- .DW
        000832 => x"706c", -- .DW
        000833 => x"6574", -- .DW
        000834 => x"650d", -- .DW
        000835 => x"0a00", -- .DW
        000836 => x"5061", -- .DW
        000837 => x"6765", -- .DW
        000838 => x"2028", -- .DW
        000839 => x"3468", -- .DW
        000840 => x"293a", -- .DW
        000841 => x"2024", -- .DW
        000842 => x"0000", -- .DW
        000843 => x"4164", -- .DW
        000844 => x"6472", -- .DW
        000845 => x"2028", -- .DW
        000846 => x"3868", -- .DW
        000847 => x"293a", -- .DW
        000848 => x"2024", -- .DW
        000849 => x"0000", -- .DW
        000850 => x"2377", -- .DW
        000851 => x"6f72", -- .DW
        000852 => x"6473", -- .DW
        000853 => x"2028", -- .DW
        000854 => x"3468", -- .DW
        000855 => x"293a", -- .DW
        000856 => x"2024", -- .DW
        000857 => x"0000", -- .DW
        000858 => x"4368", -- .DW
        000859 => x"6563", -- .DW
        000860 => x"6b73", -- .DW
        000861 => x"756d", -- .DW
        000862 => x"3a20", -- .DW
        000863 => x"2400", -- .DW
        000864 => x"202d", -- .DW
        000865 => x"3e20", -- .DW
        000866 => x"2400", -- .DW
        000867 => x"0d0a", -- .DW
        000868 => x"636d", -- .DW
        000869 => x"642f", -- .DW
        000870 => x"626f", -- .DW
        000871 => x"6f74", -- .DW
        000872 => x"2d73", -- .DW
        000873 => x"7769", -- .DW
        000874 => x"7463", -- .DW
        000875 => x"683a", -- .DW
        000876 => x"0d0a", -- .DW
        000877 => x"2030", -- .DW
        000878 => x"2f27", -- .DW
        000879 => x"3030", -- .DW
        000880 => x"273a", -- .DW
        000881 => x"2052", -- .DW
        000882 => x"6573", -- .DW
        000883 => x"7461", -- .DW
        000884 => x"7274", -- .DW
        000885 => x"2063", -- .DW
        000886 => x"6f6e", -- .DW
        000887 => x"736f", -- .DW
        000888 => x"6c65", -- .DW
        000889 => x"0d0a", -- .DW
        000890 => x"2031", -- .DW
        000891 => x"2f27", -- .DW
        000892 => x"3031", -- .DW
        000893 => x"273a", -- .DW
        000894 => x"2042", -- .DW
        000895 => x"6f6f", -- .DW
        000896 => x"7420", -- .DW
        000897 => x"5541", -- .DW
        000898 => x"5254", -- .DW
        000899 => x"0d0a", -- .DW
        000900 => x"2032", -- .DW
        000901 => x"2f27", -- .DW
        000902 => x"3130", -- .DW
        000903 => x"273a", -- .DW
        000904 => x"2042", -- .DW
        000905 => x"6f6f", -- .DW
        000906 => x"7420", -- .DW
        000907 => x"4545", -- .DW
        000908 => x"5052", -- .DW
        000909 => x"4f4d", -- .DW
        000910 => x"0d0a", -- .DW
        000911 => x"2033", -- .DW
        000912 => x"2f27", -- .DW
        000913 => x"3131", -- .DW
        000914 => x"273a", -- .DW
        000915 => x"2042", -- .DW
        000916 => x"6f6f", -- .DW
        000917 => x"7420", -- .DW
        000918 => x"6d65", -- .DW
        000919 => x"6d6f", -- .DW
        000920 => x"7279", -- .DW
        000921 => x"0d0a", -- .DW
        000922 => x"0000", -- .DW
        000923 => x"2034", -- .DW
        000924 => x"3a20", -- .DW
        000925 => x"426f", -- .DW
        000926 => x"6f74", -- .DW
        000927 => x"2057", -- .DW
        000928 => x"420d", -- .DW
        000929 => x"0a20", -- .DW
        000930 => x"703a", -- .DW
        000931 => x"2042", -- .DW
        000932 => x"7572", -- .DW
        000933 => x"6e20", -- .DW
        000934 => x"4545", -- .DW
        000935 => x"5052", -- .DW
        000936 => x"4f4d", -- .DW
        000937 => x"0d0a", -- .DW
        000938 => x"2064", -- .DW
        000939 => x"3a20", -- .DW
        000940 => x"5241", -- .DW
        000941 => x"4d20", -- .DW
        000942 => x"6475", -- .DW
        000943 => x"6d70", -- .DW
        000944 => x"0d0a", -- .DW
        000945 => x"2072", -- .DW
        000946 => x"3a20", -- .DW
        000947 => x"5265", -- .DW
        000948 => x"7365", -- .DW
        000949 => x"740d", -- .DW
        000950 => x"0a20", -- .DW
        000951 => x"773a", -- .DW
        000952 => x"2057", -- .DW
        000953 => x"4220", -- .DW
        000954 => x"6475", -- .DW
        000955 => x"6d70", -- .DW
        000956 => x"0d0a", -- .DW
        000957 => x"0000", -- .DW
        000958 => x"636d", -- .DW
        000959 => x"643a", -- .DW
        000960 => x"3e20", -- .DW
        000961 => x"0000", -- .DW
        000962 => x"494d", -- .DW
        000963 => x"4147", -- .DW
        000964 => x"4520", -- .DW
        000965 => x"4552", -- .DW
        000966 => x"5221", -- .DW
        000967 => x"0d0a", -- .DW
        000968 => x"0000", -- .DW
        000969 => x"0d0a", -- .DW
        000970 => x"4952", -- .DW
        000971 => x"5120", -- .DW
        000972 => x"4552", -- .DW
        000973 => x"5221", -- .DW
        000974 => x"0d0a", -- .DW
        000975 => x"0000", -- .DW
        000976 => x"4348", -- .DW
        000977 => x"4543", -- .DW
        000978 => x"4b53", -- .DW
        000979 => x"554d", -- .DW
        000980 => x"2045", -- .DW
        000981 => x"5252", -- .DW
        000982 => x"210d", -- .DW
        000983 => x"0a00", -- .DW
        000984 => x"5350", -- .DW
        000985 => x"492f", -- .DW
        000986 => x"4545", -- .DW
        000987 => x"5052", -- .DW
        000988 => x"4f4d", -- .DW
        000989 => x"2045", -- .DW
        000990 => x"5252", -- .DW
        000991 => x"210d", -- .DW
        000992 => x"0a00", -- .DW
        000993 => x"5742", -- .DW
        000994 => x"2042", -- .DW
        000995 => x"5553", -- .DW
        000996 => x"2045", -- .DW
        000997 => x"5252", -- .DW
        000998 => x"210d", -- .DW
        000999 => x"0a00", -- .DW
        001000 => x"5072", -- .DW
        001001 => x"6573", -- .DW
        001002 => x"7320", -- .DW
        001003 => x"616e", -- .DW
        001004 => x"7920", -- .DW
        001005 => x"6b65", -- .DW
        001006 => x"790d", -- .DW
        001007 => x"0a00", -- .DW
        others => x"0000"  -- NOP
	);
	------------------------------------------------------

begin

	-- Memory Access ---------------------------------------------------------------------------------------
	-- --------------------------------------------------------------------------------------------------------
		MEM_FILE_ACCESS: process(CLK_I)
		begin
			if rising_edge(CLK_I) then
				-- Data Read --
				if (D_EN_I = '1') then -- valid access
					if (word_mode_en_c = true) then -- read data access
						D_DAT_O <= BOOT_MEM_FILE_C(to_integer(unsigned(D_ADR_I(log2_mem_size_c-1 downto 0))));
					else
						D_DAT_O <= BOOT_MEM_FILE_C(to_integer(unsigned(D_ADR_I(log2_mem_size_c downto 1))));
					end if;
				end if;
				-- Instruction Read --
				if (I_EN_I = '1') then
					if (word_mode_en_c = true) then
						I_DAT_O <= BOOT_MEM_FILE_C(to_integer(unsigned(I_ADR_I(log2_mem_size_c-1 downto 0))));
					else
						I_DAT_O <= BOOT_MEM_FILE_C(to_integer(unsigned(I_ADR_I(log2_mem_size_c downto 1))));
					end if;
				end if;
			end if;
		end process MEM_FILE_ACCESS;



end BOOT_MEM_STRUCTURE;
