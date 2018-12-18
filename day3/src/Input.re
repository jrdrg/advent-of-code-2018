let input =
  "#1 @ 37,526: 17x23\n#2 @ 75,649: 23x11\n#3 @ 138,364: 29x12\n#4 @ 370,260: 20x17\n#5 @ 345,512: 17x22\n#6 @ 798,255: 11x21\n#7 @ 65,658: 27x12\n#8 @ 561,59: 11x24\n#9 @ 676,491: 25x24\n#10 @ 466,216: 12x14\n#11 @ 244,867: 21x22\n#12 @ 540,331: 24x15\n#13 @ 588,819: 21x16\n#14 @ 449,749: 11x11\n#15 @ 718,975: 22x24\n#16 @ 634,684: 3x4\n#17 @ 762,66: 14x22\n#18 @ 890,30: 28x12\n#19 @ 765,380: 29x13\n#20 @ 362,91: 12x20\n#21 @ 409,438: 25x14\n#22 @ 951,842: 13x26\n#23 @ 561,384: 28x26\n#24 @ 231,112: 16x13\n#25 @ 295,64: 18x11\n#26 @ 559,254: 12x27\n#27 @ 542,298: 19x22\n#28 @ 380,923: 20x11\n#29 @ 639,811: 28x26\n#30 @ 355,781: 18x15\n#31 @ 346,401: 18x27\n#32 @ 863,904: 22x20\n#33 @ 674,53: 28x23\n#34 @ 547,932: 21x27\n#35 @ 330,763: 29x14\n#36 @ 9,26: 25x12\n#37 @ 736,934: 12x29\n#38 @ 199,569: 12x26\n#39 @ 238,881: 15x21\n#40 @ 285,409: 28x21\n#41 @ 889,332: 27x28\n#42 @ 491,332: 16x19\n#43 @ 804,806: 19x22\n#44 @ 400,539: 19x11\n#45 @ 858,364: 22x18\n#46 @ 781,391: 29x12\n#47 @ 775,65: 28x24\n#48 @ 142,303: 24x15\n#49 @ 176,82: 23x14\n#50 @ 919,370: 28x11\n#51 @ 533,62: 21x10\n#52 @ 787,796: 18x20\n#53 @ 663,127: 23x29\n#54 @ 685,477: 26x29\n#55 @ 915,940: 28x13\n#56 @ 271,34: 14x16\n#57 @ 335,111: 26x17\n#58 @ 69,221: 12x14\n#59 @ 561,67: 29x28\n#60 @ 562,559: 27x19\n#61 @ 78,194: 27x24\n#62 @ 597,392: 14x22\n#63 @ 727,201: 19x29\n#64 @ 795,818: 18x10\n#65 @ 323,772: 13x14\n#66 @ 522,267: 15x23\n#67 @ 583,373: 20x20\n#68 @ 22,955: 27x11\n#69 @ 958,782: 18x16\n#70 @ 288,650: 17x25\n#71 @ 244,459: 28x10\n#72 @ 337,36: 27x28\n#73 @ 384,95: 19x11\n#74 @ 510,205: 13x21\n#75 @ 778,577: 18x22\n#76 @ 263,407: 11x26\n#77 @ 50,388: 25x26\n#78 @ 224,546: 18x27\n#79 @ 31,556: 13x26\n#80 @ 834,320: 22x29\n#81 @ 797,292: 12x15\n#82 @ 34,29: 23x11\n#83 @ 529,324: 14x29\n#84 @ 492,346: 19x16\n#85 @ 271,427: 10x29\n#86 @ 411,804: 12x13\n#87 @ 158,518: 21x15\n#88 @ 904,628: 22x26\n#89 @ 268,958: 15x10\n#90 @ 198,555: 21x11\n#91 @ 922,868: 18x25\n#92 @ 684,127: 14x12\n#93 @ 738,110: 19x16\n#94 @ 545,449: 24x19\n#95 @ 437,264: 17x22\n#96 @ 207,572: 14x28\n#97 @ 850,622: 24x19\n#98 @ 315,329: 27x10\n#99 @ 233,436: 15x29\n#100 @ 817,217: 26x28\n#101 @ 90,221: 19x15\n#102 @ 267,846: 29x27\n#103 @ 855,222: 12x21\n#104 @ 516,777: 23x16\n#105 @ 397,676: 23x12\n#106 @ 664,117: 29x20\n#107 @ 22,395: 13x26\n#108 @ 900,693: 26x16\n#109 @ 134,396: 18x19\n#110 @ 837,369: 28x18\n#111 @ 512,448: 14x21\n#112 @ 745,542: 18x15\n#113 @ 801,269: 10x24\n#114 @ 774,874: 28x29\n#115 @ 275,286: 11x25\n#116 @ 674,726: 15x13\n#117 @ 461,205: 18x17\n#118 @ 909,91: 24x13\n#119 @ 49,287: 12x14\n#120 @ 784,46: 15x11\n#121 @ 381,834: 16x22\n#122 @ 79,910: 29x24\n#123 @ 840,147: 20x13\n#124 @ 54,353: 11x21\n#125 @ 133,398: 23x18\n#126 @ 384,783: 13x11\n#127 @ 833,886: 25x28\n#128 @ 891,30: 27x25\n#129 @ 741,302: 26x12\n#130 @ 246,767: 10x18\n#131 @ 635,252: 11x21\n#132 @ 337,671: 12x12\n#133 @ 529,563: 8x15\n#134 @ 338,575: 29x17\n#135 @ 27,31: 17x10\n#136 @ 436,912: 20x27\n#137 @ 877,709: 24x25\n#138 @ 741,122: 27x16\n#139 @ 784,134: 21x21\n#140 @ 256,837: 21x21\n#141 @ 569,959: 27x28\n#142 @ 94,163: 12x10\n#143 @ 434,25: 12x12\n#144 @ 467,375: 3x16\n#145 @ 554,946: 12x27\n#146 @ 594,345: 15x27\n#147 @ 579,196: 28x27\n#148 @ 746,688: 23x28\n#149 @ 420,724: 15x18\n#150 @ 372,674: 28x12\n#151 @ 647,862: 22x25\n#152 @ 700,47: 26x16\n#153 @ 762,568: 26x18\n#154 @ 960,139: 15x28\n#155 @ 762,524: 22x27\n#156 @ 3,31: 29x13\n#157 @ 278,843: 24x12\n#158 @ 312,868: 13x22\n#159 @ 393,682: 4x6\n#160 @ 440,47: 11x21\n#161 @ 798,263: 13x10\n#162 @ 650,819: 20x22\n#163 @ 385,569: 29x19\n#164 @ 305,464: 17x22\n#165 @ 452,719: 12x19\n#166 @ 373,92: 20x13\n#167 @ 937,502: 17x29\n#168 @ 636,185: 23x10\n#169 @ 708,21: 22x29\n#170 @ 434,412: 15x10\n#171 @ 848,599: 23x13\n#172 @ 540,948: 12x19\n#173 @ 781,553: 14x24\n#174 @ 660,237: 19x24\n#175 @ 914,69: 15x27\n#176 @ 133,481: 20x20\n#177 @ 26,681: 15x14\n#178 @ 219,800: 29x23\n#179 @ 712,201: 11x29\n#180 @ 487,879: 22x13\n#181 @ 57,557: 26x15\n#182 @ 915,404: 18x12\n#183 @ 154,358: 13x17\n#184 @ 196,193: 27x23\n#185 @ 488,630: 10x17\n#186 @ 658,614: 13x23\n#187 @ 76,170: 26x27\n#188 @ 549,454: 14x10\n#189 @ 779,751: 25x20\n#190 @ 234,766: 13x29\n#191 @ 713,964: 29x11\n#192 @ 695,679: 13x15\n#193 @ 888,651: 19x11\n#194 @ 513,195: 11x24\n#195 @ 402,685: 21x4\n#196 @ 891,270: 28x12\n#197 @ 851,72: 13x25\n#198 @ 591,788: 10x14\n#199 @ 861,638: 13x15\n#200 @ 13,33: 25x23\n#201 @ 386,621: 16x12\n#202 @ 260,437: 29x29\n#203 @ 264,223: 14x18\n#204 @ 398,432: 26x27\n#205 @ 364,304: 23x23\n#206 @ 68,763: 18x15\n#207 @ 662,833: 19x24\n#208 @ 587,923: 26x22\n#209 @ 983,663: 11x18\n#210 @ 1,176: 24x21\n#211 @ 599,764: 16x24\n#212 @ 557,83: 10x25\n#213 @ 335,766: 19x7\n#214 @ 537,310: 11x12\n#215 @ 59,922: 3x3\n#216 @ 471,740: 27x17\n#217 @ 585,443: 10x13\n#218 @ 695,860: 29x11\n#219 @ 542,647: 21x25\n#220 @ 457,382: 15x29\n#221 @ 859,697: 21x19\n#222 @ 842,131: 10x29\n#223 @ 426,767: 13x27\n#224 @ 972,232: 19x21\n#225 @ 113,38: 11x15\n#226 @ 780,347: 16x28\n#227 @ 39,115: 10x12\n#228 @ 358,180: 25x27\n#229 @ 932,926: 22x15\n#230 @ 8,173: 21x27\n#231 @ 369,553: 23x15\n#232 @ 718,186: 14x10\n#233 @ 379,606: 18x17\n#234 @ 608,965: 21x11\n#235 @ 221,895: 10x27\n#236 @ 773,280: 16x22\n#237 @ 301,99: 14x17\n#238 @ 771,57: 28x14\n#239 @ 840,378: 27x19\n#240 @ 667,186: 11x23\n#241 @ 842,215: 29x23\n#242 @ 441,113: 11x26\n#243 @ 960,287: 29x14\n#244 @ 673,204: 19x23\n#245 @ 945,741: 24x11\n#246 @ 739,641: 23x22\n#247 @ 627,163: 23x14\n#248 @ 784,813: 19x15\n#249 @ 644,756: 15x15\n#250 @ 837,536: 24x28\n#251 @ 884,75: 17x24\n#252 @ 551,558: 14x29\n#253 @ 937,865: 10x12\n#254 @ 715,830: 24x28\n#255 @ 343,562: 21x22\n#256 @ 111,832: 12x10\n#257 @ 639,108: 20x16\n#258 @ 368,767: 21x24\n#259 @ 607,570: 12x14\n#260 @ 777,80: 26x23\n#261 @ 667,108: 16x12\n#262 @ 229,529: 25x12\n#263 @ 825,701: 10x28\n#264 @ 246,820: 15x18\n#265 @ 842,871: 29x15\n#266 @ 468,559: 29x10\n#267 @ 538,396: 20x28\n#268 @ 516,901: 29x19\n#269 @ 264,423: 12x14\n#270 @ 149,901: 16x12\n#271 @ 414,782: 27x29\n#272 @ 671,745: 11x27\n#273 @ 425,860: 18x27\n#274 @ 949,346: 26x18\n#275 @ 13,783: 14x27\n#276 @ 276,225: 19x14\n#277 @ 642,83: 14x25\n#278 @ 638,885: 29x10\n#279 @ 4,445: 17x21\n#280 @ 401,947: 19x15\n#281 @ 226,810: 26x27\n#282 @ 0,188: 21x24\n#283 @ 455,588: 24x26\n#284 @ 64,98: 20x10\n#285 @ 554,78: 27x26\n#286 @ 978,174: 16x26\n#287 @ 57,651: 27x24\n#288 @ 844,963: 20x22\n#289 @ 10,141: 27x21\n#290 @ 360,11: 28x29\n#291 @ 642,210: 13x15\n#292 @ 205,272: 26x11\n#293 @ 523,561: 19x20\n#294 @ 388,566: 14x17\n#295 @ 384,558: 15x12\n#296 @ 21,988: 10x11\n#297 @ 875,712: 14x16\n#298 @ 208,399: 28x14\n#299 @ 62,150: 11x13\n#300 @ 235,273: 12x27\n#301 @ 770,561: 16x20\n#302 @ 498,651: 15x24\n#303 @ 229,778: 18x21\n#304 @ 646,520: 26x17\n#305 @ 95,913: 14x17\n#306 @ 227,532: 10x14\n#307 @ 187,700: 11x24\n#308 @ 843,715: 27x28\n#309 @ 269,954: 13x11\n#310 @ 672,890: 28x13\n#311 @ 198,382: 25x22\n#312 @ 790,272: 15x20\n#313 @ 656,714: 11x19\n#314 @ 550,164: 28x13\n#315 @ 537,916: 25x23\n#316 @ 77,314: 19x27\n#317 @ 390,297: 25x19\n#318 @ 145,346: 28x11\n#319 @ 638,724: 13x16\n#320 @ 922,29: 15x3\n#321 @ 940,29: 19x20\n#322 @ 388,436: 19x19\n#323 @ 170,304: 23x26\n#324 @ 911,652: 20x28\n#325 @ 932,13: 11x21\n#326 @ 874,839: 13x12\n#327 @ 313,628: 18x26\n#328 @ 716,229: 17x25\n#329 @ 119,667: 15x27\n#330 @ 471,303: 10x10\n#331 @ 0,795: 20x10\n#332 @ 413,17: 23x26\n#333 @ 415,23: 20x13\n#334 @ 43,367: 29x21\n#335 @ 227,528: 18x13\n#336 @ 195,98: 18x19\n#337 @ 647,209: 27x10\n#338 @ 94,913: 18x22\n#339 @ 627,483: 29x22\n#340 @ 387,938: 16x11\n#341 @ 493,837: 16x23\n#342 @ 808,500: 21x22\n#343 @ 26,793: 18x18\n#344 @ 95,283: 19x23\n#345 @ 507,168: 29x12\n#346 @ 919,563: 18x16\n#347 @ 110,899: 14x18\n#348 @ 66,539: 28x27\n#349 @ 283,819: 11x27\n#350 @ 133,591: 24x10\n#351 @ 205,740: 22x14\n#352 @ 343,676: 19x11\n#353 @ 932,369: 14x11\n#354 @ 218,419: 27x18\n#355 @ 871,760: 14x29\n#356 @ 212,5: 22x12\n#357 @ 693,652: 10x24\n#358 @ 574,656: 27x25\n#359 @ 488,872: 14x17\n#360 @ 71,362: 28x24\n#361 @ 518,735: 22x23\n#362 @ 359,67: 20x29\n#363 @ 92,270: 19x22\n#364 @ 376,316: 26x13\n#365 @ 410,931: 24x13\n#366 @ 703,199: 23x21\n#367 @ 801,432: 20x18\n#368 @ 907,859: 21x20\n#369 @ 161,263: 26x21\n#370 @ 312,734: 10x10\n#371 @ 489,732: 27x28\n#372 @ 548,898: 20x23\n#373 @ 150,87: 12x18\n#374 @ 636,840: 17x17\n#375 @ 268,319: 15x25\n#376 @ 937,958: 28x24\n#377 @ 417,523: 29x24\n#378 @ 27,110: 13x23\n#379 @ 543,37: 29x13\n#380 @ 618,464: 21x29\n#381 @ 142,313: 14x29\n#382 @ 664,723: 20x21\n#383 @ 303,427: 29x18\n#384 @ 651,579: 19x22\n#385 @ 327,707: 21x16\n#386 @ 66,59: 20x25\n#387 @ 156,502: 23x20\n#388 @ 753,288: 23x25\n#389 @ 146,259: 29x20\n#390 @ 232,400: 13x24\n#391 @ 753,446: 14x15\n#392 @ 351,616: 11x19\n#393 @ 303,713: 24x11\n#394 @ 324,902: 29x12\n#395 @ 965,284: 11x18\n#396 @ 512,952: 27x14\n#397 @ 869,301: 17x17\n#398 @ 89,303: 12x10\n#399 @ 923,737: 24x20\n#400 @ 137,418: 21x23\n#401 @ 315,607: 19x23\n#402 @ 868,595: 23x13\n#403 @ 29,160: 14x17\n#404 @ 626,761: 11x28\n#405 @ 482,675: 27x28\n#406 @ 960,747: 11x29\n#407 @ 179,962: 11x13\n#408 @ 782,90: 25x28\n#409 @ 177,71: 20x13\n#410 @ 822,692: 13x14\n#411 @ 98,326: 16x27\n#412 @ 807,435: 13x13\n#413 @ 583,155: 18x16\n#414 @ 275,250: 20x23\n#415 @ 320,689: 25x25\n#416 @ 741,704: 27x24\n#417 @ 880,380: 26x15\n#418 @ 697,506: 13x13\n#419 @ 830,228: 20x24\n#420 @ 163,652: 27x25\n#421 @ 412,239: 25x10\n#422 @ 35,548: 11x16\n#423 @ 617,247: 27x22\n#424 @ 167,567: 27x10\n#425 @ 556,69: 15x21\n#426 @ 363,203: 16x13\n#427 @ 558,947: 17x28\n#428 @ 298,282: 14x23\n#429 @ 697,706: 10x18\n#430 @ 754,740: 19x10\n#431 @ 681,345: 22x12\n#432 @ 219,490: 29x25\n#433 @ 579,638: 21x29\n#434 @ 57,70: 21x13\n#435 @ 27,337: 23x17\n#436 @ 151,685: 28x28\n#437 @ 312,474: 10x23\n#438 @ 972,664: 27x21\n#439 @ 638,446: 11x26\n#440 @ 270,845: 13x15\n#441 @ 235,788: 27x22\n#442 @ 638,209: 16x10\n#443 @ 484,873: 12x25\n#444 @ 922,295: 5x7\n#445 @ 182,964: 27x23\n#446 @ 902,376: 23x14\n#447 @ 187,688: 15x13\n#448 @ 188,612: 18x13\n#449 @ 406,688: 22x11\n#450 @ 164,71: 20x12\n#451 @ 341,299: 18x23\n#452 @ 670,837: 18x14\n#453 @ 706,424: 23x26\n#454 @ 75,160: 27x24\n#455 @ 431,285: 28x25\n#456 @ 502,966: 12x20\n#457 @ 287,537: 16x16\n#458 @ 236,115: 13x28\n#459 @ 208,741: 17x11\n#460 @ 665,590: 27x15\n#461 @ 928,10: 19x10\n#462 @ 558,277: 28x15\n#463 @ 80,114: 11x27\n#464 @ 830,224: 11x24\n#465 @ 571,834: 29x16\n#466 @ 758,337: 20x19\n#467 @ 936,727: 20x10\n#468 @ 843,100: 13x13\n#469 @ 692,656: 23x12\n#470 @ 168,358: 29x26\n#471 @ 804,87: 16x11\n#472 @ 272,637: 14x13\n#473 @ 681,669: 3x14\n#474 @ 530,701: 15x26\n#475 @ 420,287: 11x14\n#476 @ 407,276: 29x24\n#477 @ 636,27: 27x10\n#478 @ 12,301: 14x15\n#479 @ 837,953: 14x25\n#480 @ 851,625: 10x20\n#481 @ 303,604: 10x16\n#482 @ 2,648: 21x17\n#483 @ 434,747: 29x16\n#484 @ 875,505: 26x12\n#485 @ 915,467: 16x13\n#486 @ 319,321: 22x23\n#487 @ 288,160: 21x25\n#488 @ 683,112: 10x27\n#489 @ 673,69: 17x20\n#490 @ 63,482: 27x22\n#491 @ 454,813: 16x17\n#492 @ 247,496: 22x19\n#493 @ 112,584: 10x21\n#494 @ 897,323: 17x20\n#495 @ 71,762: 29x28\n#496 @ 111,678: 14x20\n#497 @ 121,686: 21x19\n#498 @ 745,902: 26x15\n#499 @ 537,784: 13x26\n#500 @ 337,781: 22x15\n#501 @ 467,155: 27x28\n#502 @ 201,301: 24x18\n#503 @ 168,663: 15x10\n#504 @ 310,427: 20x26\n#505 @ 940,404: 12x28\n#506 @ 564,203: 26x12\n#507 @ 263,975: 11x21\n#508 @ 39,871: 19x11\n#509 @ 647,841: 28x10\n#510 @ 347,548: 24x23\n#511 @ 463,565: 28x10\n#512 @ 775,350: 20x20\n#513 @ 228,424: 23x13\n#514 @ 677,145: 12x18\n#515 @ 401,819: 28x29\n#516 @ 427,271: 24x22\n#517 @ 502,943: 26x18\n#518 @ 387,925: 23x11\n#519 @ 294,214: 15x15\n#520 @ 21,976: 16x13\n#521 @ 798,534: 18x22\n#522 @ 519,692: 29x23\n#523 @ 826,368: 19x13\n#524 @ 964,626: 10x21\n#525 @ 570,958: 19x20\n#526 @ 388,203: 13x13\n#527 @ 432,975: 21x15\n#528 @ 143,354: 15x13\n#529 @ 920,25: 22x13\n#530 @ 552,278: 22x19\n#531 @ 348,171: 23x27\n#532 @ 132,462: 17x28\n#533 @ 117,379: 15x11\n#534 @ 951,608: 13x12\n#535 @ 202,552: 17x19\n#536 @ 92,932: 10x14\n#537 @ 887,967: 28x17\n#538 @ 18,698: 10x16\n#539 @ 652,596: 18x20\n#540 @ 815,532: 29x29\n#541 @ 740,730: 17x14\n#542 @ 787,55: 24x29\n#543 @ 67,654: 21x29\n#544 @ 406,202: 12x21\n#545 @ 363,20: 27x15\n#546 @ 817,68: 15x18\n#547 @ 563,494: 29x15\n#548 @ 841,149: 16x23\n#549 @ 125,168: 28x15\n#550 @ 879,26: 19x23\n#551 @ 335,603: 27x13\n#552 @ 71,106: 17x14\n#553 @ 37,203: 11x19\n#554 @ 481,311: 12x14\n#555 @ 166,578: 23x20\n#556 @ 546,366: 26x24\n#557 @ 747,233: 24x20\n#558 @ 835,544: 22x18\n#559 @ 819,106: 28x17\n#560 @ 832,234: 4x8\n#561 @ 452,611: 28x13\n#562 @ 614,481: 21x20\n#563 @ 759,0: 12x14\n#564 @ 692,688: 13x10\n#565 @ 143,497: 15x20\n#566 @ 848,159: 18x12\n#567 @ 839,359: 12x12\n#568 @ 796,137: 22x18\n#569 @ 565,709: 29x16\n#570 @ 91,784: 18x28\n#571 @ 263,974: 15x20\n#572 @ 158,447: 15x14\n#573 @ 178,157: 14x14\n#574 @ 252,580: 20x28\n#575 @ 501,479: 10x26\n#576 @ 593,815: 10x11\n#577 @ 597,850: 16x29\n#578 @ 753,601: 25x29\n#579 @ 554,283: 24x15\n#580 @ 906,833: 10x28\n#581 @ 863,957: 28x19\n#582 @ 956,461: 14x16\n#583 @ 829,555: 22x24\n#584 @ 269,598: 11x16\n#585 @ 628,503: 18x12\n#586 @ 83,160: 22x25\n#587 @ 576,918: 22x12\n#588 @ 777,359: 19x10\n#589 @ 342,95: 13x28\n#590 @ 123,878: 25x21\n#591 @ 946,860: 18x29\n#592 @ 647,913: 27x28\n#593 @ 793,261: 28x18\n#594 @ 859,847: 23x11\n#595 @ 612,950: 11x22\n#596 @ 3,50: 20x20\n#597 @ 298,574: 26x10\n#598 @ 231,722: 26x25\n#599 @ 95,533: 16x17\n#600 @ 661,100: 19x18\n#601 @ 259,169: 13x18\n#602 @ 507,866: 29x17\n#603 @ 286,706: 25x19\n#604 @ 334,429: 23x20\n#605 @ 427,399: 22x16\n#606 @ 709,701: 24x15\n#607 @ 536,803: 19x26\n#608 @ 694,437: 15x26\n#609 @ 513,866: 23x20\n#610 @ 544,484: 23x25\n#611 @ 330,650: 13x17\n#612 @ 214,943: 23x25\n#613 @ 490,478: 15x22\n#614 @ 891,633: 20x16\n#615 @ 733,671: 18x23\n#616 @ 537,895: 11x16\n#617 @ 365,259: 11x16\n#618 @ 170,868: 26x16\n#619 @ 737,701: 27x10\n#620 @ 728,80: 19x15\n#621 @ 713,974: 14x25\n#622 @ 105,320: 12x16\n#623 @ 66,22: 20x10\n#624 @ 550,28: 27x24\n#625 @ 264,324: 12x25\n#626 @ 573,621: 28x13\n#627 @ 487,339: 13x16\n#628 @ 562,166: 28x13\n#629 @ 632,868: 21x12\n#630 @ 739,235: 23x29\n#631 @ 401,292: 14x28\n#632 @ 365,930: 29x22\n#633 @ 306,927: 20x25\n#634 @ 961,665: 17x20\n#635 @ 74,85: 23x13\n#636 @ 478,656: 13x26\n#637 @ 342,381: 14x11\n#638 @ 921,415: 21x15\n#639 @ 236,293: 13x12\n#640 @ 736,741: 15x29\n#641 @ 56,790: 29x11\n#642 @ 379,195: 24x29\n#643 @ 263,461: 18x19\n#644 @ 590,485: 28x20\n#645 @ 388,679: 14x16\n#646 @ 54,461: 15x23\n#647 @ 317,932: 12x22\n#648 @ 261,619: 15x12\n#649 @ 875,443: 18x15\n#650 @ 206,30: 29x18\n#651 @ 138,543: 12x27\n#652 @ 552,145: 29x25\n#653 @ 337,606: 23x19\n#654 @ 415,882: 29x10\n#655 @ 105,593: 16x12\n#656 @ 18,61: 18x23\n#657 @ 399,681: 28x22\n#658 @ 661,876: 26x12\n#659 @ 183,403: 14x23\n#660 @ 922,665: 17x16\n#661 @ 703,863: 23x27\n#662 @ 109,88: 22x18\n#663 @ 428,104: 24x26\n#664 @ 557,637: 12x19\n#665 @ 270,853: 23x21\n#666 @ 139,308: 21x22\n#667 @ 536,697: 25x16\n#668 @ 800,142: 15x11\n#669 @ 14,28: 26x26\n#670 @ 585,196: 12x22\n#671 @ 153,437: 21x26\n#672 @ 632,432: 27x15\n#673 @ 477,94: 16x13\n#674 @ 960,342: 27x21\n#675 @ 176,644: 20x29\n#676 @ 498,427: 16x29\n#677 @ 487,736: 20x23\n#678 @ 637,154: 28x18\n#679 @ 491,393: 29x12\n#680 @ 529,352: 14x11\n#681 @ 128,508: 21x14\n#682 @ 51,278: 16x17\n#683 @ 136,508: 17x19\n#684 @ 861,151: 10x14\n#685 @ 829,958: 22x11\n#686 @ 728,812: 13x26\n#687 @ 418,307: 24x23\n#688 @ 27,186: 17x29\n#689 @ 736,336: 15x26\n#690 @ 161,791: 14x24\n#691 @ 233,116: 22x16\n#692 @ 17,300: 23x24\n#693 @ 913,828: 14x29\n#694 @ 919,313: 12x15\n#695 @ 791,591: 29x22\n#696 @ 794,555: 25x25\n#697 @ 764,396: 17x25\n#698 @ 155,304: 12x15\n#699 @ 973,615: 17x19\n#700 @ 594,431: 18x15\n#701 @ 606,746: 13x21\n#702 @ 166,770: 29x20\n#703 @ 350,50: 11x10\n#704 @ 325,677: 25x20\n#705 @ 424,385: 19x12\n#706 @ 443,708: 13x15\n#707 @ 282,549: 11x20\n#708 @ 164,962: 17x20\n#709 @ 100,13: 21x10\n#710 @ 454,914: 24x27\n#711 @ 208,421: 25x17\n#712 @ 85,25: 22x26\n#713 @ 824,480: 6x10\n#714 @ 280,290: 23x29\n#715 @ 732,678: 18x26\n#716 @ 963,335: 18x11\n#717 @ 780,981: 26x18\n#718 @ 162,144: 21x11\n#719 @ 609,918: 25x26\n#720 @ 814,191: 25x27\n#721 @ 641,610: 18x16\n#722 @ 374,721: 12x25\n#723 @ 534,939: 27x16\n#724 @ 8,449: 9x8\n#725 @ 919,362: 22x25\n#726 @ 206,953: 19x22\n#727 @ 584,388: 15x11\n#728 @ 643,742: 16x28\n#729 @ 224,25: 29x18\n#730 @ 643,709: 18x19\n#731 @ 495,188: 14x23\n#732 @ 489,603: 15x21\n#733 @ 156,364: 7x7\n#734 @ 447,399: 11x17\n#735 @ 894,52: 19x15\n#736 @ 878,63: 15x21\n#737 @ 383,625: 27x21\n#738 @ 586,86: 16x25\n#739 @ 121,907: 13x14\n#740 @ 832,14: 29x27\n#741 @ 188,370: 19x25\n#742 @ 551,478: 18x12\n#743 @ 13,800: 27x26\n#744 @ 527,335: 15x14\n#745 @ 36,421: 22x13\n#746 @ 743,710: 20x8\n#747 @ 540,788: 18x25\n#748 @ 421,263: 22x26\n#749 @ 16,187: 21x20\n#750 @ 515,149: 14x16\n#751 @ 549,89: 26x25\n#752 @ 675,926: 13x22\n#753 @ 90,146: 18x18\n#754 @ 188,360: 14x22\n#755 @ 242,108: 18x21\n#756 @ 200,557: 14x22\n#757 @ 876,895: 10x13\n#758 @ 961,664: 26x28\n#759 @ 765,77: 24x12\n#760 @ 930,696: 18x23\n#761 @ 646,628: 27x17\n#762 @ 421,56: 22x14\n#763 @ 436,146: 11x28\n#764 @ 415,391: 26x25\n#765 @ 259,779: 13x29\n#766 @ 770,384: 28x16\n#767 @ 59,429: 19x16\n#768 @ 130,905: 19x17\n#769 @ 165,799: 11x13\n#770 @ 922,672: 24x11\n#771 @ 892,381: 14x10\n#772 @ 27,413: 14x23\n#773 @ 479,179: 17x27\n#774 @ 13,160: 22x10\n#775 @ 826,448: 26x27\n#776 @ 233,514: 14x13\n#777 @ 344,761: 29x28\n#778 @ 833,497: 18x28\n#779 @ 87,535: 22x14\n#780 @ 932,691: 11x11\n#781 @ 389,574: 19x4\n#782 @ 189,162: 21x22\n#783 @ 359,937: 19x21\n#784 @ 29,266: 13x20\n#785 @ 356,61: 15x29\n#786 @ 760,607: 20x28\n#787 @ 748,724: 24x29\n#788 @ 72,890: 23x12\n#789 @ 897,48: 15x14\n#790 @ 446,802: 21x20\n#791 @ 147,463: 16x20\n#792 @ 950,447: 10x18\n#793 @ 155,510: 21x12\n#794 @ 636,291: 29x13\n#795 @ 593,914: 26x18\n#796 @ 295,719: 13x28\n#797 @ 768,934: 10x22\n#798 @ 242,98: 18x29\n#799 @ 410,326: 19x13\n#800 @ 115,653: 12x19\n#801 @ 276,874: 10x25\n#802 @ 300,657: 25x22\n#803 @ 345,202: 21x14\n#804 @ 474,818: 29x26\n#805 @ 428,403: 14x23\n#806 @ 948,752: 23x14\n#807 @ 898,316: 24x17\n#808 @ 51,84: 15x24\n#809 @ 871,279: 25x16\n#810 @ 905,373: 17x20\n#811 @ 341,81: 19x23\n#812 @ 166,506: 27x15\n#813 @ 144,240: 29x26\n#814 @ 676,221: 25x20\n#815 @ 83,398: 29x22\n#816 @ 50,180: 26x16\n#817 @ 192,559: 14x24\n#818 @ 44,918: 27x12\n#819 @ 83,819: 25x10\n#820 @ 324,666: 16x26\n#821 @ 623,249: 28x15\n#822 @ 263,426: 21x12\n#823 @ 144,298: 12x29\n#824 @ 861,62: 11x27\n#825 @ 918,291: 24x19\n#826 @ 75,896: 13x18\n#827 @ 881,607: 22x24\n#828 @ 471,278: 12x28\n#829 @ 17,350: 27x12\n#830 @ 327,436: 22x22\n#831 @ 774,299: 11x21\n#832 @ 125,238: 10x17\n#833 @ 822,476: 11x18\n#834 @ 291,424: 29x18\n#835 @ 909,309: 26x29\n#836 @ 15,330: 21x10\n#837 @ 378,871: 18x19\n#838 @ 63,148: 10x12\n#839 @ 418,112: 13x16\n#840 @ 350,700: 21x12\n#841 @ 428,377: 26x10\n#842 @ 81,96: 17x11\n#843 @ 654,229: 19x29\n#844 @ 268,42: 24x10\n#845 @ 716,833: 19x12\n#846 @ 358,177: 29x21\n#847 @ 520,720: 15x22\n#848 @ 458,760: 19x21\n#849 @ 606,774: 29x14\n#850 @ 96,90: 17x29\n#851 @ 604,910: 20x25\n#852 @ 343,831: 27x28\n#853 @ 593,709: 17x16\n#854 @ 58,387: 14x17\n#855 @ 834,73: 18x19\n#856 @ 479,345: 27x15\n#857 @ 497,442: 6x9\n#858 @ 217,553: 28x12\n#859 @ 32,110: 29x19\n#860 @ 305,565: 26x26\n#861 @ 633,499: 14x21\n#862 @ 293,810: 26x29\n#863 @ 483,884: 11x14\n#864 @ 662,599: 12x16\n#865 @ 724,21: 23x11\n#866 @ 282,409: 23x10\n#867 @ 218,275: 26x15\n#868 @ 952,953: 28x25\n#869 @ 864,599: 19x24\n#870 @ 523,944: 15x23\n#871 @ 942,381: 14x25\n#872 @ 802,813: 17x24\n#873 @ 771,736: 13x12\n#874 @ 64,93: 24x19\n#875 @ 937,11: 28x21\n#876 @ 679,665: 10x29\n#877 @ 420,727: 18x25\n#878 @ 741,767: 18x19\n#879 @ 741,200: 10x15\n#880 @ 612,289: 16x13\n#881 @ 644,519: 26x20\n#882 @ 937,474: 22x28\n#883 @ 409,650: 18x20\n#884 @ 54,875: 17x20\n#885 @ 379,875: 25x11\n#886 @ 13,276: 21x25\n#887 @ 267,425: 4x7\n#888 @ 481,382: 21x16\n#889 @ 815,436: 28x10\n#890 @ 723,183: 29x11\n#891 @ 209,105: 28x17\n#892 @ 102,229: 14x23\n#893 @ 386,265: 18x27\n#894 @ 406,683: 15x26\n#895 @ 283,248: 10x10\n#896 @ 634,217: 24x23\n#897 @ 782,51: 24x23\n#898 @ 973,159: 24x19\n#899 @ 366,195: 13x23\n#900 @ 522,778: 20x11\n#901 @ 509,726: 14x20\n#902 @ 214,291: 28x25\n#903 @ 579,550: 29x24\n#904 @ 747,374: 24x21\n#905 @ 947,389: 4x13\n#906 @ 585,44: 25x28\n#907 @ 647,639: 29x11\n#908 @ 518,848: 22x22\n#909 @ 976,18: 22x27\n#910 @ 309,653: 14x14\n#911 @ 258,606: 15x15\n#912 @ 79,19: 28x23\n#913 @ 82,550: 15x13\n#914 @ 425,279: 21x29\n#915 @ 953,95: 25x11\n#916 @ 905,190: 11x24\n#917 @ 832,356: 27x24\n#918 @ 420,354: 21x28\n#919 @ 22,630: 24x22\n#920 @ 530,701: 14x10\n#921 @ 452,294: 10x19\n#922 @ 562,343: 28x29\n#923 @ 830,63: 21x28\n#924 @ 860,587: 28x23\n#925 @ 389,367: 16x24\n#926 @ 850,875: 15x29\n#927 @ 437,750: 27x17\n#928 @ 571,776: 10x29\n#929 @ 546,390: 21x19\n#930 @ 140,889: 13x19\n#931 @ 567,843: 21x14\n#932 @ 845,514: 10x15\n#933 @ 980,835: 14x13\n#934 @ 593,767: 22x22\n#935 @ 299,576: 16x25\n#936 @ 495,438: 13x23\n#937 @ 411,663: 28x14\n#938 @ 260,605: 13x29\n#939 @ 550,603: 20x23\n#940 @ 162,239: 10x25\n#941 @ 214,645: 13x24\n#942 @ 15,693: 17x13\n#943 @ 909,42: 10x25\n#944 @ 396,215: 23x15\n#945 @ 713,876: 29x21\n#946 @ 151,77: 13x15\n#947 @ 713,27: 25x23\n#948 @ 718,472: 12x13\n#949 @ 683,154: 28x13\n#950 @ 160,880: 17x23\n#951 @ 320,880: 12x24\n#952 @ 961,184: 23x10\n#953 @ 195,418: 13x25\n#954 @ 953,339: 26x15\n#955 @ 311,874: 18x12\n#956 @ 275,626: 15x29\n#957 @ 876,593: 10x18\n#958 @ 9,376: 28x17\n#959 @ 732,227: 23x25\n#960 @ 910,167: 11x29\n#961 @ 826,369: 24x19\n#962 @ 735,639: 28x12\n#963 @ 796,361: 28x19\n#964 @ 149,794: 23x23\n#965 @ 379,369: 29x20\n#966 @ 738,33: 15x16\n#967 @ 933,915: 11x14\n#968 @ 14,171: 13x17\n#969 @ 776,291: 25x12\n#970 @ 831,97: 15x28\n#971 @ 832,131: 18x11\n#972 @ 900,898: 13x24\n#973 @ 235,784: 16x17\n#974 @ 744,178: 20x25\n#975 @ 890,712: 15x27\n#976 @ 256,928: 20x26\n#977 @ 575,454: 26x26\n#978 @ 939,375: 16x16\n#979 @ 64,632: 16x21\n#980 @ 614,292: 19x28\n#981 @ 234,120: 10x24\n#982 @ 93,16: 29x23\n#983 @ 889,373: 16x14\n#984 @ 264,414: 20x16\n#985 @ 61,664: 23x23\n#986 @ 900,671: 11x26\n#987 @ 336,331: 13x26\n#988 @ 836,89: 28x26\n#989 @ 681,852: 18x12\n#990 @ 83,788: 10x15\n#991 @ 709,220: 17x27\n#992 @ 321,737: 10x27\n#993 @ 618,755: 11x26\n#994 @ 616,505: 26x17\n#995 @ 404,195: 17x27\n#996 @ 490,633: 3x10\n#997 @ 272,866: 28x17\n#998 @ 554,160: 21x14\n#999 @ 138,916: 19x25\n#1000 @ 375,379: 23x16\n#1001 @ 416,464: 18x19\n#1002 @ 720,951: 21x10\n#1003 @ 110,539: 18x15\n#1004 @ 670,345: 29x19\n#1005 @ 728,553: 22x20\n#1006 @ 374,883: 17x24\n#1007 @ 293,169: 10x8\n#1008 @ 610,734: 15x17\n#1009 @ 538,222: 21x28\n#1010 @ 355,532: 17x23\n#1011 @ 654,102: 27x16\n#1012 @ 595,110: 25x14\n#1013 @ 347,518: 12x6\n#1014 @ 366,679: 25x20\n#1015 @ 758,362: 14x25\n#1016 @ 862,297: 19x12\n#1017 @ 267,335: 26x22\n#1018 @ 141,888: 25x17\n#1019 @ 944,524: 10x15\n#1020 @ 550,548: 14x21\n#1021 @ 495,357: 24x27\n#1022 @ 189,104: 13x21\n#1023 @ 851,784: 21x14\n#1024 @ 819,545: 29x25\n#1025 @ 464,371: 11x25\n#1026 @ 601,92: 29x19\n#1027 @ 256,413: 27x26\n#1028 @ 599,941: 28x13\n#1029 @ 162,363: 29x24\n#1030 @ 84,24: 20x27\n#1031 @ 732,417: 28x29\n#1032 @ 148,300: 26x24\n#1033 @ 374,268: 15x22\n#1034 @ 952,611: 19x22\n#1035 @ 248,330: 21x19\n#1036 @ 28,90: 26x20\n#1037 @ 803,320: 18x27\n#1038 @ 633,735: 19x19\n#1039 @ 571,771: 18x13\n#1040 @ 485,888: 6x4\n#1041 @ 734,374: 13x16\n#1042 @ 737,38: 17x11\n#1043 @ 594,930: 12x15\n#1044 @ 669,880: 28x14\n#1045 @ 733,372: 11x22\n#1046 @ 507,897: 11x23\n#1047 @ 402,533: 11x19\n#1048 @ 291,54: 14x28\n#1049 @ 598,369: 17x12\n#1050 @ 784,249: 12x14\n#1051 @ 803,556: 28x25\n#1052 @ 236,418: 18x12\n#1053 @ 288,586: 29x28\n#1054 @ 108,324: 20x20\n#1055 @ 392,844: 17x14\n#1056 @ 713,949: 23x25\n#1057 @ 692,446: 22x16\n#1058 @ 738,245: 10x27\n#1059 @ 662,877: 14x22\n#1060 @ 32,411: 16x20\n#1061 @ 369,370: 28x10\n#1062 @ 66,184: 17x21\n#1063 @ 143,584: 16x10\n#1064 @ 147,216: 26x20\n#1065 @ 52,354: 19x19\n#1066 @ 108,315: 19x13\n#1067 @ 930,690: 21x26\n#1068 @ 304,579: 22x14\n#1069 @ 586,861: 18x26\n#1070 @ 96,772: 12x26\n#1071 @ 799,823: 13x10\n#1072 @ 962,144: 14x25\n#1073 @ 854,639: 15x10\n#1074 @ 550,552: 24x10\n#1075 @ 964,636: 18x18\n#1076 @ 5,179: 4x12\n#1077 @ 73,408: 19x18\n#1078 @ 145,787: 15x18\n#1079 @ 203,398: 24x10\n#1080 @ 984,281: 11x27\n#1081 @ 582,785: 23x23\n#1082 @ 766,516: 28x26\n#1083 @ 397,852: 14x10\n#1084 @ 854,961: 12x14\n#1085 @ 28,532: 20x17\n#1086 @ 350,374: 21x15\n#1087 @ 974,622: 17x24\n#1088 @ 113,834: 6x5\n#1089 @ 802,313: 21x19\n#1090 @ 106,106: 29x28\n#1091 @ 610,572: 4x5\n#1092 @ 195,301: 28x14\n#1093 @ 327,124: 15x24\n#1094 @ 249,677: 16x28\n#1095 @ 674,117: 17x10\n#1096 @ 328,320: 11x11\n#1097 @ 942,952: 10x10\n#1098 @ 718,428: 14x13\n#1099 @ 192,1: 27x14\n#1100 @ 905,867: 28x27\n#1101 @ 812,83: 18x18\n#1102 @ 401,217: 28x12\n#1103 @ 781,982: 13x11\n#1104 @ 252,178: 11x24\n#1105 @ 335,175: 14x27\n#1106 @ 514,159: 15x15\n#1107 @ 25,258: 20x28\n#1108 @ 534,102: 27x20\n#1109 @ 333,841: 5x3\n#1110 @ 31,935: 18x29\n#1111 @ 799,432: 12x12\n#1112 @ 103,7: 15x22\n#1113 @ 52,773: 20x24\n#1114 @ 152,463: 26x28\n#1115 @ 386,381: 10x14\n#1116 @ 430,246: 18x19\n#1117 @ 53,279: 16x15\n#1118 @ 61,74: 14x17\n#1119 @ 178,943: 13x25\n#1120 @ 145,418: 17x25\n#1121 @ 552,66: 13x15\n#1122 @ 980,220: 18x22\n#1123 @ 763,863: 29x16\n#1124 @ 712,844: 20x10\n#1125 @ 502,419: 23x11\n#1126 @ 631,420: 11x13\n#1127 @ 58,225: 18x18\n#1128 @ 695,465: 27x20\n#1129 @ 940,688: 17x19\n#1130 @ 505,627: 27x16\n#1131 @ 719,540: 28x16\n#1132 @ 358,320: 10x13\n#1133 @ 834,295: 19x26\n#1134 @ 632,712: 16x12\n#1135 @ 607,786: 21x23\n#1136 @ 952,453: 21x15\n#1137 @ 430,543: 10x16\n#1138 @ 568,159: 21x16\n#1139 @ 662,218: 28x10\n#1140 @ 808,511: 27x25\n#1141 @ 893,877: 26x29\n#1142 @ 517,897: 22x10\n#1143 @ 596,464: 10x26\n#1144 @ 725,233: 24x22\n#1145 @ 129,116: 25x26\n#1146 @ 411,914: 27x12\n#1147 @ 36,871: 7x7\n#1148 @ 373,187: 29x17\n#1149 @ 786,755: 12x12\n#1150 @ 555,271: 17x19\n#1151 @ 530,771: 20x23\n#1152 @ 597,463: 26x26\n#1153 @ 508,629: 18x8\n#1154 @ 682,500: 10x12\n#1155 @ 707,154: 20x28\n#1156 @ 345,848: 18x26\n#1157 @ 345,706: 27x16\n#1158 @ 473,293: 24x13\n#1159 @ 316,609: 28x23\n#1160 @ 651,504: 23x16\n#1161 @ 381,277: 26x25\n#1162 @ 674,771: 22x23\n#1163 @ 830,718: 13x25\n#1164 @ 966,131: 22x26\n#1165 @ 28,866: 28x17\n#1166 @ 183,72: 22x19\n#1167 @ 564,536: 16x26\n#1168 @ 972,30: 15x28\n#1169 @ 387,296: 22x29\n#1170 @ 385,86: 26x26\n#1171 @ 671,832: 10x26\n#1172 @ 759,241: 13x23\n#1173 @ 237,289: 6x6\n#1174 @ 25,828: 18x16\n#1175 @ 194,379: 21x13\n#1176 @ 273,624: 10x12\n#1177 @ 404,627: 12x15\n#1178 @ 596,623: 12x25\n#1179 @ 674,96: 27x26\n#1180 @ 1,342: 24x26\n#1181 @ 307,609: 10x13\n#1182 @ 379,878: 24x14\n#1183 @ 390,382: 25x17\n#1184 @ 889,427: 21x19\n#1185 @ 486,887: 21x20\n#1186 @ 213,644: 15x24\n#1187 @ 593,468: 16x28\n#1188 @ 943,13: 21x29\n#1189 @ 541,704: 27x15\n#1190 @ 23,577: 22x16\n#1191 @ 582,721: 14x13\n#1192 @ 118,241: 23x12\n#1193 @ 403,810: 11x13\n#1194 @ 648,732: 28x23\n#1195 @ 532,939: 13x12\n#1196 @ 832,370: 19x11\n#1197 @ 381,783: 18x19\n#1198 @ 644,795: 20x19\n#1199 @ 253,475: 10x22\n#1200 @ 185,305: 18x10\n#1201 @ 554,498: 16x27\n#1202 @ 249,683: 24x14\n#1203 @ 175,773: 27x26\n#1204 @ 363,816: 24x29\n#1205 @ 525,353: 18x25\n#1206 @ 368,720: 17x10\n#1207 @ 553,310: 17x13\n#1208 @ 911,328: 17x29\n#1209 @ 262,452: 23x25\n#1210 @ 727,311: 26x26\n#1211 @ 850,965: 11x28\n#1212 @ 392,821: 16x10\n#1213 @ 828,873: 21x16\n#1214 @ 377,89: 25x12\n#1215 @ 922,424: 28x13\n#1216 @ 165,677: 27x17\n#1217 @ 306,717: 12x22\n#1218 @ 390,552: 21x10\n#1219 @ 657,19: 10x23\n#1220 @ 259,679: 13x10\n#1221 @ 783,593: 11x28\n#1222 @ 186,670: 28x22\n#1223 @ 633,86: 19x22\n#1224 @ 57,425: 25x25\n#1225 @ 421,164: 21x20\n#1226 @ 881,815: 17x17\n#1227 @ 220,521: 21x17\n#1228 @ 154,282: 11x27\n#1229 @ 143,517: 16x27\n#1230 @ 45,748: 27x27\n#1231 @ 622,188: 25x24\n#1232 @ 932,969: 24x23\n#1233 @ 918,979: 23x17\n#1234 @ 748,283: 28x29\n#1235 @ 802,691: 23x22\n#1236 @ 264,609: 10x15\n#1237 @ 763,303: 21x17\n#1238 @ 702,220: 28x29\n#1239 @ 520,450: 19x23\n#1240 @ 154,584: 23x13\n#1241 @ 67,135: 21x18\n#1242 @ 827,937: 15x25\n#1243 @ 162,135: 11x26\n#1244 @ 511,943: 28x11\n#1245 @ 773,306: 10x21\n#1246 @ 655,902: 23x27\n#1247 @ 492,300: 20x21\n#1248 @ 548,118: 24x24\n#1249 @ 531,395: 22x21\n#1250 @ 932,404: 24x16\n#1251 @ 31,675: 12x24\n#1252 @ 20,534: 19x10\n#1253 @ 924,408: 25x15\n#1254 @ 58,272: 28x24\n#1255 @ 886,369: 26x10\n#1256 @ 748,307: 23x15\n#1257 @ 0,223: 11x24\n#1258 @ 397,684: 15x14\n#1259 @ 728,433: 12x13\n#1260 @ 419,120: 24x14\n#1261 @ 332,117: 13x10\n#1262 @ 293,301: 10x14\n#1263 @ 791,146: 26x21\n#1264 @ 296,907: 29x28\n#1265 @ 846,700: 15x28\n#1266 @ 584,425: 27x15\n#1267 @ 712,102: 27x14\n#1268 @ 330,838: 13x11\n#1269 @ 470,335: 29x11\n#1270 @ 200,537: 26x22\n#1271 @ 291,613: 21x10\n#1272 @ 974,838: 16x23\n#1273 @ 94,272: 4x16\n#1274 @ 255,687: 15x13\n#1275 @ 682,158: 29x16\n#1276 @ 24,220: 23x26\n#1277 @ 3,158: 29x27\n#1278 @ 251,209: 28x29\n#1279 @ 740,62: 23x20\n#1280 @ 355,621: 15x22\n#1281 @ 688,691: 17x16\n#1282 @ 498,610: 25x16\n#1283 @ 917,464: 24x14\n#1284 @ 78,540: 17x18\n#1285 @ 826,227: 24x19\n#1286 @ 235,589: 22x19\n#1287 @ 539,571: 22x25\n#1288 @ 528,274: 22x15\n#1289 @ 230,819: 21x19\n#1290 @ 460,938: 22x19\n#1291 @ 727,880: 28x27\n#1292 @ 770,4: 27x23\n#1293 @ 51,276: 25x11\n#1294 @ 177,140: 18x29\n#1295 @ 308,899: 25x11\n#1296 @ 581,179: 16x20\n#1297 @ 548,556: 26x18\n#1298 @ 850,10: 23x20\n#1299 @ 142,588: 14x20\n#1300 @ 34,241: 23x11\n#1301 @ 806,697: 15x25\n#1302 @ 72,542: 21x18\n#1303 @ 906,473: 12x10\n#1304 @ 755,386: 27x26\n#1305 @ 644,923: 15x18\n#1306 @ 942,969: 18x18\n#1307 @ 715,555: 10x20\n#1308 @ 781,655: 20x14\n#1309 @ 741,694: 27x26\n#1310 @ 636,430: 11x29\n#1311 @ 64,332: 23x23\n#1312 @ 408,238: 17x10\n#1313 @ 353,604: 26x25\n#1314 @ 164,588: 28x25\n#1315 @ 113,163: 25x12\n#1316 @ 3,293: 26x15\n#1317 @ 152,401: 18x23\n#1318 @ 898,877: 20x12\n#1319 @ 475,636: 24x21\n#1320 @ 514,706: 26x13\n#1321 @ 227,509: 24x25\n#1322 @ 538,679: 18x24\n#1323 @ 435,10: 23x10\n#1324 @ 261,891: 23x24\n#1325 @ 509,964: 21x24\n#1326 @ 424,466: 5x13\n#1327 @ 486,180: 16x18\n#1328 @ 7,234: 29x12\n#1329 @ 513,146: 19x26\n#1330 @ 43,182: 20x20\n#1331 @ 385,898: 29x17\n#1332 @ 628,283: 18x17\n#1333 @ 473,953: 26x24\n#1334 @ 933,669: 11x10\n#1335 @ 954,902: 12x27\n#1336 @ 397,422: 18x23\n#1337 @ 314,857: 16x22\n#1338 @ 398,928: 24x14\n#1339 @ 443,279: 16x10\n#1340 @ 865,491: 15x25\n#1341 @ 182,110: 26x15\n#1342 @ 184,390: 21x10\n#1343 @ 28,374: 19x17\n#1344 @ 150,597: 20x17\n#1345 @ 285,109: 20x25\n#1346 @ 535,306: 16x20\n#1347 @ 545,216: 16x19\n#1348 @ 72,258: 16x18\n#1349 @ 487,96: 18x21\n#1350 @ 942,729: 10x4\n#1351 @ 359,409: 28x13\n#1352 @ 199,292: 16x27\n#1353 @ 786,383: 17x15\n#1354 @ 593,94: 14x10\n#1355 @ 518,348: 12x17\n#1356 @ 753,444: 17x13\n#1357 @ 231,721: 16x11\n#1358 @ 328,587: 26x26\n#1359 @ 83,826: 21x24\n#1360 @ 167,146: 28x20\n#1361 @ 922,856: 13x23\n#1362 @ 124,531: 17x24\n#1363 @ 600,332: 11x15\n#1364 @ 22,825: 29x23\n#1365 @ 290,869: 25x20\n#1366 @ 942,452: 21x12\n#1367 @ 171,448: 26x14\n#1368 @ 881,646: 24x28\n#1369 @ 766,117: 10x17\n#1370 @ 602,246: 16x21\n#1371 @ 770,936: 3x16\n#1372 @ 567,600: 22x16\n#1373 @ 541,269: 23x22\n#1374 @ 934,2: 10x27\n#1375 @ 865,821: 27x10\n#1376 @ 936,550: 23x16\n#1377 @ 612,447: 23x20\n#1378 @ 558,400: 22x21\n#1379 @ 950,919: 10x18\n#1380 @ 548,404: 13x26\n#1381 @ 188,189: 14x17\n#1382 @ 819,709: 12x13\n#1383 @ 951,91: 19x21\n#1384 @ 300,821: 11x9\n#1385 @ 273,431: 18x17\n#1386 @ 300,663: 13x13\n#1387 @ 211,905: 16x18\n#1388 @ 295,395: 19x22\n#1389 @ 526,861: 11x11\n#1390 @ 306,777: 22x15\n#1391 @ 973,296: 12x25\n#1392 @ 570,109: 10x19\n#1393 @ 231,543: 27x12\n#1394 @ 265,851: 12x27\n#1395 @ 484,289: 14x25\n#1396 @ 773,636: 14x27\n#1397 @ 435,385: 11x19\n#1398 @ 960,769: 19x14\n#1399 @ 55,92: 19x22\n#1400 @ 554,922: 28x10\n#1401 @ 139,196: 16x28\n#1402 @ 112,380: 28x16\n#1403 @ 635,869: 16x13\n#1404 @ 259,924: 10x27\n#1405 @ 898,980: 26x16\n#1406 @ 75,197: 12x16\n#1407 @ 632,681: 10x11\n#1408 @ 434,974: 14x14\n#1409 @ 782,377: 24x29"
  |> Js.String.split("\n");