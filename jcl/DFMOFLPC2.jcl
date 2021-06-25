* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLPTI (ICETOOL).                              00003   
*                                                                       00004   
* REFORMAT EACH RESIZED RECORD TO ELIMINATE EXTRA BLANKS WHICH WERE AT  00005   
* THE END OF EACH ORIGINAL WRITER OUTPUT RECORD, THEREBY SQUEEZING THE  00006   
* RECORD SUCH THAT EACH FIELD IS DELIMITED BY THE FIXED STRING '" "'    00007   
* (DOUBLE-QUOTE/BLANK/DOUBLE-QUOTE). THE VERY FIRST AND VERY LAST       00008   
* DOUBLE-QUOTE OF EACH RECORD ARE ALSO REMOVED.                         00009   
*                                                                       00010   
* THESE COLUMN NUMBERS AND LENGTHS ARE DERIVED BY INSPECTION OF THE     00011   
* WRITER INPUT FILE.                                                    00012   
*INREC BUILD=(2,130,133,127,265,115,396:X)                              00013**2
 INREC BUILD=(2,130,133,127,265,126,396:X)                              00014**3
*                                                                       00015   
* AS A RESULT OF THE INREC ABOVE, WE CAN NOW REPLACE ALL OCCURRENCES    00016   
* OF THE STRING '" "' WITH A TAB DELIMITER. (THIS ASSUMES THAT THERE    00017   
* ARE NO LEGITIMATE ONE-BYTE OUTPUT FIELDS PRESENT IN THE ORIGINAL      00018   
* WRITER FILE.)                                                         00019   
 OUTREC FINDREP=(IN=(C'" "'),OUT=X'05')                                 00020   
*                                                                       00021   
