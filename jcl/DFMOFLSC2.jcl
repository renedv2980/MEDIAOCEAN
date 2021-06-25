* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLSTI (ICETOOL).                              00003   
*                                                                       00004   
* REFORMAT EACH RESIZED RECORD TO ELIMINATE EXTRA BLANKS WHICH WERE AT  00005   
* THE END OF EACH ORIGINAL WRITER OUTPUT RECORD, THEREBY SQUEEZING THE  00006   
* RECORD SUCH THAT EACH FIELD IS DELIMITED BY THE FIXED STRING '" "'    00007   
* (DOUBLE-QUOTE/BLANK/DOUBLE-QUOTE). THE VERY FIRST AND VERY LAST       00008   
* DOUBLE-QUOTE OF EACH RECORD ARE ALSO REMOVED.                         00009   
*                                                                       00010   
* THESE COLUMN NUMBERS AND LENGTHS ARE DERIVED BY INSPECTION OF THE     00011   
* WRITER INPUT FILE.                                                    00012   
*                                                                       00013   
* NOTE: WE PUT THE AGENCY CODE IN FRONT OF EACH RECORD, SO WE CAN       00014   
*       CHECK IT AGAINST AN AGENCY-SPECIFIC CLIENT LIST LATER ON.       00015   
 INREC BUILD=(JP3,2,115,133,126,265,121,397,18,528:X)                   00016   
*                                                                       00017   
* AS A RESULT OF THE INREC ABOVE, WE CAN NOW REPLACE ALL OCCURRENCES    00018   
* OF THE STRING '" "' WITH A TAB DELIMITER. (THIS ASSUMES THAT THERE    00019   
* ARE NO LEGITIMATE ONE-BYTE OUTPUT FIELDS PRESENT IN THE ORIGINAL      00020   
* WRITER FILE.)                                                         00021   
 OUTREC FINDREP=(IN=(C'" "'),OUT=X'05')                                 00022   
*                                                                       00023   
