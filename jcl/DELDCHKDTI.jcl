* INPUT:  TAPEOUT (THE DUMPED DIRECTORY)                                00001   
* OUTPUT: SHOWDUPS SYSOUT DATASET LISTS THE PROBLEMATIC KEYS.           00002   
* THIS UTILITY ALSO REQUIRES PANVALET MEMBER DELDCHKDC1.                00003   
*                                                                       00004   
*********************************************************************** 00005   
*** TOOLIN ***                                                          00006   
*********************************************************************** 00007   
*                                                                       00008   
* FIND DUMPED DIRECTORY RECORDS WITH DUPLICATE DISK ADDRESSES.          00009   
*                                                                       00010   
SELECT FROM(TAPEOUT) TO(DUPDIRS) ON(23,5,BI) ALLDUPS USING(COP1)        00011   
*                                                                       00012   
* PRINT A LISTING OF THE DUPLICATE KEYS FOR ANALYSIS.                   00013   
* THE ABSENCE OF A TITLE AND COLUMN HEADINGS IS DELIBERATE. THAT WAY,   00014   
* IF THERE ARE NO RECORDS TO PRINT, THE SHOWDUPS SYSOUT DATASET WON'T   00015   
* APPEAR IN THE (E)JES OUTPUT AT ALL.                                   00016   
*                                                                       00017   
DISPLAY FROM(DUPDIRS) LIST(SHOWDUPS) -                                  00018   
   BLANK -                                                              00019   
   NOHEADER -                                                           00020   
   ON(5,23,HEX) -                                                       00021   
   ON(5,23,CH)                                                          00022   
