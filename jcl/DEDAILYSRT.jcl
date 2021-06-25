*                                                                       00001   
* DFSORT CONTROL CARDS USED FOR THE LOCAL DAILIES CONVERSIONS.          00002   
*                                                                       00003   
* 1:  MAINTAINS PROPER SORT ORDER WHEN SORTING MULTIPLE MARKETS.        00004   
* 2:  MANIPULATE PROGRAM UPDATE RECORDS INTO PROGRAM FINAL RECORDS.     00005   
*                                                                       00006   
* SEE DEPREDALY FOR THE *INPUT* RECORD LAYOUTS READ BY THIS UTILITY.    00007   
*   DEPREDALY PARSES THE TAB-DELIMITED NIELSEN RECORDS AND PRODUCES A   00008   
*   FIXED-COLUMN FILE FOR SORTING BY THIS UTILITY.                      00009   
* SEE DEDAILYD FOR THE *OUTPUT* RECORD DSECTS PRODUCED BY THIS UTILITY. 00010   
*   I.E., THE RECORDS PRODUCED HERE ARE THE INPUT TO THE CONVERSIONS.   00011   
*                                                                       00012   
* *** ANY CHANGES TO THE LOCAL DAILIES FILE FORMAT MAY NECESSITATE      00013   
* *** CHANGES TO DEDAILYD, DEPREDALY, AND TO THIS MODULE ALSO.          00014   
*                                                                       00015   
*=====================================================================* 00016   
*                                                                     * 00017   
* DEIS JAN/2020:                                                      * 00018   
*                                                                     * 00019   
*  THE CONTROL STATEMENTS IN THIS MODULE ARE CHALLENGING TO           * 00020   
*  UNDERSTAND AND MAINTAIN, BECAUSE THEY USE HARD-CODED COLUMN        * 00021   
*  NUMBERS AND LENGTHS FOR EACH FIELD (INSTEAD OF DFSORT SYMBOLS).    * 00022   
*                                                                     * 00023   
*  ANY FUTURE REWRITE OF THIS ARCHITECTURE SHOULD USE THE DEDAILYD    * 00024   
*  DSECT TO CONSTRUCT DFSORT SYMBOLS FOR USE BY THIS MODULE. IN       * 00025   
*  ADDITION, THE PARSE/BUILD/SORT LOGIC SHOULD ALL BE IN ONE PLACE:   * 00026   
*  EITHER IN DEPREDALY, OR IN AN ICETOOL/DFSORT UTILITY. THE WAY IT   * 00027   
*  IS NOW (BROKEN UP BETWEEN DEPREDALY AND THIS MODULE) MAKES IT      * 00028   
*  DIFFICULT TO REFER TO THE FIELDS IN EACH RECORD, DUE TO THE LACK   * 00029   
*  OF COMMON SYMBOLS.                                                 * 00030   
*                                                                     * 00031   
*  DEIS MADE EXTENSIVE (BUT TRANSPARENT) CHANGES TO THIS MODULE IN    * 00032   
*  AN ATTEMPT TO MAKE IT MORE UNDERSTANDABLE. TWO IMPORTANT THINGS    * 00033   
*  WERE DONE:                                                         * 00034   
*    1. MOST INREC/OUTREC FIELDS ARE BUILT INDIVIDUALLY, EVEN WHEN    * 00035   
*       THEY ARE CONTIGUOUS.                                          * 00036   
*    2. COMMENTS WERE ADDED TO DESCRIBE EACH FIELD.                   * 00037   
*  THESE CHANGES ALTERED VIRTUALLY EVERY STATEMENT IN THIS MODULE,    * 00038   
*  THEREBY RENDERING THE LEVEL STAMP HISTORY USELESS. ACCORDINGLY,    * 00039   
*  THE MODULE WAS LEVELED BACK TO LEVEL 1.                            * 00040   
*                                                                     * 00041   
*=====================================================================* 00042   
*                                                                       00043   
  INREC IFTHEN=(WHEN=(17,2,CH,EQ,C'01'),    MARKET HEADER               00044   
                BUILD=(1,4,                   RDW                       00045   
                       5:5,12,                KEY: MKTABBR,B|C,DATE     00046   
                       17:C'AA',              (FOR SORTING)             00047   
                       19:19)),               IDMVER + REMAINING FIELDS 00048   
        IFTHEN=(WHEN=(17,2,CH,EQ,C'02'),    DISTRIBUTOR HEADER          00049   
                BUILD=(1,4,                   RDW                       00050   
                       5:5,12,                KEY: MKTABBR,B|C,DATE     00051   
                       17:C'BB',              (FOR SORTING)             00052   
                       19:82,5,               IDRSTCDE                  00053   
                       24:24,3,               IDRORIG                   00054   
                       27:27,55,              IDRMKT                    00055   
                       82:5X,                 (FOR FUTURE USE)          00056   
                       87:87)),               IDRCALL + REMAINING FLDS  00057   
        IFTHEN=(WHEN=(17,2,CH,EQ,C'03',OR,  DEMOGRAPHIC HEADER          00058   
                      17,2,CH,EQ,C'04',OR,  MARKET UNIV. EST. HEADER    00059   
                      17,2,CH,EQ,C'05',OR,  MARKET INTAB RECORD         00060   
                      17,2,CH,EQ,C'06'),    EXCLUSION RECORD            00061   
                BUILD=(1,4,                   RDW                       00062   
                       5:5,12,                KEY: MKTABBR,B|C,DATE     00063   
                       17:C'C',               (FOR SORTING)             00064   
                       18:18,1,               RECTYPE+1(1)              00065   
                       19:19)),               ALL REMAINING FIELDS      00066   
        IFTHEN=(WHEN=(17,2,CH,EQ,C'07'),    QUARTER HOUR HUT/PUT        00067   
                BUILD=(1,4,                   RDW                       00068   
                       5:5,12,                KEY: MKTABBR,B|C,DATE     00069   
                       17:26,18,              IDHSTDT(18)               00070   
                       35:C'DD',              (SORT 07 RECS BEFORE 08)  00071   
                       37:19,5,               IDHSTAC                   00072   
                       42:2X,                 (FOR FUTURE USE)          00073   
                       44:44,2,               IDHSTDT+18(2)             00074   
                       46:46)),               IDHEXC + ALL REMAINING    00075   
        IFTHEN=(WHEN=(17,2,CH,EQ,C'08'),    QUARTER HOUR DISTRIBUTOR    00076   
                BUILD=(1,4,                   RDW                       00077   
                       5:5,12,                KEY: MKTABBR,B|C,DATE     00078   
                       17:25,18,              IDQSTDT(18)               00079   
                       35:C'EE',              (SORT 08 RECS AFTER 07)   00080   
                       37:19,5,               IDQSTAC                   00081   
                       42:1X,                 (FOR FUTURE USE)          00082   
                       43:43,2,               IDQSTDT+18(2)             00083   
                       45:45)),               IDQEXC + ALL REMAINING    00084   
        IFTHEN=(WHEN=(17,2,CH,EQ,C'21'),    MARKET HEADER (PROG. NM)    00085   
                BUILD=(1,4,                   RDW                       00086   
                       5:5,12,                KEY: MKTABBR,B|C,DATE     00087   
                       17:C'FF',              (FOR SORTING)             00088   
                       19:19,3,               IDUHMKC                   00089   
                       22:22,3,               IDUHDMA                   00090   
                       25:25,25,              IDUHGEO                   00091   
                       50:1X,                 (FOR FUTURE USE)          00092   
                       51:51,6,               IDUHABR                   00093   
                       57:71,10,              IDUHDAT                   00094   
                       67:57,5,               IDUHRTG                   00095   
                       72:C'UPDATE',                                    00096   
                       78:62,9,               IDUHDST                   00097   
                       87:87)),               DEIS: ??????????          00098   
        IFTHEN=(WHEN=(17,2,CH,EQ,C'22'),    PROGRAM NAME (UPDATE)       00099   
                BUILD=(1,4,                   RDW                       00100   
                       5:5,12,                KEY: MKTABBR,B|C,DATE     00101   
                       17:C'GG',              (FOR SORTING)             00102   
                       19:19,5,               IDPUSTC                   00103   
                       24:1X,                 (FOR FUTURE USE: BLANK)   00104   
                       25:35))                IDPUSTDT + ALL REMAINING  00105   
*                                                                       00106   
* THE SORT KEY BEGINS WITH:                                             00107   
*  MARKET ABBREVIATION (3)                                              00108   
*  DISTRIBUTOR TYPE ("B" OR "C") (1)                                    00109   
*  DATA DATE (YYYYMMDD) (8)                                             00110   
* THIS SORT KEY PREFIX GUARANTEES THAT THE MARKETS ARE SORTED IN THE    00111   
* SEQUENCE THAT THE CONVERSION "I" PHASES EXPECT.                       00112   
*                                                                       00113   
* FOR MOST RECORD TYPES, THE NEXT SORT FIELD IS HARD-CODED TO FORCE THE 00114   
* RECORDS TO STAY IN SEQUENCE BY TYPE.                                  00115   
* HOWEVER: FOR THE "07" (HUT/PUT) AND "08" (DEMO) RECORDS, THE KEY      00116   
* IS DESIGNED TO KEEP THE "07" AND "08" RECORDS TOGETHER BY QUARTER-    00117   
* HOUR, WITH THE "07" RECORDS FOR THE QUARTER-HOUR ALWAYS SORTING AHEAD 00118   
* OF THEIR ASSOCIATED "08" RECORDS.                                     00119   
*                                                                       00120   
* DEIS SAYS (JAN/2020): I CANNOT FIGURE OUT WHERE THE SORT KEY LENGTH   00121   
* COMES FROM. I SUSPECT THIS IS SLIGHTLY LONGER THAN IT REALLY          00122   
* NEEDS TO BE, BUT THAT IS CERTAINLY BETTER THAN ITS BEING TOO SHORT.   00123   
*                                                                       00124   
  SORT FIELDS=(5,42,BI,A),EQUALS                                        00125   
*                                                                       00126   
  OUTREC IFTHEN=(WHEN=(17,2,CH,EQ,C'AA'),                               00127   
                 BUILD=(1,4,                   RDW                      00128   
                        05:C'01',                                       00129   
                        07:19)),               IDMVER + REMAINDER       00130   
         IFTHEN=(WHEN=(17,2,CH,EQ,C'BB'),                               00131   
                 BUILD=(1,4,                   RDW                      00132   
                        05:C'02',                                       00133   
                        07:5X,                 (FOR FUTURE USE)         00134   
                        12:24,3,               IDRORIG                  00135   
                        15:27,55,              IDRMKT                   00136   
                        70:19,5,               IDRSTCDE                 00137   
                        75:87)),               IDRCALL + REMAINING FLDS 00138   
         IFTHEN=(WHEN=(17,2,CH,EQ,C'C3',OR,     '03' RECORDS            00139   
                       17,2,CH,EQ,C'C4',OR,     '04' RECORDS            00140   
                       17,2,CH,EQ,C'C5',OR,     '05' RECORDS            00141   
                       17,2,CH,EQ,C'C6'),       '06' RECORDS            00142   
                 BUILD=(1,4,                   RDW                      00143   
                        05:C'0',               RECTYPE(1)               00144   
                        06:18,1,               RECTYPE+1(1)             00145   
                        07:19)),               ALL REMAINING FIELDS     00146   
         IFTHEN=(WHEN=(35,2,CH,EQ,C'DD'),                               00147   
                 BUILD=(1,4,                   RDW                      00148   
                        05:C'07',                                       00149   
                        07:37,5,               IDHSTAC                  00150   
                        12:2X,                 (FOR FUTURE USE: BLANKS) 00151   
                        14:17,18,              IDHSTDT(18)              00152   
                        32:44,2,               IDHSTDT+18(2)            00153   
                        34:46)),               IDHEXC + ALL REMAINING   00154   
         IFTHEN=(WHEN=(35,2,CH,EQ,C'EE'),                               00155   
                 BUILD=(1,4,                   RDW                      00156   
                        05:C'08',                                       00157   
                        07:37,5,               IDQSTAC                  00158   
                        12:1X,                 (FOR FUTURE USE: BLANK)  00159   
                        13:1X,                 ???                      00160   
                        14:17,18,              IDQSTDT(18)              00161   
                        32:1X,                                          00162   
                        33:44,1,                                        00163   
                        34:45)),               IDQEXC + ALL REMAINING   00164   
         IFTHEN=(WHEN=(17,2,CH,EQ,C'FF'),                               00165   
                 BUILD=(1,4,                   RDW                      00166   
                        05:C'11',                                       00167   
                        07:19)),               ALL DATA FIELDS          00168   
         IFTHEN=(WHEN=(17,2,CH,EQ,C'GG'),                               00169   
                 BUILD=(1,4,                   RDW                      00170   
                        05:C'12',                                       00171   
                        07:19)),               ALL DATA FIELDS          00172   
         IFTHEN=(WHEN=NONE,                                             00173   
                 BUILD=(1,4,                   RDW                      00174   
                        05:17))                ALL DATA FIELDS          00175   
*                                                                       00176   
