*          DATA SET GEDDTENG   AT LEVEL 001 AS OF 01/17/91                      
*PHASE T00D00                                                                   
         TITLE 'GENERAL TABLE DATA DICTIONARY - ENGLISH'                        
GEDDTENG CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'GEDDTENG'                                                    
         DC    AL4(BASEX-BASE+PANACEA)                                          
         DC    AL2((DDNDXX-DDNDX-2)/2)                                          
         DC    AL2(0)                                                           
         SPACE 2                                                                
DDNDX    DC    AL2(0)                                                           
*                                                                               
         DC    AL2(DD0001-BASE)    =BUY action indexed list                     
         DC    AL2(DD0002-BASE)    =BUY option indexed list                     
         DC    AL2(DD0003-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0004-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0005-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0006-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0007-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0008-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0009-BASE)    =BUY suboption v/l list                      
*                                                                               
         DC    AL2(DD0010-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0011-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0012-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0013-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0014-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0015-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0016-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0017-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0018-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0019-BASE)    =BUY suboption v/l list                      
*                                                                               
         DC    AL2(DD0020-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0021-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0022-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0023-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0024-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0025-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0026-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0027-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0028-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0029-BASE)    =BUY suboption v/l list                      
*                                                                               
         DC    AL2(DD0030-BASE)    =BUY suboption v/l list                      
         DC    AL2(DD0031-BASE)    don't use                                    
         DC    AL2(DD0032-BASE)    don't use                                    
         DC    AL2(DD0033-BASE)    don't use                                    
         DC    AL2(DD0034-BASE)    don't use                                    
         DC    AL2(DD0035-BASE)    don't use                                    
         DC    AL2(DD0036-BASE)    =BUY 'OTHER' fld keyword v/l list            
*                                                                               
DDNDXX   DC    AL2(65535)                                                       
         SPACE 2                                                                
DDTXT    DS    0C                                                               
         EJECT                                                                  
* $BUY ACTION KEYWORDS - MAPPED 1-TO-1 ONTO MEBUY00 ACTTAB                      
*                                                                               
DD0001   DC    AL1(1,12),CL12'ME##BACT'                                         
         DC    C'A AMEND     ' 01                                               
         DC    C'APAPPROVE   '                                                  
         DC    C'APAPPROVE   '                                                  
         DC    C'B BUY       '                                                  
         DC    C'BDBUDGET    ' 05                                               
         DC    C'C CANCEL    '                                                  
         DC    C'CSCANCSERIAL'                                                  
         DC    C'CTCANCEL TRN'                                                  
         DC    C'COCOSTS     '                                                  
         DC    C'Z BATCH BUY ' 10                                               
         DC    C'ZABATCH AMND'                                                  
         DC    C'ZBBATCH LIVE'                                                  
         DC    C'ZDBATCH DEL '                                                  
         DC    C'ZEBATCH ENQ '                                                  
         DC    C'ZHBATCH HOLD' 15                                               
         DC    C'ZNBATCH END '                                                  
         DC    C'D DEAL      '                                                  
         DC    C'E ENQUIRE   '                                                  
         DC    C'E ENQUIRE   '                                                  
         DC    C'ECENQ COMP  ' 20                                               
         DC    C'G GHI       '                                                  
         DC    C'H HOLD      '                                                  
         DC    C'L LIVE      '                                                  
         DC    C'LSLIST      '                                                  
         DC    C'LOLOCK      ' 25                                               
         DC    C'MAMULTIAMEND'                                                  
         DC    C'MAMULTIAMEND'                                                  
         DC    C'MBMULTIBUY  '                                                  
         DC    C'MCMULTICANC '                                                  
         DC    C'MCMULTICANC ' 30                                               
         DC    C'MLMULTILIVE '                                                  
         DC    C'MLMULTILIVE '                                                  
         DC    C'MPMULTIPAY  '                                                  
         DC    C'MPMULTIPAY  '                                                  
         DC    C'O OVERRIDE  ' 35                                               
         DC    C'P CLEAR     '                                                  
         DC    C'P CLEAR     '                                                  
         DC    C'PAPAY AMEND '                                                  
         DC    C'PAPAY AMEND '                                                  
         DC    C'PRPRINT     ' 40                                               
         DC    C'PRPRINT     '                                                  
         DC    C'PRPRINT     '                                                  
         DC    C'PRPRINT     '                                                  
         DC    C'PRPRINT     '                                                  
         DC    C'QUQUERY     ' 45                                               
         DC    C'QUQUERY     '                                                  
         DC    C'R RANGE     '                                                  
         DC    C'RCRANGECANC '                                                  
         DC    C'RLRANGELIVE '                                                  
         DC    C'RERESPONSE  ' 50                                               
         DC    C'RDRESPDETAIL'                                                  
         DC    C'RRRESPRATES '                                                  
         DC    C'S SPECIAL   '                                                  
         DC    C'SESELECT    '                                                  
         DC    C'SUSUMMARY   ' 55                                               
         DC    C'T TRNS UPDTE'                                                  
         DC    C'TDTRNS DISP '                                                  
         DC    C'TETRNS END  '                                                  
         DC    C'TMTRNS MATCH'                                                  
         DC    C'TUTRNS UNEND' 60                                               
         DC    C'TVTRN VERIFY'                                                  
         DC    C'U UNCLEAR   '                                                  
         DC    C'U UNCLEAR   '                                                  
         DC    C'UAUNCLR AMND'                                                  
         DC    C'UAUNCLR AMND' 65                                               
         DC    C'UGUNGHI     '                                                  
         DC    C'UHUNHOLD    '                                                  
         DC    C'ULUNLOCK    '                                                  
         DC    C'UOUNOVERRIDE'                                                  
         DC    C'UPUNAPPROVE ' 70                                               
         DC    C'UQUNQUERY   '                                                  
         DC    C'UQUNQUERY   '                                                  
         DC    C'UTUNCNCL TRN'                                                  
         DC    C'V VOUCHER   '                                                  
         DC    C'V VOUCHER   ' 75                                               
         DC    C'V VOUCHER   '                                                  
         DC    C'V VOUCHER   '                                                  
         DC    C'VLVOUCHLIST '                                                  
         DC    C'VMVERFY MTCH'                                                  
         DC    C'VPVERFY PRNT' 80                                               
         DC    C'VSVOUCSERIAL'                                                  
         DC    C'W WISEBUY   '                                                  
         DC    C'WEWEEK      '                                                  
         DC    C'X EXTRASPECL'                                                  
         DC    C'XCXCOMMENT  ' 85                                               
         DC    C'CBCOMBI     '                                                  
         DC    C'CHCLEARHOLD '                                                  
         DC    C'SASPEC AMEND'                                                  
         DC    C'SMSPECL MA  '                                                  
         DC    C'SMSPECL MA  ' 90                                               
         DC    C'LBLISTBUY   '     ** GER ONLY **                               
         DC    C'            '     SPARE FOR PATCH                              
         DC    C'            '     END OF TABLE ENTRY                           
*                                                                               
* $BUY OPTION KEYWORDS - MAPPED 1-TO-1 ONTO MEBUY02 OPTTAB                      
*                                                                               
DD0002   DC    AL1(1,11),CL11'ME##BOPT'                                         
         DC    C'PRINT      '      01                                           
         DC    C'PRINT      '                                                   
         DC    C'PRINT      '                                                   
         DC    C'PRINT      '                                                   
         DC    C'PRINT      '      05                                           
         DC    C'CURRENCY   '                                                   
         DC    C'AGYCOMM    '                                                   
         DC    C'INCENTIV   '                                                   
         DC    C'PARITY     '                                                   
         DC    C'POOL       '      10                                           
         DC    C'CHANGE  CHG'                                                   
         DC    C'COST       '                                                   
         DC    C'COST       '                                                   
         DC    C'PAYSER     '                                                   
         DC    C'BILL       '      15                                           
         DC    C'1          '                                                   
         DC    C'1          '                                                   
         DC    C'1          '                                                   
         DC    C'1          '                                                   
         DC    C'1          '      20                                           
         DC    C'1          '                                                   
         DC    C'1          '                                                   
         DC    C'1          '                                                   
         DC    C'1          '                                                   
         DC    C'1          '      25                                           
         DC    C'2          '                                                   
         DC    C'2          '                                                   
         DC    C'2          '                                                   
         DC    C'2          '                                                   
         DC    C'2          '      30                                           
         DC    C'2          '                                                   
         DC    C'2          '                                                   
         DC    C'2          '                                                   
         DC    C'2          '                                                   
         DC    C'2          '      35                                           
         DC    C'3          '                                                   
         DC    C'3          '                                                   
         DC    C'3          '                                                   
         DC    C'3          '                                                   
         DC    C'3          '      40                                           
         DC    C'3          '                                                   
         DC    C'3          '                                                   
         DC    C'3          '                                                   
         DC    C'3          '                                                   
         DC    C'3          '      45                                           
         DC    C'OLD        '                                                   
         DC    C'NEW        '                                                   
         DC    C'COMMENT    '                                                   
         DC    C'DELETED    '                                                   
         DC    C'DEST    DE '      50                                           
         DC    C'DEST       '                                                   
         DC    C'ACTION     '                                                   
         DC    C'DRAFT      '                                                   
         DC    C'DRAFT      '                                                   
         DC    C'DATE       '      55                                           
         DC    C'CLEARED    '                                                   
         DC    C'CLEARED    '                                                   
         DC    C'LOCKED     '                                                   
         DC    C'FILM       '                                                   
         DC    C'GHI        '      60                                           
         DC    C'MATCHED    '                                                   
         DC    C'NETWORK    '                                                   
         DC    C'POOL       '                                                   
         DC    C'SECONDS    '                                                   
         DC    C'SHOW       '      65                                           
         DC    C'SPECIAL    '                                                   
         DC    C'TRANS      '                                                   
         DC    C'UNIVERSEUNV'                                                   
         DC    C'VERIFY     '                                                   
         DC    C'MATCHED    '      70                                           
         DC    C'VERIFIED   '                                                   
         DC    C'VOUCHED    '                                                   
         DC    C'INVOICE    '                                                   
         DC    C'TOTALS     '                                                   
         DC    C'QUERY      '      75                                           
         DC    C'STATUS     '                                                   
         DC    C'CLIENT     '                                                   
         DC    C'PRODUCT    '                                                   
         DC    C'CAMPAIGN   '                                                   
         DC    C'ADVERT     '      80                                           
         DC    C'FILM       '                                                   
         DC    C'ADVERT     '                                                   
         DC    C'COUNT      '                                                   
         DC    C'ITEM       '                                                   
         DC    C'REF        '      85                                           
         DC    C'SUPPLIERSTA'                                                   
         DC    C'FOLIO      '                                                   
         DC    C'FOLIO      '                                                   
         DC    C'1          '                                                   
         DC    C'2          '      90                                           
         DC    C'3          '                                                   
         DC    C'BUDGET     '                                                   
         DC    C'DATE       '                                                   
         DC    C'DATE       '                                                   
         DC    C'DAYS       '      95                                           
         DC    C'ADVPAID AP '                                                   
         DC    C'TOGETHER   '                                                   
         DC    C'TOGETHER   '                                                   
         DC    C'START      '                                                   
         DC    C'END        '      100                                          
         DC    C'REUSE      '                                                   
         DC    C'           '      SPARE FOR PATCH                              
         DC    C'           '      END OF TABLE ENTRY                           
*                                                                               
* $BUY OPTION VALUE KEYWORDS - SEE MEBUY02 OPTTAB                               
*                                                                               
DD0003   DC    AL1(1,01),C'#',AL1(3,1)                                          
         DC    C'YESPNO N',X'00'                                                
*                                                                               
DD0004   DC    AL1(1,01),C'#',AL1(8,1)                                          
         DC    C'BUDGET  2LOCAL   LSECOND  2STERLING',X'00',X'00'               
*                                                                               
DD0005   DC    AL1(1,01),C'#',AL1(8,1)                                          
         DC    C'BUYING  BCREATIVECTOTAL   TBILLABLE',X'00',X'00'               
*                                                                               
DD0006   DC    AL1(1,01),C'#',AL1(7,1)                                          
         DC    C'GROSS  GNET    NCLIENT CBILL   B',X'00'                        
*                                                                               
DD0007   DC    AL1(1,01),C'#',AL1(5,1)                                          
         DC    C'GROSSGNET  N',X'00'                                            
*                                                                               
DD0008   DC    AL1(1,01),C'#',AL1(11,1)                                         
         DC    C'YES        YNO         NPROVISIONALP',X'00'                    
*                                                                               
DD0009   DC    AL1(1,01),C'#',AL1(3,1)                                          
         DC    C'ALLA',X'00'                                                    
*                                                                               
DD0010   DC    AL1(1,01),C'#',AL1(3,1)                                          
         DC    C'YESY',X'00'                                                    
*                                                                               
DD0011   DC    AL1(1,01),C'#',AL1(3,1)                                          
         DC    C'YESYNO NALLA',X'00'                                            
*                                                                               
DD0012   DC    AL1(1,01),C'#',AL1(2,1)                                          
         DC    C'NON',X'00'                                                     
*                                                                               
DD0013   DC    AL1(1,01),C'#',AL1(7,1)                                          
         DC    C'ADD    ACHANGE CDISPLAYDREMOVE R',X'00'                        
*                                                                               
DD0014   DC    AL1(1,01),C'#',AL1(7,1)                                          
         DC    C'FILM   FSERIAL SVOLDISCV',X'00'                                
*                                                                               
DD0015   DC    AL1(1,01),C'#',AL1(8,1)                                          
         DC    C'ALL     ACHECKED CTNOTRACETNOTRACE TQUERY   Q'                 
         DC    C'REJECT  RVOUCHED V',X'00'                                      
*                                                                               
DD0016   DC    AL1(1,01),C'#',AL1(7,1)                                          
         DC    C'APPEND AREPLACER',X'00'                                        
*                                                                               
DD0017   DC    AL1(1,01),C'#',AL1(3,1)                                          
         DC    C'TVRT',X'00'                                                    
*                                                                               
DD0018   DC    AL1(1,01),C'#',AL1(8,2)       UK TV                              
         DC    C'CARDRATECRDATE    DTDAY     DYFILM    FIMEDISC  ME'            
         DC    C'RATE    RAREMARKS RESECONDS SETIME    TITA1     T1'            
         DC    C'TA2     T2VATIN   VIVATOUT  VO',X'00'                          
*                                                                               
DD0019   DC    AL1(1,01),C'#',AL1(8,2)       UK RADIO                           
         DC    C'CARDRATECRDATE    DTDAY     DYMEDISC  MERATE    RA'            
         DC    C'REMARKS RESECONDS SETIME    TITA1     T1TA2     T2'            
         DC    C'VATIN   VIVATOUT  VO',X'00'                                    
*                                                                               
DD0020   DC    AL1(1,01),C'#',AL1(8,2)       UK PRESS                           
         DC    C'ADVERT  AD'                                                    
         DC    C'CARDRATECRCATEGORYCACOLOUR  CODATE    DTDAY     DY'            
         DC    C'MEDISC  MEPOSITIONPORATE    RAREMARKS RESPACE   SP'            
         DC    C'VATIN   VIVATOUT  VO',X'00'                                    
*                                                                               
DD0021   DC    AL1(1,01),C'#',AL1(8,2)       UK INTERNATIONAL BDCAST            
         DC    C'CARDRATECRCATEGORYCADATE    DTDAY     DYMEDISC  ME'            
         DC    C'RATE    RAREMARKS RESECONDS SETIME    TI',X'00'                
*                                                                               
DD0022   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK BROADCAST                   
         DC    C'ADVERT  ADBASIC   BPBLOCK   BLCARD    RCDATE    DT'            
         DC    C'REBADJ  RREBATE  R1REBATE2 R2SECONDS SESPOTS   NS'            
         DC    C'STATION STTIME    TI',X'00'                                    
*                                                                               
DD0023   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK NEWSPAPERS                  
         DC    C'ADVERT  ADBASIC   BPCARD    RCCOLOUR  COCONTRACTCN'            
         DC    C'DATE    DTREBADJ  RREBATE1 R1REBATE2 R2REBATE3 R3'            
         DC    C'SPACE   SPUNITPR  UPVATOUT  VO',X'00'                          
*                                                                               
DD0024   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK OTHER PRINT                 
         DC    C'ADVERT  ADBASIC   BPBLEED   ANCARD    RCCOLOUR  CO'            
         DC    C'CONTRACTCNDATE    DTDPS     BDISSUE   ISREBADJ  R'            
         DC    C'REBATE1 R1REBATE2 R2REBATE3 R3SPACE   SPUNITPR  UP'            
         DC    C'VATOUT  VO',X'00'                                              
*                                                                               
DD0025   DC    AL1(1,01),C'#',AL1(4,1)                                          
         DC    C'BILLSPAY MALL A',X'00'                                         
*                                                                               
DD0026   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK RADIO                       
         DC    C'ADVERT  ADBASIC   BPCARD    RCDATE    DTREBATE1 R1'            
         DC    C'REBATE2 R2SECONDS SESTATION STTIME    TIUNITPR  UP'            
         DC    X'00'                                                            
*                                                                               
DD0027   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK CINEMA                      
         DC    C'ADVERT  ADDATE    DTMETERS  MTRATE    BPRAB1    R1'            
         DC    C'WEEKS   WK',X'00'                                              
*                                                                               
DD0028   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK POSTERS                     
         DC    C'ADVERT  ADBASIC   BPBLOCK   BKDAYPRICEDPDECADE  DE'            
         DC    C'REBATE1 R1REBATE2 R2SIZE    SPSITES   SI',X'00'                
*                                                                               
DD0029   DC    AL1(1,01),C'#',AL1(6,1)       NETWORK=ACTIVE/ALL                 
         DC    C'ACTIVE',AL1(0),C'ALL   ',AL1(2),X'00'                          
*                                                                               
DD0030   DC    AL1(1,01),C'#',AL1(1,1)                                          
*                                                                               
DD0031   DC    AL1(1,01),C'#',AL1(1,1)                                          
DD0032   DC    AL1(1,01),C'#',AL1(1,1)                                          
DD0033   DC    AL1(1,01),C'#',AL1(1,1)                                          
DD0034   DC    AL1(1,01),C'#',AL1(1,1)                                          
DD0035   DC    AL1(1,01),C'#',AL1(1,1)                                          
*                                                                               
* $BUY OTHERS KEYWORDS - MAPPED 1-TO-1 TO MEBUY18 OTHERTAB                      
*                                                                               
DD0036   DC    AL1(1,12),CL12'ME##BOT'                                          
         DC    C'ACCOUNT ACC '     01                                           
         DC    C'AGY         '                                                  
         DC    C'ASBOF       '                                                  
         DC    C'AUD     AU  '                                                  
         DC    C'BOOKING BOOK'     05                                           
         DC    C'CDBILL  CDB '                                                  
         DC    C'CDPAYST CDPS'                                                  
         DC    C'CONTACT     '                                                  
         DC    C'DAYPART DPT '                                                  
         DC    C'INCENT  INC '     10                                           
         DC    C'FORMULA     '                                                  
         DC    C'GUAR        '                                                  
         DC    C'LEVY        '                                                  
         DC    C'LOCAL   LOC '                                                  
         DC    C'MED         '     15                                           
         DC    C'NATIONALNAT '                                                  
         DC    C'NOTES       '                                                  
         DC    C'NUMBER  NUM '                                                  
         DC    C'ORDER       '                                                  
         DC    C'OUTLET  OUT '     20                                           
         DC    C'PACKAGE PACK'                                                  
         DC    C'PARITY  PAR '                                                  
         DC    C'CDPAYDD CDPD'                                                  
         DC    C'ROTATIONROT '                                                  
         DC    C'STATION STA '     25                                           
         DC    C'SUP         '                                                  
         DC    C'VOL         '                                                  
         DC    C'VATIN   VATI'                                                  
         DC    C'VATOUT  VATO'                                                  
         DC    C'RCD         '     30                                           
         DC    C'REBADJ      '                                                  
         DC    C'            '     SPARE FOR PATCH                              
         DC    C'            '     END OF TABLE ENTRY                           
*                                                                               
DDTXTX   DC    AL2(65535)                                                       
         SPACE 1                                                                
PANACEA  EQU   24                                                               
         SPACE 1                                                                
         DC    ((((((*-BASE)/0512)+1)*0512)-PANACEA)-(*-BASE))X'00'             
BASEX    DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001GEDDTENG  01/17/91'                                      
         END                                                                    
