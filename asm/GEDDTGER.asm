*          DATA SET GEDDTGER   AT LEVEL 001 AS OF 01/17/91                      
*PHASE T00D03                                                                   
         TITLE 'GENERAL TABLE DATA DICTIONARY - GERMAN'                         
GEDDTGER CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'GEDDTGER'                                                    
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
         DC    C'NNDERN    ' 01                                               
         DC    C'RFR-FREIGABE'                                                  
         DC    C'RFR-FREIGABE'                                                  
         DC    C'B BUCHEN    '                                                  
         DC    C'BDBUDGET    ' 05  **UK ONLY**                                  
         DC    C'STSTORNO    '                                                  
         DC    C'SNSTO-LFDNR '                                                  
         DC    C'CTCANCEL TRN'     **UK ONLY**                                  
         DC    C'KOKOSTEN    '                                                  
         DC    C'D BATCH BUY ' 10  **UK ONLY**                                  
         DC    C'DABATCH AMND'     **UK ONLY**                                  
         DC    C'DBBATCH LIVE'     **UK ONLY**                                  
         DC    C'DDBATCH DEL '     **UK ONLY**                                  
         DC    C'DEBATCH ENQ '     **UK ONLY**                                  
         DC    C'DHBATCH HOLD' 15  **UK ONLY**                                  
         DC    C'DNBATCH END '     **UK ONLY**                                  
         DC    C'DLDEAL      '     **UK ONLY**                                  
         DC    C'ANANZEIGE   '                                                  
         DC    C'ANANZEIGE   '                                                  
         DC    C'ECENQ COMP  ' 20  **UK ONLY**                                  
         DC    C'G GHI       '     **UK ONLY**                                  
         DC    C'HAHALTEN    '                                                  
         DC    C'L LIVE      '                                                  
         DC    C'LILIST      '     **UK ONLY**                                  
         DC    C'LOLOCK      ' 25  **UK ONLY**                                  
         DC    C'MMULTIND  '                                                  
         DC    C'MMULTIND  '                                                  
         DC    C'MBMULTIBUY  '     **UK ONLY**                                  
         DC    C'MSMULTISTO  '                                                  
         DC    C'MSMULTISTO  ' 30                                               
         DC    C'MLMULTILIVE '                                                  
         DC    C'MLMULTILIVE '                                                  
         DC    C'MZMULTIZAHL '                                                  
         DC    C'MZMULTIZAHL '                                                  
         DC    C'O OVERRIDE  ' 35  **UK ONLY**                                  
         DC    C'ZFZAHLFREI  '                                                  
         DC    C'ZFZAHLFREI  '                                                  
         DC    C'ZZAHLND   '                                                  
         DC    C'ZZAHLND   '                                                  
         DC    C'DRDRUCK     ' 40                                               
         DC    C'DRDRUCK     '                                                  
         DC    C'DRDRUCK     '                                                  
         DC    C'DRDRUCK     '                                                  
         DC    C'DRDRUCK     '                                                  
         DC    C'REREKL      ' 45                                               
         DC    C'REREKL      '                                                  
         DC    C'R RANGE     '     **UK ONLY**                                  
         DC    C'RCRANGECANC '     **UK ONLY**                                  
         DC    C'RLRANGELIVE '     **UK ONLY**                                  
         DC    C'RERESPONSE  ' 50  **UK ONLY**                                  
         DC    C'RDRESPDETAIL'     **UK ONLY**                                  
         DC    C'RRRESPRATES '     **UK ONLY**                                  
         DC    C'SPSPECIAL   '                                                  
         DC    C'SESELECT    '     **UK ONLY**                                  
         DC    C'SUSUMMARY   ' 55  **UK ONLY**                                  
         DC    C'T TRNS UPDTE'     **UK ONLY**                                  
         DC    C'TDTRNS DISP '     **UK ONLY**                                  
         DC    C'TETRNS END  '     **UK ONLY**                                  
         DC    C'TMTRNS MATCH'     **UK ONLY**                                  
         DC    C'TUTRNS UNEND' 60  **UK ONLY**                                  
         DC    C'TVTRN VERIFY'     **UK ONLY**                                  
         DC    C'AUZF-AUFH   '                                                  
         DC    C'AUZF-AUFH   '                                                  
         DC    C'AAU-NDERN '                                                  
         DC    C'AAU-NDERN ' 65                                               
         DC    C'UGUNGHI     '     **UK ONLY**                                  
         DC    C'AHHALT-AUFH '                                                  
         DC    C'ULUNLOCK    '     **UK ONLY**                                  
         DC    C'UOUNOVERRIDE'     **UK ONLY**                                  
         DC    C'RARF-AUFH   ' 70                                               
         DC    C'ARREKL-AUFH '                                                  
         DC    C'ARREKL-AUFH '                                                  
         DC    C'UTUNCNCL TRN'     **UK ONLY**                                  
         DC    C'BEBELEG     '                                                  
         DC    C'BEBELEG     ' 75                                               
         DC    C'BEBELEG     '                                                  
         DC    C'BEBELEG     '                                                  
         DC    C'BLBEL-LISTE '                                                  
         DC    C'VMVERFY MTCH'     **UK ONLY**                                  
         DC    C'VPVERFY PRNT' 80  **UK ONLY**                                  
         DC    C'BNBEL-LFDNR '                                                  
         DC    C'W WISEBUY   '     **UK ONLY**                                  
         DC    C'WEWEEK      '     **UK ONLY**                                  
         DC    C'X EXTRASPECL'                                                  
         DC    C'XTEXTRATEXT ' 85                                               
         DC    C'KBKOMBI     '                                                  
         DC    C'ZHZAHLHALTEN'                                                  
         DC    C'SSPEZNDERN'                                                  
         DC    C'SMSPEZ-MULTI'                                                  
         DC    C'SMSPEZ-MULTI' 90                                               
         DC    C'LBLISTEBUCHE'     **GER ONLY**                                 
         DC    C'            '     SPARE FOR PATCH                              
         DC    C'            '     END OF TABLE ENTRY                           
*                                                                               
* $BUY OPTION KEYWORDS - MAPPED 1-TO-1 ONTO MEBUY02 OPTTAB                      
*                                                                               
DD0002   DC    AL1(1,11),CL11'ME##BOPT'                                         
         DC    C'DRUCK      '      01                                           
         DC    C'DRUCK      '                                                   
         DC    C'DRUCK      '                                                   
         DC    C'DRUCK      '                                                   
         DC    C'DRUCK      '      05                                           
         DC    C'WHRUNG    '                                                   
         DC    C'AGTPROV    '                                                   
         DC    C'INCENTIV   '                                                   
         DC    C'PARITY     '                                                   
         DC    C'POOL       '      10                                           
         DC    C'CHANGE  CHG'                                                   
         DC    C'KOSTEN     '                                                   
         DC    C'KOSTEN     '                                                   
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
         DC    C'ALT        '                                                   
         DC    C'NEU        '                                                   
         DC    C'KOMMENT    '                                                   
         DC    C'GEL\SCHT   '                                                   
         DC    C'DRUCKAUFDA '      50                                           
         DC    C'DRUCKAUFDA '                                                   
         DC    C'ACTION     '                                                   
         DC    C'PLANUNG    '                                                   
         DC    C'PLANUNG    '                                                   
         DC    C'DATUM      '      55                                           
         DC    C'GEPR!FT GP '                                                   
         DC    C'GEPR!FT GP '                                                   
         DC    C'GESPERRTGS '                                                   
         DC    C'FILM       '                                                   
         DC    C'GHI        '      60                                           
         DC    C'MATCHED    '                                                   
         DC    C'NETWORK    '                                                   
         DC    C'POOL       '                                                   
         DC    C'SEKUNDEN   '                                                   
         DC    C'SHOW       '      65                                           
         DC    C'SPECIAL    '                                                   
         DC    C'TRANS      '                                                   
         DC    C'UNIVERSEUNV'                                                   
         DC    C'VERIFY     '                                                   
         DC    C'MATCHED    '      70                                           
         DC    C'VERIFIED   '                                                   
         DC    C'BELEGT     '                                                   
         DC    C'RECHNUNGER '                                                   
         DC    C'SUMME      '                                                   
         DC    C'REKL       '      75                                           
         DC    C'STATUS     '                                                   
         DC    C'KUNDE      '                                                   
         DC    C'PRODUKT    '                                                   
         DC    C'KAMPAGNE   '                                                   
         DC    C'MOTIV      '      80                                           
         DC    C'FILM       '                                                   
         DC    C'MOTIV      '                                                   
         DC    C'COUNT      '                                                   
         DC    C'ITEM       '                                                   
         DC    C'REF        '      85                                           
         DC    C'WERBETR    '                                                   
         DC    C'KREDITOR   '                                                   
         DC    C'KREDITOR   '                                                   
         DC    C'1          '                                                   
         DC    C'2          '      90                                           
         DC    C'3          '                                                   
         DC    C'BUDGET     '                                                   
         DC    C'DATUM      '                                                   
         DC    C'DATUM      '                                                   
         DC    C'TAGE       '      95                                           
         DC    C'VORAUS  VB '                                                   
         DC    C'GESAMT     '                                                   
         DC    C'ZUSAMMEN   '                                                   
         DC    C'ANFDATUM   '                                                   
         DC    C'ENDDATUM   '      100                                          
         DC    C'WIEDER     '                                                   
         DC    C'           '      SPARE FOR PATCH                              
         DC    C'           '      END OF TABLE ENTRY                           
*                                                                               
* $BUY OPTION VALUE KEYWORDS - SEE MEBUY02 OPTTAB                               
*                                                                               
DD0003   DC    AL1(1,01),C'#',AL1(4,1)       VARIOUS                            
         DC    C'JA  PNEINN',X'00'                                              
*                                                                               
DD0004   DC    AL1(1,01),C'#',AL1(8,1)       ENQUIRE CURRENCY                   
         DC    C'BUDGET  2LANDESW LDMK     ',X'00',X'00'                        
*                                                                               
DD0005   DC    AL1(1,01),C'#',AL1(11,1)      ENQUIRE AGYCOMM                    
         DC    C'BUYING     BCREATIV    CGESAMT     T'                          
         DC    C'BERECHENBAR',X'00',X'00'                                       
*                                                                               
DD0006   DC    AL1(1,01),C'#',AL1(8,1)       EC COST                            
         DC    C'BRUTTO  GKND-NETTONAGT-NETTOB',X'00'                           
*                                                                               
DD0007   DC    AL1(1,01),C'#',AL1(8,1)       COST                               
         DC    C'BRUTTO  GKND-NETTON',X'00'                                     
*                                                                               
DD0008   DC    AL1(1,01),C'#',AL1(11,1)      ENQUIRE INCENTIVE                  
         DC    C'YES        YNO         NPROVISIONALP',X'00'                    
*                                                                               
DD0009   DC    AL1(1,01),C'#',AL1(4,1)       VARIOUS                            
         DC    C'ALLEA',X'00'                                                   
*                                                                               
DD0010   DC    AL1(1,01),C'#',AL1(2,1)       VARIOUS                            
         DC    C'JAY',X'00'                                                     
*                                                                               
DD0011   DC    AL1(1,01),C'#',AL1(4,1)       VARIOUS                            
         DC    C'JA  YNEINNALLEA',X'00'                                         
*                                                                               
DD0012   DC    AL1(1,01),C'#',AL1(4,1)       VARIOUS                            
         DC    C'NEINN',X'00'                                                   
*                                                                               
DD0013   DC    AL1(1,01),C'#',AL1(7,1)       SELECT ACTION                      
         DC    C'ADD    ACHANGE CDISPLAYDREMOVE R',X'00'                        
*                                                                               
DD0014   DC    AL1(1,01),C'#',AL1(7,1)       WISEBUY SHOW                       
         DC    C'FILM   FSERIAL SVOLDISCV',X'00'                                
*                                                                               
DD0015   DC    AL1(1,01),C'#',AL1(7,1)       VOUCHLIST STATUS                   
         DC    C'ALLE   AGEPR!FTCNICHTS TREKL   QZUR!CK R'                      
         DC    C'BELEGT V',X'00'                                                
*                                                                               
DD0016   DC    AL1(1,01),C'#',AL1(8,1)       MULTIAMEND COMMENT                 
         DC    C'ANHNGENAERSETZENR',X'00'                                      
*                                                                               
DD0017   DC    AL1(1,01),C'#',AL1(3,1)       WISEBUY CHANGE                     
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
DD0020   DC    AL1(1,01),C'#',AL1(8,2)       UK PRINT                           
         DC    C'ADVERT  AD'                                                    
         DC    C'CARDRATECRCATEGORYCACOLOUR  CODATE    DTDAY     DY'            
         DC    C'MEDISC  MEPOSITIONPORATE    RAREMARKS RESPACE   SP'            
         DC    C'VATIN   VIVATOUT  VO',X'00'                                    
*                                                                               
DD0021   DC    AL1(1,01),C'#',AL1(8,2)       UK INTERNATIONAL BROAD             
         DC    C'CARDRATECRCATEGORYCADATE    DTDAY     DYMEDISC  ME'            
         DC    C'RATE    RAREMARKS RESECONDS SETIME    TI',X'00'                
*                                                                               
DD0022   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK TV                          
         DC    C'BLOCK   BLBRUTTO  BPDATUM   DTMOTIV   ADMWST    VO'            
         DC    C'RAB1    R1RAB2    R2RABND  RSEKUNDENSESENDER  ST'            
         DC    C'SPOTS   NSTARIF   RCZEIT    TI',X'00'                          
         DC    X'00'                                                            
*                                                                               
DD0023   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK NEWSPAPERS                  
         DC    C'ABSCHL  CNBRUTTO  BPDATUM   DTEINZPR  UPFARBE   CO'            
         DC    C'GR\SSE  SPMOTIV   ADMWST    VORAB1    R1'                      
         DC    C'RAB2    R2RAB3    R3RABND  RTARIF   RC',X'00'                
*                                                                               
DD0024   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK OTHER PRINT                 
         DC    C'ABSCHL  CNAN      ANBD      BDBRUTTO  BPDATUM   DT'            
         DC    C'EINZPR  UPFARBE   COGR\SSE  SPMOTIV   ADMWST    VO'            
         DC    C'NUMMER  ISRAB1    R1RAB2    R2RAB3    R3RABND  R'            
         DC    C'TARIF   RC',X'00'                                              
*                                                                               
DD0025   DC    AL1(1,01),C'#',AL1(4,1)       WISEBUY POOL                       
         DC    C'BILLSPAY MALL A',X'00'                                         
*                                                                               
DD0026   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK RADIO                       
         DC    C'BRUTTO  BPDATUM   DTEINZPR  UPMOTIV   ADMWST    VO'            
         DC    C'RAB1    R1RAB2    R2RABND  RSEKUNDENSESENDER  ST'            
         DC    C'TARIF   RCZEIT    TI',X'00'                                    
*                                                                               
DD0027   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK CINEMA                      
         DC    C'BRUTTO  BPDATUM   DTMETER   MTMOTIV   ADMWST    VO'            
         DC    C'RAB1    R1RABND  RWOCHE   WK',X'00'                          
*                                                                               
DD0028   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK POSTERS                     
         DC    C'BLOCK   BKBRUTTO  BPDEKADE  DEGR\SSE  SPMOTIV   AD'            
         DC    C'MWST    VORAB1    R1RAB2    R2RABND  RSTELLEN SI'            
         DC    C'TAGESPR DP',X'00'                                              
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
         DC    C'ZAWEKURSZWK '     01                                           
         DC    C'AGTPROV AGTP'                                                  
         DC    C'ASBOF       '                                                  
         DC    C'AUD     AU  '                                                  
         DC    C'BUWEKURSBWK '     05                                           
         DC    C'SKONTOKDSKKD'                                                  
         DC    C'SKONTOWTSKTO'                                                  
         DC    C'KONTAKT     '                                                  
         DC    C'DAYPART DPT '                                                  
         DC    C'INCENT  INC '     10                                           
         DC    C'FORMEL      '                                                  
         DC    C'GUAR        '                                                  
         DC    C'LEVY        '                                                  
         DC    C'ORTSPR  OE  '                                                  
         DC    C'MITTLVG MVG '     15                                           
         DC    C'GRUNDPR GP  '                                                  
         DC    C'ANMERK  ANMK'                                                  
         DC    C'NUMMER  NR  '                                                  
         DC    C'AUFTRAG AUFT'                                                  
         DC    C'FILAUF  FIL '     20                                           
         DC    C'PACKAGE PACK'                                                  
         DC    C'PARITY  PAR '                                                  
         DC    C'SKTOBEZ SKBZ'                                                  
         DC    C'ROTATIONROT '                                                  
         DC    C'SENDER  SEN '     25                                           
         DC    C'SUP         '                                                  
         DC    C'VOL         '                                                  
         DC    C'VORST   VST '                                                  
         DC    C'MWST        '                                                  
         DC    C'RCD         '     30                                 <         
         DC    C'RABND      '                                                  
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
**PAN#1  DC    CL21'001GEDDTGER  01/17/91'                                      
         END                                                                    
