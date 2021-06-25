*          DATA SET GEDDTFRE   AT LEVEL 001 AS OF 01/17/91                      
*PHASE T00D04                                                                   
         TITLE 'GENERAL TABLE DATA DICTIONARY - FRENCH'                         
GEDDTFRE CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'GEDDTFRE'                                                    
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
         DC    C'M MODIFIER  ' 01                                               
         DC    C'APAPPROUVER '                                                  
         DC    C'APAPPROUVER '                                                  
         DC    C'A ACHETER   '                                                  
         DC    C'BDBUDGET    ' 05                                               
         DC    C'ANANNULER   '                                                  
         DC    C'ASANNULSERIE'                                        <         
         DC    C'ZBCANCEL TRN'                                        <         
         DC    C'COCOUTS     '                                        <         
         DC    C'ZCBATCH BUY ' 10                                     <         
         DC    C'ZDBATCH AMND'                                        <         
         DC    C'ZEBATCH LIVE'                                        <         
         DC    C'ZFBATCH DEL '                                        <         
         DC    C'ZGBATCH ENQ '                                        <         
         DC    C'ZHBATCH HOLD' 15                                     <         
         DC    C'ZIBATCH END '                                        <         
         DC    C'DEDEAL      '                                        <         
         DC    C'V VISUALISER'                                                  
         DC    C'V VISUALISER'                                                  
         DC    C'VCVIS COMP  ' 20                                               
         DC    C'ZJGHI       '                                                  
         DC    C'RTRETENIR   '                                                  
         DC    C'ACACTUALISER'                                                  
         DC    C'LILIST      '                                                  
         DC    C'B BLOQUER   ' 25                                               
         DC    C'MMMULTIMODIF'                                                  
         DC    C'MMMULTIMODIF'                                                  
         DC    C'ZKMULTIBUY  '                                        <         
         DC    C'MAMULTIANNUL'                                                  
         DC    C'MAMULTIANNUL' 30                                               
         DC    C'MTMULTIACTU '                                                  
         DC    C'MTMULTIACTU '                                                  
         DC    C'MVMULTIVALID'                                                  
         DC    C'MVMULTIVALID'                                                  
         DC    C'R REMPLACER ' 35                                               
         DC    C'VAVALIDER   '                                                  
         DC    C'VAVALIDER   '                                                  
         DC    C'PAPAY AMEND '                                                  
         DC    C'PAPAY AMEND '                                                  
         DC    C'I IMPRIMER  ' 40                                               
         DC    C'I IMPRIMER  '                                                  
         DC    C'I IMPRIMER  '                                                  
         DC    C'I IMPRIMER  '                                                  
         DC    C'I IMPRIMER  '                                                  
         DC    C'QUQUESTION  ' 45                                               
         DC    C'QUQUESTION  '                                                  
         DC    C'ZLRANGE     '                                        <         
         DC    C'ZMRANGECANC '                                        <         
         DC    C'ZNRANGELIVE '                                        <         
         DC    C'ZOREPONSE   ' 50                                               
         DC    C'ZPRESPDETAIL'                                        <         
         DC    C'ZQRESPRATES '                                        <         
         DC    C'S SPECIAL   '                                                  
         DC    C'SESELECT    '                                                  
         DC    C'SUSUMMARY   ' 55                                               
         DC    C'ZRTRNS UPDTE'                                        <         
         DC    C'ZSTRNS DISP '                                        <         
         DC    C'ZTTRNS END  '                                        <         
         DC    C'ZUTRNS MATCH'                                                  
         DC    C'ZVTRNS UNEND' 60                                     <         
         DC    C'ZWTRN VERIFY'                                        <         
         DC    C'DRDEREGLER  '                                                  
         DC    C'DRDEREGLER  '                                                  
         DC    C'UAUNCLR AMND'                                        <         
         DC    C'UAUNCLR AMND' 65                                     <         
         DC    C'ZXUNGHI     '                                        <         
         DC    C'DFDEFAIRE   '                                                  
         DC    C'DBDEBLOQUER '                                                  
         DC    C'UOUNOVERRIDE'                                        <         
         DC    C'DADESAPPROUV' 70                                               
         DC    C'UQUNQUERY   '                                        <         
         DC    C'UQUNQUERY   '                                        <         
         DC    C'ZYUNCNCL TRN'                                        <         
         DC    C'J JUSTIFIER '                                                  
         DC    C'J JUSTIFIER ' 75                                               
         DC    C'J JUSTIFIER '                                                  
         DC    C'J JUSTIFIER '                                                  
         DC    C'JLJUSTIFLIST'                                                  
         DC    C'ZZVERFY MTCH'                                        <         
         DC    C'YAVERFY PRNT' 80                                     <         
         DC    C'VSVOUCSERIAL'                                        <         
         DC    C'W WISEBUY   '                                        <         
         DC    C'WEWEEK      '                                        <         
         DC    C'X EXTRASPECL'                                        <         
         DC    C'OAOBSERVADD ' 85                                               
         DC    C'COCOMBI     '                                        <         
         DC    C'ARANNULRET  '                                        <         
         DC    C'SASPEC-MODIF'                                        <         
         DC    C'SMSPEC-MULTI'                                        <         
         DC    C'SMSPEC-MULTI' 90                                     <         
         DC    C'LBLISTBUY   '     ** GER ONLY **                     <         
         DC    C'            '     SPARE FOR PATCH                              
         DC    C'            '     END OF TABLE ENTRY                           
*                                                                               
* $BUY OPTION KEYWORDS - MAPPED 1-TO-1 ONTO MEBUY02 OPTTAB                      
*                                                                               
DD0002   DC    AL1(1,11),CL11'ME##BOPT'                                         
         DC    C'IMPRIMER   '      01                                           
         DC    C'IMPRIMER   '                                                   
         DC    C'IMPRIMER   '                                                   
         DC    C'IMPRIMER   '                                                   
         DC    C'IMPRIMER   '      05                                           
         DC    C'DEVISE     '                                                   
         DC    C'AGCOMM     '                                                   
         DC    C'INCENTIV   '                                         <         
         DC    C'PARITY     '                                         <         
         DC    C'POOL       '      10                                 <         
         DC    C'CHANGE  CHG'                                         <         
         DC    C'COUT       '                                         <         
         DC    C'COUT       '                                                   
         DC    C'PAYSER     '                                         <         
         DC    C'FACT       '      15                                 <         
         DC    C'1          '                                         <         
         DC    C'1          '                                         <         
         DC    C'1          '                                         <         
         DC    C'1          '                                         <         
         DC    C'1          '      20                                 <         
         DC    C'1          '                                                   
         DC    C'1          '                                         <         
         DC    C'1          '                                         <         
         DC    C'1          '                                         <         
         DC    C'1          '      25                                 <         
         DC    C'2          '                                                   
         DC    C'2          '                                         <         
         DC    C'2          '                                         <         
         DC    C'2          '                                         <         
         DC    C'2          '      30                                 <         
         DC    C'2          '                                                   
         DC    C'2          '                                         <         
         DC    C'2          '                                                   
         DC    C'2          '                                         <         
         DC    C'2          '      35                                 <         
         DC    C'3          '                                                   
         DC    C'3          '                                         <         
         DC    C'3          '                                         <         
         DC    C'3          '                                         <         
         DC    C'3          '      40                                 <         
         DC    C'3          '                                                   
         DC    C'3          '                                         <         
         DC    C'3          '                                         <         
         DC    C'3          '                                         <         
         DC    C'3          '      45                                 <         
         DC    C'ANCIEN     '                                                   
         DC    C'NOUVEAU    '                                         <         
         DC    C'COMMENT.COM'                                         <         
         DC    C'ELIMINE    '                                         <         
         DC    C'DEST    DE '      50                                 <         
         DC    C'DEST       '                                                   
         DC    C'ACTION     '                                         <         
         DC    C'PROV       '                                         <         
         DC    C'PROV       '                                         <         
         DC    C'DATE       '      55                                 <         
         DC    C'REGLE      '                                                   
         DC    C'REGLE      '                                                   
         DC    C'BLOQUE     '                                         <         
         DC    C'FILM       '                                         <         
         DC    C'GHI        '      60                                 <         
         DC    C'MATCHED    '                                                   
         DC    C'NETWORK    '                                         <         
         DC    C'POOL       '                                         <         
         DC    C'SECONDS    '                                                   
         DC    C'MONTRE     '      65                                 <         
         DC    C'SPECIAL    '                                                   
         DC    C'TRANS      '                                         <         
         DC    C'UNIVERSEUNV'                                         <         
         DC    C'VERIFY     '                                         <         
         DC    C'MATCHED    '      70                                           
         DC    C'VERIFIED   '                                                   
         DC    C'JUSTIFIE   '                                                   
         DC    C'FACTURE    '                                                   
         DC    C'TOTAUX     '                                         <         
         DC    C'QUESTION   '      75                                           
         DC    C'ETAT       '                                                   
         DC    C'CLIENT     '                                         <         
         DC    C'PRODUIT    '                                                   
         DC    C'ACTION     '                                                   
         DC    C'ADVERT     '      80                                           
         DC    C'FILM       '                                                   
         DC    C'ADVERT     '                                         <         
         DC    C'COUNT      '                                         <         
         DC    C'ITEM       '                                         <         
         DC    C'REF        '      85                                           
         DC    C'SUPPORT    '                                                   
         DC    C'CREANCIER  '                                                   
         DC    C'CREANCIER  '                                                   
         DC    C'1          '                                         <         
         DC    C'2          '      90                                           
         DC    C'3          '                                         <         
         DC    C'BUDGET     '                                         <         
         DC    C'DATE       '                                                   
         DC    C'DATE       '                                         <         
         DC    C'JOURS      '      95                                 <         
         DC    C'ADVPAID AP '                                         <         
         DC    C'ENSEMBLE   '                                         <         
         DC    C'ENSEMBLE   '                                         <         
         DC    C'DEBUT      '                                         <         
         DC    C'FIN        '      100                                <         
         DC    C'ENCORE     '                                         <         
         DC    C'           '      SPARE FOR PATCH                              
         DC    C'           '      END OF TABLE ENTRY                           
*                                                                               
* $BUY OPTION VALUE KEYWORDS - SEE MEBUY02 OPTTAB                               
*                                                                               
DD0003   DC    AL1(1,01),C'#',AL1(3,1)                                <         
         DC    C'OUIPNONN',X'00'                                      <         
*                                                                               
DD0004   DC    AL1(1,01),C'#',AL1(8,1)                                <         
         DC    C'BUDGET  2LOCAL   LSECOND  2STERLING',X'00',X'00'     <         
*                                                                               
DD0005   DC    AL1(1,01),C'#',AL1(8,1)                                <         
         DC    C'BUYING  BCREATIVECTOTAL   TBILLABLE',X'00',X'00'     <         
*                                                                               
DD0006   DC    AL1(1,01),C'#',AL1(7,1)                                <         
         DC    C'GROSS  GNET    NCLIENT CBILL   B',X'00'              <         
*                                                                               
DD0007   DC    AL1(1,01),C'#',AL1(5,1)                                <         
         DC    C'GROSSGNET  N',X'00'                                  <         
*                                                                               
DD0008   DC    AL1(1,01),C'#',AL1(11,1)                               <         
         DC    C'OUI        YNON        NPROVISIONALP',X'00'          <         
*                                                                               
DD0009   DC    AL1(1,01),C'#',AL1(4,1)                                <         
         DC    C'TOUSA',X'00'                                                   
*                                                                               
DD0010   DC    AL1(1,01),C'#',AL1(3,1)                                <         
         DC    C'OUIY',X'00'                                          <         
*                                                                               
DD0011   DC    AL1(1,01),C'#',AL1(4,1)                                <         
         DC    C'OUI YNON NTOUSA',X'00'                                         
*                                                                               
DD0012   DC    AL1(1,01),C'#',AL1(2,1)                                <         
         DC    C'NON',X'00'                                           <         
*                                                                               
DD0013   DC    AL1(1,01),C'#',AL1(7,1)                                <         
         DC    C'ADD    ACHANGE CDISPLAYDREMOVE R',X'00'              <         
*                                                                               
DD0014   DC    AL1(1,01),C'#',AL1(7,1)                                <         
         DC    C'FILM   FSERIAL SVOLDISCV',X'00'                      <         
*                                                                               
DD0015   DC    AL1(1,01),C'#',AL1(8,1)                                <         
         DC    C'ALL     ACHECKED CTNOTRACETNOTRACE TQUERY   Q'       <         
         DC    C'REJECT  RVOUCHED V',X'00'                            <         
*                                                                               
DD0016   DC    AL1(1,01),C'#',AL1(7,1)                                <         
         DC    C'APPEND AREPLACER',X'00'                              <         
*                                                                               
DD0017   DC    AL1(1,01),C'#',AL1(3,1)                                <         
         DC    C'TVRT',X'00'                                          <         
*                                                                               
DD0018   DC    AL1(1,01),C'#',AL1(8,2)       UK TV                    <         
         DC    C'CARDRATECRDATE    DTDAY     DYFILM    FIMEDISC  ME'  <         
         DC    C'RATE    RAREMARKS RESECONDS SETIME    TITA1     T1'  <         
         DC    C'TA2     T2VATIN   VIVATOUT  VO',X'00'                <         
*                                                                               
DD0019   DC    AL1(1,01),C'#',AL1(8,2)       UK RADIO                 <         
         DC    C'CARDRATECRDATE    DTDAY     DYMEDISC  MERATE    RA'  <         
         DC    C'REMARKS RESECONDS SETIME    TITA1     T1TA2     T2'  <         
         DC    C'VATIN   VIVATOUT  VO',X'00'                          <         
*                                                                               
DD0020   DC    AL1(1,01),C'#',AL1(8,2)       UK PRESS                 <         
         DC    C'CARDRATECRCATEGORYCACOLOUR  CODATE    DTDAY     DY'  <         
         DC    C'MEDISC  MEPOSITIONPORATE    RAREMARKS RESPACE   SP'  <         
         DC    C'VATIN   VIVATOUT  VO',X'00',C'DVERT  AD',X'00'       <         
*                                                                               
DD0021   DC    AL1(1,01),C'#',AL1(8,2)       UK INTERNATIONAL BDCAST  <         
         DC    C'CARDRATECRCATEGORYCADATE    DTDAY     DYMEDISC  ME'  <         
         DC    C'RATE    RAREMARKS RESECONDS SETIME    TI',X'00'      <         
*                                                                               
DD0022   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK BROADCAST         <         
         DC    C'ADVERT  ADBLOCK   BLDATE    DTRATE    BPREBATE1 R1'  <         
         DC    C'REBATE2 R2SECONDS SESPOTS   NSSTATION STTIME    TI'  <         
         DC    X'00'                                                  <         
*                                                                               
DD0023   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK NEWSPAPERS                  
         DC    C'CONTRACTCNSPACE   SPRATE    BPDATE    DTUNITPR  UP'  <         
         DC    C'COLOUR  COADVERT  ADREBATE1 R1REBATE2 R2REBATE3 R3'  <         
         DC    X'00'                                                            
*                                                                               
DD0024   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK OTHER PRINT                 
         DC    C'CONTRACTCNBLEED   ANSPACE   SPDPS     BDRATE    BP'  <         
         DC    C'COLOUR  CODATE    DTISSUE   ISUNITPR  UPADVERT  AD'  <         
         DC    C'REBATE1 R1REBATE2 R2REBATE3 R3',X'00'                <         
*                                                                               
DD0025   DC    AL1(1,01),C'#',AL1(4,1)                                <         
         DC    C'BILLSPAY MALL A',X'00'                               <         
*                                                                               
DD0026   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK RADIO             <         
         DC    C'ADVERT  ADDATE    DTRATE    BPREBATE1 R1REBATE2 R2'  <         
         DC    C'SECONDS SESTATION STTIME    TIUNITPR  UP',X'00'      <         
*                                                                               
DD0027   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK CINEMA            <         
         DC    C'ADVERT  ADDATE    DTMETERS  MTRATE    BPRAB1    R1'  <         
         DC    C'WEEKS   WK',X'00'                                    <         
*                                                                               
DD0028   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK POSTERS           <         
         DC    C'ADVERT  ADBLOCK   BKDAILYPR DPDECADE  DERATE    BP'  <         
         DC    C'RAB1    R1RAB2    R2SITES   SISPACE   SP',X'00'      <         
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
         DC    C'ACCOUNT ACC '     01                                 <         
         DC    C'AGY         '                                        <         
         DC    C'ASBOF       '                                        <         
         DC    C'AUD     AU  '                                        <         
         DC    C'BOOKING BOOK'     05                                 <         
         DC    C'CDBILL  CDB '                                        <         
         DC    C'CDPAYST CDPS'                                        <         
         DC    C'CONTACT     '                                        <         
         DC    C'DAYPART DPT '                                        <         
         DC    C'INCENT  INC '     10                                 <         
         DC    C'FORMULA     '                                        <         
         DC    C'GUAR        '                                        <         
         DC    C'LEVY        '                                        <         
         DC    C'LOCAL   LOC '                                        <         
         DC    C'MED         '     15                                 <         
         DC    C'NATIONALNAT '                                        <         
         DC    C'NOTES       '                                        <         
         DC    C'NUMBER  NUM '                                        <         
         DC    C'ORDER       '                                        <         
         DC    C'OUTLET  OUT '     20                                 <         
         DC    C'PACKAGE PACK'                                        <         
         DC    C'PARITY  PAR '                                        <         
         DC    C'CDPAYDD CDPD'                                        <         
         DC    C'ROTATIONROT '                                        <         
         DC    C'STATION STA '     25                                 <         
         DC    C'SUP         '                                        <         
         DC    C'VOL         '                                        <         
         DC    C'VATIN   VATI'                                        <         
         DC    C'VATOUT  VATO'                                        <         
         DC    C'RCD         '     30                                 <         
         DC    C'REBADJ      '                                        <         
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
**PAN#1  DC    CL21'001GEDDTFRE  01/17/91'                                      
         END                                                                    
