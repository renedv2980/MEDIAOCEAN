*          DATA SET GEDDTDUT   AT LEVEL 001 AS OF 01/17/91                      
*PHASE T00D07                                                                   
         TITLE 'GENERAL TABLE DATA DICTIONARY - DUTCH'                          
GEDDTENG CSECT                                                                  
BASE     DS    0C                                                               
         SPACE 2                                                                
         DC    CL8'GEDDTDUT'                                                    
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
         DC    C'WZWIJZIGEN  ' 01                                               
         DC    C'GKGOEDKEUREN'                                                  
         DC    C'GKGOEDKEUREN'                                                  
         DC    C'B BOEKING   '                                                  
         DC    C'BDBUDGET    ' 05  ** UK ONLY **                                
         DC    C'ANANNULEREN '                                                  
         DC    C'SASERIEANNUL'     ** UK ONLY **                                
         DC    C'CTCANCEL TRN'                                                  
         DC    C'KOKOSTEN    '                                                  
         DC    C'D BATCH BUY ' 10  ** UK ONLY **                                
         DC    C'DABATCH AMND'     ** UK ONLY **                                
         DC    C'DBBATCH LIVE'     ** UK ONLY **                                
         DC    C'DDBATCH DEL '     ** UK ONLY **                                
         DC    C'DEBATCH ENQ '     ** UK ONLY **                                
         DC    C'DHBATCH HOLD' 15  ** UK ONLY **                                
         DC    C'DNBATCH END '     ** UK ONLY **                                
         DC    C'DLDEAL      '     ** UK ONLY **                                
         DC    C'TBTOONBOEKNG'                                                  
         DC    C'TBTOONBOEKNG'                                                  
         DC    C'STSAMENSTTON' 20  ** UK ONLY **                                
         DC    C'G GHI       '     ** UK ONLY **                                
         DC    C'H HOLD      '                                                  
         DC    C'E ECHT      '                                                  
         DC    C'E ECHT      '                                                  
         DC    C'S SLUITEN   ' 25  ** UK ONLY **                                
         DC    C'MWMEERDWIJZ '                                                  
         DC    C'MWMEERDWIJZ '                                                  
         DC    C'MBMEERDBOEK '     ** UK ONLY **                                
         DC    C'MAMEERDANNL '                                                  
         DC    C'MAMEERDANNL ' 30                                               
         DC    C'MEMEERDECHT '                                                  
         DC    C'MEMEERDECHT '                                                  
         DC    C'MBMEERDBET  '                                                  
         DC    C'MBMEERDBET  '                                                  
         DC    C'ONOVERNEMEN ' 35  ** UK ONLY **                                
         DC    C'VGVRIJGEVEN '                                                  
         DC    C'VGVRIJGEVEN '                                                  
         DC    C'WBWIJZBETAL '                                                  
         DC    C'WBWIJZBETAL '                                                  
         DC    C'AFAFDRUKKEN ' 40                                               
         DC    C'AFAFDRUKKEN '                                                  
         DC    C'AFAFDRUKKEN '                                                  
         DC    C'AFAFDRUKKEN '                                                  
         DC    C'AFAFDRUKKEN '                                                  
         DC    C'KBKANTTBETAL' 45                                               
         DC    C'KBKANTTBETAL'                                                  
         DC    C'R REEKS     '     ** UK ONLY **                                
         DC    C'RAREEKSANNUL'     ** UK ONLY **                                
         DC    C'RLREEKSLIVE '     ** UK ONLY **                                
         DC    C'RERESPONSE  ' 50  ** UK ONLY **                                
         DC    C'RDRESPDETAIL'     ** UK ONLY **                                
         DC    C'RRRESPRATES '     ** UK ONLY **                                
         DC    C'SPSPECIAAL  '                                                  
         DC    C'SESELECTEREN'     ** UK ONLY **                                
         DC    C'SUSUMMARY   ' 55  ** UK ONLY **                                
         DC    C'T TRNS UPDTE'     ** UK ONLY **                                
         DC    C'TDTRNS DISP '     ** UK ONLY **                                
         DC    C'TETRNS END  '     ** UK ONLY **                                
         DC    C'TMTRNS MATCH'     ** UK ONLY **                                
         DC    C'TUTRNS UNEND' 60  ** UK ONLY **                                
         DC    C'TVTRN VERIFY'     ** UK ONLY **                                
         DC    C'OVOPHEFVRIJG'                                                  
         DC    C'OVOPHEFVRIJG'                                                  
         DC    C'OWOPHEFWIJZ '                                                  
         DC    C'OWOPHEFWIJZ ' 65                                               
         DC    C'UGUNGHI     '     ** UK ONLY **                                
         DC    C'OHOPHEFHOLD '                                                  
         DC    C'OSOPHEFSLUI '     ** UK ONLY **                                
         DC    C'OOOPHEFOVERN'     ** UK ONLY **                                
         DC    C'OGOPHEFGOEDK' 70                                               
         DC    C'OKOPHEFKANTT'                                                  
         DC    C'OKOPHEFKANTT'                                                  
         DC    C'UTUNCNCL TRN'     ** UK ONLY **                                
         DC    C'BEBEWIJZEN  '                                                  
         DC    C'BEBEWIJZEN  ' 75                                               
         DC    C'BEBEWIJZEN  '                                                  
         DC    C'BEBEWIJZEN  '                                                  
         DC    C'BLBEW.LIJST '                                                  
         DC    C'VMVERFY MTCH'     ** UK ONLY **                                
         DC    C'VPVERFY PRNT' 80  ** UK ONLY **                                
         DC    C'SBSERNRBEWZN'                                                  
         DC    C'W WISEBUY   '     ** UK ONLY **                                
         DC    C'WEWEEK      '     ** UK ONLY **                                
         DC    C'ESEXTRASPEC '                                                  
         DC    C'ECEXTRACOMM ' 85                                               
         DC    C'CBCOMBIBOEK '                                                  
         DC    C'VHVRIJGHOLD '                                                  
         DC    C'SWSPECWIJZ  '                                                  
         DC    C'SMSPECMEWIJZ'                                                  
         DC    C'SMSPECMEWIJZ' 90                                               
         DC    C'LBLISTBUY   '     ** GER ONLY **                               
         DC    C'            '     SPARE FOR PATCH                              
         DC    C'            '     END OF TABLE ENTRY                           
*                                                                               
* $BUY OPTION KEYWORDS - MAPPED 1-TO-1 ONTO MEBUY02 OPTTAB                      
*                                                                               
DD0002   DC    AL1(1,11),CL11'ME##BOPT'                                         
         DC    C'AFDRUKK.   '      01                                           
         DC    C'AFDRUKK.   '                                                   
         DC    C'AFDRUKK.   '                                                   
         DC    C'AFDRUKK.   '                                                   
         DC    C'AFDRUKK.   '      05                                           
         DC    C'VALUTA     '                                                   
         DC    C'PROVBUR    '                                                   
         DC    C'TOESLAG    '                                                   
         DC    C'WISKOERS   '                                                   
         DC    C'SAMWERK    '      10                                           
         DC    C'VERAND  VER'                                                   
         DC    C'COSTEN     '                                                   
         DC    C'COSTEN     '                                                   
         DC    C'PAYSER     '                                                   
         DC    C'REKENING   '      15                                           
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
         DC    C'OUD        '                                                   
         DC    C'NIEUW      '                                                   
         DC    C'OPMERK     '                                                   
         DC    C'GEWIST     '                                                   
         DC    C'BESTEMM BES'      50                                           
         DC    C'BESTEMM BES'                                                   
         DC    C'ACTIE      '                                                   
         DC    C'PROFORMA   '                                                   
         DC    C'PROFORMA   '                                                   
         DC    C'DATUM      '      55                                           
         DC    C'VRIJGEG    '                                                   
         DC    C'VRIJGEG    '                                                   
         DC    C'GESLOTEN   '                                                   
         DC    C'FILM       '                                                   
         DC    C'GHI        '      60                                           
         DC    C'GEMTCHD    '                                                   
         DC    C'NETWORK    '                                                   
         DC    C'POOL       '                                                   
         DC    C'SECONDEN   '                                                   
         DC    C'TOONGEG    '      65                                           
         DC    C'SPECIAL    '                                                   
         DC    C'TRANSMIS   '                                                   
         DC    C'UNIVERSMUNV'                                                   
         DC    C'VERIFIER   '                                                   
         DC    C'GEMTCHD    '      70                                           
         DC    C'GEVERIF    '                                                   
         DC    C'GECONTRBEW '                                                   
         DC    C'FAKTUUR    '                                                   
         DC    C'TOTBEDR    '                                                   
         DC    C'KANTTBET   '      75                                           
         DC    C'STATUS     '                                                   
         DC    C'KLANT      '                                                   
         DC    C'PRODUKT    '                                                   
         DC    C'CAMPAGNE   '                                                   
         DC    C'ADVERT     '      80                                           
         DC    C'FILM       '                                                   
         DC    C'ADVERT     '                                                   
         DC    C'TELLING    '                                                   
         DC    C'ITEM       '                                                   
         DC    C'REFER      '      85                                           
         DC    C'RECLMED RME'                                                   
         DC    C'FOLIO      '                                                   
         DC    C'FOLIO      '                                                   
         DC    C'1          '                                                   
         DC    C'2          '      90                                           
         DC    C'3          '                                                   
         DC    C'BUDGET     '                                                   
         DC    C'DATUM      '                                                   
         DC    C'DATUM      '                                                   
         DC    C'DAGEN      '      95                                           
         DC    C'BETALADVBAD'                                                   
         DC    C'SAMEN      '                                                   
         DC    C'SAMEN      '                                                   
         DC    C'BEGINDAT   '                                                   
         DC    C'EINDDAT    '      100                                          
         DC    C'REUSE      '                                                   
         DC    C'           '      SPARE FOR PATCH                              
         DC    C'           '      END OF TABLE ENTRY                           
*                                                                               
* $BUY OPTION VALUE KEYWORDS - SEE MEBUY02 OPTTAB                               
*                                                                               
DD0003   DC    AL1(1,01),C'#',AL1(3,1)       VARIOUS                            
         DC    C'JA PNEEN',X'00'                                                
*                                                                               
DD0004   DC    AL1(1,01),C'#',AL1(8,1)       ENQUIRE CURRENCY                   
         DC    C'BUDGET  2LOKAAL  LSECONDE 2BRPOND  ',X'00',X'00'               
*                                                                               
DD0005   DC    AL1(1,01),C'#',AL1(8,1)       ENQUIRE AGYCOMM                    
         DC    C'BOEKEN  BCREATIEFCTOTAAL  TTEBOEKNN',X'00',X'00'               
*                                                                               
DD0006   DC    AL1(1,01),C'#',AL1(7,1)       EC COST                            
         DC    C'BRUTO  GNETTO  NCLIENT CREKEN  B',X'00'                        
*                                                                               
DD0007   DC    AL1(1,01),C'#',AL1(5,1)       COST                               
         DC    C'BRUTOGNETTON',X'00'                                            
*                                                                               
DD0008   DC    AL1(1,01),C'#',AL1(11,1)      ENQUIRE INCENTIVE                  
         DC    C'JA         YNEE        NVOORLOPIG  P',X'00'                    
*                                                                               
DD0009   DC    AL1(1,01),C'#',AL1(4,1)       VARIOUS                            
         DC    C'ALLEA',X'00'                                                   
*                                                                               
DD0010   DC    AL1(1,01),C'#',AL1(3,1)       VARIOUS                            
         DC    C'JA Y',X'00'                                                    
*                                                                               
DD0011   DC    AL1(1,01),C'#',AL1(4,1)       VARIOUS                            
         DC    C'JA  YNEE NALLEA',X'00'                                         
*                                                                               
DD0012   DC    AL1(1,01),C'#',AL1(3,1)       VARIOUS                            
         DC    C'NEEN',X'00'                                                    
*                                                                               
DD0013   DC    AL1(1,01),C'#',AL1(7,1)       SELECT ACTION                      
         DC    C'TOEVOEGAVERAND CLATZIENDVERWIJDR',X'00'                        
*                                                                               
DD0014   DC    AL1(1,01),C'#',AL1(7,1)       WISEBUY SHOW                       
         DC    C'FILM   FSERIENRSKWAKORTV',X'00'                                
*                                                                               
DD0015   DC    AL1(1,01),C'#',AL1(9,1)       VOUCHLIST STATUS                   
         DC    C'ELKE     AGECONTROLCNIETGEV  TKANTTBET Q'                      
         DC    C'AFGEWEZENRBEWEZEN  V',X'00'                                    
*                                                                               
DD0016   DC    AL1(1,01),C'#',AL1(7,1)       MULTIAMEND COMMENT                 
         DC    C'UITBRE AVERVANGR',X'00'                                        
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
         DC    C'ADVERT  ADBLOK    BLBRUTO   BPDATUM   DTKORTNG1 R1'            
         DC    C'KORTNG2 R2RECSPOTSNSSECONDENSESTATION STTIJD    TI'            
         DC    X'00'                                                            
*                                                                               
DD0023   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK NEWSPAPERS                  
         DC    C'ADVERT  ADBRUTO   BPCONTRACTCNDATUM   DTEENHPR  UP'            
         DC    C'KLEUR   COKORTNG1 R1KORTNG2 R2KORTNG3 R3RUIMTE  SP'            
         DC    X'00'                                                            
*                                                                               
DD0024   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK OTHER PRINT                 
         DC    C'ADVERT  ADBLEED   ANBRUTO   BPCONTRACTCNDATUM   DT'            
         DC    C'DPS     BDEENHPR  UPKLEUR   CONRTIJDSSISKORTNG1 R1'            
         DC    C'KORTNG2 R2KORTNG3 R3RUIMTE  SP',X'00'                          
*                                                                               
DD0025   DC    AL1(1,01),C'#',AL1(4,1)                                          
         DC    C'REK SBET MALLEA',X'00'                                         
*                                                                               
DD0026   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK RADIO                       
         DC    C'ADVERT  ADBRUTO   BPDATUM   DTEENHPR  UPKORTNG1 R1'            
         DC    C'KORTNG2 R2SECONDENSESTATION STTIJD    TI',X'00'                
*                                                                               
DD0027   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK CINEMA                      
         DC    C'ADVERT  ADBRUTO   BPDATUM   DTMETERS  MTKORTNG1 R1'            
         DC    C'WEKEN   WK',X'00'                                              
*                                                                               
DD0028   DC    AL1(1,01),C'#',AL1(8,2)       NON-UK POSTERS                     
         DC    C'ADVERT  ADBLOK    BKBRUTO   BPDGBLPERSDPDECENN  DE'            
         DC    C'KORTNG1 R1KORTNG2 R2LOKATIESSIRUIMTE  SP',X'00'                
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
         DC    C'REKENINGREK '     01                                           
         DC    C'BUREAU      '                                                  
         DC    C'ASBOF       '                                                  
         DC    C'PUBLIEK PUBL'                                                  
         DC    C'INSCHR  INSC'     05                                           
         DC    C'KTDIRBETKDB '                                                  
         DC    C'CDPAYST CDPS'                                                  
         DC    C'CONTPERS    '                                                  
         DC    C'DAGDEEL DAGD'                                                  
         DC    C'TOESLAG TOES'     10                                           
         DC    C'FORMULE     '                                                  
         DC    C'GARAND      '                                                  
         DC    C'HEFFING     '                                                  
         DC    C'LOKAAL  LOK '                                                  
         DC    C'MEDIA       '     15                                           
         DC    C'NATION  NATI'                                                  
         DC    C'OPM.        '                                                  
         DC    C'NUMMER  NMR '                                                  
         DC    C'ORDER       '                                                  
         DC    C'AFZET   AFZT'     20                                           
         DC    C'PAKKET  PAKK'                                                  
         DC    C'WISKOERSWIKO'                                                  
         DC    C'CDPAYDD CDPD'                                                  
         DC    C'ROTATIE ROTA'                                                  
         DC    C'STATION STAT'     25                                           
         DC    C'LEV.        '                                                  
         DC    C'RUIMTE      '                                                  
         DC    C'INKBTW  IBTW'                                                  
         DC    C'UITBTW  UBTW'                                                  
         DC    C'HERPL.      '     30                                           
         DC    C'REBADJ      '                                        >         
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
**PAN#1  DC    CL21'001GEDDTDUT  01/17/91'                                      
         END                                                                    
