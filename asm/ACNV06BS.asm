*          DATA SET ACNV06BS   AT LEVEL 013 AS OF 09/19/00                      
*PHASE ACNV6BSA ACNV06BS                                                        
         TITLE 'BACKER -  CONVERSION TABLES'                                    
*                                                                               
ACNV06   CSECT                                                                  
         DC    A(OFFT),AL4((OFFEND-OFFT)/OFFLNQ)                                
         DC    A(ACCT),AL4((ACCEND-ACCT)/ACCLNQ)                                
         DC    A(CLOT),AL4((CLOEND-CLOT)/CLOLNQ)                                
         DC    A(WRKT),AL4((WRKEND-WRKT)/WRKLNQ)                                
         DC    A(MEDT),AL4((MEDEND-MEDT)/MEDLNQ)                                
         DC    A(CLIT),AL4((CLIEND-CLIT)/CLILNQ)                                
         DC    A(BLST),AL4((BLSEND-BLST)/BLSLNQ)                                
         EJECT                                                                  
***********************************************************************         
* OFFICE TABLE                                                        *         
***********************************************************************         
                                                                                
OFFT     DS    0H                                                               
*        DC    X'00',C'00',PL5'0'                                               
*        DC    C' ',C'40',PL5'0'                                                
*        DC    C'1',C'11',PL5'0'                                                
OFFEND   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ACCOUNT TABLE                                                       *         
***********************************************************************         
                                                                                
ACCT     DS    0H                                                               
         DC    CL14'1C15FDWUPWNE  ',CL14'1C15FDWNEWNE  ',CL36' ',PL5'0'         
         DC    CL14'2915FDWUPWNE  ',CL14'2915FDWNEWNE  ',CL36' ',PL5'0'         
         DC    CL14'1C15FDWSWDA   ',CL14'1C15FDWWSWDA  ',CL36' ',PL5'0'         
         DC    CL14'2915FDWSWDA   ',CL14'2915FDWWSWDA  ',CL36' ',PL5'0'         
*&&DO                                                                           
         DC    CL14'SR122WUPWND   ',CL14'SR115WUPWND   ',CL36' ',PL5'0'         
         DC    CL14'SR122OH WEF   ',CL14'SR115OH WEF   ',CL36' ',PL5'0'         
         DC    CL14'SR122WEFWSP   ',CL14'SR115WEFWSP   ',CL36' ',PL5'0'         
         DC    CL14'SR122WUPTAH   ',CL14'SR115WUPTAH   ',CL36' ',PL5'0'         
         DC    CL14'SR122WUPKNG   ',CL14'SR115WUPKNG   ',CL36' ',PL5'0'         
         DC    CL14'SR122WUPTKN   ',CL14'SR115WUPTKN   ',CL36' ',PL5'0'         
         DC    CL14'SR122WUPCHI   ',CL14'SR115WUPCHI   ',CL36' ',PL5'0'         
         DC    CL14'SR122WMDLAF   ',CL14'SR115WMDLAF   ',CL36' ',PL5'0'         
         DC    CL14'SR122WUPBEN   ',CL14'SR115WUPBEN   ',CL36' ',PL5'0'         
         DC    CL14'SR122WMDWDM   ',CL14'SR115WMDWDM   ',CL36' ',PL5'0'         
         DC    CL14'SR122WMDWEL   ',CL14'SR115WMDWEL   ',CL36' ',PL5'0'         
         DC    CL14'SR122WNEWFS   ',CL14'SR115WNEWFS   ',CL36' ',PL5'0'         
         DC    CL14'SR122WUPWAY   ',CL14'SR115WUPWAY   ',CL36' ',PL5'0'         
         DC    CL14'SR122OH WII   ',CL14'SR115OH WII   ',CL36' ',PL5'0'         
         DC    CL14'SR122WMDWIC   ',CL14'SR115WMDWIC   ',CL36' ',PL5'0'         
         DC    CL14'SR122WMDWMD   ',CL14'SR115WMDWMD   ',CL36' ',PL5'0'         
         DC    CL14'SR122WMDLVL   ',CL14'SR115WMDLVL   ',CL36' ',PL5'0'         
         DC    CL14'SR122WUPMIN   ',CL14'SR115WUPMIN   ',CL36' ',PL5'0'         
         DC    CL14'SR122WUPWNB   ',CL14'SR115WUPWNB   ',CL36' ',PL5'0'         
         DC    CL14'SR122WNEWNE   ',CL14'SR115WNEWNE   ',CL36' ',PL5'0'         
         DC    CL14'SR122WUPWSD   ',CL14'SR115WUPWSD   ',CL36' ',PL5'0'         
         DC    CL14'SR122WWSWSW   ',CL14'SR115WWSWSW   ',CL36' ',PL5'0'         
         DC    CL14'SR122WUPWTN   ',CL14'SR115WUPWTN   ',CL36' ',PL5'0'         
         DC    CL14'SR111WEN0     ',CL14'SR115WEN0     ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPKNG  ',CL14'1C15FDWUPKNG  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPTKN  ',CL14'1C15FDWUPTKN  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPWNB  ',CL14'1C15FDWUPWNB  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWMDLAF  ',CL14'1C15FDWMDLAF  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPBEN  ',CL14'1C15FDWUPBEN  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWMDWSC  ',CL14'1C15FDWMDWSC  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWMDWEL  ',CL14'1C15FDWMDWEL  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWNEWFS  ',CL14'1C15FDWNEWFS  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPWAY  ',CL14'1C15FDWUPWAY  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPCHI  ',CL14'1C15FDWUPCHI  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWMDWIC  ',CL14'1C15FDWMDWIC  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDOH WII  ',CL14'1C15FDOH WII  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWMDWMD  ',CL14'1C15FDWMDWMD  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWMDLVL  ',CL14'1C15FDWMDLVL  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPMIN  ',CL14'1C15FDWUPMIN  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWNEWNE  ',CL14'1C15FDWUPWNE  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPWSD  ',CL14'1C15FDWUPWSD  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDOH WSP  ',CL14'1C15FDOH WSP  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWSW  ',CL14'1C15FDWWSWSW  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPWTN  ',CL14'1C15FDWUPWTN  ',CL36' ',PL5'0'         
         DC    CL14'1C11FDWENNGA  ',CL14'1C15FDWENNGA  ',CL36' ',PL5'0'         
         DC    CL14'1C11FDWENNTN  ',CL14'1C15FDWENNTN  ',CL36' ',PL5'0'         
         DC    CL14'1C11FDWENNWE  ',CL14'1C15FDWENNWE  ',CL36' ',PL5'0'         
         DC    CL14'1C11FDWENNZZ  ',CL14'1C15FDWENNZZ  ',CL36' ',PL5'0'         
         DC    CL14'1C11FDWENWEC  ',CL14'1C15FDWENWEC  ',CL36' ',PL5'0'         
         DC    CL14'1C11FDWENWGC  ',CL14'1C15FDWENWGC  ',CL36' ',PL5'0'         
         DC    CL14'1C11FDWENNCO  ',CL14'1C15FDWENNCO  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWMDCIN  ',CL14'1C15FDWMDCIN  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWMDDAY  ',CL14'1C15FDWMDDAY  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWMDTHA  ',CL14'1C15FDWMDTHA  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPDUL  ',CL14'1C15FDWUPDUL  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPQUI  ',CL14'1C15FDWUPQUI  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPROC  ',CL14'1C15FDWUPROC  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPSPR  ',CL14'1C15FDWUPSPR  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWUPCHL  ',CL14'1C15FDWUPCHL  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWAU  ',CL14'1C15FDWWSWAU  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWBE  ',CL14'1C15FDWWSWBE  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWDA  ',CL14'1C15FDWSWDA   ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWFB  ',CL14'1C15FDWWSWFB  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWHU  ',CL14'1C15FDWWSWHU  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWHV  ',CL14'1C15FDWWSWHV  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWPO  ',CL14'1C15FDWWSWPO  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWSE  ',CL14'1C15FDWWSWSE  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWEU  ',CL14'1C15FDWWSWEU  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWPX  ',CL14'1C15FDWWSWPX  ',CL36' ',PL5'0'         
         DC    CL14'1C22FDWWSWTR  ',CL14'1C15FDWWSWTR  ',CL36' ',PL5'0'         
*                                                                               
         DC    CL14'2922FDWUPKNG  ',CL14'2915FDWUPKNG  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPTKN  ',CL14'2915FDWUPTKN  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPWNB  ',CL14'2915FDWUPWNB  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPCHI  ',CL14'2915FDWUPCHI  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWMDLAF  ',CL14'2915FDWMDLAF  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPBEN  ',CL14'2915FDWUPBEN  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWMDWSC  ',CL14'2915FDWMDWSC  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWMDWEL  ',CL14'2915FDWMDWEL  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWNEWFS  ',CL14'2915FDWNEWFS  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPWAY  ',CL14'2915FDWUPWAY  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWMDWIC  ',CL14'2915FDWMDWIC  ',CL36' ',PL5'0'         
         DC    CL14'2922FDOH WII  ',CL14'2915FDOH WII  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWMDWMD  ',CL14'2915FDWMDWMD  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWMDLVL  ',CL14'2915FDWMDLVL  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPMIN  ',CL14'2915FDWUPMIN  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWNEWNE  ',CL14'2915FDWUPWNE  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPWSD  ',CL14'2915FDWUPWSD  ',CL36' ',PL5'0'         
         DC    CL14'2922FDOH WSP  ',CL14'2915FDOH WSP  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWSW  ',CL14'2915FDWWSWSW  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPWTN  ',CL14'2915FDWUPWTN  ',CL36' ',PL5'0'         
         DC    CL14'2911FDWENNGA  ',CL14'2915FDWENNGA  ',CL36' ',PL5'0'         
         DC    CL14'2911FDWENWGC  ',CL14'2915FDWENWGC  ',CL36' ',PL5'0'         
         DC    CL14'2911FDWENNCO  ',CL14'2915FDWENNCO  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWMDCIN  ',CL14'2915FDWMDCIN  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWMDDAY  ',CL14'2915FDWMDDAY  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWMDTHA  ',CL14'2915FDWMDTHA  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPDUL  ',CL14'2915FDWUPDUL  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPQUI  ',CL14'2915FDWUPQUI  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPROC  ',CL14'2915FDWUPROC  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPSPR  ',CL14'2915FDWUPSPR  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWUPCHL  ',CL14'2915FDWUPCHL  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWAU  ',CL14'2915FDWWSWAU  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWBE  ',CL14'2915FDWWSWBE  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWDA  ',CL14'2915FDWSWDA   ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWFB  ',CL14'2915FDWWSWFB  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWHU  ',CL14'2915FDWWSWHU  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWHV  ',CL14'2915FDWWSWHV  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWPO  ',CL14'2915FDWWSWPO  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWSE  ',CL14'2915FDWWSWSE  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWEU  ',CL14'2915FDWWSWEU  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWPX  ',CL14'2915FDWWSWPX  ',CL36' ',PL5'0'         
         DC    CL14'2922FDWWSWTR  ',CL14'2915FDWWSWTR  ',CL36' ',PL5'0'         
*&&                                                                             
*                                                                               
ACCEND   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CLIENT/OFFICE TABLE                                                 *         
***********************************************************************         
                                                                                
CLOT     DS    0H                                                               
*&&DO                                                                           
         DC    C'KNG',C'15',PL5'0'                                              
         DC    C'KN6',C'15',PL5'0'                                              
         DC    C'TKN',C'15',PL5'0'                                              
         DC    C'TK6',C'15',PL5'0'                                              
         DC    C'WB6',C'15',PL5'0'                                              
         DC    C'WCA',C'15',PL5'0'                                              
         DC    C'WCL',C'15',PL5'0'                                              
         DC    C'WC6',C'15',PL5'0'                                              
         DC    C'WD6',C'15',PL5'0'                                              
         DC    C'WEF',C'15',PL5'0'                                              
         DC    C'WEL',C'15',PL5'0'                                              
         DC    C'WE6',C'15',PL5'0'                                              
         DC    C'WFS',C'15',PL5'0'                                              
         DC    C'WFW',C'15',PL5'0'                                              
         DC    C'WF6',C'15',PL5'0'                                              
         DC    C'WH6',C'15',PL5'0'                                              
         DC    C'WH7',C'15',PL5'0'                                              
         DC    C'WH8',C'15',PL5'0'                                              
         DC    C'WIC',C'15',PL5'0'                                              
         DC    C'WII',C'15',PL5'0'                                              
         DC    C'WI6',C'15',PL5'0'                                              
         DC    C'WJ7',C'15',PL5'0'                                              
         DC    C'WLM',C'15',PL5'0'                                              
         DC    C'WL6',C'15',PL5'0'                                              
         DC    C'WMS',C'15',PL5'0'                                              
         DC    C'WM6',C'15',PL5'0'                                              
         DC    C'WNB',C'15',PL5'0'                                              
         DC    C'WNE',C'15',PL5'0'                                              
         DC    C'WNY',C'15',PL5'0'                                              
         DC    C'WN6',C'15',PL5'0'                                              
         DC    C'WN7',C'15',PL5'0'                                              
         DC    C'WN8',C'15',PL5'0'                                              
         DC    C'WSC',C'15',PL5'0'                                              
         DC    C'WSD',C'15',PL5'0'                                              
         DC    C'WSP',C'15',PL5'0'                                              
         DC    C'WSW',C'15',PL5'0'                                              
         DC    C'WS6',C'15',PL5'0'                                              
         DC    C'WTN',C'15',PL5'0'                                              
         DC    C'WTX',C'15',PL5'0'                                              
         DC    C'WT6',C'15',PL5'0'                                              
         DC    C'WV6',C'15',PL5'0'                                              
         DC    C'WW6',C'15',PL5'0'                                              
         DC    C'WW7',C'15',PL5'0'                                              
         DC    C'WW8',C'15',PL5'0'                                              
         DC    C'WEN',C'15',PL5'0'                                              
         DC    C'WEC',C'15',PL5'0'                                              
         DC    C'WJ8',C'15',PL5'0'                                              
         DC    C'WDM',C'15',PL5'0'                                              
*&&                                                                             
CLOEND   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* WORKCODE TABLE                                                      *         
***********************************************************************         
                                                                                
WRKT     DS    0H                                                               
*        DC    C'AA',C'A1',PL5'0'                                               
*        DC    C'AD',C'A4',PL5'0'                                               
*        DC    C'DR',C'4R',PL5'0'                                               
WRKEND   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* MEDIA TABLE                                                         *         
***********************************************************************         
                                                                                
MEDT     DS    0H                                                               
*        DC    C'L',C'A',PL5'0'                                                 
*        DC    C'N',C'B',PL5'0'                                                 
MEDEND   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* CLIENT CODE TABLE                                                   *         
***********************************************************************         
                                                                                
CLIT     DS    0H                                                               
*        DC    C'AA ',C'AA1',PL5'0'                                             
*        DC    C'AAA',C'AA1',PL5'0'                                             
*        DC    C'AAC',C'AA1',PL5'0'                                             
*        DC    C'AAX',C'AA1',PL5'0'                                             
*        DC    C'GGG',C'GG3',PL5'0'                                             
CLIEND   DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* BILLING SOURCE TABLE                                                *         
***********************************************************************         
                                                                                
BLST     DS    0H                                                               
*        DC    CL12'SPOT TV     ',CL12'SPOT TV     ',PL5'0'                     
BLSEND   DS    0H                                                               
*                                                                               
         EJECT                                                                  
ACNVD    DSECT                                                                  
       ++INCLUDE ACNVWORK                                                       
         EJECT                                                                  
       ++INCLUDE ACNVDSECT                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACNV06BS  09/19/00'                                      
         END                                                                    
