*          DATA SET RELDJWTS   AT LEVEL 034 AS OF 05/01/02                      
*PHASE RELDJWT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'RELDJWT - CONVERT J. WALTER TO MINDSHARE'                       
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
*******************************************************************             
*                                                                 *             
*******************************************************************             
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*******************************************************************             
* CONTROL FLOW LOGIC                                              *             
*******************************************************************             
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         SPACE 1                                                                
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         SPACE 1                                                                
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         SPACE 1                                                                
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         AP    PURGE,=P'1'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* INITIALISE LOGIC                                                *             
*******************************************************************             
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         CLI   0(R5),X'0C'         CONTRACTS                                    
         BE    DMXR05                                                           
         CLI   0(R5),X'11'         MAKEGOODS                                    
         BE    DMXR100                                                          
         CLC   =C'4301',0(R5)      PROPOSALS                                    
         BE    DMXR200                                                          
         CLI   0(R5),X'41'         DARE                                         
         BE    DMXR300                                                          
         CLI   0(R5),X'51'         CONFIRMED DARE                               
         BE    DMXR300                                                          
         B     DMXKEEP                                                          
*                                                                               
* PROCESS CONTRACT RECORDS                                                      
*                                                                               
DMXR05   DS    0H                                                               
         USING RCONREC,R5                                                       
         LA    R3,REPTABLE                                                      
DMXR10   CLC   RCONKREP,0(R3)                                                   
         BNE   DMXR20                                                           
         CLC   RCONKCON,10(R3)                                                  
         BNE   DMXR20                                                           
         CLC   =C'JW',RCONKAGY                                                  
         BNE   DMXR20                                                           
*                                                                               
         AP    CONCHG,=P'1'                                                     
         MVC   RCONKAGY,2(R3)                                                   
*                                                                               
         MVC   P(9),=C'CONTRACT:'                                               
         MVC   P+12(27),0(R5)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
DMXR20   DS    0H                                                               
         AHI   R3,14                                                            
         CLI   0(R3),X'FF'                                                      
         BNE   DMXR10                                                           
         B     DMXKEEP             DONE                                         
         DROP  R5                                                               
*                                                                               
* MAKEGOOD                                                                      
*                                                                               
DMXR100  DS    0H                                                               
         USING RMKGREC,R5                                                       
         LA    R3,REPTABLE                                                      
DMXR110  CLC   RMKGKREP,0(R3)                                                   
         BNE   DMXR120                                                          
*                                                                               
         PACK  MYWORK(1),RMKGKCON+3(1) REVERSE THE COMPLIMENT                   
         PACK  MYWORK+1(1),RMKGKCON+2(1)                                        
         PACK  MYWORK+2(1),RMKGKCON+1(1)                                        
         PACK  MYWORK+3(1),RMKGKCON(1)                                          
*                                                                               
         ZAP   MYWORK+20(5),=P'0'                                               
         MVO   MYWORK+20(5),MYWORK(4)                                           
         ZAP   MYWORK+10(5),=P'99999999'                                        
         SP    MYWORK+10(5),MYWORK+20(5)                                        
         MVO   MYWORK(5),MYWORK+10(5)                                           
*                                                                               
         CLC   MYWORK(4),10(R3)                                                 
         BNE   DMXR120                                                          
*                                                                               
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXR120                                                          
         USING RMKGXEL,R5                                                       
         CLC   =C'JW',RMKGXAGY                                                  
         BNE   DMXR120                                                          
         AP    MKGCHG,=P'1'                                                     
         MVC   RMKGXAGY,2(R3)                                                   
*                                                                               
         MVC   P(9),=C'MAKEGOOD:'                                               
         L     R5,AREC             POINT TO RECORD                              
         MVC   P+12(27),0(R5)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
DMXR120  DS    0H                                                               
         AHI   R3,14                                                            
         CLI   0(R3),X'FF'                                                      
         BNE   DMXR110                                                          
         B     DMXKEEP             DONE                                         
         DROP  R5                                                               
*                                                                               
* PROPOSAL                                                                      
*                                                                               
DMXR200  DS    0H                                                               
         USING RPROHDRD,R5                                                      
         LA    R3,REPTABLE                                                      
DMXR210  CLC   RPROKRCD,0(R3)                                                   
         BNE   DMXR220                                                          
*                                                                               
         ZAP   MYWORK+20(5),=P'0'                                               
         MVO   MYWORK+20(5),RPROKCON(4)                                         
         ZAP   MYWORK+10(5),=P'99999999'                                        
         SP    MYWORK+10(5),MYWORK+20(5)                                        
         MVO   MYWORK(5),MYWORK+10(5)                                           
*                                                                               
         CLC   MYWORK(4),10(R3)                                                 
         BNE   DMXR220                                                          
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXR220                                                          
         USING RPRSWELD,R5                                                      
         CLC   =C'JW',RPRSWAGY                                                  
         BNE   DMXR220                                                          
         AP    PROCHG,=P'1'                                                     
         MVC   RPRSWAGY,2(R3)                                                   
*                                                                               
         MVC   P(9),=C'PROPOSAL:'                                               
         L     R5,AREC             POINT TO RECORD                              
         MVC   P+12(27),0(R5)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
DMXR220  DS    0H                                                               
         AHI   R3,14                                                            
         CLI   0(R3),X'FF'                                                      
         BNE   DMXR210                                                          
         B     DMXKEEP             DONE                                         
         DROP  R5                                                               
*                                                                               
* DARE                                                                          
*                                                                               
DMXR300  DS    0H                                                               
         USING RDARREC,R5                                                       
         LA    R3,REPTABLE                                                      
DMXR310  CLC   RDARKREP,0(R3)                                                   
         BNE   DMXR320                                                          
         CLC   RDARKORD,6(R3)                                                   
         BNE   DMXR320                                                          
         CLC   =C'JW ',RDARKAGY                                                 
         BNE   DMXR320                                                          
*                                                                               
         AP    DARECHG,=P'1'                                                    
         MVC   RDARKAGY,=C'H7 '                                                 
*                                                                               
         MVC   P(9),=C'DARE:    '                                               
         MVC   P+12(27),0(R5)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         CLI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   =C'JW',RDARSNDR                                                  
         BE    DMXR313                                                          
         MVC   P(35),=C'*** ROUTING CODE MISMATCH ERROR ***'                    
         GOTO1 VPRINTER                                                         
*                                                                               
DMXR313  DS    0H                                                               
         MVC   RDARSNDR(2),=C'H7'                                               
*                                                                               
         CLC   =C'JWB1',RDARRTS+4                                               
         BE    DMXR315                                                          
         MVC   P(17),=C'*** RTS ERROR ***'                                      
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXR315  DS    0H                                                               
         AP    DARECHGO,=P'1'                                                   
         MVC   RDARRTS+4(4),=C'H731'                                            
         MVC   P(9),=C'ROUTING: '                                               
         MVC   P+12(100),34(R5)                                                 
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
DMXR320  DS    0H                                                               
         AHI   R3,14                                                            
         CLI   0(R3),X'FF'                                                      
         BNE   DMXR310                                                          
         B     DMXKEEP             DONE                                         
         DROP  R5                                                               
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
DMXEOF   DS    0H                                                               
         BAS   RE,DMCNT                                                         
         B     DMXIT               OUTPUT COUNTS                                
         EJECT                                                                  
*******************************************************************             
*              END OF FILE                                        *             
*******************************************************************             
         SPACE 1                                                                
         USING RECD,R3                                                          
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CHANGED'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'CONTRACT'                                              
         EDIT  (P5,CONCHG),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'MAKEGOOD'                                              
         EDIT  (P5,MKGCHG),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'PROPOSAL'                                              
         EDIT  (P5,PROCHG),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'DARE ORD'                                              
         EDIT  (P5,DARECHGO),(7,P+12)                                           
         GOTO1 VPRINTER                                                         
         MVC   P+3(4),=C'DARE ALL'                                              
         EDIT  (P5,DARECHG),(7,P+12)                                            
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
CONCHG   DC    PL5'0'                                                           
MKGCHG   DC    PL5'0'                                                           
PROCHG   DC    PL5'0'                                                           
DARECHGO DC    PL5'0'                                                           
DARECHG  DC    PL5'0'                                                           
SAVEKEY  DC    CL27' '                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
REPTABLE DS    0CL14                                                            
         DC    C'41',C'MIND',X'01530096',X'00000519'                            
         DC    C'AM',C'MIND',X'01510277',X'06870446'                            
         DC    C'AM',C'MIND',X'01510210',X'06871058'                            
         DC    C'AM',C'MIND',X'01520231',X'06872464'                            
         DC    C'AM',C'MIND',X'01610229',X'06873279'                            
         DC    C'AM',C'MIND',X'01430197',X'06867160'                            
         DC    C'AM',C'MIND',X'01510362',X'06870350'                            
         DC    C'AM',C'MIND',X'01540108',X'06871793'                            
         DC    C'AM',C'MIND',X'01590006',X'06872394'                            
         DC    C'BL',C'MSU ',X'01520229',X'05012300'                            
         DC    C'BL',C'MSU ',X'01450362',X'05009745'                            
         DC    C'BL',C'MSU ',X'01530473',X'05011570'                            
         DC    C'BL',C'MSU ',X'01530475',X'05011687'                            
         DC    C'BL',C'MSU ',X'01580095',X'05012940'                            
         DC    C'BL',C'MSU ',X'01610230',X'05015045'                            
         DC    C'BL',C'MSU ',X'01430367',X'05007707'                            
         DC    C'BL',C'MSU ',X'01430443',X'05007594'                            
         DC    C'BL',C'MSU ',X'01520104',X'05010886'                            
         DC    C'BL',C'MSU ',X'01580002',X'05012990'                            
         DC    C'BL',C'MSU ',X'01010092',X'04988194'                            
         DC    C'BL',C'MSU ',X'01010095',X'04988199'                            
         DC    C'BL',C'MSU ',X'01010104',X'04988197'                            
         DC    C'BL',C'MSU ',X'01010107',X'04988203'                            
         DC    C'BL',C'MSU ',X'01040021',X'04988262'                            
         DC    C'BL',C'MSU ',X'01040025',X'04988263'                            
         DC    C'BL',C'MSU ',X'01040035',X'04988254'                            
         DC    C'BL',C'MSU ',X'01040038',X'04988255'                            
         DC    C'BL',C'MSU ',X'01080033',X'04989709'                            
         DC    C'BL',C'MSU ',X'01080034',X'04989728'                            
         DC    C'BL',C'MSU ',X'01080045',X'04989720'                            
         DC    C'BL',C'MSU ',X'01080046',X'04989740'                            
         DC    C'CQ',C'MIND',X'01590233',X'06844652'                            
         DC    C'CQ',C'MIND',X'01440247',X'06840873'                            
         DC    C'CQ',C'MIND',X'01510478',X'06841665'                            
         DC    C'CQ',C'MIND',X'01530472',X'06842961'                            
         DC    C'CQ',C'MIND',X'01530476',X'06845481'                            
         DC    C'CQ',C'MIND',X'01580003',X'06842687'                            
         DC    C'CQ',C'MIND',X'00830043',X'06769983'                            
         DC    C'CQ',C'MIND',X'00830046',X'06770009'                            
         DC    C'CQ',C'MIND',X'00830055',X'06807544'                            
         DC    C'CQ',C'MIND',X'00830058',X'06807546'                            
         DC    C'CQ',C'MIND',X'01010020',X'06814149'                            
         DC    C'CQ',C'MIND',X'01010024',X'06814163'                            
         DC    C'CQ',C'MIND',X'01010031',X'06772859'                            
         DC    C'CQ',C'MIND',X'01010034',X'06772853'                            
         DC    C'CQ',C'MIND',X'01010093',X'06814379'                            
         DC    C'CQ',C'MIND',X'01010096',X'06814384'                            
         DC    C'CQ',C'MIND',X'01010105',X'06814393'                            
         DC    C'CQ',C'MIND',X'01010108',X'06814396'                            
         DC    C'CQ',C'MIND',X'01040019',X'06769517'                            
         DC    C'CQ',C'MIND',X'01040023',X'06809578'                            
         DC    C'CQ',C'MIND',X'01040033',X'06815565'                            
         DC    C'CQ',C'MIND',X'01080035',X'06792392'                            
         DC    C'CQ',C'MIND',X'01080036',X'06792374'                            
         DC    C'CQ',C'MIND',X'01080047',X'06792444'                            
         DC    C'CQ',C'MIND',X'01080048',X'06792443'                            
         DC    C'FN',C'MSU ',X'01520230',X'02756893'                            
         DC    C'FN',C'MSU ',X'01610232',X'02756931'                            
         DC    C'FN',C'MSU ',X'01430198',X'02754753'                            
         DC    C'FN',C'MSU ',X'01320008',X'02752775'                            
         DC    C'NB',C'MIND',X'01320004',X'00063549'                            
         DC    C'PV',C'MSU ',X'01510211',X'02997079'                            
         DC    C'PV',C'MSU ',X'01590231',X'03000447'                            
         DC    C'PV',C'MSU ',X'01440246',X'02995103'                            
         DC    C'PV',C'MSU ',X'01450387',X'02996172'                            
         DC    C'PV',C'MSU ',X'01510477',X'02999290'                            
         DC    C'PV',C'MSU ',X'01530477',X'02999288'                            
         DC    C'PV',C'MSU ',X'01430199',X'02997031'                            
         DC    C'PV',C'MSU ',X'01430426',X'02996769'                            
         DC    C'PV',C'MSU ',X'01520107',X'02998061'                            
         DC    C'PV',C'MSU ',X'01580000',X'02999194'                            
         DC    C'PV',C'MSU ',X'00830045',X'02979474'                            
         DC    C'PV',C'MSU ',X'00830047',X'02980991'                            
         DC    C'PV',C'MSU ',X'00830057',X'02980979'                            
         DC    C'PV',C'MSU ',X'00830060',X'02980973'                            
         DC    C'PV',C'MSU ',X'01010019',X'02981032'                            
         DC    C'PV',C'MSU ',X'01010023',X'02981029'                            
         DC    C'PV',C'MSU ',X'01010030',X'02981022'                            
         DC    C'PV',C'MSU ',X'01010033',X'02981020'                            
         DC    C'PV',C'MSU ',X'01040020',X'02982384'                            
         DC    C'PV',C'MSU ',X'01040024',X'02982390'                            
         DC    C'PV',C'MSU ',X'01040034',X'02982388'                            
         DC    C'PV',C'MSU ',X'01040037',X'02982394'                            
         DC    C'SZ',C'MIND',X'01450412',X'06831012'                            
         DC    C'SZ',C'MIND',X'01510279',X'06832575'                            
         DC    C'SZ',C'MIND',X'01580094',X'06834555'                            
         DC    C'SZ',C'MIND',X'01430427',X'06829327'                            
         DC    C'SZ',C'MIND',X'01580001',X'06834488'                            
         DC    C'SZ',C'MIND',X'00830044',X'06805558'                            
         DC    C'SZ',C'MIND',X'00830048',X'06805555'                            
         DC    C'SZ',C'MIND',X'00830056',X'06805527'                            
         DC    C'SZ',C'MIND',X'00830059',X'06805513'                            
         DC    C'SZ',C'MIND',X'01010021',X'06812062'                            
         DC    C'SZ',C'MIND',X'01010022',X'06812088'                            
         DC    C'SZ',C'MIND',X'01010032',X'06773454'                            
         DC    C'SZ',C'MIND',X'01010035',X'06812085'                            
         DC    C'SZ',C'MIND',X'01010091',X'06812346'                            
         DC    C'SZ',C'MIND',X'01010094',X'06812356'                            
         DC    C'SZ',C'MIND',X'01010103',X'06812348'                            
         DC    C'SZ',C'MIND',X'01010106',X'06812381'                            
         DC    C'SZ',C'MIND',X'01080041',X'06815452'                            
         DC    C'SZ',C'MIND',X'01080042',X'06815460'                            
         DC    C'SZ',C'MIND',X'01080053',X'06815446'                            
         DC    C'SZ',C'MIND',X'01080054',X'06815450'                            
         DC    C'SZ',C'MIND',X'01320006',X'06822949'                            
         DC    C'SZ',C'MIND',X'01540106',X'06836195'                            
         DC    C'SZ',C'MIND',X'01590007',X'06836048'                            
         DC    C'SZ',C'MIND',X'01590008',X'06837324'                            
         DC    C'UT',C'MIND',X'01450334',X'00124363'                            
         DC    X'FF'                                                            
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
RECUP    DS    V                                                                
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
ELEMENT  DS    XL256                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENMKG                                                       
       ++INCLUDE REGENPRO                                                       
       ++INCLUDE REGENDAR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034RELDJWTS  05/01/02'                                      
         END                                                                    
