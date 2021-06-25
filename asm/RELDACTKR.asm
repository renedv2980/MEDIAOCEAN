*          DATA SET RELDACTKR  AT LEVEL 047 AS OF 05/01/02                      
*PHASE RELDACT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
         TITLE 'RELDXYH - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
*                                                                               
* LEV  42 SEP04/96 BG SET UP FOR JAN THRU MAY, NOT APRIL                        
* LEV  44 DEC02/96 BG SET UP FOR V4 ONLY, JAN95-JUN96                           
* LEV  45 MAR27/97 BG SET UP FOR KRG RADIO JAN96-DEC96                          
*                                                                               
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
*  KRG ACTUALS                                                    *             
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
         AP    10(5,R5),=P'1'                                                   
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
         CLI   0(R5),X'02'         STATION RECORD                               
         BE    DMXR10                                                           
         CLI   0(R5),X'0C'         CONTRACT RECORD                              
         BE    DMXR100                                                          
         B     DMXKEEP                                                          
         EJECT                                                                  
*******************************************************************             
* PROCESS FOR CONTRACT RECORDS                                                  
*******************************************************************             
DMXR10   DS    0H                  KRG SUBREPS                                  
         USING RSTAREC,R5                                                       
         CLC   =C'BF',RSTAKREP                                                  
         BE    DMXR20                                                           
         CLC   =C'CR',RSTAKREP                                                  
         BE    DMXR20                                                           
         CLC   =C'EA',RSTAKREP                                                  
         BE    DMXR20                                                           
         CLC   =C'KF',RSTAKREP                                                  
         BE    DMXR20                                                           
         CLC   =C'KU',RSTAKREP                                                  
         BE    DMXR20                                                           
         CLC   =C'K4',RSTAKREP                                                  
         BE    DMXR20                                                           
         CLC   =C'S3',RSTAKREP                                                  
         BNE   DMXKEEP                                                          
*                                                                               
DMXR20   DS    0H                  MARK ALL STATION CLOSED THRU DEC/95          
         LA    R4,COUNTS                                                        
DMXR25   CLC   RSTAKREP,0(R4)                                                   
         BE    DMXR30                                                           
         CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R4,L'COUNTS(R4)                                                  
         B     DMXR25                                                           
*                                                                               
DMXR30   DS    0H                                                               
         AP    2(5,R4),=P'1'                                                    
*                                                                               
         CLC   RSTACLDT,ECLSDTE    ONLY IF CLOSED DATE IS EARLIER THAN          
         BNL   DMXKEEP                                                          
         MVC   RSTACLDT,ECLSDTE                                                 
         AP    7(5,R4),=P'1'                                                    
*                                                                               
         CLC   7(5,R4),=PL5'10'                                                 
         BH    DMXR40                                                           
         MVC   P+3(2),RSTAKREP                                                  
         MVC   P+7(5),RSTAKSTA                                                  
         EDIT  (1,RSTACLDT),(2,P+13)                                            
         EDIT  (1,RSTACLDT+1),(2,P+16)                                          
         GOTO1 VPRINTER                                                         
DMXR40   DS    0H                                                               
*                                                                               
         B     DMXKEEP                                                          
         DROP  R5                                                               
         EJECT                                                                  
*******************************************************************             
* PROCESS FOR CONTRACT RECORDS                                                  
*******************************************************************             
DMXR100  DS    0H                  KRG SUBREPS                                  
         USING RCONREC,R5                                                       
         CLC   =C'BF',RCONKREP                                                  
         BE    DMXR110                                                          
         CLC   =C'CR',RCONKREP                                                  
         BE    DMXR110                                                          
         CLC   =C'EA',RCONKREP                                                  
         BE    DMXR110                                                          
         CLC   =C'KF',RCONKREP                                                  
         BE    DMXR110                                                          
         CLC   =C'KU',RCONKREP                                                  
         BE    DMXR110                                                          
         CLC   =C'K4',RCONKREP                                                  
         BE    DMXR110                                                          
         CLC   =C'S3',RCONKREP                                                  
         BNE   DMXKEEP                                                          
*                                                                               
DMXR110  DS    0H                                                               
         LA    R4,COUNTS                                                        
DMXR112  CLC   RCONKREP,0(R4)                                                   
         BE    DMXR113                                                          
         CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R4,L'COUNTS(R4)                                                  
         B     DMXR112                                                          
*                                                                               
DMXR113  DS    0H                                                               
         AP    12(5,R4),=P'1'                                                   
*                                                                               
         CLC   RCONDATE(2),ECLSDTE     SKIP IF START IS AFTER CLOSE             
         BH    DMXKEEP                                                          
         CLC   RCONDATE+3(2),SCLSDTE   SKIP IF BEFORE START DATE                
         BL    DMXKEEP                                                          
*                                                                               
         CLC   17(5,R4),=PL5'10'                                                
         BH    DMXR114                                                          
         MVC   P+3(2),RCONKREP                                                  
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P+8,4,=C'TOG'                           
         GOTO1 =V(DATCON),DMCB,(3,RCONDATE),(6,P+18)                            
         GOTO1 =V(DATCON),DMCB,(3,RCONDATE+3),(6,P+25)                          
         GOTO1 VPRINTER                                                         
DMXR114  DS    0H                                                               
*                                                                               
         AP    17(5,R4),=P'1'                                                   
         MVC   STARTYM,RCONDATE                                                 
         MVC   ENDYM,RCONDATE+3                                                 
         DROP  R5                                                               
*                                                                               
         CLC   STARTYM,SCLSDTE     START CLOSE DATE                             
         BNL   DMXR115                                                          
         MVC   STARTYM,SCLSDTE                                                  
*                                                                               
DMXR115  DS    0H                                                               
         CLC   ENDYM,ECLSDTE       END CLOSE DATE                               
         BNH   DMXR118                                                          
         MVC   ENDYM,ECLSDTE                                                    
*                                                                               
DMXR118  DS    0H                                                               
         MVC   THISYM,STARTYM                                                   
*                                                                               
         L     R5,AREC             POINT TO RECORD                              
         LA    R5,34(R5)           POINT TO START OF FIRST ELEMENT              
*                                                                               
DMXR120  DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    DMXR160                                                          
         CLI   0(R5),4                                                          
         BE    DMXR140                                                          
         BH    DMXR160                                                          
*                                                                               
DMXR130  DS    0H                                                               
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DMXR120                                                          
*                                                                               
DMXR140  DS    0H                                                               
         USING RCONSTEL,R5                                                      
****                                                                            
*                                                                               
         CLC   17(5,R4),=PL5'10'                                                
         BH    DMXR145                                                          
         MVC   P+12(7),=C'FOUND :'                                              
         EDIT  (1,RCONSTYR),(2,P+20)                                            
         EDIT  (1,RCONSTYR+1),(2,P+23)                                          
         GOTO1 VPRINTER                                                         
DMXR145  DS    0H                                                               
*                                                                               
****                                                                            
         CLC   THISYM,RCONSTYR                                                  
         BH    DMXR130                                                          
         CLC   THISYM,RCONSTYR                                                  
         BL    DMXR160                                                          
         DROP  R5                                                               
*                                                                               
         CLC   THISYM,ENDYM        ALL DONE, KEEP RECORD EXIT                   
         BE    DMXKEEP                                                          
*                                                                               
* BUMP TO NEXT MONTH. IF DECEMBER, BUMP YEAR TO NEXT                            
* RESET MONTH TO JANUARY                                                        
*                                                                               
         CLI   THISYM+1,12         DECEMBER?                                    
         BNL   DMXR150                                                          
         ZIC   R1,THISYM+1          NO, BUMP TO NEXT MONTH                      
         LA    R1,1(R1)                                                         
         STC   R1,THISYM+1                                                      
         B     DMXR130                                                          
*                                                                               
DMXR150  DS    0H                   YES, BUMP TO NEXT YEAR AND RESET            
         ZIC   R1,THISYM            MONTH TO JANUARY                            
         LA    R1,1(R1)                                                         
         STC   R1,THISYM                                                        
         MVI   THISYM+1,1                                                       
         B     DMXR130                                                          
*                                                                               
DMXR160  DS    0H                  INVOICE NOT FOUND, ADD IT                    
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RCONSTEL,R6                                                      
*                                                                               
         MVI   RCONSTCO,X'04'                                                   
         MVI   RCONSTLN,10                                                      
         MVC   RCONSTYR(2),THISYM                                               
*                                                                               
* GET 1ST MONDAY OF THE BROADCAST MONTH 2 MONTHS LATER                          
*                                                                               
         XC    WORK,WORK                                                        
         MVC   YMD,THISYM                                                       
         MVI   YMD+2,15                                                         
         GOTO1 =V(DATCON),DMCB,(3,YMD),(0,WORK)                                 
         GOTO1 =V(GETBROAD),DMCB,(0,WORK),WORK+6                                
         GOTO1 =V(DATCON),DMCB,(0,WORK+12),(5,WORK+18)                          
         MVC   WORK+21(2),=C'15'                                                
         MVC   WORK+26(6),=C'(+2M)'                                             
*                                                                               
         GOTO1 =V(PERVAL),DMCB,(14,WORK+18),(0,WORK2)                           
         CLI   DMCB+4,1                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
PDATED   USING PERVALD,WORK2                                                    
         GOTO1 =V(GETBROAD),DMCB,(0,PDATED.PVALESTA),WORK                       
         DROP  PDATED              MOVE COMPRESSED DATE                         
*                                                                               
         GOTO1 =V(DATCON),DMCB,(0,WORK),(2,RCONSTWK)                            
         AP    22(5,R4),=P'1'                                                   
****                                                                            
*                                                                               
         CLC   17(5,R4),=PL5'10'                                                
         BH    DMXR170                                                          
         MVC   P+12(7),=C'ADDING:'                                              
         EDIT  (1,RCONSTYR),(2,P+20)                                            
         EDIT  (1,RCONSTYR+1),(2,P+23)                                          
         GOTO1 =V(DATCON),DMCB,(2,RCONSTWK),(5,P+26)                            
         GOTO1 VPRINTER                                                         
DMXR170  DS    0H                                                               
*                                                                               
****                                                                            
         DROP  R6                                                               
*                                                                               
         GOTO1 =V(RECUP),DMCB,(2,AREC),ELEM,(R5)                                
         B     DMXR140                                                          
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
*        MVC   P+3(7),=C'UTS:   '                                               
*        EDIT  (P5,UTSCTR),(7,P+12)                                             
*        GOTO1 VPRINTER                                                         
*        MVC   P+3(7),=C'SEL:   '                                               
*        EDIT  (P5,SELCTR),(7,P+12)                                             
*        GOTO1 VPRINTER                                                         
         MVC   P+3(33),=C'DETAILS OF UPDATED RECORDS FOLLOWS'                   
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R5,COUNTS                                                        
DC10     MVC   P+3(2),0(R5)                                                     
         EDIT  (P5,2(R5)),(7,P+6)                                               
         EDIT  (P5,7(R5)),(7,P+15)                                              
         EDIT  (P5,12(R5)),(7,P+24)                                             
         EDIT  (P5,17(R5)),(7,P+33)                                             
         EDIT  (P5,22(R5)),(7,P+42)                                             
         GOTO1 VPRINTER                                                         
         LA    R5,L'COUNTS(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DC10                                                             
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
SCLSDTE  DC    X'6001'             START OF CLOSE PERIOD                        
ECLSDTE  DC    X'600C'             END OF CLOSE PERIOD                          
PURGE    DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
UTSCTR   DC    PL5'0'                                                           
SELCTR   DC    PL5'0'                                                           
*                                                                               
COUNTS   DS    0CL27                                                            
*                      STA    STA    CON    CON    ELEMENTS                     
*                      READ   CHGED  READ   CHGED  ADDED                        
         DC    CL2'BF',PL5'0',PL5'0',PL5'0',PL5'0',PL5'0'                       
         DC    CL2'CR',PL5'0',PL5'0',PL5'0',PL5'0',PL5'0'                       
         DC    CL2'EA',PL5'0',PL5'0',PL5'0',PL5'0',PL5'0'                       
         DC    CL2'KU',PL5'0',PL5'0',PL5'0',PL5'0',PL5'0'                       
         DC    CL2'K4',PL5'0',PL5'0',PL5'0',PL5'0',PL5'0'                       
         DC    CL2'KF',PL5'0',PL5'0',PL5'0',PL5'0',PL5'0'                       
         DC    CL2'S3',PL5'0',PL5'0',PL5'0',PL5'0',PL5'0'                       
         DC    X'FF'                                                            
*        DC    CL2'K6',PL5'0',PL5'0',PL5'0',PL5'0',PL5'0'                       
*        DC    CL2'V4',PL5'0',PL5'0',PL5'0',PL5'0',PL5'0'                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
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
STARTYM  DS    XL2                                                              
ENDYM    DS    XL2                                                              
THISYM   DS    XL2                                                              
YMD      DS    XL3                                                              
WORK     DS    CL64                                                             
WORK2    DS    CL64                                                             
ELEM     DS    CL256                                                            
ELEM2    DS    CL256                                                            
WORKX    EQU   *                                                                
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENSDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047RELDACTKR 05/01/02'                                      
         END                                                                    
