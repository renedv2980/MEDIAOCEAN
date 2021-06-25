*          DATA SET TAGENA2    AT LEVEL 012 AS OF 02/13/15                      
*PHASE T702A2A                                                                  
         TITLE 'T702A2 - CANADIAN CONVERSION RATE MAINTENANCE'                  
T702A2   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702A2                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   SYS10                                                            
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'40',0)                                    
         B     XIT                                                              
         SPACE 3                                                                
SYS10    CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    SYS15                                                            
         CLI   MODE,XRECPUT        OR IF RECORD CHANGED                         
         BNE   SYS20                                                            
*                                                                               
SYS15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
SYS20    CLI   MODE,RECPUT         VALIDATE THE RECORD                          
         BNE   XIT                                                              
         CLC   SCRLAST+1(2),=X'0101'  IF SCREEN JUST LOADED                     
         BNE   SYS30                                                            
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)  GET THE RECORD                    
         BAS   RE,DISPLAY                     DISPLAY RECORD FIRST              
         MVC   TWAKEYSV,KEY        NEED BECAUSE DON'T GET VALKEY MODE           
*                                  WHEN DO ACTION CHANGE FROM OTHER REC         
         B     DISPLYD             (NEED THIS WHEN DON'T HAVE KEY FLDS)         
         SPACE 1                                                                
SYS30    BAS   RE,BLDREC                                                        
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SCRCCVTH            CLEAR THE SCREEN                             
         SPACE 1                                                                
         MVI   ELCODE,TASYELQ      GET SYSTEM CONTROL ELEMENT                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TASYD,R4                                                         
         SPACE 1                                                                
         EDIT  TASYCCVT,(6,SCRCCVT),2,ALIGN=LEFT                                
         OI    SCRCCVTH+6,X'80'    CALCULATE INPUT LENGTH                       
         LA    R2,SCRCCVT          (NOT RETURNED IN EDIT)                       
         SR    R1,R1                                                            
         LA    R3,L'SCRCCVT        MAXIMUM INPUT LENGTH                         
         SPACE 1                                                                
DISP10   CLI   0(R2),C' '                                                       
         BNH   DISP20                                                           
         LA    R1,1(R1)            INCREMENT COUNTER                            
         LA    R2,1(R2)            BUMP TO NEXT INPUT                           
         BCT   R3,DISP10                                                        
         SPACE 1                                                                
DISP20   STC   R1,SCRCCVTH+5                                                    
         SPACE 1                                                                
         EDIT  TASYECVT,(6,SCRECVT),2,ALIGN=LEFT                                
         OI    SCRECVTH+6,X'80'    CALCULATE INPUT LENGTH                       
         LA    R2,SCRECVT          (NOT RETURNED IN EDIT)                       
         SR    R1,R1                                                            
         LA    R3,L'SCRECVT        MAXIMUM INPUT LENGTH                         
         SPACE 1                                                                
DISP30   CLI   0(R2),C' '                                                       
         BNH   DISP40                                                           
         LA    R1,1(R1)            INCREMENT COUNTER                            
         LA    R2,1(R2)            BUMP TO NEXT INPUT                           
         BCT   R3,DISP30                                                        
         SPACE 1                                                                
DISP40   STC   R1,SCRECVTH+5                                                    
         GOTO1 ACTVOUT,DMCB,(X'80',SCRLCHGH)  LAST CHANGED                      
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAPCELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP90                                                           
         USING TAPCD,R4                                                         
                                                                                
         EDIT  TAPCGSTR,(5,SCRCGST),2,ZERO=NOBLANK                              
         OI    SCRCGSTH+6,X'80'                                                 
                                                                                
DISP50   ZIC   R3,TAPCNSUB         NUMBER OF SUB-ELEMENTS                       
         LA    R5,SCRNAMEH                                                      
S1       USING SCRNAMEH,R5                                                      
         LA    R6,TAPCPROV                                                      
E1       USING TAPCPROV,R6                                                      
                                                                                
DISP60   MVI   S1.SCRHHST,C'N'                                                  
         TM    E1.TAPCSTAT,TAPCSHST   DOES PROVINCE HAVE HST?                   
         BZ    *+8                                                              
         MVI   S1.SCRHHST,C'Y'                                                  
         OI    S1.SCRHHSTH+6,X'80'                                              
                                                                                
         EDIT  E1.TAPCPSTR,(5,S1.SCRPSTR),2,ZERO=NOBLANK                        
         OI    S1.SCRPSTRH+6,X'80'                                              
                                                                                
DISP70   BAS   RE,BUMPPROV         BUMP PAST PROV LINE                          
         AHI   R6,TAPCSLNQ                                                      
         BCT   R3,DISP60                                                        
                                                                                
         USING TAC1D,R4                                                         
DISP90   DS    0H                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAC1ELQ                                                   
         GOTO1 GETL,DMCB,(1,=C'CN ')                                            
         BNE   DISP92                                                           
         L     R4,TGELEM                                                        
         EDIT  TAC1DCC1,(9,SCRFCC1),2,ZERO=NOBLANK                              
                                                                                
DISP92   LHI   R3,13                                                            
         LA    R5,SCRNAMEH                                                      
         L     R4,AIO                                                           
         MVI   ELCODE,TAC1ELQ                                                   
         BAS   RE,GETEL                                                         
         B     DISP94                                                           
DISP93   BAS   RE,NEXTEL                                                        
DISP94   BNE   DISP99                                                           
DISP95   CLC   TAC1UNIT,=C'CN'                                                  
         BE    DISP93                                                           
         CLC   S1.SCRUNIT,TAC1UNIT                                              
         BNE   DISP97                                                           
         EDIT  TAC1DCC1,(9,S1.SCRPCC1),2,ZERO=NOBLANK                           
         OI    S1.SCRPCC1H+6,X'80'                                              
         BAS   RE,BUMPPROV                                                      
         BCT   R3,DISP93                                                        
         B     DISP99                                                           
                                                                                
DISP97   BAS   RE,BUMPPROV                                                      
         BCT   R3,DISP95                                                        
                                                                                
DISP99   DS    0H                                                               
         B     XIT                                                              
                                                                                
BUMPFLD  DS    0H                                                               
         ZIC   R1,0(R5)                                                         
         AR    R5,R1                                                            
         BR    RE                                                               
*                                  BUMP PAST PROV LINE'S 5 FIELDS               
BUMPPROV NTR1                                                                   
         LHI   RF,5                NAME, HST, PROV, RATE, CC AMT                
BPROV10  BAS   RE,BUMPFLD          BUMP PAST CLAIM CODE VALUE                   
         BCT   RF,BPROV10                                                       
         XIT1  REGS=(R5)                                                        
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
         USING TASYD,R4                                                         
BLDREC   NTR1                                                                   
         MVI   ELCODE,TASYELQ      GET SYSTEM CONTROL ELEMENT                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TASYD,R4                                                         
         SPACE 1                                                                
         LA    R2,SCRCCVTH                                                      
         CLI   5(R2),0             IF THERE ISN'T INPUT                         
         BNE   BLD10                                                            
         TM    SCRSTAT,SCRCHG                                                   
         BO    BLD20                                                            
         ZAP   TASYCCVT,=P'0'      ENSURE IT'S A PACKED FIELD                   
         B     BLD20                                                            
         SPACE 1                                                                
BLD10    ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             VALIDATE INPUT                               
         BNE   FLDINV                                                           
         L     R2,4(R1)                                                         
         CVD   R2,DUB                                                           
         ZAP   TASYCCVT,DUB                                                     
         SPACE 1                                                                
BLD20    LA    R2,SCRECVTH                                                      
         CLI   5(R2),0             IF THERE ISN'T INPUT                         
         BNE   BLD30                                                            
         TM    SCRSTAT,SCRCHG                                                   
         BO    BLD40                                                            
         ZAP   TASYECVT,=P'0'      ENSURE IT'S A PACKED FIELD                   
         B     BLD40                                                            
         SPACE 1                                                                
BLD30    ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),0             VALIDATE INPUT                               
         BNE   FLDINV                                                           
         L     R2,4(R1)                                                         
         CVD   R2,DUB                                                           
         ZAP   TASYECVT,DUB                                                     
                                                                                
BLD40    GOTO1 ACTVIN,DMCB,(X'80',0)  LAST CHANGED                              
                                                                                
BLD50    MVI   ELCODE,TAPCELQ      CANADIAN PROVINCE ELEMENT                    
         GOTO1 REMELEM             DELETE CURRENT                               
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAPCD,R4                                                         
         MVI   TAPCEL,TAPCELQ      CANADIAN PROVINCE ELEMENT                    
         MVI   TAPCLEN,TAPCLNQ                                                  
         MVI   TAPCNSUB,13         NUMBER OF PROVINCES                          
                                                                                
         LA    R2,SCRCGSTH                                                      
         CLI   SCRCGSTH+5,0                                                     
         BE    FLDINV                                                           
         ZIC   RF,SCRCGSTH+5                                                    
         GOTO1 CASHVAL,DMCB,SCRCGST,(RF)                                        
         CLI   0(R1),0             VALIDATE INPUT                               
         BNE   FLDINV                                                           
         TM    4(R1),X'80'         NEGATIVE IS INVALID                          
         BO    FLDINV                                                           
         CLC   4(4,R1),=X'0000270F'  CAN'T BE > 99.99%                          
         BH    FLDINV                                                           
         MVC   TAPCGSTR,4(R1)                                                   
                                                                                
BLD55    ZIC   R3,TAPCNSUB                                                      
         LA    R5,SCRNAMEH                                                      
SC2      USING SCRNAMEH,R5                                                      
         LA    R6,TAPCPROV                                                      
EU2      USING TAPCPROV,R6                                                      
                                                                                
BLD60    MVC   EU2.TAPCPROV(2),SC2.SCRUNIT SAVE PROVINCE CODE                   
         MVI   EU2.TAPCPROV+2,C' '                                              
         LA    R2,SC2.SCRHHSTH                                                  
         CLI   SC2.SCRHHST,C'N'                                                 
         BE    BLD62                                                            
         CLI   SC2.SCRHHST,C'Y'                                                 
         BNE   FLDINV                                                           
         OI    EU2.TAPCSTAT,TAPCSHST    PROVINCE HAS HST                        
                                                                                
BLD62    LA    R2,SC2.SCRPSTRH                                                  
         CLI   SC2.SCRPSTRH+5,0                                                 
         BE    FLDINV                                                           
         ZIC   RF,SC2.SCRPSTRH+5                                                
         GOTO1 CASHVAL,DMCB,SC2.SCRPSTR,(RF)                                    
         CLI   0(R1),0             VALIDATE INPUT                               
         BNE   FLDINV                                                           
         TM    4(R1),X'80'         NEGATIVE IS INVALID                          
         BO    FLDINV                                                           
         CLC   4(4,R1),=X'0000270F'  CAN'T BE > 99.99%                          
         BH    FLDINV                                                           
         MVC   EU2.TAPCPSTR,4(R1)                                               
                                                                                
BLD63    BAS   RE,BUMPPROV         BUMP PAST PROV LINE                          
                                                                                
         AHI   R6,TAPCSLNQ                                                      
         ZIC   RF,TAPCLEN                                                       
         AHI   RF,TAPCSLNQ                                                      
         STC   RF,TAPCLEN                                                       
         BCT   R3,BLD60                                                         
                                                                                
         GOTO1 ADDELEM                                                          
                                                                                
         MVI   ELCODE,TAC1ELQ      CLAIM CODE 1 ELEMENT                         
         GOTO1 REMELEM             DELETE CURRENT                               
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAC1D,R4                                                         
         MVI   TAC1EL,TAC1ELQ      CLAIM CODE 1 ELEMENT                         
         MVI   TAC1LEN,TAC1LNQ                                                  
         MVC   TAC1UNIT,=C'CN '    CANADIAN FEDERAL                             
                                                                                
         TM    SCRFCC1H+1,X'20'                                                 
         BO    BLD65                                                            
         LA    R2,SCRFCC1H                                                      
         CLI   SCRFCC1H+5,0        ANY INPUT?                                   
         BE    FLDMISS                                                          
         ZIC   RF,SCRFCC1H+5                                                    
         GOTO1 CASHVAL,DMCB,SCRFCC1,(RF)                                        
         CLI   0(R1),0             VALIDATE INPUT                               
         BNE   FLDINV                                                           
         TM    4(R1),X'80'         NEGATIVE IS INVALID                          
         BO    FLDINV                                                           
         MVC   TAC1DCC1,4(R1)                                                   
         GOTO1 ADDELEM                                                          
                                                                                
BLD65    LA    R3,13               13 PROVINCES TO CHECK                        
         LA    R5,SCRNAMEH                                                      
                                                                                
BLD70    CLI   SC2.SCRPCC1H+5,0    ANY INPUT?                                   
         BE    BLD79                                                            
         LA    R2,SC2.SCRPCC1H                                                  
         ZIC   RF,SC2.SCRPCC1H+5                                                
         GOTO1 CASHVAL,DMCB,SC2.SCRPCC1,(RF)                                    
                                                                                
         CLI   0(R1),0             VALIDATE INPUT                               
         BNE   FLDINV                                                           
         TM    4(R1),X'80'         NEGATIVE IS INVALID                          
         BO    FLDINV                                                           
         MVC   TAC1DCC1,4(R1)                                                   
         MVC   TAC1UNIT,SC2.SCRUNIT                                             
         MVI   TAC1UNIT+2,C' '                                                  
                                                                                
         GOTO1 ADDELEM                                                          
                                                                                
BLD79    BAS   RE,BUMPPROV         BUMP PAST PROV LINE                          
         BCT   R3,BLD70                                                         
                                                                                
         B     XIT                                                              
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE                                                                  
DISPLYD  MVI   MYMSGNO1,4          RECORD DISPLAYED - ENTER CHANGES             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         L     R2,AFRSTREC                                                      
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRA2D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012TAGENA2   02/13/15'                                      
         END                                                                    
