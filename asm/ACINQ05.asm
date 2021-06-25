*          DATA SET ACINQ05    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T60605A,*,NOAUTO                                                         
         TITLE 'ACCOUNT ENQUIRY MK2 - LIST - T60605'                            
T60605   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*              TABLES FOR TYPE 'LIST' IN ACCOUNT ENQUIRY PROGRAM                
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60605)                                               
         DC    A(FILTABLE-T60605)                                               
         DC    A(KNTRYPNT-T60605)                                               
         DC    A(FNTRYPNT-T60605)                                               
         DC    A(DNTRYPNT-T60605)                                               
         DS    A                                                                
         DS    A                                                                
         EJECT                                                                  
*                                                                               
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
*                                                                               
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS+2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*                                                                               
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
*                                                                               
FILTABLE DC    CL10'ALL'                                                        
         DC    CL2'AL'                                                          
         DC    X'00'                                                            
         DC    CL8'ZERO'                                                        
         DC    X'02'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'BALANCE'                                                    
         DC    CL2'BA'                                                          
         DC    X'00'                                                            
         DC    CL8'OUT'                                                         
         DC    X'01'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'CLOSED'                                                     
         DC    CL2'CL'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'40'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL1(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'CREDIT'                                                     
         DC    CL2'CR'                                                          
         DC    X'00'                                                            
         DC    CL8'ZERO'                                                        
         DC    X'04'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACBLCR-ACBALD)                                               
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'DATE'                                                       
         DC    CL2'DA'                                                          
         DC    X'40'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL1(ACSTLAST-ACSTATD)                                            
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DEBIT'                                                      
         DC    CL2'DR'                                                          
         DC    X'00'                                                            
         DC    CL8'ZERO'                                                        
         DC    X'04'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACBLDR-ACBALD)                                               
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'40'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL1(ACSTLAST-ACSTATD)                                            
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
         DC    CL10'LEVEL'                                                      
         DC    CL2'LE'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ALEVEL-GWS)                                                  
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LEVEL'                                                      
         DC    CL2'LE'                                                          
         DC    X'00'                                                            
         DC    CL8'1-2'                                                         
         DC    X'04'                                                            
         DC    AL2(ALEVEL-GWS)                                                  
         DC    AL1(3)                                                           
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'LOCKED'                                                     
         DC    CL2'LO'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'20'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL1(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'MEDIA'                                                      
         DC    CL2'ME'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'OFFICE'                                                     
         DC    CL2'OF'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AOFFICE-GWS)                                                 
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
*&&US                                                                           
         DC    CL10'OPENDATE'                                                   
         DC    CL2'OP'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'02'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*&&                                                                             
*                                                                               
         DC    CL10'OVERRIDE'                                                   
         DC    CL2'OV'                                                          
         DC    X'80'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(3)                                                           
         DC    AL2(EDITOVER-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'PEELDATE'                                                   
         DC    CL2'PE'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'01'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'START'                                                      
         DC    CL2'ST'                                                          
         DC    X'40'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL1(ACSTLAST-ACSTATD)                                            
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITSTRT-GWS)                                                
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
*                                                                               
*              VALIDATION STAGE HANDLING OF LEVEL=, MEDIA= AND                  
*              ALL/BAL/CR/DR= FILTERS                                           
*                                                                               
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ5**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60605,RB                                                        
         USING T606TWA,RA                                                       
         USING FTBD,R6                                                          
         USING FILTERSD,R4                                                      
*                                                                               
FV1      L     R5,AIO                                                           
         CLI   0(R5),0             ARE WE VALIDATING FILTERS                    
         BNE   F01                 NO                                           
*                                                                               
FV2      CLC   FILSHTKW,=C'LE'     LEVEL                                        
         BNE   FV3                                                              
         CLI   FTBSIGN,C'P'                                                     
         BNE   FXITA                                                            
         OI    FTBSTAT,X'04'       SUB-RECORD                                   
         OI    OPTIONS,LEVFILT     SET INDICATOR & CLEAR TOTS PENDING           
         NI    OPTIONS,X'FF'-LEVTOTS                                            
         MVI   LOWLEVEL,2          SET LOWEST LEVEL OF A/C FOR PRINTING         
         CLI   FILVALUE,X'04'                                                   
         BE    FXITA                                                            
         MVC   LOWLEVEL,FTBVAL                                                  
         NI    LOWLEVEL,X'0F'                                                   
         B     FXITA                                                            
*                                                                               
FV3      CLC   FILSHTKW,=C'ME'     MEDIA                                        
         BNE   FV4                                                              
         CLC   KEY+1(2),PRODUNIT   MUST BE PRODUCTION                           
         BE    *+16                                                             
         MVI   DMCB,1                                                           
         MVI   ERROR,LEDGNVAL                                                   
         B     FXITA                                                            
         ZIC   R1,SAVEHIER+2       GET DISPLACEMENT INTO KEY OF MEDIA           
         LA    R1,3(R1)                                                         
         STC   R1,FILDISP                                                       
         B     FXITA                                                            
*                                                                               
FV4      TM    OPTIONS,LEVFILT     ELSE CREATE FILTER TO DROP RECS              
         BO    FXITA               WITHOUT BAL ELS - UNLESS LEVEL FILT          
         BO    FXITA                                                            
         MVC   FTBTBLEN(FTBTBLEN,R6),0(R6)                                      
         XC    0(FTBTBLEN,R6),0(R6)                                             
         LA    RE,ABAL                                                          
         ST    RE,FTBELMNT                                                      
         MVI   FTBLEN,1                                                         
         MVI   FTBMARK,C'C'                                                     
         MVI   FTBSIGN,C'P'                                                     
         MVI   FTBVAL,X'32'                                                     
         LA    R6,FTBTBLEN(R6)                                                  
FXITA    XIT1  REGS=(R6)                                                        
         EJECT                                                                  
*                                                                               
*              HANDLE ALL/BAL/DR/CR FILTERS AT FILTERING STAGE                  
*                                                                               
F01      MVI   WORK,0                                                           
         TM    OPTIONS,LEVFILT     IF WERE LEVEL FILTERING                      
         BZ    F01A                DONT DO OTHER RECORD FILTERING               
         LA    R1,IOZ              UNLESS THIS IS THE LOWEST DISPLAY            
         C     R1,AKEY             LEVEL                                        
         BE    F01A                                                             
         CLI   FTBSIGN,C'N'                                                     
         BE    FXIT                                                             
         MVC   WORK(1),FTBVAL                                                   
         B     FXIT                                                             
*                                                                               
F01A     ICM   R5,15,ABAL                                                       
         BZ    TXIT                                                             
*                                                                               
         USING ACBALD,R5                                                        
F01B     CLI   FTBVAL,1            BALANCE=OUT                                  
         BE    F05                                                              
         CLI   FTBVAL,2            ALL=ZERO                                     
         BE    F10                                                              
*                                                                               
F02      MVI   WORK,4              DR OR CR=ZERO                                
         LA    R2,ACBLDR                                                        
         CLI   FTBDISP,(ACBLDR-ACBALD)                                          
         BE    *+8                                                              
         LA    R2,ACBLCR                                                        
         CP    0(L'ACBLDR,R2),=P'0'                                             
         BE    FXIT                                                             
         MVI   WORK,0                                                           
         B     FXIT                                                             
*                                                                               
F05      ZAP   WORK(8),ACBLFRWD                                                 
         SP    WORK(8),ACBLCR                                                   
         AP    WORK(8),ACBLDR                                                   
         CP    WORK(8),=P'0'                                                    
         BE    *+12                IF ITS OUT                                   
         MVI   WORK,1              THE TEST UNDER MASK BIT IS SET ON            
         B     FXIT                                                             
         MVI   WORK,0              OTHERWISE NOT                                
         B     FXIT                                                             
*                                                                               
F10      MVI   WORK,0              ALL=ZERO                                     
         CP    ACBLFRWD,=P'0'                                                   
         BNE   FXIT                                                             
         CP    ACBLCR,=P'0'                                                     
         BNE   FXIT                                                             
         CP    ACBLDR,=P'0'                                                     
         BNE   FXIT                                                             
         MVI   WORK,2              IF ALL ARE ZERO THE BIT IS SET ON            
*                                                                               
FXIT     LA    R2,WORK                                                          
         XIT1  REGS=(R2)                                                        
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*              MAIN PROCESS                                                     
*                                                                               
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQ5**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60605,RB                                                        
         USING T606TWA,RA                                                       
         L     R5,AIO                                                           
         CLI   0(R5),RUNLAST       END                                          
         BNE   T0                                                               
         TM    OPTIONS,LEVTOTS     ANYTHING STILL TO DISPLAY                    
         BZ    TXIT                NO                                           
         B     T0D                 YES                                          
*                                                                               
T0       TM    OPTIONS,LEVFILT     ANY LEVEL FILTER                             
         BZ    T1                                                               
         MVI   DMCB,0                                                           
         GOTO1 AFILTER                                                          
         BNZ   T0A                                                              
         ICM   R1,15,ABAL          REC NOT REQUIRED SO ADD ITS BALANCE          
         BZ    TNEXT               VALUES TO HIGHER LEVEL                       
         ICM   R5,15,ASAV32                                                     
         BZ    TNEXT                                                            
         USING ACBALD,R5                                                        
         AP    ACBLFRWD,(ACBLFRWD-ACBALD)(L'ACBLFRWD,R1)                        
         AP    ACBLDR,(ACBLDR-ACBALD)(L'ACBLDR,R1)                              
         AP    ACBLCR,(ACBLCR-ACBALD)(L'ACBLCR,R1)                              
         ICM   R5,15,ATIA          AND RESET DATES AT HIGHER LEVEL              
         BZ    TNEXT               (ATIA CONTAINS SAVED STATUS EL ADDR)         
         ICM   R1,15,ASTA                                                       
         BZ    TNEXT                                                            
         USING ACSTATD,R5                                                       
         CLC   ACSTLAST,(ACSTLAST-ACSTATD)(R1)                                  
         BNL   *+10                                                             
         MVC   ACSTLAST,(ACSTLAST-ACSTATD)(R1)                                  
         CLC   ACSTBFDT,(ACSTBFDT-ACSTATD)(R1)                                  
         BNL   TNEXT                                                            
         MVC   ACSTBFDT,(ACSTBFDT-ACSTATD)(R1)                                  
         B     TNEXT                                                            
*                                                                               
T0A      TM    OPTIONS,LEVTOTS     REC IS REQUIRED - ANY DISPLAY PENDNG         
         BNZ   T0D                 IF SO GO AND DEAL WITH IT                    
*                                                                               
T0B      CLI   LINE+1,19                                                        
         BE    TFULL                                                            
         CLC   RECOUNT,LIMIT       NOW CHECK REC LIMIT                          
         BH    T8                                                               
         CLC   LOWLEVEL,LEVEL+1    IF THIS IS THE LOWEST DISPLAY LEVEL          
         BNE   T1                  SAVE REC IN IOZ & ENSURE IT HAS A            
         OI    OPTIONS,LEVTOTS                                                  
         L     RE,AIO              BALANCE ELEMENT                              
         USING ACKEYD,RE                                                        
         LH    RF,ACLENGTH                                                      
         DROP  RE                                                               
         LR    R1,RF                                                            
         LA    R0,IOZ                                                           
         ST    R0,DMCB                                                          
         MVCL  R0,RE                                                            
         GOTO1 ASETELAD                                                         
         ICM   R5,15,ABAL                                                       
         BNZ   T0C                                                              
*                                                                               
         ICM   R5,15,AKEY                                                       
         USING ACKEYD,R5                                                        
         LH    R2,ACLENGTH                                                      
         LA    R1,X'1A'(R2)                                                     
         STH   R1,ACLENGTH                                                      
         AR    R5,R2                                                            
         BCTR  R5,0                                                             
         USING ACBALD,R5                                                        
         XC    ACBALD(ACBLLNQ),ACBALD BUILD A BALANCE ELEMENT                   
         MVI   ACBLEL,ACBLELQ                                                   
         MVI   ACBLLEN,ACBLLNQ                                                  
         ZAP   ACBLFRWD,=P'0'                                                   
         ZAP   ACBLDR,=P'0'                                                     
         ZAP   ACBLCR,=P'0'                                                     
         ZAP   ACBLURG,=P'0'                                                    
         MVI   ACBALD+ACBLLNQ,0                                                 
*                                                                               
T0C      ST    R5,ASAV32           SAVE ITS ADDRESS                             
         MVC   ATIA,ASTA           AND STATUS EL ADDR (IN ATIA)                 
         B     TNEXT                                                            
         EJECT                                                                  
*                                                                               
*              DISPLAY A LINE                                                   
*                                                                               
T0D      LA    R1,IOZ             COME HERE IF WE HAVE A SAVED REC TO           
         ST    R1,DMCB            DISPLAY                                       
         GOTO1 ASETELAD                                                         
         MVI   DMCB,C'R'          APPLY REC LEVEL FILTERS                       
         GOTO1 AFILTER                                                          
         BZ    T7                                                               
*                                                                               
T1       CLI   LINE+1,19           COME HERE TO DISPLAY A RECORD                
         BE    TFULL                                                            
         CLI   VIRGIN,C'H'                                                      
         BE    T2                                                               
         MVI   VIRGIN,C'H'                                                      
         CLI   LINE+1,3                                                         
         BNL   T2                                                               
         MVC   INFDAT2,HEADING1    FIRST TIME SET UP HEADINGS                   
         MVC   INFDAT3,HEADING2                                                 
         OI    INFDAT2H+6,X'80'                                                 
         OI    INFDAT3H+6,X'80'                                                 
         TM    OPTIONS,PEELDATE    OPTION TO SHOW PEEL DATE INSTEAD OF          
         BNO   *+10                ACTIVITY DATE                                
         MVC   INFDAT2+30(8),=CL8'  PEEL'                                       
*&&US                                                                           
         TM    OPTIONS,OPENDATE    OPTION TO SHOW OPEN DATE INSTEAD OF          
         BNO   *+10                ACTIVITY DATE                                
         MVC   INFDAT2+30(8),=CL8' OPENED '                                     
*&&                                                                             
         MVI   LINE+1,3                                                         
         B     T2                                                               
HEADING1 DC    CL39'ACCOUNT      ACCOUNT NAME     ACTIVITY '                    
         DC    CL39'  B/FRWD     DEBIT    CREDIT   BALANCE'                     
HEADING2 DC    CL39'-------      ------------       DATE   '                    
         DC    CL39'  ------     -----    ------   -------'                     
*                                                                               
T2       LH    R6,LINE             SET UP DISPLAY FOR A RECORD                  
         LR    R9,R6               R9 = LINE NUMBER                             
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         ICM   R5,15,ANAM                                                       
         BZ    T4                                                               
         USING ACNAMED,R5          NAME - ON UP TO 3 LINES                      
         ZIC   R7,ACNMLEN                                                       
         SH    R7,=H'2'                                                         
         GOTO1 VCHOPPER,DMCB,((R7),ACNMNAME),(16,SCANBLCK),3                    
         ICM   R7,15,DMCB+8                                                     
         BZ    T4                                                               
         AR    R9,R7                                                            
         CH    R9,=H'20'                                                        
         BNL   TFULL                                                            
         BCTR  R9,0                                                             
         LR    R8,R6                                                            
         USING LINED,R8                                                         
         LA    R3,SCANBLCK                                                      
*                                                                               
T3       OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA+13(16),0(R3)                                            
         LA    R3,16(R3)                                                        
         LA    R8,86(R8)                                                        
         BCT   R7,T3                                                            
         DROP  R8                                                               
*                                                                               
T4       L     R5,AKEY                                                          
         USING ACKEYD,R5                                                        
         MVC   LINEDATA(12),ACKEYACC+3                                          
         ICM   R5,15,ASTA                                                       
         BZ    T4A                                                              
         USING ACSTATD,R5          DATE                                         
         LA    R3,ACSTLAST                                                      
         TM    OPTIONS,PEELDATE                                                 
         BNO   *+8                                                              
         LA    R3,ACSTBFDT                                                      
         GOTO1 VDATCON,DMCB,(1,(R3)),(8,LINEDATA+30)                            
*&&UK*&& OI    LINEDATA+30,X'F0'                                                
*                                                                               
T4A      DS    0H                                                               
*&&US                                                                           
         TM    OPTIONS,OPENDATE                                                 
         BNO   T5                                                               
         MVC   LINEDATA+30(8),SPACES                                            
         ICM   R5,15,AJOB          JOB ELEMENT                                  
         BZ    T5                                                               
         USING ACJOBD,R5          DATE                                          
         CLI   ACJBLEN,16                                                       
         BL    T5                                                               
         OC    ACJBOPND,ACJBOPND                                                
         BZ    T5                                                               
         LA    R3,ACJBOPND                                                      
         GOTO1 VDATCON,DMCB,(1,(R3)),(8,LINEDATA+30)                            
*&&                                                                             
T5       ICM   R5,15,ABAL                                                       
         BZ    T6                                                               
         USING ACBALD,R5           VALUES                                       
         LA    R2,ACBLFRWD                                                      
         LA    R3,LINEDATA+38                                                   
         BAS   RE,EDIT                                                          
         LA    R2,ACBLDR                                                        
         LA    R3,LINEDATA+48                                                   
         BAS   RE,EDIT                                                          
         LA    R2,ACBLCR                                                        
         LA    R3,LINEDATA+58                                                   
         BAS   RE,EDIT                                                          
         SP    ACBLFRWD,ACBLCR                                                  
         AP    ACBLFRWD,ACBLDR                                                  
         LA    R2,ACBLFRWD                                                      
         LA    R3,LINEDATA+68                                                   
         LA    RE,T6                                                            
*                                                                               
EDIT     ZAP   DUB,0(L'ACBLDR,R2)                                               
         EDIT  (P8,DUB),(13,WORK+20),2,MINUS=YES                                
         CLC   WORK+20(9),SPACES                                                
         BER   RE                                                               
         MVC   WORK+29(1),WORK+32                                               
         MVC   0(10,R3),WORK+20                                                 
         BR    RE                                                               
*                                                                               
T6       OI    LINEHDR+6,X'80'                                                  
         LA    R9,1(R9)                                                         
         STH   R9,LINE                                                          
         XC    ASAV32,ASAV32       CLEAR OUT SAVE ELEMENT                       
*                                                                               
T7       L     R1,AIO                                                           
         CLI   0(R1),RUNLAST                                                    
         BE    TXIT                                                             
         TM    OPTIONS,LEVTOTS                                                  
         BZ    TNEXT                                                            
         NI    OPTIONS,X'FF'-LEVTOTS                                            
         ST    R1,DMCB                                                          
         GOTO1 ASETELAD                                                         
         B     T0B                                                              
*                                                                               
T8       L     R5,AIO                                                           
         USING ACTRECD,R5                                                       
         ZIC   R1,ACTRECD+L'ACTKCULA-1   REC LIMIT EXCEEDED SO FORCE            
         BCTR  R1,0                ROOT TO RE-READ AND COP OUT                  
         STC   R1,ACTRECD+L'ACTKCULA-1                                          
         DROP  R5                                                               
*                                                                               
TNEXT    LTR   RB,RB                                                            
         B     TXIT                                                             
*                                                                               
TFULL    TM    OPTIONS,LEVTOTS     SET CC TO NEG FOR SCREEN FULL                
         BZ    TFULL2                                                           
         L     R1,AIO                                                           
         LA    RE,IOZ                                                           
         MVC   0(L'ACCKEY,R1),0(RE) RESTORE SAVED KEY IF NEC.                   
*                                                                               
TFULL2   LNR   RB,RB                                                            
         MVI   LINE+1,3            AND SAVE HEADS                               
TXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
LEVFILT  EQU   X'40'                                                            
LEVTOTS  EQU   X'20'                                                            
OPENDATE EQU   X'02'                                                            
PEELDATE EQU   X'01'                                                            
         EJECT                                                                  
*                                                                               
*              NESTED INCLUDE FOR ACINQDSECT                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
*                                                                               
*              STORAGE HOOKED ONTO THE END OF THE TWA                           
*                                                                               
         ORG   TWANEXT                                                          
ASAV32   DS    A                                                                
IOZ      DS    CL2000                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACINQ05   05/01/02'                                      
         END                                                                    
