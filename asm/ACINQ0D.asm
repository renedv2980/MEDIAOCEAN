*          DATA SET ACINQ0D    AT LEVEL 026 AS OF 05/01/02                      
*PHASE T6060DA,*,NOAUTO                                                         
         TITLE 'ACCOUNT ENQUIRY MK2 - ORDER LIST - T6060D'                      
T6060D   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLE FOR TYPE 'OL' IN ACCOUNT ENQUIRY PROGRAM                   
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T6060D)                                               
         DC    A(FILTABLE-T6060D)                                               
         DC    A(KNTRYPNT-T6060D)                                               
         DC    A(FNTRYPNT-T6060D)                                               
         DC    A(DNTRYPNT-T6060D)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'RECORDTYPE'                                                 
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'1A'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'SPARE'                                                      
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(2)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ORDER #'                                                    
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(4)                                                           
         DC    AL1(6)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'THE REST'                                                   
         DC    C' '                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(10)                                                          
         DC    AL1(32)                                                          
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'CLIENT'                                                     
         DC    CL2'CL'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AORD-GWS)                                                    
         DC    AL1(ACORJOB+3-ACORDRD)                                           
         DC    AL1(6)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DATE'                                                       
         DC    CL2'DA'                                                          
         DC    X'40'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AORD-GWS)                                                    
         DC    AL1(ACORDATE-ACORDRD)                                            
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DELETED'                                                    
         DC    CL2'DE'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'40'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AORD-GWS)                                                    
         DC    AL1(ACORDATE-ACORDRD)                                            
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
         DC    CL10'SUPPLIER'                                                   
         DC    CL2'SU'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AORD-GWS)                                                    
         DC    AL1(ACORSUPP+3-ACORDRD)                                          
         DC    AL1(6)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'START'                                                      
         DC    CL2'ST'                                                          
         DC    X'40'                                                            
*&&UK*&& DC    CL8'DDMMMYY'                                                     
*&&US*&& DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AORD-GWS)                                                    
         DC    AL1(ACORDATE-ACORDRD)                                            
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITSTRT-GWS)                                                
*                                                                               
         DC    CL10'WORKCODE'                                                   
         DC    CL2'WC'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AOAM-GWS)                                                    
         DC    AL1(ACOAWC-ACOAMTD)                                              
         DC    AL1(L'ACOAWC)                                                    
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'TYPE'                                                       
         DC    CL2'TY'                                                          
         DC    X'00'                                                            
         DC    CL8'PR'                                                          
         DC    X'01'                                                            
         DC    AL2(AORD-GWS)                                                    
         DC    AL1(ACORJOB+1-ACORDRD)                                           
         DC    AL1(3)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'TYPE'                                                       
         DC    CL2'TY'                                                          
         DC    X'00'                                                            
         DC    CL8'EXP'                                                         
         DC    X'02'                                                            
         DC    AL2(AORD-GWS)                                                    
         DC    AL1(ACORJOB+1-ACORDRD)                                           
         DC    AL1(3)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
         EJECT                                                                  
KNTRYPNT DS    0D                  RIGHT ALIGN INPUT ORDER # IN KEY             
         NMOD1 0,**INQD**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060D,RB                                                        
         USING T606TWA,RA                                                       
         MVC   WORK(6),=6C'0'                                                   
         LA    RF,WORK+5                                                        
         ZIC   RE,INFKEYH+5                                                     
         SH    RE,=H'1'                                                         
         BM    KXIT                                                             
         CLI   INFKEYH+5,6                                                      
         BNH   *+16                                                             
         MVI   ERROR,37            INPUT FLD TOO LONG                           
         MVI   DMCB,1              ERROR                                        
         B     KXIT                                                             
         SR    RF,RE                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)                                                    
KXIT     LA    R2,WORK                                                          
         XIT1  REGS=(R2)                                                        
*                                                                               
FNTRYPNT DS    0H                                                               
         NMOD1 0,**INQD**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060D,RB                                                        
         USING T606TWA,RA                                                       
         USING FTBD,R6                                                          
         MVI   FTBLEN,2            LENGTH OF COMPARE                            
         MVI   FTBMARK,C'C'        COMPARE                                      
         TM    FTBVAL,X'02'                                                     
         BZ    *+8                                                              
         XI    FTBSIGN,X'02'                                                    
         MVC   FTBVAL(2),=C'SJ'                                                 
         XIT1                                                                   
         EJECT                                                                  
*              MAIN PROCESS                                                     
         SPACE 1                                                                
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQD**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060D,RB                                                        
         USING T606TWA,RA                                                       
         L     R7,ALOCAL                                                        
         USING LOCALD,R7                                                        
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         B     T02                                                              
         EJECT                                                                  
*              FIRST TIME ACTIONS                                               
         SPACE 1                                                                
T02      OC    LINE,LINE                                                        
         BNZ   T07                                                              
         ZAP   ACESTT,=P'0'                                                     
         ZAP   ACNUMT,=P'0'                                                     
         ZAP   ACACTT,=P'0'                                                     
         MVC   INFDATA,HEADING                                                  
         OI    INFDATAH+6,X'80'                                                 
         MVC   INFDAT2,HEADING2                                                 
         OI    INFDAT2H+6,X'80'                                                 
         MVI   LINE+1,2                                                         
T07      MVI   VIRGIN,C'H'                                                      
         B     T1                                                               
HEADING  DC    CL42'---ORDER---     SUPPLIER       JOB/EXPENSE'                 
*&&UK*&& DC    CL36'    WORKCODE       ORDER   INVOICED '                       
*&&US*&& DC    CL36'    WORKCODE(S)    ORDER   INVOICED '                       
HEADING2 DC    CL42'NUMBER DATE     --------       -----------'                 
*&&UK*&& DC    CL36'    --------      AMOUNT     AMOUNT '                       
*&&US*&& DC    CL36'    -----------   AMOUNT     AMOUNT '                       
         EJECT                                                                  
*              EACH TIME ACTIONS                                                
         SPACE 1                                                                
T1       CLI   LINE+1,18           CHECK FOR SCREEN FULL                        
         BH    TFULL                                                            
         SPACE 1                                                                
T2       LH    R6,LINE             SET UP DISPLAY FOR A RECORD                  
         LR    R9,R6               R9 = LINE NUMBER                             
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         CLI   0(R5),RUNLAST                                                    
         BE    TEND                                                             
         SPACE 1                                                                
T3       ICM   R4,15,AORD                                                       
         BZ    TNEXT                                                            
         USING ACORDRD,R4                                                       
         SPACE 1                                                                
T3A      CLI   DELETED,C'Y'        HANDLE DELETES                               
         BE    *+16                                                             
         TM    ACSTATUS-ACKEYD(R5),ACORDMCH+ACORDDEL+ACORDPHS                   
         BNZ   TNEXT                                                            
         B     *+12                                                             
         TM    ACSTATUS-ACKEYD(R5),ACORDMCH+ACORDDEL+ACORDPHS                   
         BZ    TNEXT                                                            
         SPACE 1                                                                
         MVC   LINEDATA(6),4(R5)                                                
         GOTO1 VDATCON,DMCB,(1,ACORDATE),(8,LINEDATA+7)                         
*&&UK*&& OI    LINEDATA+7,X'F0'                                                 
         MVC   LINEDATA+16(14),ACORSUPP+1                                       
         MVC   LINEDATA+31(14),ACORJOB+1                                        
         CLC   ACORJOB+1(2),PRODUNIT U/L IF EXPENSE ORDER                       
         BNE   T3B                                                              
         MVC   LINEDATA+31(14),SPACES                                           
         MVC   LINEDATA+31(12),ACORJOB+3                                        
T3B      BAS   RE,SECCHK                                                        
         BE    T3D                 BAD OFFICE SECURITY                          
         XC    LINEDATA,LINEDATA                                                
         B     TNEXT                                                            
T3D      ICM   R4,15,AOAM                                                       
         BZ    TNEXT                                                            
         USING ACOAMTD,R4                                                       
         ZAP   ORESTT,=P'0'                                                     
         ZAP   ORNUMT,=P'0'                                                     
         ZAP   ORACTT,=P'0'                                                     
         LA    RF,LINEDATA+46                                                   
         SR    RE,RE                                                            
         LA    R0,4                SET UP LOOP FOR MAX WC'S PER LINE            
         B     T5                                                               
*                                                                               
T4       IC    RE,ACOALEN          LOOP FOR A WORKCODE                          
         AR    R4,RE                                                            
         CLI   0(R4),X'68'                                                      
         BNE   T6                                                               
         BCT   R0,T4A              CAN ONLY HOLD 4 ON A LINE                    
         OI    LINEHDR+6,X'80'                                                  
         LA    R9,1(R9)                                                         
         STH   R9,LINE             GET TO NEXT LINE                             
         LR    R6,R9                                                            
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         LA    RF,LINEDATA+46                                                   
         LA    R0,4                                                             
         B     T5                  GET THE REST                                 
*                                                                               
T4A      MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
T5       MVC   0(2,RF),ACOAWC                                                   
         LA    RF,2(RF)                                                         
         AP    ORESTT,ACOAMT                                                    
         AP    ORNUMT,ACOAINUM                                                  
         AP    ORACTT,ACOAIVAL                                                  
         B     T4                                                               
         SPACE 1                                                                
T6       LA    RF,LINEDATA-2       DISPLAY ORDER TOTS AND ADD TO GRAND          
         EDIT  (P6,ORESTT),(10,59(RF)),2,MINUS=YES                              
         CP    ORACTT,=P'0'                                                     
         BE    T7                                                               
         EDIT  (P6,ORACTT),(10,70(RF)),2,MINUS=YES                              
T7       AP    ACESTT,ORESTT                                                    
         AP    ACNUMT,ORNUMT                                                    
         AP    ACACTT,ORACTT                                                    
         OI    LINEHDR+6,X'80'                                                  
         LA    R9,1(R9)                                                         
         STH   R9,LINE                                                          
         SPACE 1                                                                
TNEXT    LTR   RB,RB                                                            
         B     TXIT                                                             
         EJECT                                                                  
*              RUNLAST ACTIONS                                                  
         SPACE 1                                                                
TEND     MVC   LINEDATA(5),=C'TOTAL'                                            
         LA    RF,LINEDATA-2                                                    
         EDIT  (P6,ACESTT),(11,58(RF)),2,MINUS=YES                              
         EDIT  (P6,ACACTT),(11,69(RF)),2,MINUS=YES                              
         OI    LINEHDR+6,X'80'                                                  
         SPACE 1                                                                
TEND1    SR    RF,RF               SET CC TO EQU FOR END                        
         B     TXIT                                                             
         SPACE 1                                                                
TFULL    LNR   RB,RB               SET CC TO NEGATIVE FOR SCREEN FULL           
         MVI   LINE+1,2            AND SAVE SOME HEADS                          
TXIT     XIT1                                                                   
         EJECT                                                                  
* SECURITY CHECK SUB-ROUTINE NEVER HANDLED EXPENSE ACCOUNTS                     
* PROPERLY.  FOR THE PRESENT, THIS ROUTINE WILL ONLY ATTEMPT TO                 
* DEAL WITH JOBS.                                                               
*                                                                               
         USING ACORDRD,R4                                                       
SECCHK   NTR1  ,                                                                
         CLC   ACORJOB+1(2),PRODUNIT TEST FOR PRODUCTION LEDGER                 
         BNE   SECX                NO-PASS SECURITY                             
         CLI   PRODOPOS,0          ONLY READ PROD LEDG FIRST TIME               
         BNE   SEC10                                                            
*                                                                               
SEC2     MVC   KEYB(L'ACCKEY),SPACES                                            
         MVC   KEYB(3),ACORJOB     IE. CUL                                      
         GOTO1 AREADB                                                           
         BZ    SECY                                                             
         L     R9,AIOB                                                          
         MVI   ELCODE,ACLTELQ      LEDGER ELEMENT                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACLEDGD,R9                                                       
         MVC   PRODOPOS,ACLTOFF    EXTRACT OFFICE POSITION                      
*                                                                               
SEC4     L     R9,AIOB                                                          
         MVI   ELCODE,ACHRELQ      HIERARCHY ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACHEIRD,R9                                                       
         MVC   CLILEN,ACHRLEVA     SAVE LENGTH OF CLIENT CODE                   
         MVC   PROLEN,ACHRLEVB     LENGTH OF CLIENT/PRODUCT                     
*                                                                               
SEC10    MVC   KEYB(L'ACCKEY),SPACES                                            
         ZIC   R1,CLILEN                                                        
         LA    R1,2(R1)            READ THE CLIENT                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEYB(0),ACORJOB                                                  
         GOTO1 AREADB                                                           
         BE    SECY                                                             
*                                                                               
         L     R9,AIOB                                                          
         MVI   ELCODE,ACPRELQ      PROFILE ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   SECY                MISSING PROFILE THEN OK                      
         USING ACPROFD,R9                                                       
         MVC   MYOFFICE,ACPROFFC   EXTRACT OFFICE CODE                          
*                                                                               
         L     R9,AIOB                                                          
         MVI   ELCODE,ACSTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SECY                                                             
         USING ACSTATD,R9                                                       
         CLC   TERMAUTH+1(1),ACSTSECY+1                                         
         BL    SECY                SECURITY VIOLATION                           
*                                                                               
SEC15    MVC   KEYB(L'ACCKEY),SPACES                                            
         ZIC   R1,PROLEN           READ THE PRODUCT                             
         LA    R1,2(R1)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEYB(0),ACORJOB                                                  
         GOTO1 AREADB                                                           
         BE    SECY                                                             
*                                                                               
         L     R9,AIOB                                                          
         MVI   ELCODE,ACPRELQ      GET THE PROFILE ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   SEC20               SOME PRODUCTS DO NOT HAVE PROFILE            
         USING ACPROFD,R9                                                       
         CLC   ACPROFFC,SPACES                                                  
         BNH   *+10                                                             
         MVC   MYOFFICE,ACPROFFC                                                
*                                                                               
SEC20    L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AIOB                                                     
         MVC   OFFAOPOS,PRODOPOS                                                
         MVC   OFFAOFFC,MYOFFICE                                                
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BE    SECX                PASSES SECURITY                              
*                                                                               
SECY     LTR   RB,RB               BAD                                          
         B     TXIT                                                             
SECX     CR    RB,RB               GOOD                                         
         B     TXIT                                                             
         DROP  R1,R4,R9                                                         
*                                                                               
         GETEL R9,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              LOCAL WS AND EQUATES                                             
         SPACE 1                                                                
LOCALD   DSECT                                                                  
ORESTT   DS    PL6                 ORDER TOTALS                                 
ORNUMT   DS    PL6                                                              
ORACTT   DS    PL6                                                              
MYOFFICE DS    CL2                                                              
         SPACE 3                                                                
*                                                                               
*              NESTED INCLUDES FOR ACINQDSECT                                   
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
ACESTT   DS    PL6                 GRAND TOTALS                                 
ACNUMT   DS    PL6                                                              
ACACTT   DS    PL6                                                              
PRODOPOS DS    X                   OFFICE POSITION                              
CLILEN   DS    C                   LENGTH OF CLIENT IN ACORJOB                  
PROLEN   DS    C                   LENGTH OF CLIENT/PRODUCT IN ACORJOB          
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACINQ0D   05/01/02'                                      
         END                                                                    
