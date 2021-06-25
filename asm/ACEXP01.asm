*          DATA SET ACEXP01    AT LEVEL 086 AS OF 09/11/02                      
*PHASE T61501A,*                                                                
         TITLE 'T61501 - COKE EXPENDITURE - ACCOUNT ADD/CHA/DIS/DEL'            
T61501   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61501,R7,R8,RR=R2                                             
         L     RC,0(,R1)                                                        
*                                                                               
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
*                                                                               
         USING T615FFD,RA                                                       
         USING SYSD,R9                                                          
*                                                                               
         LA    R6,SAVXTWA                                                       
         USING LWSD,R6                                                          
         ST    R2,RELO                                                          
         GOTO1 AUTH                VALID AUTHORIZATION                          
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         EJECT ,                                                                
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   ACC10                                                            
         LA    R4,KEY              BUILD THE RECORD KEY                         
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         LA    R2,LOGUNITH                                                      
         GOTO1 ANY                 UNIT IS ALWAYS REQUIRED                      
         MVC   ACKEYACC+1(1),WORK                                               
         CLC   ACKEYACC+1(1),UNTLDG                                             
         BE    ACC03               SAME UNIT                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGUNAM,WORK                                                     
         OI    LOGUNAMH+6,X'80'                                                 
         MVC   UNTLDG,ACKEYACC+1                                                
         SPACE 1                                                                
ACC03    LA    R2,LOGLEDGH                                                      
         GOTO1 ANY                 LEDGER IS ALWAYS REQUIRED                    
         MVC   ACKEYACC+2(1),WORK                                               
         BAS   RE,LDGET                                                         
         SPACE 1                                                                
ACC05    LA    R2,LOGACCH                                                       
         GOTO1 ANY                                                              
         MVC   ACKEYACC+3(12),WORK    ACCOUNT KEY                               
         B     XIT                                                              
         EJECT                                                                  
ACC10    CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    ACC15                                                            
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BE    ACC15                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BE    ACC15                                                            
         CLI   MODE,XRECDEL        OR DELETED                                   
         BE    ACC15                                                            
         CLI   MODE,XRECREST       OR RESTORED                                  
         BNE   ACC20                                                            
ACC15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 1                                                                
ACC20    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   ACC30                                                            
         MVI   GLSTAT,0                                                         
         BAS   RE,BLDREC                                                        
         L     R2,AIO                                                           
         CLC   1(2,R2),=C'3P'      ONLY ON 3P                                   
         BNE   XIT                                                              
         LA    R2,LOGPROFH                                                      
         TM    GLSTAT,GLSYES       GL=#### PROFILE REQ'D ON 3P                  
         BNO   ERRGLREQ                                                         
         B     XIT                                                              
         SPACE 1                                                                
ACC30    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   ACC40                                                            
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVC   LOGUNIT,ACKEYACC+1                                               
         OI    LOGUNITH+6,X'80'                                                 
         MVC   LOGLEDG,ACKEYACC+2                                               
         OI    LOGLEDGH+6,X'80'                                                 
         MVC   LOGACC,ACKEYACC+3                                                
         OI    LOGACCH+6,X'80'                                                  
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         MVC   HOLDKEY,KEY                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(2),ACKEYACC                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGUNAM,WORK                                                     
         OI    LOGUNAMH+6,X'80'                                                 
         MVC   KEY(3),ACKEYACC                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGLNAM,WORK                                                     
         OI    LOGLNAMH+6,X'80'                                                 
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'HOLDKEY),HOLDKEY                                           
         B     XIT                                                              
         SPACE 1                                                                
ACC40    CLI   MODE,RECDEL                                                      
         BNE   XIT                                                              
         B     AC40                DELETE RECORD                                
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         BAS   RE,LDGET            GET LEDGER ELEMENT                           
         TWAXC LOGANAMH,LOGTABH                                                 
         GOTO1 NAMOUT,DMCB,AIO,LOGANAM     PUT OUT NAME                         
         GOTO1 ADDROUT,DMCB,AIO,BLOCK      AND ADDRESS                          
         LA    R3,BLOCK                                                         
         MVC   LOGADD1,0(R3)                                                    
         LA    R3,26(R3)                                                        
         MVC   LOGADD2,0(R3)                                                    
         LA    R3,26(R3)                                                        
         MVC   LOGADD3,0(R3)                                                    
         LA    R3,26(R3)                                                        
         MVC   LOGADD4,0(R3)                                                    
         BAS   RE,ACDSLY           DISPLAY STATUS ELEMENTS                      
         LA    R2,LOGANAMH                                                      
         B     XIT                                                              
         EJECT                                                                  
ACDSLY   NTR1                                                                   
         GOTO1 GETL,DMCB,(X'30',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
         L     R4,ELADDR                                                        
         USING ACSTATD,R4                                                       
         MVC   LOGFILT,ACSTFILT                                                 
         MVC   LOGFLT2,ACSTFILT+1                                               
         MVC   LOGANAL,SPACES                                                   
*&&US                                                                           
         CLC   KEY+1(2),=C'SP'     PRINT AND SPOT PAYEES                        
         BE    ACDSP1              HAVE HEX IN ACSTANAL                         
         CLC   KEY+1(2),=C'SS'                                                  
         BE    ACDSP1                                                           
         CLC   KEY+1(2),=C'ST'                                                  
         BE    ACDSP1                                                           
         CLC   KEY+1(2),=C'SQ'                                                  
         BE    ACDSP1                                                           
*&&                                                                             
*&&UK                                                                           
         CLC   KEY+1(2),=C'SF'                                                  
         BE    ACDSP1                                                           
*&&                                                                             
         MVC   LOGANAL,ACSTANAL                                                 
ACDSP1   MVC   LOGSUB,ACSTSUB                                                   
         SPACE 1                                                                
         CLC   ACSTSECY,SPACES                                                  
         BE    AC10A                                                            
         EDIT  (2,ACSTSECY),(3,LOGSEC),ALIGN=LEFT                               
         SPACE 1                                                                
AC10A    LA    R2,BLOCK                                                         
         SR    R5,R5                                                            
         MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTAT,X'80'                                                   
         BZ    AC11A                                                            
         MVC   0(5,R2),=C'STAFF'                                                
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
*C11     MVC   0(26,R2),SPACES                                                  
*        TM    ACSTSTAT,X'10'                                                   
*        BZ    AC11A                                                            
*        MVC   0(4,R2),=C'DEPT'                                                 
*        MVI   10(R2),C'Y'                                                      
*        LA    R2,26(R2)                                                        
*        LA    R5,1(R5)                                                         
*        SPACE 1                                                                
AC11A    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTAT,X'40'                                                   
         BZ    AC11B                                                            
         MVC   0(6,R2),=C'CLOSED'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC11B    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTAT,X'20'                                                   
         BZ    AC11C                                                            
         MVC   0(6,R2),=C'LOCKED'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC11C    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTAT,X'08'                                                   
         BZ    AC11D                                                            
         MVC   0(7,R2),=C'OUTFILE'                                              
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
*       - - - - - - - -                                                         
AC11D    MVC   0(26,R2),SPACES DEFAULT TASK CODE FOR TMS                        
         CLI   ACSTLEN,ACSTLNQ3                                                 
         BL    AC11E                                                            
         CLC   ACSTDTSK,SPACES                                                  
         BNH   AC11E                                                            
         MVC   0(4,R2),=C'TASK'                                                 
         MVC   10(2,R2),ACSTDTSK    DISPLAY TASK CODE                           
         LA    R2,26(,R2)                                                       
         LA    R5,1(,R5)                                                        
*       - - - - - - - -                                                         
AC11E    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTAT,X'04'                                                   
         BZ    AC11F                                                            
*&&US*&& MVC   0(4,R2),=C'EXEC'                                                 
*&&UK*&& MVC   0(7,R2),=C'PLUSVAT'                                              
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
*&&US                                                                           
AC11F    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTAT,X'02'                                                   
         BZ    AC11G                                                            
         MVC   0(6,R2),=C'VEND2C'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
*&&                                                                             
*&&UK                                                                           
AC11F    CLC   KEY+1(2),=C'SG'                                                  
         BNE   AC11G               VAT ACCOUNTS ONLY.                           
         MVC   0(26,R2),SPACES                                                  
         MVC   0(6,R2),=C'VATTYP'                                               
         MVI   10(R2),C'I'         X'02' = INPUT.                               
         TM    ACSTSTAT,X'02'                                                   
         BO    *+8                                                              
         MVI   10(R2),C'O'         BIT OFF = OUTPUT.                            
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
*&&                                                                             
         SPACE 1                                                                
AC11G    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTAT,X'01'                                                   
         BZ    AC12                                                             
         MVC   0(6,R2),=C'ACTUAL'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC12     MVC   0(26,R2),SPACES                                                  
         CLI   ACSTCOST,C' '                                                    
         BE    AC13A                                                            
         CLI   ACSTCOST,0                                                       
         BE    AC13A                                                            
         MVC   0(8,R2),=C'ANALYSIS'                                             
         MVC   10(1,R2),ACSTCOST                                                
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC13A    MVC   0(26,R2),SPACES                                                  
         OC    ACSTCNTR,SPACES                                                  
         CLC   ACSTCNTR,SPACES                                                  
         BE    AC13B                                                            
         MVC   0(2,R2),=C'CC'                                                   
         CLI   ACSTCPOS,0          IS THERE A START POINT                       
         BE    *+14                                                             
         MVC   2(1,R2),ACSTCPOS    YES. GET IT                                  
         OI    2(R2),X'F0'         CHANGE IT FROM BIN. TO CHAR. FORM            
         MVC   10(3,R2),ACSTCNTR                                                
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
AC13B    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTX,X'80'                                                    
         BZ    AC13C                                                            
         MVC   0(6,R2),=C'VEND29'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC13C    MVC   0(26,R2),SPACES                                                  
         CLC   KEY+1(2),=C'SC'                                                  
         BNE   AC13E                                                            
         TM    ACSTSTX,X'40'                                                    
         BZ    AC13D                                                            
         MVC   0(5,R2),=C'RECCR'                                                
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC13D    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTX,X'20'                                                    
         BZ    AC13E                                                            
         MVC   0(5,R2),=C'RECDR'                                                
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC13E    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTX,X'10'                                                    
         BZ    AC13F                                                            
         MVC   0(2,R2),=C'PC'                                                   
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC13F    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTX,X'08'                                                    
         BZ    AC13G                                                            
         MVC   0(3,R2),=C'IND'                                                  
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC13G    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTX,X'04'                                                    
         BZ    AC13H                                                            
         MVC   0(3,R2),=C'P/L'                                                  
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC13H    MVC   0(26,R2),SPACES                                                  
         TM    ACSTSTX,X'02'                                                    
         BZ    AC13I                                                            
         MVC   0(3,R2),=C'BAL'                                                  
         MVI   10(R2),C'Y'                                                      
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC13I    MVC   0(26,R2),SPACES                                                  
         CLI   ACSTLEN,ACSTLNQ1     IS THIS NEW LENGTH?                         
         BNH   AC13Z                NO                                          
         TM    ACSTSTA2,X'20'       IS PAYMENT ON HOLD?                         
         BZ    AC13Z                NO                                          
         MVC   0(3,R2),=C'PAY'                                                  
         MVI   10(R2),C'N'                                                      
         LA    R2,26(R2)            DISPLAY PAY=N                               
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC13Z    L     R4,AIO                                                           
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
         SPACE 1                                                                
AC14     CLI   0(R4),0                                                          
         BE    AC19                                                             
         CLI   0(R4),X'36'         LOOK FOR VAT OR                              
         BE    AC16                                                             
         CLI   0(R4),X'38'         DISCOUNT ELEMENTS                            
         BE    AC17                                                             
         CLI   0(R4),X'23'         OR NUMBER                                    
         BE    AC18A                                                            
         CLI   0(R4),X'64'         OR HOMES                                     
         BE    AC18C                                                            
         CLI   0(R4),X'15'         OR GENERAL LEDGER                            
         BE    AC18D                                                            
         CLI   0(R4),X'3E'         OR COMMENTS                                  
         BE    AC18E                                                            
         CLI   0(R4),X'25'         OR EXTRA NUMBER                              
         BE    AC18H                                                            
         CLI   0(R4),X'27'         OR ACCT BILLING INFO                         
         BE    AC18I                                                            
         CLI   0(R4),X'39'         OR COKE EXPENDITURE                          
         BE    AC18J                                                            
         CLI   0(R4),X'A2'         OR MARKET NUMBER                             
         BE    AC18K                                                            
         CLI   0(R4),X'DB'         FREE FORM TEXT EL                            
         BNE   AC15                NEXT EL                                      
         CLI   2(R4),FFTTVEND      OR VENDOR NUMBER                             
         BE    AC18L                                                            
         CLI   2(R4),FFTTGLNO      OR GENERAL LEDGER NUMBER                     
         BE    AC18M                                                            
         CLI   2(R4),FFTTCDPT      OR COKE DEPARTMENT NUMBER                    
         BE    AC18N                                                            
         SPACE 1                                                                
AC15     ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     AC14                                                             
         SPACE 1                                                                
AC16     MVC   0(26,R2),SPACES                                                  
         MVC   0(3,R2),=C'VAT'                                                  
         MVC   10(4,R2),=C'ZERO'                                                
         USING ACVATD,R4                                                        
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         OC    ACVTRATE,ACVTRATE                                                
         BZ    AC15                                                             
         SH    R2,=H'14'                                                        
         B     AC18                                                             
         SPACE 1                                                                
AC17     MVC   0(26,R2),SPACES                                                  
         MVC   0(8,R2),=C'DISCOUNT'                                             
         LA    R2,10(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC18     EDIT  (2,ACVTRATE),(5,(R2)),2,ALIGN=LEFT                               
         LA    R2,16(R2)                                                        
         B     AC15                                                             
         SPACE 1                                                                
AC18A    MVC   0(26,R2),SPACES                                                  
         USING ACOTHERD,R4                                                      
         CLC   ACOTNUM,SPACES                                                   
         BE    AC18B                                                            
         CLI   ACOTPROF,C'I'                                                    
         BNE   *+14                                                             
         MVC   0(2,R2),=C'ID'                                                   
         B     AC18A2                                                           
         MVC   0(6,R2),=C'NUMBER'                                               
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   *+10                                                             
         MVC   0(7,R2),=C'BENEFIT'                                              
         CLC   KEY+1(2),=C'3M'                                                  
         BNE   AC18A2                                                           
         MVC   0(7,R2),=C'ACN    '                                              
*MN                                                                             
         CLC   ACOTDATE,SPACES                                                  
         BE    AC18A2                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(L'ACOTDATE),ACOTDATE                                        
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,WORK),(6,4(R2))                                   
         MVI   3(R2),C'-'                                                       
*MN                                                                             
AC18A2   MVC   10(9,R2),ACOTNUM                                                 
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
AC18B    DS    0H                                                               
*        MVC   0(26,R2),SPACES                                                  
*        CLC   ACOTPROF,SPACES                                                  
*        BE    AC15                                                             
*        CLC   ACOTPROF,=C'I   '   MUST BE ID                                   
*        BE    AC15                                                             
*        MVC   0(7,R2),=C'PROFILE'                                              
*        MVC   10(4,R2),ACOTPROF                                                
*        LA    R2,26(R2)                                                        
*        LA    R5,1(R5)                                                         
         B     AC15                                                             
         SPACE 1                                                                
         USING ACHOMED,R4                                                       
AC18C    MVC   0(26,R2),SPACES                                                  
         MVC   0(5,R2),=C'HOMES'                                                
         LA    R2,10(R2)                                                        
         EDIT  ACHMNO,(8,(R2)),ALIGN=LEFT                                       
         LA    R2,16(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
         SPACE 1                                                                
         USING ACGENLD,R4                                                       
AC18D    MVC   0(26,R2),SPACES                                                  
         CLI   KEY+1,C'F'            FOR UNIT F DON'T DISPLAY                   
         BE    AC15                15 ELEMENT                                   
         MVC   0(7,R2),=C'GENERAL'                                              
         MVC   10(10,R2),ACGLACC                                                
         CLI   ACGLLEN,26                                                       
         BL    *+10                                                             
         MVC   10(14,R2),ACGLACC                                                
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
         SPACE 1                                                                
         USING ACOMMD,R4                                                        
AC18E    CLI   ACOMTYPE,0          ONLY SHOW STANDARD COMMENTS                  
         BE    AC15                                                             
         MVC   0(26,R2),SPACES                                                  
         MVC   0(3,R2),=C'EST'                                                  
         TM    ACOMTYPE,X'40'                                                   
         BO    *+10                                                             
         MVC   0(3,R2),=C'BIL'                                                  
         TM    ACOMTYPE,X'C0'                                                   
         BM    *+10                                                             
         MVC   0(3,R2),=C'B+E'                                                  
         SPACE 1                                                                
         MVC   WORK(6),ACOMMENT                                                 
         LA    RF,5                                                             
AC18F    CLI   WORK,C' '                                                        
         BNE   AC18G                                                            
         MVC   WORK(5),WORK+1                                                   
         MVI   WORK+5,C' '                                                      
         BCTR  RF,0                                                             
         B     AC18F                                                            
AC18G    EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R2),WORK                                                    
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
         SPACE 1                                                                
         USING ACNOD,R4                                                         
AC18H    CLC   KEY+1(2),=C'3M'                                                  
         BE    AC18HV                                                           
         MVC   0(26,R2),SPACES                                                  
         MVC   0(4,R2),=C'NUM2'                                                 
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   *+10                                                             
         MVC   0(5,R2),=C'ADMIN'                                                
         ZIC   RF,ACNOLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R2),ACNO                                                    
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
         SPACE 1                                                                
AC18HV   MVC   0(26,R2),SPACES                                                  
         MVC   0(7,R2),=C'VEHICLE'                                              
         MVC   10(5,R2),ACNO+3                                                  
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
         SPACE 1                                                                
         USING ACABILLD,R4                                                      
AC18I    MVC   0(26,R2),SPACES                                                  
         CLC   ACABEANO,SPACES     IS THERE A BEANO                             
         BE    AC18I1                                                           
         MVC   0(2,R2),=C'EA'                                                   
         MVC   10(14,R2),ACABEANO                                               
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
AC18I1   MVC   0(26,R2),SPACES                                                  
         CLC   ACABACNO,SPACES     IS THERE AN ACNO                             
         BE    AC18I2                                                           
         MVC   0(2,R2),=C'AC'                                                   
         MVC   10(14,R2),ACABACNO                                               
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
AC18I2   MVC   0(26,R2),SPACES                                                  
         CLI   ACABLEN,X'39'                                                    
         BL    AC15                                                             
         CLC   ACABESNO,SPACES                                                  
         BE    AC18I3                                                           
         MVC   0(3,R2),=C'ENO'                                                  
         MVC   10(12,R2),ACABESNO                                               
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
AC18I3   MVC   0(26,R2),SPACES                                                  
         CLC   ACABBUNO,SPACES                                                  
         BE    AC18I4                                                           
         MVC   0(3,R2),=C'BUD'                                                  
         MVC   10(15,R2),ACABBUNO                                               
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
AC18I4   CLI   ACABLEN,X'39'                                                    
         BNH   AC15                                                             
         MVC   0(26,R2),SPACES                                                  
         CLC   ACABBINO,SPACES                                                  
         BE    AC18I5                                                           
         MVC   0(2,R2),=C'BN'                                                   
         MVC   10(15,R2),ACABBINO                                               
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
AC18I5   CLI   ACABLEN,X'48'                                                    
         BNH   AC15                                                             
         MVC   0(26,R2),SPACES                                                  
         CLC   ACABBMEM,SPACES                                                  
         BE    AC15                                                             
         MVC   0(2,R2),=C'BG'                                                   
         MVC   10(15,R2),ACABBMEM                                               
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
         EJECT                                                                  
*              AREA ELEMENT FOR COKE EXPENDITURE                                
         USING ACEXPD,R4                                                        
AC18J    MVC   0(26,R2),SPACES                                                  
         MVC   0(4,R2),=C'AREA'                                                 
         MVC   10(5,R2),ACEXPACC+3                                              
         OC    ACEXPDTE,ACEXPDTE                                                
         BZ    AC18J3                                                           
         LA    R3,10(R2)                                                        
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   0(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(1,ACEXPDTE),(8,1(R3))                               
AC18J3   LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
         SPACE 1                                                                
         USING ACUFD,R4                                                         
AC18K    MVC   0(26,R2),SPACES                                                  
         MVC   0(3,R2),=C'MKT'                                                  
         MVC   10(4,R2),ACUFDATA                                                
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
         SPACE 1                                                                
         USING FFTELD,R4                                                        
AC18L    MVC   0(26,R2),SPACES                                                  
         MVC   0(6,R2),=C'VENDOR'                                               
         MVC   10(10,R2),FFTDATA                                                
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
AC18M    MVC   0(26,R2),SPACES                                                  
         MVC   0(2,R2),=C'GL'                                                   
         MVC   10(4,R2),FFTDATA                                                 
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
AC18N    MVC   0(26,R2),SPACES                                                  
         MVC   0(4,R2),=C'DEPT'                                                 
         MVC   10(4,R2),FFTDATA                                                 
         LA    R2,26(R2)                                                        
         LA    R5,1(R5)                                                         
         B     AC15                                                             
         SPACE 1                                                                
AC19     LTR   R5,R5                                                            
         BZ    XIT                                                              
         GOTO1 UNSCAN,DMCB,((R5),BLOCK),(16,LOGPROFH),0                         
         CLI   DMCB,0                                                           
         BE    XIT                                                              
         GOTO1 (RF),(R1),,(16,LOGPRO2H)                                         
         CLI   DMCB,0                                                           
         BE    XIT                                                              
         GOTO1 (RF),(R1),,(16,LOGPRO3H)                                         
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         BAS   RE,LDGET            GET LEDGER ELEMENT                           
         L     R4,AIO                                                           
         LA    R2,LOGANAMH                                                      
         GOTO1 ANY                                                              
         GOTO1 NAMIN,DMCB,AIO,(R2)                                              
         LA    R2,LOGADD1H                                                      
         GOTO1 ADDRIN,DMCB,AIO,(R2)                                             
         SPACE 1                                                                
         GOTO1 STATIN,DMCB,AIO           ADD/REFRESH STATUS ELEMENT             
         GOTO1 GETL,DMCB,(X'30',AIO),0   GET ADDRESS OF STATUS                  
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
         L     R4,ELADDR           NOW UPDATE IT                                
         SPACE 1                                                                
         USING ACSTATD,R4                                                       
         CLI   LOGFILTH+5,0                                                     
         BE    *+10                                                             
         MVC   ACSTFILT(1),LOGFILT                                              
         CLI   LOGFLT2H+5,0                                                     
         BE    *+10                                                             
         MVC   ACSTFILT+1(1),LOGFLT2                                            
*&&US                                                                           
         CLC   KEY+1(2),=C'SP'     CAN NOT CHANGE ANALYSIS FILTER               
         BE    AC20A               FOR SPOT AND PRINT PAYEES                    
         CLC   KEY+1(2),=C'SS'                                                  
         BE    AC20A                                                            
         CLC   KEY+1(2),=C'ST'                                                  
         BE    AC20A                                                            
         CLC   KEY+1(2),=C'SQ'                                                  
         BE    AC20A                                                            
*&&                                                                             
*&&UK                                                                           
         CLC   KEY+1(2),=C'SF'     OR UK MEDLINE PAYEES                         
         BE    AC20A                                                            
*&&                                                                             
         SPACE 1                                                                
         LA    R2,LOGANALH                                                      
         LA    R3,COMPEL                                                        
         USING ACCOMPD,R3                                                       
         CLC   KEY+1(2),ACMPJOB                                                 
         BNE   AC20AA                                                           
         CLI   LOGANALH+5,0                                                     
         BE    AC20AA                                                           
         B     FINV                                                             
AC20AA   DS    0H                                                               
         SPACE 1                                                                
         MVC   ACSTANAL,SPACES                                                  
         CLI   LOGANALH+5,0                                                     
         BE    *+10                                                             
         MVC   ACSTANAL,LOGANAL                                                 
         SPACE 1                                                                
AC20A    MVC   ACSTSUB,SPACES                                                   
         CLI   LOGSUBH+5,0                                                      
         BE    *+10                                                             
         MVC   ACSTSUB,LOGSUB                                                   
         XC    ACSTSECY,ACSTSECY                                                
         LA    R2,LOGSECH                                                       
         CLI   5(R2),0                                                          
         BE    AC20C                                                            
         GOTO1 NUMERIC                                                          
         GOTO1 PACK                                                             
         STH   R1,ACSTSECY                                                      
         CH    R1,=H'256'                                                       
         BL    AC20C                                                            
         B     ERR62                                                            
         SPACE 1                                                                
AC20C    L     R2,AIO                                                           
         AH    R2,DATADISP                                                      
         USING FFTELD,R2                                                        
AC20C1   CLI   0(R2),0                                                          
         BE    AC20C3                                                           
         CLI   FFTEL,FFTELQ                                                     
         BNE   AC20C2                                                           
         CLI   FFTTYPE,FFTTVEND                                                 
         BE    AC20C2D                                                          
         CLI   FFTTYPE,FFTTCDPT                                                 
         BE    AC20C2D                                                          
         CLI   FFTTYPE,FFTTGLNO                                                 
         BNE   AC20C2                                                           
AC20C2D  MVI   0(R2),X'FF'                                                      
AC20C2   ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     AC20C1                                                           
AC20C3   GOTO1 DELL,DMCB,(X'FF',AIO),0                                          
         LA    R3,BLOCK                                                         
         ST    R3,ADBLOCK                                                       
         XC    TOTBLK,TOTBLK                                                    
         LA    R2,LOGPROFH                                                      
         BAS   RE,SCAN                                                          
         LA    R2,LOGPRO2H                                                      
         BAS   RE,SCAN                                                          
         LA    R2,LOGPRO3H                                                      
         BAS   RE,SCAN                                                          
         SPACE 1                                                                
         LA    R2,LOGPROFH                                                      
         LH    R5,TOTBLK                                                        
         LTR   R5,R5                                                            
         BZ    AC23                                                             
         LA    R3,BLOCK                                                         
         B     AC21                                                             
         SPACE 1                                                                
SCAN     NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         L     R3,ADBLOCK                                                       
         GOTO1 SCANNER,DMCB,(16,(R2)),(8,(R3)),0                                
         CLI   4(R1),0                                                          
         BE    FINV                INVALID INPUT FIELD                          
         ZIC   R3,4(R1)            NUMBER ON THIS LINE                          
         AH    R3,TOTBLK                                                        
         STH   R3,TOTBLK           SAVE TOTAL NUMBER                            
         MH    R3,=H'38'           NUMBER X 38                                  
         LA    R3,BLOCK(R3)        TO NEXT BLOCK AREA                           
         ST    R3,ADBLOCK          NEXT BLOCK AREA                              
         B     XIT                                                              
         SPACE 1                                                                
AC21     CLI   1(R3),0                                                          
         BE    FINV                                                             
         CLC   12(8,R3),=C'ANALYSIS'                                            
         BNE   AC21A                                                            
         MVI   ACSTCOST,C' '                                                    
         CLC   22(2,R3),=C'NO'                                                  
         BE    AC22                                                             
         CLI   22(R3),C'('                                                      
         BE    AC211                                                            
         CLI   22(R3),C')'                                                      
         BE    AC211                                                            
         CLI   22(R3),C'A'                                                      
         BL    FINV                                                             
         CLI   22(R3),C'9'                                                      
         BH    FINV                                                             
AC211    MVC   ACSTCOST,22(R3)                                                  
         B     AC22                                                             
         SPACE 1                                                                
AC21A    CLC   12(6,R3),=C'LOCKED'                                              
         BNE   AC21B                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTAT,X'DF'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTAT,X'20'                                                   
         B     AC22                                                             
         SPACE 1                                                                
AC21B    CLC   12(6,R3),=C'CLOSED'                                              
         BNE   AC21C                                                            
         CLI   22(R3),C'N'                                                      
         BNE   FINV                                                             
         NI    ACSTSTAT,X'BF'                                                   
         B     AC22                                                             
         SPACE 1                                                                
AC21C    CLC   12(5,R3),=C'STAFF'                                               
         BNE   AC21D                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTAT,X'7F'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTAT,X'80'                                                   
         B     AC22                                                             
         SPACE 1                                                                
*C21D    CLC   12(4,R3),=C'DEPT'                                                
*        BNE   AC21E                                                            
*        CLI   22(R3),C'N'                                                      
*        BNE   *+12                                                             
*        NI    ACSTSTAT,X'EF'                                                   
*        B     AC22                                                             
*        CLI   22(R3),C'Y'                                                      
*        BNE   FINV                                                             
*        OI    ACSTSTAT,X'10'                                                   
*        B     AC22                                                             
*        SPACE 1                                                                
AC21D    CLC   12(7,R3),=C'OUTFILE'                                             
         BNE   AC21F                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTAT,X'F7'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTAT,X'08'                                                   
         B     AC22                                                             
         SPACE 1                                                                
AC21F    DS    0H                                                               
*&&US*&& CLC   12(4,R3),=C'EXEC'                                                
*&&UK*&& CLC   12(7,R3),=C'PLUSVAT'                                             
         BNE   AC21G                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTAT,X'FB'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTAT,X'04'                                                   
         B     AC22                                                             
         SPACE 1                                                                
*&&US                                                                           
AC21G    CLC   12(6,R3),=C'VEND2C'                                              
         BNE   AC21H                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTAT,X'FD'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTAT,X'02'                                                   
         B     AC22                                                             
*&&                                                                             
*&&UK                                                                           
AC21G    CLC   KEY+1(2),=C'SG'                                                  
         BNE   AC21H               VAT ACCOUNTS ONLY.                           
         CLC   12(6,R3),=C'VATTYP'                                              
         BNE   AC21H                                                            
         NI    ACSTSTAT,X'FD'      SET TYPE TO OUTPUT.                          
         CLI   22(R3),C'O'                                                      
         BE    AC22                THAT'S RIGHT.                                
         CLI   22(R3),C'I'                                                      
         BNE   FINV                BAD ENTRY.                                   
         OI    ACSTSTAT,X'02'      IT'S INPUT.                                  
         B     AC22                                                             
*&&                                                                             
         SPACE 1                                                                
AC21H    CLC   12(6,R3),=C'ACTUAL'                                              
         BNE   AC21I                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTAT,X'FE'                                                   
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTAT,X'01'                                                   
         B     AC22                                                             
         SPACE 1                                                                
AC21I    CLC   12(2,R3),=C'CC'                                                  
         BNE   AC21I2                                                           
         CLI   0(R3),3             MAX 3 CHARS. 'CC' + DIGIT                    
         BH    FINV                                                             
         MVI   ACSTCPOS,0                                                       
         CLC   22(6,R3),=C'DELETE'                                              
         BNE   *+14                                                             
         MVC   ACSTCNTR,SPACES                                                  
         B     AC22                                                             
         CLC   KEY+1(2),=C'SJ'     OPTION INVALID FOR SJ                        
         BE    FINV                                                             
         CLI   14(R3),X'40'                                                     
         BE    *+30                                                             
         CLI   14(R3),C'1'         MUST BE 1-9                                  
         BL    FINV                                                             
         CLI   14(R3),C'9'                                                      
         BH    FINV                                                             
         MVC   ACSTCPOS,14(R3)                                                  
         NI    ACSTCPOS,X'0F'      MAKE IT BINARY                               
         CLI   1(R3),3             MAX 3 CHARS TO BE CHANGED                    
         BH    FINV                                                             
         MVC   ACSTCNTR,22(R3)                                                  
         B     AC22                                                             
         SPACE 1                                                                
*       - - - - - - - - - - - - - -                                             
AC21I2   CLC   KEY+1(2),=C'1R'     DEFAULT TASK CODE FOR TMS                    
         BNE   AC21J                                                            
         CLC   12(4,R3),=C'TASK'                                                
         BNE   AC21J                                                            
         MVI   ACSTLEN,ACSTLNQ3                                                 
         MVC   ACSTDTSK,SPACES                                                  
         CLC   22(3,R3),=C'DEL'                                                 
         BE    AC22                                                             
         CLI   1(R3),2             MUST BE 2 CHARS                              
         BNE   XIT                                                              
         CLI   22(R3),C' '         MUST BE 2 CHARS                              
         BE    XIT                                                              
         MVC   ACSTDTSK,22(R3)                                                  
         B     AC22                                                             
*       - - - - - - - - - - - - - -                                             
AC21J    CLC   12(6,R3),=C'VEND29'                                              
         BNE   AC21K                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTX,X'7F'                                                    
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTX,X'80'                                                    
         B     AC22                                                             
         SPACE 1                                                                
AC21K    CLC   KEY+1(2),=C'SC'                                                  
         BNE   AC21M                                                            
         CLC   12(5,R3),=C'RECCR'                                               
         BNE   AC21L                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTX,X'BF'                                                    
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTX,X'40'                                                    
         B     AC22                                                             
         SPACE 1                                                                
AC21L    CLC   12(5,R3),=C'RECDR'                                               
         BNE   AC21M                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTX,X'DF'                                                    
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTX,X'20'                                                    
         B     AC22                                                             
         SPACE 1                                                                
AC21M    CLC   12(2,R3),=C'PC'                                                  
         BNE   AC21N                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTX,X'EF'                                                    
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTX,X'10'                                                    
         B     AC22                                                             
         SPACE 1                                                                
AC21N    CLC   12(3,R3),=C'IND'                                                 
         BNE   AC21P                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTX,X'F7'                                                    
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTX,X'08'                                                    
         B     AC22                                                             
         SPACE 1                                                                
AC21P    CLC   12(3,R3),=C'P/L'                                                 
         BNE   AC21R                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTX,X'FB'                                                    
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTX,X'04'                                                    
         B     AC22                                                             
         SPACE 1                                                                
AC21R    CLC   12(3,R3),=C'BAL'                                                 
         BNE   AC21S                                                            
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTX,X'FD'                                                    
         B     AC22                                                             
         CLI   22(R3),C'Y'                                                      
         BNE   FINV                                                             
         OI    ACSTSTX,X'02'                                                    
         B     AC22                                                             
         SPACE 1                                                                
AC21S    CLC   12(6,R3),=C'VENDOR'                                              
         BNE   AC21T                                                            
         CLI   1(R3),0                                                          
         BE    FINV                                                             
         CLI   1(R3),10                                                         
         BNE   FINV                                                             
         L     R2,AIO                                                           
         CLC   1(2,R2),=C'SX'                                                   
         BNE   FINV                                                             
         XC    DBELEM,DBELEM                                                    
         LA    R2,DBELEM                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,15                                                         
         MVI   FFTTYPE,FFTTVEND                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,10                                                       
         MVC   FFTDATA(10),22(R3)                                               
         GOTO1 ADDL,DMCB,AIO,DBELEM                                             
         LA    R2,LOGPROFH                                                      
         B     AC22                                                             
         SPACE 1                                                                
AC21T    CLC   12(2,R3),=C'GL'     GENERAL LEDGER PRODUCT NUMBER                
         BNE   AC21V                                                            
         CLI   1(R3),0                                                          
         BE    ERRGLREQ                                                         
         CLI   1(R3),4             LENGTH OF 4                                  
         BNE   ERRGLREQ                                                         
         TM    3(R3),X'80'         VALID NUMERIC                                
         BNO   ERRGLREQ                                                         
         L     R2,AIO                                                           
         CLC   1(2,R2),=C'3P'      ONLY ON 3P                                   
         LA    R2,LOGPROFH         SET CURSOR                                   
         BNE   FINV                                                             
         XC    DBELEM,DBELEM                                                    
         LA    R2,DBELEM                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,9                                                          
         MVI   FFTTYPE,FFTTGLNO                                                 
         MVI   FFTDLEN,4                                                        
         MVC   FFTDATA(4),22(R3)                                                
         GOTO1 ADDL,DMCB,AIO,DBELEM                                             
         OI    GLSTAT,GLSYES                                                    
         LA    R2,LOGPROFH                                                      
         B     AC22                                                             
         SPACE 1                                                                
AC21V    CLC   12(4,R3),=C'DEPT'   DEPARTMENT NUMBER                            
         BNE   AC21W                                                            
         CLI   1(R3),0                                                          
         BE    FINV                                                             
         CLI   1(R3),4             LENGTH OF 4                                  
         BNE   FINV                                                             
         TM    3(R3),X'80'         VALID NUMERIC                                
         BNO   FINV                                                             
         L     R2,AIO                                                           
         CLC   1(2,R2),=C'3P'      ONLY ON 3P                                   
         LA    R2,LOGPROFH         SET CURSOR                                   
         BNE   FINV                                                             
         XC    DBELEM,DBELEM                                                    
         LA    R2,DBELEM                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,9                                                          
         MVI   FFTTYPE,FFTTCDPT                                                 
         MVI   FFTDLEN,4                                                        
         MVC   FFTDATA(4),22(R3)                                                
         GOTO1 ADDL,DMCB,AIO,DBELEM                                             
         LA    R2,LOGPROFH                                                      
         B     AC22                                                             
         SPACE 1                                                                
AC21W    CLC   12(3,R3),=C'PAY'      IS PAYMENT ON HOLD?                        
         BNE   AC22                                                             
         CLI   22(R3),C'Y'           TURN BIT OFF?                              
         BNE   *+12                                                             
         NI    ACSTSTA2,X'FF'-X'20'  YES                                        
         B     AC22                                                             
         CLI   22(R3),C'N'           TURN BIT ON?                               
         BNE   XIT                                                              
         OI    ACSTSTA2,X'20'        YES                                        
         B     AC22                                                             
         SPACE 1                                                                
AC22     LA    R3,38(R3)                                                        
         BCT   R5,AC21                                                          
         EJECT                                                                  
         LH    R5,TOTBLK                                                        
         LA    R3,BLOCK                                                         
         SPACE 1                                                                
         LA    R4,EXPELM                                                        
         USING ACVATD,R4                                                        
         MVI   ACVTLEN,X'04'                                                    
         SPACE 1                                                                
AC22A    CLC   12(4,R3),=CL4'VAT'                                               
         BNE   AC22B                                                            
         MVI   ACVTEL,X'36'                                                     
         B     AC22C                                                            
         SPACE 1                                                                
AC22B    CLC   12(8,R3),=C'DISCOUNT'                                            
         BNE   AC22D                                                            
         MVI   ACVTEL,X'38'                                                     
         SPACE 1                                                                
AC22C    GOTO1 DELL,DMCB,(ACVTEL,AIO),0                                         
         CLI   22(R3),C'N'                                                      
         BE    AC22D                                                            
         XC    ACVTRATE,ACVTRATE                                                
         CLC   22(4,R3),=C'ZERO'                                                
         BE    AC22CA                                                           
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,22(R3)                                              
         CLI   DMCB,X'FF'                                                       
         BE    XIT                                                              
         MVC   ACVTRATE,DMCB+6                                                  
AC22CA   GOTO1 ADDL,DMCB,AIO,EXPELM                                             
         SPACE 1                                                                
AC22D    LA    R3,38(R3)                                                        
         BCT   R5,AC22A                                                         
         EJECT                                                                  
*              NOW SCAN FOR NUMBER AND PROFILE                                  
         LH    R5,TOTBLK                                                        
         LA    R3,BLOCK                                                         
         LA    R4,EXPELM                                                        
         USING ACOTHERD,R4                                                      
         MVC   ACOTEL(2),=X'230F'                                               
         MVC   ACOTNUM(13),SPACES                                               
*MN      GOTO1 GETL,DMCB,(X'23',AIO),0                                          
*MN      CLI   ELERR,0                                                          
*MN      BNE   AC22E                                                            
                                                                                
         XC    DEL23EL,DEL23EL                                                  
         L     R4,AIO                                                           
         AH    R4,DATADISP                                                      
AC22DA   CLI   0(R4),0                                                          
         BE    AC22E                                                            
         CLI   0(R4),X'23'                                                      
         BE    AC22DC                                                           
AC22DB   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     AC22DA                                                           
AC22DC   CLC   ACOTDATE,SPACES                                                  
         BNE   AC22DB                                                           
         MVC   EXPELM(15),0(R4)                                                 
         ST    R4,DEL23EL                                                       
                                                                                
AC22E    LA    R4,EXPELM                                                        
         CLC   12(6,R3),=C'NUMBER'                                              
         BE    AC22EA                                                           
         CLC   12(2,R3),=C'ID'                                                  
         BNE   *+12                                                             
         MVI   ACOTPROF,C'I'                                                    
         B     AC22EA                                                           
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   AC22E0                                                           
         CLC   12(7,R3),=C'BENEFIT'                                             
         BNE   AC22E0                                                           
         B     AC22EA                                                           
AC22E0   CLC   KEY+1(2),=C'3M'                                                  
         BNE   AC22F                                                            
         CLC   12(7,R3),=C'ACN    '                                             
*MN      CLC   12(3,R3),=C'ACN'                                                 
         BNE   AC22F                                                            
         CLC   22(2,R3),=C'NO'                                                  
         BNE   AC22E01                                                          
                                                                                
         OC    DEL23EL,DEL23EL                                                  
         BZ    AC22H                                                            
         L     R2,DEL23EL                                                       
         MVI   0(R2),X'FF'                                                      
         GOTO1 DELL,DMCB,(X'FF',AIO),0  DELETE ACN NUMBER                       
         B     AC22H                                                            
                                                                                
AC22E01  DS    0H                                                               
         CLI   1(R3),5                                                          
         BNE   FINV                ACN MUST BE 5                                
         MVC   HOLDKEY,KEY                                                      
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SE'                                                  
         MVC   KEY+3(5),22(R3)     ACN MUST BE ON SE LEDGER                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   KEY(L'HOLDKEY),HOLDKEY                                           
         MVC   AIO,AIO1                                                         
AC22EA   MVC   ACOTNUM,SPACES                                                   
         CLI   1(R3),1                                                          
         BH    *+12                                                             
         CLI   22(R3),C'N'                                                      
         BE    AC22G                                                            
         MVC   ACOTNUM,22(R3)                                                   
AC22F    DS    0H                                                               
*MN      CLC   12(7,R3),=C'PROFILE'                                             
*MN      BNE   AC22G                                                            
*MN      MVC   ACOTPROF,SPACES                                                  
*MN      CLI   1(R3),1                                                          
*MN      BH    *+12                                                             
*MN      CLI   22(R3),C'N'                                                      
*MN      BE    AC22G                                                            
*MN      MVC   ACOTPROF,22(R3)                                                  
AC22G    LA    R3,38(R3)                                                        
         BCT   R5,AC22E                                                         
                                                                                
         OC    DEL23EL,DEL23EL                                                  
         BZ    AC22G1                                                           
         L     R2,DEL23EL                                                       
         MVI   0(R2),X'FF'                                                      
         GOTO1 DELL,DMCB,(X'FF',AIO),0                                          
AC22G1   CLC   ACOTNUM(13),SPACES                                               
         BE    AC22H                                                            
         GOTO1 ADDL,DMCB,AIO,EXPELM                                             
         EJECT                                                                  
                                                                                
*              NOW SCAN FOR NUMBER AND PROFILE                                  
AC22H    DS    0H                                                               
         LH    R5,TOTBLK                                                        
         LA    R3,BLOCK                                                         
         LA    R4,EXPELM                                                        
         USING ACOTHERD,R4                                                      
         MVC   ACOTEL(2),=X'230F'                                               
         MVC   ACOTNUM(13),SPACES                                               
                                                                                
         XC    DEL23EL,DEL23EL                                                  
         L     R4,AIO                                                           
         AH    R4,DATADISP                                                      
AC22HA   CLI   0(R4),0                                                          
         BE    AC22HE                                                           
         CLI   0(R4),X'23'                                                      
         BE    AC22HC                                                           
AC22HB   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     AC22HA                                                           
AC22HC   CLC   ACOTDATE,SPACES                                                  
         BE    AC22HB                                                           
         MVC   EXPELM(15),0(R4)                                                 
         ST    R4,DEL23EL                                                       
                                                                                
AC22HE   DS    0H                                                               
         LA    R4,EXPELM                                                        
         CLC   KEY+1(2),=C'3M'                                                  
         BNE   AC22HY                                                           
         CLC   12(4,R3),=C'ACN-'                                                
         BNE   AC22HY                                                           
         CLC   22(2,R3),=C'NO'                                                  
         BNE   AC22HF                                                           
         OC    DEL23EL,DEL23EL                                                  
         BZ    AC22HZ                                                           
         L     R2,DEL23EL                                                       
         MVI   0(R2),X'FF'                                                      
         GOTO1 DELL,DMCB,(X'FF',AIO),0  DELETE ACN NUMBER                       
         B     AC22HZ                                                           
                                                                                
AC22HF   DS    0H                                                               
         CLI   1(R3),5                                                          
         BNE   FINV                ACN MUST BE 5                                
         MVC   HOLDKEY,KEY                                                      
         MVC   AIO,AIO2                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SE'                                                  
         MVC   KEY+3(5),22(R3)     ACN MUST BE ON SE LEDGER                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   KEY(L'HOLDKEY),HOLDKEY                                           
         MVC   AIO,AIO1                                                         
         MVC   ACOTNUM,22(R3)                                                   
         MVC   ACOTDATE,SPACES                                                  
         MVC   WORK(6),16(R3)                                                   
         GOTO1 DATVAL,DMCB,(2,WORK),WORK+6                                      
         OC    DMCB(4),DMCB                                                     
         BZ    FINV                                                             
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,WORK+12)                               
         MVC   ACOTDATE,WORK+12                                                 
         B     AC22HY1                                                          
                                                                                
AC22HY   LA    R3,38(R3)                                                        
         BCT   R5,AC22HE                                                        
                                                                                
AC22HY1  OC    DEL23EL,DEL23EL                                                  
         BZ    AC22HY2                                                          
         L     R2,DEL23EL                                                       
         MVI   0(R2),X'FF'                                                      
         GOTO1 DELL,DMCB,(X'FF',AIO),0                                          
AC22HY2  CLC   ACOTNUM(13),SPACES                                               
         BE    AC22HZ                                                           
         GOTO1 ADDL,DMCB,AIO,EXPELM                                             
         EJECT                                                                  
                                                                                
AC22HZ   DS    0H                                                               
         LH    R5,TOTBLK                                                        
         LA    R3,BLOCK                                                         
         LA    R4,EXPELM                                                        
         USING ACHOMED,R4                                                       
         MVC   ACHMEL(2),=X'6408'                                               
         ZAP   ACHMNO,=P'0'                                                     
         GOTO1 GETL,DMCB,(X'64',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   AC22J                                                            
         L     RF,ELADDR                                                        
         MVC   EXPELM(8),0(RF)                                                  
         SPACE 1                                                                
AC22J    CLC   12(5,R3),=C'HOMES'                                               
         BNE   AC22K                                                            
         ZAP   ACHMNO,=P'0'                                                     
         CLI   1(R3),1                                                          
         BNE   *+12                                                             
         CLI   22(R3),C'N'                                                      
         BE    AC22L                                                            
         TM    3(R3),X'80'         NUMERIC                                      
         BZ    FINV                                                             
         MVC   FULL,8(R3)                                                       
         L     RF,FULL                                                          
         CVD   RF,DUB                                                           
         ZAP   ACHMNO,DUB                                                       
         SPACE 1                                                                
AC22K    LA    R3,38(R3)                                                        
         BCT   R5,AC22J                                                         
         GOTO1 DELL,DMCB,(X'64',AIO),0                                          
         CP    ACHMNO,=P'0'        AMEND TO ZERO DELETES ELEMENT                
         BE    AC22L                                                            
         GOTO1 ADDL,DMCB,AIO,EXPELM                                             
         EJECT                                                                  
AC22L    CLI   KEY+1,C'F'          FOR UNIT F DON'T HANDLE GENERAL              
         BE    AC22P                                                            
         LH    R5,TOTBLK           NOW SCAN FOR GENERAL LEDGER                  
         LA    R3,BLOCK                                                         
         LA    R4,EXPELM                                                        
         USING ACGENLD,R4                                                       
         MVC   ACGLEL(2),=X'151A'                                               
         SPACE 1                                                                
AC22M    CLC   12(7,R3),=C'GENERAL'                                             
         BE    AC22N                                                            
         LA    R3,38(R3)                                                        
         BCT   R5,AC22M                                                         
         B     AC22P                                                            
         SPACE 1                                                                
AC22N    GOTO1 DELL,DMCB,(X'15',AIO),0                                          
         CLC   22(2,R3),=C'NO'                                                  
         BE    AC22P                                                            
         MVC   ACGLSUB,22(R3)                                                   
         MVC   ACGLACC,22(R3)                                                   
         MVC   HOLDKEY,KEY                                                      
         MVC   KEY,SPACES          CHECK A/C EXISTS                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),22(R3)                                                 
         LA    RE,12                                                            
         LA    RF,KEY+3                                                         
AC22NA   CLI   0(RF),C'*'          LOOK FOR AN ASTERISK                         
         BE    AC22NC                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,AC22NA                                                        
         B     AC22NE                                                           
AC22NC   MVI   0(RF),C' '          BLANK IT OUT                                 
         LA    RE,KEY                                                           
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         STC   RF,BYTE                                                          
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         ZIC   RF,BYTE                                                          
         EX    RF,*+8                                                           
         B     *+10                SEE IF WE CAN MATCH ON WHAT IS LEFT          
         CLC   KEY(0),KEYSAVE                                                   
         BNE   ERR17                                                            
         B     AC22NG                                                           
AC22NE   MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 GETL,DMCB,(X'32',AIO2),0                                         
         CLI   ELERR,0                                                          
         BE    AC22NG                                                           
         B     ERR17                                                            
AC22NG   MVC   KEY(L'HOLDKEY),HOLDKEY                                           
         MVC   AIO,AIO1                                                         
         GOTO1 ADDL,DMCB,AIO,EXPELM                                             
         EJECT                                                                  
AC22P    DS    0H                                                               
         LH    R5,TOTBLK           SCAN FOR COMMENTS                            
         LA    R3,BLOCK                                                         
         LA    R4,EXPELM                                                        
         USING ACOMMD,R4                                                        
         MVI   BYTE,1                                                           
         MVC   ACOMEL(2),=X'3E0A'                                               
         SPACE 1                                                                
AC22Q    MVI   ACOMTYPE,X'44'                                                   
         CLC   12(3,R3),=C'EST'                                                 
         BE    AC22S                                                            
         MVI   ACOMTYPE,X'84'                                                   
         CLC   12(3,R3),=C'BIL'                                                 
         BE    AC22S                                                            
         OI    ACOMTYPE,X'40'                                                   
         CLC   12(3,R3),=C'B+E'                                                 
         BE    AC22S                                                            
AC22R    LA    R3,38(R3)                                                        
         BCT   R5,AC22Q                                                         
         B     AC22U                                                            
         SPACE 1                                                                
AC22S    CLI   BYTE,1                                                           
         BNE   AC22T                                                            
         L     R1,AIO                                                           
         AH    R1,DATADISP         REMOVE ALL STANDARD COMMENT ELEMENTS         
AC22SA   CLI   0(R1),0                                                          
         BE    AC22SE                                                           
         CLI   0(R1),X'3E'                                                      
         BNE   AC22SC                                                           
         CLI   3(R1),0                                                          
         BE    AC22SC                                                           
         MVI   0(R1),X'FF'                                                      
AC22SC   ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     AC22SA                                                           
         SPACE 1                                                                
AC22SE   GOTO1 DELL,DMCB,(X'FF',AIO),0                                          
         SPACE 1                                                                
AC22T    MVC   ACOMSEQ,BYTE                                                     
         ZIC   RF,BYTE                                                          
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         CLC   22(6,R3),=CL6'N'  N REMOVES ELEMENT                              
         BE    AC22R                                                            
         MVC   ACOMMENT(6),SPACES                                               
         IC    RF,1(R3)                                                         
         LTR   RF,RF                                                            
         BZ    AC22R                                                            
         LA    RE,6                                                             
         SR    RE,RF                                                            
         LA    RE,ACOMMENT(RE)                                                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),22(R3)                                                   
         SPACE 1                                                                
         MVC   HOLDKEY,KEY                                                      
         XC    KEY,KEY             SEE IF COMMENT EXISTS                        
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(6),ACOMMENT                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   KEY(L'HOLDKEY),HOLDKEY                                           
         MVC   AIO,AIO1                                                         
         GOTO1 ADDL,DMCB,AIO,EXPELM                                             
         B     AC22R                                                            
         EJECT                                                                  
AC22U    DS    0H                                                               
         LH    R5,TOTBLK           SCAN FOR EXTRA NUMBER                        
         LA    R3,BLOCK                                                         
         LA    R4,EXPELM                                                        
AC22UA   CLC   12(4,R3),=C'NUM2'                                                
         BE    AC22V                                                            
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   AC22UX                                                           
         CLC   12(5,R3),=C'ADMIN'                                               
         BE    AC22V                                                            
AC22UX   LA    R3,38(R3)                                                        
         BCT   R5,AC22UA                                                        
         B     AC22UY                                                           
         SPACE 1                                                                
         USING ACNOD,R4                                                         
AC22V    MVI   ACNOEL,X'25'                                                     
         MVC   ACNO,SPACES                                                      
         CLI   1(R3),1                                                          
         BH    *+12                                                             
         CLI   22(R3),C'N'         N REMOVES ELEMENT                            
         BE    AC22W                                                            
         ZIC   RF,1(R3)                                                         
         LA    RF,2(RF)                                                         
         STC   RF,ACNOLEN                                                       
         MVC   ACNO,22(R3)                                                      
         SPACE 1                                                                
AC22W    GOTO1 DELL,DMCB,(X'25',AIO),0                                          
         CLC   ACNO,SPACES                                                      
         BE    AC22UY                                                           
         GOTO1 ADDL,DMCB,AIO,EXPELM                                             
         EJECT                                                                  
*       BUILD 27 ELEMENT IN HOLD AREA                                           
         USING ACABILLD,R4                                                      
AC22UY   LH    R5,TOTBLK                                                        
         LA    R3,BLOCK                                                         
         LA    R4,MYELEM                                                        
         MVI   MYELEM,C' '        FIRST CLEAR 27 EL HOLD AREA                   
         MVC   MYELEM+1(L'MYELEM-1),MYELEM                                      
AC22UY1  CLC   12(2,R3),=C'EA'    EA NUMBER                                     
         BNE   AC22UYA                                                          
         MVC   ACABEANO,SPACES                                                  
         CLI   27(R3),C'-'        CHECK STRUCTURE                               
         BNE   FINV                                                             
         CLI   30(R3),C'-'                                                      
         BNE   FINV                                                             
         MVC   WORK(8),=11X'F0'   AND NUMERICS                                  
         MVZ   WORK(5),22(R3)                                                   
         MVZ   WORK+5(2),28(R3)                                                 
         MVZ   WORK+7(1),31(R3)                                                 
         CLC   WORK(8),=11X'F0'                                                 
         BNE   FINV                                                             
         ZIC   RF,1(R3)                                                         
         CH    RF,=H'10'           FIELD IS 10 POSITIONS CURRENTLY              
         BNE   XIT                                                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACABEANO(0),22(R3)                                               
         B     AC22UYB                                                          
AC22UYA  LA    R3,38(R3)                                                        
         BCT   R5,AC22UY1                                                       
         EJECT                                                                  
AC22UYB  LH    R5,TOTBLK           CHECK FOR ACCT NO                            
         LA    R3,BLOCK                                                         
         LA    R4,MYELEM                                                        
AC22UYB1 CLC   12(3,R3),=C'AC '                                                 
         BNE   AC22UYC                                                          
         MVC   ACABACNO,SPACES                                                  
         ZIC   RF,1(R3)                                                         
         CH    RF,=H'13'                                                        
         BL    TRY3                                                             
         CLI   26(R3),C'-'         STRUCTURE AND NUMERICS                       
         BNE   FINV                                                             
         CLI   31(R3),C'-'         SCHEME 1 (XXXX-XXXX-XXX)                     
         BNE   SCHEME2                                                          
         MVC   WORK(11),=11X'F0'                                                
         MVZ   WORK(4),22(R3)                                                   
         MVZ   WORK+4(4),27(R3)                                                 
         MVZ   WORK+8(3),32(R3)                                                 
         CLC   WORK(11),=11X'F0'                                                
         BNE   FINV                                                             
         B     ACSIZE                                                           
SCHEME2  CLI   30(R3),C'-'         SCHEME 2 (XXXX-XXX-XXXX)                     
         BNE   ACSIZE                                                           
         MVC   WORK(11),=11X'F0'                                                
         MVZ   WORK(4),22(R3)                                                   
         MVZ   WORK+4(3),27(R3)                                                 
         MVZ   WORK+7(4),31(R3)                                                 
         CLC   WORK(11),=11X'F0'                                                
         BNE   FINV                                                             
         B     ACSIZE                                                           
TRY3     CH    RF,=H'4'                                                         
         BNE   FINV                                                             
SCHEME3  MVC   WORK(4),=11X'F0'        SCHEME3 FOUR NUMERICS                    
         MVZ   WORK(4),22(R3)                                                   
         CLC   WORK(4),=11X'F0'                                                 
         BNE   FINV                                                             
ACSIZE   ZIC   RF,1(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACABACNO(0),22(R3)                                               
         B     AC22VA                                                           
AC22UYC  LA    R3,38(R3)                                                        
         BCT   R5,AC22UYB1                                                      
         B     AC22VA                                                           
         EJECT                                                                  
AC22VA   DS    0H                                                               
         LH    R5,TOTBLK                                                        
         LA    R3,BLOCK                                                         
         LA    R4,MYELEM                                                        
AC22VB   CLC   =C'ENO',12(R3)                                                   
         BNE   AC22VB1                                                          
         ZIC   RF,1(R3)                                                         
         CH    RF,=H'12'                                                        
         BH    FINV                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACABESNO(0),22(R3)                                               
         B     AC22VB2                                                          
AC22VB1  LA    R3,38(R3)                                                        
         BCT   R5,AC22VB                                                        
         SPACE 1                                                                
AC22VB2  LH    R5,TOTBLK                                                        
         LA    R3,BLOCK                                                         
         LA    R4,MYELEM                                                        
AC22VB3  CLC   =C'BUD',12(R3)                                                   
         BNE   AC22VB4                                                          
         ZIC   RF,1(R3)                                                         
         CH    RF,=H'15'                                                        
         BNE   FINV                                                             
         BCTR  RF,0                                                             
         EX    RF,INST1                                                         
         EX    RF,INST2                                                         
         EX    RF,INST3                                                         
         B     INST4                                                            
INST1    MVC   WORK(0),=15X'F0'                                                 
INST2    MVZ   WORK(0),22(R3)                                                   
INST3    CLC   WORK(0),=15X'F0'                                                 
INST4    BNE   FINV                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACABBUNO(0),22(R3)                                               
         B     AC22VB5                                                          
AC22VB4  LA    R3,38(R3)                                                        
         BCT   R5,AC22VB3                                                       
AC22VB5  DS    0H                                                               
         LH    R5,TOTBLK                                                        
         LA    R3,BLOCK                                                         
         LA    R4,MYELEM                                                        
AC22WB   CLC   =C'BN',12(R3)                                                    
         BNE   AC22WB1                                                          
         ZIC   RF,1(R3)                                                         
         CH    RF,=H'15'                                                        
         BH    FINV                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACABBINO(0),22(R3)                                               
         B     AC22WB2                                                          
AC22WB1  LA    R3,38(R3)                                                        
         BCT   R5,AC22WB                                                        
         SPACE 1                                                                
AC22WB2  LH    R5,TOTBLK                                                        
         LA    R3,BLOCK                                                         
         LA    R4,MYELEM                                                        
AC22WB3  CLC   =C'BG',12(R3)                                                    
         BNE   AC22WB4                                                          
         ZIC   RF,1(R3)                                                         
         CH    RF,=H'15'                                                        
         BH    FINV                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACABBMEM(0),22(R3)                                               
         B     AC22WB5                                                          
AC22WB4  LA    R3,38(R3)                                                        
         BCT   R5,AC22WB3                                                       
         SPACE 1                                                                
AC22WB5  DS    0H                                                               
         SPACE 1                                                                
         LA    R4,MYELEM           IF THERE IS EA OR AC NOS BUILD AND           
*                                  ADD A 27 EL                                  
         CLC   ACABEANO(76),SPACES                                              
         BE    AC22VA1             NO GET RID OF OLD ONE IF ANY                 
         MVI   ACABEL,X'27'                                                     
         MVI   ACABLEN,X'1E'                                                    
         MVC   EXPELM(L'MYELEM),MYELEM                                          
         LA    R4,EXPELM                                                        
AC22VA1  GOTO1 DELL,DMCB,(X'27',AIO),0                                          
         CLC   ACABEANO(76),SPACES                                              
         BE    AC22X                                                            
         CLC   ACABESNO(48),SPACES                                              
         BE    *+8                                                              
         MVI   ACABLEN,X'39'                                                    
         CLC   ACABBINO,SPACES                                                  
         BE    *+8                                                              
         MVI   ACABLEN,X'48'                                                    
         CLC   ACABBMEM,SPACES                                                  
         BE    ADDELX                                                           
         MVI   ACABLEN,X'57'                                                    
ADDELX   GOTO1 ADDL,DMCB,AIO,EXPELM                                             
         SPACE 1                                                                
AC22X    DC    0H'0'                                                            
         EJECT                                                                  
*              COKE EXPENDITURE                                                 
AC23     GOTO1 GETL,DMCB,(X'39',AIO),0   DELETE 39 ELEMENTS                     
         CLI   ELERR,0                                                          
         BNE   AC23AA              NO MORE 39'S                                 
         GOTO1 DELL,DMCB,(X'39',AIO),0                                          
         B     AC23                                                             
AC23AA   LH    R5,TOTBLK                                                        
         LTR   R5,R5                                                            
         BZ    AC23X                                                            
         LA    R3,BLOCK                                                         
AC23AB   CLC   12(4,R3),=C'AREA'                                                
         BNE   AC23AX                                                           
         LA    R4,EXPELM                                                        
         USING ACEXPD,R4                                                        
         XC    EXPELM,EXPELM                                                    
         MVC   ACEXPEL(2),=X'391D'                                              
         MVC   ACEXPACC,SPACES      SCAN FOR ACC(/DATE)                         
         MVC   ACEXPACC(1),COMPANY                                              
         MVC   ACEXPACC+1(2),=C'3A'                                             
         LA    R1,ACEXPACC+3                                                    
         LA    R0,12                                                            
         LA    RF,22(R3)                                                        
AC23AC   CLI   0(RF),C'-'                                                       
         BE    AC23AD                                                           
         CLI   0(RF),C' '                                                       
         BE    AC23AE                                                           
         MVC   0(1,R1),0(RF)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,AC23AC                                                        
         B     FINV                                                             
AC23AD   GOTO1 DATVAL,DMCB,(0,1(RF)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERR13                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,ACEXPDTE)                                
AC23AE   MVC   HOLDKEY,KEY                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACEXPACC                                                 
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 GETL,DMCB,(X'32',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   ERR18                                                            
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'HOLDKEY),HOLDKEY                                           
         GOTO1 ADDL,DMCB,AIO,EXPELM                                             
AC23AX   LA    R3,38(R3)                                                        
         BCT   R5,AC23AB                                                        
AC23X    DC    0H'0'                                                            
         EJECT                                                                  
*              ADD MARKET NUMBER IF SPECIFIED                                   
AC24     DS    0H                                                               
         GOTO1 GETL,DMCB,(X'A2',AIO),0   DO I ALREADY HAVE AN A2 ELEM           
         CLI   ELERR,0                                                          
         BNE   AC24C                                                            
         GOTO1 DELL,DMCB,(X'A2',AIO),0                                          
AC24C    LH    R5,TOTBLK                                                        
         LTR   R5,R5                                                            
         BZ    AC29                                                             
         LA    R3,BLOCK                                                         
AC24D    CLC   12(3,R3),=C'MKT'                                                 
         BE    AC24E                                                            
         LA    R3,38(R3)                                                        
         BCT   R5,AC24D                                                         
         B     AC29                                                             
AC24E    DS    0H                                                               
         LA    R4,EXPELM                                                        
         USING ACUFD,R4                                                         
         XC    EXPELM,EXPELM                                                    
         MVI   ACUFEL,ACUFELQ                                                   
         MVI   ACUFLEN,36                                                       
         MVI   ACUFSEQ,1                                                        
         MVC   ACUFCODE,=C'MK'                                                  
         MVC   ACUFDESC,=CL12'MARKET'                                           
         MVI   ACUFEDIT,C'C'                                                    
         MVI   ACUFMXLN,4                                                       
         MVI   ACUFSTAT,0                                                       
         MVC   ACUFDATA(4),22(R3)                                               
         GOTO1 ADDL,DMCB,AIO,EXPELM                                             
         EJECT                                                                  
*              ADD VEHICLE NUMBER IF REQUIRED                                   
         SPACE 1                                                                
AC29     LA    R4,LEDGEL                                                        
         USING ACLEDGD,R4                                                       
         TM    ACLTSTAT,X'10'                                                   
         BNO   AC29X               VEHICLE NUMBER NOT REQUIRED                  
         CLI   LOWLEV,C'Y'                                                      
         BNE   AC29X               CAN'T CHANGE HIGH LEVEL NUMBER               
         LH    R5,TOTBLK                                                        
         LTR   R5,R5                                                            
         BZ    AC29VA                                                           
         LA    R3,BLOCK                                                         
AC29A    CLC   12(7,R3),=C'VEHICLE'                                             
         BE    AC29C                                                            
         LA    R3,38(R3)                                                        
         BCT   R5,AC29A                                                         
         B     AC29VA                                                           
AC29C    CLI   1(R3),5                                                          
         BNE   FINV                VEHICLE=NNNNN                                
         OC    8(4,R3),8(R3)                                                    
         BZ    FINV                                                             
         GOTO1 DELL,DMCB,(X'25',AIO),0   DELETE VEHICLE NUMBER                  
         LA    R4,EXPELM                                                        
         USING ACNOD,R4                                                         
         MVC   ACNOEL(2),=X'250A'                                               
         MVC   ACNO(8),=C'00000000'                                             
         MVC   ACNO+3(5),22(R3)                                                 
         GOTO1 ADDL,DMCB,AIO,EXPELM                                             
         B     AC29X               ADD OVERRIDE AND EXIT                        
*              AUTO VEHICLE NUMBER                                              
AC29VA   MVC   HOLDKEY,KEY                                                      
         MVC   KEY,SPACES                                                       
         LA    R4,HEIREL                                                        
         USING ACHEIRD,R4                                                       
         ZIC   R5,ACHRLEVA                                                      
         AH    R5,=H'2'            C/U/L + LENGTH OF LEVEL A                    
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),HOLDKEY                                                   
         MVC   AIO,AIO2                                                         
         LA    R4,EXPELM                                                        
         USING ACNOD,R4                                                         
         MVC   ACNOEL(2),=X'250A'                                               
         MVC   ACNO(8),=C'00000000' SET UP FOR FIRST TIME                       
         GOTO1 READ                GET LEVEL A                                  
AC29V1   GOTO1 GETL,DMCB,(X'25',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    AC29V2              ALREADY HAVE VEHICLE                         
         GOTO1 ADDL,DMCB,AIO,EXPELM ADD VEHICLE ELEMENT TO LEVEL A              
         B     AC29V1                                                           
AC29V2   L     R4,ELADDR                                                        
         PACK  DUB,ACNO(8)                                                      
         AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  ACNO(8),DUB                                                      
         GOTO1 WRITE     WRITE LEVEL A WITH UPDATED VEHICLE NUMBER              
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'HOLDKEY),HOLDKEY                                           
         GOTO1 DELL,DMCB,(X'25',AIO),0   DELETE VEHICLE NUMBER                  
         GOTO1 ADDL,DMCB,AIO,(R4)      ADD VEHICLE ELEMENT TO ACCOUNT           
AC29X    DC    0H'0'                                                            
         EJECT                                                                  
*              SPECIAL ROUTINES FOR HEIRARCHY                                   
         SPACE 1                                                                
AC34     LA    R2,LOGACCH                                                       
         LA    R4,HEIREL                                                        
         USING ACHEIRD,R4                                                       
         SR    R5,R5                                                            
         IC    R5,ACHRLEVA                                                      
         CLC   5(1,R2),ACHRLEVA                                                 
         BH    AC35                                                             
         CLI   ACHRLEVB,0                                                       
         BNE   AC39                                                             
         GOTO1 BALIN                                                            
         B     AC39                                                             
         SPACE 1                                                                
AC35     CLI   ACHRLEVB,0                                                       
         BE    ERR63                                                            
         CLC   5(1,R2),ACHRLEVB                                                 
         BH    AC36                                                             
         CLI   ACHRLEVC,0                                                       
         BNE   CHECKHIR                                                         
         GOTO1 BALIN                                                            
         B     CHECKHIR                                                         
         SPACE 1                                                                
AC36     IC    R5,ACHRLEVB                                                      
         CLI   ACHRLEVC,0                                                       
         BE    ERR63                                                            
         CLC   5(1,R2),ACHRLEVC                                                 
         BH    AC38                                                             
         CLI   ACHRLEVD,0                                                       
         BNE   CHECKHIR                                                         
         GOTO1 BALIN                                                            
         B     CHECKHIR                                                         
         SPACE 1                                                                
AC38     IC    R5,ACHRLEVC                                                      
         CLI   ACHRLEVD,0                                                       
         BE    ERR63                                                            
         GOTO1 BALIN                                                            
         SPACE 1                                                                
CHECKHIR MVC   HOLDKEY,KEY                                                      
         MVC   KEY,SPACES          CHECK IF HIGHER LEVEL IS THERE               
         MVC   KEY(3),HOLDKEY                                                   
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),HOLDKEY+3                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERR61                                                            
         MVC   KEY(L'HOLDKEY),HOLDKEY                                           
AC39     B     XIT                                                              
         EJECT                                                                  
*              SPECIAL DELETE CODE                                              
AC40     L     R4,AIO                                                           
         AH    R4,DATADISP                                                      
*&&US                                                                           
         SPACE 1                                                                
AC41     CLI   0(R4),0                                                          
         BE    AC41X                                                            
         CLI   0(R4),X'62'         CAN'T DELETE RECORDS WITH A                  
         BNE   AC41B               SCHEME CODE                                  
         USING ACDISTD,R4                                                       
         CP    ACDIVAL,=P'0'                                                    
         BNE   ERR67               ERROR CAN'T DELETE                           
AC41B    ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     AC41                                                             
AC41X    L     R4,AIO                                                           
         AH    R4,DATADISP                                                      
*&&                                                                             
AC42     CLI   0(R4),0                                                          
         BE    AC46                                                             
         CLI   0(R4),X'30'                                                      
         BNE   AC43                                                             
         USING ACSTATD,R4                                                       
         OC    ACSTBFDT,ACSTBFDT   OLD RECORDS                                  
         BZ    AC43                                                             
*&&US                                                                           
         CLI   0(R4),C'3'          SKIP TEST BELOW IF RETAIL                    
         BE    AC43                                                             
*&&                                                                             
         CLC   ACSTLAST,ACSTBFDT   THIS CHECKS FOR SELF-BALANCING DR/CR         
         BH    ERR67                                                            
         SPACE 1                                                                
AC43     CLI   0(R4),X'32'                                                      
         BNE   AC44                                                             
         USING ACBALD,R4                                                        
         CP    ACBLFRWD,=P'0'      ACCOUNT MUST HAVE BALANCE ELEMENT            
         BNE   ERR67               AND ALL BALANCES MUST BE ZERO                
         CP    ACBLDR,=P'0'                                                     
         BNE   ERR67                                                            
         CP    ACBLCR,=P'0'                                                     
         BNE   ERR67                                                            
         B     XIT                                                              
         SPACE 1                                                                
AC44     SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     AC42                                                             
         SPACE 1                                                                
AC46     L     R4,AIO                                                           
         MVC   KEY,0(R4)     IF RECORD HAS NO BALANCE ELEMENT                   
         MVC   KEYSAVE,KEY                                                      
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                WE NEED TO READ THE NEXT RECORD TO           
         MVI   RDUPDATE,C'N'                                                    
         LA    R4,HEIREL           GET ACCOUNT LEVEL                            
         USING ACHEIRD,R4                                                       
         ZIC   R3,ACHRLEVA                                                      
         LA    R5,KEY+3(R3)                                                     
         CLI   ACHRLEVA,12                                                      
         BE    AC48                                                             
         CLI   0(R5),C' '                                                       
         BE    AC48                LEVEL B IS BLANK THIS IS LEVEL A             
         ZIC   R3,ACHRLEVB                                                      
         LA    R5,KEY+3(R3)                                                     
         CLI   ACHRLEVB,12                                                      
         BE    AC48                                                             
         CLI   0(R5),C' '                                                       
         BE    AC48                LEVEL C IS BLANK THIS IS LEVEL B             
         ZIC   R3,ACHRLEVC                                                      
         LA    R5,KEY+3(R3)                                                     
         CLI   ACHRLEVC,12                                                      
         BE    AC48                                                             
         CLI   0(R5),C' '                                                       
         BE    AC48                LEVEL D IS BLANK THIS IS LEVEL C             
         LA    R3,12               LOWLEVEL ACCOUNT                             
         LA    R5,KEY+3(R3)                                                     
         SPACE 1                                                                
AC48     ZIC   R0,0(R5)            READ NEXT  UNDELETED RECORD                  
         AH    R0,=H'1'                                                         
         STC   R0,0(R5)                                                         
         GOTO1 HIGH                INTO IO2                                     
         L     R4,AIO                                                           
         MVC   AIO,AIO1            RESTORE A(IO1)                               
         SPACE 1                                                                
         LA    R3,2(R3)            LENGTH FOR COMPARE                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),KEYSAVE                                                  
         BE    ERR67                                                            
*                                                                               
         CLI   LOWLEV,C'Y'         ONLY CHECK FOR LOW LEVELS                    
         BNE   XIT                                                              
         USING ASTELD,RF                                                        
         GOTO1 GETL,DMCB,(ASTELQ,AIO),0                                         
         CLI   ELERR,0                                                          
         BNE   XIT                 NO DRAFT TRANS ELEMENTS, OK TO DEL           
         L     RF,ELADDR                                                        
         OC    ASTDRAFT,ASTDRAFT                                                
         BZ    XIT                 OK TO DELETE THIS                            
         B     ERR67               CAN'T DELETE DRAFTS                          
         EJECT                                                                  
*              GET LEDGER AND HEIRARCHY ELEMENTS                                
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
LDGET    NTR1                                                                   
         LA    R4,KEY                                                           
         CLC   ACKEYACC+1(2),UNTLDG    SAME UNIT/LEDGER                         
         BE    LDLEV                   ALREADY CORRECT ELEMENTS                 
         MVC   HOLDKEY,ACKEYACC                                                 
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),HOLDKEY                                                   
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 NAMOUT,DMCB,AIO,WORK                                             
         MVC   LOGLNAM,WORK                                                     
         OI    LOGLNAMH+6,X'80'                                                 
         GOTO1 GETL,DMCB,(X'16',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO LEDGER ELEMENT                            
         L     R2,ELADDR                                                        
         MVC   HEIREL,0(R2)        SAVE HEIRARCHY ELEMENT                       
         GOTO1 GETL,DMCB,(X'14',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO LEDGER ELEMENT                            
         L     R2,ELADDR                                                        
         MVC   LEDGEL,0(R2)        SAVE LEDGER ELEMENT                          
         MVC   UNTLDG,ACKEYACC+1                                                
         MVC   ACKEYACC,HOLDKEY                                                 
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         USING ACHEIRD,R4                                                       
LDLEV    LA    R4,HEIREL           SEE IF THIS IS A LOW LEVEL ACCOUNT           
         MVI   LOWLEV,C'N'                                                      
         CLI   ACHRLEVB,0                                                       
         BE    YESLOW                                                           
         ZIC   R5,ACHRLEVA                                                      
         LA    R5,KEY+3(R5)                                                     
         CLI   0(R5),C' '                                                       
         BE    XIT                                                              
         CLI   ACHRLEVC,0                                                       
         BE    YESLOW                                                           
         ZIC   R5,ACHRLEVB                                                      
         LA    R5,KEY+3(R5)                                                     
         CLI   0(R5),C' '                                                       
         BE    XIT                                                              
         CLI   ACHRLEVD,0                                                       
         BE    YESLOW                                                           
         ZIC   R5,ACHRLEVC                                                      
         LA    R5,KEY+3(R5)                                                     
         CLI   0(R5),C' '                                                       
         BE    XIT                                                              
YESLOW   MVI   LOWLEV,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
CANTCHA  MVC   CONHEAD(L'MSG1),MSG1                                             
         B     MYEND                                                            
MYEND    MVI   ERROR,X'FE'                                                      
         B     THEEND                                                           
ERR13    MVI   ERROR,13                                                         
         B     ACMESG                                                           
ERR17    MVI   ERROR,ACCINVAL                                                   
         B     ACMESG                                                           
ERR18    MVI   ERROR,18                                                         
         B     ACMESG                                                           
ERR61    MVI   ERROR,NOHIGHER                                                   
         B     ACMESG                                                           
ERR62    MVI   ERROR,ONETO255                                                   
         B     ACMESG                                                           
ERR63    MVI   ERROR,ACTOOLNG                                                   
         B     ACMESG                                                           
ERRGLREQ MVI   ERROR,GLNUMREQ      GL=#### REQ'D ON 3P                          
         B     ACMESG                                                           
ERR67    MVI   ERROR,CANTDEL                                                    
         LA    R2,LOGACCH                                                       
         B     ACMESG                                                           
FINV     MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
ACMESG   MVI   GETMSYS,6           ACCOUNT MESSAGES (SYST 6)                    
THEEND   GOTO1 EXIT                                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT ,                                                                
*              CONSTANTS TABLES ETC                                             
         SPACE 1                                                                
MSG1     DC    C'** ERROR ** THIS DATA CAN NOT BE CHANGED'                      
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*              DSECT FOR THIS MODULE                                            
         SPACE 1                                                                
LWSD     DSECT                                                                  
RELO     DS    A                                                                
ADBLOCK  DS    F                                                                
DEL23EL  DS    F                                                                
*                                                                               
TOTBLK   DS    H                                                                
*                                                                               
SPACES   DS    CL132                                                            
UNTLDG   DS    CL2                 UNIT/LEDGER                                  
HEIREL   DS    CL75                HEIRARCHY ELEMENT                            
LEDGEL   DS    CL50                LEDGER ELEMENT                               
HOLDKEY  DS    CL32                                                             
MYELEM   DS    CL87                                                             
LOWLEV   DS    CL1                                                              
DBELEM   DS    CL15                                                             
GLSTAT   DS    XL1                                                              
GLSYES   EQU   X'80'               YES GL= ENTERED                              
LWSEND   EQU   *                                                                
         EJECT ,                                                                
       ++INCLUDE ACEXPWORKD                                                     
         EJECT ,                                                                
       ++INCLUDE ACEXPFFD                                                       
         EJECT ,                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE ACEXPFED                                                       
         EJECT ,                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT ,                                                                
         SPACE 1                                                                
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086ACEXP01   09/11/02'                                      
         END                                                                    
