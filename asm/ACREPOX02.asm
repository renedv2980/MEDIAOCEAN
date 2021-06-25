*          DATA SET ACREPOX02  AT LEVEL 014 AS OF 05/01/02                      
*PHASE ACOX02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE POPTCONV                                                               
         TITLE 'PRODUCTION OPTION CONVERSION'                                   
ACOX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACOX**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACOXD,RC                                                         
*              OPTIONS                                                          
***********************************************************************         
* OPTION1     D=DELETE OLD ELMENTS (X'3C', X'42')                     *         
* OPTION2     Y= DUMP OUTPUT RECORDS(RCVTAPE)                        *          
* OPTION3     Y= DUMP INPUT RECORDS BEFORE AND AFTER                  *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   ACC10                                                            
         MVC   PRNTBL(VTYPLNQ),VTYPES                                           
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         L     R2,=A(PUTRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         B     XIT                                                              
         EJECT                                                                  
ACC10    CLI   MODE,REQFRST                                                     
         BNE   ACC30                                                            
         ZAP   OUTRCD,=P'0'                                                     
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   EVERY,=P'1'                                                      
         ZAP   PDUMP,=P'0'                                                      
         ZAP   MAXDUMP,=P'50'                                                   
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         L     R3,ADCOMP                                                        
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         XC    0(50,R4),0(R4)      SET-UP OPTIONS KEY                           
         MVI   ACOPRTYP,ACOPEQU                                                 
         MVI   ACOPSREC,ACOPSEQU                                                
         MVC   ACOPCUL(1),0(R3)        COMPANY                                  
         MVC   ACOPCUL+1(2),=C'SJ'                                              
         L     R5,APUTHOOK         GET COMPANY OPTIONS/RATES                    
         GOTO1 POPTCONV,DMCB,(C'C',(R3)),(R4),(R5),ADCOMFAC                     
ACC15    BAS   RE,DELETE           DELETE OLD ELEMENTS                          
         B     XIT                                                              
         EJECT                                                                  
ACC30    CLI   MODE,PROCLEVA                                                    
         BNE   ACC40                                                            
         L     R3,ADHEIRA          SETUP KEY AT CLIENT LEVEL                    
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         XC    ACOPCLI(50),ACOPCLI                                              
         MVC   ACOPCLI,SPACES                                                   
         L     R5,ADLDGHIR                                                      
         USING ACHEIRD,R5                                                       
         ZIC   R1,ACHRLEVA                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOPCLI(0),3(R3)                                                 
         L     R5,APUTHOOK                                                      
         GOTO1 POPTCONV,DMCB,(C'C',(R3)),(R4),(R5),ADCOMFAC                     
         BAS   RE,DELETE           DELETE OLD ELEMENTS                          
         B     XIT                                                              
         EJECT                                                                  
ACC40    CLI   MODE,PROCLEVB                                                    
         BNE   ACC50                                                            
         L     R3,ADHEIRB          SETUP PRODUCT CODE                           
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         XC    ACOPPRO(50),ACOPPRO                                              
         MVC   ACOPPRO,SPACES                                                   
         L     R5,ADLDGHIR                                                      
         USING ACHEIRD,R5                                                       
         ZIC   R1,ACHRLEVB                                                      
         ZIC   RF,ACHRLEVA                                                      
         LA    RE,3(RF,R3)         RE TO START OF PRODUCT FIELD                 
         SR    R1,RF               R1 FOR LENGTH OF PRODUCT                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOPPRO(0),0(RE)                                                 
         L     R5,APUTHOOK                                                      
         GOTO1 POPTCONV,DMCB,(C'P',(R3)),(R4),(R5),ADCOMFAC                     
         BAS   RE,DELETE           DELETE OLD ELEMENTS                          
         B     XIT                                                              
         EJECT                                                                  
ACC50    CLI   MODE,PROCLEVC                                                    
         BNE   ACC60                                                            
         L     R3,ADHEIRC          SETUP JOB KEY                                
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         XC    ACOPJOB(50),ACOPJOB                                              
         MVC   ACOPJOB,SPACES                                                   
         L     R5,ADLDGHIR                                                      
         USING ACHEIRD,R5                                                       
         ZIC   R1,ACHRLEVC                                                      
         ZIC   RF,ACHRLEVB                                                      
         LA    RE,3(RF,R3)         RE TO JOB FIELD                              
         SR    R1,RF               R1 TO LENGTH OF JOB JOB                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOPJOB(0),0(RE)                                                 
         L     R5,APUTHOOK                                                      
         GOTO1 POPTCONV,DMCB,(C'J',(R3)),(R4),(R5),ADCOMFAC                     
         BAS   RE,DELETE           DELETE OLD ELEMENTS                          
         B     XIT                                                              
         EJECT                                                                  
ACC60    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         OPEN  (RCVTAPE,(OUTPUT))                                               
ACC63    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    ACC70               END OF INPUT                                 
         XR    R5,R5                                                            
         ICM   R5,3,0(R4)                                                       
         LA    R2,RCVHEAD                                                       
         LA    R3,1028                                                          
         MVCL  R2,R4               MOVE SORT RECORD TO IO AREA                  
         BAS   RE,PRNTIT           PRINT THE REPORT                             
         LA    R5,OUTREC           ADD TO RECOVERY TAPE                         
         LA    R8,RCVTAPE                                                       
         PUT   (R8),(R5)                                                        
         CLI   QOPT2,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,DMPRCV                                                        
         AP    OUTRCD,=P'1'                                                     
         B     ACC63               AND GET NEXT SORT RECORD                     
*                                                                               
ACC70    CLOSE (RCVTAPE)                                                        
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P+1(15),=C'RECORDS ADDED'                                        
         EDIT  OUTRCD,(12,P+17),COMMAS=YES                                      
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
PRNTIT   NTR1                                                                   
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         MVC   P+2(1),ACOPOG       OFFICE GROUP                                 
         MVC   P+6(1),ACOPOFF      OFFICE                                       
         MVC   P+9(6),ACOPCLI      CLIENT                                       
         MVC   P+16(6),ACOPPRO     PRODUCT                                      
         MVC   P+24(6),ACOPJOB     JOB                                          
         MVC   P+33(1),ACOPMG      MEDIA GROUP                                  
         MVC   P+37(1),ACOPMED     MEDIA                                        
         MVC   P+41(1),ACOPWG      WORK GROUP                                   
         MVC   P+45(2),ACOPWORK    WORK CODE                                    
         LA    R2,P+50                                                          
         LA    R6,IO                                                            
         USING ACOPD,R6                                                         
         MVI   ELCODE,X'A4'                                                     
         BAS   RE,GETEL                                                         
PRNT15   BNE   PRNT20                                                           
         BAS   RE,DISFIELD                                                      
         GOTO1 ACREPORT                                                         
         BAS   RE,NEXTEL                                                        
         B     PRNT15                                                           
PRNT20   GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SPECIFIC FIELD                                           
*                                  ACOPNUM HAS FIELD NUMBER                     
*                                  ACOPLEN HAS LENGTH                           
*                                  ACOPDATA HAS DATA                            
         SPACE 1                                                                
DISFIELD NTR1                                                                   
         MVC   WORK(1),ACOPNUM                                                  
         BAS   RE,FINDTAB          POSITION R1 TO TABLE ENTRY                   
         USING FDTABD,R1                                                        
         MVC   0(L'FDTHEAD,R2),FDTHEAD                                          
         LA    R2,L'FDTHEAD(R2)                                                 
         ZIC   RF,FDTROUT          PICK UP ROUTINE NUMBER                       
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     DFBRANCH(RF)                                                     
         SPACE 1                                                                
DFBRANCH DS    0F                                                               
         B     DF1                                                              
         B     DF2                                                              
         B     DF3                                                              
         B     DF4                                                              
         B     DF5                                                              
         B     DF6                                                              
         B     DF7                                                              
         B     DF8                                                              
         B     DF9                                                              
         B     DF10                                                             
         SPACE 1                                                                
DF1      MVC   8(5,R2),=C'ZERO%'   PERCENTAGE EXPRESSION (4 DEC)                
         CP    ACOPDATA(4),=P'0'                                                
         BZ    XIT                                                              
         EDIT  (P4,ACOPDATA),(9,8(R2)),4,ALIGN=LEFT                             
         LA    R1,17(R2)           R1 TO END OF EXPRESSION                      
         SPACE 1                                                                
DF1B     BCTR  R1,0                POSITION R1 TO DEC POINT                     
         CLI   0(R1),X'41'                                                      
         BL    DF1B                                                             
         CLI   0(R1),C'0'          GET RID OF TRAILING ZERO                     
         BNE   *+12                                                             
         MVI   0(R1),C' '                                                       
         B     DF1B                                                             
         CLI   0(R1),C'.'                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   0(R1),C'%'                                                       
         B     XIT                                                              
         SPACE 1                                                                
DF2      LA    R3,ACOPDATA         UNBILLABLE WORK CODES                        
         LA    R4,8(R2)                                                         
         LA    R0,6                UP TO 6 ENTRIES                              
         SPACE 1                                                                
DF2B     MVC   0(2,R4),0(R3)                                                    
         CH    R0,=H'1'                                                         
         BE    XIT                                                              
         CLI   2(R3),X'41'                                                      
         BL    XIT                                                              
         CLI   1(R4),X'41'         CHECK 1 BYTE WORK CODE                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         MVI   2(R4),C','          COMMA IF NOT LAST ENTRY                      
         LA    R3,2(R3)                                                         
         LA    R4,3(R4)                                                         
         BCT   R0,DF2B                                                          
         SPACE 1                                                                
DF3      CLI   ACOPDATA,C'E'       BILLING TYPE                                 
         BE    DF3E                                                             
         CLI   ACOPDATA,C'F'                                                    
         BE    DF3F                                                             
         CLI   ACOPDATA,C'S'                                                    
         BE    DF3S                                                             
         MVC   8(5,R2),=C'TOTAL'                                                
         CLI   ACOPDATA,C'T'                                                    
         BE    XIT                                                              
         MVC   8(6,R2),=C'1 LINE'                                               
         CLI   ACOPDATA,C'1'                                                    
         BE    XIT                                                              
         MVC   8(6,R2),=C'CLIENT'                                               
         CLI   ACOPDATA,C'C'                                                    
         BE    XIT                                                              
         MVC   8(10,R2),=C'UNBILLABLE'                                          
         CLI   ACOPDATA,C'U'                                                    
         BE    XIT                                                              
         MVC   8(11,R2),=C'PROGRESSIVE'                                         
         B     XIT                                                              
         SPACE 1                                                                
DF3E     LA    R3,ACOPDATA+1       ESTIMATE PERCENT(S)                          
         LA    R4,8(R2)                                                         
         LA    R5,3                UP TO 3 PERCENTS SUPPORTED                   
         SPACE 1                                                                
DF3E2    EDIT  (4,(R3)),(7,0(R4)),2,ALIGN=LEFT,FLOAT=-                          
         LR    R1,R4                                                            
         BCTR  R1,0                                                             
         SPACE 1                                                                
DF3E4    LA    R1,1(R1)                                                         
         CLI   0(R1),C'.'          LOOK FOR THE DECIMAL POINT                   
         BNE   DF3E4                                                            
         LA    R4,3(R1)                                                         
         CLC   1(2,R1),=C'00'      GET RID OF SUPERFLUOUS .00                   
         BNE   DF3E6                                                            
         MVC   0(3,R1),=C'   '                                                  
         LR    R4,R1                                                            
         SPACE 1                                                                
DF3E6    MVI   0(R4),C'%'          POP IN THE PERCENT SIGN                      
         CH    R5,=H'1'                                                         
         BE    XIT                                                              
         OC    4(4,R3),4(R3)                                                    
         BZ    XIT                                                              
         MVI   1(R4),C','          AND COMMA IF NOT LAST                        
         LA    R4,2(R4)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,DF3E2                                                         
         SPACE 1                                                                
DF3F     MVC   8(3,R2),=C'FEE'     FEE BILLING                                  
         LA    R3,ACOPDATA+1                                                    
         EDIT  (4,(R3)),(12,11(R2)),2,ALIGN=LEFT                                
         B     XIT                                                              
         SPACE 1                                                                
DF3S     LA    R3,ACOPDATA+1       SPECIAL AMOUNT                               
         EDIT  (4,(R3)),(12,8(R2)),2,ALIGN=LEFT,FLOAT=-                         
         B     XIT                                                              
         SPACE 1                                                                
DF4      DS    0H                  NUMERIC 1-255                                
         EDIT  (1,ACOPDATA),(3,8(R2)),ALIGN=LEFT,FLOAT=-                        
         B     XIT                                                              
         SPACE 1                                                                
DF5      DS    0H                  CASH FIELD                                   
         EDIT  (P4,ACOPDATA),(12,8(R2)),2,ALIGN=LEFT,FLOAT=-                    
         B     XIT                                                              
         SPACE 1                                                                
DF6      MVC   8(2,R2),=C'NO'      Y(ES) N(O) FIELD                             
         CLI   ACOPDATA,C'N'                                                    
         BE    XIT                                                              
         MVC   8(3,R2),=C'YES'                                                  
         B     XIT                                                              
         SPACE 1                                                                
DF7      ZIC   R1,ACOPLEN          CHARACTERS NO VALIDATION                     
         SH    R1,=H'17'                                                        
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   8(0,R2),ACOPDATA                                                 
         SPACE 1                                                                
DF8      MVC   8(1,R2),ACOPDATA    NEW BILL TYPE                                
         B     XIT                                                              
         SPACE 1                                                                
DF9      MVC   8(1,R2),ACOPDATA    VAT TAX CODE                                 
         B     XIT                                                              
         SPACE 1                                                                
DF10     MVC   8(5,R2),=C'ZERO%'   PERCENTAGE EXPRESSION (2 DEC)                
         CP    ACOPDATA(4),=P'0'                                                
         BZ    XIT                                                              
         EDIT  (P4,ACOPDATA),(9,8(R2)),2,ALIGN=LEFT                             
         LA    R1,17(R2)           R1 TO END OF EXPRESSION                      
         B     DF1B                                                             
         EJECT                                                                  
FINDTAB  LA    R1,FDTABLE                                                       
         SPACE 1                                                                
FINDTAB2 CLC   WORK(1),0(R1)       LOOK UP TABLE FOR ENTRY NUMBER               
         BER   RE                                                               
         AH    R1,FDENLEN                                                       
         CLI   0(R1),X'FF'                                                      
         BNE   FINDTAB2                                                         
         DC    H'0'                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              DELETE THE OLD ELEMENTS                                          
*                                                                               
         DROP  R4                                                               
         USING ACKEYD,R3                                                        
DELETE   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R3),IO2                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   AFTSW,C'N'                                                       
         BAS   RE,DMPBFOR          GET A DUMP BEFORE                            
         CLI   QOPT1,C'D'                                                       
         BNE   DEL03                                                            
         GOTO1 DELL,DMCB,(X'3C',IO2),0    EXTRA PROFILE                         
         GOTO1 DELL,DMCB,(X'42',IO2),0    AND RULES                             
DEL03    CLI   MODE,REQFRST                                                     
         BNE   DEL05                                                            
         GOTO1 GETL,DMCB,(X'10',IO2),0    EXTRA PROFILE                         
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
         L     R6,DMCB+12                                                       
         USING ACCOMPD,R6                                                       
         OI    ACMPSTA4,X'20'      TURN ON CONVERTED BIT                        
DEL05    CLI   RCWRITE,C'N'                                                     
         BE    DEL07                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCOUNT',IO2,IO2                           
DEL07    BAS   RE,DMPAFTR            GET DUMP AFTER                             
         MVC   IO2(50),SPACES                                                   
         MVC   IO2(32),KEY                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',IO2,IO2    RE-READ KEY           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
DMPBFOR  NTR1                                                                   
         CLI   QOPT3,C'Y'                                                       
         BNE   XIT                                                              
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         XR    R8,R8                                                            
         LA    R3,IO2                                                           
         ICM   R8,3,ACLENGTH                                                    
         GOTO1 PRNTBL,DMCB,(6,=C'BEFORE'),(R3),C'DUMP',(R8),=C'2D'              
         MVI   AFTSW,C'Y'                                                       
         B     XIT                                                              
         SPACE 1                                                                
DMPAFTR  NTR1                                                                   
         CLI   AFTSW,C'Y'                                                       
         BNE   XIT                                                              
         XR    R8,R8                                                            
         LA    R3,IO2                                                           
         ICM   R8,3,ACLENGTH                                                    
         GOTO1 PRNTBL,DMCB,(5,=C'AFTER'),(R3),C'DUMP',(R8),=C'2D'               
         MVI   AFTSW,C'Y'                                                       
         B     XIT                                                              
         SPACE 1                                                                
DMPRCV   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'RCVTAPE'                                                   
         XR    R8,R8                                                            
         ICM   R8,3,RCVHEAD                                                     
         LA    R3,RCVHEAD                                                       
         GOTO1 PRNTBL,DMCB,(7,=C'RCVTAPE'),(R3),C'DUMP',(R8),=C'2D'             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
DELL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCOUNT '),(R2),(R3)                         
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
VTYPES   DS    0F                                                               
         DC    V(PRNTBL)                                                        
         DC    V(HELLO)                                                         
         DC    V(SORTER)                                                        
         DC    V(POPTCONV)                                                      
         DC    A(PUTHOOK)                                                       
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(29,42,A),FORMAT=BI,WORK=1 '                    
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(1100,,,,) '                           
         SPACE 2                                                                
*              FIELD DETAIL TABLE                                               
         SPACE 1                                                                
FDENLEN  DC    H'34'                                                            
FDTABLE  DS    0H                                                               
*                 FIELD   FIELD DESCRIPTION               FIELD                 
*                  NO.    -----------------               ROUT#                 
         SPACE 1                                                                
         DC    AL1(101),C'AGENCY COMMISSION RATE         ',AL1(1,0)             
         DC    AL1(102),C'UNBILLABLE WORK CODE LIST      ',AL1(2,0)             
         DC    AL1(103),C'BILLING TYPE                   ',AL1(3,0)             
         DC    AL1(104),C'NUMBER OF DAYS BILL DUE        ',AL1(4,0)             
         DC    AL1(105),C'MAXIMUM PERCENT OF ESTIMATE    ',AL1(10,0)            
         DC    AL1(106),C'MAXIMUM $AMOUNT OVER ESTIMATE  ',AL1(5,0)             
         DC    AL1(107),C'ESTIMATE NEEDED FOR BILLS?     ',AL1(6,0)             
         DC    AL1(108),C'MINIMUM BILL AMOUNT            ',AL1(5,0)             
         DC    AL1(109),C'PRINT CLIENT SUMMARY ON BILLS? ',AL1(6,0)             
         DC    AL1(110),C'PRINT PRODUCT SUMMARY ON BILLS?',AL1(6,0)             
         DC    AL1(111),C'SHOW DETAIL LINES ON BILLS?    ',AL1(6,0)             
         DC    AL1(112),C'SHOW WORK CODE ON EST. BILLS?  ',AL1(6,0)             
         DC    AL1(113),C'PASS CASH DISCOUNT TO CLIENT?  ',AL1(6,0)             
         DC    AL1(114),C'PAYMENTS ARE NET?              ',AL1(6,0)             
         DC    AL1(115),C'SUPPRESS AGENCY COMMISSION?    ',AL1(6,0)             
         DC    AL1(116),C'AUTOMATIC ESTIMATE TURN-AROUND?',AL1(6,0)             
         DC    AL1(117),C'TRANSFER TO SPOT/PRINT?        ',AL1(6,0)             
         DC    AL1(118),C'ESTIMATE BILLS COMMISSIONABLE? ',AL1(6,0)             
         DC    AL1(119),C'GROSS + CASH DISCOUNT COSTING? ',AL1(6,0)             
         DC    AL1(120),C'NEW JOBS ARE UNAPPROVED?       ',AL1(6,0)             
         DC    AL1(121),C'IS THIS ESTIMATE APPROVED?     ',AL1(6,0)             
         DC    AL1(122),C'AUTO UNHOLD WHEN BILLING?      ',AL1(6,0)             
         DC    AL1(123),C'NEW BILL TYPE FOR %EST         ',AL1(8,0)             
         DC    AL1(124),C'FILTER 2 WHEN EST TO TOT       ',AL1(7,0)             
*&&US*&& DC    AL1(125),C'PRINT WORK CODE ON AC21?       ',AL1(6,0)             
*&&UK*&& DC    AL1(126),C'VAT TAX CODE (S/X/Z)           ',AL1(9,0)             
         DC    AL1(127),C'SUPPRESS STICKY LABELS?        ',AL1(6,0)             
         SPACE 1                                                                
         DC    AL1(131),C'SHOW CLIENT WORK CODE AS ...   ',AL1(7,0)             
         DC    AL1(132),C'OVERRIDE WORKCODE DESCRIPTION  ',AL1(7,0)             
         DC    AL1(133),C'ONE LINE PER WORKCODE ON BILLS?',AL1(6,0)             
         DC    AL1(134),C'ONE LINE PER WORKCODE ON P1    ',AL1(6,0)             
         DC    X'FF'                                                            
         EJECT                                                                  
*              DTF FOR OUTPUT TAPE                                              
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,RECFM=VB,DSORG=PS,MACRF=(PM),            X        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PUTHOOK  NMOD1 0,*PHOOK                                                         
         L     RC,PUTRC                                                         
         USING ACKEYD,R4                                                        
         XC    OUTREC(28),OUTREC                                                
         LA    R4,IO                                                            
         XR    R5,R5                                                            
         ICM   R5,3,ACLENGTH                                                    
         LA    R5,28(R5)           ADD ON L'HEADER+4 TO RECORD LENGTH           
         STH   R5,RCVHEAD                                                       
         MVC   RCVHEAD+4(2),=X'6103'     INDICATE ACCOUNT/ADD                   
         LA    R4,RCVHEAD                                                       
         GOTO1 SORTER,DMCB,=C'PUT',(R4),0                                       
         XIT1                                                                   
PUTRC    DC    A(0)                                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BOX ROUTINES (HOOK)                                              
         SPACE 2                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC           RESTORE REG C                                 
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+7,C'T'        SET ROWS                                     
         MVI   MYROW+9,C'M'                                                     
         MVI   MYROW+56,C'B'                                                    
         MVI   MYCOL,C'L'          SET LH MARGIN                                
         MVI   MYCOL+4,C'C'                                                     
         MVI   MYCOL+8,C'C'                                                     
         MVI   MYCOL+15,C'C'                                                    
         MVI   MYCOL+23,C'C'                                                    
         MVI   MYCOL+31,C'C'                                                    
         MVI   MYCOL+34,C'C'                                                    
         MVI   MYCOL+40,C'C'                                                    
         MVI   MYCOL+43,C'C'                                                    
         MVI   MYCOL+48,C'C'                                                    
         MVI   MYCOL+87,C'C'                                                    
         MVI   MYCOL+104,C'R'                                                   
         SPACE 1                                                                
         MVC   BOXROWS,MYROW                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XIT1                                                                   
         SPACE 2                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ACOXD    DSECT                                                                  
PRNTBL   DS    V                                                                
HELLO    DS    V                                                                
SORTER   DS    V                                                                
POPTCONV DS    V                                                                
APUTHOOK DS    A                                                                
*                                                                               
OUTRCD   DS    PL6                                                              
DUMPCNT  DS    PL4                                                              
EVERY    DS    PL4                                                              
PDUMP    DS    PL4                                                              
MAXDUMP  DS    PL4                                                              
ELCODE   DS    CL1                                                              
TODAY2   DS    CL2                                                              
AFTSW    DS    CL1                                                              
ADBOX    DS    A                                                                
MYROW    DS    CL100                                                            
MYCOL    DS    CL132                                                            
         DS    0D                                                               
OUTREC   DS    1028C                                                            
         ORG   OUTREC                                                           
RCVHEAD  DS    28C                                                              
IO       DS    1000C                                                            
IO2      DS    1000C                                                            
         EJECT                                                                  
*                                  DSECT TO COVER ENTRY IN TABLE ABOVE          
         SPACE 1                                                                
FDTABD   DSECT                                                                  
FDTFNO   DS    XL1                 FIELD NUMBER (ALSO HELP NO)                  
FDTHEAD  DS    CL31                TEXT THAT APPEARS ON THE SCREEN              
FDTROUT  DS    XL1                 INPUT VALIDATION ROUTINE NUMBER              
         DS    CL1                 SPARE                                        
         SPACE 1                                                                
*                                                                               
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  DDBIGBOX                                                                     
*  DDREPXTRAD                                                                   
*  DDREPMASTD                                                                   
*  DDBOXEQUS                                                                    
*  DDREMOTED                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACREPOX02 05/01/02'                                      
         END                                                                    
