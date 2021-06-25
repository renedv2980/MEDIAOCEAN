*          DATA SET ACREP7302  AT LEVEL 072 AS OF 03/18/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045076.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE AC7302A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'ACCOUNT NAME AND PROFILE LISTING'                               
AC7302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC73**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING WRKD,RC             RC=A(TEMP W/S)                               
         ST    R5,PRELOC                                                        
         L     RE,=A(BLOCK)        RELOCATE A(PRINT BLOCK)                      
         A     RE,PRELOC                                                        
         ST    RE,ABLOCK                                                        
         L     R8,ADIO                                                          
         USING OUTD,R8             R8=A(RECORD)                                 
         SPACE 1                                                                
         L     RE,=A(SORTC)                                                     
         A     RE,PRELOC                                                        
         ST    RE,ASORTC                                                        
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE LEVELS PRE-SORT                                                 
*---------------------------------------------------------------------*         
*&&US*&& CLI   QOPT7,C'S'          IF OPTION 7 IS A 'S',                        
*&&UK*&& CLI   QOPT7,C'Y'          (OR A 'Y' IN UK)                             
         BE    LEVMODES            SUPPRESS HIGH LEVELS IF NO ACCOUNTS.         
         B     PRCMODES            ELSE, PRINT ALL LEVELS THAT                  
LEVMODES CLI   MODE,PROCACC        PASS THE FILTER TESTS                        
         BE    AC1                                                              
         CLI   MODE,LEVCFRST                                                    
         BE    AC2                                                              
         CLI   MODE,LEVBFRST                                                    
         BE    AC3                                                              
         CLI   MODE,LEVAFRST                                                    
         BE    AC4                                                              
         B     AC6                                                              
         SPACE 1                                                                
PRCMODES CLI   MODE,PROCLEVD                                                    
         BE    AC1                                                              
         CLI   MODE,PROCLEVC                                                    
         BE    AC2                                                              
         CLI   MODE,PROCLEVB                                                    
         BE    AC3                                                              
         CLI   MODE,PROCLEVA                                                    
         BE    AC4                                                              
         B     AC6                                                              
         SPACE 1                                                                
AC1      L     R2,ADACC            LEVEL=4                                      
         L     R3,ADACCSTA                                                      
         MVC   AADDRESS,ADACCADD   A(AADDRESS ELEMENT)                          
         CLI   QOPT1,C' '          ALL                                          
         BE    AC10                                                             
         CLI   QOPT1,C'4'          OR LEVEL 4-ONLY                              
         BNE   EXIT                                                             
         B     AC10                                                             
         SPACE 1                                                                
AC2      L     R2,ADHEIRC          LEVEL=3                                      
         L     R3,ADLVCSTA                                                      
         MVC   AADDRESS,ADLVCADD   A(AADDRESS ELEMENT)                          
         CLI   QOPT1,C'J'                                                       
         BE    AC10                                                             
         CLI   QOPT1,C' '          ALL                                          
         BE    AC10                                                             
         CLI   QOPT1,C'3'                                                       
         BE    AC10                                                             
         CLI   QOPT1,C'B'          LEVEL 3 ONLY                                 
         BE    AC10                                                             
         B     EXIT                                                             
*                                                                               
AC3      L     R2,ADHEIRB          LEVEL=2                                      
         L     R3,ADLVBSTA                                                      
         MVC   AADDRESS,ADLVBADD   A(AADDRESS ELEMENT)                          
         CLI   QOPT1,C'2'          1 & 2                                        
         BE    AC10                                                             
         CLI   QOPT1,C' '          ALL                                          
         BE    AC10                                                             
         CLI   QOPT1,C'3'                                                       
         BE    AC10                                                             
         CLI   QOPT1,C'A'          LEVEL 2 ONLY                                 
         BE    AC10                                                             
         B     EXIT                                                             
*                                                                               
AC4      CLI   QOPT1,C'J'          LEVEL=1                                      
         BE    EXIT                                                             
         CLI   QOPT1,C'4'                                                       
         BE    EXIT                                                             
         CLI   QOPT1,C'A'                                                       
         BE    EXIT                                                             
         CLI   QOPT1,C'B'                                                       
         BE    EXIT                                                             
         L     R2,ADHEIRA                                                       
         L     R3,ADLVASTA                                                      
         MVC   AADDRESS,ADLVAADD   A(AADDRESS ELEMENT)                          
         B     AC10                                                             
*                                                                               
AC6      CLI   MODE,LEDGFRST                                                    
         BNE   AC8                                                              
         MVI   VEHICLE,C'N'        ASSUME NOT VEHICLE LEDGER                    
         L     R2,ADLDGEL                                                       
         USING ACLEDGD,R2                                                       
         TM    ACLTSTAT,X'10'                                                   
         BNO   *+8                                                              
         MVI   VEHICLE,C'Y'                                                     
         SPACE 1                                                                
         MVI   MYSPACE,1                                                        
         MVI   MYSKIP,0                                                         
         MVC   PERPAGE,=H'45'      NUMBER PER PAGE                              
         CLI   QOPT2,C'P'          IF PROFILE OPTION                            
         BE    AC6A                                                             
         CLI   QOPT6,C'Y'          OR ADDRESS OPTION SKIP AFTER PRINT           
         BE    AC6A                                                             
         LA    R7,PROGPROF                                                      
         USING PROFD,R7                                                         
         CLI   PROF1,0                                                          
         BE    AC6B                                                             
         ZIC   R1,PROF1            BLANK LINES                                  
         AH    R1,=H'1'                                                         
         STC   R1,MYSPACE                                                       
         MVC   PERPAGE,=H'23'      23 PER PAGE IF ONE SPACE                     
         CLI   PROF1,1                                                          
         BE    AC6B                                                             
         MVC   PERPAGE,=H'15'      15 PER PAGE IF TWO SPACES                    
         CLI   PROF1,2                                                          
         BE    AC6B                                                             
         MVC   PERPAGE,=H'11'      11 PER PAGE IF THREE SPACES                  
         B     AC6B                                                             
         SPACE 1                                                                
AC6A     LA    R7,PROGPROF                                                      
         USING PROFD,R7                                                         
         MVC   MYSKIP,PROF1                                                     
         SPACE 1                                                                
AC6B     BAS   RE,ACINTBUF         SET PRINT BUFFER TO EMPTY                    
         BAS   RE,ACINTREC         INITIALIZE SORT RECORD                       
         L     R2,ADLEDGER                                                      
         MVC   OUTCMP(114),SPACES                                               
         MVC   OUTCMP(1),0(R2)     EXTRACT C/U/L CODES                          
         MVC   OUTUNT(1),1(R2)                                                  
         MVC   OUTLDG(1),2(R2)                                                  
         L     R2,ADCMPNAM         EXTRACT C/U/L NAMES                          
         LA    R5,OUTCMP+2                                                      
         BAS   RE,ACEXNAM                                                       
         L     R2,ADUNTNAM                                                      
         LA    R5,OUTUNT+2                                                      
         BAS   RE,ACEXNAM                                                       
         L     R2,ADLDGNAM                                                      
         LA    R5,OUTLDG+2                                                      
         BAS   RE,ACEXNAM                                                       
         MVI   INSWITCH,C'N'                                                    
         CLI   QSORT,C' '                                                       
         BE    AC26                                                             
         MVC   OUTLEN,=H'122'      SET RECORD LENGTH                            
         MVI   OUTYPE,0            AND TYPE                                     
         L     R7,ADOUT            A(DTF)                                       
         BAS   RE,SETSORT                                                       
         BAS   RE,PUTSORT                                                       
         MVI   INSWITCH,C'N'                                                    
         B     AC26                                                             
*                                                                               
AC8      CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         BAS   RE,ACPRINT                                                       
         CLI   QSORT,C' '                                                       
         BE    EXIT                                                             
         BAS   RE,ACINTBUF                                                      
         MVI   INSWITCH,C'Y'                                                    
         B     AC26                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        APPLY FILTERS TO RECORD                                                
*---------------------------------------------------------------------*         
AC10     DS    0H                                                               
         USING ACSTATD,R3                                                       
         CLI   QOPT3,C' '          ALL (CLOSED/OPEN)                            
         BE    AC12                                                             
         CLI   QOPT3,C'S'          SUPPRESS CLOSED ACCOUNTS                     
         BNE   *+16                                                             
         TM    ACSTSTAT,X'40'                                                   
         BO    EXIT                                                             
         B     AC12                                                             
         CLI   QOPT3,C'C'          CLOSED ACCOUNTS ONLY                         
         BNE   EXIT                                                             
         TM    ACSTSTAT,X'40'                                                   
         BZ    EXIT                                                             
*                                                                               
AC12     CLI   QOPT4,C' '          ALL (LOCKED/UNLOCKED)                        
         BE    AC13                                                             
         CLI   QOPT4,C'S'          SUPPRESS LOCKED ACCOUNTS                     
         BNE   *+16                                                             
         TM    ACSTSTAT,X'20'                                                   
         BO    EXIT                                                             
         B     AC13                                                             
         CLI   QOPT4,C'L'          LOCKED ACCOUNTS ONLY                         
         BNE   EXIT                                                             
         TM    ACSTSTAT,X'20'                                                   
         BZ    EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BUILD A SORT RECORD                                                    
*---------------------------------------------------------------------*         
AC13     BAS   RE,ACINTREC                                                      
         MVI   XJOB,C' '           CLEAR XJOB INDICATOR                         
         MVC   OUTFLT(2),ACSTFILT                                               
         MVC   OUTFLT+2(1),ACSTANAL                                             
         MVC   OUTFLT+3(1),ACSTSUB                                              
         MVI   OUTFLT+4,C' '                                                    
         CLI   ACSTLEN,ACSTLNQ1                                                 
         BE    *+10                                                             
         MVC   OUTFLT+4(1),ACSTFLT5                                             
         OC    OUTFLT,SPACES                                                    
         MVC   OUTAKY,0(R2)                                                     
         AH    R2,DATADISP         POINT TO FIRST ELEMENT                       
         MVI   OUTYPE,1            ACCOUNT                                      
         CLI   QOPT2,C'P'                                                       
         BNE   *+8                                                              
         MVI   OUTYPE,2            ACCOUNT WITH PROFILE                         
         CLI   QOPT6,C'Y'          AND/OR ADDRESS                               
         BNE   *+8                                                              
         MVI   OUTYPE,2                                                         
         SR    R3,R3                                                            
*                                                                               
*                                                                               
AC14     LA    R4,ELLIST                                                        
         CLI   0(R2),0             END OF RECORD                                
         BE    AC20                                                             
* ********************************************************************          
         CLI   0(R2),X'FF'         CODE TO IGNORE FAULTILY DELETED              
         BE    AC19                ELEMENTS. SIMON 16/07/82.                    
* ********************************************************************          
         CLI   0(R2),X'20'         NAME ELEMENT                                 
         BNE   AC15                                                             
         LA    R5,OUTNAM                                                        
         BAS   RE,ACEXNAM          EXTRACT NAME                                 
         B     AC19                                                             
*                                                                               
AC15     CLI   0(R2),X'26'         JOB ELEMENT                                  
         BNE   AC16                                                             
         BAS   RE,EXTXJOB          EXTRACT XJOB STATUS                          
         B     AC19                                                             
*                                                                               
AC16     CLC   0(1,R2),0(R4)       COMPARE ELEMENT WITH TABLE                   
         BE    AC18                                                             
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    AC19                                                             
         LA    R4,L'ELLIST(R4)                                                  
         B     AC16                                                             
*                                                                               
AC18     CLI   QOPT2,C'P'          PROFILES REQUESTED ?                         
         BNE   AC19                                                             
         L     RF,0(R4)                                                         
         A     RF,PRELOC           RF=A(ROUTINE)                                
         BASR  RE,RF               ADD SOME PROFILE INFO                        
*                                                                               
AC19     IC    R3,1(R2)            BUMP TO NEXT  ELEMENT                        
         AR    R2,R3                                                            
         B     AC14                                                             
         DROP  R3                                                               
*                                                                               
AC20     MVI   OUTFLAG,C' '        CLEAR XJOB INDICATOR                         
         CLI   XJOB,C'Y'           INDICATE IF XJOB                             
         BNE   AC21                                                             
         CLI   QXJOB,C'O'          ARE WE SHOWING ONLY XJOBS?                   
         BE    AC21                YES, NO NEED TO FLAG                         
         MVI   OUTFLAG,C'*'                                                     
*                                                                               
AC21     CLI   QOPT6,C'Y'          ADDRESS OPTION ?                             
         BNE   AC22                                                             
         L     R2,AADDRESS                                                      
         LTR   R2,R2                                                            
         BZ    AC22                                                             
         BAS   RE,ADDADD                                                        
         B     AC22                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        LIST OF ELEMENTS AND ROUTINES TO PROCESS THEM                          
*---------------------------------------------------------------------*         
         CNOP  0,4                                                              
ELLIST   DS    0CL4                                                             
         DC    X'15',AL3(ACPGLA)  *G/L POSTING INSTRUCTIONS                     
         DC    X'21',AL3(ACPNUM)  *NUMBER                                       
         DC    X'23',AL3(ACPOTH)  *OTHERS                                       
         DC    X'24',AL3(ACPPRO)  *PROFILE                                      
         DC    X'25',AL3(ACPNM2)  *EXTRA NUMBER                                 
         DC    X'26',AL3(ACPJOB)  *JOB                                          
         DC    X'27',AL3(ACPBIL)  *ACCOUNT BILLING INFORMATION                  
         DC    X'2B',AL3(ACPRET)  *RETAIL BILLING INFORMATION                   
         DC    X'2C',AL3(ACSPCL)  *SPECIAL POSTING INFORMATION                  
         DC    X'30',AL3(ACPSTA)  *STATUS                                       
         DC    X'36',AL3(ACPVAT)  *VAT                                          
         DC    X'38',AL3(ACPDSC)  *DISCOUNT                                     
         DC    X'39',AL3(ACPARA)  *AREA/REGION/DISTRICT                         
         DC    X'3C',AL3(ACPXPR)  *EXTRA PROFILE                                
         DC    X'3D',AL3(ACPSAL)  *SALES ANALYSIS                               
         DC    X'3E',AL3(ACCOMM)  *COMMENT                       *AWIL*         
         DC    X'56',AL3(ACPDAT)  *HIRE AND TERMINATION DATES                   
         DC    AL1(FFTELQ),AL3(ACPFFT) *FREE FORM- FAX#, VENDOR, ETC            
         DC    AL1(ITCELQ),AL3(ACGST)  *GST/PST INFORMATION                     
         DC    AL1(GDAELQ),AL3(ACGDA)  *GENERAL DATE INFORMATION *AWIL*         
         DC    AL1(UFSELQ),AL3(ACUSER) *USER FIELDS                             
         DC    X'FFFF'             END                                          
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        BUILD AN OUTPUT RECORD                                                 
*---------------------------------------------------------------------*         
AC22     CLI   OUTFLT,C' '         FILL IN BLANK FILTERS                        
         BNE   *+8                                                              
         MVI   OUTFLT,C'.'                                                      
         CLI   OUTFLT+1,C' '                                                    
         BNE   *+8                                                              
         MVI   OUTFLT+1,C'.'                                                    
         CLI   OUTFLT+2,C' '                                                    
         BNE   *+8                                                              
         MVI   OUTFLT+2,C'.'                                                    
         CLI   OUTFLT+3,C' '                                                    
         BNE   *+8                                                              
         MVI   OUTFLT+3,C'.'                                                    
         CLI   OUTFLT+4,C' '                                                    
         BNE   *+8                                                              
         MVI   OUTFLT+4,C'.'                                                    
         SR    R1,R1               WORK OUT 6'RECORD                            
         IC    R1,LCNT                                                          
         LA    R1,1(R1)                                                         
         CLI   LENG,0                                                           
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MH    R1,=H'50'                                                        
         AH    R1,=H'78'                                                        
         STH   R1,OUTLEN                                                        
         CLI   QSORT,C' '          NO SORT REQUIRED                             
         BE    AC28                                                             
         CLI   QSORT,C'F'          SORT ON FILTER                               
         BNE   *+14                                                             
         MVC   OUTSKY(5),OUTFLT                                                 
         B     AC25                                                             
         CLI   QSORT,C'A'          SORT ON NAME                                 
         BNE   *+14                                                             
         MVC   OUTSKY,OUTNAM                                                    
         B     AC25                                                             
         CLI   QSORT,C'T'          SORT ON TAX ID NUMBER                        
         BNE   AC23                                                             
         LA    R3,600                                                           
         LA    R1,OUTPRO           SEE IF ID IS ANYWHERE IN PROFILE             
AC22B10  CLC   0(7,R1),=C'TAX ID='                                              
         BE    AC22B20                                                          
         CLC   0(7,R1),=C'NUMBER='                                              
         BE    AC22B20                                                          
         LA    R1,1(R1)                                                         
         BCT   R3,AC22B10                                                       
         MVI   OUTSKY,X'FF'        IF NO ID THEN FORCE TO END.                  
         B     AC25                                                             
AC22B20  MVC   OUTSKY(9),7(R1)                                                  
         B     AC25                                                             
*       - - - - - - - - - - - - - - - -                                         
AC23     CLI   QSORT,C'V'          SORT ON VEHICLE                              
         BNE   AC24                                                             
         LA    R3,600                                                           
         LA    R1,OUTPRO           SEE IF ID IS ANYWHERE IN PROFILE             
AC23A    CLC   0(8,R1),=C'VEHICLE='                                             
         BE    AC23B                                                            
         LA    R1,1(R1)                                                         
         BCT   R3,AC23A                                                         
         MVI   OUTSKY,X'FF'        IF NO ID THEN FORCE TO END.                  
         B     AC25                                                             
AC23B    MVC   OUTSKY(5),8(R1)                                                  
         B     AC25                                                             
*       - - - - - - - - - - - - - - - -                                         
AC24     CLI   QSORT,C'1'          SORT ON KEY                                  
         BL    EXIT                                                             
         CLI   QSORT,C'9'                                                       
         BH    EXIT                                                             
         PACK  DUB,QSORT                                                        
         CVB   R1,DUB                                                           
         LA    R3,OUTAKY                                                        
         LA    R1,2(R1,R3)         POINT TO POSITION IN KEY                     
         MVC   OUTSKY,0(R1)                                                     
*                                                                               
AC25     L     R7,ADOUT                                                         
         BAS   RE,PUTSORT                                                       
         CLI   QOPT5,C'Y'          PRINT BEFORE AND AFTER SORT                  
         BE    AC26                                                             
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT DECISION HANDLING BEFORE AND AFTER                               
*---------------------------------------------------------------------*         
AC26     CLI   INSWITCH,C'Y'                                                    
         BNE   AC28                                                             
         BAS   RE,GETSORT                                                       
         L     R8,DMCB+4                                                        
         LTR   R8,R8                                                            
         BNZ   AC30                                                             
*&&OS                                                                           
         BAS   RE,ENDSORT                                                       
*&&                                                                             
         BAS   RE,ACPRINT                                                       
         MVI   INSWITCH,C'N'                                                    
         B     EXIT                                                             
*                                                                               
AC28     CLI   QSORT,C' '                                                       
         BE    AC30                                                             
         CLI   QOPT5,C'Y'                                                       
         BNE   EXIT                                                             
*                                                                               
AC30     CLI   OUTYPE,1            ACCOUNT WITHOUT PROFILE                      
         BNE   AC32                                                             
         LH    R5,PERPAGE                                                       
         MH    R5,=H'2'                                                         
         STC   R5,BYTE                                                          
         CLC   PLINES,BYTE         IS BUFFER FULL                               
         BNH   *+8                                                              
         BAS   RE,ACPRINT          YES-EMPTY IT                                 
         SR    R5,R5                                                            
         IC    R5,PLINES                                                        
         LA    R6,1(R5)                                                         
         STC   R6,PLINES                                                        
         L     R6,ABLOCK                                                        
         CH    R5,PERPAGE                                                       
         BNH   *+12                                                             
         LA    R6,55(R6)                                                        
         SH    R5,PERPAGE                                                       
         BCTR  R5,0                                                             
         MH    R5,=H'110'                                                       
         LA    R5,0(R5,R6)         R5=A(PRINT LINE)                             
         MVC   0(12,R5),OUTAKY+3                                                
         MVC   12(1,R5),OUTFLAG                                                 
         MVC   13(5,R5),OUTFLT                                                  
         MVC   19(36,R5),OUTNAM                                                 
         CLI   INSWITCH,C'Y'                                                    
         BE    AC26                                                             
         B     EXIT                                                             
*                                                                               
AC32     CLI   OUTYPE,2            ACCOUNT WITH PROFILE                         
         BNE   AC34                                                             
         MVI   RCSUBPRG,1                                                       
         BAS   RE,ACPREPT                                                       
         MVC   P+1(12),OUTAKY+3                                                 
         MVC   P+13(1),OUTFLAG                                                  
         MVC   P+14(5),OUTFLT                                                   
         MVC   P+20(36),OUTNAM                                                  
         LH    R5,OUTLEN                                                        
         SH    R5,=H'78'           R5=L'PROFILE INFO                            
         SR    R4,R4                                                            
         D     R4,=F'50'                                                        
         LTR   R5,R5                                                            
         BNZ   *+12                                                             
         BAS   RE,ACPREPT                                                       
         B     AC33A                                                            
         LA    R4,OUTPRO                                                        
*                                                                               
AC33     MVC   P+57(50),0(R4)                                                   
         BAS   RE,ACPREPT                                                       
         LA    R4,50(R4)                                                        
         BCT   R5,AC33                                                          
*                                                                               
AC33A    ZIC   R5,MYSKIP           SKIP AFTER IF PROFILES                       
         LTR   R5,R5                                                            
         BZ    AC33B                                                            
         BAS   RE,ACPREPT                                                       
         BCT   R5,*-4                                                           
         SPACE 1                                                                
AC33B    CLI   INSWITCH,C'Y'                                                    
         BE    AC26                                                             
         B     EXIT                                                             
*                                                                               
AC34     DS    0H                  LEDGER BREAK                                 
         MVC   PAGE,=H'1'                                                       
         MVC   SAVECMP,OUTCMP                                                   
         MVC   SAVEUNT,OUTUNT                                                   
         MVC   SAVELDG,OUTLDG                                                   
         CLI   INSWITCH,C'Y'                                                    
         BE    AC26                                                             
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINTING ROUTINES                                                      
*---------------------------------------------------------------------*         
ACPRINT  NTR1                                                                   
         CLI   QOPT2,C'P'          IGNORE IF PROFILE OPTION                     
         BE    ACPRIN4                                                          
         CLI   QOPT6,C'Y'          OR ADDRESS OPTION                            
         BE    ACPRIN4                                                          
         CLI   PLINES,1                                                         
         BE    XIT                                                              
         L     R6,ABLOCK           R6=A(PRINT BUFFER)                           
         LH    R5,PERPAGE          NUMBER OF LINES PER PAGE                     
         MVI   RCSUBPRG,0                                                       
*                                                                               
ACPRIN2  MVC   P+1(110),0(R6)                                                   
         BAS   RE,ACPREPT                                                       
         LA    R6,110(R6)                                                       
         BCT   R5,ACPRIN2                                                       
*                                                                               
ACPRIN4  DS    0H                                                               
         BAS   RE,ACINTBUF         CLEAR PRINT BUFFER                           
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
*                                                                               
ACPREPT  NTR1                                                                   
         MVC   HEAD4+10(38),SAVECMP                                             
         MVI   HEAD4+10,C' '       NO COMPANY CODE                              
         MVC   HEAD5+10(38),SAVEUNT                                             
         MVC   HEAD6+10(38),SAVELDG                                             
         MVC   SPACING,MYSPACE                                                  
*                                                                               
         CLI   QXJOB,C'Y'                                                       
         BNE   *+10                                                             
         MVC   HEAD6+43(L'XJINC),XJINC                                          
         CLI   QXJOB,C'O'                                                       
         BNE   *+10                                                             
         MVC   HEAD6+47(L'XJONLY),XJONLY                                        
*                                                                               
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        INITIALIZE PRINT BUFFER                                                
*---------------------------------------------------------------------*         
ACINTBUF NTR1                                                                   
         MVI   PLINES,1                                                         
         L     R6,ABLOCK                                                        
         LH    R5,=H'50'                                                        
*                                                                               
ACINTBU2 MVC   0(110,R6),SPACES                                                 
         LA    R6,110(R6)                                                       
         BCT   R5,ACINTBU2                                                      
         B     XIT                                                              
*              INITIALIZE SORT RECORD                                           
*                                                                               
ACINTREC NTR1                                                                   
         XC    OUTLEN(8),OUTLEN                                                 
         MVC   OUTRQN,RCRQTOT                                                   
         MVC   OUTSKY(200),SPACES                                               
         MVI   LCNT,0                                                           
         MVI   LENG,0                                                           
         LA    R4,OUTPRO                                                        
         LA    R5,6                                                             
         MVC   0(100,R4),SPACES                                                 
         LA    R4,100(R4)                                                       
         BCT   R5,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ROUTINE TO EXTRACT NAME                                                
*        R2=A(ELEMENT),R5=A(OUTPUT)                                             
*---------------------------------------------------------------------*         
ACEXNAM  DS    0H                                                               
         MVC   0(36,R5),SPACES                                                  
         ZIC   RF,1(R2)                                                         
         SH    RF,=H'3'                                                         
         BMR   RE                                                               
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R5),2(R2)                                                    
*                                                                               
*---------------------------------------------------------------------*         
*        ROUTINE TO EXTRACT XJOB STATUS                                         
*---------------------------------------------------------------------*         
EXTXJOB  DS    0H                                                               
         USING ACJOBD,R2                                                        
         TM    ACJBSTAT,ACJBXJOB                                                
         BZR   RE                                                               
         MVI   XJOB,C'Y'                                                        
         BR    RE                                                               
         DROP  R2                                                               
*                                                                               
*              HANDLE G/L POSTING INSTRUCTION ELEMENTS                          
*                                                                               
ACPGLA   NTR1                                                                   
         USING ACGENLD,R2                                                       
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(8),=C'GENERAL='                                            
         MVC   PWORK+8(10),ACGLACC                                              
         CLI   ACGLLEN,26                                                       
         BL    *+10                                                             
         MVC   PWORK+8(14),ACGLACC                                              
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE NUMBER ELEMENT                                                  
*---------------------------------------------------------------------*         
ACPNUM   NTR1                                                                   
         USING ACNUMD,R2                                                        
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'MEDIA'                                               
         MVC   PWORK+6(1),ACNUMTYP                                              
         LA    R5,PWORK+8                                                       
         CLI   ACNUMLEN,15                                                      
         BE    *+14                                                             
         MVC   PWORK+6(3),=C'ALL'                                               
         LA    R5,2(R5)                                                         
         MVC   0(12,R5),=C'BILL NUMBER='                                        
         LA    R5,12(R5)                                                        
         MVC   0(6,R5),ACNUMAFT                                                 
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE OTHERS ELEMENT                                                  
*---------------------------------------------------------------------*         
ACPOTH   NTR1                                                                   
         USING ACOTHERD,R2                                                      
         CLI   VEHICLE,C'Y'                                                     
         BE    ACPOTHV                                                          
         CLC   ACOTNUM,SPACES                                                   
         BE    ACPOT2                                                           
         MVC   PWORK,SPACES                                                     
         MVC   PWORK+7(9),ACOTNUM                                               
         MVC   PWORK(7),=C'NUMBER='                                             
         CLC   QUNIT(2),=C'2C'                                                  
         BE    *+14                                                             
         CLC   QUNIT(2),=C'SV'                                                  
         BNE   ACPOT1              IF NOT 2C OR SV                              
         CLI   ACOTPROF,C'I'                                                    
         BNE   *+10                BRANCH IF NOT ID=                            
         MVC   PWORK(7),=C'TAX ID='                                             
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         SPACE                                                                  
ACPOT1   BAS   RE,ADDPRO                                                        
*                                                                               
ACPOT2   CLC   ACOTPROF,SPACES                                                  
         BE    XIT                                                              
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(8),=C'PROFILE='                                            
         MVC   PWORK+8(4),ACOTPROF                                              
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
*                                                                               
ACPOTHV  MVC   PWORK,SPACES                                                     
         MVC   PWORK(4),=C'ACN='                                                
         MVC   PWORK+4(5),ACOTNUM                                               
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE PROFILE ELEMENT                                                 
*---------------------------------------------------------------------*         
ACPPRO   NTR1                                                                   
         USING ACPROFD,R2                                                       
         OC    ACPRRECV,ACPRRECV                                                
         BZ    ACPPR2                                                           
         CLC   ACPRRECV,SPACES                                                  
         BE    ACPPR2                                                           
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'RECV='                                               
         MVC   PWORK+5(12),ACPRRECV+3                                           
         BAS   RE,ADDPRO                                                        
*                                                                               
ACPPR2   OC    ACPRCOST,ACPRCOST                                                
         BZ    ACPPR4                                                           
         CLC   ACPRCOST,SPACES                                                  
         BE    ACPPR4                                                           
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'COST='                                               
         MVC   PWORK+5(12),ACPRCOST+3                                           
         BAS   RE,ADDPRO                                                        
*                                                                               
ACPPR4   OC    ACPRBILL,ACPRBILL                                                
         BZ    ACPPRA                                                           
         CLC   ACPRBILL,SPACES                                                  
         BE    ACPPRA                                                           
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(10),=C'BILL TYPE='                                         
         MVC   PWORK+10(1),ACPRBILL                                             
         CLI   ACPRBILL,C'S'                                                    
         BNE   ACPPR6                                                           
         MVI   PWORK+11,C'-'                                                    
         EDIT  (B4,ACPRBLAM),(10,PWORK+12),2,ALIGN=LEFT                         
         B     ACPPR8                                                           
*                                                                               
ACPPR6   CLI   ACPRBILL,C'E'                                                    
         BNE   ACPPR8                                                           
         MVI   PWORK+11,C'-'                                                    
         EDIT  (B4,ACPRBLAM),(10,PWORK+12),2,ALIGN=LEFT                         
         LA    R6,PWORK+11                                                      
         CLI   0(R6),C'.'                                                       
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         MVC   0(8,R6),=C' PCT EST'                                             
*                                                                               
ACPPR8   BAS   RE,ADDPRO                                                        
*                                                                               
ACPPRA   CLC   ACPROFFC,SPACES                                                  
         BNH   XIT                                                              
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(7),=C'OFFICE='                                             
         MVC   PWORK+7(L'ACPROFFC),ACPROFFC                                     
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE EXTRA NUMBER ELEMENT                                            
*---------------------------------------------------------------------*         
ACPNM2   NTR1                                                                   
         USING ACNOD,R2                                                         
         MVC   PWORK,SPACES                                                     
         CLI   VEHICLE,C'Y'        IS IT COKE MEDIA/VEHICLE                     
         BE    ACPNM2V                                                          
         MVC   PWORK(13),=C'EXTRA NUMBER='                                      
         ZIC   RF,ACNOLEN                                                       
         SH    RF,=H'3'                                                         
         BM    ACPNM3                                                           
         EXMVC RF,PWORK+13,ACNO                                                 
ACPNM3   BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         SPACE 1                                                                
ACPNM2V  MVC   PWORK(8),=C'VEHICLE='                                            
         MVC   PWORK+8(5),ACNO+3                                                
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE ACCOUNT BILLING INFORMATION ELEMENT                             
*---------------------------------------------------------------------*         
ACPBIL   NTR1                                                                   
         USING ACABILLD,R2                                                      
         MVC   PWORK,SPACES                                                     
         CLC   ACABEANO,SPACES                                                  
         BE    ACPBIL2                                                          
         MVC   PWORK(3),=C'EA='                                                 
         MVC   PWORK+3(L'ACABEANO),ACABEANO                                     
         BAS   RE,ADDPRO                                                        
         SPACE 1                                                                
ACPBIL2  CLC   ACABACNO,SPACES                                                  
         BE    ACPBIL4                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(3),=C'AC='                                                 
         MVC   PWORK+3(L'ACABACNO),ACABACNO                                     
         BAS   RE,ADDPRO                                                        
         SPACE 1                                                                
ACPBIL4  CLI   ACABLEN,X'39'       SOMETIMES HAVE SMALL ELEMENTS                
         BL    ACPBILX                                                          
         CLC   ACABESNO,SPACES                                                  
         BE    ACPBIL6                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(4),=C'ENO='                                                
         MVC   PWORK+4(L'ACABESNO),ACABESNO                                     
         BAS   RE,ADDPRO                                                        
         SPACE 1                                                                
ACPBIL6  CLC   ACABBUNO,SPACES                                                  
         BE    ACPBIL7                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(4),=C'BUD='                                                
         MVC   PWORK+4(L'ACABBUNO),ACABBUNO                                     
         BAS   RE,ADDPRO                                                        
*       - - - - - - - - - - - - -                                               
ACPBIL7  CLI   ACABLEN,X'39'                                                    
         BNH   ACPBILX             NO,  BRANCH OUT                              
         CLC   ACABBINO,SPACES                                                  
         BE    ACPBIL8                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(2),=C'BN='                                                 
         MVC   PWORK+3(L'ACABBINO),ACABBINO                                     
         BAS   RE,ADDPRO                                                        
*       - - - - - - - - - - - - -                                               
ACPBIL8  CLI   ACABLEN,X'48'                                                    
         BNH   ACPBILX             NO,  BRANCH TO SKIP DATA                     
         CLC   ACABBMEM,SPACES                                                  
         BE    ACPBILX                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(2),=C'BG='                                                 
         MVC   PWORK+3(L'ACABBMEM),ACABBMEM                                     
         BAS   RE,ADDPRO                                                        
*   - - - - - - - - - - - - - - -                                               
ACPBILX  B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE RETAIL BILLING INFORMATION ELEMENT                              
*---------------------------------------------------------------------*         
ACPRET   NTR1                                                                   
         USING ACRBD,R2                                                         
         OC    ACRBRECV,ACRBRECV                                                
         BZ    ACPRET2                                                          
         CLC   ACRBRECV,SPACES                                                  
         BE    ACPRET2                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'RECV='                                               
         MVC   PWORK+5(12),ACRBRECV+2                                           
**NOP    MVC   PWORK+7(12),ACRBRECV+2                                           
         BAS   RE,ADDPRO                                                        
*       - - - - - - - - - - - -                                                 
ACPRET2  OC    ACRBCOST,ACRBCOST                                                
         BZ    ACPRETX                                                          
         CLC   ACRBCOST,SPACES                                                  
         BE    ACPRETX                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'COST='                                               
         MVC   PWORK+5(12),ACRBCOST+2                                           
         BAS   RE,ADDPRO                                                        
*                                                                               
ACPRETX  B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        HANDLE COMMENT (BIL AND B+E) INFORMATION ELEMENT                       
*-----------------------------------------------------------AWIL-------         
ACCOMM   NTR1                                                                   
         USING ACOMMD,R2                                                        
         CLI   ACOMTYPE,0          ONLY SHOW STANDARD COMMENTS                  
         BE    ACCOMMX             YES, BRANCH TO SKIP DATA                     
         MVC   PWORK,SPACES                                                     
         TM    ACOMTYPE,X'40'                                                   
         BO    *+10                                                             
         MVC   PWORK(4),=C'BIL='                                                
         TM    ACOMTYPE,X'C0'                                                   
         BM    *+10                                                             
         MVC   PWORK(4),=C'B+E='                                                
*                                                                               
         MVC   WORK(6),ACOMMENT                                                 
         LA    RF,5                                                             
ACCOM05  CLI   WORK,C' '                                                        
         BNE   ACCOM10                                                          
         MVC   WORK(5),WORK+1                                                   
         MVI   WORK+5,C' '                                                      
         BCT   RF,ACCOM05                                                       
ACCOM10  EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PWORK+4(0),WORK                                                  
*                                                                               
         BAS   RE,ADDPRO                                                        
ACCOMMX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE GENERAL DATE INFORMATION ELEMENT                                
*---------------------------------------------------------AWIL--------*         
ACGDA    NTR1                                                                   
         USING GDAELD,R2                                                        
         MVC   PWORK,SPACES                                                     
         CLI   GDATYPE,GDATRTLS    RETAIL EFFECTIVE START/END DATE              
         BE    ACGDA02             NO,  BRANCH TO SKIP DATA                     
         CLI   GDATYPE,GDATRTLE    RETAIL EFFECTIVE START/END DATE              
         BNE   ACGDAX              NO,  BRANCH TO SKIP DATA                     
         MVC   PWORK(7),=C'EFFEND='                                             
         GOTO1 DATCON,DMCB,(1,GDADATE),(6,PWORK+7)                              
         B     ACGDAX                                                           
ACGDA02  MVC   PWORK(9),=C'EFFSTART='                                           
         GOTO1 DATCON,DMCB,(1,GDADATE),(6,PWORK+9)                              
ACGDAX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE SPECIAL POSTING INFORMATION                                     
*---------------------------------------------------------------------*         
ACSPCL   NTR1                                                                   
         USING ACSPECD,R2                                                       
         MVC   PWORK,SPACES                                                     
         CLI   ACSPTYP,0                                                        
         BE    XIT                                                              
         LA    RF,0                                                             
         CLI   ACSPTYP,ACSPOIN                                                  
         BNE   *+8                                                              
         LA    RF,MOVE1                                                         
         CLI   ACSPTYP,ACSPOWO                                                  
         BNE   *+8                                                              
         LA    RF,MOVE2                                                         
         CLI   ACSPTYP,ACSPOAN                                                  
         BNE   *+8                                                              
         LA    RF,MOVE3                                                         
         CLI   ACSPTYP,ACSPOCD                                                  
         BNE   *+8                                                              
         LA    RF,MOVE4                                                         
         CLI   ACSPTYP,ACSPOMC                                                  
         BNE   *+8                                                              
         LA    RF,MOVE5                                                         
*                                                                               
         LTR   RF,RF                                                            
         BZ    XIT                                                              
*                                                                               
         EX    0,0(RF)                                                          
         SR    R1,R1                                                            
         IC    R1,1(RF)                                                         
*                                                                               
         LA    RF,PWORK+1(R1)                                                   
         MVC   0(L'ACSPACCT,RF),ACSPACCT                                        
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
*                                                                               
MOVE1    MVC   PWORK(7),=C'INCOME='                                             
MOVE2    MVC   PWORK(10),=C'WRITE-OFF='                                         
MOVE3    MVC   PWORK(9),=C'ANALYSIS='                                           
MOVE4    MVC   PWORK(14),=C'CASH DISCOUNT='                                     
MOVE5    MVC   PWORK(14),=C'MEDIA CONTROL='                                     
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE JOB ELEMENT                                                     
*---------------------------------------------------------------------*         
ACPJOB   NTR1                                                                   
         USING ACJOBD,R2                                                        
         CLI   ACJBLEN,16          IF BIG ELEMENT                               
         BL    ACPJOB2                                                          
         OC    ACJBOPND,ACJBOPND   AND THERE'S AN OPEN DATE                     
         BZ    ACPJOB2                                                          
         MVC   PWORK,SPACES        PRINT IT FIRST                               
         MVC   PWORK(14),=C'JOB OPEN DATE='                                     
         GOTO1 DATCON,DMCB,(1,ACJBOPND),(8,PWORK+14)                            
         BAS   RE,ADDPRO                                                        
         SPACE 1                                                                
ACPJOB2  MVC   PWORK,SPACES        NOW PRINT JOB CLOSE DATE                     
         MVC   PWORK(15),=C'JOB CLOSE DATE='                                    
         GOTO1 DATCON,DMCB,(1,ACJBCLOS),(8,PWORK+15)                            
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE STATUS ELEMENT                                                  
*---------------------------------------------------------------------*         
ACPSTA   NTR1                                                                   
         USING ACSTATD,R2                                                       
*     - - - - - - - - - - - - - - - - - - - *AWIL*                              
         TM    ACSTSTAT,X'80'                                                   
         BZ    ACPST02                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(7),=C'STAFF=Y'                                             
         BAS   RE,ADDPRO                                                        
ACPST02  DS    0H                                                               
*     - - - - - - - - - - - - - - - - - - -                                     
         L     RF,ADACC                                                         
         CLC   1(2,RF),=C'SJ'                                                   
         BE    ACPST08                                                          
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTAT,X'40'       IS ACCOUNT CLOSED?                          
         BZ    ACPST04              NO                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(8),=C'CLOSED=Y'                                            
         BAS   RE,ADDPRO                                                        
ACPST04  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTAT,X'20'       IS ACCOUNT LOCKED OUT?                      
         BZ    ACPST06              NO                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(8),=C'LOCKED=Y'                                            
         BAS   RE,ADDPRO                                                        
ACPST06  DS    0H                                                               
         B     ACPST11                                                          
*       - - - - - - - - - - - - -                                               
ACPST08  TM    ACSTSTAT,X'60'                                                   
         BZ    ACPST11                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(10),=C'STATUS=C/L'                                         
         BO    ACPST10                                                          
         MVC   PWORK+7(3),=C'C  '                                               
         TM    ACSTSTAT,X'40'                                                   
         BO    ACPST10                                                          
         MVI   PWORK+7,C'L'                                                     
ACPST10  BAS   RE,ADDPRO                                                        
ACPST11  DS    0H                                                               
*     - - - - - - - - - - - - - - - - - - -                                     
         TM    ACSTSTAT,X'10'                                                   
         BZ    ACPST14                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(6),=C'DEPT=Y'                                              
         BAS   RE,ADDPRO                                                        
ACPST14  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTAT,X'08'       IS OUTPUT FILE FOR BANK RECON?              
         BZ    ACPST16              NO                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(9),=C'OUTFILE=Y'                                           
         BAS   RE,ADDPRO                                                        
ACPST16  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTAT,X'04'                                                   
         BZ    ACPST16A                                                         
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(6),=C'EXEC=Y'                                              
         BAS   RE,ADDPRO                                                        
ACPST16A DS    0H                                                               
*       - - - - - - - - - - - - -                                               
*&&US                                                                           
         TM    ACSTSTAT,X'02'                                                   
         BZ    ACPST19                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(8),=C'VEND2C=Y'                                            
*&&                                                                             
*&&UK                                                                           
         L     RF,ADACC                                                         
         CLC   1(2,RF),=C'SG'      UK VAT                                       
         BNE   ACPST19                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(8),=C'VATTYP=O'                                            
         TM    ACSTSTAT,X'02'                                                   
         BZ    ACPST18                                                          
         MVI   PWORK+7,C'I'                                                     
*&&                                                                             
ACPST18  BAS   RE,ADDPRO                                                        
ACPST19  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTAT,X'01'      IS SALARY FIGURE ACTUAL AMT?                 
         BZ    ACPST22             NO                                           
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(8),=C'ACTUAL=Y'  DISPLAY ACTUAL                            
         BAS   RE,ADDPRO                                                        
ACPST22  DS    0H                                                               
*     - - - - - - - - - - - - - - - - - - - -                                   
         CLI   ACSTLEN,ACSTLNQ1                                                 
         BNH   ACPST50                                                          
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTA2,X'20'                                                   
         BZ    ACPST24                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(6),=C'PAY=NO'    DISPLAY PAY                               
         BAS   RE,ADDPRO                                                        
ACPST24  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTA2,X'08'       IS DUPLICATE CHECKING DISABLED              
         BZ    ACPST26              NO                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(7),=C'CKDUP=N'   DISPLAY CKDUP                             
         BAS   RE,ADDPRO                                                        
ACPST26  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         CLI   ACSTLEN,ACSTLNQ3                                                 
         BL    ACPST50                                                          
*       - - - - - - - - - - - - -                                               
         MVC   PWORK,SPACES                                                     
         TM    ACST1099,ACSTRENT                                                
         BZ    AC1099A                                                          
         MVC   PWORK+5(3),=C'RNT'                                               
         B     AC1099D                                                          
AC1099A  TM    ACST1099,ACSTPRIZ                                                
         BZ    AC1099B                                                          
         MVC   PWORK+5(3),=C'PRZ'                                               
         B     AC1099D                                                          
AC1099B  TM    ACST1099,ACSTROYL                                                
         BZ    AC1099C                                                          
         MVC   PWORK+5(3),=C'ROY'                                               
         B     AC1099D                                                          
AC1099C  TM    ACST1099,ACSTMEDC                                                
         BZ    ACPST28                                                          
         MVC   PWORK+5(3),=C'MED'                                               
AC1099D  MVC   PWORK(5),=C'1099='         DISPLAY 1099                          
         BAS   RE,ADDPRO                                                        
ACPST28  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTA5,ACSTHOUS                                                
         BZ    ACPST30                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(7),=C'HOUSE=Y'       DISPLAY HOUSE                         
         BAS   RE,ADDPRO                                                        
ACPST30  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         MVC   PWORK,SPACES                                                     
         TM    ACSTSTA5,ACSTPROD                                                
         BZ    *+12                                                             
         MVI   PWORK+4,C'P'                                                     
         B     ACPST32                                                          
         TM    ACSTSTA5,ACSTPRJB                                                
         BZ    ACPST34                                                          
         MVI   PWORK+4,C'J'                                                     
ACPST32  MVC   PWORK(4),=C'T/S='        DISPLAY T/S                             
         BAS   RE,ADDPRO                                                        
ACPST34  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         CLC   ACSTDTSK,SPACES                                                  
         BNH   ACPST36                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'TASK='                                               
         MVC   PWORK+5(2),ACSTDTSK    DISPLAY TASK CODE                         
         BAS   RE,ADDPRO                                                        
ACPST36  DS    0H                                                               
*     - - - - - - - - - - - - - - - - - - - -                                   
ACPST50  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         CLI   ACSTCOST,C' '                                                    
         BE    ACPST52                                                          
         CLI   ACSTCOST,0                                                       
         BE    ACPST52                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(9),=C'ANALYSIS='                                           
         MVC   PWORK+9(1),ACSTCOST    DISPLAY ANALYSIS                          
         BAS   RE,ADDPRO                                                        
ACPST52  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         OC    ACSTCNTR,SPACES     ANY CC= CODES                                
         CLC   ACSTCNTR,SPACES                                                  
         BE    ACPST56                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(2),=C'CC'                                                  
         LA    RE,PWORK+2                                                       
         CLI   ACSTCPOS,0          ANY CCN= VALUE                               
         BE    ACPST54                                                          
         MVC   PWORK+2(1),ACSTCPOS                                              
         OI    PWORK+2,X'F0'                                                    
         LA    RE,1(RE)                                                         
ACPST54  MVI   0(RE),C'='                                                       
         MVC   1(3,RE),ACSTCNTR                                                 
         BAS   RE,ADDPRO                                                        
ACPST56  DS    0H                                                               
*     - - - - - - - - - - - - - - - - - - -                                     
         TM    ACSTSTX,X'80'       POSTING IS VENDOR?                           
         BZ    ACPST58             YES                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(8),=C'VEND29=Y'                                            
         BAS   RE,ADDPRO                                                        
ACPST58  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         L     RF,ADACC                                                         
         CLC   1(2,RF),=C'SC'                                                   
         BNE   ACPST62                                                          
         TM    ACSTSTX,X'40'                                                    
         BZ    ACPST60                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(7),=C'RECCR=Y'                                             
         BAS   RE,ADDPRO                                                        
*                                                                               
ACPST60  TM    ACSTSTX,X'20'                                                    
         BZ    ACPST62                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(7),=C'RECDR=Y'                                             
         BAS   RE,ADDPRO                                                        
ACPST62  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTX,X'10'       PROJECT REQ. ON TIME SHEET                   
         BZ    ACPST64                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(4),=C'PC=Y'                                                
         BAS   RE,ADDPRO                                                        
ACPST64  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTX,X'08'                                                    
         BZ    ACPST66                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'IND=Y'                                               
         BAS   RE,ADDPRO                                                        
ACPST66  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTX,X'04'       IS LEDGER ACCT P/L?                          
         BZ    ACPST68             NO                                           
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'P/L=Y'                                               
         BAS   RE,ADDPRO                                                        
ACPST68  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         TM    ACSTSTX,X'02'       IS LEDGER ACCT B/S?                          
         BZ    ACPST70             NO                                           
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'BAL=Y'                                               
         BAS   RE,ADDPRO                                                        
ACPST70  DS    0H                                                               
*       - - - - - - - - - - - - - -                                             
         TM    ACSTSTX,X'01'       IS COSTING BY JOB?                           
         BZ    ACPST72             NO                                           
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(7),=C'JCOST=Y'                                             
         BAS   RE,ADDPRO                                                        
ACPST72  DS    0H                                                               
*     - - - - - - - - - - - - - - - - - - - -                                   
         CLI   ACSTLEN,ACSTLNQ2    OLD ELEMENT                                  
         BL    ACPST80                                                          
*       - - - - - - - - - - - - - -                                             
         CLI   ACSTOFFC,C' '       G/L DEFAULT OFFICE                           
         BNH   ACPST74                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(6),=C'GLOFF='                                              
         MVC   PWORK+6(2),ACSTOFFC                                              
         BAS   RE,ADDPRO                                                        
ACPST74  DS    0H                                                               
*     - - - - - - - - - - - - - - - - - - -                                     
ACPST80  MVC   PWORK,SPACES                                                     
         MVC   PWORK(14),=C'LAST ACTIVITY='                                     
         GOTO1 DATCON,DMCB,(1,ACSTLAST),(5,PWORK+14)                            
         BAS   RE,ADDPRO                                                        
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
         USING RSTELD,R2                                                        
*       - - - - - - - - - - - - -                                               
         L     RF,ADACC                                                         
         CLC   1(2,RF),=C'SI'      SI ONLY                                      
         BNE   ACPST82                                                          
         OC    RSTSYSME,RSTSYSME   SYSTEM MEDIA?                                
         BZ    ACPST82             NONE                                         
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(4),=C'SMC='                                                
         MVC   PWORK+4(2),RSTSYSME   SHOW IT                                    
         BAS   RE,ADDPRO                                                        
ACPST82  DS    0H                                                               
*       - - - - - - - - - - - - -                                               
         CLI   RSTLN,RSTLN1Q       SKIP ALL BELOW IF WE DON'T HAVE THEM         
         BNH   ACPST94                                                          
         TM    RSTSTAT2,RSTSYCST+RSTSNCST                                       
         BZ    ACPST84                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(6),=C'COST=Y'                                              
         TM    RSTSTAT2,RSTSYCST                                                
         BO    *+8                                                              
         MVI   PWORK+5,C'N'                                                     
         BAS   RE,ADDPRO                                                        
*                                                                               
ACPST84  CLI   RSTLN,RSTLN2Q       SKIP BELOW IF WE DON'T HAVE THEM             
         BNH   ACPST94                                                          
         TM    RSTSTAT5,RSTSNBIZ                                                
         BNO   ACPST86                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(6),=C'NEWBIZ'                                              
         BAS   RE,ADDPRO                                                        
*                                                                               
ACPST86  TM    RSTSTAT5,RSTSBONO                                                
         BNO   ACPST88                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(7),=C'PROBONO'                                             
         BAS   RE,ADDPRO                                                        
*                                                                               
ACPST88  TM    RSTSTAT5,RSTSPRJB                                                
         BNO   ACPST90                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(14),=C'PROD/JOB FORCE'                                     
         BAS   RE,ADDPRO                                                        
*                                                                               
ACPST90  TM    RSTSTAT5,RSTSPROD                                                
         BNO   ACPST92                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(10),=C'PROD FORCE'                                         
         BAS   RE,ADDPRO                                                        
*                                                                               
ACPST92  TM    RSTSTAT5,RSTSPROV                                                
         BNO   ACPST94                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(11),=C'PROVISIONAL'                                        
         BAS   RE,ADDPRO                                                        
*                                                                               
ACPST94  DS    0H                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE VAT ELEMENT/DISCOUNT ELEMENT                                    
*---------------------------------------------------------------------*         
ACPVAT   NTR1                                                                   
         USING ACVATD,R2                                                        
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(9),=C'VAT RATE='                                           
         EDIT  (B2,ACVTRATE),(6,PWORK+9),2,ALIGN=LEFT                           
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
*                                                                               
ACPDSC   NTR1                                                                   
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(12),=C'DISCNT RATE='                                       
         EDIT  (B2,ACVTRATE),(6,PWORK+12),2,ALIGN=LEFT                          
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE COKE AREA/REGION/DISTRICT                                       
*---------------------------------------------------------------------*         
ACPARA   NTR1                                                                   
         USING ACEXPD,R2                                                        
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'AREA='                                               
         MVC   PWORK+5(12),ACEXPACC+3                                           
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE EXTRA PROFILE ELEMENT                                           
*---------------------------------------------------------------------*         
ACPXPR   NTR1                                                                   
         USING ACXPROFD,R2                                                      
         MVC   PWORK,SPACES                                                     
         CP    ACXPDUE,=P'10'                                                   
         BE    ACPXPR2                                                          
         MVC   PWORK(9),=C'DUE DAYS='                                           
         EDIT  (P2,ACXPDUE),(3,PWORK+9),ALIGN=LEFT                              
         BAS   RE,ADDPRO                                                        
ACPXPR2  MVC   PWORK,SPACES                                                     
         CP    ACXPOVER,=P'10000'                                               
         BE    ACPXPR4                                                          
         MVC   PWORK(5),=C'OVER='                                               
         EDIT  (P3,ACXPOVER),(6,PWORK+5),2,ALIGN=LEFT                           
         BAS   RE,ADDPRO                                                        
ACPXPR4  MVC   PWORK,SPACES                                                     
         CP    ACXPLOW,=P'5000'                                                 
         BE    ACPXPR6                                                          
         MVC   PWORK(6),=C'UNDER='                                              
         EDIT  (P4,ACXPLOW),(8,PWORK+6),2,ALIGN=LEFT                            
         BAS   RE,ADDPRO                                                        
ACPXPR6  MVC   PWORK,SPACES                                                     
         MVC   PWORK(11),=C'SUMMARY=YES'                                        
         CLI   ACXPSUM,C'N'                                                     
         BNE   *+10                                                             
         MVC   PWORK+8(3),=C'NO '                                               
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         CLI   ACXPNET,C'Y'                                                     
         BNE   *+14                                                             
         MVC   PWORK(7),=C'PAY=NET'                                             
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         CLI   ACXPDET,C'Y'                                                     
         BE    *+14                                                             
         MVC   PWORK(9),=C'DETAIL=NO'                                           
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         CLI   ACXPEDET,C'N'                                                    
         BE    *+14                                                             
         MVC   PWORK(16),=C'ESTDETAIL=YES'                                      
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         CLI   ACXPCD,C'N'                                                      
         BNE   ACPXPR8                                                          
         MVC   PWORK(7),=C'DISC=NO'                                             
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         CLI   ACXPLEN,18                                                       
         BL    XIT                                                              
ACPXPR8  CLI   ACXPEST,C'N'                                                     
         BNE   *+14                                                             
         MVC   PWORK(11),=C'ESTIMATE=NO'                                        
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         TM    ACXPST1,X'80'                                                    
         BZ    *+14                                                             
         MVC   PWORK(6),=C'ETA=NO'                                              
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         TM    ACXPST1,X'40'                                                    
         BZ    *+14                                                             
         MVC   PWORK(8),=C'ECOMM=NO'                                            
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         TM    ACXPST1,X'20'                                                    
         BZ    *+14                                                             
         MVC   PWORK(11),=C'TRANSFER=NO'                                        
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         TM    ACXPST1,X'10'                                                    
         BZ    *+14                                                             
         MVC   PWORK(13),=C'PRODUCTION=NO'                                      
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         TM    ACXPST1,X'08'                                                    
         BZ    *+14                                                             
         MVC   PWORK(13),=C'POST GROSS+CD'                                      
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         TM    ACXPST1,X'04'                                                    
         BZ    *+14                                                             
         MVC   PWORK(14),=C'EST=UNAPPROVED'                                     
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         TM    ACXPST1,X'02'                                                    
         BZ    *+14                                                             
         MVC   PWORK(15),=C'JOBS=UNAPPROVED'                                    
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         TM    ACXPST2,X'80'                                                    
         BZ    *+14                                                             
         MVC   PWORK(10),=C'UNHOLD=YES'                                         
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
         CLI   ACXPREP,0                                                        
         BE    DG1                                                              
         MVC   PWORK(6),=C'FILT2='                                              
         MVC   PWORK+6(1),ACXPREP                                               
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
DG1      CLI   ACXPBILL,0                                                       
         BE    DG2                                                              
         MVC   PWORK(5),=C'%EST='                                               
         MVC   PWORK+5(1),ACXPBILL                                              
         BAS   RE,ADDPRO                                                        
         MVC   PWORK,SPACES                                                     
DG2      B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        HANDLE ADDRESS ELEMENT                                                 
*---------------------------------------------------------------------*         
ADDADD   NTR1                                                                   
         USING ACADDD,R2                                                        
         SR    R3,R3                                                            
         IC    R3,ACADLNES                                                      
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         CH    R3,=H'5'                                                         
         BNH   *+6                                                              
         DC    H'0'                BAD RECORD                                   
         LA    R4,ACADADD                                                       
         LA    R5,1                                                             
*                                                                               
ADDADD2  SR    R1,R1                                                            
         IC    R1,LCNT                                                          
         CLI   LENG,0                                                           
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         STC   R1,LCNT                                                          
         MVI   LENG,0                                                           
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(14),=C'ADDRESS LINE ='                                     
         STC   R5,PWORK+12                                                      
         OI    PWORK+12,X'F0'                                                   
         MVC   PWORK+14(26),0(R4)                                               
         CLC   0(26,R4),SPACES                                                  
         BE    ADDADD4                                                          
         BAS   RE,ADDPRO                                                        
*                                                                               
ADDADD4  LA    R4,26(R4)                                                        
         LA    R5,1(R5)                                                         
         BCT   R3,ADDADD2                                                       
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT SALES ANALYSIS ELEMENT                                           
*---------------------------------------------------------------------*         
ACPSAL   NTR1                                                                   
         USING ACSAND,R2                                                        
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(6),=C'SALES='                                              
         MVC   PWORK+6(12),ACSACODE+3                                           
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT FFT ELEM DATA - FAX# OR VENDOR OR GL OR DEPT                     
*---------------------------------------------------------------------*         
ACPFFT   NTR1                                                                   
         USING FFTELD,R2                                                        
         MVC   PWORK,SPACES                                                     
         CLI   FFTTYPE,FFTTPFAX    IS THIS A FAX NUMBER                         
         BNE   ACPFF30             NO                                           
         MVC   PWORK(4),=C'FAX='                                                
         ZIC   R1,FFTDLEN                                                       
         LTR   R1,R1                                                            
         BZ    ACPFF10                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PWORK+4(0),FFTDATA                                               
ACPFF10  BAS   RE,ADDPRO                                                        
         B     ACPFFX                                                           
*                                                                               
ACPFF30  CLI   FFTTYPE,FFTTVEND    COKE VENDOR NUMBER                           
         BNE   ACPFF50             NO                                           
         MVC   PWORK(7),=C'VENDOR='                                             
         ZIC   R1,FFTDLEN                                                       
         SH    R1,=H'1'                                                         
         BM    ACPFFX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PWORK+7(0),FFTDATA                                               
         BAS   RE,ADDPRO                                                        
         B     ACPFFX                                                           
*                                                                               
ACPFF50  CLI   FFTTYPE,FFTTGLNO    COKE GENERAL LEDGER PRODUCT NUMBER           
         BNE   ACPFF60             NO                                           
         MVC   PWORK(3),=C'GL='                                                 
         ZIC   R1,FFTDLEN                                                       
         SH    R1,=H'1'                                                         
         BM    ACPFFX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PWORK+3(0),FFTDATA                                               
         BAS   RE,ADDPRO                                                        
         B     ACPFFX                                                           
*                                                                               
ACPFF60  CLI   FFTTYPE,FFTTCDPT    COKE DEPARTMENT NUMBER                       
         BNE   ACPFFX              NO                                           
         MVC   PWORK(5),=C'DEPT='                                               
         ZIC   R1,FFTDLEN                                                       
         SH    R1,=H'1'                                                         
         BM    ACPFFX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PWORK+5(0),FFTDATA                                               
         BAS   RE,ADDPRO                                                        
ACPFFX   B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT USER FIELDS                                                      
*---------------------------------------------------------------------*         
ACUSER   NTR1                                                                   
         USING UFSELD,R2                                                        
         MVC   PWORK,SPACES                                                     
         CLC   UFSCODE,=C'MK'      MARKET                                       
         BNE   XIT                                                              
         ZIC   R1,UFSLN                                                         
         SH    R1,=Y(UFSLN1Q+1)                                                 
         LTR   R1,R1                                                            
         BM    XIT                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PWORK+4(0),UFSDATA                                               
         MVC   PWORK(4),=C'MKT='                                                
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ADD TO PROFILE LIST                                                    
*---------------------------------------------------------------------*         
ADDPRO   NTR1                                                                   
         LA    R6,PWORK+50                                                      
         LA    R5,50                                                            
ADDPR2   CLI   0(R6),C' '                                                       
         BNE   *+10                                                             
         BCTR  R6,0                                                             
         BCT   R5,ADDPR2                                                        
         LA    R5,PWORK-1                                                       
         SR    R6,R5               R6=L'CHUNK TO BE ADDED                       
         SR    R5,R5                                                            
         IC    R5,LCNT                                                          
         MH    R5,=H'50'                                                        
         LA    R5,OUTPRO(R5)       R5=A(THIS LINE)                              
ADDPR4   SR    R4,R4                                                            
         IC    R4,LENG             R4=L'THIS LINE SO FAR                        
         LA    R3,0(R4,R5)         R3=A(THIS ENTRY)                             
         LA    R2,2(R4,R6)                                                      
         CH    R2,=H'50'                                                        
         BL    ADDPR6                                                           
         MVI   0(R3),C','          IF ENTRY DOESNT FIT ON LINE                  
         IC    R2,LCNT             CHAIN TO NEXT AND BUMP LINE                  
         LA    R2,1(R2)                                                         
         STC   R2,LCNT                                                          
         LA    R5,50(R5)                                                        
         MVI   LENG,0                                                           
         B     ADDPR4                                                           
ADDPR6   CLI   LENG,0              FIRST FOR LINE - DONT CHAIN                  
         BE    ADDPR8                                                           
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         LA    R2,1(R4,R6)                                                      
         B     *+8                                                              
ADDPR8   LA    R2,0(R4,R6)                                                      
         STC   R2,LENG                                                          
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),PWORK       MOVE IN DATA                                 
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        INPUT HIRE AND TERMINATION DATE TO PROFILE BLOCK FOR PRINTING          
*---------------------------------------------------------------------*         
ACPDAT   NTR1                                                                   
         USING ACEMPD,R2                                                        
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(5),=C'HIRE='                                               
         GOTO1 DATCON,DMCB,(1,ACEMPHIR),(5,PWORK+5)                             
         BAS   RE,ADDPRO                       AMEND TO PROFILE BLOCK           
         OC    ACEMPTRM,ACEMPTRM                                                
         BZ    ACPDAT2                                                          
         MVC   PWORK(5),=C'TERM='                                               
         GOTO1 DATCON,DMCB,(1,ACEMPTRM),(5,PWORK+5)                             
         BAS   RE,ADDPRO                       AMEND TO PROFILE BLOCK           
ACPDAT2  EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        PRINT GST/PST INFORMATION                                              
*---------------------------------------------------------------------*         
ACGST    NTR1                                                                   
         USING ITCELD,R2                                                        
         MVC   PWORK,SPACES                                                     
         LA    R5,PWORK                                                         
         OC    ITCPROV,ITCPROV     ANY PROVINCE CODE?                           
         BNZ   ACGST2              YES                                          
         MVC   0(7,R5),=C'GSTCODE'                                              
         LA    R5,7(R5)                                                         
         B     ACGST4                                                           
*                                                                               
ACGST2   MVC   0(2,R5),ITCPROV                                                  
         LA    R5,2(R5)                                                         
*                                                                               
ACGST4   MVI   0(R5),C'='                                                       
         MVC   1(1,R5),ITCTYPE                                                  
         MVI   2(R5),C'-'                                                       
         GOTO1 DATCON,DMCB,(1,ITCEFFD),(17,3(R5))                               
         BAS   RE,ADDPRO                                                        
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        INTERFACE WITH SORTER                                                  
*---------------------------------------------------------------------*         
SETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,(40,ASORTC),RR=PRELOC           
         B     SORTEXIT                                                         
*                                                                               
PUTSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R8),RR=PRELOC                           
         B     SORTEXIT                                                         
*                                                                               
GETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET',RR=PRELOC                                
         B     SORTEXIT                                                         
*&&OS                                                                           
ENDSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'END',RR=PRELOC                                
         B     SORTEXIT                                                         
*&&                                                                             
SORTEXIT XIT1                                                                   
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,40,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(800,,,,)'                             
*                                                                               
XJONLY   DC    C'EXPENSE JOBS ONLY'                                             
XJINC    DC    C'EXPENSE JOBS INCLUDED (*)'                                     
*              EXITS                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        LITERALS                                                               
*---------------------------------------------------------------------*         
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DSECT TO COVER SPACEND                                                 
*---------------------------------------------------------------------*         
WRKD     DSECT                                                                  
ASORTC   DS    A                                                                
ABLOCK   DS    A                                                                
PRELOC   DS    F                                                                
AADDRESS DS    A                   A(CURRENT ADDRESS ELEMENT)                   
SAVECMP  DS    CL38                                                             
SAVEUNT  DS    CL38                                                             
SAVELDG  DS    CL38                                                             
PWORK    DS    CL110                                                            
PLINES   DS    C                                                                
LCNT     DS    C                                                                
LENG     DS    C                                                                
INSWITCH DS    C                                                                
MYSKIP   DS    CL1                                                              
MYSPACE  DS    CL1                                                              
PERPAGE  DS    H                                                                
VEHICLE  DS    CL1                                                              
XJOB     DS    C                   EXPENSE JOB INDICATOR                        
*                                                                               
*---------------------------------------------------------------------*         
*        DSECT TO COVER SORT RECORDS                                            
*---------------------------------------------------------------------*         
OUTD     DSECT                                                                  
OUTLEN   DS    H                   RECORD LENGTH                                
         DS    CL2                                                              
OUTRQN   DS    CL3                 REQUEST NUMBER                               
OUTYPE   DS    CL1                 RECORD TYPE 0=LEDGER BREAK                   
*                                               1=ACCOUNT                       
*                                               2=ACCOUNT WITH PROFILE          
OUTSKY   DS    CL15                SORT KEY OF RECORD                           
OUTAKY   DS    CL15                ACCOUNT KEY                                  
OUTNAM   DS    CL36                       NAME                                  
OUTFLAG  DS    CL1                 XJOB FLAG                                    
OUTFLT   DS    CL5                         FILTER                               
OUTPRO   DS    CL600               PROFILE                                      
         ORG   OUTSKY                                                           
OUTCMP   DS    CL38                COMPANY CODE AND NAME                        
OUTUNT   DS    CL38                UNIT CODE AND NAME                           
OUTLDG   DS    CL38                LEDGER CODE AND NAME                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DSECT FOR PROFILE                                                      
*---------------------------------------------------------------------*         
PROFD    DSECT                                                                  
PROF1    DS    CL1              BLANK LINES BETWEEN ACCOUNTS 0,1,2              
         DS    CL15             SPARE                                           
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        INCLUDED DSECTS                                                        
*---------------------------------------------------------------------*         
* ACGENBOTH                                                                     
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
BLOCK    CSECT                                                                  
         DS    50CL110                                                          
         SPACE 1                                                                
SORTC    CSECT                                                                  
         DS    41000C                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072ACREP7302 03/18/15'                                      
         END                                                                    
