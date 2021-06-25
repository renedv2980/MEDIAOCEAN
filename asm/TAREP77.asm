*          DATA SET TAREP77    AT LEVEL 001 AS OF 08/22/14                      
*PHASE T70377C,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'T70377 - T4A-NR REPORT'                                         
*=====================================================================*         
*  COPY OF TAREP75 FOR NOW.                                           *         
*  NOT NEEDED YET, ONLY T4 AND RL1 WERE APPROVED.                     *         
*=====================================================================*         
T70377   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYEND-MYSTART,T70377,R7                                          
         LR    RA,RC                                                            
         USING MYD,RA                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R6,ATWA                                                          
         USING CONHEADH-64,R6                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALREC                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,PREP                                                          
         B     XIT                                                              
                                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE TALVKSEF                                                       
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 2                                                                
VOPTS    NTR1                                                                   
         ZAP   TRALIMIT,=P'0'                                                   
         ZAP   RECLIMIT,=P'99999999'                                            
         ZAP   REPLIMIT,=P'0'                                                   
         MVI   INOPT,C'D'          DEFAULT INPUT FROM DISK                      
         MVI   CSCOPT,C'N'         DEFAULT NOT FOR CSC                          
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         SPACE 1                                                                
OPT2     CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT4                                                             
         MVI   TRACOPT,C'Y'                                                     
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    OPTEND                                                           
         CVD   R1,DUB                                                           
         ZAP   TRALIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(5,R4),=C'LIMIT'  LIMIT OPTION                                 
         BNE   OPT5                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    FLDINV                                                           
         CVD   R1,DUB                                                           
         ZAP   RECLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT5     CLC   12(6,R4),=C'REPORT' REPORT LIMIT                                 
         BNE   OPT6                                                             
         L     R1,8(R4)            (CHECK NUMERIC)                              
         LTR   R1,R1                                                            
         BZ    FLDINV                                                           
         CVD   R1,DUB                                                           
         ZAP   REPLIMIT,DUB                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(3,R4),=C'SSN'    TESTING OPTION 1 SS#                         
         BNE   OPT7                                                             
         MVC   TIFSSN,22(R4)       (CHECK NUMERIC)                              
         B     OPTEND                                                           
         SPACE 1                                                                
OPT7     CLC   12(3,R4),=C'CSC'    CSC OPTION (ADDREC)                          
         BNE   OPT8                                                             
         MVI   CSCOPT,C'Y'                                                      
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(4,R4),=C'TAPE'   OPTIONAL TAPE INPUT                          
         BNE   FLDINV                                                           
         MVI   INOPT,C'T'                                                       
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 2                                                                
PREP     NTR1                                                                   
         CLI   RECNUM,T4                                                        
         BNE   PREP100                                                          
         BRAS  RE,PRT4                                                          
         B     XIT                                                              
                                                                                
PREP100  CLI   RECNUM,T4B                                                       
         BNE   PREP200                                                          
         BRAS  RE,PRT4B                                                         
         B     XIT                                                              
                                                                                
PREP200  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
*              ODD ROUTINES                                                     
         SPACE 2                                                                
*              ODD ROUTINES                                                     
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              DCB'S, LTORG, ETC                                                
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
                                                                                
ALLLIST  DS    0H                                                               
         DC    C'C',C'PP '                                                      
         DC    C'C',C'P+ '         REPLACED PG WITH P+                          
         DC    C'C',C'TP '                                                      
         DC    X'FF'                                                            
                                                                                
         ENTRY T4TAPE                                                           
         SPACE 1                                                                
T4TAPE   DCB   DDNAME=T4TAPE,DSORG=PS,MACRF=(PM),                      X        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
                                                                                
         ENTRY INTAPE                                                           
         SPACE 1                                                                
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,MACRF=(GM),EODAD=INEOF,          X        
               RECFM=FB,LRECL=449,BUFNO=2,BLKSIZE=0                             
                                                                                
ALLZEROS DC    C'000000000000000'             15 ZEROS                          
ALLZERO2 DC    C'000000000000000000000000000000000000000000000000' 48ZS         
         LTORG                                                                  
         EJECT                                                                  
*              PRINT A LINE                                                     
SPLAT    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*              SORT UTILITIES                                                   
         SPACE 2                                                                
SORTPUT  NTR1  BASE=*,LABEL=*                                                   
         CLI   SORTFRST,C'Y'                                                    
         JNE   SORTPUT2                                                         
         GOTO1 MYSORTER,DMCB,SORTCARD,RECCARD                                   
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
SORTPUT2 DS    0H                                                               
         GOTO1 MYSORTER,DMCB,=C'PUT',INIO                                       
         J     XIT                                                              
                                                                                
         DS    0F                                                               
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,11,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(490)'                                 
                                                                                
         LTORG                                                                  
         EJECT                                                                  
SORTMOVE NTR1  BASE=*,LABEL=*                                                   
         MOVE  (INIO,490),(R2)                                                  
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              ADD AN ELEMENT      R5=A(ELEMENT)                                
ADDEL    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'TALFIL'),TAPEIO,0(R5)                    
         CLI   12(R1),0                                                         
         JE    XIT                                                              
         DC    H'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        T4'S PREP                                                    *         
***********************************************************************         
PRT4     NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTEQU,ACTDOWN                                                   
         JNE   PRT4R000                                                         
*------------------------------*                                                
*  T4 / DOWN                   *                                                
*------------------------------*                                                
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BRAS  RE,INITDOWN         INITIALIZE DOWNLOAD                          
*---------------------------------*                                             
*  READ T4'S BY YEAR / EMP / UNIT *                                             
*---------------------------------*                                             
         BRAS  RE,CHKSYS           SWITCH TO CHKDIR / CHKFIL                    
         MVC   AIO,AIO1                                                         
                                                                                
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING TLT4D,R3                                                         
         MVI   TLT4CD,TLT4CDQ      X'24'                                        
         MVI   TLT4SCD,TLT4SCDQ    X'05'                                        
         MVC   TLT4YEAR,THISYEAR   YEAR                                         
         MVI   TLT4CUR,C'C'        CANADIAN                                     
         MVC   TLT4EMP,TIFEMP                                                   
         GOTO1 HIGH                                                             
         B     PRT4D150                                                         
PRT4D100 GOTO1 SEQ                                                              
                                                                                
PRT4D150 CLC   KEY(TLT4SSN-TLT4D),KEYSAVE     COMPARE KEY UP TO EMP             
         JNE   PRT4D900                                                         
         GOTO1 GETREC                                                           
         MVC   LASTSSN,TLT4SSN                                                  
                                                                                
         MVC   SAVEKEY,KEY                                                      
                                                                                
         BRAS  RE,TALSYS                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',LASTSSN)  READ/SET GLOBAL             
                                                                                
         MVC   NAME1,SPACES                                                     
         MVC   NAME2,SPACES                                                     
                                                                                
         USING TAW4D,R6                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,TAW4ELQ      W4 ELEMENT                                   
         BRAS  RE,GETEL                                                         
         JNE   PRT4D200                                                         
         MVC   NAME1,TAW4NAM1                                                   
         MVC   NAME2,TAW4NAM2                                                   
                                                                                
         USING TAA2D,R6                                                         
PRT4D200 MVC   ADDR1,SPACES                                                     
         MVC   ADDR2,SPACES                                                     
         MVC   CITY,SPACES                                                      
         MVC   PROV,SPACES                                                      
         MVC   ZIP,SPACES                                                       
         MVC   CNTRY,SPACES                                                     
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,TAA2ELQ      ADDRESS ELEMENT                              
         BRAS  RE,GETEL                                                         
         JNE   PRT4D300                                                         
         MVC   ADDR1,TAA2ADD1                                                   
         MVC   ADDR2,TAA2ADD2                                                   
         MVC   CITY,TAA2CITY                                                    
         MVC   PROV,TAA2ST                                                      
         MVC   ZIP,TAA2ZIP                                                      
         MVC   CNTRY,=C'CAN'                                                    
*                                                                               
PRT4D300 MVC   AIO,AIO1                                                         
*                                                                               
         XC    CNFEDTAX,CNFEDTAX                                                
         USING TAT4D,R6                                                         
         MVI   ELCODE,TAT4ELQ                                                   
         GOTO1 GETL,DMCB,(3,=C'CN ')                                            
         BNE   PRT4D320                                                         
         L     R6,TGELEM                                                        
         MVC   CNFEDTAX,TAT4TAX                                                 
                                                                                
PRT4D320 L     R6,AIO                                                           
         MVI   ELCODE,TAT4ELQ                                                   
         BRAS  RE,GETEL                                                         
         J     PRT4D450                                                         
PRT4D400 BRAS  RE,NEXTEL                                                        
PRT4D450 JNE   PRT4D800                                                         
         CLC   TAT4UNIT,=C'CN '                                                 
         JE    PRT4D400                                                         
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',LASTSSN),L'LASTSSN                           
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
         GOTO1 (RF),DMCB,(C'T',NAME2),L'NAME2                                   
         GOTO1 (RF),DMCB,(C'T',NAME1),L'NAME1                                   
         GOTO1 (RF),DMCB,(C'T',ADDR1),L'ADDR1                                   
         GOTO1 (RF),DMCB,(C'T',ADDR2),L'ADDR2                                   
         GOTO1 (RF),DMCB,(C'T',CITY),L'CITY                                     
         GOTO1 (RF),DMCB,(C'T',PROV),L'PROV                                     
         GOTO1 (RF),DMCB,(C'T',ZIP),L'ZIP                                       
         GOTO1 (RF),DMCB,(C'T',CNTRY),L'CNTRY                                   
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
         GOTO1 (RF),DMCB,(C'T',=C'0'),1              T4 SLIP TYPE               
         GOTO1 (RF),DMCB,(C'T',THISYEAR),L'THISYEAR  TAXATION YEAR              
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
         GOTO1 (RF),DMCB,(C'T',=C'Y'),1              T4 SLIP                    
         GOTO1 (RF),DMCB,(C'T',=C'N'),1              RL-1 SLIP                  
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
         GOTO1 (RF),DMCB,(C'T',=C'N'),1              DATA TO RL-1               
         GOTO1 (RF),DMCB,(C'T',=C'Y'),1              T4 PRNT IMSTR              
         GOTO1 (RF),DMCB,(C'T',=C'1'),1              PENSION PLAN               
         GOTO1 (RF),DMCB,(C'T',TAT4UNIT),L'TAT4UNIT  PROVINCE                   
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
         GOTO1 (RF),DMCB,(C'T',SPACES),1             EXMPT EI                   
         GOTO1 (RF),DMCB,(C'T',SPACES),1             EXMPT CPP/QPP              
         GOTO1 (RF),DMCB,(C'T',SPACES),1             EXMPT PPIP                 
         GOTO1 (RF),DMCB,(C'T',SPACES),1             PAY PERIODS                
         GOTO1 (RF),DMCB,(C'T',SPACES),1             PERIODS OF PENS            
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
                                                                                
         EDIT  (4,TAT4EARN),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-                     
         GOTO1 OUTPDOWN,DMCB,(C'N',BLOCK),12         T4 RENMRATION              
                                                                                
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
         GOTO1 (RF),DMCB,(C'T',=C'Go to Form AUTOTAX'),18                       
                                                                                
         GOTO1 (RF),DMCB,(C'N',BLOCK),12             T4 EMPLY INCME             
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
                                                                                
         EDIT  (4,TAT4EINE),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-                     
         GOTO1 OUTPDOWN,DMCB,(C'N',BLOCK),12         T4 EI INS EARN             
         EDIT  (4,TAT4PPER),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-                     
         GOTO1 OUTPDOWN,DMCB,(C'N',BLOCK),12         T4 CPP PENS ERN            
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
         EDIT  (4,TAT4QPIE),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-                     
         GOTO1 OUTPDOWN,DMCB,(C'N',BLOCK),12         T4 PPIP INS ERN            
         GOTO1 (RF),DMCB,(C'T',SPACES),1                                        
                                                                                
         MVC   BLOCK(4),=C'0.00'                                                
         GOTO1 (RF),DMCB,(C'N',BLOCK),4              T4 CPP CNTR DIFF           
         EDIT  (4,TAT4CPPC),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-                     
         GOTO1 OUTPDOWN,DMCB,(C'N',BLOCK),12         T4 CPP ON SLIP             
         GOTO1 (RF),DMCB,(C'N',BLOCK),12             T4 CPP DEDCT AMT           
         GOTO1 (RF),DMCB,(C'N',BLOCK),12             T4 CPP CALCD AMT           
                                                                                
         EDIT  (4,TAT4EPRM),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-                     
         GOTO1 OUTPDOWN,DMCB,(C'N',BLOCK),12         T4 EI CALCD AMT            
         GOTO1 (RF),DMCB,(C'N',BLOCK),12             T4 EI DEDCT AMT            
         GOTO1 (RF),DMCB,(C'N',BLOCK),12             T4 EI ON SLIP              
         MVC   BLOCK(4),=C'0.00'                                                
         GOTO1 (RF),DMCB,(C'N',BLOCK),4              T4 EI CNTR DIFF            
                                                                                
         L     R2,CNFEDTAX                                                      
         A     R2,TAT4TAX                                                       
         EDIT  (R2),(12,BLOCK),2,ALIGN=LEFT,FLOAT=-                             
         GOTO1 OUTPDOWN,DMCB,(C'N',BLOCK),12         T4 FD DEDCT AMT            
         GOTO1 (RF),DMCB,(C'N',BLOCK),12             T4 FD ON SLIP              
         MVC   BLOCK(4),=C'0.00'                                                
         GOTO1 (RF),DMCB,(C'N',BLOCK),4              T4 FD CNTR DIFF            
                                                                                
         BRAS  RE,EOLDOWN                                                       
         B     PRT4D400            BUMP TO NEXT ELEMENT                         
                                                                                
PRT4D800 BRAS  RE,CHKSYS                                                        
         MVC   KEY,SAVEKEY         RESTORE READ SEQUENCE                        
         GOTO1 HIGH                                                             
         B     PRT4D100                                                         
                                                                                
PRT4D900 BRAS  RE,ENDDOWN                                                       
         J     XIT                                                              
         EJECT                                                                  
*------------------------------*                                                
*  T4 / REPORT                 *                                                
*------------------------------*                                                
PRT4R000 DS    0H                                                               
         J     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        T4BLD'S PREP                                                 *         
***********************************************************************         
PRT4B    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,OPENTAPE                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK             INITIALIZE FOR REPORT                        
         ST    R1,HEADHOOK                                                      
         MVC   MYSORTER,SORTER                                                  
         ZAP   CHKCOUNT,=P'0'                                                   
         MVI   ANYT4,C'N'                                                       
                                                                                
         CLI   INOPT,C'T'                                                       
         JNE   *+12                                                             
         BAS   RE,TREP                                                          
         J     XIT                                                              
                                                                                
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IHKB                                                          
         ST    R1,TIHOOK                                                        
                                                                                
         MVI   TIFCUR,C'U'         SET CURRENCY US$                             
         MVI   TIFW4TY,C'A'        ONLY INTERESTED IN CANADIANS                 
         MVI   TIREAD,TLCKYCDQ     SET TO READ YTD CHECKS                       
         OI    TIQFLAGS,TIQFSKIP       SKIPPING                                 
         MVI   TIQDTYPE,TIQDCHK        BY CHECK DATE                            
                                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         BAS   RE,PUTTAPE                                                       
                                                                                
         BRAS  RE,CLOSTAPE                                                      
         J     XIT                                                              
         EJECT                                                                  
*              SYSIO HOOK                                                       
         SPACE 2                                                                
IHKB     NTR1                                                                   
         CLI   TIMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         JNE   XIT                                                              
                                                                                
         LA    R1,TIKEY                                                         
         USING TLCKPKEY,R1                                                      
*        CLC   TLCKYTXU,=C'CN '    SKIP CN                                      
*        JE    XIT                                                              
         CLC   TLCKYTXU,=C'FD '    SKIP FD                                      
         JE    XIT                                                              
                                                                                
         L     R6,TIAREC                                                        
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         USING TAPDD,R6                                                         
         CLI   TAPDW4TY,TAW4TYCA   AND CANADIAN CHECKS                          
         JNE   XIT                                                              
         AP    CHKCOUNT,=P'1'                                                   
         CP    CHKCOUNT,TRALIMIT                                                
         JH    IHKB2                                                            
         BRAS  RE,TRACEINP                                                      
                                                                                
IHKB2    BAS   RE,PROCHECK         PROCESS A CHECK RECORD                       
         J     XIT                                                              
         EJECT                                                                  
*              PROCESS A CHECK RECORD                                           
         SPACE 2                                                                
PROCHECK NTR1                                                                   
         LA    R5,TAPEIO                                                        
         USING TLT4D,R5                                                         
         CLC   TLT4EMP,TIEMP       CHECK IF FOR SAME EMP/CUR/SSN                
         JNE   PROC2                                                            
         CLI   TICUR,C'U'                                                       
         JNE   PROC2                                                            
         CLC   TLT4SSN,TISSN                                                    
         JNE   PROC2                                                            
         J     PROC4                                                            
                                                                                
PROC2    OC    TLT4SSN,TLT4SSN     IF THIS IS NOT THE FIRST                     
         JZ    PROC3                                                            
         CP    TAPCOUNT,RECLIMIT                                                
         JH    PROC3                                                            
         BAS   RE,PUTTAPE             RELEASE PENDING TAPE RECORD               
                                                                                
PROC3    LR    RE,R5                                                            
         LA    RF,2000                                                          
         XCEF                                                                   
         MVI   TLT4CD,TLT4CDQ      BUILD NEW T4 KEY                             
         MVI   TLT4SCD,TLT4SCDQ                                                 
         MVC   TLT4YEAR,THISYEAR                                                
         MVC   TLT4EMP,TIEMP                                                    
         MVI   TLT4CUR,C'C'                                                     
         MVC   TLT4SSN,TISSN                                                    
*                                                                               
PROC4    XR    R4,R4                                                            
         L     R6,TIAREC           YTD DETAILS                                  
         MVI   ELCODE,TAYEELQ                                                   
         BRAS  RE,GETEL                                                         
         J     PROC4T                                                           
PROC4H   BRAS  RE,NEXTEL                                                        
PROC4T   JNE   PROC5                                                            
         USING TAYED,R6                                                         
         CLI   TAYETYPE,TAYETCHK                                                
         JNE   PROC4H                                                           
         CLI   TAYELEN,TAYELN2Q                                                 
         JL    PROC5                                                            
         LR    R4,R6                                                            
         SPACE 1                                                                
PROC5    L     R6,TIAREC           NOW LOOK FOR YTD ELEMENTS                    
         MVI   ELCODE,TACYELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
         SPACE 1                                                                
PROC6    BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         USING TACYD,R6                                                         
         LA    R1,TIKEY                                                         
         USING TLCKPKEY,R1                                                      
         CLC   TLCKYTXU,TACYUNIT   LOOK FOR MATCH OF UNIT                       
         JNE   PROC6                                                            
         CLC   TIEMP,=C'TP '       IF EMPLOYER EXCLUDING DEFAULT STATES         
         JE    *+12                                                             
         TM    TLCKYSTS,TACWSTAX+TACWSDEF SKIP STATES TAXABLE BY DFLT           
         JO    XIT                                                              
         DROP  R1                                                               
         LA    R5,ELEMENT                                                       
         XC    ELEMENT(64),ELEMENT                                              
         USING TAT4D,R5                                                         
         MVI   TAT4EL,TAT4ELQ                                                   
         MVI   TAT4LEN,TAT4LNQ                                                  
         MVC   TAT4UNIT,TACYUNIT                                                
                                                                                
         CLI   TACYLEN,TACYLN5Q                                                 
         JL    PROC7                                                            
         MVC   TAT4EARN,TACYCERN                                                
         MVC   TAT4TAX,TACYCTAX                                                 
         MVC   TAT4EPRM,TACYCEI                                                 
         MVC   TAT4CPPC,TACYCPP                                                 
         MVC   TAT4QPPR,TACYCPIP                                                
                                                                                
         USING TAYED,R4                                                         
         LTR   R4,R4                                                            
         JZ    PROC7                                                            
         MVC   TAT4PPER,TAYETCPP                                                
         MVC   TAT4EINE,TAYETCEI                                                
         MVC   TAT4QPIE,TAYETCPI                                                
                                                                                
PROC7    OC    TAT4AMTS,TAT4AMTS                                                
         JZ    XIT                                                              
         BRAS  RE,ADDEL                                                         
         MVI   ANYT4,C'Y'                                                       
         J     XIT                                                              
         EJECT                                                                  
*              READ TAPE AND BUILD NEW T4 RECORD                                
         SPACE 3                                                                
TREP     NTR1                                                                   
         LA    R4,INIO                                                          
         USING IND,R4                                                           
         BAS   RE,GETTAPE                                                       
         XC    LASTSSN,LASTSSN                                                  
                                                                                
TREP2    GOTO1 MYSORTER,DMCB,=C'GET'                                            
         L     R2,4(R1)                                                         
         LTR   R2,R2                                                            
         JZ    TREPEOF                                                          
         BRAS  RE,SORTMOVE                                                      
         OC    INSSN,INSSN         IGNORE RECORDS WITHOUT SSN                   
         JZ    TREP2                                                            
         UNPK  WORK(9),INSSN       CONVERT SSN FROM PACKED                      
         CLC   WORK(9),=C'255708108'                                            
         JNE   *+8                                                              
         BRAS  RE,TRACEM                                                        
         CLC   WORK(9),=C'041285240'                                            
         JNE   *+8                                                              
         BRAS  RE,TRACEM                                                        
         AP    CHKCOUNT,=P'1'                                                   
         CP    CHKCOUNT,RECLIMIT                                                
         JH    XIT                                                              
         CP    CHKCOUNT,TRALIMIT                                                
         JH    TREP3                                                            
         BRAS  RE,TRACEM                                                        
                                                                                
TREP3    CLC   LASTSSN,INSSN                                                    
         JE    TREP4                                                            
         OC    LASTSSN,LASTSSN                                                  
         JZ    *+8                                                              
         BAS   RE,PUTTAPE                                                       
         MVC   LASTSSN,INSSN                                                    
         LA    R5,TAPEIO                                                        
         USING TLT4D,R5                                                         
         LR    RE,R5                                                            
         LA    RF,1000                                                          
         XCEF                                                                   
         MVI   TLT4CD,TLT4CDQ      BUILD NEW T4 KEY                             
         MVC   TLT4YEAR,THISYEAR                                                
         CLC   INEMP,=C'00'                                                     
         JNE   *+10                                                             
         MVC   TLT4EMP,=C'TP '                                                  
         CLC   INEMP,=C'56'                                                     
         JNE   *+10                                                             
         MVC   TLT4EMP,=C'P+ '                                                  
         CLC   INEMP,=C'62'                                                     
         JNE   *+10                                                             
         MVC   TLT4EMP,=C'DM '                                                  
         MVI   TLT4CUR,C'U'        ONLY GETTING US, I HOPE                      
         UNPK  TLT4SSN,INSSN       CONVERT SSN FROM PACKED                      
*                                                                               
TREP4    LA    R5,ELEMENT          ELEMENT FOR EACH UNIT ACTIVE                 
         USING TAT4D,R5                                                         
*&&DO                                                                           
         XC    ELEMENT,ELEMENT     FEDERAL                                      
         MVI   CASHACT,C'N'                                                     
         MVI   TAT4EL,TAT4ELQ                                                   
         MVI   TAT4LEN,TAT4LNQ                                                  
         MVC   TAT4UNIT,=C'CN '                                                 
         GOTO1 CONVCASH,DMCB,INFGROS,TAT4EARN                                   
         GOTO1 CONVCASH,DMCB,INFTAX,TAT4TAX                                     
         CLI   CASHACT,C'Y'        ADD IF ACTIVE                                
         JNE   *+8                                                              
         BRAS  RE,ADDEL                                                         
*&&                                                                             
         XC    ELEMENT(64),ELEMENT STATE                                        
         MVI   CASHACT,C'N'                                                     
         MVI   TAT4EL,TAT4ELQ                                                   
         MVI   TAT4LEN,TAT4LNQ                                                  
         MVC   TAT4UNIT,INSTATE                                                 
         MVI   TAT4UNIT+2,C' '                                                  
         GOTO1 CONVCASH,DMCB,INSGROS,TAT4EARN                                   
         GOTO1 CONVCASH,DMCB,INSTAX,TAT4TAX                                     
         CLI   CASHACT,C'Y'        ADD IF ACTIVE                                
         JNE   *+8                                                              
         BRAS  RE,ADDEL                                                         
                                                                                
         XC    ELEMENT(64),ELEMENT LOCAL                                        
         MVI   CASHACT,C'N'                                                     
         MVI   TAT4EL,TAT4ELQ                                                   
         MVI   TAT4LEN,TAT4LNQ                                                  
         MVC   TAT4UNIT,INLOCAL                                                 
         GOTO1 CONVCASH,DMCB,INLGROS,TAT4EARN                                   
         GOTO1 CONVCASH,DMCB,INLTAX,TAT4TAX                                     
         CLI   CASHACT,C'Y'        ADD IF ACTIVE                                
         JNE   *+8                                                              
         BRAS  RE,ADDEL                                                         
         J     TREP2                                                            
                                                                                
TREPEOF  BAS   RE,PUTTAPE                                                       
         J     XIT                                                              
                                                                                
*              HEADLINES ETC                                                    
         SPACE 2                                                                
HOOK     NTR1                                                                   
         MVC   H1+52(9),=C'T4 CREATE'                                           
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H3+58(6),=C'PERIOD'                                              
         MVC   H3+65(4),THISYEAR                                                
         XIT1                                                                   
         SPACE 1                                                                
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         DC    H'0'                                                             
         EJECT                                                                  
*              TAPE ROUTINES                                                    
         SPACE 3                                                                
OPENTAPE NTR1                                                                   
         ZAP   TAPCOUNT,=P'0'                                                   
         CLI   CSCOPT,C'Y'                                                      
         JE    XIT                                                              
         L     R2,=A(T4TAPE)                                                    
         OPEN  ((2),OUTPUT)                                                     
         CLI   INOPT,C'T'          MAYBE AN INPUT TAPE                          
         JNE   XIT                                                              
         L     R2,=A(INTAPE)                                                    
         OPEN  ((2),INPUT)                                                      
         J     XIT                                                              
                                                                                
CLOSTAPE NTR1                                                                   
         BRAS  RE,SPLAT                                                         
         LA    R2,P                                                             
         EDIT  (P6,TAPCOUNT),(7,0(R2))                                          
         MVC   P+8(12),=C'TAPE RECORDS'                                         
         BRAS  RE,SPLAT                                                         
                                                                                
         CLI   CSCOPT,C'Y'                                                      
         JE    XIT                                                              
         L     R2,=A(T4TAPE)                                                    
         CLOSE ((2))                                                            
         CLI   INOPT,C'T'                                                       
         JNE   XIT                                                              
         L     R2,=A(INTAPE)                                                    
         CLOSE ((2))                                                            
         J     XIT                                                              
                                                                                
PUTTAPE  NTR1                                                                   
         CLI   ANYT4,C'N'                                                       
         JE    XIT                                                              
         MVI   ANYT4,C'N'                                                       
*                                                                               
         MVC   RECTYPE,=CL16'OUTPUT'                                            
         CLI   TRACOPT,C'Y'                                                     
         JNE   PUTTAPE2                                                         
         CP    TAPCOUNT,TRALIMIT                                                
         JH    PUTTAPE2                                                         
         BAS   RE,TRACETAP                                                      
                                                                                
PUTTAPE2 LA    R5,TAPEIO                                                        
         USING TLT4D,R5                                                         
         CLI   CSCOPT,C'Y'         IF CSC OPTION, ADD ON FILE                   
         JE    PUTTAPE5                                                         
                                                                                
         LH    R1,TLT4LEN          NOT CSC, ADD TO TAPE                         
         LA    R1,4(R1)                                                         
         STH   R1,PRETAPIO                                                      
         L     R1,=A(T4TAPE)                                                    
         LA    R0,PRETAPIO                                                      
         PUT   (1),(0)                                                          
         J     PUTTAPE9                                                         
                                                                                
PUTTAPE5 ST    R5,AIO                                                           
         MVC   FILENAME,=CL8'CHKFIL'     ADDING DIRECTLY ON FILE (CSC)          
         GOTO1 ADDREC                                                           
                                                                                
PUTTAPE9 AP    TAPCOUNT,=P'1'                                                   
         J     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO GET FROM INPUT TAPE AND PUT TO SORT                   
         SPACE 1                                                                
GETTAPE  NTR1                                                                   
         LA    R0,INIO                                                          
         L     R1,=A(INTAPE)                                                    
         GET   (1),(0)                                                          
         BRAS  RE,SORTPUT                                                       
INEOF    J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              SWITCH TO CHKDIR / CHKFIL                                        
         SPACE 1                                                                
CHKSYS   NTR1  BASE=*,LABEL=*                                                   
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         J     XIT                                                              
                                                                                
*              SWITCH TO TALDIR / TALFIL                                        
         SPACE 1                                                                
TALSYS   NTR1  BASE=*,LABEL=*                                                   
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         J     XIT                                                              
         EJECT                                                                  
*              TRACING ROUTINES                                                 
         SPACE 1                                                                
TRACEINP NTR1  BASE=*,LABEL=*                                                   
         L     R6,TIAREC                                                        
         MVC   RECTYPE,=CL16'CHECK'                                             
         BRAS  RE,TRACEREC                                                      
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         SPACE 1                                                                
TRACETAP NTR1  BASE=*,LABEL=*                                                   
         LA    R6,TAPEIO                                                        
         MVC   RECTYPE,=CL16'OUTPUT'                                            
         BRAS  RE,TRACEREC                                                      
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
TRACEREC NTR1  BASE=*,LABEL=*                                                   
         MVI   P,X'BF'                                                          
         MVC   P+1(131),P                                                       
         MVC   P+60(16),RECTYPE                                                 
         BRAS  RE,SPLAT                                                         
         LA    R2,40                                                            
         BAS   RE,TRACEL                                                        
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
                                                                                
TRACERC2 BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         ZIC   R2,1(R6)                                                         
         BAS   RE,TRACEL                                                        
         J     TRACERC2                                                         
                                                                                
TRACEM   NTR1                                                                   
         MVI   P,X'BF'                                                          
         MVC   P+1(131),P                                                       
         MVC   P+60(16),=CL16'INPUT TAPE'                                       
         BRAS  RE,SPLAT                                                         
         LA    R6,INIO                                                          
         LA    R0,4                                                             
                                                                                
TRACE2   MVC   P(100),0(R2)                                                     
         LA    R2,100                                                           
         BAS   RE,TRACEL                                                        
         LA    R6,100(R6)                                                       
         BCT   R0,TRACE2                                                        
         LA    R2,90                                                            
         BAS   RE,TRACEL                                                        
         J     XIT                                                              
                                                                                
TRACEL   NTR1                                                                   
*                                  R2=LENGTH, R6=ADDRESS                        
         MVC   P,SPACES                                                         
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   P(0),0(R6)                                                       
         OC    P,SPACES                                                         
         GOTO1 HEXOUT,DMCB,(R6),P3,132,=C'SEP'                                  
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   P2(0),P3                                                         
         MVC   P3,SPACES                                                        
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   P3(0),P4                                                         
         MVC   P4,SPACES                                                        
         BRAS  RE,SPLAT                                                         
         BRAS  RE,SPLAT                                                         
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
CONVCASH NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)         INPUT/OUTPUT                                 
         ZAP   DUB,0(7,R2)         INPUT IS PL7                                 
         CVB   R1,DUB                                                           
         ST    R1,0(R3)            OUTPUT IS F                                  
         LTR   R1,R1                                                            
         JZ    XIT                                                              
         MVI   CASHACT,C'Y'        RETURN INDIC IF ACTIVE CASH                  
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
         USING DLCBD,R5                                                         
INITDOWN NTR1  BASE=*,LABEL=*                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1  BASE=*,LABEL=*                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
*                                                                               
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
*                                                                               
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
*                                                                               
         BCTR  RE,0                                                             
                                                                                
         LA    R1,DLCBFLD                                                       
         CLI   DLCBTYP,DLCBNUM     DATA TYPE IS NUMERIC                         
         JE    *+8                                                              
         LA    R1,DLCBFLX                                                       
                                                                                
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R1),0(RF)                                                    
                                                                                
OPD50    GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1  BASE=*,LABEL=*                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1  BASE=*,LABEL=*                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              MY WORKING STORAGE                                               
***********************************************************************         
         SPACE 2                                                                
MYD      DSECT                                                                  
MYSTART  DS    0D                                                               
MYSORTER DS    A                                                                
         SPACE 1                                                                
INOPT    DS    CL1                 D=DISK (SYSIO) T=TAPE (IDC)                  
TRACOPT  DS    CL1                 Y=TRACE                                      
CSCOPT   DS    CL1                 CSC OPTION, ADDREC TO FILE                   
TRALIMIT DS    PL6                                                              
CHKCOUNT DS    PL6                                                              
RECLIMIT DS    PL6                                                              
REPLIMIT DS    PL6                                                              
TAPCOUNT DS    PL6                                                              
         DS    CL6                 SPARE                                        
PRINTOPT DS    CL1                 PRINT=Y PRINT T4 FORMS                       
TAPEOPT  DS    CL1                 TAPE=Y WRITE T4 TAPE                         
YEAR     DS    CL4                                                              
SVDATE   DS    XL3                                                              
*                                                                               
         SPACE 1                                                                
CASHACT  DS    CL1                                                              
ANYT4    DS    CL1                                                              
THISYEAR DS    CL4                                                              
THISYREX DS    F                                                                
CNFEDTAX DS    F                   CANADIAN FEDERAL TAX                         
LASTSSN  DS    CL(L'TLW4SSN)                                                    
NAME1    DS    CL(L'TAW4NAM1)                                                   
NAME2    DS    CL(L'TAW4NAM2)                                                   
ADDR1    DS    CL(L'TAA2ADD1)                                                   
ADDR2    DS    CL(L'TAA2ADD2)                                                   
CITY     DS    CL(L'TAA2CITY)                                                   
PROV     DS    CL(L'TAA2ST)                                                     
ZIP      DS    CL(L'TAA2ZIP)                                                    
CNTRY    DS    CL3                                                              
*                                                                               
RECTYPE  DS    CL16                                                             
CEMPLIST DS    CL32                                                             
*                                  OPTIONS                                      
SAVEEL   DS    CL1                                                              
SAVETPKY DS    CL20                                                             
SAVEAMTS DS    CL36                                                             
SAVEKEY  DS    CL(L'KEY)                                                        
*                                                                               
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
*                                                                               
         DS    0D                                                               
PRETAPIO DS    F                                                                
TAPEIO   DS    2000C                                                            
INIO     DS    2000C                                                            
         SPACE 1                                                                
MYEND    DS    0D                                                               
         EJECT                                                                  
*              DSECT TO COVER INPUT TAPE RECORDS                                
         SPACE 2                                                                
IND      DSECT                                                                  
INYEAR   DS    CL4                                                              
INEMP    DS    CL2                                                              
INSSN    DS    PL5                 (SSN IS PWOS)                                
INCORPID DS    PL5                 (CORP IS SIGNED PACKED)                      
INSTATE  DS    CL2                                                              
INLOCAL  DS    CL3                                                              
INLAST   DS    CL16                                                             
INFIRST  DS    CL16                                                             
INADD1   DS    CL30                                                             
INADD2   DS    CL30                                                             
INADD3   DS    CL30                                                             
INZIP    DS    CL9                                                              
INENAME  DS    CL30                                                             
INEADD1  DS    CL30                                                             
INEADD2  DS    CL30                                                             
INEADD3  DS    CL30                                                             
INEADD4  DS    CL30                                                             
INFGROS  DS    PL7                                                              
INFTXBL  DS    PL7                                                              
INFICA   DS    PL7                                                              
INLGROS  DS    PL7                                                              
INSTAX   DS    PL7                                                              
INCALDIS DS    PL5                                                              
INNYDIS  DS    PL5                                                              
INFTAX   DS    PL7                                                              
INODIS   DS    PL5                                                              
INSGROS  DS    PL7                                                              
INREXP   DS    PL7                                                              
INSDI    DS    PL7                                                              
INSUI    DS    PL7                                                              
INLTAX   DS    PL7                                                              
INTIPS   DS    PL5                                                              
INWAGTIP DS    PL7                                                              
INMSTAT  DS    CL1                                                              
INIDNO   DS    CL10                                                             
INSFTXBL DS    PL7                                                              
INSFICA  DS    PL7                                                              
INFICAR  DS    PL9                                                              
         EJECT                                                                  
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE TAREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE TAREPFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TAREP77   08/22/14'                                      
         END                                                                    
