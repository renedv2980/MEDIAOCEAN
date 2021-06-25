*          DATA SET ACLNK13    AT LEVEL 021 AS OF 03/18/11                      
*PHASE T61F13A                                                                  
ACLNK13  TITLE '- P&&L DOWNLOADS'                                               
         PRINT NOGEN                                                            
SVRDEF   LKSVR IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,ABENDLIST=FAILS,  +        
               SLOWLIST=SLOWS,WORKERKEY=ACPL,TYPE=D,SYSTEM=ACCSYSQ,    +        
               SYSPHASE=SYSPHASE,APPEND=Y,SERVERTYPE=TSTAPAL,          +        
               SEGMENT=Y,BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED)                   
                                                                                
B#LDGREC EQU   3                   I/O AREA FOR LEDGER RECORDS                  
B#ACTREC EQU   B#LDGREC            I/O AREA FOR ACCOUNT RECORDS                 
B#MTHREC EQU   B#LDGREC            I/O AREA FOR METHOD RECORDS                  
B#FLTREC EQU   B#LDGREC            I/O AREA FOR FILTER RECORDS                  
B#BUDREC EQU   B#LDGREC            I/O AREA FOR BUDGET RECORDS                  
B#PLAREC EQU   B#LDGREC            I/O AREA FOR PLCREC RECORDS                  
B#CNTREC EQU   B#LDGREC            I/O AREA FOR CONTRA RECORDS                  
B#BGTREC EQU   B#LDGREC            I/O AREA FOR BUDGET RECORDS                  
B#RAPREC EQU   B#LDGREC            I/O AREA FOR RAPPER RECORDS                  
B#PLDREC EQU   B#LDGREC            I/O AREA FOR DIRECT TIME PTS RECS            
B#PERREC EQU   B#LDGREC            I/O AREA FOR 1R ACCOUNT RECORDS              
B#FRMREC EQU   B#LDGREC            I/O AREA FOR SCRIBE FORMAT RECORD            
B#GLSREC EQU   B#LDGREC            I/O AREA FOR G/L SUMMARY RECORD              
B#GLAREC EQU   B#LDGREC            I/O AREA FOR G/L ACCOUNT RECORD              
         EJECT                                                                  
CODE     NMOD1 0,**AL13**,RR=RE                                                 
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
*                                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   INIT02                                                           
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         B     INIT04                                                           
*                                                                               
INIT02   L     R9,RSVRSAVE                                                      
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LAY   R8,WORKD+OWORKL                                                  
         USING SAVED,R8            R8=A(SAVE W/S)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         MVC   ATWA,LP_ATWA                                                     
*                                                                               
INIT04   MVC   ACOMFACS,RCOMFACS   EXTRACT COMFACS ADDRESS                      
         MVC   AMASTC,RMASTC       EXTRACT MASTC ADDRESS                        
         MVC   ABUFFRIN,RBUFFRIN   EXTRACT BUFFRIN ADDRESS                      
         DROP  R6,R7                                                            
*                                                                               
         ST    R5,ALP              SAVE A(LP_D)                                 
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
         SPACE 1                                                                
RUNSTR   CLI   LP_CMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
*                                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTR10                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)                                                    
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)                                                    
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
*                                                                               
         GOTOR (#WRKINI,AWRKINI)                                                
*                                                                               
RUNSTR10 MVC   LP_BLKS+((B#LDGREC-1)*L'LP_BLKS),AIO2                            
         LA    R0,WVALUES          MOVE LITERALS TO SAVED                       
         LHI   R1,WVALUEL                                                       
         LA    RE,LVALUES                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
         SPACE 1                                                                
PRCWRK   CLI   LP_CMODE,RPRCWRKQ   TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         XC    REQVALS(REQVALL),REQVALS                                         
         MVC   ENDMOA,EFFS                                                      
         MVC   LEDGTAB(DEFLLNQ),DEFLTAB                                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RUN A WORK REQUEST                                                  *         
***********************************************************************         
         SPACE 1                                                                
RUNREQ   CLI   LP_CMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
         MVC   COMPANY,LP_AGYB     SET COMPANY CODE                             
*                                                                               
         ICM   RF,15,AMASTC        SET TRACE OPTION IF OFFLINE                  
         BZ    *+10                                                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
*                                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNREQ10                                                         
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
*                                                                               
         GOTOR (#GETCPY,AGETCPY)                                                
*                                                                               
RUNREQ10 LA    R0,OUTVALS          CLEAR OUTPUT VALUES                          
         LHI   R1,OUTVALL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOR ININDX              INITIALIZE REQUEST INDICES                   
*                                                                               
         CLC   LP_QMAPN,PLADLD     ARE WE DOING P&L AMTS DOWNLOAD               
         BE    RUNREQ20                                                         
         CLC   LP_QMAPN,PERDLD     ARE WE DOING PERSONNEL DWNLD                 
         BE    RUNREQ20                                                         
         CLC   LP_QMAPN,CNTDLD     OR THE CONTRA DOWNLOAD?                      
         BE    RUNREQ20                                                         
         CLC   LP_QMAPN,GLSDLD     OR THE G/L DOWNLOAD?                         
         BE    RUNREQ20                                                         
         CLC   LP_QMAPN,GLTDLD     OR THE G/L TRANSACTION DOWNLOAD?             
         BNE   RUNREQ60                                                         
*                                                                               
         USING CPYRECD,R2          R2=A(COMPANY RECORD)                         
RUNREQ20 L     R2,ACPYREC                                                       
         USING CPYELD,R3                                                        
         LA    R3,CPYRFST                                                       
RUNREQ30 CLI   CPYEL,0             TEST END OF RECORD                           
         BE    RUNREQ50                                                         
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BE    RUNREQ40                                                         
         LLC   R0,CPYLN                                                         
         AR    R3,R0                                                            
         B     RUNREQ30                                                         
*                                                                               
RUNREQ40 MVC   SVCPYST1,CPYSTAT1   COMPANY STATUS BYTES FOR OFFAL               
         MVC   SVCPYST2,CPYSTAT2                                                
         MVC   SVCPYST3,CPYSTAT3                                                
         MVC   SVCPYST4,CPYSTAT4                                                
         MVC   SVCPYST5,CPYSTAT5                                                
         MVC   SVCPYST6,CPYSTAT6                                                
         MVC   SVCPYST7,CPYSTAT7                                                
         MVC   SVCPYST8,CPYSTAT8                                                
         MVC   SVCDPTLN,CPYDEPTL   SAVE OFF DEPT LENGTH                         
*                                                                               
         MVI   CPYINDS,0                                                        
         TM    CPYSTAT1,CPYSOROE                                                
         BZ    *+8                                                              
         OI    CPYINDS,CPYIOFF1                                                 
         TM    CPYSTAT4,CPYSOFF2                                                
         BZ    RUNREQ50                                                         
         OI    CPYINDS,CPYIOFF2                                                 
*                                                                               
RUNREQ50 LA    RE,LP_ACCS          SAVE LIMIT ACCESS VALUE                      
         CLC   0(2,RE),=C'**'      TWO BYTE OFFICE LIST                         
         BNE   *+8                                                              
         LA    RE,LP_ACCS+2        SKIP TO OFFICES                              
         MVC   LIMACC,0(RE)                                                     
         OC    LIMACC,LIMACC                                                    
         JZ    RUNREQ60                                                         
*                                                                               
         LA    R0,OFFTAB           CLEAR OFFICE TABLE                           
         LHI   R1,L'OFFTAB                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   OFFTABND,FF         SET END OF TABLE                             
*                                                                               
         USING OFFALD,R1                                                        
         LA    R1,OFFBLK           INITIALIZE OFFAL FOR OFFICE ACCESS           
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFAALPH,LP_AGY                                                  
         MVC   OFFAAUTH,LP_AUTH                                                 
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFALIMA,LP_ACCS                                                 
         MVC   OFFACST1(SVCPYLQ1),SVCPYST1   COMPANY STATUS BYTES 1-4           
         MVC   OFFACST5(SVCPYLQ2),SVCPYST5   COMPANY STATUS BYTES 5-8           
         MVI   OFFAACT,OFFAINI                                                  
         OI    OFFACTRL,OFFACCNV   NEW STYLE RECORDS                            
         GOTOR VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T INITIALIZE OFFAL                
         DROP  R1                                                               
*                                                                               
         GOTOR INIOFF              INITIALIZE OFFICE BLOCK                      
*                                                                               
         CLC   LP_QMAPN,PERDLD     ARE WE DOING PERSONNELL DWNLD                
         BNE   RUNREQ60                                                         
         TM    CPYINDS,CPYIOFF2                                                 
         BO    RUNREQ60                                                         
         GOTOR SETOFF              MAKE LIST OF 1 BYTE OFF 1 BYTE EACH          
*                                                                               
RUNREQ60 CLC   LP_QMAPN,PLADLD     P&L AMOUNTS DOWNLOAD?                        
         BE    RUNREQ70                                                         
         CLC   LP_QMAPN,PERDLD     PERSONNEL DOWNLOAD?                          
         BE    RUNREQ70                                                         
         CLC   LP_QMAPN,CNTDLD     CONTRA BUCKETS DOWNLOAD?                     
         BE    RUNREQ70                                                         
         CLC   LP_QMAPN,GLSDLD     G/L SUMMARY DOWNLOAD?                        
         BE    *+14                                                             
         CLC   LP_QMAPN,GLTDLD     G/L TRANSACTION DOWNLOAD?                    
         BNE   RUNREQX                                                          
         GOTO1 VDATCON,DMCB,(5,0),(1,STRMOA)                                    
         MVC   ENDMOA,STRMOA                                                    
         OC    GLSTRMOA,GLSTRMOA   ANY START DATE?                              
         BZ    RUNREQ80                                                         
         MVC   WORK(4),GLSTRMOA                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(1,STRMOA)                                 
         OC    GLENDMOA,GLENDMOA   ANY END DATE?                                
         BZ    RUNREQ80                                                         
         MVC   WORK(4),GLENDMOA                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(1,ENDMOA)                                 
         B     RUNREQ80                                                         
*                                                                               
RUNREQ70 LHI   R0,1                BUILD DEFAULT METHOD LIST                    
         STCM  R0,3,MTHLSTN                                                     
         MVI   MTHLST,0                                                         
         ICM   RE,7,AMTH           POINT TO METHOD LIST                         
         BZ    RUNREQ80                                                         
         SR    R1,R1                                                            
         ICM   R1,1,MTHIND         R1=N'METHODS                                 
         BZ    RUNREQ80                                                         
         AHI   R1,1                BUMP UP INDEX TO INCLUDE 0                   
         CHI   R1,L'MTHLST         MAX IS 10 METHODS-IF NEEDED BUMB UP          
         BNH   *+8                                                              
         LHI   R1,L'MTHLST                                                      
         STCM  R1,3,MTHLSTN        STORE IN METHOD LIST                         
         SHI   R1,2                                                             
         EX    R1,*+8                                                           
         B     RUNREQ80                                                         
         MVC   MTHLST+1(0),0(RE)                                                
*                                                                               
RUNREQ80 MVC   STRMOA2,STRMOA      SAVE START DATE AS 2 BYTE MOA                
         MVC   ENDMOA2,ENDMOA      SAVE END   DATE AS 2 BYTE MOA                
*                                                                               
         MVC   WORK+0(2),STRMOA2   BUILD MONTH OF ACTIVITY LIST                 
         MVI   WORK+2,1                                                         
         GOTOR VDATCON,DMCB,(1,WORK),WORK+6                                     
         LHI   R0,MOATABM          R0=MAXIMUM N'MONTHS                          
         CLC   LP_QMAPN,GLSDLD     G/L SUMMARY DOWNLOAD?                        
         BNE   *+8                                                              
         LHI   R0,MOATABM2         R0=MAXIMUM N'MONTHS FOR G/L                  
         LA    R2,MOATAB           R2=A(MONTH TABLE)                            
         LHI   R3,1                R3=N'ENTRIES IN MOATAB                       
RUNREQ90 MVC   0(L'MOATAB,R2),WORK                                              
         CLC   ENDMOA2,WORK        TEST END MONTH REACHED                       
         BE    RUNREQ92                                                         
         GOTOR VADDAY,DMCB,(C'M',WORK+6),WORK,1                                 
         MVC   WORK+6(6),WORK                                                   
         GOTOR VDATCON,DMCB,WORK+6,(1,WORK)                                     
         AHI   R2,L'MOATAB         BUMP TO NEXT ENTRY                           
         AHI   R3,1                BUMP ENTRY COUNT                             
         BCT   R0,RUNREQ90         DO FOR NUMBER OF MONTHS                      
         DC    H'0'                                                             
RUNREQ92 STH   R3,MOATABN          SET N'ENTRIES IN MOATAB                      
*                                                                               
RUNREQX  GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCOUNT EQUIVALENCY AND TRANSLATE                          *         
***********************************************************************         
         SPACE 1                                                                
VALAEQ   L     RE,LP_AINP                                                       
         L     RF,LP_AOUT                                                       
*                                                                               
         MVI   0(RF),0                                                          
         LA    R1,AEQTAB           VALIDATE VALID LEVEL CODE                    
VALAEQ10 CLI   0(R1),EOF                                                        
         JE    EXITN                                                            
         CLC   0(1,R1),0(RE)                                                    
         JE    *+12                                                             
         LA    R1,L'AEQTAB(R1)                                                  
         B     VALAEQ10                                                         
*                                                                               
         MVC   0(L'GLEQLEV,RF),1(R1)                                            
         J     EXITY                                                            
*                                                                               
AEQTAB   DS    0CL2                                                             
         DC    C'A',X'01'                                                       
         DC    C'B',X'02'                                                       
         DC    C'C',X'03'                                                       
         DC    C'D',X'04'                                                       
         DC    C'E',X'05'                                                       
         DC    A(EOF)                                                           
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* READ LEDGER RECORDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
NXTLDG   J     *+12                                                             
         DC    C'*NXTLDG*'                                                      
         LR    RB,RF                                                            
         USING NXTLDG,RB                                                        
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST 'FIRST TIME'                            
         BNE   NXTL10                                                           
         NI    FLAG,FF-FLGFLT                                                   
         MVC   CURUL,LEDGER1R      UNIT/LEDGER - 1R                             
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,PERLDBLK                            
*                                                                               
NXTL10   CLC   LP_QMAPN,ACCDLD     ARE WE DOING ACC DOWNLOAD                    
         BE    NXTL200                                                          
*                                                                               
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',LDGKEYT),('B#LDGREC',0), +        
               SAVED,0,0                                                        
         JNE   EXITY                                                            
         LA    R0,LDGVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         XC    LASTS(LASTL),LASTS                                               
         XC    LDGVALS(LDGVALL),LDGVALS                                         
         L     R2,IOADDR                                                        
         USING LDGRECD,R2                                                       
         MVC   LDGCODE,LDGKUNT                                                  
*                                                                               
         LA    R3,LDGRFST                                                       
NXTL20   CLI   0(R3),0                                                          
         BE    NXTLDGX                                                          
         CLI   0(R3),LDGELQ        LEDGER ELEMENT                               
         BE    NXTL40                                                           
         CLI   0(R3),ACLELQ        ACCOUNT LEVELS/LENGTHS                       
         BE    NXTL80                                                           
         CLI   0(R3),NAMELQ        NAME ELEMENT                                 
         BE    NXTL130                                                          
NXTL30   LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     NXTL20                                                           
*                                                                               
         USING LDGELD,R3                                                        
NXTL40   MVI   LDGCLIP,0           INIT CLIENT POSITION FIELD AS NULL           
         CLC   LDGCODE,LEDGER1C    ONLY SEND DOWN CLIENT POS FOR 1C             
         BNE   *+10                                                             
         MVC   LDGCLIP,LDGCPOS     CLIENT LEVEL IN KEY                          
         MVI   LDGOFFP,0           INIT TO NOT IN KEY                           
*                                                                               
         USING LEDGTBD,R4                                                       
         LA    R4,LEDGTAB          CHECK IF ANY OVERRIDES IN EFFECT             
NXTL50   CLI   0(R4),FF            EOF?                                         
         BE    NXTL70                                                           
         CLC   LDGCODE,LEDGUNT     SAME U/L?                                    
         BE    *+12                                                             
NXTL60   AHI   R4,LEDGLNQ                                                       
         B     NXTL50                                                           
*                                                                               
         TM    LEDGSTAT,LEDGOR     OVERRIDE IN EFFECT?                          
         BNO   NXTL65                                                           
         TM    LEDGSTAT,LEDGPOR    IS THIS AN OFFICE OVERRIDE?                  
         BO    NXTL65                                                           
         MVC   LDGOFFP,LEDGOFFP    MOVE IN OVERRIDE OFFICE POSITION             
         B     NXTL30                                                           
*                                                                               
NXTL65   CLI   LDGOPOS,LDGOFLT1    CHECK IF IN FILTERS                          
         BL    NXTL60                                                           
         OI    LEDGSTAT,LEDGOFLT   SHOW THAT OFFICE IS BY FILTER                
         MVC   BYTE,LDGOPOS                                                     
         NI    BYTE,X'0F'          TURN OFF THE HIGH ORDER BITS                 
         MVC   LEDGOFFP,BYTE       SET FILTER NUMBER IN OFFPOS                  
         LLC   R1,BYTE                                                          
         AHI   R1,12                                                            
         STC   R1,BYTE             ADD 12 TO THE DISP                           
         MVC   LDGOFFP,BYTE                                                     
         B     NXTL30                                                           
         DROP  R4                                                               
*                                                                               
NXTL70   CLI   LDGOPOS,LDGONKHI                                                 
         BNL   NXTL30              OFFICE NOT IN KEY                            
         MVC   BYTE,LDGOPOS                                                     
         NI    BYTE,X'FF'-LDGOKEY2    DON'T CARE IF ITS 2 BYTES                 
         LLC   RE,BYTE                                                          
         STC   RE,LDGOFFP                                                       
         B     NXTL30                                                           
*                                                                               
         USING ACLELD,R3                                                        
NXTL80   MVC   LDGL1LN,ACLVLEN+(L'ACLVALS*0)                                    
         MVC   LDGL1DSC,ACLVDESC+(L'ACLVALS*0)                                  
         MVC   LDGL2LN,ACLVLEN+(L'ACLVALS*1)                                    
         MVC   LDGL2DSC,ACLVDESC+(L'ACLVALS*1)                                  
         MVC   LDGL3LN,ACLVLEN+(L'ACLVALS*2)                                    
         MVC   LDGL3DSC,ACLVDESC+(L'ACLVALS*2)                                  
         CLC   LDGKUNT(2),LEDGER14 ONLY DO 4TH LEVEL FOR 13 AND UNDER           
         BL    *+14                                                             
         CLC   LDGKUNT(2),LEDGER15                                              
         BNH   NXTL90                                                           
         MVC   LDGL4LN,ACLVLEN+(L'ACLVALS*3)                                    
         MVC   LDGL4DSC,ACLVDESC+(L'ACLVALS*3)                                  
*                                                                               
         USING LEDGTBD,R4                                                       
NXTL90   LA    R4,LEDGTAB          CHECK IF ANY OVERRIDES IN EFFECT             
NXTL100  CLI   0(R4),FF            EOF?                                         
         BE    NXTL110                                                          
         CLC   LDGCODE,LEDGUNT     SAME U/L?                                    
         BE    *+12                                                             
         AHI   R4,LEDGLNQ                                                       
         B     NXTL100                                                          
*                                                                               
         MVC   LEDGLEVA(LEDGLVL),LDGL1LN                                        
*                                                                               
NXTL110  GOTOR GETLEVS,LDGL1LN                                                  
         MVC   LEDGLEV#,LEVNUM     SAVE OFF LEVEL NUMBER                        
         MVC   LDGLNS(LDGLNSQ),LEVLNQS  INDIVIDUAL LENGTHS                      
*                                                                               
         CLC   LDGCODE,LEDGER13    SPECIAL CODE FOR U/L 13                      
         BNE   NXTL30                                                           
         CLC   LP_VRSN1,V12012     SPECIAL 13 LOGIC FOR VERSIONS                
         BH    NXTL120             12 OR EARLIER                                
         CLI   LDGOFFP,0           DID WE HAVE AN OFFPOS?                       
         BNE   NXTL30              YES - KEEP IT                                
         LLC   RE,LEVNUM                                                        
         CHI   RE,1                IF 1 LEVEL NO OFF POS                        
         BE    NXTL30                                                           
         CHI   RE,2                IF 2 LEVELS OFF POS IS 2ND LEVEL             
         BE    *+8                                                              
         AHI   RE,-1               FOR ALL ELSE BUMP BACK 1 LEVEL               
         STC   RE,LDGOFFP                                                       
         B     NXTL30                                                           
*                                                                               
NXTL120  CLI   LDGOFFP,0           DID WE GET AN OFFPOS FOR 13                  
         BNE   NXTL30              YES- LEAVE AS IS                             
         LA    RF,LEDGLEVB         ASSUME 4 LEVEL                               
         CLI   LEDGLEV#,2          IF ONLY 1 LEVEL FORGET IT                    
         BL    NXTL30                                                           
         CLI   LEDGLEV#,3          THREE OR LESS                                
         BH    *+12                NO                                           
         LA    RF,LEDGLEVA         START OFF/DEPT AT 2ND LEVEL                  
         MVI   LEDGLEV#,3          SET LEVELS TO 3 WHETHER 2 OR 3               
         LLC   RE,0(RF)                                                         
         AHI   RE,1                START AT 1ST POS OF NEXT LEV                 
         STC   RE,LDGOFFP                                                       
         MVC   LEDGOFFP,LDGOFFP    SET OFFPOS IN TABLE                          
         B     NXTL30                                                           
         DROP  R4                                                               
*                                                                               
         USING NAMELD,R3                                                        
NXTL130  LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         EX    RF,*+8                                                           
         B     NXTL30                                                           
         MVC   LDGNAME(0),NAMEREC                                               
*                                                                               
NXTL200  CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTL210                                                          
         NI    FLAG,X'FF'-FLGNOUL  TURN OFF U/L FLAG IF ON                      
         SR    RE,RE                                                            
         ICM   RE,7,AUL            POINT TO UNIT/LEDGER LIST                    
         SR    R1,R1                                                            
         ICM   R1,1,ULIND          R1=N'U/LS                                    
         BNZ   *+12                                                             
         OI    FLAG,FLGNOUL                                                     
         J     NOMORE                                                           
         ST    RE,SVADDR2                                                       
         STC   R1,SVACCNUM                                                      
         B     NXTL220                                                          
*                                                                               
NXTL210  L     RE,SVADDR2          RESTORE ADDRESS OF LEDGER TABLE              
         AHI   RE,L'UNTLDG+L'RECLEV                                             
         ST    RE,SVADDR2                                                       
         LLC   R1,SVACCNUM                                                      
         AHI   R1,-1                                                            
         JNP   NOMORE                                                           
         STC   R1,SVACCNUM                                                      
*                                                                               
NXTL220  NI    FLAG,FF-FLG1R       TURN OFF 16 TO 1R OVERRIDE                   
         L     RE,SVADDR2          RESTORE ADDRESS OF TABLE                     
         MVC   UNTLDG,0(RE)        MOVE IN U/L                                  
         MVC   RECLEV,L'UNTLDG(RE) SAVE OFF LEVEL FOR NXTACT                    
         CLC   0(L'UNTLDG,RE),LEDGER16 ARE WE ABOUT TO DO 16?                   
         BNE   *+14                                                             
         MVC   UNTLDG,LEDGER1R     CHANGE IT TO 1R                              
         OI    FLAG,FLG1R          SHOW THAT WE ARE DOING 16 TO 1R              
*                                                                               
         USING LDGRECD,R2          R2=A(LEDGER RECORD)                          
         LA    R2,IOKEY                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(L'LDGKUNT+L'LDGKLDG),UNTLDG                              
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOFIL+IO1'                               
*                                                                               
         L     R2,AIO1                                                          
         CLC   0(LDGKEND,R2),IOKEY                                              
         BNE   NXTLDGX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,LDGVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         XC    LDGVALS(LDGVALL),LDGVALS                                         
*                                                                               
         USING LDGRECD,R2                                                       
         L     R2,IOADDR                                                        
*                                                                               
         USING NAMELD,R3                                                        
         LA    R3,LDGRFST                                                       
NXTL230  CLI   NAMEL,0                                                          
         BE    NXTLDGX                                                          
         CLI   NAMEL,ACLELQ        ACCOUNT LEVELS/LENGTHS                       
         BE    NXTL250                                                          
NXTL240  LLC   R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     NXTL230                                                          
*                                                                               
         USING ACLELD,R3                                                        
NXTL250  MVC   LDGL1LN,ACLVLEN+(L'ACLVALS*0)                                    
         MVC   LDGL2LN,ACLVLEN+(L'ACLVALS*1)                                    
         MVC   LDGL3LN,ACLVLEN+(L'ACLVALS*2)                                    
         MVC   LDGL4LN,ACLVLEN+(L'ACLVALS*3)                                    
*                                                                               
         MVC   LDGCODE,UNTLDG                                                   
         CLC   LDGCODE,LEDGER16    ARE WE DOING A 14,15,16 - 1R CONVERS         
         BH    NXTL290             NO - SKIP                                    
         CLC   LDGCODE,LEDGER14                                                 
         BL    NXTL260                                                          
         GOTOR CNVLDG,LDGCODE                                                   
         B     NXTL240                                                          
*                                                                               
NXTL260  CLC   LDGCODE,LEDGER13                                                 
         BNE   NXTL290                                                          
*                                                                               
         USING LEDGTBD,R4                                                       
         LA    R4,LEDGTAB          CHECK IF ANY OVERRIDES IN EFFECT             
NXTL270  CLI   0(R4),FF            EOF?                                         
         BE    NXTL280                                                          
         CLC   LDGCODE,LEDGUNT     SAME U/L?                                    
         BE    *+12                                                             
         AHI   R4,LEDGLNQ                                                       
         B     NXTL270                                                          
*                                                                               
         MVC   LEDGLEVA(LEDGLVL),LDGL1LN                                        
*                                                                               
NXTL280  GOTOR GETLEVS,LDGL1LN                                                  
         MVC   LDGLNS(LDGLNSQ),LEVLNQS  INDIVIDUAL LENGTHS                      
*                                                                               
         CLI   LDGOFFP,0           DID WE GET AN OFFPOS FOR 13                  
         BNE   NXTL240             YES- LEAVE AS IS                             
         LA    RF,LEVLNQB          ASSUME 4 LEVEL                               
         CLI   LEVNUM,2            IF ONLY 1 LEVEL FORGET IT                    
         BL    NXTL240                                                          
         CLI   LEVNUM,3            THREE OR LESS                                
         BH    *+12                NO                                           
         LA    RF,LEVLNQA          START OFF/DEPT AT 2ND LEVEL                  
         MVI   LEVNUM,3            SET LEVELS TO 3 WHETHER 2 OR 3               
         LLC   RE,0(RF)                                                         
         AHI   RE,1                START AT 1ST POS OF NEXT LEV                 
         STC   RE,LDGOFFP                                                       
         MVC   LEDGOFFP,LDGOFFP    SET OFFPOS IN TABLE                          
         B     NXTL240                                                          
         DROP  R4                                                               
*                                                                               
NXTL290  GOTOR GETLEVS,LDGL1LN                                                  
         B     NXTL240                                                          
*                                                                               
NXTLDGX  CLC   LP_QMAPN,ACCDLD     ARE WE DOING ACCOUNT DOWNLOAD?               
         JE    EXITY                                                            
         CLC   LDGCODE,LEDGER16    ARE WE DOING A 14,15,16 - 1R CONVERS         
         JH    EXITY               NO - SKIP                                    
         CLC   LDGCODE,LEDGER13                                                 
         JL    EXITY                                                            
         GOTOR CNVLDG,LDGCODE                                                   
         J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* READ ALLOCATION METHOD RECORDS                                      *         
***********************************************************************         
         SPACE 1                                                                
NXTMTH   J     *+12                                                             
         DC    C'*NXTMTH*'                                                      
         LR    RB,RF                                                            
         USING NXTMTH,RB                                                        
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',MTHKEYT),('B#MTHREC',0), +        
               SAVED,0,0                                                        
         JNE   EXITY                                                            
         LA    R0,MTHVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         XC    LASTS(LASTL),LASTS                                               
         XC    MTHVALS(MTHVALL),MTHVALS                                         
*                                                                               
         USING CMTRECD,R2          R2=A(ALLOCATION METHOD RECORD)               
         L     R2,IOADDR                                                        
*                                                                               
         USING NAMELD,R3                                                        
         LA    R3,CMTRFST                                                       
NXTMTH02 CLI   NAMEL,0             PROCESS RECORD                               
         JE    EXITY                                                            
         CLI   NAMEL,NAMELQ                                                     
         BE    NXTMTH06                                                         
         CLI   NAMEL,METELQ                                                     
         BE    NXTMTH08                                                         
NXTMTH04 LLC   R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     NXTMTH02                                                         
*                                                                               
NXTMTH06 LLC   RF,NAMLN            SET METHOD NAME                              
         SHI   RF,NAMLN1Q+1                                                     
         EX    RF,*+8                                                           
         B     NXTMTH04                                                         
         MVC   MTHNAME(0),NAMEREC                                               
*                                                                               
         USING METELD,R3                                                        
NXTMTH08 MVC   MTHNUMB,METNUM      SET METHOD NUMBER                            
         MVC   MTHCODE,METCODE     SET METHOD CODE                              
         B     NXTMTH04                                                         
         DROP  R2,R3,RB                                                         
         EJECT                                                                  
***********************************************************************         
* READ FILTER RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
NXTFLT   J     *+12                                                             
         DC    C'*NXTFLT*'                                                      
         LR    RB,RF                                                            
         USING NXTFLT,RB                                                        
         LA    R0,FLTVALS          SET A(OUTPUT ROW)                            
         ST    R0,LP_ADATA                                                      
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTFL10                                                          
         CLC   LDGCODE,LEDGER1C    ONLY 1C FOR NOW                              
         JNE   EXITN                                                            
*                                                                               
         MVC   SVIOKEY,IOKEY                                                    
         XC    FLTNUM,FLTNUM                                                    
         XC    LASTS(LASTL),LASTS                                               
         XC    LSTFLTS(LSTFLTLN),LSTFLTS                                        
         MVI   LASTFLAG,0                                                       
         MVI   FLAG,0                                                           
*                                                                               
         USING CTVREC,R2           R2=A(VALUE RECORD)                           
         LA    R2,IOKEY                                                         
         XC    CTVKEY,CTVKEY                                                    
         MVI   CTVKTYP,CTVKTYPQ                                                 
         MVI   CTVKREC,C'F'        FILTERS                                      
         MVI   CTVKSYS,C'A'        ACCOUNT                                      
         MVC   CTVKKEY(1),COMPANY                                               
         MVC   CTVKKEY+1(2),SVIOKEY+(LDGKUNT-LDGRECD)                           
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTFILE+IO1'                            
         B     NXTFL20                                                          
*                                                                               
NXTFL10  TM    LASTFLAG,LASTLAST                                                
         BNO   *+14                                                             
         MVC   IOKEY,SVIOKEY                                                    
         J     NOMORE                                                           
         TM    LASTFLAG,LASTPROC   DO WE CURRENTLY HAVE A RECORD?               
         BO    NXTFL30                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCTFILE+IO1'                            
NXTFL20  L     R2,AIO1                                                          
         CLC   0(CTVKKEY-CTVKEY+3,R2),IOKEY                                     
         BE    *+16                                                             
         OI    LASTFLAG,LASTLAST                                                
         MVI   LP_RMODE,LP_RNEXT   RESET LP_RNEXT TO NEXT TIME                  
         B     NXTFLX                                                           
*                                                                               
         OC    LASTFLN,LASTFLN                                                  
         BZ    NXTFL30                                                          
         CLC   LASTFLN,CTVKNUM     SAME NUMBER AS BEFORE?                       
         BE    NXTFL40                                                          
         OI    LASTFLAG,LASTPROC                                                
         B     NXTFLX                                                           
*                                                                               
NXTFL30  L     R2,AIO1                                                          
         NI    LASTFLAG,FF-(LASTPROC)                                           
*                                                                               
         LA    R0,FLTVALS          CLEAR OUTPUT VALUES                          
         LHI   R1,FLTVALL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   FLTNUMB,CTVKNUM     SET FILTER NUMBER                            
         MVC   LASTFLN,FLTNUMB     SAVE FOR NEXT COMPARE                        
         XC    FLTNUM,FLTNUM                                                    
*                                                                               
NXTFL40  L     R2,AIO1                                                          
         LA    RE,FLTENT                                                        
ENTRY    USING FLTENT,RE                                                        
         OC    ENTRY.FLTENT,ENTRY.FLTENT   FIND NEXT AVAILABLE SPACE            
         BZ    *+12                                                             
         AHI   RE,L'FLTENT                                                      
         B     *-14                                                             
*                                                                               
         LA    R3,CTVDATA                                                       
NXTFL50  CLI   0(R3),0             PROCESS RECORD                               
         BE    NXTFL10                                                          
         CLI   0(R3),CTVNMELQ      VALUE NAME ELEMENT                           
         BE    NXTFL70                                                          
         CLI   0(R3),CTVVLELQ      VALUE BYTE ELEMENT                           
         BE    NXTFL80                                                          
NXTFL60  LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     NXTFL50                                                          
*                                                                               
         USING CTVNMD,R3                                                        
NXTFL70  TM    FLAG,FLGREC         DO WE CURRENTLY HAVE A RECORD?               
         BO    NXTFL60                                                          
         LLC   RF,CTVNMLEN         SET FILTER DESCRIPTION                       
         SHI   RF,(CTVNMNAM-CTVNMD)+1                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLTDSC(0),CTVNMNAM                                               
         B     NXTFL60                                                          
*                                                                               
         USING CTVVLD,R3                                                        
NXTFL80  OI    FLAG,FLGREC         MARK AS CURRENTLY PROCESSING A REC           
         MVC   ENTRY.FLTCODE,CTVVLCHR   SET FILTER CODE                         
*                                                                               
         LLC   RF,CTVVLLEN         SET FILTER DESCRIPTION                       
         SHI   RF,(CTVVLNAM-CTVVLD)+1                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ENTRY.FLTNAME(0),CTVVLNAM                                        
         DROP  ENTRY                                                            
*                                                                               
         AHI   RE,FLTENTQ          BUMP TO NEXT AVAIL SPACE IN FLTENT           
         LLH   R1,FLTNUM                                                        
         AHI   R1,1                                                             
         CHI   R1,FLTMAX                                                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  R1,3,FLTNUM                                                      
         B     NXTFL60                                                          
*                                                                               
NXTFLX   NI    FLAG,FF-FLGREC                                                   
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* READ BUDGET RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
NXTBUD   J     *+12                                                             
         DC    C'*NXTBUD*'                                                      
         LR    RB,RF                                                            
         USING NXTBUD,RB                                                        
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',BUDKEYT),('B#BUDREC',0), +        
               SAVED,0,0                                                        
         JNE   EXITY                                                            
         LA    R0,BUDVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         XC    LASTS(LASTL),LASTS                                               
         XC    BUDVALS(BUDVALL),BUDVALS                                         
*                                                                               
         USING BUDRECD,R2          R2=A(BUDGET RECORD)                          
         L     R2,IOADDR                                                        
*                                                                               
         LA    R3,BUDRFST                                                       
NXTBUD10 CLI   0(R3),0             PROCESS RECORD                               
         JE    EXITY                                                            
         CLI   0(R3),BIVELQ        BUDGET INPUT VALID'TN ELEMENTS               
         BE    NXTBUD30                                                         
         CLI   0(R3),NAMELQ        NAME ELEMENT                                 
         BE    NXTBUD40                                                         
NXTBUD20 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     NXTBUD10                                                         
*                                                                               
         USING BIVELD,R3                                                        
NXTBUD30 CLC   BIVAUNT(2),LEDGER1C ONLY CONCERNED WITH COSTING BUDGETS          
         BNE   NXTBUD20                                                         
         MVC   BUDNUMB,BUDKNO1     SET BUDGET NUMBER                            
         MVC   BUDCODE,BUDKCOD     SET BUDGET CODE                              
         MVC   BUDLEV,BIVACLV      SET ACCOUNT LEVEL FOR BUDGET                 
         B     NXTBUD20                                                         
*                                                                               
         USING NAMELD,R3                                                        
NXTBUD40 OC    BUDNUMB(L'BUDNUMB+L'BUDKCOD),BUDNUMB  ANY INFO?                  
         BZ    NXTBUD20                                                         
*                                                                               
         LLC   RF,NAMLN            SET METHOD NAME                              
         SHI   RF,NAMLN1Q+1                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUDNAME(0),NAMEREC                                               
         B     NXTBUD20                                                         
         DROP  R2,R3,RB                                                         
         EJECT                                                                  
***********************************************************************         
* READ ACCOUNT RECORDS                                                *         
***********************************************************************         
         SPACE 1                                                                
NXTACT   J     *+12                                                             
         DC    C'*NXTACT*'                                                      
         LR    RB,RF                                                            
         USING NXTACT,RB                                                        
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTAC10                                                          
         TM    FLAG,FLGNOUL        Any U/L defined?                             
         JO    NOMORE                                                           
*                                                                               
         MVC   ACCEDTE,EFFS        SET END DATE TO INFINITY (RANGE)             
         XC    ACCSTIM,ACCSTIM     START TIME SET TO ZEROES (RANGE)             
         MVC   ACCETIM,EFFS        SET END TIME TO INFINITY (RANGE)             
         NI    FLAG,FF-FLGDTTM     CLEAR DATE/TIME FLAG                         
*                                                                               
         MVC   LEVEL,RECLEV        ACCOUNT LEVEL                                
         NI    LEVEL,X'0F'         MAKE IT BINARY                               
         XC    LASTS(LASTL),LASTS                                               
*                                                                               
         USING ACTRECD,R2          R2=A(ACCOUNT RECORD)                         
         LA    R2,IOKEY            SET UP LAST KEY FOR 1ST READ                 
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),UNTLDG                              
         MVC   LASTKEY,IOKEY       SAVE OFF KEY                                 
*                                                                               
NXTAC10  CLC   LP_VRSN1,V10019     DO NOT DOWNLOAD ANYTHING BUT                 
         BH    NXTAC20             1C fOR VRSN 19 OR HIGHER                     
         OC    ACCSDTE,ACCSDTE                                                  
         BZ    NXTAC20                                                          
         CLC   UNTLDG,LEDGER1C                                                  
         BE    NXTAC20                                                          
         CLC   UNTLDG,LEDGER1R                                                  
         BE    NXTAC20                                                          
         CLC   UNTLDG,LEDGERSJ                                                  
         JNE   EXITN                                                            
*                                                                               
NXTAC20  TM    FLAG,FLG1R          ARE WE DOING THE 16 TO 1R OVERRIDE?          
         BO    NXTAC40                                                          
*                                                                               
         LA    R0,RAPKEYT          USE RAP POINTER 1 FOR 1C                     
         CLC   UNTLDG,LEDGER1C     ONLY CHECK POINTERS FOR 1C/1R                
         BE    NXTAC30                                                          
         LA    R0,RAPKYT2          USE RAP POINTER 2 FOR 1R                     
         CLC   UNTLDG,LEDGER1R                                                  
         BE    NXTAC30                                                          
         CLC   UNTLDG,LEDGERSJ                                                  
         BNE   NXTAC50                                                          
         LA    R0,RAPKYT3          USE RAP POINTER 3 FOR SJ                     
NXTAC30  ST    R0,SVADDR                                                        
         OC    ACCSDTE,ACCSDTE     ANY DATE PASSED?                             
         BZ    NXTAC50             NO DATE MEANS RUN FOR ALL ACCOUNTS           
*                                                                               
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',SVADDR),('B#RAPREC',0),  +        
               SAVED,FLTRPK,0                                                   
         JNE   EXITY                                                            
         B     NXTAC60                                                          
*                                                                               
         USING ACTRECD,R2          R2=A(ACCOUNT RECORD)                         
NXTAC40  LA    R2,IOKEY            FOR 16 READ FILE MANUALLY                    
         MVC   ACTKEY,LASTKEY                                                   
         LA    RE,ACTKACT                                                       
         LLC   R1,LEVLNQA                                                       
         AR    RE,R1                                                            
         MVI   0(RE),FF            BUMP TO NEXT 1ST LEVEL                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOFIL+IO1'                               
*                                                                               
         L     R2,AIO1                                                          
         CLC   0(ACTKACT-ACTKEY,R2),LASTKEY                                     
         JNE   EXITN                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LASTKEY(L'ACTKEY),0(R2)                                          
         B     NXTAC60                                                          
*                                                                               
NXTAC50  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',ACCKEYT),('B#ACTREC',0), +        
               SAVED,0,0                                                        
         JNE   EXITY                                                            
*                                                                               
NXTAC60  LA    R0,ACTVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         NI    FLAG,FF-FLGREC                                                   
         XC    ACTVALS(ACTVALL),ACTVALS                                         
*                                                                               
         USING ACTRECD,R2                                                       
         L     R2,IOADDR                                                        
*                                                                               
         GOTOR SETCDE,ACTKACT      SET CODES IN LEVS                            
         JNE   EXITY                                                            
*                                                                               
         LLC   R0,LEVNUM           MAX LEVELS                                   
         LHI   R1,0                INIT LEVEL COUNTER                           
         LA    R3,LASTLV1C         LAST ACCOUNT LEVEL FIELDS                    
         LA    RE,LEVS             FIND OUT WHAT LEVEL WE ARE AT                
         LA    RF,ACTLV1CD         MOVE CODE INTO RIGHT FIELD                   
NXTAC70  CLC   0(L'LEVACDE,RE),SPACES                                           
         BE    NXTAC80                                                          
         MVC   CURACDE,0(RE)       SAVE OFF CURRENT CODE                        
         CLC   0(L'CURACDE,R3),CURACDE                                          
         BE    *+10                                                             
         MVC   0(L'CURACDE,RF),CURACDE      MOVE CODE INTO RIGHT FIELD          
         MVC   0(L'CURACDE,R3),CURACDE      SAVE OFF CODE IN LAST FLD           
         AHI   R3,L'LASTLV1C                                                    
         AHI   RE,L'LEVS                                                        
         AHI   RF,L'ACTLV1CD+L'ACTLV1NM                                         
         AHI   R1,1                                                             
         BCT   R0,NXTAC70                                                       
*                                                                               
NXTAC80  CLC   UNTLDG,LEDGER16     ONLY SEND THE 1ST LEVEL FOR 14,15,16         
         BH    NXTAC90                                                          
         CLC   UNTLDG,LEDGER14                                                  
         BNL   *+12                                                             
         TM    FLAG,FLG1R          ARE WE DOING THE 16 TO 1R OVERRIDE?          
         BNO   NXTAC90                                                          
         CHI   R1,1                                                             
         BH    NXTAC10                                                          
NXTAC90  OI    FLAG,FLGREC         YOU ALWAYS HAVE A RECORD HERE                
*                                                                               
         OC    LEVEL,LEVEL         ANY LEVEL REQUESTED?                         
         BZ    *+12                NO - SKIP COMPARE                            
         CLM   R1,1,LEVEL          CHECK LEVEL                                  
         BH    NXTAC10                                                          
         STH   R1,HALF             SAVE OFF LEDGER                              
*                                                                               
         CLC   LASTUL,ACTKUNT      SAME UNIT/LEDGER                             
         BE    NXTAC100                                                         
         MVC   ACTALDG,ACTKUNT     MOVE IN THE UNIT/LEDGER                      
         TM    FLAG,FLG1R          ARE WE DOING THE 16 TO 1R OVERRIDE?          
         BNO   NXTAC100                                                         
         MVC   ACTALDG,LEDGER16    MOVE IN THE 16 UNIT/LEDGER                   
*                                                                               
NXTAC100 MVC   LASTUL,ACTKUNT                                                   
*                                                                               
         CLC   LASTLEV,HALF        IF A CHANGE IN LEV SEND ACT                  
         BNE   *+14                IF SAME LEVEL CHECK ACCOUNT                  
         CLC   LASTACT,ACTKACT     IF SAME EXIT                                 
         BE    NXTAC10                                                          
         MVC   LASTACT,ACTKACT     CURRENT ACCOUNT LEVEL CODE                   
         MVC   LASTLEV,HALF        SAVE OFF CURRENT LEVEL                       
*                                                                               
         MVC   ACTFLTS,SPACES      CLEAR FIELD TO SPACES                        
*                                                                               
         LA    R3,ACTRFST                                                       
NXTAC110 CLI   0(R3),0                                                          
         BE    NXTACX                                                           
         CLI   0(R3),NAMELQ        NAME ELEMENT                                 
         BE    NXTAC130                                                         
         CLI   0(R3),PPRELQ        PRODUCTION PROFILE ELEMENT                   
         BE    NXTAC150                                                         
         CLI   0(R3),RSTELQ        RECORD STATUS ELEMENT                        
         BE    NXTAC170                                                         
NXTAC120 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     NXTAC110                                                         
*                                                                               
         USING NAMELD,R3                                                        
NXTAC130 MVC   CURANME,SPACES                                                   
         LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CURANME(0),NAMEREC                                               
*                                                                               
         LA    RF,ACTLV1CD         MOVE CODE INTO RIGHT FIELD                   
         CLI   HALF+1,1            FIRST LEVEL - JUST MOVE IT IN                
         BNH   NXTAC140                                                         
*                                                                               
         LH    R1,HALF                                                          
         AHI   R1,-1                                                            
         STH   R1,HALF2                                                         
         LHI   RE,L'ACTLV1CD+L'ACTLV1NM                                         
         MH    RE,HALF2                                                         
         AR    RF,RE                                                            
NXTAC140 MVC   0(CURALNQ,RF),CURACDE                                            
         B     NXTAC120                                                         
*                                                                               
         USING PPRELD,R3                                                        
NXTAC150 CLC   PPRGAOFF,SPACES     ANY OFFICE?                                  
         BH    NXTAC160                                                         
         CLC   ACTLV3CD,SPACES     ARE WE AT JOB LEVEL                          
         BH    NXTAC160                                                         
         OI    ACTSTAT,ACTLEVQ     NO OFFICE IS DEFINED AT THIS LEV             
         B     NXTAC120                                                         
NXTAC160 CLC   LASTOFFV,PPRGAOFF   SAME OFFICE AS PREVIOUS                      
         BE    NXTAC120                                                         
         MVC   ACTOFFVL,PPRGAOFF                                                
         MVC   LASTOFFV,ACTOFFVL                                                
         B     NXTAC120                                                         
*                                                                               
         USING RSTELD,R3                                                        
NXTAC170 CLC   LASTFLT1,RSTFILT1   SAME FILTER 1 AS BEFORE?                     
         BNE   NXTAC180                                                         
         CLC   LASTFLT2,RSTFILT2   SAME FILTER 2 AS BEFORE?                     
         BNE   NXTAC180                                                         
         CLC   LASTFLT3,RSTFILT3   SAME FILTER 3 AS BEFORE?                     
         BNE   NXTAC180                                                         
         CLC   LASTFLT4,RSTFILT4   SAME FILTER 4 AS BEFORE?                     
         BNE   NXTAC180                                                         
         CLC   LASTFLT5,RSTFILT5   SAME FILTER 5 AS BEFORE?                     
         BE    NXTAC120            ALL ARE THE SAME-SKIP                        
NXTAC180 MVC   ACTFLT1,RSTFILT1    FILTER 1                                     
         MVC   ACTFLT2,RSTFILT2    FILTER 2                                     
         MVC   ACTFLT3,RSTFILT3    FILTER 3                                     
         MVC   ACTFLT4,RSTFILT4    FILTER 4                                     
         MVC   ACTFLT5,RSTFILT5    FILTER 5                                     
         LA    R1,ACTFLTS                                                       
         LHI   R0,L'ACTFLTS                                                     
         CLI   0(R1),C' '          ANY SIGNIFICANT VALUE?                       
         BH    NXTAC190                                                         
         AHI   R1,1                                                             
         BCT   R0,*-12                                                          
         OI    ACTSTAT,ACTFLTEQ    MARK AS UNDEFINED                            
         B     NXTAC120                                                         
NXTAC190 MVC   LASTFLTS,ACTFLTS                                                 
         B     NXTAC120                                                         
*                                                                               
NXTACX   TM    FLAG,FLGREC         DID WE FIND A RECORD?                        
         BO    NXTACX3                    IF YES- SET DATE AND TIME             
         XC    ACTVALS(ACTVALL),ACTVALS   IF NO - CLEAR OUTPUT                  
         B     NXTAC10                                                          
*                                                                               
NXTACX3  CLC   UNTLDG,LEDGER1C     SEND DOWN DATE/TIME FOR 1C/SJ/1R             
         BE    NXTACX4                                                          
         CLC   UNTLDG,LEDGERSJ                                                  
         BE    NXTACX4                                                          
         CLC   UNTLDG,LEDGER1R                                                  
         BNE   NXTACX5                                                          
         TM    FLAG,FLG1R          AND NOT FOR 16 TO 1R OVERRIDE.               
         BO    NXTACX5                                                          
*                                                                               
NXTACX4  GOTOR VDATCON,DMCB,(5,0),(2,SVDTE)                                     
         TIME  BIN                                                              
         SRDL  R0,32               TIME IS IN 100THS OF A SECOND                
         D     R0,=F'100'                                                       
         STCM  R1,7,SVTIM                                                       
*                                                                               
         XC    ACTDATE(ACTDTLNQ),ACTDATE  INIT DATE/TIME FIELDS                 
         TM    FLAG,FLGDTTM        HAS TIME BEEN SENT ALREADY                   
         BO    NXTACX5                                                          
         MVC   ACTDATE,SVDTE                                                    
         MVC   ACTTIME,SVTIM       CLEAR IT IF THE SAME                         
         OI    FLAG,FLGDTTM        SHOW THAT DATE/TIME WAS SENT                 
*                                                                               
NXTACX5  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* P&L AMOUNTS BY ALLOCATION METHOD OR BUDGET NUMBER                   *         
***********************************************************************         
         SPACE 1                                                                
NXTPLA   J     *+12                                                             
         DC    C'*NXTPLA*'                                                      
         LR    RB,RF                                                            
         USING NXTPLA,RB                                                        
         CLI   LP_RMODE,LP_RFRST   TEST 'FIRST TIME'                            
         BNE   NXTPL100                                                         
*                                                                               
         GOTOR (#GETLDG,AGETLDG),DMCB,CCOSTLDG,CSTLDBLK                         
*                                                                               
         MVC   CNTACT,SPACES       INIT CONTRA ACCOUNT FIELD                    
         MVI   SVACCNUM,0                                                       
*                                                                               
         ICM   RE,7,AACT           POINT TO UNIT/LEDGER LIST                    
         SR    R1,R1                                                            
         ICM   R1,1,ACTIND         R1=NO. OF ACCOUNTS IN LIST                   
         BZ    *+10                                                             
NXTPL10  MVC   CNTACT,0(RE)                                                     
         ST    RE,SVADDR2                                                       
         STC   R1,SVACCNUM                                                      
*                                                                               
NXTPL20  XC    LASTS(LASTL),LASTS                                               
         NI    FLAG,FF-FLGREC                                                   
         MVI   LASTMTH,FF                                                       
*                                                                               
         MVC   PL1CRNGE,ACTRANGE   INIT PL1CRNGE AS ACTRANGE                    
         MVC   CNTRANGE,ACTRANGE   INIT CONTRA RANGE AS ACTRANGE                
*                                                                               
         CLI   SVACCNUM,0          ANY ENTRIES IN THE TABLE?                    
         BE    NXTPL40                                                          
*                                                                               
         LA    RE,CNTACT+L'CNTACT-1                                             
         LHI   R1,L'CNTACT                                                      
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
*                                                                               
         USING LDGTABD,RE                                                       
         LA    RE,CSTLDBLK         R1=A(LEDGER VALUES BLOCK)                    
         LA    RF,LDGTLVA          START ADDRESS AT LEVEL A LENGTH              
         DROP  RE                                                               
*                                                                               
         LHI   R0,4                MAXIMUM 4 LEVELS                             
         SR    RE,RE                                                            
NXTPL30  IC    RE,0(RF)                                                         
         CR    R1,RE                                                            
         BH    *+10                                                             
         LR    R1,RE                                                            
         B     *+12                                                             
         AHI   RF,1                                                             
         BCT   R0,NXTPL30                                                       
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PL1CRNGE(0),CNTACT                                               
         EX    R1,*+8                                                           
         B     NXTPL40                                                          
         MVC   PL1CRNGE+L'ACTKACT(0),CNTACT                                     
*                                                                               
NXTPL40  MVC   CURUL,LEDGER1R      UNIT/LEDGER                                  
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,PERLDBLK                            
*                                                                               
         USING LEDGTBD,R4                                                       
         LA    R4,LEDGTAB          POINT TO LEDGER TABLE                        
NXTPL50  CLI   0(R4),FF            EOT?                                         
         BE    NXTPL100                                                         
         CLC   LEDGUL,LEDGER1R     DONT BOTHER DOING FOR 1R OR 1C               
         BE    NXTPL60                                                          
         CLC   LEDGUL,LEDGER1C                                                  
         BE    NXTPL60                                                          
         MVC   CURUL,LEDGUL                                                     
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,CNTLDBLK                            
*                                                                               
         USING LDGTABD,RE                                                       
NXTPL60  LA    RE,CNTLDBLK         RE=A(LEDGER VALUES BLOCK)                    
         CLC   LEDGUL,LEDGER1R     UNIT/LEDGER                                  
         BNE   *+8                                                              
         LA    RE,PERLDBLK         RE=A(PERSON LEDGER BLOCK)                    
         CLC   LEDGUL,LEDGER1C     UNIT/LEDGER                                  
         BNE   *+8                                                              
         LA    RE,CSTLDBLK         RE=A(COSTING LEDGER BLOCK)                   
         LA    R1,LEDGLVL          FOR ALL LEDGERS MOVE IN ALL LEVELS           
         TM    LEDGSTAT,LEDG3LV    EXCEPT THOSE FORCED TO 3 LEVELS              
         BNO   *+8                                                              
         LHI   R1,3                                                             
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LEDGLEVS(0),LDGTLVA LEDGER LEVELS                                
*                                                                               
         TM    LEDGSTAT,LEDGOR     DID WE HAVE AN OFF DISP OVERRIDE             
         BNO   *+12                                                             
         TM    LEDGSTAT,LEDGPOR    PUT IN DISPL FOR PARTIAL OVERRIDES           
         BNO   NXTPL80                                                          
         CLC   LEDGUL,LEDGER11                                                  
         BL    *+20                                                             
         CLC   LEDGUL,LEDGER12                                                  
         BH    *+10                                                             
         MVC   LEDGOFFP,LDGTOFFP                                                
*                                                                               
         CLI   LDGTOFFP,LDGONKHI                                                
         BNL   NXTPL70             OFFICE NOT IN KEY                            
         MVC   BYTE1,LDGTOFFP                                                   
         NI    BYTE1,FF-LDGOKEY2   DON'T CARE IF ITS 2 BYTES                    
         MVC   LEDGOFFP,BYTE1                                                   
         B     NXTPL80             GET NEXT                                     
*                                                                               
NXTPL70  CLC   LEDGUL,LEDGER1C     GET OFFPOS FOR U/L 1C                        
         BNE   NXTPL80                                                          
         CLI   LDGTOFFP,C'1'       CHECK IF IN FILTERS                          
         BL    NXTPL80                                                          
         OI    LEDGSTAT,LEDGOFLT   SHOW THAT OFFICE IS BY FILTER                
         MVC   BYTE1,LDGTOFFP                                                   
         NI    BYTE1,X'0F'         TURN OFF THE HIGH ORDER BITS                 
         MVC   LEDGOFFP,BYTE1      SET FILTER NUMBER IN OFFPOS                  
         DROP  RE                                                               
*                                                                               
NXTPL80  CLC   LEDGUL,LEDGER13     SPECIAL CODE FOR U/L 13                      
         BNE   NXTPL90                                                          
         GOTOR GETLEVS,LEDGLEVS                                                 
         MVC   LEDGLEV#,LEVNUM     SAVE OFF LEVEL NUMBER                        
         CLI   LEDGOFFP,0          DID WE GET AN OFFPOS FOR 13                  
         BNE   NXTPL90             YES- LEAVE AS IS                             
         LA    RF,LEDGLEVB         ASSUME 4 LEVEL                               
         CLI   LEDGLEV#,2          IF ONLY 1 LEVEL FORGET IT                    
         BL    NXTPL90                                                          
         CLI   LEDGLEV#,3          THREE OR LESS                                
         BH    *+12                NO                                           
         LA    RF,LEDGLEVA         START OFF/DEPT AT 2ND LEVEL                  
         MVI   LEDGLEV#,3          SET LEVELS TO 3 WHETHER 2 OR 3               
         LLC   RE,0(RF)                                                         
         AHI   RE,1                START AT 1ST POS OF NEXT LEV                 
         STC   RE,LEDGOFFP         SET OFFPOS IN TABLE                          
*                                                                               
NXTPL90  AHI   R4,LEDGLNQ                                                       
         B     NXTPL50                                                          
         DROP  R4                                                               
*                                                                               
NXTPL100 GOTOR CNVLDG,0            CONVERT LEDGERS                              
         LA    R1,CLIAMTS                                                       
         LHI   R0,CLIAMLNQ                                                      
         ZAP   0(L'CLIAMTS,R1),=P'0'                                            
         AHI   R1,L'CLIAMTS                                                     
         BCT   R0,*-10                                                          
*                                                                               
         CLI   BUDIND,0            DID THEY GIVE US ANY BUDGETS                 
         BE    NXTPL110            NO DEFAULT TO ALLOC METHOD                   
         CLI   MTHIND,0            DID THEY GIVE US BOTH                        
         BE    NXTPL370            NO DO BUDGETS                                
         J     NOMORE              CAN'T GIVE US BOTH                           
*                                                                               
NXTPL110 TM    LASTFLAG,LASTLAST   TEST PREVIOUS WAS 'LAST TIME'                
         BZ    NXTPL130                                                         
*                                                                               
NXTPL120 L     RE,SVADDR2          RESTORE ADDRESS OF ACCT TABLE                
         AHI   RE,L'CNTACT         BUMP TO NEXT ACCOUNT IN LIST                 
         ST    RE,SVADDR2          SAVE ADDRESS OF NEXT ACCOUNT                 
         LLC   R1,SVACCNUM         N0. OF ENTRIES LEFT IN LIST                  
         AHI   R1,-1                                                            
         JNP   NOMORE                                                           
         MVI   LP_RMODE,LP_RFRST   RESET LP_RNEXT TO FIRST TIME                 
         B     NXTPL10                                                          
*                                                                               
NXTPL130 TM    LASTFLAG,LASTPROC   TEST 'PROCESS PREVIOUS'                      
         BNZ   NXTPL310                                                         
*                                                                               
NXTPL140 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',CLIKEYT),('B#PLAREC',0), +        
               SAVED,0,0                                                        
         BNE   NXTPL350                                                         
*                                                                               
         USING PLCRECD,IOKEY                                                    
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,EXCIND         R1=NO. OF ACCOUNTS IN EXCLUDED LIST          
         BZ    NXTPL180                                                         
         ICM   RE,7,AEXACT         ADDRESS OF EXCLUDED ACCOUNT LIST             
*                                                                               
NXTPL150 LA    RF,L'ACTKACT-1(RE)                                               
         LHI   R1,L'ACTKACT                                                     
NXTPL160 CLI   0(RF),C' '                                                       
         BH    NXTPL170                                                         
         AHI   R1,-1                                                            
         AHI   RF,-1                                                            
         B     NXTPL160                                                         
*                                                                               
NXTPL170 AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),PLCK1CAC    IF THE SAME SKIP                             
         BE    NXTPL140                                                         
         AHI   RE,L'ACTKACT        BUMP TO NEXT ENTRY IN EXCLUDE TABLE          
         BCT   R0,NXTPL150                                                      
*                                                                               
NXTPL180 OC    LIMACC,LIMACC       ANY LIMITED ACCESS?                          
         BZ    NXTPL240                                                         
*                                                                               
         MVC   CURUL,LEDGER1C      DEFAULT IS 1C ACCOUNT                        
         LA    RF,PLCK1CAC                                                      
         CLI   SECACC,C'A'         IF ACCOUNT FILTERING CONTINUE                
         BE    *+18                                                             
         LA    RF,PLCKCACT                                                      
         MVI   CURUL,C'1'                                                       
         MVC   CURUL+1(1),PLCKCLDG                                              
         ST    RF,SVADDR3                                                       
*                                                                               
         USING LEDGTBD,R3                                                       
         LA    R3,LEDGTAB          R3=A(LEDGER VALUES BLOCK)                    
NXTPL190 CLI   0(R3),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CURUL,LEDGUNT       GET LEDGER INFO                              
         BE    *+12                                                             
         AHI   R3,LEDGLNQ                                                       
         B     NXTPL190                                                         
*                                                                               
         CLC   LEDGUL,LEDGER1C     ONLY CHECK FILTERS FOR 1C                    
         BNE   NXTPL200                                                         
         TM    LEDGSTAT,LEDGOFLT   SHOULD WE CHECK THE FILTER?                  
         BNO   NXTPL200                                                         
*                                                                               
         GOTOR GETFLT                                                           
         BNE   NXTPL200            COULD FINE FILTER-SKIP                       
         LA    RF,BYTE             OFFICE IS RETURNED IN BYTE                   
         B     NXTPL220                                                         
*                                                                               
NXTPL200 L     RF,SVADDR3          GET TO OFFICE IN KEY                         
         CLC   LEDGUL,LEDGER11                                                  
         BL    NXTPL210                                                         
         CLC   LEDGUL,LEDGER12                                                  
         BH    NXTPL210                                                         
         CLI   LEDGOFFP,C'T'       IS THE OFFICE IN THE TRANSACTION?            
         BNE   NXTPL210                                                         
         LA    RF,PLCKAGYO                                                      
         B     NXTPL220                                                         
*                                                                               
NXTPL210 SR    R1,R1                                                            
         ICM   R1,1,LEDGOFFP       GET LEVEL                                    
         BZ    NXTPL220                                                         
         AHI   R1,-1                                                            
         AR    RF,R1                                                            
*                                                                               
NXTPL220 LHI   R1,1                                                             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         LHI   R1,2                                                             
         AHI   R1,-1                                                            
         LLH   R0,OFFNUM                                                        
         LA    RE,OFFTAB           ONLY SEND DOWN $ WITHIN LIMACC               
NXTPL230 CLI   0(RE),FF            IF NOT IN TABLE-SKIP                         
         BE    NXTPL140                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       MATCH ON OFFICE                              
         BE    NXTPL240                                                         
         LA    RE,1(R1,RE)                                                      
         BCT   R0,NXTPL230                                                      
         B     NXTPL140                                                         
*                                                                               
NXTPL240 SR    R0,R0               CHECK SUB LIST NOW                           
         ICM   R0,1,SUBIND                                                      
         BZ    NXTPL290                                                         
         SR    RE,RE               ONLY SEND DOWN $ WITHIN SUBLST               
         ICM   RE,7,ASUB                                                        
*                                                                               
         LA    RF,PLCKCACT                                                      
         MVI   CURUL,C'1'                                                       
         MVC   CURUL+1(1),PLCKCLDG                                              
*                                                                               
         USING LEDGTBD,R3                                                       
         LA    R3,LEDGTAB          R3=A(LEDGER VALUES BLOCK)                    
NXTPL250 CLI   0(R3),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CURUL,LEDGUNT       GET LEDGER INFO                              
         BE    *+12                                                             
         AHI   R3,LEDGLNQ                                                       
         B     NXTPL250                                                         
*                                                                               
         CLC   CURUL,LEDGER11                                                   
         BL    NXTPL260                                                         
         CLC   CURUL,LEDGER12                                                   
         BH    NXTPL260                                                         
         CLI   LEDGOFFP,C'T'                                                    
         BNE   NXTPL260                                                         
         LA    RF,PLCKAGYO                                                      
         B     NXTPL270                                                         
*                                                                               
NXTPL260 SR    R1,R1                                                            
         ICM   R1,1,LEDGOFFP       GET LEVEL                                    
         BZ    *+8                                                              
         AHI   R1,-1                                                            
         AR    RF,R1                                                            
*                                                                               
NXTPL270 LHI   R1,1                                                             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         LHI   R1,2                                                             
         AHI   R1,-1                                                            
NXTPL280 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)                                                    
         BE    NXTPL290                                                         
         LA    RE,1(R1,RE)                                                      
         BCT   R0,NXTPL280                                                      
         B     NXTPL140                                                         
*                                                                               
         USING LEDGTBD,R3                                                       
NXTPL290 LA    R3,LEDGTAB          R3=A(LEDGER VALUES BLOCK)                    
NXTPL300 CLI   0(R3),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LEDGLDG,PLCKCLDG    MATCH ON LEDGER                              
         BE    *+12                                                             
         AHI   R3,LEDGLNQ                                                       
         B     NXTPL300                                                         
*                                                                               
         GOTOR GETLEVS,LEDGLEVS                                                 
         MVC   LEDGLEV#,LEVNUM     SAVE OFF LEVEL NUMBER                        
         DROP  R3                                                               
*                                                                               
         LA    R1,PLCKCACT-PLCKEY                                               
         LLC   RE,LEVLNQA                                                       
         AR    R1,RE                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PLCKEY(0),LASTKEY                                                
         BE    NXTPL340                                                         
         OC    LASTKEY,LASTKEY     TEST 'FIRST TIME'                            
         BZ    NXTPL310                                                         
         OI    LASTFLAG,LASTPROC   SET 'PROCESS PREVIOUS' FOR NEXT TIME         
         B     NXTPL360            EXIT TO SEND CURRENT                         
*                                                                               
NXTPL310 MVC   LASTKEY,PLCKEY      SAVE CURRENT KEY                             
         MVI   LASTFLAG,0          RESET FLAG                                   
*                                                                               
         GOTOR CLROUT,DMCB,PLAVALS,PLAVALL    CLEAR OUTPUT VALUES               
*                                                                               
         TM    FLAG,FLGREC         DID WE PROCESS A RECD?                       
         BO    *+8                                                              
         MVI   PLATYP,C'M'         M-METHOD                                     
*                                                                               
         CLC   LASTMTH,PLCKMTHD    SAME METHOD AS BEFORE?                       
         BE    *+10                                                             
         MVC   PLANO(L'PLCKMTHD),PLCKMTHD     METHOD CODE                       
         MVC   LASTMTH,PLCKMTHD                                                 
*                                                                               
         CLC   LASTOFF,PLACLIO     SAME OFFICE AS PREVIOUS?                     
         BNE   *+10                                                             
         XC    PLACLIO,PLACLIO     CLEAR OUT OFFICE                             
         MVC   LASTOFF,PLACLIO     SAVE OFFICE FOR LATER COMPARE                
*                                                                               
         CLC   LASTACT,PLCK1CAC    SAME ACCOUNT AS PREVIOUS?                    
         BE    NXTPL320                                                         
*                                                                               
         USING LDGTABD,R3                                                       
         LA    R3,CSTLDBLK         R1=A(LEDGER VALUES BLOCK)                    
         MVC   LASTACT,PLCK1CAC    SAVE OFF ACCOUNT FOR LATER COMPARES          
         GOTOR GETLEVS,LDGTLVA     R1=A(LEDGER VALUES BLOCK)                    
         GOTOR SETCDE,PLCK1CAC                                                  
         JNE   EXITY                                                            
*                                                                               
         MVC   PLAACL1,LEVACDE     MOVE LEV A CODE INTO OUTPUT                  
         MVC   PLAACL2,LEVBCDE     MOVE LEV B CODE INTO OUTPUT                  
         MVC   PLAACL3,LEVCCDE     MOVE LEV C CODE INTO OUTPUT                  
         MVC   PLAACL4,LEVDCDE     MOVE LEV D CODE INTO OUTPUT                  
*                                                                               
         USING LEDGTBD,R3                                                       
NXTPL320 LA    R3,LEDGTAB          R3=A(LEDGER VALUES BLOCK)                    
NXTPL330 CLI   0(R3),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LEDGLDG,PLCKCLDG    MATCH ON LEDGER                              
         BE    *+12                                                             
         AHI   R3,LEDGLNQ                                                       
         B     NXTPL330                                                         
*                                                                               
         MVC   CURUL,LEDGUL        SAVE OFF UNIT/LEDGER                         
         GOTOR GETLEVS,LEDGLEVS                                                 
         MVC   LEDGLEV#,LEVNUM     SAVE OFF LEVEL NUMBER                        
         DROP  R3                                                               
*                                                                               
         CLC   LASTUL,CURUL        SAME UNIT/LEDGER AS BEFORE?                  
         BE    *+14                                                             
         MVC   PLACLDG,PLCKCLDG    CONTRA LEDGER CODE                           
         B     *+14                                                             
         CLC   LASTCACT,PLCKCACT   SAME CONTRA ACCOUNT AS PREVIOUS?             
         BE    NXTPL340                                                         
*                                                                               
         GOTOR SETCDE,PLCKCACT                                                  
         JNE   EXITY                                                            
*                                                                               
         MVC   PLACAL1,LEVACDE     MOVE LEV A CODE INTO OUTPUT                  
*                                                                               
NXTPL340 MVC   LASTUL,CURUL        SAVE IT OFF FOR NEXT COMPARE                 
         MVC   LASTCACT,PLCKCACT   SAVE IT OFF FOR NEXT COMPARE                 
         MVI   PAYTYPE,0                                                        
         GOTOR PUTAMT,DMCB,PLCKYYMM,PLCKAMT                                     
         B     NXTPL140                                                         
*                                                                               
NXTPL350 OC    LASTKEY,LASTKEY     TEST ANY RECORD SAVED                        
         BZ    NXTPL120                                                         
         MVI   LP_RMODE,LP_RNEXT   RESET LP_RLAST SET BY NXTREC                 
         OI    LASTFLAG,LASTLAST   YES - EXIT TO OUTPUT LAST ONE                
*                                                                               
NXTPL360 GOTOR SETAMT,PLAENT       SET AMOUNTS INTO OUTPUT                      
         OI    FLAG,FLGREC         SHOW WE HAVE A RECORD                        
         J     EXITY                                                            
*                                                                               
* DOING BY BUDGET NUMBER                                                        
*                                                                               
NXTPL370 CLI   BUDIND,0            ANY BUDGETS?                                 
         JE    NOMORE                                                           
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST 'FIRST TIME'                            
         BNE   NXTPL380                                                         
         MVI   CNTRANGE,C' '       CLEAR FIRST BYTE TO INCLUDE LEDGER           
         XC    LASTS(LASTL),LASTS                                               
*                                                                               
NXTPL380 TM    LASTFLAG,LASTLAST   TEST PREVIOUS WAS 'LAST TIME'                
         BNZ   NXTPL120                                                         
         TM    LASTFLAG,LASTPROC   TEST 'PROCESS PREVIOUS'                      
         BNZ   NXTPL500                                                         
*                                                                               
NXTPL390 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',BGTKEYT),('B#BGTREC',0), +        
               SAVED,0,0                                                        
         BNE   NXTPL630                                                         
*                                                                               
         USING BUDRECD,R2                                                       
         L     R2,IOADDR                                                        
*                                                                               
         USING LDGTABD,R3                                                       
         LA    R3,CSTLDBLK         R1=A(LEDGER VALUES BLOCK)                    
*                                                                               
         USING LEDGTBD,R4                                                       
         LA    R4,LEDGTAB          R4=A(LEDGER VALUES BLOCK)                    
NXTPL400 CLI   0(R4),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LEDGUL,LDGTUL       MATCH ON UNIT/LEDGER                         
         BE    *+12                                                             
         AHI   R4,LEDGLNQ                                                       
         B     NXTPL400                                                         
*                                                                               
         OC    LIMACC,LIMACC       ANY LIMITED ACCESS?                          
         BZ    NXTPL410                                                         
*                                                                               
         TM    LEDGSTAT,LEDGOFLT   SHOULD WE CHECK THE FILTER?                  
         BNO   NXTPL410                                                         
         DROP  R4                                                               
*                                                                               
         GOTOR GETFLT                                                           
         BNE   NXTPL410            COULD FINE FILTER-SKIP                       
         LA    RF,BYTE             OFFICE IS RETURNED IN BYTE                   
         B     NXTPL430                                                         
*                                                                               
NXTPL410 CLI   SECACC,C'C'         ARE WE DOING BY CONTRA ACCOUNT?              
         BNE   NXTPL420                                                         
         GOTOR (#GETLDG,AGETLDG),DMCB,BUDKCUNT,CNTLDBLK                         
         LA    R3,CNTLDBLK         R3=A(LEDGER VALUES BLOCK)                    
*                                                                               
NXTPL420 OC    LDGTOFFP,LDGTOFFP   ANY OFFICE POSITION IN KEY?                  
         BZ    NXTPL450            NO - SKIP LIMIT ACCESS CHECK                 
         CLI   LDGTOFFP,LDGONKHI                                                
         BNL   NXTPL450            OFFICE NOT IN KEY                            
         MVC   BYTE,LDGTOFFP                                                    
         NI    BYTE,FF-LDGOKEY2    DON'T CARE IF ITS 2 BYTES                    
         LLC   R1,BYTE                                                          
         AHI   R1,-1               MAKE THE POSITION A DISPLACEMENT             
*                                                                               
         LA    RF,BUDKACT          RF=A(BUDGET KEY 1C ACCOUNT)                  
         CLI   SECACC,C'C'         IF WE ARE DOING BY CONTRA RF=CONTRA          
         BNE   *+8                                                              
         LA    RF,BUDKCACT         RF=A(BUDGET KEY CONTRA ACCOUNT)              
         AR    RF,R1               BUMP TO OFFICE POSITION IN KEY               
         DROP  R3                                                               
*                                                                               
NXTPL430 LHI   R1,1                                                             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         LHI   R1,2                                                             
*                                                                               
         OC    LIMACC,LIMACC       ANY LIMITED ACCESS?                          
         BZ    NXTPL450                                                         
*                                                                               
         LLH   R0,OFFNUM                                                        
         LA    RE,OFFTAB           ONLY SEND DOWN $ WITHIN LIMACC               
NXTPL440 CLI   0(RE),FF            IF NOT IN TABLE-SKIP                         
         BE    NXTPL390                                                         
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       MATCH ON OFFICE                              
         BE    NXTPL450                                                         
         AHI   R1,1                                                             
         AR    RE,R1                                                            
         BCT   R0,NXTPL440                                                      
         B     NXTPL390                                                         
*                                                                               
NXTPL450 SR    R0,R0                                                            
         ICM   R0,1,SUBIND                                                      
         BZ    NXTPL470                                                         
         SR    RE,RE                                                            
         ICM   RE,7,ASUB           ONLY SEND DOWN $ WITHIN SUBLST               
NXTPL460 AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       MATCH ON OFFICE                              
         BE    NXTPL470                                                         
         AHI   R1,1                                                             
         AR    RE,R1                                                            
         BCT   R0,NXTPL460                                                      
         B     NXTPL390                                                         
*                                                                               
         USING LEDGTBD,R4                                                       
NXTPL470 LA    R4,LEDGTAB          R4=A(LEDGER VALUES BLOCK)                    
NXTPL480 CLI   0(R4),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LEDGLDG,BUDKCLDG    MATCH ON LEDGER                              
         BE    *+12                                                             
         AHI   R4,LEDGLNQ                                                       
         B     NXTPL480                                                         
*                                                                               
         GOTOR GETLEVS,LEDGLEVS    R1=A(LEDGER VALUES BLOCK)                    
         MVC   LEDGLEV#,LEVNUM     SAVE OFF LEVEL NUMBER                        
         DROP  R4                                                               
*                                                                               
         OC    LASTKEY,LASTKEY     TEST FIRST TIME                              
         BZ    NXTPL500                                                         
*                                                                               
         CLC   BUDKEY(L'BUDKTYP+L'BUDKCULA),LASTKEY                             
         BNE   NXTPL490                                                         
         LLC   RE,LEVLNQA          FIRST LEVEL OF CONTRA ACCOUNT                
         AHI   RE,3                ADD 3 FOR C/U/L                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   BUDKCCPY(0),LASTKEY+(BUDKCCPY-BUDKEY)                            
         BE    NXTPL590                                                         
NXTPL490 OI    LASTFLAG,LASTPROC   SET 'PROCESS PREVIOUS' FOR NEXT TIME         
         B     NXTPL640            EXIT TO SEND CURRENT                         
*                                                                               
NXTPL500 L     R2,IOADDR                                                        
         MVC   LASTKEY,BUDKEY      SAVE CURRENT KEY                             
         MVI   LASTFLAG,0          RESET FLAG                                   
*                                                                               
         GOTOR CLROUT,DMCB,PLAVALS,PLAVALL    CLEAR OUTPUT VALUES               
*                                                                               
         TM    FLAG,FLGREC         DID WE PROCESS A RECD?                       
         BO    *+8                                                              
         MVI   PLATYP,C'B'         B-BUDGET                                     
*                                                                               
         CLC   LASTLDG,BUDKCLDG    SAME LEDGER AS BEFORE                        
         BE    *+16                                                             
         MVC   LASTLDG,BUDKCLDG    SAVE OFF CURRENT LEDGER                      
         MVC   PLACLDG,BUDKCLDG    CONTRA LEDGER CODE                           
*                                                                               
         CLC   LASTBUDN,BUDKBUDN   SAME BUDGET NUMBER AS BEFORE                 
         BE    NXTPL510                                                         
         MVC   LASTBUDN,BUDKBUDN   SAVE OFF CURRENT BUD NO.                     
*                                                                               
         OC    BUDKBUDN,BUDKBUDN                                                
         BZ    NXTPL510                                                         
         LLH   R0,BUDKBUDN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  WORK(5),DUB                                                      
         LA    RE,WORK             ELIMINATE LEADING ZEROES                     
         LHI   R1,5                                                             
         CLI   0(RE),C'0'                                                       
         BH    *+12                                                             
         AHI   RE,1                                                             
         BCT   R1,*-12                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     NXTPL510                                                         
         MVC   PLANO(0),0(RE)                                                   
*                                                                               
NXTPL510 CLC   LASTACT,BUDKACT     SAME ACCOUNT AS PREVIOUS?                    
         BE    NXTPL570                                                         
*                                                                               
         USING LDGTABD,R3                                                       
         LA    R3,CSTLDBLK         R1=A(LEDGER VALUES BLOCK)                    
         MVC   LASTACT,BUDKACT     SAVE OFF ACCOUNT FOR LATER COMPARES          
         GOTOR GETLEVS,LDGTLVA     R1=A(LEDGER VALUES BLOCK)                    
*                                                                               
         GOTOR SETCDE,BUDKACT                                                   
         JNE   EXITY                                                            
*                                                                               
         CLC   LASTLV1,LEVACDE     COMPARE LEV 1 CODE                           
         BNE   NXTPL520                                                         
         CLC   LASTLV2,LEVBCDE     COMPARE LEV 2 CODE                           
         BNE   NXTPL530                                                         
         CLC   LASTLV3,LEVCCDE     COMPARE LEV 3 CODE                           
         BNE   NXTPL540                                                         
         CLC   LASTLV4,LEVDCDE     COMPARE LEV 4 CODE                           
         BNE   NXTPL550                                                         
         B     NXTPL560                                                         
*                                                                               
NXTPL520 MVC   PLAACL1,LEVACDE     MOVE LEV A CODE INTO OUTPUT                  
NXTPL530 MVC   PLAACL2,LEVBCDE     MOVE LEV B CODE INTO OUTPUT                  
NXTPL540 MVC   PLAACL3,LEVCCDE     MOVE LEV C CODE INTO OUTPUT                  
NXTPL550 MVC   PLAACL4,LEVDCDE     MOVE LEV D CODE INTO OUTPUT                  
*                                                                               
NXTPL560 MVC   LASTLV1,LEVACDE                                                  
         MVC   LASTLV2,LEVBCDE                                                  
         MVC   LASTLV3,LEVBCDE                                                  
         MVC   LASTLV4,LEVDCDE                                                  
*                                                                               
NXTPL570 MVC   CURUL,BUDKCUNT                                                   
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,CNTLDBLK                            
*                                                                               
         LA    R3,CNTLDBLK         R3=A(LEDGER VALUES BLOCK)                    
         GOTOR GETLEVS,LDGTLVA     R1=A(LEDGER VALUES BLOCK)                    
         GOTOR SETCDE,BUDKCACT                                                  
         JNE   EXITY                                                            
*                                                                               
         CLC   LASTCLV1,LEVACDE    COMPARE LEV 1 CODE                           
         BE    NXTPL580                                                         
         MVC   PLACAL1,LEVACDE     MOVE LEV A CODE INTO OUTPUT                  
         DROP  R3                                                               
*                                                                               
NXTPL580 MVC   LASTCLV1,LEVACDE                                                 
*                                                                               
NXTPL590 LA    R3,BUDRFST          POINT TO FIRST ELEM                          
NXTPL600 CLI   0(R3),0                                                          
         BE    NXTPL390                                                         
         CLI   0(R3),BAMELQ        IS THIS A BUCKET ELEM                        
         BE    NXTPL620                                                         
NXTPL610 LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     NXTPL600                                                         
*                                                                               
         USING BAMELD,R3                                                        
NXTPL620 CLC   BAMMNTH,STRMOA                                                   
         BL    NXTPL610                                                         
         CLC   BAMMNTH,ENDMOA                                                   
         BH    NXTPL610                                                         
         ZAP   PKAMT,BAMBUDG                                                    
         MVI   PAYTYPE,0                                                        
         GOTOR PUTAMT,DMCB,BAMMNTH,PKAMT                                        
         B     NXTPL610                                                         
*                                                                               
NXTPL630 OC    LASTKEY,LASTKEY     TEST ANY RECORD SAVED                        
         BZ    NXTPL120                                                         
         MVI   LP_RMODE,LP_RNEXT   RESET LP_RLAST SET BY NXTREC                 
         OI    LASTFLAG,LASTLAST   YES - EXIT TO OUTPUT LAST ONE                
*                                                                               
NXTPL640 GOTOR SETAMT,PLAENT       SET AMOUNTS INTO OUTPUT                      
         OI    FLAG,FLGREC         SHOW WE HAVE FOUND A RECORD                  
         J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* READ CONTRA ACCOUNT RECORDS                                         *         
***********************************************************************         
         SPACE 1                                                                
NXTCNT   J     *+12                                                             
         DC    C'*NXTCNT*'                                                      
         LR    RB,RF                                                            
         USING NXTCNT,RB                                                        
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTCN130                                                         
*                                                                               
         L     RE,LP_AWMP                                                       
         STCM  RE,7,ASUBLST        SET UP ADDRESS OF SUBLST                     
         AHI   RE,255+LW_LN2Q                                                   
         ST    RE,LP_AWMP                                                       
*                                                                               
         LA    R0,CNTKEYT          USE SPACE PADDED KEY FOR 11-13               
         CLC   CNTUL,LEDGER13                                                   
         BNH   *+8                                                              
         LA    R0,CNTKYT2          USE KEY W/MTH AND PAY TYP FOR 14-16          
         ST    R0,SVADDR                                                        
*                                                                               
         CLC   CNTUL,LEDGER11                                                   
         BL    NXTCN60                                                          
         CLC   CNTUL,LEDGER12                                                   
         BH    NXTCN60                                                          
         LA    R0,CNTKYT3          IF SUBLST IS PASSED FOR 11/12                
         ST    R0,SVADDR                                                        
*                                                                               
         USING LW_D,RE                                                          
         SR    RE,RE                                                            
         ICM   RE,7,ASUBLST        DEFAULT ENTRY IS ALL                         
         MVI   LW_TYPE,LW_TRNGQ                                                 
         MVC   LW_DATA1(2),=X'41FF'                                             
         LHI   R0,LW_LN1Q+(1*2)                                                 
         STCM  R0,3,LW_LN          1 BYTE OFFICE DEFAULT IS 10                  
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   NXTCN60                                                          
         MVC   LW_DATA1(4),=X'4041FFFF'                                         
         LHI   R0,LW_LN1Q+(2*2)                                                 
         STCM  R0,3,LW_LN          2 BYTE OFFICE DEFAULT IS 12                  
         DROP  RE                                                               
*                                                                               
NXTCN10  CLI   SECACC,C'C'                                                      
         BE    NXTCN20                                                          
         SR    R1,R1                                                            
         ICM   R1,1,SUBIND                                                      
         BZ    NXTCN60                                                          
         SR    RF,RF                                                            
         ICM   RF,7,ASUB                                                        
         B     NXTCN40                                                          
*                                                                               
NXTCN20  SR    R1,R1                                                            
         ICM   R1,1,SUBIND                                                      
         BZ    NXTCN30                                                          
         SR    RF,RF                                                            
         ICM   RF,7,ASUB                                                        
         B     NXTCN40                                                          
*                                                                               
NXTCN30  ICM   R1,3,OFFNUM                                                      
         BZ    NXTCN60                                                          
         LA    RF,OFFLST                                                        
*                                                                               
         USING LW_D,RE                                                          
NXTCN40  SR    RE,RE                                                            
         ICM   RE,7,ASUBLST                                                     
         MVI   LW_TYPE,LW_TLSTQ                                                 
         STCM  R1,3,LW_NUMN                                                     
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         SLL   R1,1                                                             
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LW_DATA2(0),0(RF)                                                
         AHI   R1,LW_LN2Q+1                                                     
         STCM  R1,3,LW_LN                                                       
         DROP  RE                                                               
*                                                                               
NXTCN60  XC    LASTS(LASTL),LASTS                                               
         NI    FLAG,FF-FLGREC                                                   
*                                                                               
         GOTOR (#GETLDG,AGETLDG),DMCB,CCOSTLDG,CSTLDBLK                         
         USING LDGTABD,R3                                                       
         LA    R3,CSTLDBLK         R1=A(LEDGER VALUES BLOCK)                    
         MVC   CSTLEVS,LDGTLVA     COSTING LEDGER LEVS LENGTHS                  
*                                                                               
         GOTOR (#GETLDG,AGETLDG),DMCB,CNTUL,CNTLDBLK                            
         LA    R3,CNTLDBLK         R3=A(LEDGER VALUES BLOCK)                    
         MVC   CNTRLEVS,LDGTLVA    CONTRA LEDGER LEVS LENGTHS                   
*                                                                               
         USING LEDGTBD,R2                                                       
         LA    R2,LEDGTAB          CHECK IF ANY OVERRIDES IN EFFECT             
NXTCN70  CLI   0(R2),FF            EOF?                                         
         BE    NXTCN100                                                         
         CLC   CNTUL,LEDGUNT       SAME U/L?                                    
         BNE   *+12                                                             
         TM    LEDGSTAT,LEDGOR     OVERRIDE IN EFFECT?                          
         BO    *+12                                                             
         AHI   R2,LEDGLNQ                                                       
         B     NXTCN70                                                          
*                                                                               
         MVC   LEDGLEVS(LEDGLVL),LDGTLVA    MOVE IN LEVELS INTO TABLE           
         CLC   LEDGUL,LEDGER13     SPECIAL CODE FOR U/L 13                      
         BNE   NXTCN90                                                          
         GOTOR GETLEVS,LEDGLEVS                                                 
         MVC   LEDGLEV#,LEVNUM     SAVE OFF LEVEL NUMBER                        
         CLC   LP_VRSN1,V12012     SPECIAL 13 LOGIC FOR VERSIONS                
         BH    NXTCN80             12 OR EARLIER                                
         MVI   LDGOFFP,0           SET OFF POS AS DEFAULT=0                     
         LLC   RE,LEVNUM                                                        
         CHI   RE,1                IF 1 LEVEL NO OFF POS                        
         BE    NXTCN90                                                          
         CHI   RE,2                IF 2 LEVELS OFF POS IS 2ND LEVEL             
         BE    *+8                                                              
         AHI   RE,-1               FOR ALL ELSE BUMP BACK 1 LEVEL               
         STC   RE,LDGOFFP                                                       
         B     NXTCN90                                                          
*                                                                               
NXTCN80  CLI   LEDGOFFP,0          DID WE GET AN OFFPOS FOR 13                  
         BNE   NXTCN90             YES - LEAVE AS IS                            
         LA    RF,LEDGLEVB         ASSUME 4 LEVEL                               
         CLI   LEDGLEV#,2          IF ONLY 1 LEVEL FORGET IT                    
         BL    NXTCN90                                                          
         CLI   LEDGLEV#,3          THREE OR LESS                                
         BH    *+12                NO                                           
         LA    RF,LEDGLEVA         START OFF/DEPT AT 2ND LEVEL                  
         MVI   LEDGLEV#,3          SET LEVELS TO 3 WHETHER 2 OR 3               
         LLC   RE,0(RF)                                                         
         AHI   RE,1                START AT 1ST POS OF NEXT LEV                 
         STC   RE,LEDGOFFP         SET OFFPOS IN TABLE                          
*                                                                               
NXTCN90  GOTOR CNVLDG,CNTUL        CONVERT LEDGERS                              
         MVC   CNTRLEVS,LEDGLEVS   CONTRA LEDGER LEVS LENGTHS                   
         DROP  R2,R3                                                            
*                                                                               
NXTCN100 ICM   RE,7,AULA           POINT TO UNIT/LEDGER/ACCOUNT LIST            
         SR    R1,R1                                                            
         ICM   R1,1,ULAIND         R1=NO. OF U/L/ACCOUNTS IN LIST               
         BZ    *+10                                                             
NXTCN110 MVC   ACTULA,0(RE)                                                     
         ST    RE,SVADDR2                                                       
         STC   R1,SVACCNUM                                                      
*                                                                               
         MVC   CACRANGE,ACTRANGE   INIT 1C ACCOUNT RANGE AS ACTRANGE            
         MVC   CNTRANGE,ACTRANGE   INIT CNTRANGE AS ACTRANGE                    
*                                                                               
         XC    LASTS(LASTL),LASTS                                               
*                                                                               
         LA    RE,ACTACT+L'ACTACT-1                                             
         LHI   R1,L'ACTACT                                                      
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
*                                                                               
         LTR   R1,R1                                                            
         BZ    NXTCN120                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CACRANGE(0),ACTACT                                               
         EX    R1,*+8                                                           
         B     NXTCN120                                                         
         MVC   CACRANGE+L'ACTKACT(0),ACTACT                                     
*                                                                               
NXTCN120 LA    RE,CNTACT+L'CNTACT-1                                             
         LHI   R1,L'CNTACT                                                      
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
*                                                                               
         LTR   R1,R1                                                            
         BZ    NXTCN130                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CNTRANGE(0),CNTACT                                               
         EX    R1,*+8                                                           
         B     NXTCN130                                                         
         MVC   CNTRANGE+L'ACTKACT(0),CNTACT                                     
*                                                                               
NXTCN130 GOTOR CLROUT,DMCB,CNTVALS,CNTVALL  CLEAR OUTPUT VALUES                 
*                                                                               
         LA    R1,CLIAMTS                                                       
         LHI   R0,CLIAMLNQ                                                      
         ZAP   0(L'CLIAMTS,R1),=P'0'                                            
         AHI   R1,L'CLIAMTS                                                     
         BCT   R0,*-10                                                          
*                                                                               
         CLI   BUDIND,0            DID THEY GIVE US ANY BUDGETS                 
         BE    NXTCN140            NO DEFAULT TO ALLOC METHOD                   
         CLI   MTHIND,0            DID THEY GIVE US BOTH                        
         BE    NXCBG00             NO DO CONTRA BUDGETS                         
         J     NOMORE              CAN'T GIVE US BOTH                           
*                                                                               
NXTCN140 TM    LASTFLAG,LASTLAST   DID WE OUTPUT THE LAST RECORD?               
         BZ    NXTCN160                                                         
*                                                                               
NXTCN150 L     RE,SVADDR2          RESTORE ADDRESS OF U/L/A TABLE               
         AHI   RE,L'ACTULA         BUMP TO NEXT U/L/A IN LIST                   
         ST    RE,SVADDR2          SAVE ADDRESS OF NEXT ACCOUNT                 
         LLC   R1,SVACCNUM         N0. OF ENTRIES LEFT IN LIST                  
         AHI   R1,-1                                                            
         JNP   NOMORE                                                           
         MVI   LP_RMODE,LP_RFRST   RESET LP_RNEXT TO FIRST TIME                 
         B     NXTCN110                                                         
*                                                                               
NXTCN160 TM    LASTFLAG,LASTPROC   WAS LAST REC PROCESSED                       
         BNO   NXTCN170                                                         
         NI    LASTFLAG,FF-LASTPROC                                             
         B     NXTCN180                                                         
*                                                                               
NXTCN170 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',SVADDR),('B#CNTREC',0),  +        
               SAVED,0,0                                                        
         BNE   NXTCN460                                                         
NXTCN180 LA    R0,CNTVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
*                                                                               
         USING CACRECD,R2                                                       
         L     R2,IOADDR                                                        
*                                                                               
         CLC   CNTUL,LEDGER11                                                   
         BE    NXTCN260                                                         
         CLC   CNTUL,LEDGER12                                                   
         BE    NXTCN260                                                         
*                                                                               
         OC    LIMACC,LIMACC       ANY LIMITED ACCESS                           
         BZ    NXTCN230                                                         
*                                                                               
         USING LDGTABD,R3                                                       
         LA    R3,CSTLDBLK         R1=A(LEDGER VALUES BLOCK)                    
         LA    RF,CACKUNT          POINT TO ACCOUNT U/L                         
         CLI   SECACC,C'A'         ARE WE DOING BY ACCOUNT?                     
         BE    NXTCN190            CONTINUE AS NORMAL                           
*                                                                               
         LA    R3,CNTLDBLK         R3=A(LEDGER VALUES BLOCK)                    
         LA    RF,CACKCUNT         POINT TO CONTRA U/L                          
         CLC   CNTUL,LEDGER13                                                   
         BL    NXTCN260                                                         
         CLC   CNTUL,LEDGER16                                                   
         BH    NXTCN260                                                         
*                                                                               
NXTCN190 MVI   BYTE1,0                                                          
         CLI   LDGTOFFP,LDGONKHI                                                
         BNL   NXTCN200            OFFICE NOT IN KEY                            
         MVC   BYTE1,LDGTOFFP                                                   
         NI    BYTE1,FF-LDGOKEY2   DON'T CARE IF ITS 2 BYTES                    
*                                                                               
         USING LEDGTBD,R4                                                       
NXTCN200 LA    R4,LEDGTAB          CHECK IF ANY OVERRIDES IN EFFECT             
NXTCN210 CLI   0(R4),FF            EOF?                                         
         BE    NXTCN260                                                         
         CLC   LEDGUL,0(RF)        SAME U/L?                                    
         BE    *+12                                                             
         AHI   R4,LEDGLNQ                                                       
         B     NXTCN210                                                         
*                                                                               
         TM    LEDGSTAT,LEDGOR     OVERRIDE IN EFFECT?                          
         BNO   *+10                                                             
         MVC   BYTE1,LEDGOFFP      MOVE IN OVERRIDE OFFICE POSITION             
         DROP  R4                                                               
*                                                                               
         CLI   BYTE1,0             DID WE GET A DISPLACEMENT                    
         BE    NXTCN230            NO OFF POS - SKIP                            
         LA    RE,CACKACT          POINT TO ACCOUNT                             
         CLI   SECACC,C'A'                                                      
         BE    *+8                                                              
         LA    RE,CACKCACT         POINT TO CONTRA ACCOUNT                      
         DROP  R3                                                               
*                                                                               
         LLC   R1,BYTE1                                                         
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R1                                                            
         LLH   R0,OFFNUM                                                        
         LA    RF,OFFTAB                                                        
         LHI   R1,1                                                             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         LHI   R1,2                                                             
         AHI   R1,-1                                                            
NXTCN220 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)                                                    
         BE    NXTCN230                                                         
         LA    RF,1(R1,RF)                                                      
         BCT   R0,NXTCN220                                                      
         B     NXTCN170            SKIP THIS RECORD                             
*                                                                               
NXTCN230 SR    R1,R1                                                            
         ICM   R1,1,SUBIND                                                      
         BZ    NXTCN260            DONE IF NO SUBLST ENTERED                    
*                                                                               
         USING LDGTABD,R3                                                       
         LA    R3,CNTLDBLK         R3=A(LEDGER VALUES BLOCK)                    
         MVI   BYTE1,0                                                          
         CLI   LDGTOFFP,LDGONKHI                                                
         BNL   *+14                OFFICE NOT IN KEY                            
         MVC   BYTE1,LDGTOFFP                                                   
         NI    BYTE1,FF-LDGOKEY2   DON'T CARE IF ITS 2 BYTES                    
*                                                                               
         USING LEDGTBD,R4                                                       
         LA    R4,LEDGTAB          CHECK IF ANY OVERRIDES IN EFFECT             
NXTCN240 CLI   0(R4),FF            EOF?                                         
         BE    NXTCN260                                                         
         CLC   LEDGUL,CACKCUNT     SAME U/L?                                    
         BE    *+12                                                             
         AHI   R4,LEDGLNQ                                                       
         B     NXTCN240                                                         
*                                                                               
         TM    LEDGSTAT,LEDGOR     OVERRIDE IN EFFECT?                          
         BNO   *+10                                                             
         MVC   BYTE1,LEDGOFFP      MOVE IN OVERRIDE OFFICE POSITION             
         DROP  R4                                                               
*                                                                               
         CLI   BYTE1,0             DID WE GET A DISPLACEMENT                    
         BE    NXTCN260            NO OFF POS - SKIP                            
         LA    RE,CACKCACT         POINT TO CONTRA ACCOUNT                      
         DROP  R3                                                               
*                                                                               
         LLC   R1,BYTE1                                                         
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R1                                                            
         LHI   R1,1                                                             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         LHI   R1,2                                                             
         LLC   R0,SUBIND                                                        
         SR    RF,RF                                                            
         ICM   RF,7,ASUB                                                        
         AHI   R1,-1                                                            
NXTCN250 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)                                                    
         BE    NXTCN260                                                         
         LA    RF,1(R1,RF)                                                      
         BCT   R0,NXTCN250                                                      
         B     NXTCN170            SKIP THIS RECORD                             
*                                                                               
NXTCN260 TM    FLAG,FLGREC         DID WE PROCESS A RECD?                       
         BO    *+8                                                              
         MVI   CNTTYP,C'M'         M-METHOD                                     
*                                                                               
         GOTOR GETLEVS,CSTLEVS     GET INDIV COSTING LEVS LENGTHS               
         GOTOR SETCDE,ACTACT       SET LEVELS FOR REQUESTED ACT                 
         JNE   EXITY                                                            
*                                                                               
         MVC   ACCLEVA,LEVACDE     MOVE IN LEVEL A CODE                         
         MVC   ACCLEVB,LEVBCDE     MOVE IN LEVEL B CODE                         
         MVC   ACCLEVC,LEVCCDE     MOVE IN LEVEL C CODE                         
         MVC   ACCLEVD,LEVDCDE     MOVE IN LEVEL D CODE                         
*                                                                               
         GOTOR SETCDE,CACKACT      SET LEVELS FOR ACTUAL ACT                    
         JNE   EXITY                                                            
*                                                                               
* CHECK THAT EACH LEVEL PASSED BACK IS AN EXACT MATCH FOR THAT                  
* LEVELS REQUESTED                                                              
*                                                                               
         CLC   ACCLEVA,SPACES      WAS ANY LEVEL A REQUESTED?                   
         BE    *+14                NO - SKIP                                    
         CLC   ACCLEVA,LEVACDE     EXACT MATCH?                                 
         BNE   NXTCN170                                                         
         CLC   ACCLEVB,SPACES      WAS ANY LEVEL B REQUESTED?                   
         BE    *+14                NO - SKIP                                    
         CLC   ACCLEVB,LEVBCDE     EXACT MATCH?                                 
         BNE   NXTCN170                                                         
         CLC   ACCLEVC,SPACES      WAS ANY LEVEL C REQUESTED?                   
         BE    *+14                NO - SKIP                                    
         CLC   ACCLEVC,LEVCCDE     EXACT MATCH?                                 
         BNE   NXTCN170                                                         
         CLC   ACCLEVD,SPACES      WAS ANY LEVEL D REQUESTED?                   
         BE    *+14                NO - SKIP                                    
         CLC   ACCLEVD,LEVDCDE     EXACT MATCH?                                 
         BNE   NXTCN170                                                         
*                                                                               
         OC    LASTKEY,LASTKEY                                                  
         BZ    NXTCN270                                                         
         CLC   LASTKEY(CACKSTYP-CACKEY),0(R2)                                   
         BE    NXTCN270                                                         
         MVC   LASTKEY,0(R2)       SAVE KEY FOR NEXT COMPARE                    
         OI    LASTFLAG,LASTPROC   PROCESS  PREVIOUS REC                        
         B     NXTCNTX                                                          
*                                                                               
NXTCN270 MVC   LASTKEY,0(R2)       SAVE KEY FOR NEXT COMPARE                    
*                                                                               
         OC    LSTCN1C(LSTCN1CL),LSTCN1C IS THIS FIRST TIME IN FOR 1C           
         BZ    NXTCN280                                                         
         CLC   LSTCN1CA,LEVACDE    IS LAST SAME AS CURRENT                      
         BNE   NXTCN280                                                         
         CLC   LSTCN1CB,LEVBCDE                                                 
         BNE   NXTCN290                                                         
         CLC   LSTCN1CC,LEVCCDE                                                 
         BNE   NXTCN300                                                         
         CLC   LSTCN1CD,LEVDCDE                                                 
         BNE   NXTCN310                                                         
         B     NXTCN320                                                         
NXTCN280 MVC   CNTLV1CD,LEVACDE    MOVE 1C LEV A CODE INTO OUTPUT               
         MVC   LSTCN1CA,LEVACDE    MOVE 1C LEV A CODE INTO LAST                 
NXTCN290 MVC   CNTLV2CD,LEVBCDE    MOVE 1C LEV B CODE INTO OUTPUT               
         MVC   LSTCN1CB,LEVBCDE    MOVE 1C LEV B CODE INTO LAST                 
NXTCN300 MVC   CNTLV3CD,LEVCCDE    MOVE 1C LEV C CODE INTO OUTPUT               
         MVC   LSTCN1CC,LEVCCDE    MOVE 1C LEV C CODE INTO LAST                 
NXTCN310 MVC   CNTLV4CD,LEVDCDE    MOVE 1C LEV D CODE INTO OUTPUT               
         MVC   LSTCN1CD,LEVDCDE    MOVE 1C LEV D CODE INTO LAST                 
*                                                                               
NXTCN320 OC    LSTCNLDG,LSTCNLDG   IS THIS FIRST TIME TO PRINT LEDGER           
         BZ    NXTCN330                                                         
         CLC   LSTCNLDG,CACKCLDG   IS LAST SAME AS CURRENT                      
         BE    NXTCN340                                                         
NXTCN330 MVC   CNTCLDG,CACKCLDG    MOVE IN CONTRA LEDGER CODE                   
         MVC   LSTCNLDG,CACKCLDG   MOVE IN CONTRA LEDGER INTO LAST              
*                                                                               
NXTCN340 GOTOR GETLEVS,CNTRLEVS    GET INDIV CONTRA LEVS LENGTHS                
         GOTOR SETCDE,CACKCACT                                                  
         JNE   EXITY                                                            
*                                                                               
         OC    LSTCN14(LSTCN14L),LSTCN14 IS THIS FIRST TIME IN FOR 1C           
         BZ    NXTCN350                                                         
         CLC   LSTCN14A,LEVACDE    IS LAST SAME AS CURRENT                      
         BNE   NXTCN350                                                         
         CLC   LSTCN14B,LEVBCDE                                                 
         BNE   NXTCN360                                                         
         CLC   LSTCN14C,LEVCCDE                                                 
         BNE   NXTCN370                                                         
         CLC   LSTCN14D,LEVDCDE                                                 
         BNE   NXTCN380                                                         
         B     NXTCN390                                                         
NXTCN350 MVC   CNTCALV1,LEVACDE    MOVE 1R LEV A CODE INTO OUTPUT               
         MVC   LSTCN14A,LEVACDE    MOVE 1R LEV A CODE INTO LAST                 
NXTCN360 MVC   CNTCALV2,LEVBCDE    MOVE    LEV B CODE INTO OUTPUT               
         MVC   LSTCN14B,LEVBCDE    MOVE 1R LEV B CODE INTO LAST                 
NXTCN370 MVC   CNTCALV3,LEVCCDE    MOVE    LEV C CODE INTO OUTPUT               
         MVC   LSTCN14C,LEVCCDE    MOVE 1R LEV C CODE INTO LAST                 
NXTCN380 MVC   CNTCALV4,LEVDCDE    MOVE    LEV D CODE INTO OUTPUT               
         MVC   LSTCN14D,LEVDCDE    MOVE 1R LEV D CODE INTO LAST                 
*                                                                               
NXTCN390 OC    LSTCNMTH,LSTCNMTH   IS THIS FIRST TIME TO PRINT METHOD           
         BZ    NXTCN400                                                         
         CLC   LSTCNMTH,CACKBTYP   IS LAST SAME AS CURRENT                      
         BE    NXTCN410                                                         
NXTCN400 XC    CNTNO,CNTNO                                                      
         MVC   CNTNO(1),CACKBTYP   MOVE IN METHOD/BUCKET TYPE                   
         MVC   LSTCNMTH,CACKBTYP   MOVE IN METH/BUCKt TYP INTO LAST             
*                                                                               
NXTCN410 LA    R3,CACRFST          POINT TO FIRST ELEM                          
NXTCN420 CLI   0(R3),0                                                          
         BE    NXTCN170                                                         
         CLI   0(R3),BUKELQ        IS IT A BUCKET ELEM                          
         BE    NXTCN440                                                         
NXTCN430 LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     NXTCN420                                                         
*                                                                               
         USING BUKELD,R3                                                        
NXTCN440 CLC   BUKMOS,STRMOA                                                    
         BL    NXTCN430                                                         
         CLC   BUKMOS,ENDMOA                                                    
         BH    NXTCN430                                                         
*                                                                               
         ZAP   PKAMT,BUKDR         AMOUNT IS IN DR FOR US                       
         AP    PKAMT,BUKCR         AMOUNT IS IN CR FOR UK                       
         MVI   PAYTYPE,0                                                        
         CLC   LEDGER16,CACKCUNT   AWAYS FORCE PAYTYPE=0 FOR 16                 
         BE    NXTCN450                                                         
         CLI   CACKSTYP,C' '       NO TYPE MAKE IT ALL SALARY                   
         BNH   NXTCN450                                                         
         MVC   PAYTYPE,CACKSTYP                                                 
         NI    PAYTYPE,X'0F'                                                    
         LLC   R1,PAYTYPE                                                       
         BCTR  R1,0                PAYTYPE IS DISPL INTO AMT ARRAY              
         STC   R1,PAYTYPE                                                       
NXTCN450 GOTOR PUTAMT,DMCB,BUKMOS,PKAMT,PAYTYPE                                 
         B     NXTCN430                                                         
*                                                                               
NXTCN460 OC    LASTKEY,LASTKEY     DID WE GET ANY RECORDS?                      
         BZ    NXTCN150            DO NEXT ENTRY                                
         LA    R0,CNTVALS                                                       
         STCM  R0,15,LP_ADATA      RESET OUTPUT DATA                            
         MVI   LP_RMODE,LP_RNEXT   RESET LP_RLAST SET BY NXTREC                 
         OI    LASTFLAG,LASTLAST   ELSE PUT OUT LAST RECORD                     
         LA    R1,CNTVALS                                                       
         LHI   R0,CNTVALL                                                       
         CLI   0(R1),0                                                          
         BNE   NXTCNTX                                                          
         AHI   R1,1                                                             
         BCT   R0,*-12                                                          
         B     NXTCN150            CHECK FOR NEXT                               
*                                                                               
NXTCNTX  GOTOR SETAMT,CNTENT                                                    
         OI    FLAG,FLGREC         SHOW WE HAVE FOUND A RECORD                  
         J     EXITY                                                            
         DROP  R2,R3                                                            
         J     EXITY                                                            
*                                                                               
* DOING BY BUDGET NUMBER                                                        
*                                                                               
NXCBG00  CLI   BUDIND,0            ANY BUDGETS?                                 
         JE    NOMORE                                                           
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST 'FIRST TIME'                            
         BNE   NXCBG20                                                          
*                                                                               
         LA    R0,BGTKEYT          DEFAULT IS BUDGET ACROSS LEDGERS             
         CLC   CNTUL,SPACES        DID THEY PASS ANY U/L?                       
         BNH   *+8                                                              
         LA    R0,BGTKYT2          USE CONTRA U/L SPECIFIC DRIVER               
         ST    R0,SVADDR                                                        
*                                                                               
         ICM   RE,7,ABUD           POINT TO BUDGET LIST                         
         SR    R1,R1                                                            
         ICM   R1,1,BUDIND         R1=NO. OF BUDGETS IN LIST                    
         BZ    *+10                                                             
NXCBG10  MVC   BUD#,0(RE)                                                       
         ST    RE,SVADDR2                                                       
         STC   R1,SVACCNUM                                                      
*                                                                               
         XC    LASTS(LASTL),LASTS                                               
*                                                                               
NXCBG20  TM    LASTFLAG,LASTLAST   TEST PREVIOUS WAS 'LAST TIME'                
         BZ    NXCBG40                                                          
*                                                                               
NXCBG30  L     RE,SVADDR2          RESTORE ADDRESS OF BUDGET TABLE              
         AHI   RE,L'BUD#           BUMP TO NEXT BUDGET IN LIST                  
         ST    RE,SVADDR2          SAVE ADDRESS OF NEXT BUDGET                  
         LLC   R1,SVACCNUM         N0. OF ENTRIES LEFT IN LIST                  
         AHI   R1,-1                                                            
         JNP   NOMORE                                                           
         MVI   LP_RMODE,LP_RFRST   RESET LP_RNEXT TO FIRST TIME                 
         B     NXCBG10                                                          
*                                                                               
NXCBG40  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',SVADDR),('B#BGTREC',0),  +        
               SAVED,0,0                                                        
         BNE   NXCBG220                                                         
*                                                                               
         LA    R0,CNTVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
*                                                                               
         USING BUDRECD,R2                                                       
         L     R2,IOADDR                                                        
*                                                                               
         GOTOR CLROUT,DMCB,CNTVALS,CNTVALL    CLEAR OUTPUT VALUES               
*                                                                               
         TM    FLAG,FLGREC         DID WE PROCESS A RECD?                       
         BO    *+8                                                              
         MVI   CNTTYP,C'B'         B-BUDGET                                     
*                                                                               
         CLC   LASTBUDN,BUDKBUDN   SAME BUDGET NUMBER AS BEFORE                 
         BE    NXCBG50                                                          
         MVC   LASTBUDN,BUDKBUDN   SAVE OFF CURRENT BUD NO.                     
*                                                                               
         OC    BUDKBUDN,BUDKBUDN                                                
         BZ    NXCBG50                                                          
         EDIT  BUDKBUDN,(5,WORK)   BUDGET NUMBER                                
         LA    RE,WORK             ELIMINATE LEADING SPACES                     
         LHI   R1,5                                                             
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,1                                                             
         BCT   R1,*-12                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     NXCBG50                                                          
         MVC   CNTNO(0),0(RE)                                                   
*                                                                               
NXCBG50  CLC   LASTLDG,BUDKCLDG    SAME LEDGER AS BEFORE                        
         BE    *+16                                                             
         MVC   LASTLDG,BUDKCLDG    SAVE OFF CURRENT LEDGER                      
         MVC   CNTCLDG,BUDKCLDG    CONTRA LEDGER CODE                           
*                                                                               
         USING LDGTABD,R3                                                       
         LA    R3,CSTLDBLK         R1=A(LEDGER VALUES BLOCK)                    
*                                                                               
         OC    LIMACC,LIMACC       ANY LIMITED ACCESS?                          
         BZ    NXCBG60                                                          
*                                                                               
         OC    LDGTOFFP,LDGTOFFP   ANY OFFICE POSITION IN KEY?                  
         BZ    NXCBG60             NO - SKIP LIMIT ACCESS CHECK                 
         CLI   LDGTOFFP,LDGONKHI                                                
         BNL   NXCBG60             OFFICE NOT IN KEY                            
         MVC   BYTE,LDGTOFFP                                                    
         NI    BYTE,FF-LDGOKEY2    DON'T CARE IF ITS 2 BYTES                    
         LLC   R1,BYTE                                                          
         AHI   R1,-1               MAKE THE POSITION A DISPLACEMENT             
*                                                                               
NXCBG60  GOTOR GETLEVS,LDGTLVA     R1=A(LEDGER VALUES BLOCK)                    
         GOTOR SETCDE,BUDKACT                                                   
         JNE   EXITY                                                            
*                                                                               
         CLC   LSTCN1CA,LEVACDE    COMPARE LEV 1 CODE                           
         BNE   NXCBG70                                                          
         CLC   LSTCN1CB,LEVBCDE    COMPARE LEV 2 CODE                           
         BNE   NXCBG80                                                          
         CLC   LSTCN1CC,LEVCCDE    COMPARE LEV 3 CODE                           
         BNE   NXCBG90                                                          
         CLC   LSTCN1CD,LEVDCDE    COMPARE LEV 4 CODE                           
         BNE   NXCBG100                                                         
         B     NXCBG110                                                         
*                                                                               
NXCBG70  MVC   CNTLV1CD,LEVACDE    MOVE LEV A CODE INTO OUTPUT                  
NXCBG80  MVC   CNTLV2CD,LEVBCDE    MOVE LEV B CODE INTO OUTPUT                  
NXCBG90  MVC   CNTLV3CD,LEVCCDE    MOVE LEV C CODE INTO OUTPUT                  
NXCBG100 MVC   CNTLV4CD,LEVDCDE    MOVE LEV D CODE INTO OUTPUT                  
*                                                                               
NXCBG110 MVC   LSTCN1CA,LEVACDE                                                 
         MVC   LSTCN1CB,LEVBCDE                                                 
         MVC   LSTCN1CC,LEVBCDE                                                 
         MVC   LSTCN1CD,LEVDCDE                                                 
*                                                                               
         MVC   CURUL,BUDKCUNT                                                   
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,CNTLDBLK                            
*                                                                               
         OC    LSTCNLDG,LSTCNLDG   IS THIS FIRST TIME TO PRINT LEDGER           
         BZ    NXCBG120                                                         
         CLC   LSTCNLDG,BUDKCLDG   IS LAST SAME AS CURRENT                      
         BE    NXCBG130                                                         
NXCBG120 MVC   CNTCLDG,BUDKCLDG    MOVE IN CONTRA LEDGER CODE                   
         MVC   LSTCNLDG,BUDKCLDG   MOVE IN CONTRA LEDGER INTO LAST              
*                                                                               
NXCBG130 LA    R3,CNTLDBLK         R3=A(LEDGER VALUES BLOCK)                    
         GOTOR GETLEVS,LDGTLVA     R1=A(LEDGER VALUES BLOCK)                    
         GOTOR SETCDE,BUDKCACT                                                  
         JNE   EXITY                                                            
*                                                                               
         CLC   LSTCN14A,LEVACDE    COMPARE LEV 1 CODE                           
         BNE   NXCBG140                                                         
         CLC   LSTCN14B,LEVBCDE    COMPARE LEV 2 CODE                           
         BNE   NXCBG150                                                         
         CLC   LSTCN14C,LEVCCDE    COMPARE LEV 3 CODE                           
         BNE   NXCBG160                                                         
         CLC   LSTCN14D,LEVDCDE    COMPARE LEV 4 CODE                           
         BNE   NXCBG170                                                         
         B     NXCBG180                                                         
NXCBG140 MVC   CNTCALV1,LEVACDE    MOVE LEV A CODE INTO OUTPUT                  
NXCBG150 MVC   CNTCALV2,LEVBCDE    MOVE LEV B CODE INTO OUTPUT                  
NXCBG160 MVC   CNTCALV3,LEVCCDE    MOVE LEV C CODE INTO OUTPUT                  
NXCBG170 MVC   CNTCALV4,LEVDCDE    MOVE LEV D CODE INTO OUTPUT                  
         DROP  R3                                                               
*                                                                               
NXCBG180 MVC   LSTCN14A,LEVACDE                                                 
         MVC   LSTCN14B,LEVBCDE                                                 
         MVC   LSTCN14C,LEVBCDE                                                 
         MVC   LSTCN14D,LEVDCDE                                                 
*                                                                               
         LA    R3,BUDRFST          POINT TO FIRST ELEM                          
NXCBG190 CLI   0(R3),0                                                          
         BE    NXCBG230                                                         
         CLI   0(R3),BAMELQ        IS IT A BUCKET ELEM                          
         BE    NXCBG210                                                         
NXCBG200 LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     NXCBG190                                                         
*                                                                               
         USING BAMELD,R3                                                        
NXCBG210 CLC   BAMMNTH,STRMOA                                                   
         BL    NXCBG200                                                         
         CLC   BAMMNTH,ENDMOA                                                   
         BH    NXCBG200                                                         
         ZAP   PKAMT,BAMBUDG                                                    
         MVI   PAYTYPE,0                                                        
         GOTOR PUTAMT,DMCB,BAMMNTH,PKAMT                                        
         B     NXCBG200                                                         
*                                                                               
NXCBG220 OC    LASTKEY,LASTKEY     TEST ANY RECORD SAVED                        
         BZ    NXCBG30                                                          
         MVI   LP_RMODE,LP_RNEXT   RESET LP_RLAST SET BY NXTREC                 
         OI    LASTFLAG,LASTLAST   YES - EXIT TO OUTPUT LAST ONE                
         B     NXCBG30                                                          
*                                                                               
NXCBG230 GOTOR SETAMT,CNTENT       SET AMOUNTS INTO OUTPUT                      
         OI    FLAG,FLGREC         SHOW WE HAVE FOUND A RECORD                  
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PERSONNEL DOWNLOAD PLD TOTAL RECORDS                                *         
***********************************************************************         
         SPACE 1                                                                
NXTPLD   J     *+12                                                             
         DC    C'*NXTPLD*'                                                      
         LR    RB,RF                                                            
         USING NXTPLD,RB                                                        
         CLI   LP_RMODE,LP_RFRST   TEST 'FIRST TIME'                            
         BNE   NXTPD60                                                          
*                                                                               
         MVC   PERRANGE,ACTRANGE   INIT PERRANGE AS ACTRANGE                    
*                                                                               
         USING LEDGTBD,R3                                                       
         LA    R3,LEDGTAB          POINT TO LEDGER TABLE                        
NXTPD10  CLI   0(R3),FF            EOT?                                         
         BE    NXTPD20                                                          
         MVC   CURUL,LEDGUL        UNIT/LEDGER                                  
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,CNTLDBLK                            
*                                                                               
         USING LDGTABD,RE                                                       
         LA    RE,CNTLDBLK         R3=A(LEDGER VALUES BLOCK)                    
         MVC   LEDGLEVS(LEDGLVL),LDGTLVA                                        
         CLC   CURUL,LEDGER1C                                                   
         BNE   *+16                                                             
         MVC   CSTLEVS,LDGTLVA     SAVE OFF COSTING LEVELS                      
         MVC   CSTLDBLK,CNTLDBLK   SAVE COSTING BLOCK                           
         CLC   CURUL,LEDGER1R                                                   
         BNE   *+10                                                             
         MVC   PERLDBLK,CNTLDBLK   SAVE OFF PERSON BLOCK                        
         DROP  RE                                                               
*                                                                               
         AHI   R3,LEDGLNQ                                                       
         B     NXTPD10                                                          
         DROP  R3                                                               
*                                                                               
         USING LEDGTBD,R3                                                       
NXTPD20  LA    R3,LEDGTAB          R3=A(LEDGER VALUES BLOCK)                    
NXTPD30  CLI   0(R3),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LEDGLDG,PER14LDG    MATCH ON LEDGER 14                           
         BE    *+12                                                             
         AHI   R3,LEDGLNQ                                                       
         B     NXTPD30                                                          
*                                                                               
         GOTOR GETLEVS,LEDGLEVS                                                 
         MVC   LEDGLEV#,LEVNUM     SAVE OFF LEVEL NUMBER                        
         DROP  R3                                                               
*                                                                               
         LA    RE,PER14ACT         SETUP 1R'S ACCOUNT RANGE                     
         LLC   RF,LEVLNQA                                                       
         AR    RE,RF                                                            
         LLC   R1,LEVLNQB                                                       
         LLC   RF,LEVLNQC                                                       
         AR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SPACES      IF NO ACCOUNT IS PROVIDED                    
         BNH   NXTPD40             LEAVE THE RANGE AS IS                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERRANGE(0),0(RE)                                                
         EX    R1,*+8                                                           
         B     NXTPD40                                                          
         MVC   PERRANGE+L'ACTKACT(0),0(RE)                                      
*                                                                               
NXTPD40  CLI   PER14ACT,C' '                                                    
         BNH   *+16                                                             
         MVC   ANLRANGE(L'PLDKANAL),PER14ACT                                    
         MVC   ANLRANGE+L'PLDKANAL(L'PLDKANAL),PER14ACT                         
*                                                                               
         ICM   RE,7,AULA           POINT TO UNIT/LEDGER/ACCOUNT LIST            
         SR    R1,R1                                                            
         ICM   R1,1,ULAIND         R1=NO. OF U/L/ACCOUNTS IN LIST               
         BZ    *+10                                                             
NXTPD50  MVC   PER1CULA,0(RE)                                                   
         ST    RE,SVADDR2                                                       
         STC   R1,SVACCNUM                                                      
*                                                                               
         MVC   CSTRANGE,ACTRANGE   INIT CSTRANGE AS ACTRANGE                    
         XC    LASTS(LASTL),LASTS                                               
*                                                                               
         LA    RE,PER1CACT+L'PER1CACT-1     SETUP 1C ACCOUNT'S RANGE            
         LHI   R1,L'PER1CACT                                                    
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
*                                                                               
         LTR   R1,R1                                                            
         BZ    NXTPD60                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSTRANGE(0),PER1CACT                                             
         EX    R1,*+8                                                           
         B     NXTPD60                                                          
         MVC   CSTRANGE+L'ACTKACT(0),PER1CACT                                   
*                                                                               
NXTPD60  LA    R1,CLIAMTS                                                       
         LHI   R0,CLIAMLNQ                                                      
         ZAP   0(L'CLIAMTS,R1),=P'0'                                            
         AHI   R1,L'CLIAMTS                                                     
         BCT   R0,*-10                                                          
*                                                                               
         TM    LASTFLAG,LASTLAST   WAS THE LAST RECORD PROCESSED?               
         BZ    NXTPD80                                                          
*                                                                               
NXTPD70  L     RE,SVADDR2          RESTORE ADDRESS OF U/L/A TABLE               
         AHI   RE,L'PER1CULA       BUMP TO NEXT U/L/A IN LIST                   
         ST    RE,SVADDR2          SAVE ADDRESS OF NEXT ACCOUNT                 
         LLC   R1,SVACCNUM         N0. OF ENTRIES LEFT IN LIST                  
         AHI   R1,-1                                                            
         JNP   NOMORE                                                           
         MVI   LP_RMODE,LP_RFRST   RESET LP_RNEXT TO FIRST TIME                 
         B     NXTPD50                                                          
*                                                                               
NXTPD80  TM    LASTFLAG,LASTPROC   TEST 'PROCESS PREVIOUS'                      
         BNO   NXTPD90                                                          
         NI    LASTFLAG,FF-LASTPROC                                             
         B     NXTPD170                                                         
*                                                                               
NXTPD90  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',PLDKEYT),('B#PLDREC',0), +        
               SAVED,0,0                                                        
         BNE   NXTPD180                                                         
*                                                                               
         USING PLDRECD,IOKEY                                                    
*                                                                               
         OC    LIMACC,LIMACC       ANY LIMITED ACCESS?                          
         BZ    NXTPD110            NO-SKIP TESTING ACCESS                       
         USING LDGTABD,R4                                                       
         LA    R4,CSTLDBLK         GET OFFPOS FOR U/L 1C                        
         LA    RF,PLDKCACT         POINT TO 1C ACCOUNT                          
         CLI   SECACC,C'A'         ARE WE DOING ACCOUNT SECURITY                
         BE    *+12                                                             
         LA    R4,PERLDBLK         GET OFFPOS FOR U/L 1R                        
         LA    RF,PLDKRACT         POINT TO 1R ACCOUNT                          
         LLH   R0,OFFNUM                                                        
         LA    RE,OFFTAB           ONLY SEND DOWN $ WITHIN LIMACC               
*                                                                               
         CLI   LDGTOFFP,LDGONKHI                                                
         BNL   NXTPD110            OFFICE NOT IN KEY                            
         MVC   BYTE1,LDGTOFFP                                                   
         NI    BYTE1,FF-LDGOKEY2   DON'T CARE IF ITS 2 BYTES                    
         DROP  R4                                                               
*                                                                               
         LHI   R1,1                                                             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         LHI   R1,2                                                             
         LLC   R4,BYTE1                                                         
         AHI   R4,-1               DISP STARTS AT 1 NOT 0                       
         BM    NXTPD130                                                         
         AR    RF,R4                                                            
         AHI   R1,-1                                                            
NXTPD100 CLI   0(RE),FF            IF NOT IN TABLE-SKIP                         
         BE    NXTPD90                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       MATCH ON OFFICE                              
         BE    NXTPD110                                                         
         LA    RE,1(R1,RE)                                                      
         BCT   R0,NXTPD100                                                      
         B     NXTPD90                                                          
*                                                                               
NXTPD110 SR    R0,R0               CHECK SUB LIST NOW                           
         ICM   R0,1,SUBIND                                                      
         BZ    NXTPD130                                                         
         SR    RE,RE               ONLY SEND DOWN $ WITHIN SUBLST               
         ICM   RE,7,ASUB                                                        
*                                                                               
         USING LDGTABD,R4                                                       
         LA    R4,PERLDBLK         GET OFFPOS FOR U/L 1R                        
         LA    RF,PLDKRACT         POINT TO 1R ACCOUNT                          
*                                                                               
         CLI   LDGTOFFP,LDGONKHI                                                
         BNL   NXTPD130            OFFICE NOT IN KEY                            
         MVC   BYTE1,LDGTOFFP                                                   
         NI    BYTE1,FF-LDGOKEY2   DON'T CARE IF ITS 2 BYTES                    
         DROP  R4                                                               
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,BYTE1                                                       
         BZ    NXTPD130                                                         
         AHI   R1,-1               DISP STARTS AT 1 NOT 0                       
         AR    RF,R1                                                            
         LHI   R1,1                                                             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         LHI   R1,2                                                             
         AHI   R1,-1                                                            
NXTPD120 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       RF-SET FROM ABOVE                            
         BE    NXTPD130                                                         
         LA    RE,1(R1,RE)                                                      
         BCT   R0,NXTPD120                                                      
         B     NXTPD90                                                          
*                                                                               
NXTPD130 GOTOR GETLEVS,CSTLEVS     GET INDIV COSTING LEVS LENGTHS               
         GOTOR SETCDE,PER1CACT     SET LEVELS FOR REQUESTED ACT                 
         JNE   EXITY                                                            
*                                                                               
         MVC   ACCLEVA,LEVACDE     MOVE IN LEVEL A CODE                         
         MVC   ACCLEVB,LEVBCDE     MOVE IN LEVEL B CODE                         
         MVC   ACCLEVC,LEVCCDE     MOVE IN LEVEL C CODE                         
         MVC   ACCLEVD,LEVDCDE     MOVE IN LEVEL D CODE                         
*                                                                               
         GOTOR SETCDE,PLDKCACT     SET LEVELS FOR ACTUAL ACT                    
         JNE   EXITY                                                            
*                                                                               
* CHECK THAT EACH LEVEL PASSED BACK IS AN EXACT MATCH FOR THAT                  
* LEVELS REQUESTED                                                              
*                                                                               
         CLC   ACCLEVA,SPACES      WAS ANY LEVEL A REQUESTED?                   
         BE    *+14                NO - SKIP                                    
         CLC   ACCLEVA,LEVACDE     EXACT MATCH?                                 
         BNE   NXTPD90                                                          
         CLC   ACCLEVB,SPACES      WAS ANY LEVEL B REQUESTED?                   
         BE    *+14                NO - SKIP                                    
         CLC   ACCLEVB,LEVBCDE     EXACT MATCH?                                 
         BNE   NXTPD90                                                          
         CLC   ACCLEVC,SPACES      WAS ANY LEVEL C REQUESTED?                   
         BE    *+14                NO - SKIP                                    
         CLC   ACCLEVC,LEVCCDE     EXACT MATCH?                                 
         BNE   NXTPD90                                                          
         CLC   ACCLEVD,SPACES      WAS ANY LEVEL D REQUESTED?                   
         BE    *+14                NO - SKIP                                    
         CLC   ACCLEVD,LEVDCDE     EXACT MATCH?                                 
         BNE   NXTPD90                                                          
*                                                                               
         OC    LST1C,LST1C         IS THIS THE FIRST TIME IN                    
         BZ    NXTPD140                                                         
         CLC   LST1C,PLDKCACT      ARE WE DOING THE SAME 1C AS PREV             
         BNE   NXTPD160                                                         
NXTPD140 OC    LST1R,LST1R         IS THIS THE FIRST TIME IN                    
         BZ    NXTPD150                                                         
         CLC   LST1R,PLDKRACT      ARE WE DOING THE SAME 1R AS PREV             
         BNE   NXTPD160                                                         
NXTPD150 OC    LSTANAL,LSTANAL                                                  
         BZ    NXTPD170                                                         
         CLC   LSTANAL,PLDKANAL                                                 
         BE    NXTPD170                                                         
NXTPD160 OI    LASTFLAG,LASTPROC   SET 'PROCESS PREVIOUS' FOR NEXT TIME         
         B     NXTPLDX             EXIT TO SEND CURRENT                         
*                                                                               
NXTPD170 MVC   LST1R,PLDKRACT      SAVE OFF CURRENT 1R                          
         MVC   LST1C,PLDKCACT      SAVE OFF CURRENT 1C                          
         MVC   LSTANAL,PLDKANAL    SAVE OFF CURRENT ANALYSIS CODE               
*                                                                               
* SAVE OFF VALUES FOR NXTPER ROUTINE                                            
*                                                                               
         MVC   METHOD,PLDKMTHD     SAVE OFF METHOD                              
         MVC   CSTACT,PLDKCACT     SAVE OFF 1C ACCOUNT                          
         MVC   PERACT,PLDKRACT     SAVE OFF 1R ACCOUNT                          
         MVC   ANAL14,PLDKANAL     SAVE OFF ANALYSIS CODE                       
*                                                                               
         ZAP   PKAMT,PLDKAMT       ZAP IT INTO 8 BYTE FIELD                     
         MVC   PAYTYPE,PLDKPTYP                                                 
         NI    PAYTYPE,X'0F'       MAKE CHAR TO HEX                             
         LLC   R1,PAYTYPE                                                       
         BCTR  R1,0                PAYTYPE IS DISPL INTO AMT ARRAY              
         STC   R1,PAYTYPE                                                       
         GOTOR PUTAMT,DMCB,PLDKYYMM,PKAMT,PAYTYPE                               
         B     NXTPD90                                                          
*                                                                               
NXTPD180 OC    LSTPRS(LSTPR1CL),LSTPRS  ANY RECORD SAVED                        
         BZ    NXTPD70                                                          
         OI    LASTFLAG,LASTLAST                                                
         MVI   LP_RMODE,LP_RNEXT   RESET LP_RNEXT TO NEXT TIME                  
*                                                                               
NXTPLDX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PERSONNEL P&L DOWNLOAD - 1R PERSON RECORDS                          *         
***********************************************************************         
         SPACE 1                                                                
NXTPER   J     *+12                                                             
         DC    C'*NXTPER*'                                                      
         LR    RB,RF                                                            
         USING NXTPER,RB                                                        
         GOTOR CLROUT,DMCB,PERVALS,PERVALL                                      
*                                                                               
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',PERKEYT),                +        
               ('B#PERREC',SVIOKEY),SAVED,0,0                                   
         BNE   NXTPERX                                                          
         LA    R0,PERVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
*                                                                               
         USING CHDRECD,R2                                                       
         L     R2,IOADDR                                                        
*                                                                               
         USING LEDGTBD,R3                                                       
         LA    R3,LEDGTAB          R3=A(LEDGER VALUES BLOCK)                    
NXTPR10  CLI   0(R3),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LEDGUL,CHDKUNT      MATCH ON LEDGER 1R                           
         BE    *+12                                                             
         AHI   R3,LEDGLNQ                                                       
         B     NXTPR10                                                          
*                                                                               
         GOTOR GETLEVS,LEDGLEVS                                                 
         MVC   LEDGLEV#,LEVNUM     SAVE OFF LEVEL NUMBER                        
         GOTOR SETCDE,CHDKACT                                                   
         JNE   EXITY                                                            
*                                                                               
         OC    LSTPR1R(LSTPR1RL),LSTPR1R IS THIS FIRST TIME IN FOR 1R           
         BZ    NXTPR20                                                          
*                                                                               
         CLC   LSTPR1RA,LEVACDE                                                 
         BNE   NXTPR20                                                          
         CLC   LSTPR1RB,LEVBCDE                                                 
         BNE   NXTPR30                                                          
         CLC   LSTPR1RC,LEVCCDE                                                 
         BNE   NXTPR40                                                          
         CLC   LSTPR1RD,LEVDCDE                                                 
         BNE   NXTPR50                                                          
         B     NXTPR60                                                          
NXTPR20  MVC   PER1RL1,LEVACDE     MOVE 1R LEV A CODE INTO OUTPUT               
         MVC   LSTPR1RA,LEVACDE    MOVE 1R LEV A CODE INTO LAST                 
NXTPR30  MVC   PER1RL2,LEVBCDE     MOVE    LEV B CODE INTO OUTPUT               
         MVC   LSTPR1RB,LEVBCDE    MOVE 1R LEV B CODE INTO LAST                 
NXTPR40  MVC   PER1RL3,LEVCCDE     MOVE    LEV C CODE INTO OUTPUT               
         MVC   LSTPR1RC,LEVCCDE    MOVE 1R LEV C CODE INTO LAST                 
NXTPR50  MVC   PER1RL4,LEVDCDE     MOVE    LEV D CODE INTO OUTPUT               
         MVC   LSTPR1RD,LEVDCDE    MOVE 1R LEV D CODE INTO LAST                 
*                                                                               
NXTPR60  LA    R3,LEDGTAB          R3=A(LEDGER VALUES BLOCK)                    
NXTPR70  CLI   0(R3),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LEDGUL,CHDKCUNT     MATCH ON LEDGER 1C                           
         BE    *+12                                                             
         AHI   R3,LEDGLNQ                                                       
         B     NXTPR70                                                          
*                                                                               
         GOTOR GETLEVS,LEDGLEVS                                                 
         MVC   LEDGLEV#,LEVNUM     SAVE OFF LEVEL NUMBER                        
         GOTOR SETCDE,CHDKCACT                                                  
         JNE   EXITY                                                            
         DROP  R3                                                               
*                                                                               
         OC    LSTPR1C(LSTPR1CL),LSTPR1C IS THIS FIRST TIME IN FOR 1C           
         BZ    NXTPR80                                                          
*                                                                               
         CLC   LSTPR1CA,LEVACDE                                                 
         BNE   NXTPR80                                                          
         CLC   LSTPR1CB,LEVBCDE                                                 
         BNE   NXTPR90                                                          
         CLC   LSTPR1CC,LEVCCDE                                                 
         BNE   NXTPR100                                                         
         CLC   LSTPR1CD,LEVDCDE                                                 
         BNE   NXTPR110                                                         
         B     NXTPR120                                                         
NXTPR80  MVC   PER1CL1,LEVACDE     MOVE 1R LEV A CODE INTO OUTPUT               
         MVC   LSTPR1CA,LEVACDE    MOVE 1R LEV A CODE INTO LAST                 
NXTPR90  MVC   PER1CL2,LEVBCDE     MOVE    LEV B CODE INTO OUTPUT               
         MVC   LSTPR1CB,LEVBCDE    MOVE 1R LEV B CODE INTO LAST                 
NXTPR100 MVC   PER1CL3,LEVCCDE     MOVE    LEV C CODE INTO OUTPUT               
         MVC   LSTPR1CC,LEVCCDE    MOVE 1R LEV C CODE INTO LAST                 
NXTPR110 MVC   PER1CL4,LEVDCDE     MOVE    LEV D CODE INTO OUTPUT               
         MVC   LSTPR1CD,LEVDCDE    MOVE 1R LEV D CODE INTO LAST                 
*                                                                               
NXTPR120 OC    LSTPRMTH,LSTPRMTH   IS THIS FIRST TIME IN                        
         BZ    NXTPR130                                                         
         CLC   LSTPRMTH,METHOD                                                  
         BE    NXTPR140                                                         
NXTPR130 MVC   PERMTH,METHOD                                                    
         MVC   LSTPRMTH,METHOD                                                  
NXTPR140 OC    LSTPRAN,LSTPRAN     IS THIS FIRST TIME INT LVL OF 14             
         BZ    NXTPR150                                                         
         CLC   LSTPRAN,ANAL14                                                   
         BE    NXTPR160                                                         
NXTPR150 MVC   PERANAL,ANAL14      ANALYSIS CODE FROM 1ST LVL OF 14             
         MVC   LSTPRAN,ANAL14      SAVE OFF ANALYSIS CODE TO LAST               
*                                                                               
NXTPR160 LA    R3,CHDRFST                                                       
NXTPR170 CLI   0(R3),0                                                          
         BE    NXTPR210                                                         
         CLI   0(R3),BUKELQ        IS IT A BUCKET ELEM                          
         BE    NXTPR190                                                         
NXTPR180 LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     NXTPR170                                                         
*                                                                               
         USING BUKELD,R3                                                        
NXTPR190 CLC   BUKMOS,STRMOA                                                    
         BL    NXTPR180                                                         
         CLC   BUKMOS,ENDMOA                                                    
         BH    NXTPR180                                                         
         ZAP   PKAMT,BUKDR         AMOUNT IS IN DR FOR US                       
         AP    PKAMT,BUKCR         AMOUNT IS IN CR FOR UK                       
         MVI   PAYTYPE,3                                                        
         GOTOR PUTAMT,DMCB,BUKMOS,PKAMT,PAYTYPE                                 
         B     NXTPR180                                                         
NXTPR210 GOTOR SETAMT,PERENT       SET AMOUNTS INTO OUTPUT                      
*                                                                               
NXTPERX  J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SCRIBE FORMAT DOWNLOAD                                              *         
***********************************************************************         
         SPACE 1                                                                
NXTFRM   J     *+12                                                             
         DC    C'*NXTFRM*'                                                      
         LR    RB,RF                                                            
         USING NXTFRM,RB                                                        
         CLI   LP_RMODE,LP_RFRST   TEST 'FIRST TIME'                            
         BNE   NXTFRM10                                                         
         NI    FLAG,X'FF'-FLGDTTM  CLEAR FLAG FOR SEQ RECORD                    
         XC    LSTFRMS(LSTFRML),LSTFRMS                                         
*                                                                               
NXTFRM10 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',SCRKEYT),('B#FRMREC',0), +        
               SAVED,0,0                                                        
         JNE   EXITY                                                            
         LA    R0,FRMVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         L     R2,IOADDR                                                        
         USING RESRECD,R2                                                       
*                                                                               
         CLI   RESKSEQ,X'40'       ALWAYS RESET FOR 40 REC                      
         BNE   *+10                                                             
         XC    FRMVALS(FRMVALL),FRMVALS                                         
*                                                                               
         NI    FLAG,X'FF'-FLGSCR41 CLEAR FLAG FOR SEQ RECORD                    
         MVC   FRMCODE,RESKFORM                                                 
*                                                                               
         LA    R3,RESRFST                                                       
NXTFRM20 CLI   0(R3),0             PROCESS RECORD                               
         JE    NXTFRMX                                                          
         CLI   0(R3),NAMELQ        X'20' - NAME ELEMENT                         
         BE    NXTFRM40                                                         
         CLI   0(R3),FFNELQ        X'25' - FREE FORM SCRIBE ELEMENT             
         BE    NXTFRM50                                                         
         CLI   0(R3),PACELQ        X'A1' - PERSON ACTIVITY ELEMENT              
         BE    NXTFRM60                                                         
         CLI   0(R3),FFTELQ        X'DB' - FREEFORM TEXT ELEMENT                
         BE    NXTFRM70                                                         
         CLI   0(R3),DTSELQ        X'FB' - PERSON ACTIVITY ELEMENT              
         BE    NXTFRM80                                                         
NXTFRM30 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     NXTFRM20                                                         
*                                                                               
         USING NAMELD,R3                                                        
NXTFRM40 LLC   RF,NAMLN            SET FORMAT NAME                              
         SHI   RF,NAMLN1Q+1                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FRMNAME(0),NAMEREC                                               
         CLC   LSTFNME,FRMNAME     SAME AS BEFORE?                              
         BNE   NXTFRM30                                                         
         MVC   FRMNAME,SPACES                                                   
         B     NXTFRM30                                                         
*                                                                               
         USING STYELD,R3                                                        
NXTFRM50 NI    FLAG,X'FF'-FLGREC   CLEAR GOT A REC FLAG                         
         TM    STYSTAT,STYSACNT    IS THIS AN ACCENT APPROVED FORMAT?           
         BNO   NXTFRM10            SKIP RECORD                                  
         OI    FLAG,FLGREC                                                      
         TM    STYSTAT,STYS2ND     IS THERE A 2ND RECORD                        
         BNO   NXTFRM30                                                         
         OI    FLAG,FLGSCR41       SHOW TO READ NEXT REC                        
         B     NXTFRM30                                                         
*                                                                               
         USING PACELD,R3                                                        
NXTFRM60 OC    FRMADTE,FRMADTE     ANY ACTIVITY DATE PASSED?                    
         BZ    NXTFRM30                                                         
         GOTOR VDATCON,DMCB,(2,FRMADTE),(1,WORK)                                
         CLC   WORK(3),PACDATE     COMPARE FOR ONLY CURRENT ACTIVITY            
         BH    NXTFRM10                                                         
         B     NXTFRM30                                                         
*                                                                               
         USING FFTELD,R3                                                        
NXTFRM70 LLC   R1,FFTDLEN                                                       
         CHI   R1,L'FRMEXT                                                      
         BNH   *+8                                                              
         LA    R1,L'FRMEXT                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FRMEXT(0),FFTDATA                                                
         CLC   LSTFEXT,FRMEXT      SAME AS BEFORE?                              
         BNE   NXTFRM30                                                         
         MVC   FRMEXT,SPACES                                                    
         B     NXTFRM30                                                         
*                                                                               
         USING DTSELD,R3                                                        
NXTFRM80 CLI   DTSTYPE,DTSTSCR     IS THIS A SCRIBE FORMATE ELEM?               
         BNE   NXTFRM30                                                         
         OC    FRMADTE,FRMADTE     NO DATE SKIP FURTHER CHECKS                  
         BZ    NXTFRM30                                                         
         CLC   FRMADTE,DTSDATE     REQ DATE < DATE ON FILE = NEQ                
         BH    NXTFRM10                                                         
         CLC   FRMADTE,DTSDATE     REQ DATE=DATE ON FILE CHECK TIME             
         BNE   NXTFRM30                                                         
         CLC   FRMATIM,DTSTIME     REQ TIME < TIME ON FILE = NEQ                
         BH    NXTFRM10                                                         
         B     NXTFRM30                                                         
*                                                                               
NXTFRMX  TM    FLAG,FLGSCR41       DO WE NEED TO READ ANOTHER REC?              
         BO    NXTFRM10                                                         
         TM    FLAG,FLGREC         DID WE GET A GOOD REC?                       
         BNO   NXTFRM10                                                         
         CLC   FRMNAME,SPACES      DO WE HAVE A VALID FORMAT NAME               
         BNH   *+10                                                             
         MVC   LSTFNME,FRMNAME                                                  
         CLC   FRMEXT,SPACES       DO WE HAVE A VALID EXTENTION                 
         BNH   *+10                                                             
         MVC   LSTFEXT,FRMEXT                                                   
         GOTOR VDATCON,DMCB,(5,0),(2,SVDTE)                                     
         TIME  BIN                                                              
         SRDL  R0,32               TIME IS IN 100THS OF A SECOND                
         D     R0,=F'100'                                                       
         STCM  R1,7,SVTIM                                                       
*                                                                               
         XC    FRMDATE(FRMDTLNQ),FRMDATE  INIT DATE/TIME FIELDS                 
         TM    FLAG,FLGDTTM        HAS TIME BEEN SENT ALREADY                   
         JO    EXITY                                                            
         MVC   FRMDATE,SVDTE                                                    
         MVC   FRMTIME,SVTIM       CLEAR IT IF THE SAME                         
         OI    FLAG,FLGDTTM        SHOW THAT DATE/TIME WAS SENT                 
*                                                                               
         J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST G/L SUMMARY RECORDS DOWNLOAD                                *         
***********************************************************************         
         SPACE 1                                                                
NXTGLS   J     *+12                                                             
         DC    C'*NXTGLS*'                                                      
         LR    RB,RF                                                            
         USING NXTGLS,RB                                                        
         CLI   LP_RMODE,LP_RFRST   TEST 'FIRST TIME'                            
         BNE   NXTGL160                                                         
         MVC   LEDGTAB(DFGLLNQ),DFGLTAB                                         
*                                                                               
         GOTOR BUFFER,DMCB,('GLABUFQ',TSAINI),('GLAKEYL',GLARECL)               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING LEDGTBD,R4                                                       
         LA    R4,LEDGTAB          POINT TO LEDGER TABLE                        
NXTGL00A CLI   0(R4),FF            EOT?                                         
         BE    NXTGL00B                                                         
*                                                                               
         MVC   CURUL,LEDGUL                                                     
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,GLSLDBLK                            
*                                                                               
         USING LDGTABD,RE                                                       
         LA    RE,GLSLDBLK                  RE=A(LEDGER VALUES BLOCK)           
         MVC   LEDGLEVS(LEDGLVL),LDGTLVA    MOVE IN LEVELS INTO TABLE           
         GOTOR GETLEVS,LEDGLEVA                                                 
         MVC   LEDGLEV#,LEVNUM              SAVE OFF LEVEL NUMBER               
         DROP  RE                                                               
*                                                                               
         AHI   R4,LEDGLNQ                                                       
         B     NXTGL00A                                                         
         DROP  R4                                                               
*                                                                               
NXTGL00B MVC   GLSARNGE,ACTRANGE   INIT GLSARNGE AS GLSACRNG                    
         MVC   GLTULRNG(L'ACTUL),=C'GB'                                         
         MVC   GLTULRNG+L'ACTUL(L'ACTUL),=C'GP'                                 
*                                                                               
         MVC   GLAREC(GLARECL),SPACES                                           
         MVI   BYTE,X'FF'                                                       
         CLI   GLEQLEV,0           DID THEY PASS AN ACCOUNT EQUIV?              
         BE    NXTGL00                                                          
         LLC   R1,GLEQLEV                                                       
         AHI   R1,-1               AE'S START AT 0                              
         STC   R1,BYTE                                                          
*                                                                               
NXTGL00  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',GLAKEYT),('B#GLAREC',0), +        
               SAVED,0,0                                                        
         BNE   NXTGL05                                                          
*                                                                               
         USING ACTRECD,R2                                                       
         L     R2,IOADDR                                                        
*                                                                               
         MVC   GLAULA,ACTKULA      G/L Account CODE                             
         LA    R3,ACTRFST                                                       
NXTGL02  CLI   0(R3),0                                                          
         BE    NXTGL08                                                          
         CLI   0(R3),NAMELQ        X'20'- Name Element                          
         BE    NXTGL04                                                          
         CLI   0(R3),FFTELQ        X'DB'- FREEFORM TEXT ELEMENT                 
         BE    NXTGL06                                                          
NXTGL03  LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     NXTGL02                                                          
*                                                                               
         USING NAMELD,R3                                                        
NXTGL04  MVC   GLANME,SPACES                                                    
         LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   GLANME(0),NAMEREC                                                
         B     NXTGL03                                                          
         DROP  R3                                                               
*                                                                               
         USING FFTELD,R3                                                        
NXTGL06  CLC   BYTE,FFTSEQ         DID WE GET THE RIGHT LEVEL?                  
         BNE   NXTGL03                                                          
         MVC   GLAACTE,SPACES                                                   
         LLC   R1,FFTDLEN                                                       
         AHI   R1,-1                                                            
         MVC   GLAACTE(0),FFTDATA                                               
         EX    R1,*-6                                                           
         B     NXTGL03                                                          
         DROP  R3                                                               
*                                                                               
         USING LEDGTBD,R4                                                       
NXTGL08  LA    R4,LEDGTAB          POINT TO LEDGER TABLE                        
NXTGL08A CLI   0(R4),FF            EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LEDGUL,ACTKUNT      Find Right Ledger                            
         BE    *+12                                                             
         AHI   R4,LEDGLNQ                                                       
         B     NXTGL08A                                                         
*                                                                               
         LLC   R1,LEVNUM           get # of levels of Ledger                    
         AHI   R1,-2               Levels start at 0 and go back 1 for          
         BNM   *+6                        Disp to lowest level                  
         DC    H'0'                                                             
         LA    RE,LEDGLEVS                                                      
         AR    RE,R1                                                            
         IC    R1,0(RE)                                                         
         LA    RF,ACTKACT                                                       
         AR    RF,R1               Bump to 1st position of lowest level         
         CLI   0(RF),X'40'                                                      
         BNH   NXTGL00             Only put lowest level to buffer              
         DROP  R4                                                               
*                                                                               
         GOTOR BUFFER,DMCB,('GLABUFQ',TSAADD)                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         B     NXTGL00                                                          
*                                                                               
NXTGL05  XC    IOKEY,IOKEY         Reset Read Sequence                          
         MVI   LP_RMODE,LP_RFRST   Reset 'FIRST TIME' Mode                      
         CLC   GLOFF,SPACES        Any Office/List Passed?                      
         BNH   NXTGL140                                                         
         CLC   GLOFF,LIMACC        Did they pass us their Limited Acc?          
         BE    NXTGL140                                                         
*                                                                               
         USING OFFRECD,R2                                                       
         LA    R2,IOKEY            Read for Office/List provided                
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         MVC   OFFKOFF,GLOFF                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOFIL+IO1'                               
*                                                                               
         L     R2,AIO1                                                          
         CLC   0(OFFKEND,R2),IOKEY                                              
         BNE   NXTGL140                                                         
*                                                                               
         TM    OFFRSTA,OFFSLIST        Is this a list?                          
         BO    NXTGL20                                                          
         MVC   OFFNUM,=X'0001'         Not an office list                       
         MVC   OFFTAB(L'GLOFF),GLOFF   Just add the office to OFFTAB            
         B     NXTGL140                                                         
*                                                                               
NXTGL20  L     R0,AIO3             CLEAR IO3                                    
         LHI   R1,IOLENQ                                                        
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIO3             AIO3 will be temp off list                   
         LA    R3,OFFRFST          Point to first element                       
NXTGL30  CLI   0(R3),0                                                          
         BE    NXTGL60                                                          
         CLI   0(R3),OFLELQ        X'D2' - Office List Element                  
         BE    NXTGL50                                                          
NXTGL40  LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     NXTGL30                                                          
*                                                                               
         USING OFLELD,R3                                                        
NXTGL50  LLC   R1,OFLLN                                                         
         SHI   R1,OFLLN1Q+1                                                     
         LA    RE,OFLNTRY                                                       
         MVC   0(0,R4),0(RE)                                                    
         EX    R1,*-6                                                           
         LA    R4,1(R1,R4)         Point to next open position                  
         B     NXTGL40                                                          
         DROP  R3                                                               
*                                                                               
NXTGL60  SR    RF,RF               Office Counter                               
         LA    R1,1                Assume 1 byte office                         
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         LA    R1,2                                                             
         L     R4,AIO3                                                          
         AHI   R1,-1                                                            
*                                                                               
         SR    R0,R0                                                            
NXTGL70  CLI   0(R4),0             End of List?                                 
         BE    NXTGL110                                                         
         ICM   R0,3,OFFNUM         No Limited Access-everything goes in         
         BZ    NXTGL100                                                         
         LA    RE,OFFTAB                                                        
NXTGL80  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(RE)       Match on office                              
         BE    NXTGL100                                                         
         LA    RE,1(R1,RE)                                                      
         BCT   R0,NXTGL80                                                       
         MVC   0(0,R4),SPACES      Office not included in Limited ACC           
         EX    R1,*-6                                                           
NXTGL90  LA    R4,1(R1,R4)                                                      
         B     NXTGL70                                                          
NXTGL100 AHI   RF,1                Found an office-Increment counter            
         B     NXTGL90                                                          
*                                                                               
NXTGL110 STCM  RF,3,OFFNUM                                                      
         SR    R2,R2                                                            
         LA    R3,L'OFFTAB                                                      
         LA    R4,1(R1)                                                         
         DR    R2,R4                                                            
         CR    RF,R3                                                            
         BNH   *+6                                                              
         DC    H'0'                Offtab not big enough                        
*                                                                               
         LA    R2,OFFTAB           CLEAR OFFICE TABLE                           
         LHI   R3,L'OFFTAB                                                      
         SR    RF,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
         LA    RE,OFFTAB                                                        
         L     R4,AIO3                                                          
NXTGL120 CLI   0(R4),0             End of Office List                           
         BE    NXTGL140                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),SPACES      Do we have an office?                        
         BE    NXTGL130                                                         
         MVC   0(0,RE),0(R4)       Move Office into OFFTAB                      
         EX    R1,*-6                                                           
         LA    RE,1(R1,RE)                                                      
NXTGL130 LA    R4,1(R1,R4)                                                      
         B     NXTGL120                                                         
*                                                                               
NXTGL140 XC    GLSLDBLK,GLSLDBLK                                                
         LA    RE,GLSKEYT                                                       
         OC    OFFNUM,OFFNUM       Any office supplied?                         
         BNZ   *+8                                                              
         LA    RE,GLSKYT2                                                       
         ST    RE,SVADDR                                                        
*                                                                               
         MVC   GLSLDRNG,=C'BP'     Default ledger list is B and P               
*                                                                               
         ZAP   TOTRECS,=P'0'       INITIALIZE TOTALS FOR NXTTOT                 
         LA    RE,TOTENT                                                        
         LA    R0,MOATABM2         maximum # of months for G/L                  
         ZAP   0(L'TOTAMTS,RE),=P'0'                                            
         AHI   RE,TOTENTL                                                       
         BCT   R0,*-10                                                          
*                                                                               
         ICM   RE,7,GLSULA         POINT TO UNIT/LEDGER/ACCOUNT LIST            
         SR    R1,R1                                                            
         ICM   R1,1,GLSIND         R1=NO. OF U/L/ACCOUNTS IN LIST               
         BZ    *+10                                                             
NXTGL150 MVC   ACTULA,0(RE)                                                     
         ST    RE,SVADDR2                                                       
         STC   R1,SVACCNUM                                                      
*                                                                               
         MVC   GLSARNGE,ACTRANGE   INIT GLSARNGE AS GLSACRNG                    
*                                                                               
         XC    LASTS(LASTL),LASTS                                               
*                                                                               
         CLC   ACTLDG,SPACES       Any ledger entered?                          
         BNH   NXTGL160                                                         
         MVC   GLSLDRNG(1),ACTLDG                                               
         MVC   GLSLDRNG+1(1),ACTLDG                                             
*                                                                               
         MVC   CURUL,ACTUL         UNIT/LEDGER - G*                             
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,GLSLDBLK                            
*                                                                               
         LA    RE,ACTACT+L'ACTACT-1                                             
         LHI   R1,L'ACTACT                                                      
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
*                                                                               
         LTR   R1,R1                                                            
         BZ    NXTGL160                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GLSARNGE(0),ACTACT                                               
         EX    R1,*+8                                                           
         B     NXTGL160                                                         
         MVC   GLSARNGE+L'ACTKACT(0),ACTACT                                     
*                                                                               
NXTGL160 GOTOR CLROUT,DMCB,GLSVALS,GLSVALL  CLEAR OUTPUT VALUES                 
         NI    FLAG,FF-FLGREC                                                   
*                                                                               
         LA    R1,CLIAMTS                                                       
         LA    R0,MOATABM2         maximum # of months for G/L                  
         ZAP   0(L'CLIAMTS,R1),=P'0'                                            
         AHI   R1,L'CLIAMTS                                                     
         BCT   R0,*-10                                                          
*                                                                               
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',SVADDR),('B#GLSREC',0),  +        
               SAVED,0,0                                                        
         JNE   NXTGL350                                                         
         LA    R0,GLSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
*                                                                               
         USING GLBRECD,R2                                                       
         L     R2,IOADDR                                                        
*                                                                               
         LA    R3,GLBRFST          POINT TO FIRST ELEM                          
NXTGL170 CLI   0(R3),0                                                          
         BE    NXTGL210                                                         
         CLI   0(R3),CACELQ        Is it a Contra-Acct header element           
         BE    NXTGL190                                                         
         CLI   0(R3),BUKELQ        Is it a Bucket Element                       
         BE    NXTGL200                                                         
NXTGL180 LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     NXTGL170                                                         
*                                                                               
         USING CACELD,R3                                                        
NXTGL190 LLC   R1,CACLN            get length                                   
         SHI   R1,CACLN1Q+1                                                     
         BNP   NXTGL180                                                         
         LA    RE,L'GLSSANM-1                                                   
         CR    R1,RE                                                            
         BNH   *+6                                                              
         LR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GLSSANM(0),CACNAME                                               
         B     NXTGL180                                                         
         DROP  R3                                                               
*                                                                               
         USING BUKELD,R3                                                        
NXTGL200 CLC   BUKMOS,STRMOA                                                    
         BL    NXTGL180                                                         
         CLC   BUKMOS,ENDMOA                                                    
         BH    NXTGL180                                                         
*                                                                               
         OI    FLAG,FLGREC                                                      
         ZAP   PKAMT,BUKDR                                                      
         SP    PKAMT,BUKCR         Amount is always DR-CR                       
         GOTOR PUTAMT,DMCB,BUKMOS,PKAMT,0                                       
         B     NXTGL180                                                         
         DROP  R3                                                               
*                                                                               
NXTGL210 TM    FLAG,FLGREC         do we have a record?                         
         BNO   NXTGL160                                                         
*                                                                               
         AP    TOTRECS,=P'1'                                                    
         MVC   GLSUNT,=C'G'               ALWAYS UNIT G                         
         MVC   GLSLDG(L'GLBKGLA),GLBKGLA  LEDGER/ACCT                           
         MVC   GLSOFF,GLBKGOFF                                                  
         CLC   GLBKGSPC,SPACES                                                  
         BH    NXTGL220                                                         
*                                                                               
         MVC   CURUL,LEDGERSJ      UNIT/LEDGER - SJ                             
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,CNTLDBLK                            
*                                                                               
         USING LDGTABD,R4                                                       
         LA    R4,CNTLDBLK                                                      
*                                                                               
         GOTOR GETLEVS,LDGTLVA                                                  
*                                                                               
         MVC   GLSCLI,SPACES                                                    
         MVC   GLSPRO,SPACES                                                    
         LA    RE,GLBKCP                                                        
         LLC   R1,LEVLNQA                                                       
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GLSCLI(0),0(RE)                                                  
         LA    RE,1(R1,RE)                                                      
         LLC   R1,LEVLNQB                                                       
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GLSPRO(0),0(RE)                                                  
         MVI   GLSSYSMD,C'J'       FOR PRODUCTION                               
         MVC   GLSSYSMD+1(L'GLBKMEDC),GLBKMEDC                                  
         MVC   GLSMEDNM,GLBKMEDN                                                
         MVC   GLSSACT(2),LEDGERSJ                                              
         SR    R1,R1                                                            
         ICM   R1,1,LDGTLVB                                                     
         BNP   NXTGL260                                                         
         AHI   R1,-1                                                            
         MVC   GLSSACT+2(0),GLBKCP                                              
         EX    R1,*-6                                                           
         B     NXTGL260                                                         
         DROP  R4                                                               
*                                                                               
NXTGL220 MVC   GLSSACT,GLBKSULA                                                 
         MVC   GLSSOFF,GLBKSOFF                                                 
         MVC   GLSSCUL,GLBKSCUL                                                 
         CLC   GLBKSUNT(2),=C'SR'  FOR SR GLBKSCA HAS SYS/MED                   
         BNE   *+14                                                             
         MVC   GLSSYSMD,GLBKSCA                                                 
         B     NXTGL260                                                         
*                                                                               
         LA    RE,MEDTAB                                                        
NXTGL230 CLI   0(RE),EOF           NOT A MEDIA CLEARANCE                        
         BE    NXTGL250                                                         
         CLC   GLBKSUNT(2),0(RE)                                                
         BE    NXTGL240                                                         
         LA    RE,L'MEDTAB(RE)                                                  
         B     NXTGL230                                                         
*                                                                               
NXTGL240 CLI   GLBKSCU,X'41'       TRUE CLIENT?                                 
         BE    *+12                                                             
         CLI   GLBKSCU,X'40'                                                    
         BNE   NXTGL250                                                         
         MVC   GLSCLI,GLBKSCA                                                   
         B     NXTGL260                                                         
*                                                                               
NXTGL250 MVC   GLSSCAC,SPACES                                                   
         MVC   GLSSCAC(L'GLBKSCA),GLBKSCA                                       
*                                                                               
NXTGL260 GOTOR SETAMT,GLSENT                                                    
         LA    RE,GLSENT           ADD TO TOTAL                                 
         LA    RF,TOTENT                                                        
         LA    R0,MOATABM2         maximum # of months for G/L                  
NXTGL270 OC    0(L'GLSAMTS,RE),0(RE)                                            
         BZ    NXTGL272                                                         
         AP    0(L'TOTAMTS,RF),0(L'GLSAMTS,RE)                                  
         AHI   RE,GLSENTL                                                       
         AHI   RF,TOTENTL                                                       
         BCT   R0,NXTGL270                                                      
*                                                                               
NXTGL272 CLC   GLAULA,GLSUNLA      Same G/L Account?                            
         JE    NXTGL275                                                         
         MVC   GLAREC(GLARECL),SPACES                                           
         MVC   GLAULA,GLSUNLA      Read For G/L Account                         
         GOTOR BUFFER,DMCB,('GLABUFQ',TSARDH)                                   
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NXTGL275 MVC   GLSANME,GLANME      G/L Account Name                             
         MVC   GLSACTE,GLAACTE     G/L Account Equivalency                      
         B     NXTGLSX                                                          
*                                                                               
NXTGL350 L     RE,SVADDR2          RESTORE ADDRESS OF U/L/A TABLE               
         AHI   RE,L'ACTULA         BUMP TO NEXT U/L/A IN LIST                   
         ST    RE,SVADDR2          SAVE ADDRESS OF NEXT ACCOUNT                 
         LLC   R1,SVACCNUM         N0. OF ENTRIES LEFT IN LIST                  
         AHI   R1,-1                                                            
         JNP   NOMORE                                                           
         MVI   LP_RMODE,LP_RFRST   RESET LP_RNEXT TO FIRST TIME                 
         B     NXTGL150                                                         
*                                                                               
NXTGLSX  J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST G/L SUMMARY RECORDS TOTALS DOWNLOAD                         *         
***********************************************************************         
         SPACE 1                                                                
NXTTOT   J     *+12                                                             
         DC    C'*NXTTOT*'                                                      
         LR    RB,RF                                                            
         USING NXTTOT,RB                                                        
         CLI   LP_RMODE,LP_RFRST   TEST 'FIRST TIME'                            
         BNE   NOMORE                                                           
*                                                                               
         LA    R0,TOTVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
*                                                                               
         LA    RE,CLIAMTS                                                       
         LA    RF,TOTENT                                                        
         LA    R0,MOATABM2         maximum # of months for G/L                  
NXTTOT10 ZAP   0(L'CLIAMTS,RE),0(L'TOTAMTS,RF)                                  
         LA    RE,L'CLIAMTS(RE)                                                 
         AHI   RF,TOTENTL                                                       
         BCT   R0,NXTTOT10                                                      
*                                                                               
         GOTOR SETAMT,TOTENT                                                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST G/L TRANSACTION RECORDS DOWNLOAD                            *         
***********************************************************************         
         SPACE 1                                                                
NXTGLT   J     *+12                                                             
         DC    C'*NXTGLT*'                                                      
         LR    RB,RF                                                            
         USING NXTGLT,RB                                                        
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST 'FIRST TIME'                            
         BNE   NXTGT140                                                         
         MVC   LEDGTAB(DFGCLNQ),DFGCTAB                                         
         XC    LASTS(LASTL),LASTS                                               
         MVI   FLAG,0                                                           
*                                                                               
         MVC   GLTOFFRG,OFFRANGE   Initialize Office Range                      
         MVC   GLTARNGE,ACTRANGE   Initialize Account Range                     
         MVC   GLTCRNGE,GLSACRNG   Initialize Contra Account Range              
         MVC   ACTULA,SPACES                                                    
*                                                                               
         ICM   RE,7,GLSULA         POINT TO UNIT/LEDGER/ACCOUNT LIST            
         SR    R1,R1                                                            
         ICM   R1,1,GLSIND         R1=NO. OF U/L/ACCOUNTS IN LIST               
         BZ    *+10                                                             
NXTGT10  MVC   ACTULA,0(RE)                                                     
         ST    RE,SVADDR2                                                       
         STC   R1,SVACCNUM                                                      
*                                                                               
         CLC   ACTUNT,SPACES       Any Subsidary Unit entered?                  
         BNH   NXTGT40                                                          
         CLC   ACTLDG,SPACES       Any Subsidary Ledger entered?                
         BNH   NXTGT40                                                          
         MVC   GLTULRNG,ACTUL                                                   
         MVC   GLTULRNG+L'ACTUL(L'ACTUL),ACTUL                                  
*                                                                               
         MVC   CURUL,ACTUL         UNIT/LEDGER                                  
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,GLSLDBLK                            
         MVC   CURUL,LEDGER1R      UNIT/LEDGER - 1R                             
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,PERLDBLK                            
*                                                                               
         USING LEDGTBD,R4                                                       
         LA    R4,LEDGTAB          POINT TO LEDGER TABLE                        
NXTGT20  CLI   0(R4),FF            EOT?                                         
         BE    NXTGT40                                                          
*                                                                               
         MVC   CURUL,LEDGUL                                                     
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,CNTLDBLK                            
*                                                                               
         USING LDGTABD,RE                                                       
         LA    RE,CNTLDBLK         RE=A(LEDGER VALUES BLOCK)                    
*                                                                               
         CLI   LDGTOFFP,LDGONKHI                                                
         BNL   NXTGT30             OFFICE NOT IN KEY                            
         MVC   BYTE1,LDGTOFFP                                                   
         NI    BYTE1,FF-LDGOKEY2   DON'T CARE IF ITS 2 BYTES                    
         MVC   LEDGOFFP,BYTE1                                                   
         B     *+10                GET NEXT                                     
*                                                                               
NXTGT30  MVC   LEDGLEVS(LEDGLVL),LDGTLVA    MOVE IN LEVELS INTO TABLE           
         DROP  RE                                                               
*                                                                               
         AHI   R4,LEDGLNQ                                                       
         B     NXTGT20                                                          
         DROP  R4                                                               
*                                                                               
NXTGT40  LA    RE,ACTACT+L'ACTACT-1                                             
         LHI   R1,L'ACTACT                                                      
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
*                                                                               
         LTR   R1,R1                                                            
         BZ    NXTGT50                                                          
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GLTARNGE(0),ACTACT                                               
         EX    R1,*+8                                                           
         B     NXTGT50                                                          
         MVC   GLTARNGE+L'ACTKACT(0),ACTACT                                     
*                                                                               
NXTGT50  CLC   GLTOFF,SPACES       Any Office?                                  
         BNH   NXTGT60                                                          
         MVC   GLTOFFRG(L'GLTOFF),GLTOFF                                        
         MVC   GLTOFFRG+L'GLTOFF(L'GLTOFF),GLTOFF                               
*                                                                               
NXTGT60  ICM   RE,7,ASUB           Point to Contra U/L/A LIST                   
         SR    R1,R1                                                            
         ICM   R1,1,SUBIND         R1=NO. of U/L/A in list                      
         BZ    NXTGT70                                                          
         MVC   CNTULA,0(RE)                                                     
*                                                                               
         MVC   LASTULA,0(RE)       Find last account                            
         LA    RE,L'ACTKULA(RE)                                                 
         BCT   R1,*-10                                                          
*                                                                               
         MVC   CURUL,CNTUL         Contra U/L                                   
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,CNTLDBLK                            
*                                                                               
NXTGT70  LA    RE,MEDTAB                                                        
NXTGT80  CLI   0(RE),EOF           NOT A MEDIA CLEARANCE                        
         BE    NXTGT110                                                         
         CLC   ACTUL,0(RE)                                                      
         BE    NXTGT90                                                          
         LA    RE,L'MEDTAB(RE)                                                  
         B     NXTGT80                                                          
*                                                                               
NXTGT90  OI    FLAG,FLGMPAY        Working on a Media Payable                   
         CLC   GLTCLI,SPACES       True Client?                                 
         BNH   NXTGT110                                                         
         CLC   CNTACT,SPACES       Any Contra passed?                           
         BNH   NXTGT100                                                         
         CLC   CNTACT+9(L'GLTCLI),GLTCLI    Does Contra contain cli?            
         BE    NXTGT110                                                         
         MVI   LP_RMODE,LP_RERRR   SET ERROR MODE FOR RUNNER                    
         MVC   LP_ERROR,=AL2(AE$INCLI)  Invalid Client                          
         J     EXITN                                                            
*                                                                               
NXTGT100 MVC   CNTULA,SPACES                                                    
         MVC   LASTULA,SPACES                                                   
         MVC   CNTACT+9(L'GLTCLI),GLTCLI    Put client in Contra                
         MVC   LASTACT+9(L'GLTCLI),GLTCLI                                       
*                                                                               
NXTGT110 CLC   CNTULA,SPACES                                                    
         BNH   NXTGT120                                                         
         LA    RE,CNTACT+L'CNTACT-1                                             
         LHI   R1,L'CNTACT                                                      
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GLTCRNGE(0),CNTACT                                               
*                                                                               
         LA    RE,LASTACT+L'LASTACT-1                                           
         LHI   R1,L'LASTACT                                                     
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GLTCRNGE+L'ACTKACT(0),LASTACT                                    
*                                                                               
         USING TRNRECD,R2                                                       
NXTGT120 LA    R2,IOKEY                                                         
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,COMPANY                                                  
         MVC   TRNKUNT(L'TRNKUNT+L'TRNKLDG),ACTUL                               
         MVC   TRNKACT,GLTARNGE                                                 
NXTGT130 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOFIL+IO1'                               
         B     NXTGT150                                                         
*                                                                               
NXTGT140 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOFIL+IO1'                               
NXTGT150 L     R2,AIO1                                                          
         CLC   0(TRNKACT-TRNKEY,R2),IOKEYSAV                                    
         BNE   NXTGT999                                                         
         CLC   TRNKACT,GLTARNGE+L'TRNKACT                                       
         BH    NXTGT999                                                         
*                                                                               
         NI    FLAG,FF-FLGREC                                                   
         MVC   SVULA,SPACES                                                     
         XC    SVGLDTE,SVGLDTE                                                  
         MVC   SVHALF,SPACES                                                    
*                                                                               
         CLC   TRNKOFF(TRNKEND-(TRNKOFF-TRNKEY)),SPACES   Acc record?           
         BE    NXTGT180                                                         
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+14                                                             
         CLC   TRNKOFF,SPACES                                                   
         BNH   NXTGT140                                                         
         CLC   TRNKOFF,GLTOFFRG                  Make sure office fits          
         BL    *+14                                                             
         CLC   TRNKOFF,GLTOFFRG+L'TRNKOFF                                       
         BNH   NXTGT160                                                         
         LA    R2,IOKEY                                                         
         LLC   R1,TRNKOFF+L'TRNKOFF-1                                           
         AHI   R1,1                                                             
         STC   R1,TRNKOFF+L'TRNKOFF-1                                           
         B     NXTGT130                                                         
NXTGT160 CLC   TRNKCACT,GLTCRNGE                 Make sure contra fits          
         BL    *+14                                                             
         CLC   TRNKCACT,GLTCRNGE+L'TRNKCACT                                     
         BNH   NXTGT170                                                         
         LA    R2,IOKEY                                                         
         LLC   R1,TRNKCACT+L'TRNKCACT-1                                         
         AHI   R1,1                                                             
         STC   R1,TRNKCACT+L'TRNKCACT-1                                         
         B     NXTGT130                                                         
NXTGT170 CLI   CACKSTYP-CACKEY(R2),X'00'         Is this a Contra Acct          
         BE    NXTGT180                                                         
         CLC   TRNRSMOS,STRMOA     Is this a valid transactions?                
         BL    NXTGT140                                                         
         CLC   TRNRSMOS,ENDMOA                                                  
         BH    NXTGT140                                                         
*                                                                               
         USING TRNRECD,R2                                                       
NXTGT180 LA    R3,TRNRFST          Point to first elem                          
*                                                                               
NXTGT190 CLI   0(R3),0                                                          
         BNE   *+16                Not Uploaded to the G/L                      
         TM    FLAG,FLGREC         ARE WE AT THE TRANSACTION LEVEL?             
         BO    NXTGT290                                                         
         B     NXTGT140                                                         
         CLI   0(R3),NAMELQ        Name Element                                 
         BE    NXTGT210                                                         
         CLI   0(R3),CACELQ        Contra Header Name Element                   
         BE    NXTGT220                                                         
         CLI   0(R3),TRNELQ        Transaction Element                          
         BE    NXTGT230                                                         
         CLI   0(R3),GLDELQ        General Ledger Detail Element                
         BE    NXTGT240                                                         
         CLI   0(R3),MDTELQ        Media Transfer Element                       
         BE    NXTGT270                                                         
         CLI   0(R3),MDPELQ        Media Transfer Element (Packed)              
         BE    NXTGT270                                                         
         CLI   0(R3),EXOELQ        Expense Order Analysis Element               
         BE    NXTGT280                                                         
         CLI   0(R3),FFTELQ        Freeform Text Eelemnt                        
         BE    NXTGT282                                                         
NXTGT200 LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     NXTGT190                                                         
*                                                                               
         USING NAMELD,R3                                                        
NXTGT210 MVC   CURANME,SPACES                                                   
         LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CURANME(0),NAMEREC                                               
         B     NXTGT200                                                         
         DROP  R3                                                               
*                                                                               
         USING CACELD,R3                                                        
NXTGT220 MVC   CURCNME,SPACES                                                   
         LLC   R1,CACLN            get length                                   
         SHI   R1,CACLN1Q+1                                                     
         BNP   NXTGT200                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CURCNME(0),CACNAME                                               
         B     NXTGT200                                                         
         DROP  R3                                                               
*                                                                               
         USING TRNELD,R3                                                        
NXTGT230 ZAP   DUB,TRNAMNT                                                      
         TM    TRNSTAT,TRNSDR      CR need to be sent a negs                    
         BO    *+10                                                             
         MP    DUB,=P'-1'                                                       
         OI    FLAG,FLGREC                                                      
         B     NXTGT200                                                         
         DROP  R3                                                               
*                                                                               
         USING GLDELD,R3                                                        
NXTGT240 CLC   GLTGUNT,SPACES      Did they give a Unit?                        
         BNH   NXTGT250                                                         
         LA    R1,0                Just Unit                                    
         CLC   GLTGLDG,SPACES      Did they giv a Ledger as well?               
         BNH   *+8                                                              
         LA    R1,1                Unit/Ledger                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   GLDUNT(0),GLTGUNT   Match on U/L                                 
         BNE   NXTGT140                                                         
NXTGT250 CLC   GLTGACT,SPACES      Did they give an Account?                    
         BNH   NXTGT260                                                         
         LA    R1,0                Just Unit                                    
         CLC   GLTGLDG,SPACES      Did they giv a Ledger as well?               
         BNH   *+8                                                              
         LA    R1,1                Unit/Ledger                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   GLDUNT(0),GLTGUNT   Match on U/L                                 
         BNE   NXTGT140                                                         
NXTGT260 MVC   SVULA,GLDULA        Put U/L/A to Storage for output              
         MVC   SVGLDTE,GLDDATE     Put Date Uploaded to Storage                 
         B     NXTGT200                                                         
         DROP  R3                                                               
*                                                                               
         USING MDTELD,R3                                                        
NXTGT270 CLC   GLTSMD,SPACES       Did they give a System/Media?                
         BNH   *+14                                                             
         CLC   GLTSMD,MDTSYS       Match on System/Media                        
         BNE   NXTGT140                                                         
         MVC   SVHALF,MDTSYS       Put System/Media to Storage                  
         B     NXTGT200                                                         
         DROP  R3                                                               
*                                                                               
         USING EXOELD,R3                                                        
NXTGT280 MVC   SVOFF,EXODOF        Office Code                                  
         MVC   SVDEPT,EXODEP       Department Code                              
         MVC   SVPER,EXOPER        Person Code                                  
         MVC   SVCLI,EXOCLI        Client code                                  
         MVC   SVPRO,EXOPRO        Product code                                 
         B     NXTGT200                                                         
         DROP  R3                                                               
*                                                                               
         USING FFTELD,R3                                                        
NXTGT282 CLI   FFTTYPE,FFTTCLPR    Client and Product                           
         BNE   NXTGT200                                                         
         MVC   SVCLI,FFTCLAC       Client code                                  
         MVC   SVPRO,FFTPRAC       Product code                                 
         B     NXTGT200                                                         
*                                                                               
NXTGT290 CLC   SVULA,SPACES        Did we find a G/L Account?                   
         BNH   NXTGT140                                                         
*                                                                               
         GOTOR CLROUT,DMCB,GLTVALS,GLTVALL  CLEAR OUTPUT VALUES                 
         LA    R0,GLTVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
*                                                                               
         MVC   OGLTGULA,SVULA      Put U/L/A to output                          
         MVC   OGLTGDTE,SVGLDTE    Put Date Uploaded to output                  
         MVC   OGLTSMD,SVHALF      Match on System/Media                        
         MVC   OGLTANME,CURANME    Account Name                                 
         MVC   OGLTCNME,CURCNME    Contra-Account Name                          
         MVC   OGLTSULA,TRNKULA    Transaction U/L/A                            
         MVC   OGLTOFF,TRNKOFF     Transaction Office                           
         MVC   OGLTCULA,TRNKULC    Transaction U/L/A                            
         MVC   OGLTCLI,SVCLI       Client Code                                  
         MVC   OGLTPRO,SVPRO       Product Code                                 
*                                                                               
         CLC   TRNKUNT(2),LEDGERSJ Send down CLI/PROD for SJ                    
         BNE   *+16                                                             
         MVC   OGLTCLI,TRNKACT             Cli in 1st 3 bytes of Acc            
         MVC   OGLTPRO,TRNKACT+L'OGLTCLI   Prod in next 3 bytes                 
         TM    FLAG,FLGMPAY        Are we doing a media Payable?                
         BNO   NXTGT300                                                         
         MVC   OGLTCULA,SPACES                                                  
         MVC   OGLTCLI,TRNKCACT+9  Cli in last 3 bytes of contra                
         MVC   OGLTPRO,TRNKREF     Prod in 1st 3 bytes of ref                   
NXTGT300 MVC   OGLTREF,TRNKREF     Transaction Reference #                      
         MVC   OGLTDATE,TRNKDATE   Transaction Date                             
         ZAP   OGLTAMNT,DUB        Transaction Amount                           
         MVC   OGLTPOFF,SVOFF      Person Office                                
         MVC   OGLTPDPT,SVDEPT     Person Department                            
         MVC   OGLTPPER,SVPER      Person Code                                  
*                                                                               
         CLC   TRNKCUNT(2),LEDGER1R  Send down Off/Dep/Per for 1R               
         BNE   NXTGT310                                                         
*                                                                               
         USING LDGTABD,R4                                                       
         LA    R4,PERLDBLK         GET OFFPOS FOR U/L 1R                        
         GOTOR GETLEVS,LDGTLVA     R1=A(LEDGER VALUES BLOCK)                    
         GOTOR SETCDE,TRNKCACT     SET LEVELS FOR REQUESTED ACT                 
         JNE   EXITY                                                            
*                                                                               
         MVC   OGLTPOFF,LEVACDE    Person Office                                
         MVC   OGLTPDPT,LEVBCDE    Person Department                            
         MVC   OGLTPPER,LEVDCDE    Person Code                                  
         DROP  R4                                                               
*                                                                               
NXTGT310 DS    0H                                                               
         B     NXTGLTX                                                          
*                                                                               
NXTGT999 L     RE,SVADDR2          RESTORE ADDRESS OF U/L/A TABLE               
         AHI   RE,L'ACTULA         BUMP TO NEXT U/L/A IN LIST                   
         ST    RE,SVADDR2          SAVE ADDRESS OF NEXT ACCOUNT                 
         LLC   R1,SVACCNUM         N0. OF ENTRIES LEFT IN LIST                  
         AHI   R1,-1                                                            
         JNP   NOMORE                                                           
         MVI   LP_RMODE,LP_RFRST   RESET LP_RNEXT TO FIRST TIME                 
         B     NXTGT10                                                          
*                                                                               
NXTGLTX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* EXIT CONDITIONS                                                     *         
***********************************************************************         
         SPACE 1                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS AND EXIT                 
         J     EXITY                                                            
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
EXITCC   XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALIZE OFFICE TABLE FOR LIMITED ACCESS                          *         
***********************************************************************         
         SPACE 1                                                                
SETOFF   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*SETOFF*'                                                      
*                                                                               
         XC    ELEM(L'OFFTAB),ELEM ELEM=TEMP STORAGE OF OFF LIST                
         SR    R0,R0                                                            
         ICM   R0,1,SUBIND         ANY ITEMS IN LIST                            
         JZ    EXITY                                                            
         SR    RF,RF                                                            
         ICM   RF,7,ASUB                                                        
         LA    RE,ELEM                                                          
         LR    R1,R0                                                            
SETOFF10 MVC   0(1,RE),1(RF)                                                    
         AHI   RE,1                                                             
         AHI   RF,2                                                             
         BCT   R0,SETOFF10                                                      
         SR    RF,RF                                                            
         ICM   RF,7,ASUB                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),ELEM                                                     
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE OFFICE TABLE FOR LIMITED ACCESS                          *         
***********************************************************************         
         SPACE 1                                                                
INIOFF   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*INIOFF*'                                                      
*                                                                               
         XC    OFFNUM,OFFNUM                                                    
         USING OFFALD,R1                                                        
         LA    R1,OFFBLK                                                        
         LA    R3,OFFAWORK         OFFICE LIST                                  
         DROP  R1                                                               
*                                                                               
         LHI   R0,32               MAX # OF OLD OFFICES (16/PAGE*2)             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   INIOFF10            # OF OFFICES IS NOT INCL IN LIST             
         LH    R1,0(R3)            GET NUMBER OF OFFICES IN LIST                
         STCM  R1,3,OFFNUM                                                      
         SLL   R1,1                                                             
         AHI   R3,2                BUMP TO BEGINNING OF OFFICES                 
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     INIOFFX                                                          
         MVC   OFFTAB(0),0(R3)                                                  
*                                                                               
INIOFF10 LA    R2,OFFTAB                                                        
         SR    RE,RE                                                            
INIOFF20 CLI   0(R3),0             END OF FILE FOR 1 BYTE OFFICES               
         BE    INIOFFX                                                          
         CLI   0(R3),C'0'          END OF FILE FOR 1 BYTE OFFICES               
         BE    INIOFFX                                                          
*                                                                               
INIOFF30 MVC   0(1,R2),0(R3)                                                    
         AHI   R3,1                BUMP TO NEXT OFFICE                          
         ICM   RE,3,OFFNUM                                                      
         AHI   RE,1                                                             
         STCM  RE,3,OFFNUM                                                      
         AHI   R2,1                                                             
         BCT   R0,INIOFF20                                                      
*                                                                               
INIOFFX  LA    RF,OFFTAB                                                        
         LLH   R0,OFFNUM                                                        
         LHI   R3,1                                                             
         TM    CPYINDS,CPYIOFF2                                                 
         BNO   *+8                                                              
         LHI   R3,2                                                             
         GOTOR SORTIT,(R3)                                                      
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SORT A LIST OF VALUES INTO ASCENDING SEQUENCE                       *         
*                                                                     *         
* NTRY:- RF=A(LIST OF VALUES)                                         *         
*        R1=L'ENTRY                                                   *         
*        R0=N'ENTRIES                                                 *         
***********************************************************************         
         SPACE 1                                                                
SORTIT   NTR1  BASE=*,LABEL=*                                                   
         J     *+12                                                             
         DC    C'*SORTIT*'                                                      
*                                                                               
         SHI   R0,1                R0=N'ENTRIES-1                               
         BZ    SORTITX                                                          
         BCTR  R1,0                R1=L'DATA-1                                  
         LR    R2,RF                                                            
*                                                                               
SORTIT02 LA    RF,1(R1,R2)                                                      
         LR    RE,R0                                                            
*                                                                               
SORTIT04 EX    R1,SORTCLC          TEST IN SEQUENCE                             
         BNH   SORTIT06                                                         
         EX    R1,SORTXC2F         SWAP ENTRIES IF OUT                          
         EX    R1,SORTXCF2                                                      
         EX    R1,SORTXC2F                                                      
*                                                                               
SORTIT06 LA    RF,1(RF,R1)                                                      
         BCT   RE,SORTIT04                                                      
         LA    R2,1(R2,R1)                                                      
         BCT   R0,SORTIT02                                                      
*                                                                               
SORTITX  J     EXIT                                                             
*                                                                               
SORTCLC  CLC   0(0,R2),0(RF)                                                    
SORTXC2F XC    0(0,R2),0(RF)                                                    
SORTXCF2 XC    0(0,RF),0(R2)                                                    
         EJECT                                                                  
         DROP  RB                                                               
***********************************************************************         
* PUT AMOUNTS IN CORRECT BUCKETS                                      *         
*     P1=(MOA)                                                        *         
*     P2=(AMOUNT)                                                     *         
* FOR CONTRA BUCKET DOWNLOAD                                          *         
*        PAYTYPE=PAYMENT TYPE (0=SALARY,1=PENSION,2=BENEFITS)         *         
* FOR PERSONNEL DOWNLOAD                                              *         
*        PAYTYPE=PAYMENT TYPE (0=SALARY,1=PENSION,2=BENEFITS,3=HOURS) *         
***********************************************************************         
         SPACE 1                                                                
PUTAMT   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*PUTAMT*'                                                      
*                                                                               
         L     RE,0(R1)                                                         
         MVC   PUTAMOS,0(RE)       MOS DATE                                     
         L     RE,4(R1)                                                         
         MVC   PUTAAMT,0(RE)       AMOUNT                                       
*                                                                               
         LA    R1,CLIAMTS                                                       
         LA    RF,MOATAB                                                        
         LH    R0,MOATABN                                                       
         LHI   RE,L'CLIAMTS        DEFAULT LENGTH                               
         CLC   LP_QMAPN,CNTDLD     ARE WE DOING A CONTRA BUCKET DWNLD           
         BNE   PUTAMT10                                                         
         CLI   BUDIND,0            ONE BUCKET PER MONTH FOR BUDGETS             
         BNE   PUTAMT10                                                         
         MHI   RE,3                - 3 BUCKETS SAL/PEN/BEN                      
PUTAMT10 CLC   LP_QMAPN,PERDLD     ARE WE DOING PERSON DOWNLOAD                 
         BNE   *+8                                                              
         MHI   RE,4                - 4 BUCKETS HRS/SAL/PEN/BEN                  
*                                                                               
PUTAMT20 CLC   PUTAMOS,0(RF)                                                    
         BE    PUTAMT30                                                         
         AR    R1,RE               BUMP TO NEXT AMOUNT                          
         AHI   RF,L'MOATAB                                                      
         BCT   R0,PUTAMT20                                                      
         DC    H'0'                BAD MONTH                                    
*                                                                               
PUTAMT30 LLC   R3,PAYTYPE                                                       
         MHI   R3,L'CLIAMTS                                                     
         LA    R1,0(R3,R1)                                                      
*                                                                               
         AP    0(L'CLIAMTS,R1),PUTAAMT                                          
*                                                                               
PUTAMTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CLEAR OUTPUT AREA                                                   *         
* P1=START OF OUTPUT AREA TO BE CLEARED                               *         
* P2=LENGTH OF OUTPUT AREA TO BE CLEARED                              *         
***********************************************************************         
         SPACE 1                                                                
CLROUT   STM   RE,R1,12(RD)                                                     
         L     R0,0(R1)                                                         
         L     R1,4(R1)                                                         
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET AMOUNT INTO OUTPUT BLOCK                                        *         
*     P1 - AMOUNT BLOCK                                               *         
***********************************************************************         
         SPACE 1                                                                
SETAMT   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*SETAMT*'                                                      
*                                                                               
         LR    R3,R1               R3=A(AMOUNT/REPLICATION TABLE)               
         ZAP   0(L'CNTAMTS,R3),=P'0'                                            
         LHI   R0,1                                                             
         STCM  R0,3,CLINUM                                                      
*                                                                               
         LA    RE,CLIAMTS          FIND LAST VALID AMOUNT IN CLIAMTS            
         LH    R0,MOATABN                                                       
*                                                                               
         CLC   LP_QMAPN,CNTDLD     ARE WE DOWNLOADING CONTRA BUCKETS            
         BNE   SETAMT10                                                         
         CLI   BUDIND,0            ONE BUCKET PER MONTH FOR BUDGETS             
         BNE   SETAMT10                                                         
         MHI   R0,3                - 3 BUCKETS SAL/PEN/BEN                      
SETAMT10 CLC   LP_QMAPN,PERDLD     ARE WE DOWNLOADING PERSONNEL RECD            
         BNE   *+8                                                              
         MHI   R0,4                - 4 BUCKETS HRS/SAL/PEN/BEN                  
*                                                                               
         AHI   R0,-1               MINUS 1 FOR LAST ENTRY                       
         STH   R0,HALF                                                          
         LHI   R1,L'CLIAMTS                                                     
         LR    RF,R1                                                            
         MH    RF,HALF                                                          
         AR    RE,RF               POINT TO LAST ENTRY IN CLIAMTS               
         LNR   R1,R1                                                            
         AHI   R0,1                SET TO ACTUAL NUMBER OF ENTRIES              
SETAMT20 CP    0(L'CLIAMTS,RE),=P'0' ANY AMOUNT?                                
         BNE   *+10                                                             
         AR    RE,R1               BUMP BACK UP 1 ENTRY                         
         BCT   R0,SETAMT20                                                      
*                                                                               
         NI    FLAG,FF-FLGEOF      INIT EOT FLAG                                
         XC    LASTAMT,LASTAMT     CLEAR LAST AMOUNT FIELD FOR COMPARES         
         SR    R1,R1               REP COUNTER                                  
         SR    RE,RE               ENTRY COUNTER                                
         LTR   R0,R0               ANY ACTUAL NUMBERS?                          
         BZ    SETAMT60                                                         
*                                                                               
         LA    R2,CLIAMTS                                                       
         B     SETAMT50                                                         
*                                                                               
SETAMT30 TM    FLAG,FLGEOF         ARE WE AT EOF?                               
         BO    *+14                SET RECORD AND END                           
         CLC   0(L'CLIAMTS,R2),LASTAMT     SAME AS BEFORE?                      
         BE    SETAMT40                                                         
         STC   R1,CNTREP-CNTENT(R3)                                             
         MVC   CNTAMTS-CNTENT(L'CNTAMTS,R3),LASTAMT                             
         AHI   RE,1                                                             
         SR    R1,R1                                                            
         AHI   R3,CNTENTL                                                       
         B     SETAMT50                                                         
SETAMT40 AHI   R1,1                                                             
SETAMT50 TM    FLAG,FLGEOF         ARE WE AT THE END OF THE TABLE               
         BO    SETAMT60                                                         
         MVC   LASTAMT,0(R2)                                                    
         AHI   R2,L'CLIAMTS                                                     
         BCT   R0,SETAMT30                                                      
         OI    FLAG,FLGEOF         SET FLAG TO SHOW EOT                         
         B     SETAMT30                                                         
SETAMT60 LTR   RE,RE                                                            
         BZ    *+8                                                              
         STH   RE,CLINUM                                                        
*                                                                               
SETAMTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE REQUEST INDEX VALUES                                     *         
***********************************************************************         
         SPACE 1                                                                
ININDX   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*ININDX*'                                                      
*                                                                               
         LA    RF,FLDTAB           RF=A(FIELD TABLE)                            
         LHI   R0,FLDTABN          R0=N'TABLE ENTRIES                           
ININDX10 LLH   R1,0(RF)            GET THE DISPLACEMENT TO THE INDEX            
         LA    R1,SAVED(R1)        ADD THE DISPLACEMENT TO THE ADDRESS          
         MVI   0(R1),0                                                          
*                                                                               
         ICM   RE,7,1(R1)          RE=A(WMP ENTRY)                              
         BZ    ININDX20                                                         
*                                                                               
         CLI   LW_TYPE-LW_D(RE),LW_TALLQ                                        
         BNE   *+10                                                             
         SR    RE,RE                                                            
         B     ININDX20                                                         
*                                                                               
         CLI   LW_TYPE-LW_D(RE),LW_TSINQ                                        
         BNE   ININDX15                                                         
         MVI   0(R1),1                                                          
         AHI   RE,LW_LN1Q                                                       
         B     ININDX20                                                         
*                                                                               
ININDX15 CLI   LW_TYPE-LW_D(RE),LW_TLSTQ                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R1),LW_NUMN+1-LW_D(RE)                                       
         AHI   RE,LW_LN2Q                                                       
*                                                                               
ININDX20 STCM  RE,7,1(R1)          SET A(FIRST DATA ITEM)                       
         AHI   RF,L'FLDTAB         BUMP TO NEXT FIELD SET                       
         BCT   R0,ININDX10         DO FOR NUMBER OF FIELD SETS                  
         J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
*     P1 - R1=ACCOUNT TO SECTION UP                                  *          
*             R1=ACTKACT FOR NXTACT                                  *          
*             R1=PLCK1CAC FOR NXTPLA                                 *          
**********************************************************************          
         SPACE 1                                                                
SETCDE   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*SETCDE*'                                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,LEVNUM         NUMBER OF LEVELS/LOOPS                       
         JZ    SETCDEN                                                          
*                                                                               
         MVC   LEVACDE,SPACES      CLEAR CODES ONLY                             
         MVC   LEVBCDE,SPACES                                                   
         MVC   LEVCCDE,SPACES                                                   
         MVC   LEVDCDE,SPACES                                                   
*                                                                               
         LA    R2,LEVS             FIRST LEVEL CODE                             
         LA    R3,LEVLNQS          FIRST LEVEL LENGTHS                          
*                                                                               
SETCDE10 LLC   RE,0(R3)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R1)                                                    
         LA    R1,1(RE,R1)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         AHI   R2,L'LEVS           BUMP TO NEXT LEVEL CODE AREA                 
         AHI   R3,1                BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         J     EXITY                                                            
*                                                                               
SETCDEN  MVI   LP_RMODE,LP_RERRR   SET ERROR MODE FOR RUNNER                    
         MVC   LP_ERROR,=AL2(AE$NOLEV)                                          
         J     EXITN                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT LEDGER LEVELS/NAMES                                         *         
*     CONVERT 14/15/16 -> 1R (IF NECESSARY)                           *         
*     CONVERT 13 -> 1R (PARTIALLY IF NECESSARY)                       *         
*             ONLY ENTERED FROM NXTLDG - NXTPLA - NXTCNT              *         
***********************************************************************         
         SPACE 1                                                                
CNVLDG   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*CNVLDG*'                                                      
*                                                                               
         XC    SVUL,SVUL                                                        
         LTR   R1,R1                                                            
         BZ    *+10                                                             
         MVC   SVUL,0(R1)                                                       
*                                                                               
         USING LDGTABD,R2                                                       
         LA    R2,PERLDBLK         RE=A(LEDGER VALUES BLOCK)                    
*                                                                               
         CLC   LDGTUL,LEDGER1R     DO WE HAVE 1R ALREADY?                       
         BE    CNVL10                                                           
         MVC   CURUL,LEDGER1R      UNIT/LEDGER - 1R                             
         GOTOR (#GETLDG,AGETLDG),DMCB,CURUL,PERLDBLK                            
*                                                                               
CNVL10   CLC   LP_QMAPN,PLADLD     ARE WE DOING AMOUNTS DOWNLOAD?               
         BE    CNVL70                                                           
         CLC   LP_QMAPN,CNTDLD     ARE WE DOING CONTRA BUCKETS DWNLD            
         BE    CNVL70                                                           
*                                                                               
         OC    SVUL,SVUL           SOMETHING HAS TO BE IN THIS FIELD            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SVUL,LEDGER16       16->1R STRAIGHT                              
         BE    CNVL50                                                           
         CLC   SVUL,LEDGER13       13->1R PARTIALLY                             
         BE    CNVL70                                                           
*                                                                               
         GOTOR GETLEVS,LDGTLVA     R1=A(LEDGER VALUES BLOCK-LEVELS)             
         SR    R1,R1                                                            
         SR    RF,RF                                                            
         MVI   LDGL1LN,X'01'       ALWAYS 1 BYTE                                
         IC    R1,LDGL1LN                                                       
         MVC   LDGL2LN,LEVLNQA     2ND LEVEL IS 1ST LEVEL OF 1R                 
         IC    RF,LDGL2LN                                                       
         AR    R1,RF                                                            
         MVC   LDGL3LN,LEVLNQB     3RD LEVEL IS 2ND LEVEL OF 1R                 
*                                                                               
         USING LEDGTBD,R3                                                       
         LA    R3,LEDGTAB                                                       
CNVL20   CLI   0(R3),FF            END OF TABLE?                                
         BE    CNVL30                                                           
         OC    SVUL,SVUL           SOMETHING HAS TO BE IN THIS FIELD            
         BZ    *+14                                                             
         CLC   SVUL,LEDGUL         MATCH ON UNIT/LEDGER                         
         BNE   *+12                                                             
         TM    LEDGSTAT,LEDG3LV    ARE WE DOING A 4 level?                      
         BO    CNVL40                                                           
         AHI   R3,LEDGLNQ                                                       
         B     CNVL20                                                           
*                                                                               
CNVL30   IC    RF,LDGL3LN                                                       
         AR    R1,RF                                                            
         LHI   RF,L'ACTKACT                                                     
         SR    RF,R1                                                            
         STC   RF,LDGL4LN          PUT WHAT IS EVER LEFT IN LEVEL 4             
CNVL40   MVC   LDGL1DSC,=CL15'ANALYSIS CODE'                                    
         LHI   R1,LDGDSCLN-L'LDGL1DSC   LENGTH OF LEV DESCS -1                  
         LA    RF,LDGL2DSC         POINT TO 2ND LEVEL DESCRIPTION               
         TM    LEDGSTAT,LEDG3LV    ARE WE DOING A 4 level?                      
         BNO   CNVL60                                                           
         LHI   R1,LDGDSCLN-(L'LDGL1DSC+L'LDGL4DSC)                              
         B     CNVL60                                                           
         DROP  R3                                                               
*                                                                               
CNVL50   GOTOR GETLEVS,LDGTLVA     R1=A(LEDGER VALUES BLOCK-LEVELS)             
         MVC   LDGLNS(LDGLNSQ),LEVLNQS      USE ALL 1R LEVEL LENS               
         LHI   R1,LDGDSCLN         LENGTH OF LEV DESCS                          
         LA    RF,LDGL1DSC         POINT TO 1ST LEVEL DESCRIPTION               
CNVL60   CLC   LP_QMAPN,ACCDLD     ARE WE DOING ACCOUNTS DOWNLOAD?              
         BE    CNVLX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     CNVLX                                                            
         MVC   0(0,RF),LDGTLVAD    USE ALL 1R LEVEL NAMES                       
*                                                                               
         USING LEDGTBD,R3                                                       
CNVL70   LA    R3,LEDGTAB                                                       
CNVL80   CLI   0(R3),FF            END OF TABLE?                                
         BE    CNVLX                                                            
         OC    SVUL,SVUL                                                        
         BZ    *+14                                                             
         CLC   SVUL,LEDGUL         MATCH ON UNIT/LEDGER                         
         BNE   CNVL90                                                           
         TM    LEDGSTAT,LEDGOR     ARE WE DOING A LEDGER OVERRIDE?              
         BO    CNVL100                                                          
CNVL90   AHI   R3,LEDGLNQ                                                       
         B     CNVL80                                                           
*                                                                               
CNVL100  TM    LEDGSTAT,LEDG1ROR   IS IT A DIRECT 1R OVERRIDE?                  
         BNO   CNVL110                                                          
         MVC   LEDGLEVS(LEDGLVL),LDGTLVA                                        
         B     CNVL90                                                           
*                                                                               
CNVL110  GOTOR GETLEVS,LDGTLVA                                                  
         TM    LEDGSTAT,LEDGPOR    ARE WE DOING A PARTIAL OVERRIDE?             
         BO    CNVL120                                                          
         SR    R1,R1                                                            
         SR    RF,RF                                                            
         MVI   LEDGLEVA,X'01'      ALWAYS 1 BYTE                                
         IC    R1,LEDGLEVA                                                      
         IC    RF,LEVLNQA                                                       
         AR    R1,RF                                                            
         STC   R1,LEDGLEVB                                                      
         IC    R1,LEDGLEVB                                                      
         IC    RF,LEVLNQB                                                       
         AR    R1,RF                                                            
         STC   R1,LEDGLEVC                                                      
         LHI   RF,L'ACTKACT                                                     
         STC   RF,LEDGLEVD         PUT WHAT IS EVER LEFT IN LEVEL 4             
         B     CNVL90                                                           
*                                                                               
CNVL120  CLC   LP_VRSN1,V12012     SPECIAL 13 LOGIC FOR VERSIONS                
         BH    CNVL130             12 OR EARLIER                                
         LLC   R0,LEDGLEV#                                                      
         CHI   R0,2                                                             
         BNH   CNVL90                                                           
         AHI   R0,-2               GO BACK 2 LEVELS                             
         LA    RE,LEDGLEVS                                                      
         SR    R1,R1                                                            
         SR    RF,RF                                                            
         IC    R1,0(RE)                                                         
         AHI   RE,1                                                             
         BCT   R0,*-8                                                           
         IC    RF,LEVLNQA                                                       
         AR    R1,RF                                                            
         STC   R1,0(RE)                                                         
         AHI   RE,1                                                             
         IC    RF,LEVLNQB                                                       
         CLI   SVCDPTLN,0          DO WE ALREADY HAVE A DEPT LENGTH?            
         BE    *+8                                                              
         IC    RF,SVCDPTLN         IF YES-USE THAT INSTEAD                      
         AR    R1,RF                                                            
         STC   R1,0(RE)                                                         
         B     CNVL90                                                           
*                                                                               
CNVL130  SR    RE,RE                                                            
         ICM   RE,1,LEDGOFFP                                                    
         BZ    CNVL150                                                          
         LHI   R0,4                4 LEVEL MAX                                  
         LA    RF,LEDGLEVS         13 LEVELS COMBINED                           
         SR    R1,R1                                                            
         SR    R4,R4                                                            
CNVL140  IC    R4,0(RF)                                                         
         CHI   R4,0                                                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         CR    RE,R4                                                            
         BNH   CNVL180                                                          
         LR    R1,R4               SAVE CURRENT LEVEL                           
         AHI   RF,1                                                             
         BCT   R0,CNVL140                                                       
         DC    H'0'                                                             
*                                                                               
CNVL150  LHI   R0,4                MAX LEVELS IS 4                              
         LA    RE,LEDGLEVS         13 LEVELS COMBINED                           
         SR    R1,R1                                                            
CNVL160  CLI   0(RE),0             CHECK FOR LAST LEVEL                         
         BE    CNVL170                                                          
         AHI   R1,1                                                             
         AHI   RE,1                                                             
         BCT   R0,CNVL160                                                       
*                                                                               
CNVL170  LR    R0,R1               SET ACTUAL LEVEL NUM FOR 13                  
         CHI   R0,2                CHECK LEDGER STRUCTURE                       
         BL    CNVL90              IF LESS THAN TWO LEAVE AS IS                 
         LA    RF,LEDGLEVC                                                      
         CHI   R0,3                THREE OR LESS                                
         BH    *+8                                                              
         LA    RF,LEDGLEVB                                                      
         LHI   R0,2                                                             
         LR    RE,RF               GET PREVIOUS LEVEL LENGTH TO START           
         AHI   RE,-1                                                            
         LLC   R1,0(RE)                                                         
CNVL180  LA    RE,LEVLNQS                                                       
         SR    R4,R4                                                            
CNVL190  IC    R4,0(RE)                                                         
         AR    R1,R4                                                            
         STC   R1,0(RF)                                                         
         AHI   RF,1                                                             
         AHI   RE,1                                                             
         CLI   SVCDPTLN,0          DO WE ALREADY HAVE A DEPT LENGTH?            
         BE    *+10                                                             
         MVC   0(1,RE),SVCDPTLN    IF YES-USE THAT INSTEAD                      
         BCT   R0,CNVL190                                                       
*                                                                               
         GOTOR GETLEVS,LEDGLEVA                                                 
         MVC   LDGLNS(LDGLNSQ),LEVLNQS                                          
         B     CNVL90                                                           
*                                                                               
CNVLX    J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET FILTERS FROM ACCOUNT RECORD                                     *         
*     R3 - LEDGTAB ENTRY                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING LEDGTBD,R3                                                       
GETFLT   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETFLT*'                                                      
*                                                                               
         MVC   SVIOKEY,IOKEY      SAVE OFF PLCKEY                               
         MVC   SVACT,PLCK1CAC      SAVE OFF 1C ACCOUNT                          
*                                                                               
         USING ACTRECD,R2         R2=A(ACCOUNT RECORD)                          
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),LEDGUNT                             
         MVC   ACTKACT,SVACT                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOFIL+IO1'                               
*                                                                               
         L     R2,AIO1                                                          
         CLC   0(ACTKEND,R2),IOKEY                                              
         BNE   GETFXN                                                           
*                                                                               
         LA    R1,ACTRFST                                                       
GETFLT10 CLI   0(R1),0                                                          
         BE    GETFXN                                                           
         CLI   0(R1),RSTELQ        RECORD STATUS ELEMENT                        
         BE    GETFLT20                                                         
         LLC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETFLT10                                                         
*                                                                               
         USING RSTELD,R1                                                        
GETFLT20 MVC   ACTFLT1,RSTFILT1    FILTER 1                                     
         MVC   ACTFLT2,RSTFILT2    FILTER 2                                     
         MVC   ACTFLT3,RSTFILT3    FILTER 3                                     
         MVC   ACTFLT4,RSTFILT4    FILTER 4                                     
         MVC   ACTFLT5,RSTFILT5    FILTER 5                                     
*                                                                               
         MVI   BYTE,0              INIT OUTPUT FIELD                            
         LA    RE,ACTFLT1          POINT TO FILTERS                             
         SR    R1,R1                                                            
         ICM   R1,1,LEDGOFFP       BUMP TO CORRECT FILTER                       
         BZ    GETFXN                                                           
         CHI   R1,L'ACTFLTS        CANT BE > THAN NUMBER OF FILTERS             
         BH    GETFXN                                                           
         AHI   R1,-1               SUBTRACT TO MAKE DISPLACEMENT                
         BM    GETFXN                                                           
         AR    RE,R1                                                            
         MVC   BYTE,0(RE)                                                       
*                                                                               
GETFX    MVC   IOKEY,SVIOKEY       CC=EQ                                        
         J     EXITY                                                            
*                                                                               
GETFXN   MVC   IOKEY,SVIOKEY       CC=NEQ                                       
         J     EXITN                                                            
         DROP  R3,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GET INDIVIDUAL LEVELS                                               *         
*     CONVERT 1,3,5,12 -> 1,2,2,7                                     *         
*     P1 - R1=LEDGER LEVEL A                                          *         
*             R1=LDGL1LN FOR NXTLDG                                   *         
*             R1=LDGTLVA FOR NXTPLA                                   *         
***********************************************************************         
         SPACE 1                                                                
GETLEVS  NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*GETLVS*'                                                      
*                                                                               
         XC    LEVLNQS(LEVLQ),LEVLNQS INIT INDIVIDUAL LENGTHS                   
*                                                                               
         LR    RE,R1               RE=A(LEDGER VALUES BLOCK)                    
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    RE,RE                                                            
GETL10   ICM   RE,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GETL20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    RE,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   RE,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         AHI   R1,1                                                             
         AHI   R2,1                                                             
         BCT   R0,GETL10                                                        
         B     GETLX                                                            
*                                                                               
GETL20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GETLX    J     EXIT                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO TSAR (ONLINE) OR BUFFERIN (OFFLINE)                    *         
*                                                                     *         
* NTRY:- R1 POINTS TO A PARAMETER LIST AS FOLLOWS:-                   *         
*                                                                     *         
*        P1/0   - BUFFER NUMBER                                       *         
*        P1/1-3 - TSAR ACTION CODE                                    *         
*        P2/0   - KEY LENGTH (INITIALIZATION CALL)                    *         
*        P2/2-3 - RECORD LENGTH (INITIALIZATION CALL)                 *         
***********************************************************************         
         SPACE 1                                                                
BUFFER   NTR1  BASE=*,LABEL=NO                                                  
         J     *+12                                                             
         DC    C'*BUFFER*'                                                      
*                                                                               
         LR    R2,R1               R2=A(PARAMETER LIST)                         
                                                                                
         LA    RE,GLABLK                                                        
         LA    RF,GLABUF                                                        
         LA    R1,GLAREC                                                        
         CLI   0(R2),GLABUFQ       TEST CLIENT BUFFER                           
         JE    BUFFER02                                                         
         DC    H'0'                                                             
                                                                                
BUFFER02 ST    R1,BUFFREC          SET A(INPUT RECORD)                          
         MVC   TSARKSAV,0(R1)      SAVE CURRENT RECORD KEY                      
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   BUFFER06                                                         
                                                                                
         LTR   R3,RE                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING TSARD,R3            R3=A(TSAR CONTROL BLOCK)                     
                                                                                
         MVC   TSACTN,3(R2)        SET ACTION CODE                              
         MVC   TSAREC,BUFFREC      SET A(RECORD)                                
                                                                                
         CLI   TSACTN,TSAINI       TEST INITIALIZATION CALL                     
         JNE   BUFFER04                                                         
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSKEYL,4(R2)                                                     
         MVC   TSRECL,6(R2)                                                     
         MVI   TSPAGN,8                                                         
         OI    TSINDS,TSINODSK+TSIXTTWA                                         
         CLI   0(R2),1             TEST BUFFER 1                                
         JE    BUFFER04                                                         
         OI    TSIND2,TSI2BUF2     SET TSAR BUFFER 2 FLAG                       
                                                                                
BUFFER04 GOTOR VTSAR,TSARD                                                      
         MVC   BUFFRET,TSERRS                                                   
         TM    BUFFRET,TSEINIF     TEST INITIALIZATION FAILURE                  
         JZ    BUFFER12                                                         
         DC    H'0'                YES - TAKE A HIT                             
         DROP  R3                                                               
                                                                                
BUFFER06 LR    R3,RF                                                            
         USING BUFFD,R3            R3=A(BUFFERIN CONTROL BLOCK)                 
         LHI   R0,BUFFAINI         CONVERT TSAR ACTION TO BUFFERIN              
         CLI   3(R2),TSAINI                                                     
         JE    BUFFER08                                                         
         LHI   R0,BUFFAPUT                                                      
         CLI   3(R2),TSAADD                                                     
         JE    BUFFER08                                                         
         CLI   3(R2),TSAWRT                                                     
         JE    BUFFER08                                                         
         CLI   3(R2),TSAPUT                                                     
         JE    BUFFER08                                                         
         LHI   R0,BUFFASEQ                                                      
         CLI   3(R2),TSANXT                                                     
         JE    BUFFER08                                                         
         LHI   R0,BUFFARDH                                                      
         CLI   3(R2),TSARDH                                                     
         JE    BUFFER08                                                         
         DC    H'0'                                                             
                                                                                
BUFFER08 CHI   R0,BUFFAINI         TEST INITIALIZATION CALL                     
         JNE   BUFFER10                                                         
         LLC   RE,4(R2)            RE=KEY LENGTH                                
         LH    RF,6(R2)            RF=RECORD LENGTH                             
         SR    RF,RE                                                            
         STCM  RE,3,BUFFLKEY       SET KEY LENGTH                               
         STCM  RF,3,BUFFLCOM       SET COMMENT LENGTH                           
                                                                                
BUFFER10 GOTO1 ABUFFRIN,DMCB,((R0),BUFFD),BUFFREC,ACOMFACS                      
         MVC   BUFFRET,BUFFERRS-BUFFPARM(R1)                                    
         CHI   R0,BUFFARDH         EMULATE TSAR NOT FOUND ON READ HIGH          
         JNE   BUFFER12                                                         
         L     RF,BUFFREC                                                       
         LH    R1,BUFFLKEY                                                      
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         JE    BUFFER12                                                         
         CLC   TSARKSAV(0),0(RF)                                                
         OI    BUFFRET,BUFFERNF                                                 
                                                                                
BUFFER12 CLI   BUFFRET,0           SET CONDITION CODE FOR CALLER                
         J     EXITCC                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST MAPS                                                        *         
***********************************************************************         
         SPACE 1                                                                
REQINI   LKREQ *,I#PALDLD,OUTINI   ** P&L INITIAL DOWNLOAD **                   
*                                                                               
REQACC   LKREQ H,I#ACCDLD,OUTACC   ** P&L ACCOUNTS DOWNLOAD **                  
Un/Ld    LKREQ F,D#UNTLDG,(I,B#SAVED,ULIND),CHAR,OLEN=L'UNTLDG,LIST=F, +        
               SORT=N,TEXT=AC#UNTLD,COL=*,ARRAY=S                               
AcLev    LKREQ F,D#ACCLEV,,CHAR,OLEN=L'RECLEV,TEXT=AC#ACLVL,COL=*,     +        
               ARRAY=E                                                          
AcDte    LKREQ F,D#ACTDTE,(D,B#SAVED,ACCSDTE),CDAT,TEXT=AC#ACTYD,COL=*          
AcTim    LKREQ F,D#ACTTIM,(D,B#SAVED,ACCTIME),UBIN,TEXT=AC#ACTTM,COL=*          
         LKREQ E                                                                
*                                                                               
REQPLA   LKREQ H,I#CLIPLA,OUTPLA   ** P&L AMOUNTS DOWNLOAD **                   
Methd    LKREQ F,D#METHOD,(I,B#SAVED,MTHIND),CHAR,OLEN=L'METHOD,       +        
               LIST=F,SORT=N,TEXT=AC#ALMHD,COL=*                                
Budgt    LKREQ F,D#BUDNO,(I,B#SAVED,BUDIND),UBIN,OLEN=L'BUDKNO1,       +        
               LIST=F,SORT=N,TEXT=AC#BGTNO,COL=*                                
StrMO    LKREQ F,D#STRMOA,(D,B#SAVED,STRMOA),PDAT,TEXT=AC#STRMN,COL=*           
EndMO    LKREQ F,D#ENDMOA,(D,B#SAVED,ENDMOA),PDAT,TEXT=AC#ENDMN,COL=*           
ActCd    LKREQ F,D#ACTCD,(I,B#SAVED,ACTIND),CHAR,OLEN=L'ACTKACT,LIST=F,+        
               SORT=N,TEXT=AC#ACC,COL=*                                         
ExcCd    LKREQ F,D#EXCLAC,(I,B#SAVED,EXCIND),CHAR,OLEN=L'ACTKACT,      +        
               LIST=F,SORT=N,TEXT=AC#EXCL,COL=*                                 
LAACA    LKREQ F,D#SECACC,(D,B#SAVED,SECACC),CHAR,TEXT=AC#SEC,COL=*             
ExcAc    LKREQ F,D#OFFLST,(I,B#SAVED,SUBIND),CHAR,OLEN=L'OFFKOFF,      +        
               LIST=F,TEXT=AC#OFLST,COL=*                                       
         LKREQ E                                                                
*                                                                               
REQCNT   LKREQ H,I#CNTBKT,OUTCNT   ** CONTRA ACCOUNT DOWNLOAD **                
UnLdA    LKREQ F,D#ULACC,(I,B#SAVED,ULAIND),CHAR,OLEN=L'ACTULA,LIST=F, +        
               TEXT=AC#UNLDA,COL=*                                              
CnAcc    LKREQ F,D#CNTACC,(D,B#SAVED,CNTULA),CHAR,TEXT=AC#CTR,COL=*             
Methd    LKREQ F,D#METHOD,(I,B#SAVED,MTHIND),CHAR,OLEN=L'METHOD,       +        
               LIST=F,SORT=N,TEXT=AC#ALMHD,COL=*                                
Bdget    LKREQ F,D#BUDNUM,(I,B#SAVED,BUDIND),UBIN,OLEN=L'BUDKNO1,      +        
               LIST=F,SORT=N,TEXT=AC#BGTNO,COL=*                                
StrMO    LKREQ F,D#STRMOA,(D,B#SAVED,STRMOA),PDAT,TEXT=AC#STRMN,COL=*           
EndMO    LKREQ F,D#ENDMOA,(D,B#SAVED,ENDMOA),PDAT,TEXT=AC#ENDMN,COL=*           
LAACA    LKREQ F,D#SECACC,(D,B#SAVED,SECACC),CHAR,TEXT=AC#SEC,COL=*             
ExcAc    LKREQ F,D#OFFLST,(I,B#SAVED,SUBIND),CHAR,OLEN=L'OFFKOFF,      +        
               LIST=F,TEXT=AC#OFLST,COL=*                                       
         LKREQ E                                                                
*                                                                               
REQPER   LKREQ H,I#PERDLD,OUTPER   ** PERSONNEL AMOUNTS DOWNLOAD **             
UnLdA    LKREQ F,D#ULACC,(I,B#SAVED,ULAIND),CHAR,OLEN=L'ACTULA,LIST=F, +        
               TEXT=AC#UNLDA,COL=*                                              
CnAcc    LKREQ F,D#CNTACC,(D,B#SAVED,PER14ULA),CHAR,TEXT=AC#CTR,COL=*           
Methd    LKREQ F,D#METHOD,(I,B#SAVED,MTHIND),CHAR,OLEN=L'METHOD,       +        
               LIST=F,SORT=N,TEXT=AC#ALMHD,COL=*                                
StrMO    LKREQ F,D#STRMOA,(D,B#SAVED,STRMOA),PDAT,TEXT=AC#STRMN,COL=*           
EndMO    LKREQ F,D#ENDMOA,(D,B#SAVED,ENDMOA),PDAT,TEXT=AC#ENDMN,COL=*           
LAACA    LKREQ F,D#SECACC,(D,B#SAVED,SECACC),CHAR,TEXT=AC#SEC,COL=*             
ExcAc    LKREQ F,D#OFFLST,(I,B#SAVED,SUBIND),CHAR,OLEN=L'OFFKOFF,      +        
               LIST=F,TEXT=AC#OFLST,COL=*                                       
         LKREQ E                                                                
*                                                                               
REQFRM   LKREQ H,I#FRMDLD,OUTFRM   ** SCRIBE FORMAT DOWNLOAD **                 
ACDTE    LKREQ F,D#ACTDTE,(D,B#SAVED,FRMADTE),CDAT,TEXT=AC#ACTYD,COL=*          
ACTIM    LKREQ F,D#ACTTIM,(D,B#SAVED,FRMATIM),UBIN,TEXT=AC#ACTTM,COL=*          
         LKREQ E                                                                
*                                                                               
REQGLS   LKREQ H,I#GLSDLD,OUTGLS   ** G/L Summary download **                   
AccDte   LKREQ F,1,(D,B#SAVED,GLSADTE),CDAT,TEXT=AC#ACTYD,COL=*                 
AccTim   LKREQ F,2,(D,B#SAVED,GLSATIM),UBIN,TEXT=AC#ACTTM,COL=*                 
MoaRg    LKREQ F,3,(D,B#SAVED,GLMOARNG),EDYM,RANGE=Y,                  +        
               OLEN=L'GLSTRMOA,TEXT=AC#MOARA,COL=*                              
UnLda    LKREQ F,4,(I,B#SAVED,GLSIND),CHAR,OLEN=L'ACTULA,LIST=F,       +        
               TEXT=AC#UNLDA,COL=*                                              
AcLev    LKREQ F,5,(D,B#SAVED,GLEQLEV),(R,VALAEQ),OLEN=L'GLEQLEV,      +        
               TEXT=(*,ACEQLIT),COL=*,*                                         
OffCd    LKREQ F,6,(D,B#SAVED,GLOFF),CHAR,MAXLEN=L'GLOFF,              +        
               TEXT=(*,OFFCLIT),COL=*,*                                         
         LKREQ E                                                                
*                                                                               
REQGLT   LKREQ H,I#GLTDLD,OUTGLT   ** G/L Transaction download **               
SuLda    LKREQ F,1,(I,B#SAVED,GLSIND),CHAR,OLEN=L'ACTULA,LIST=F,       +        
               TEXT=(*,UNSALIT),COL=*,*                                         
GuLda    LKREQ F,2,(D,B#SAVED,GLTGULA),CHAR,OLEN=L'ACTULA,LIST=F,      +        
               TEXT=(*,UNGALIT),COL=*,*                                         
MoaRg    LKREQ F,3,(D,B#SAVED,GLMOARNG),EDYM,RANGE=Y,                  +        
               OLEN=L'GLSTRMOA,TEXT=AC#MOARA,COL=*                              
OffCd    LKREQ F,4,(D,B#SAVED,GLTOFF),CHAR,MAXLEN=L'GLTOFF,            +        
               TEXT=(*,OFFCLIT),COL=*,*                                         
MedCd    LKREQ F,5,(D,B#SAVED,GLTMED),CHAR,MAXLEN=L'GLTMED,            +        
               TEXT=AC#MEDC,COL=*                                               
CuLda    LKREQ F,6,(I,B#SAVED,SUBIND),CHAR,OLEN=L'ACTULA,LIST=F,       +        
               TEXT=(*,CULALIT),COL=*,*                                         
CliCd    LKREQ F,7,(D,B#SAVED,GLTCLI),CHAR,MAXLEN=L'GLTCLI,            +        
               TEXT=AC#CLIC,COL=*                                               
SysMd    LKREQ F,8,(D,B#SAVED,GLTSMD),CHAR,MAXLEN=L'GLTSMD,            +        
               TEXT=AC#RSSMC,COL=*                                              
         LKREQ E                                                                
*                                                                               
         LKREQ X                                                                
*                                                                               
ACEQLIT  DC    C'Accnt Equiv Level'                                             
OFFCLIT  DC    C'Office/Office List'                                            
*                                                                               
UNSALIT  DC    C'Unit S U/L/A'                                                  
UNGALIT  DC    C'Unit G U/L/A'                                                  
CULALIT  DC    C'Unit S Contra U/L/A'                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR P&L INITIAL DOWNLOAD                                *         
***********************************************************************         
         SPACE 1                                                                
OUTINI   LKOUT H                                                                
*                                                                               
LDGREC   LKOUT R,O#LDGVAL          ** LEDGER VALUES **                          
Array    LKOUT C,O#LDGVAL,(A,ARYLDG)                                            
         LKOUT E                                                                
*                                                                               
MTHREC   LKOUT R,O#MTHVAL          ** METHOD VALUES **                          
Array    LKOUT C,O#MTHVAL,(A,ARYMTH)                                            
         LKOUT E                                                                
*                                                                               
BUDREC   LKOUT R,O#BUDVAL          ** BUDGET VALUES **                          
Array    LKOUT C,O#BUDVAL,(A,ARYBUD)                                            
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR P&L ACCOUNTS DOWNLOAD                               *         
***********************************************************************         
         SPACE 1                                                                
OUTACC   LKOUT H                                                                
*                                                                               
ACCREC   LKOUT R,O#ACTVAL          ** ACCOUNT VALUES **                         
Array    LKOUT C,O#ACTVAL,(A,ARYALDG)                                           
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR P&L AMOUNTS DOWNLOAD                                *         
***********************************************************************         
         SPACE 1                                                                
OUTPLA   LKOUT H                                                                
*                                                                               
PLAREC   LKOUT R,O#PLAVAL          ** P&L AMOUNTS **                            
Array    LKOUT C,O#PLAVAL,(A,ARYCLI)                                            
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR CONTRA BUCKET DOWNLOAD                              *         
***********************************************************************         
         SPACE 1                                                                
OUTCNT   LKOUT H                                                                
*                                                                               
CNTREC   LKOUT R,O#CNTBKT          ** CONTRA BUCKET AMOUNTS **                  
Array    LKOUT C,O#CNTBKT,(A,ARYCNT)                                            
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR PERSONNEL DOWNLOAD                                  *         
***********************************************************************         
         SPACE 1                                                                
OUTPER   LKOUT H                                                                
*                                                                               
PERREC   LKOUT R,O#PERVAL          ** PERSONNEL DOWNLOAD **                     
Array    LKOUT C,O#PERVAL,(A,ARYPLD)                                            
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR SCRIBE FORMAT DOWNLOAD                              *         
***********************************************************************         
         SPACE 1                                                                
OUTFRM   LKOUT H                                                                
*                                                                               
FRMREC   LKOUT R,O#FRMREC          ** PERSONNEL DOWNLOAD **                     
Array    LKOUT C,O#FRMREC,(A,ARYFRM)                                            
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR G/L SUMMARY RECORDS DOWNLOAD                        *         
***********************************************************************         
         SPACE 1                                                                
OUTGLS   LKOUT H                                                                
*                                                                               
GLSREC   LKOUT R,O#GLSREC          ** G/L SUMMARY RECORD **                     
Array    LKOUT C,O#GLSREC,(A,ARYGLS)                                            
         LKOUT E                                                                
*                                                                               
*OTREC   LKOUT R,O#GLSTOT          ** G/L SUMMARY RECORD TOTALS **              
*rray    LKOUT C,O#GLSTOT,(A,ARYTOT)                                            
*        LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS FOR G/L TRANSACTION RECORDS DOWNLOAD                    *         
***********************************************************************         
         SPACE 1                                                                
OUTGLT   LKOUT H                                                                
*                                                                               
GLTREC   LKOUT R,O#GLTRNS          ** G/L TRANSACTION RECORD **                 
Array    LKOUT C,O#GLTRNS,(A,ARYGLT)                                            
         LKOUT E                                                                
*                                                                               
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR LEDGER RECORDS                                 *         
***********************************************************************         
         SPACE 1                                                                
ARYLDG   LKOUT A,(R,NXTLDG),MULTIROW=Y,ROWNAME=LDGVALS                          
*                                                                               
LdgCd    LKOUT C,D#RECCOD,(D,,LDGCODE),CHAR,ND=Y                                
LdgNm    LKOUT C,D#RECNAM,(D,,LDGNAME),CHAR,ND=Y                                
OfPos    LKOUT C,D#OFFPOS,(D,,LDGOFFP),LBIN,ND=Y                                
ClPos    LKOUT C,D#CLIPOS,(D,,LDGCLIP),LBIN,ND=Y                                
L1Len    LKOUT C,D#LVLN,(D,,LDGL1LN),LBIN,ND=Y                                  
L1Dsc    LKOUT C,D#LVDESC,(D,,LDGL1DSC),CHAR,ND=Y                               
L2Len    LKOUT C,D#LVLN,(D,,LDGL2LN),LBIN,ND=Y                                  
L2Dsc    LKOUT C,D#LVDESC,(D,,LDGL2DSC),CHAR,ND=Y                               
L3Len    LKOUT C,D#LVLN,(D,,LDGL3LN),LBIN,ND=Y                                  
L3Dsc    LKOUT C,D#LVDESC,(D,,LDGL3DSC),CHAR,ND=Y                               
L4Len    LKOUT C,D#LVLN,(D,,LDGL4LN),LBIN,ND=Y                                  
L4Dsc    LKOUT C,D#LVDESC,(D,,LDGL4DSC),CHAR,ND=Y                               
Array    LKOUT C,O#FLTVAL,(A,ARYFLT)                                            
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR FILTER RECORDS                                 *         
***********************************************************************         
         SPACE 1                                                                
ARYFLT   LKOUT A,(R,NXTFLT),MULTIROW=Y,ROWNAME=FLTVALS                          
*                                                                               
FltNo    LKOUT C,D#RECNUM,(D,,FLTNUMB),LBIN,ND=Y                                
FltDs    LKOUT C,D#FLTDSC,(D,,FLTDSC),CHAR,ND=Y                                 
Array    LKOUT C,255,(A,ARYCDE)                                                 
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR FILTER RECORDS (CODES & NAMES)                 *         
***********************************************************************         
         SPACE 1                                                                
ARYCDE   LKOUT A,(D,B#SAVED,FLTENT),NROWS=(B#SAVED,FLTNUM),            +        
               ROWWIDTH=L'FLTENT                                                
*                                                                               
FltCd    LKOUT C,D#RECCOD,(D,,FLTCODE),CHAR,ND=Y                                
FltNm    LKOUT C,D#RECNAM,(D,,FLTNAME),CHAR,ND=Y                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR METHOD RECORDS                                 *         
***********************************************************************         
         SPACE 1                                                                
ARYMTH   LKOUT A,(R,NXTMTH),MULTIROW=Y,ROWNAME=MTHVALS                          
*                                                                               
MthNo    LKOUT C,D#RECNUM,(D,,MTHNUMB),CHAR                                     
MthCd    LKOUT C,D#RECCOD,(D,,MTHCODE),CHAR                                     
MthNm    LKOUT C,D#RECNAM,(D,,MTHNAME),CHAR                                     
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR BUDGET RECORDS                                 *         
***********************************************************************         
         SPACE 1                                                                
ARYBUD   LKOUT A,(R,NXTBUD),MULTIROW=Y,ROWNAME=BUDVALS                          
*                                                                               
BudNo    LKOUT C,D#RECNUM,(D,,BUDNUMB),LBIN,ND=Y                                
BudCd    LKOUT C,D#RECCOD,(D,,BUDCODE),CHAR,ND=Y                                
BudLv    LKOUT C,D#BUDLEV,(D,,BUDLEV),LBIN,ND=Y                                 
BudNm    LKOUT C,D#RECNAM,(D,,BUDNAME),CHAR,ND=Y                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR ACCOUNTS LEDGER RECORDS                        *         
***********************************************************************         
         SPACE 1                                                                
ARYALDG  LKOUT A,(R,NXTLDG),MULTIROW=Y                                          
*                                                                               
Array    LKOUT C,O#ACTVAL,(A,ARYACT)                                            
*                                                                               
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR ACCOUNTS                                       *         
***********************************************************************         
         SPACE 1                                                                
ARYACT   LKOUT A,(R,NXTACT),MULTIROW=Y,ROWNAME=ACTVALS                          
*                                                                               
ActLd    LKOUT C,D#ACTLDG,(D,,ACTALDG),CHAR,ND=Y                                
AcL1C    LKOUT C,D#AL1COD,(D,,ACTLV1CD),CHAR,ND=Y                               
AcL1N    LKOUT C,D#AL1NAM,(D,,ACTLV1NM),CHAR,ND=Y                               
AcL2C    LKOUT C,D#AL2COD,(D,,ACTLV2CD),CHAR,ND=Y                               
AcL2N    LKOUT C,D#AL2NAM,(D,,ACTLV2NM),CHAR,ND=Y                               
AcL3C    LKOUT C,D#AL3COD,(D,,ACTLV3CD),CHAR,ND=Y                               
AcL3N    LKOUT C,D#AL3NAM,(D,,ACTLV3NM),CHAR,ND=Y                               
AcL4C    LKOUT C,D#AL4COD,(D,,ACTLV4CD),CHAR,ND=Y                               
AcL4N    LKOUT C,D#AL4NAM,(D,,ACTLV4NM),CHAR,ND=Y                               
OffVl    LKOUT C,D#OFFVAL,(D,,ACTOFFVL),CHAR,ND=Y                               
ActFl    LKOUT C,D#AFLTS,(D,,ACTFLTS),CHAR,ND=Y                                 
ActSt    LKOUT C,D#ASTAT,(D,,ACTSTAT),LBIN,ND=Y                                 
ActDt    LKOUT C,D#ADATE,(D,,ACTDATE),CDAT,ND=Y                                 
ActTm    LKOUT C,D#ATIME,(D,,ACTTIME),UBIN,ND=Y                                 
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR CLIENT P&L AMOUNTS                             *         
***********************************************************************         
         SPACE 1                                                                
ARYCLI   LKOUT A,(R,NXTPLA),MULTIROW=Y                                          
*                                                                               
Array    LKOUT C,O#PLAVAL,(A,ARYPLA)                                            
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* P&L AMOUNTS VALUES                                                  *         
***********************************************************************         
         SPACE 1                                                                
ARYPLA   LKOUT A,(D,B#SAVED,PLAVALS),NEWEL=Y,NROWS=1,ROWWIDTH=OUTVALL           
*                                                                               
PlTyp    LKOUT C,D#PLATYP,(D,,PLATYP),CHAR,ND=Y                                 
RecNo    LKOUT C,D#RECNUM,(D,,PLANO),CHAR,ND=Y                                  
AcLv1    LKOUT C,D#AL1COD,(D,,PLAACL1),CHAR,ND=Y                                
AcLv2    LKOUT C,D#AL2COD,(D,,PLAACL2),CHAR,ND=Y                                
AcLv3    LKOUT C,D#AL3COD,(D,,PLAACL3),CHAR,ND=Y                                
AcLv4    LKOUT C,D#AL4COD,(D,,PLAACL4),CHAR,ND=Y                                
1CCLC    LKOUT C,D#1CCLDG,(D,,PLACLDG),CHAR,ND=Y                                
CaLv1    LKOUT C,D#CAL1CD,(D,,PLACAL1),CHAR,ND=Y                                
CaLv2    LKOUT C,D#CAL2CD,(D,,PLACAL2),CHAR,ND=Y                                
CaLv3    LKOUT C,D#CAL3CD,(D,,PLACAL3),CHAR,ND=Y                                
CaLv4    LKOUT C,D#CAL4CD,(D,,PLACAL4),CHAR,ND=Y                                
Array    LKOUT C,255,(A,ARYAMT)                                                 
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* AMOUNTS                                                             *         
***********************************************************************         
         SPACE 1                                                                
ARYAMT   LKOUT A,(D,B#SAVED,PLAENT),NROWS=(B#SAVED,CLINUM),            +        
               ROWNAME=PLAENT,ROWWIDTH=PLAENTL                                  
*                                                                               
Value    LKOUT C,D#AMOUNT,(D,,PLAAMTS),SPAK                                     
RepFt    LKOUT C,D#REPFAC,(D,,PLAREP),LBIN,ND=Y                                 
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR CONTRA-ACCOUNTS                                *         
***********************************************************************         
         SPACE 1                                                                
ARYCNT   LKOUT A,(R,NXTCNT),MULTIROW=Y,ROWNAME=CNTVALS                          
*                                                                               
PlTyp    LKOUT C,D#PLATYP,(D,,CNTTYP),CHAR,ND=Y                                 
RecNo    LKOUT C,D#RECNUM,(D,,CNTNO),CHAR,ND=Y                                  
AcLv1    LKOUT C,D#AL1COD,(D,,CNTLV1CD),CHAR,ND=Y                               
AcLv2    LKOUT C,D#AL2COD,(D,,CNTLV2CD),CHAR,ND=Y                               
AcLv3    LKOUT C,D#AL3COD,(D,,CNTLV3CD),CHAR,ND=Y                               
AcLv4    LKOUT C,D#AL4COD,(D,,CNTLV4CD),CHAR,ND=Y                               
1CCLC    LKOUT C,D#1CCLDG,(D,,CNTCLDG),CHAR,ND=Y                                
CaLv1    LKOUT C,D#CAL1CD,(D,,CNTCALV1),CHAR,ND=Y                               
CaLv2    LKOUT C,D#CAL2CD,(D,,CNTCALV2),CHAR,ND=Y                               
CaLv3    LKOUT C,D#CAL3CD,(D,,CNTCALV3),CHAR,ND=Y                               
CaLv4    LKOUT C,D#CAL4CD,(D,,CNTCALV4),CHAR,ND=Y                               
Array    LKOUT C,255,(A,ARYBUK)                                                 
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* CONTRA BUCKET AMOUNTS                                               *         
***********************************************************************         
         SPACE 1                                                                
ARYBUK   LKOUT A,(D,B#SAVED,CNTENT),NROWS=(B#SAVED,CLINUM),            +        
               ROWNAME=CNTENT,ROWWIDTH=CNTENTL                                  
*                                                                               
Value    LKOUT C,D#AMOUNT,(D,,CNTAMTS),SPAK                                     
RepFt    LKOUT C,D#REPFAC,(D,,CNTREP),LBIN,ND=Y                                 
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR PERSONNEL DOWNLOAD                             *         
***********************************************************************         
         SPACE 1                                                                
ARYPLD   LKOUT A,(R,NXTPLD),MULTIROW=Y                                          
*                                                                               
Array    LKOUT C,O#PERVAL,(A,ARYPER)                                            
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR CLIENT P&L                                     *         
***********************************************************************         
         SPACE 1                                                                
ARYPER   LKOUT A,(R,NXTPER),MULTIROW=Y,ROWNAME=PERVALS                          
*                                                                               
MthCd    LKOUT C,D#METHOD,(D,,PERMTH),CHAR,ND=Y                                 
AcLv1    LKOUT C,D#AL1COD,(D,,PER1CL1),CHAR,ND=Y                                
AcLv2    LKOUT C,D#AL2COD,(D,,PER1CL2),CHAR,ND=Y                                
AcLv3    LKOUT C,D#AL3COD,(D,,PER1CL3),CHAR,ND=Y                                
AcLv4    LKOUT C,D#AL4COD,(D,,PER1CL4),CHAR,ND=Y                                
AnlCd    LKOUT C,D#ANLCDE,(D,,PERANAL),CHAR,ND=Y                                
CaLv1    LKOUT C,D#CAL1CD,(D,,PER1RL1),CHAR,ND=Y                                
CaLv2    LKOUT C,D#CAL2CD,(D,,PER1RL2),CHAR,ND=Y                                
CaLv3    LKOUT C,D#CAL3CD,(D,,PER1RL3),CHAR,ND=Y                                
CaLv4    LKOUT C,D#CAL4CD,(D,,PER1RL4),CHAR,ND=Y                                
Array    LKOUT C,255,(A,ARYPAMT)                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* AMOUNTS                                                             *         
***********************************************************************         
         SPACE 1                                                                
ARYPAMT  LKOUT A,(D,B#SAVED,PERENT),NROWS=(B#SAVED,CLINUM),            +        
               ROWNAME=PERENT,ROWWIDTH=PERENTL                                  
*                                                                               
Value    LKOUT C,D#AMOUNT,(D,,PERHOUR),SPAK                                     
RepFt    LKOUT C,D#REPFAC,(D,,PERREP),LBIN,ND=Y                                 
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR SCRIBE FORMAT DOWNLOAD                         *         
***********************************************************************         
         SPACE 1                                                                
ARYFRM   LKOUT A,(R,NXTFRM),MULTIROW=Y,ROWNAME=FRMVALS                          
*                                                                               
FrmCd    LKOUT C,D#RECCOD,(D,,FRMCODE),CHAR,ND=Y                                
FrmNm    LKOUT C,D#RECNAM,(D,,FRMNAME),CHAR,ND=Y                                
FlExt    LKOUT C,D#FILEXT,(D,,FRMEXT),CHAR,ND=Y                                 
FrmDt    LKOUT C,D#ADATE,(D,,FRMDATE),CDAT,ND=Y                                 
FrmTm    LKOUT C,D#ATIME,(D,,FRMTIME),UBIN,ND=Y                                 
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR G/L SUMMARY RECORDS                            *         
***********************************************************************         
         SPACE 1                                                                
ARYGLS   LKOUT A,(R,NXTGLS),MULTIROW=Y,ROWNAME=GLSVALS                          
*                                                                               
GlsAc    LKOUT C,1,(D,,GLSUNLA),CHAR,ND=Y                                       
GlsAe    LKOUT C,2,(D,,GLSACTE),CHAR,ND=Y                                       
GlsOf    LKOUT C,3,(D,,GLSOFF),CHAR,ND=Y                                        
GlsSa    LKOUT C,4,(D,,GLSSACT),CHAR,ND=Y                                       
GlsSn    LKOUT C,5,(D,,GLSSANM),CHAR,ND=Y                                       
GlsSo    LKOUT C,6,(D,,GLSSOFF),CHAR,ND=Y                                       
GlCul    LKOUT C,7,(D,,GLSSCUL),CHAR,ND=Y                                       
GlsCa    LKOUT C,8,(D,,GLSSCAC),CHAR,ND=Y                                       
GlsMd    LKOUT C,9,(D,,GLSMED),CHAR,ND=Y                                        
GlsSm    LKOUT C,10,(D,,GLSSYSMD),CHAR,ND=Y                                     
GlsCl    LKOUT C,11,(D,,GLSCLI),CHAR,ND=Y                                       
GlsPr    LKOUT C,14,(D,,GLSPRO),CHAR,ND=Y                                       
GlsAnm   LKOUT C,15,(D,,GLSANME),CHAR,ND=Y                                      
Array    LKOUT C,255,(A,ARYGLAMT)                                               
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* G/L Amounts                                                         *         
***********************************************************************         
         SPACE 1                                                                
ARYGLAMT LKOUT A,(D,B#SAVED,GLSENT),NROWS=(B#SAVED,CLINUM),            +        
               ROWNAME=GLSENT,ROWWIDTH=GLSENTL                                  
*                                                                               
Value    LKOUT C,D#AMOUNT,(D,,GLSAMTS),SPAK                                     
RepFt    LKOUT C,D#REPFAC,(D,,GLSREP),LBIN,ND=Y                                 
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR G/L TRANSACTION RECORDS                        *         
***********************************************************************         
         SPACE 1                                                                
ARYGLT   LKOUT A,(R,NXTGLT),MULTIROW=Y,ROWNAME=GLTVALS                          
*                                                                               
GltSula  LKOUT C,1,(D,,OGLTSULA),CHAR,ND=Y                                      
GltCula  LKOUT C,2,(D,,OGLTCULA),CHAR,ND=Y                                      
GltGula  LKOUT C,3,(D,,OGLTGULA),CHAR,ND=Y                                      
GltGdte  LKOUT C,4,(D,,OGLTGDTE),PDAT,ND=Y                                      
GltOff   LKOUT C,5,(D,,OGLTOFF),CHAR,ND=Y                                       
GltAmnt  LKOUT C,6,(D,,OGLTAMNT),SPAK,ND=Y                                      
GltRef   LKOUT C,7,(D,,OGLTREF),CHAR,ND=Y                                       
GltTdte  LKOUT C,8,(D,,OGLTDATE),PDAT,ND=Y                                      
GltCli   LKOUT C,9,(D,,OGLTCLI),CHAR,ND=Y                                       
GltSm    LKOUT C,10,(D,,OGLTSMD),CHAR,ND=Y                                      
GltAnm   LKOUT C,11,(D,,OGLTANME),CHAR,ND=Y                                     
GltCnm   LKOUT C,12,(D,,OGLTCNME),CHAR,ND=Y                                     
GltPro   LKOUT C,13,(D,,OGLTPRO),CHAR,ND=Y                                      
GltPoff  LKOUT C,14,(D,,OGLTPOFF),CHAR,ND=Y                                     
GltPdpt  LKOUT C,15,(D,,OGLTPDPT),CHAR,ND=Y                                     
GltPper  LKOUT C,16,(D,,OGLTPPER),CHAR,ND=Y                                     
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR G/L SUMMARY RECORDS TOTALS                     *         
***********************************************************************         
         SPACE 1                                                                
ARYTOT   LKOUT A,(R,NXTTOT),MULTIROW=Y,ROWNAME=TOTVALS                          
*                                                                               
RecNo    LKOUT C,1,(D,,TOTRECS),SPAK,ND=Y                                       
Array    LKOUT C,255,(A,ARYTOAMT)                                               
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* Total G/L Amounts                                                   *         
***********************************************************************         
         SPACE 1                                                                
ARYTOAMT LKOUT A,(D,B#SAVED,TOTENT),NROWS=(B#SAVED,CLINUM),            +        
               ROWNAME=TOTENT,ROWWIDTH=TOTENTL                                  
*                                                                               
Value    LKOUT C,2,(D,,TOTAMTS),SPAK                                            
RepFt    LKOUT C,3,(D,,TOTREP),LBIN,ND=Y                                        
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* GLOBAL LITERALS                                                     *         
***********************************************************************         
         SPACE 1                                                                
GLOBALS  DS    0D                                                               
                                                                                
         LTORG                                                                  
*                                                                               
V10019   DC    AL1(1,0,0,19)       ACCENT VERSION 1.0.0.19                      
V12012   DC    AL1(1,2,0,12)       ACCENT VERSION 1.2.0.12                      
*                                                                               
ACCDLD   DC    AL2(I#ACCDLD)       Account Download                             
PALDLD   DC    AL2(I#PALDLD)       Initial Profit/Loss Download                 
PLADLD   DC    AL2(I#CLIPLA)       Client/Profit-Loss Amounts                   
CNTDLD   DC    AL2(I#CNTBKT)       Contra Bucket Download                       
PERDLD   DC    AL2(I#PERDLD)       Personnel Download                           
RQTDLD   DC    AL2(I#REQTMP)       Request Template Download                    
GLSDLD   DC    AL2(I#GLSDLD)       G/L Summary Record Download                  
GLTDLD   DC    AL2(I#GLTDLD)       G/L Transaction Record Download              
*                                                                               
FLDTAB   DS    0XL2                ** DISPS. TO FIELD INDICES **                
         DC    AL2(ULIND-SAVED)                                                 
         DC    AL2(ACTIND-SAVED)                                                
         DC    AL2(EXCIND-SAVED)                                                
         DC    AL2(MTHIND-SAVED)                                                
*        DC    AL2(BUDIND-SAVED)                                                
         DC    AL2(ULAIND-SAVED)                                                
         DC    AL2(SUBIND-SAVED)                                                
         DC    AL2(GLSIND-SAVED)                                                
FLDTABN  EQU   (*-FLDTAB)/L'FLDTAB                                              
*                                                                               
MEDTAB   DS    0CL2                media payable table                          
         DC    C'SP'                                                            
         DC    C'SQ'                                                            
         DC    C'SS'                                                            
         DC    C'ST'                                                            
         DC    C'SU'                                                            
         DC    AL1(EOF)                                                         
*                                                                               
DEFLTAB  DS    0X                  ** DEFAULT LEDGER TABLE **                   
         DC    C'SJ',XL1'00',XL1'00',XL4'00',AL1(LEDGOR)                        
         DC    C'11',XL1'00',XL1'00',XL4'00',AL1(0)                             
         DC    C'12',XL1'00',XL1'00',XL4'00',AL1(0)                             
         DC    C'13',XL1'00',XL1'00',XL4'00',AL1(LEDGOR+LEDGPOR)                
         DC    C'14',XL1'02',XL1'00',XL4'00',AL1(LEDGOR+LEDG3LV)                
         DC    C'15',XL1'02',XL1'00',XL4'00',AL1(LEDGOR+LEDG3LV)                
         DC    C'16',XL1'01',XL1'00',XL4'00',AL1(LEDGOR+LEDG1ROR)               
         DC    C'1R',XL1'00',XL1'00',XL4'00',AL1(0)                             
         DC    C'1C',XL1'00',XL1'00',XL4'00',AL1(0)                             
         DC    X'FF'                                                            
DEFLLNQ  EQU   *-DEFLTAB                                                        
*                                                                               
DFGCTAB  DS    0X                  ** DEFAULT G/L CONTRA TABLE **               
         DC    C'SA',XL1'00',XL1'00',XL4'00',AL1(0)                             
         DC    C'2D',XL1'00',XL1'00',XL4'00',AL1(0)                             
         DC    X'FF'                                                            
DFGCLNQ  EQU   *-DFGCTAB                                                        
*                                                                               
DFGLTAB  DS    0X                  ** DEFAULT G/L TABLE **                      
         DC    C'GB',XL1'00',XL1'00',XL4'00',AL1(0)                             
         DC    C'GP',XL1'00',XL1'00',XL4'00',AL1(0)                             
         DC    X'FF'                                                            
DFGLLNQ  EQU   *-DFGLTAB                                                        
*                                                                               
DMKEY    DC    C'DMKEY   '                                                      
*                                                                               
FILES    DS    0C                  ** SYSTEM/FILE LIST **                       
         DC    C'ACCOUNT'                                                       
*                                                                               
         DC    C'N'                                                             
ACCDIR   DC    C'ACCDIR '                                                       
         DC    C'N'                                                             
ACCMST   DC    C'ACCMST '                                                       
         DC    C'N'                                                             
ACCARC   DC    C'ACCARC '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
         DC    C'X'                                                             
*                                                                               
MQHVLIT  DC    C'MQ Header'                                                     
MQKVLIT  DC    C'MQ Key'                                                        
*                                                                               
         EJECT                                                                  
LVALUES  DS    0F                  ** LITERALS MOVED TO W/S **                  
*                                                                               
LDGLST#  DC    AL2(LDGLSTN)                                                     
LDGLSTS  DS    0CL2                                                             
LEDGERSJ DC    C'SJ'                                                            
LEDGER1C DC    C'1C'                                                            
LEDGER1R DC    C'1R'                                                            
LEDGER11 DC    C'11'                                                            
LEDGER12 DC    C'12'                                                            
LEDGER13 DC    C'13'                                                            
LEDGER14 DC    C'14'                                                            
LEDGER15 DC    C'15'                                                            
LEDGER16 DC    C'16'                                                            
LEDGER2D DC    C'2D'                                                            
LDGLSTN  EQU   (*-LDGLSTS)/L'LDGLSTS                                            
LDGLSTL  EQU   *-LDGLST#                                                        
*                                                                               
TYPLST#  DC    AL2(TYPLSTN)                                                     
TYPLSTS  DC    C'C',C'D',C'O',C'1',C'2',C'3'                                    
TYPLSTN  EQU   (*-TYPLSTS)/L'TYPLSTS                                            
TYPLSTL  EQU   *-TYPLST#                                                        
*                                                                               
RAP1RLS# DC    AL2(RAP1RLSN)                                                    
RAP1RLSS DC    AL1(RAPKR1RA),AL1(RAPKR1RB),AL1(RAPKR1RC),AL1(RAPKR1RD)          
RAP1RLSN EQU   (*-RAP1RLSS)/L'RAP1RLSS                                          
RAP1RLSL EQU   *-RAP1RLS#                                                       
RAPSJLS# DC    AL2(RAPSJLSN)                                                    
RAPSJLSS DC    AL1(RAPKRCLI),AL1(RAPKRPRO),AL1(RAPKRJOB)                        
RAPSJLSN EQU   (*-RAPSJLSS)/L'RAPSJLSS                                          
RAPSJLSL EQU   *-RAPSJLS#                                                       
         DC    X'41',(L'ACTKACT-1)X'40',(L'ACTKACT)X'FF'                        
         DC    X'40',(L'ACTKACT-1)X'40',(L'ACTKACT)X'FF'                        
         DC    X'41',(L'CMTKMTHD-1)X'40',(L'CMTKMTHD)X'FF'                      
         DC    X'00',(L'BUDKNO1-1)X'01',(L'BUDKNO1)X'FF'                        
         DC    X'41',(L'BUDKCOD-1)X'40',(L'BUDKCOD)X'FF'                        
         DC    X'40',(L'OFFKOFF-1)X'40',(L'OFFKOFF)X'FF'                        
         DC    X'40',(L'CURUL-1)X'40',(L'CURUL)X'FF'                            
         DC    X'41',(L'PLDKANAL-1)X'40',(L'PLDKANAL)X'FF'                      
         DC    (L'PLCKCLDG)X'00',(L'PLCKCLDG)X'FF'                              
         DC    X'41',(L'RESKFORM-1)X'40',(L'RESKFORM)X'FF'                      
         DC    X'40',X'41'                                                      
         DC    X'00',(L'RAPKASPR-1)X'00',(L'RAPKASPR)X'40'                      
         DC    X'40',(L'GLBKSCA-1)X'40',(L'GLBKSCA)X'FF'                        
         DC    X'41',(L'TRNKREF-1)X'40',(L'TRNKREF)X'FF'                        
         DC    X'414040',X'FFFFFF'                                              
         DC    X'00',X'FF'                                                      
*                                                                               
*&&US                                                                           
FAILS    DC    C'JSHA,RGUP:'                                                    
SLOWS    DC    C'JSHA,RGUP:'                                                    
*&&                                                                             
*&&UK                                                                           
FAILS    DC    C'JFOSDDLO,NSHEDDLO,PMARDDLO:'                                   
SLOWS    DC    C'JFOSDDLO,NSHEDDLO,PMARDDLO:'                                   
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* KEY DRIVER TABLES                                                   *         
***********************************************************************         
         SPACE 1                                                                
ACCKEYT  LKKEY H,ACTKEY,SAVED      ** ACCOUNT KEY DRIVER TABLE **               
         LKKEY SIN,ACTKCPY,COMPANY                                              
         LKKEY SIN,ACTKUNT,UNTLDG,L'ACTKUNT+L'ACTKLDG                           
         LKKEY RNG,ACTKACT,ACTRANGE                                             
         LKKEY LIT,ACTKACT+L'ACTKACT,C' ',                             +        
               L'ACTKEY-(ACTKACT+L'ACTKACT-ACTKEY)                              
         LKKEY E                                                                
*                                                                               
BUDKEYT  LKKEY H,BUDKEY            ** BUDGET DRIVER TABLE **                    
         LKKEY LIT,BUDKTYP,BUDKTYPQ                                             
         LKKEY SIN,BUDKCPY,COMPANY                                              
         LKKEY LIT,BUDKIND,0                                                    
         LKKEY RNG,BUDKNO1,BUDRANGE                                             
         LKKEY RNG,BUDKCOD,BCDRANGE                                             
         LKKEY LIT,BUDKCOD+L'BUDKCOD,0,                                +        
               L'BUDKEY-(BUDKCOD+L'BUDKCOD-BUDKEY)                              
         LKKEY E                                                                
*                                                                               
BGTKEYT  LKKEY H,BUDKEY            ** BUDGET 2 DRIVER TABLE **                  
         LKKEY LIT,BUDKTYP,BUDKTYPQ                                             
         LKKEY SIN,BUDKCPY,COMPANY                                              
         LKKEY LIT,BUDKUNT,C'1'                                                 
         LKKEY LIT,BUDKLDG,C'C'                                                 
         LKKEY RNG,BUDKACT,PL1CRNGE                                             
         LKKEY LIT,BUDKWORK,C' '                                                
         LKKEY SIN,BUDKCCPY,COMPANY                                             
         LKKEY LIT,BUDKCUNT,C'1'                                                
         LKKEY RNG,BUDKCLDG,LDGRANGE                                            
         LKKEY RNG,BUDKCACT,CNTRANGE                                            
         LKKEY WMP,BUDKBUDN,ABUD                                                
         LKKEY LIT,BUDKBCKT,0                                                   
         LKKEY LIT,BUDKBCKT+L'BUDKBCKT,0,                              +        
               L'BUDKEY-(BUDKBCKT+L'BUDKBCKT-BUDKEY)                            
         LKKEY E                                                                
*                                                                               
BGTKYT2  LKKEY H,BUDKEY            ** BUDGET 3 TABLE W/ CONTRA U/L              
         LKKEY LIT,BUDKTYP,BUDKTYPQ                                             
         LKKEY SIN,BUDKCPY,COMPANY                                              
         LKKEY LIT,BUDKUNT,C'1'                                                 
         LKKEY LIT,BUDKLDG,C'C'                                                 
         LKKEY RNG,BUDKACT,PL1CRNGE                                             
         LKKEY LIT,BUDKWORK,C' '                                                
         LKKEY SIN,BUDKCCPY,COMPANY                                             
         LKKEY SIN,BUDKCUNT,CNTUL,L'BUDKCUNT+L'BUDKCLDG                         
         LKKEY RNG,BUDKCACT,CNTRANGE                                            
         LKKEY WMP,BUDKBUDN,ABUD                                                
         LKKEY LIT,BUDKBCKT,0                                                   
         LKKEY LIT,BUDKBCKT+L'BUDKBCKT,0,                              +        
               L'BUDKEY-(BUDKBCKT+L'BUDKBCKT-BUDKEY)                            
         LKKEY E                                                                
*                                                                               
CLIKEYT  LKKEY H,PLCKEY            ** CLIENT P&L KEY DRIVER **                  
         LKKEY LIT,PLCKTYP,PLCKTYPQ                                             
         LKKEY LIT,PLCKSUB,PLCKSUBQ                                             
         LKKEY SIN,PLCKCPY,COMPANY                                              
         LKKEY LST,PLCKMTHD,MTHLSTN                                             
         LKKEY LIT,PLCKSPR1,0                                                   
         LKKEY RNG,PLCK1CAC,PL1CRNGE                                            
         LKKEY RNG,PLCKCLDG,LDGRANGE                                            
         LKKEY RNG,PLCKCACT,ACTRANGE                                            
         LKKEY RNG,PLCKAGYO,OFFRANGE                                            
         LKKEY RNG,PLCKYYMM,STRMOA2                                             
         LKKEY E                                                                
*                                                                               
CNTKEYT  LKKEY H,CACKEY            ** Contra Acc Key Driver 11-13 **            
         LKKEY SIN,CACKCPY,COMPANY                                              
         LKKEY SIN,CACKUNT,ACTUL,L'CACKUNT+L'CACKLDG                            
         LKKEY RNG,CACKACT,CACRANGE                                             
         LKKEY LIT,CACKOFF,C' '                                                 
         LKKEY SIN,CACKCCPY,COMPANY                                             
         LKKEY SIN,CACKCUNT,CNTUL,L'CACKCUNT+L'CACKCLDG                         
         LKKEY RNG,CACKCACT,CNTRANGE                                            
         LKKEY LIT,CACKCACT+L'CACKCACT,C' ',                           +        
               L'CACKEY-(CACKCACT+L'CACKCACT-CACKEY)                            
         LKKEY E                                                                
*                                                                               
CNTKYT2  LKKEY H,CACKEY            ** Contra Acc Key Driver 14-16 **            
         LKKEY SIN,CACKCPY,COMPANY                                              
         LKKEY SIN,CACKUNT,ACTUL,L'CACKUNT+L'CACKLDG                            
         LKKEY RNG,CACKACT,CACRANGE                                             
         LKKEY LIT,CACKOFF,C' '                                                 
         LKKEY SIN,CACKCCPY,COMPANY                                             
         LKKEY SIN,CACKCUNT,CNTUL,L'CACKCUNT+L'CACKCLDG                         
         LKKEY RNG,CACKCACT,CNTRANGE                                            
         LKKEY LIT,CACKSPAC,C' '                                                
         LKKEY LST,CACKBTYP,MTHLSTN                                             
         LKKEY LST,CACKSTYP,TYPLST,L'CACKSTYP-2                                 
         LKKEY LIT,CACKSTYP+1,C' ',L'CACKSTYP-1                                 
         LKKEY E                                                                
*                                                                               
CNTKYT3  LKKEY H,CACKEY            ** Contra Acc Key Driver 11,12 **            
         LKKEY SIN,CACKCPY,COMPANY        ** with sublist **                    
         LKKEY SIN,CACKUNT,ACTUL,L'CACKUNT+L'CACKLDG                            
         LKKEY RNG,CACKACT,CACRANGE                                             
         LKKEY WMP,CACKOFF,ASUBLST                                              
         LKKEY SIN,CACKCCPY,COMPANY                                             
         LKKEY SIN,CACKCUNT,CNTUL,L'CACKCUNT+L'CACKCLDG                         
         LKKEY RNG,CACKCACT,CNTRANGE                                            
         LKKEY LIT,CACKCACT+L'CACKCACT,C' ',                           +        
               L'CACKEY-(CACKCACT+L'CACKCACT-CACKEY)                            
         LKKEY E                                                                
*                                                                               
LDGKEYT  LKKEY H,LDGKEY            ** LEDGER DRIVER TABLE **                    
         LKKEY SIN,LDGKCPY,COMPANY                                              
         LKKEY LST,LDGKUNT,LDGLST,L'LDGKUNT+L'LDGKLDG                           
         LKKEY LIT,LDGKUNT+L'LDGKUNT+L'LDGKLDG,C' ',                   +        
               L'LDGKEY-(LDGKUNT+L'LDGKUNT+L'LDGKLDG-LDGKEY)                    
         LKKEY E                                                                
*                                                                               
MTHKEYT  LKKEY H,CMTKEY            ** METHOD DRIVER TABLE **                    
         LKKEY LIT,CMTKTYP,CMTKTYPQ                                             
         LKKEY LIT,CMTKSUB,CMTKSUBQ                                             
         LKKEY SIN,CMTKCPY,COMPANY                                              
         LKKEY RNG,CMTKMTHD,MTHRANGE                                            
         LKKEY LIT,CMTKMTHD+L'CMTKMTHD,C' ',                           +        
               L'CMTKEY-(CMTKMTHD+L'CMTKMTHD-CMTKEY)                            
         LKKEY E                                                                
*                                                                               
PERKEYT  LKKEY H,CHDKEY            ** 1R PERSONNEL KEY DRIVER **                
         LKKEY SIN,CHDKCPY,COMPANY                                              
         LKKEY LIT,CHDKUNT,C'1'                                                 
         LKKEY LIT,CHDKLDG,C'R'                                                 
         LKKEY SIN,CHDKACT,PERACT                                               
         LKKEY LIT,CHDKWRK,C' '                                                 
         LKKEY SIN,CHDKCCPY,COMPANY                                             
         LKKEY LIT,CHDKCUNT,C'1'                                                
         LKKEY LIT,CHDKCLDG,C'C'                                                
         LKKEY SIN,CHDKCACT,CSTACT                                              
         LKKEY LIT,CHDKSPCS,C' '                                                
         LKKEY LIT,CHDKBTYP,C'H'                                                
         LKKEY LIT,CHDKNULL,C' '                                                
         LKKEY E                                                                
*                                                                               
PLDKEYT  LKKEY H,PLDKEY            ** DIRECT TIME KEY DRIVER **                 
         LKKEY LIT,PLDKTYP,PLDKTYPQ                                             
         LKKEY LIT,PLDKSUB,PLDKSUBQ                                             
         LKKEY SIN,PLDKCPY,COMPANY                                              
         LKKEY LST,PLDKMTHD,MTHLSTN                                             
         LKKEY LIT,PLDKSPR1,0                                                   
         LKKEY RNG,PLDKCACT,CSTRANGE                                            
         LKKEY RNG,PLDKRACT,PERRANGE                                            
         LKKEY RNG,PLDKANAL,ANLRANGE                                            
         LKKEY RNG,PLDKYYMM,STRMOA2                                             
         LKKEY LST,PLDKPTYP,TYPLST                                              
         LKKEY E                                                                
*                                                                               
RAPKEYT  LKKEY H,RAPKEY            ** RAP KEY DRIVER TABLE FOR 1C **            
         LKKEY LIT,RAPKTYP,RAPKTYPQ                                             
         LKKEY SIN,RAPKCPY,COMPANY                                              
         LKKEY LIT,RAPKRTYP,RAPKRPAL                                            
         LKKEY RNG,RAPKDATE,ACCSDTE                                             
         LKKEY RNG,RAPKTIME,ACCSTIM                                             
         LKKEY SIN,RAPKACPY,COMPANY                                             
         LKKEY SIN,RAPKAUNT,UNTLDG,L'RAPKAUNT+L'RAPKALDG                        
         LKKEY RNG,RAPKAACT,ACTRANGE                                            
         LKKEY RNG,RAPKASPR,RAPKRNGE                                            
         LKKEY E                                                                
*                                                                               
RAPKYT2  LKKEY H,RAPKEY            ** RAP KEY DRIVER TABLE 2 FOR 1R **          
         LKKEY LIT,RAPKTYP,RAPKTYPQ                                             
         LKKEY SIN,RAPKCPY,COMPANY                                              
         LKKEY LST,RAPKRTYP,RAP1RLS                                             
         LKKEY RNG,RAPKDATE,ACCSDTE                                             
         LKKEY RNG,RAPKTIME,ACCSTIM                                             
         LKKEY SIN,RAPKACPY,COMPANY                                             
         LKKEY SIN,RAPKAUNT,UNTLDG,L'RAPKAUNT+L'RAPKALDG                        
         LKKEY RNG,RAPKAACT,ACTRANGE                                            
         LKKEY RNG,RAPKASPR,RAPKRNGE                                            
         LKKEY E                                                                
*                                                                               
RAPKYT3  LKKEY H,RAPKEY            ** RAP KEY DRIVER TABLE 3 FOR SJ **          
         LKKEY LIT,RAPKTYP,RAPKTYPQ                                             
         LKKEY SIN,RAPKCPY,COMPANY                                              
         LKKEY LST,RAPKRTYP,RAPSJLS                                             
         LKKEY RNG,RAPKDATE,ACCSDTE                                             
         LKKEY RNG,RAPKTIME,ACCSTIM                                             
         LKKEY SIN,RAPKACPY,COMPANY                                             
         LKKEY SIN,RAPKAUNT,UNTLDG,L'RAPKAUNT+L'RAPKALDG                        
         LKKEY RNG,RAPKAACT,ACTRANGE                                            
         LKKEY RNG,RAPKASPR,RAPKRNGE                                            
         LKKEY E                                                                
*                                                                               
SCRKEYT  LKKEY H,RESKEY            ** SCRIBE KEY DRIVER **                      
         LKKEY LIT,RESKTYP,RESKTYPQ                                             
         LKKEY LIT,RESKSUB,RESKSUBQ                                             
         LKKEY SIN,RESKCPY,COMPANY                                              
         LKKEY RNG,RESKFORM,FRMRANGE                                            
         LKKEY LIT,RESKSPCS,C' '                                                
         LKKEY RNG,RESKSEQ,SEQRANGE                                             
         LKKEY LIT,RESKSEQ+L'RESKSEQ,C' ',                             +        
               L'RESKEY-(RESKSEQ+L'RESKSEQ-RESKEY)                              
         LKKEY E                                                                
*                                                                               
GLSKEYT  LKKEY H,GLBKEY,SAVED      ** G/L Summary driver table **               
         LKKEY LIT,GLBKTYP,GLBKTYPQ        ** with office **                    
         LKKEY SIN,GLBKCPY,COMPANY                                              
         LKKEY RNG,GLBKGLDG,GLSLDRNG                                            
         LKKEY RNG,GLBKGACT,GLSARNGE                                            
         LKKEY LST,GLBKGOFF,OFFLST                                              
         LKKEY RNG,GLBKSUNT,ULRANGE,L'GLBKSUNT+L'GLBKSLDG                       
         LKKEY RNG,GLBKSACT,GLSACRNG                                            
         LKKEY RNG,GLBKSOFF,OFFRANGE                                            
         LKKEY RNG,GLBKSCUL,ULRANGE                                             
         LKKEY RNG,GLBKSCA,GLSSCRNG                                             
         LKKEY E                                                                
*                                                                               
GLSKYT2  LKKEY H,GLBKEY,SAVED      ** G/L Summary driver table **               
         LKKEY LIT,GLBKTYP,GLBKTYPQ      ** without Office **                   
         LKKEY SIN,GLBKCPY,COMPANY                                              
         LKKEY RNG,GLBKGLDG,GLSLDRNG                                            
         LKKEY RNG,GLBKGACT,GLSARNGE                                            
         LKKEY RNG,GLBKGOFF,OFFRANGE                                            
         LKKEY RNG,GLBKSUNT,ULRANGE,L'GLBKSUNT+L'GLBKSLDG                       
         LKKEY RNG,GLBKSACT,GLSACRNG                                            
         LKKEY RNG,GLBKSOFF,OFFRANGE                                            
         LKKEY RNG,GLBKSCUL,ULRANGE                                             
         LKKEY RNG,GLBKSCA,GLSSCRNG                                             
         LKKEY E                                                                
*                                                                               
GLAKEYT  LKKEY H,ACTKEY,SAVED      ** G/L Account Key Driver Table **           
         LKKEY SIN,ACTKCPY,COMPANY                                              
         LKKEY RNG,ACTKUNT,GLTULRNG,L'TRNKUNT+L'TRNKLDG                         
         LKKEY RNG,ACTKACT,GLTARNGE                                             
         LKKEY LIT,ACTKACT+L'ACTKACT,C' ',                             +        
               L'ACTKEY-(ACTKACT+L'ACTKACT-ACTKEY)                              
         LKKEY E                                                                
                                                                                
FLTRPK   CLC   ACCSDTE,IOKEY+(RAPKDATE-RAPKEY)  REQ DATE < FILE = EQ            
         JL    FLTRPKY                                                          
         JH    FLTRPKN                                                          
         CLC   ACCTIME,IOKEY+(RAPKTIME-RAPKEY)  REQ TIME > FILE = NEQ           
         JH    FLTRPKN                                                          
FLTRPKY  LHI   R1,0                                                             
         J     FLTRPKX                                                          
FLTRPKN  LHI   R1,1                                                             
FLTRPKX  CHI   R1,0                                                             
         BR    RE                                                               
                                                                                
GLABUFQ  EQU   1                   ** G/L Account Buffer **                     
GLABUF   BUFFD TYPE=D,KEYLEN=0,COMLEN=0,BUFFERS=255                             
                                                                                
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
SAVED    DSECT ,                   ** DSECT TO COVER SAVED STORAGE **           
*                                                                               
DUMMY    DS    0X                  DUMMY OUTPUT FOR DDLINK VALUES               
*                                                                               
WVALUES  DS    0X                  ** LITERAL VALUES **                         
*                                                                               
LDGLST   DS    XL(LDGLSTL)         LEDGER LIST                                  
TYPLST   DS    XL(TYPLSTL)         SALARY TYPE LIST                             
RAP1RLS  DS    XL(RAP1RLSL)        RAPPER POINTER TYPE LIST FOR 1R              
RAPSJLS  DS    XL(RAPSJLSL)        RAPPER POINTER TYPE LIST FOR SJ              
ACTRANGE DS    XL(L'ACTKACT*2)     ACCOUNT RANGE                                
GLSACRNG DS    XL(L'ACTKACT*2)     ACCOUNT RANGE                                
MTHRANGE DS    XL(L'CMTKMTHD*2)    METHOD RANGE                                 
BUDRANGE DS    XL(L'BUDKNO1*2)     BUDGET RANGE                                 
BCDRANGE DS    XL(L'BUDKCOD*2)     BUDGET CODE RANGE                            
OFFRANGE DS    XL(L'OFFKOFF*2)     OFFICE RANGE                                 
ULRANGE  DS    XL((L'ACTKUNT+L'ACTKLDG)*2)   UNIT/LEDGER RANGE                  
ANLRANGE DS    XL(L'PLDKANAL*2)    ANALYSIS CODE RANGE                          
LDGRANGE DS    XL(L'PLCKCLDG*2)    LEDGER RANGE                                 
FRMRANGE DS    XL(L'RESKFORM*2)    SCRIBE FORMAT RANGE                          
SEQRANGE DS    XL(L'RESKSEQ*2)     SCRIBE SEQUENCE NUMBER RANGE                 
RAPKRNGE DS    XL(L'RAPKASPR*2)    RAP KEY FIELD SPARE RANGE                    
GLSSCRNG DS    CL(L'GLBKSCA*2)     G/L SUMMARY RECORD UNIT S CONTRA AC          
TRNRFRNG DS    CL(L'TRNKREF*2)     TRANSACTION REFERENCE RANGE                  
RNG341FF DS    XL6                 3 Byte Range from 41 - FF                    
RNG100FF DS    XL2                 1 Byte Range from 00 - FF                    
*                                                                               
WVALUEL  EQU   *-WVALUES                                                        
*                                                                               
LEDGTAB  DS    XL(DEFLLNQ)         LEDGER TABLE                                 
*                                                                               
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
*                                                                               
PUTAMOS  DS    XL(L'BUKMOS)        MOS DATE                                     
PUTAAMT  DS    PL(L'PKAMT)         AMOUNT                                       
*                                                                               
* ACCOUNTS DOWNLOAD                                                             
*                                                                               
REQVALS  DS    0F                  ** REQUEST VALUES **                         
ENDMOA   DS    PL3                 END DATE OF ACTIVITY (YYMMDD)                
ACCTIME  DS    XL3                 BINARY TIME FOR REQUEST                      
ACCSDTE  DS    XL2                 LAST DATE OF ACTIVITY (COMPRESS)             
ACCEDTE  DS    XL2                                                              
ACCSTIM  DS    XL3                 LAST TIME OF ACTIVITY (BINARY)               
ACCETIM  DS    XL3                                                              
*                                                                               
* P&L AMOUNTS                                                                   
*                                                                               
METHOD   DS    CL(L'CAPKMTHD)      ALLOCATION METHOD                            
BUDIND   DS    XL1                 BUDGET INDEX                                 
ABUD     DS    AL3                 ADDRESS OF BUDGET BLOCK                      
SUB2IND  DS    XL1                 SAVED AREA OFFICE SUB LIST INDEX             
ASUBLST  DS    AL3                 SAVED ADDRESS OF OFFICE SUB LIST             
SECACC   DS    XL1                 SECURITY ACCESS (A-ACCT/C-CONTRA)            
STRMOA   DS    PL3                 START DATE OF ACTIVITY (YYMMDD)              
STRMOA2  DS    PL2                 START MONTH OF ACTIVITY (YYMM)               
ENDMOA2  DS    PL2                 END MONTH OF ACTIVITY (YYMM)                 
MOATABN  DS    H                   ACTUAL N'ENTRIES IN MOATAB                   
MOATABM  EQU   36                  MAXIMUM N'REQUEST MONTHS                     
MOATABM2 EQU   12                  MAXIMUM N'REQUEST MONTHS for G/L             
MOATAB   DS    (MOATABM)PL2        MOA TABLE                                    
*                                                                               
* CONTRA BUCKETS                                                                
*                                                                               
ACTULA   DS    0CL(L'ACTKULA)      UNIT/LEDGER/ACC                              
ACTUL    DS    0CL(L'ACTKUNT+L'ACTKLDG)  U/L                                    
ACTUNT   DS    CL(L'ACTKUNT)              Unit                                  
ACTLDG   DS    CL(L'ACTKLDG)               Ledger                               
ACTACT   DS    CL(L'ACTKACT)                ACCOUNT                             
CNTULA   DS    0CL(L'ACTKULA)      CONTRA U/L/A                                 
CNTUL    DS    CL(L'ACTKUNT+L'ACTKLDG)     UNIT/LEDGER                          
CNTACT   DS    CL(L'ACTKACT)                  ACCOUNT                           
*                                                                               
* PERSONNEL DOWNLOAD                                                            
*                                                                               
PER1CULA DS    0CL(L'ACTKULA)      UNIT/LEDGER/ACC                              
PER1CUL  DS    0CL(L'ACTKUNT+L'ACTKLDG)    UNIT/LEDGER                          
PER1CUNT DS    CL(L'ACTKUNT)                   UNIT                             
PER1CLDG DS    CL(L'ACTKLDG)                   LEDGER                           
PER1CACT DS    CL(L'ACTKACT)                      ACCOUNT                       
PER14ULA DS    0CL(L'ACTKULA)      CONTRA U/L/A                                 
PER14UL  DS    0CL(L'ACTKUNT+L'ACTKLDG)    UNIT/LEDGER                          
PER14UNT DS    CL(L'ACTKUNT)                   UNIT                             
PER14LDG DS    CL(L'ACTKLDG)                   LEDGER                           
PER14ACT DS    CL(L'ACTKACT)                      ACCOUNT                       
*                                                                               
* SCRIBE FORMAT DOWNLOAD                                                        
*                                                                               
FRMADTE  DS    XL2                 FORMAT ACTIVITY DATE                         
FRMATIM  DS    PL3                 FORMAT ACTIVITY TIME                         
*                                                                               
* REQUEST TEMPLATE DOWNLOAD                                                     
*                                                                               
RQTMPLT  DS    CL10                REQUEST TEMPLATE                             
*                                                                               
         ORG   PER1CULA                                                         
*                                                                               
* General Ledger Summary Records Download                                       
*                                                                               
GLMOARNG DS    0CL8                G/L Summary MOA Range                        
GLSTRMOA DS    CL4                     Starting MOA                             
GLENDMOA DS    CL4                     Ending Moa                               
GLSADTE  DS    XL2                 G/L Summary ACTIVITY DATE                    
GLSATIM  DS    PL3                 G/L Summary ACTIVITY TIME                    
GLEQLEV  DS    CL1                 Account Equivalency level                    
GLOFF    DS    CL2                 Office/Office List                           
*                                                                               
         ORG   GLSADTE                                                          
*                                                                               
* GENERAL LEDGER TRANSACTION  DOWNLOAD                                          
*                                                                               
*                                  MOA range used from above                    
GLTGULA  DS    0CL14               G/L Account U/L/A                            
GLTGUL   DS    0CL2                    G/L Account U/L                          
GLTGUNT  DS    CL1                         G/L Account Unit                     
GLTGLDG  DS    CL1                         G/L Account Ledger                   
GLTGACT  DS    CL12                        G/L Account Account                  
GLTOFF   DS    CL2                 Office/Office List                           
GLTMED   DS    CL1                 Media                                        
GLTCULA  DS    0CL14               G/L Contra Account U/L/A                     
GLTCUL   DS    0CL2                    G/L Contra Account U/L                   
GLTCUNT  DS    CL1                         G/L Contra Account Unit              
GLTCLDG  DS    CL1                         G/L Contra Account Ledger            
GLTCACT  DS    CL12                        G/L Contra Account Account           
GLTCLI   DS    CL3                 Client                                       
GLTSMD   DS    CL2                 System/Media                                 
         ORG                                                                    
*                                                                               
ULIND    DS    XL1                 U/L INDEX                                    
AUL      DS    AL3                 ADDRESS OF UNIT/LEDGER BLOCK                 
ACTIND   DS    XL1                 ACCOUNT INDEX                                
AACT     DS    AL3                 ADDRESS OF ACCOUNT BLOCK                     
EXCIND   DS    XL1                 EXCLUDED ACCOUNT INDEX                       
AEXACT   DS    AL3                 ADDRESS OF EXCLUDED ACCOUNT BLOCK            
MTHIND   DS    XL1                 METHOD INDEX                                 
AMTH     DS    AL3                 ADDRESS OF METHOD BLOCK                      
ULAIND   DS    XL1                 UNIT/LEDGER/ACCOUNT INDEX                    
AULA     DS    AL3                 ADDRESS OF U/L/A BLOCK                       
SUBIND   DS    XL1                 OFFICE SUB LIST INDEX                        
ASUB     DS    AL3                 ADDRESS OF OFFICE SUB LIST BLOCK             
GLSIND   DS    XL1                 G/L U/L/A INDEX                              
GLSULA   DS    AL3                 Address of U/L/A block                       
*                                                                               
REQVALL  EQU   *-REQVALS                                                        
*                                                                               
*** OUTPUT VALUES ****                                                          
*                                                                               
OUTVALS  DS    0X                  ** OUTPUT VALUES **                          
*                                                                               
* ** ACCOUNT VALUES **                                                          
*                                                                               
ACTVALS  DS    0X                                                               
ACTALDG  DS    CL(L'ACTKUNT+L'ACTKLDG)                                          
ACTLVS   DS    0C                                                               
ACTLV1CD DS    CL(L'ACTKACT)       ACCOUNT LEVEL 1 CODE                         
ACTLV1NM DS    CL(L'NAMEREC)       ACCOUNT LEVEL 1 NAME                         
ACTLV2CD DS    CL(L'ACTKACT)       ACCOUNT LEVEL 2 CODE                         
ACTLV2NM DS    CL(L'NAMEREC)       ACCOUNT LEVEL 2 NAME                         
ACTLV3CD DS    CL(L'ACTKACT)       ACCOUNT LEVEL 3 CODE                         
ACTLV3NM DS    CL(L'NAMEREC)       ACCOUNT LEVEL 3 NAME                         
ACTLV4CD DS    CL(L'ACTKACT)       ACCOUNT LEVEL 4 CODE                         
ACTLV4NM DS    CL(L'NAMEREC)       ACCOUNT LEVEL 4 NAME                         
ACTLVLNQ EQU   *-ACTLVS                                                         
*                                                                               
ACTFLTS  DS    0CL5                RSTFILT1-5                                   
ACTFLT1  DS    CL(L'RSTFILT1)      ACCOUNT FILTER 1                             
ACTFLT2  DS    CL(L'RSTFILT2)      ACCOUNT FILTER 2                             
ACTFLT3  DS    CL(L'RSTFILT3)      ACCOUNT FILTER 3                             
ACTFLT4  DS    CL(L'RSTFILT4)      ACCOUNT FILTER 4                             
ACTFLT5  DS    CL(L'RSTFILT5)      ACCOUNT FILTER 5                             
*                                                                               
ACTLEV   DS    CL1                 LEVEL OF THE ACCOUNT                         
ACTOFFVL DS    CL(L'PPRGAOFF)      OFFICE FROM PRODUCTION PROFILE               
ACTSTAT  DS    XL1                                                              
ACTFLTEQ EQU   X'01'               FILTERS ARE UNDEFINED                        
ACTLEVQ  EQU   X'02'               OFFICE IS NOT DEFINED AT CUR LEV             
*                                                                               
* ALWAYS KEEP DATE/TIME TOGETHER                                                
*                                                                               
ACTDATE  DS    XL(L'RAPKDATE)      ACTIVITY DATE (COMPRESSED)                   
ACTTIME  DS    XL(L'RAPKTIME)      ACTIVITY TIME (BINARY SECONDS)               
ACTDTLNQ EQU   *-ACTDATE                                                        
ACTVALL  EQU   *-ACTVALS                                                        
*                                                                               
         ORG   OUTVALS             RESET DISPLACEMENT                           
*                                                                               
* ** METHOD VALUES **                                                           
*                                                                               
MTHVALS  DS    0X                                                               
MTHCODE  DS    CL(L'METCODE)       ALLOCATION METHOD CODE                       
MTHNUMB  DS    XL(L'METNUM)        ALLOCATION METHOD NUMBER                     
MTHNAME  DS    CL(L'NAMEREC)       ALLOCATION METHOD NAME                       
MTHVALL  EQU   *-MTHVALS                                                        
*                                                                               
         ORG   OUTVALS             RESET DISPLACEMENT                           
*                                                                               
* ** BUDGET VALUES **                                                           
*                                                                               
BUDVALS  DS    0X                                                               
BUDNUMB  DS    XL(L'BUDKNO1)       BUDGET NUMBER                                
BUDCODE  DS    CL(L'BUDKCOD)       BUDGET CODE                                  
BUDLEV   DS    XL(L'BIVACLV)       ACCOUNT LEVEL OF BUDGET                      
BUDNAME  DS    CL(L'NAMEREC)       BUDGET NAME                                  
BUDVALL  EQU   *-BUDVALS                                                        
*                                                                               
         ORG   OUTVALS             RESET DISPLACEMENT                           
*                                                                               
* ** P&L AMOUNTS VALUES **                                                      
*                                                                               
PLAVALS  DS    0X                                                               
PLATYP   DS    CL1                 M-METHOD OR B-BUDGET                         
PLANO    DS    CL5                 METHOD CODE OR BUDGET NUMBER                 
PLAACL1  DS    CL(L'PLCK1CAC)      1C ACCOUNT CODE LEVEL 1                      
PLAACL2  DS    CL(L'PLCK1CAC)      1C ACCOUNT CODE LEVEL 2                      
PLAACL3  DS    CL(L'PLCK1CAC)      1C ACCOUNT CODE LEVEL 3                      
PLAACL4  DS    CL(L'PLCK1CAC)      1C ACCOUNT CODE LEVEL 4                      
PLACLDG  DS    CL(L'PLCKCLDG)      CONTRA LEDGER CODE                           
PLACAL1  DS    CL(L'PLCKCACT)      CONTRA ACCOUNT LEVEL 1 CODE                  
PLACAL2  DS    CL(L'PLCKCACT)      CONTRA ACCOUNT LEVEL 2 CODE                  
PLACAL3  DS    CL(L'PLCKCACT)      CONTRA ACCOUNT LEVEL 3 CODE                  
PLACAL4  DS    CL(L'PLCKCACT)      CONTRA ACCOUNT LEVEL 4 CODE                  
PLACLIO  DS    CL(L'PLAKCLIO)      CLIENT OFFICE                                
*                                                                               
PLAENT   DS    0X                  MONTHLY AMOUNTS/REPLICATION                  
PLAAMTS  DS    PL8                 AMOUNT VALUE                                 
PLAREP   DS    XL1                 REPLICATION FACTOR                           
PLAENTL  EQU   *-PLAENT                                                         
         DS    (MOATABM-1)XL(PLAENTL)                                           
PLAVALL  EQU   *-PLAVALS                                                        
*                                                                               
         ORG   OUTVALS             RESET DISPLACEMENT                           
*                                                                               
* ** CONTRA-ACCOUNT VALUES **                                                   
*                                                                               
CNTVALS  DS    0X                                                               
CNTLVS   DS    0C                                                               
CNTLV1CD DS    CL(L'CACKACT)       ACCOUNT LEVEL 1 CODE                         
CNTLV2CD DS    CL(L'CACKACT)       ACCOUNT LEVEL 2 CODE                         
CNTLV3CD DS    CL(L'CACKACT)       ACCOUNT LEVEL 3 CODE                         
CNTLV4CD DS    CL(L'CACKACT)       ACCOUNT LEVEL 4 CODE                         
CNTCLDG  DS    CL(L'CACKLDG)       CONTRA LEDGER CODE                           
CNTCALV1 DS    CL(L'CACKCACT)      CONTRA ACCOUNT LEVEL 1 CODE                  
CNTCALV2 DS    CL(L'CACKCACT)      CONTRA ACCOUNT LEVEL 2 CODE                  
CNTCALV3 DS    CL(L'CACKCACT)      CONTRA ACCOUNT LEVEL 3 CODE                  
CNTCALV4 DS    CL(L'CACKCACT)      CONTRA ACCOUNT LEVEL 4 CODE                  
CNTLVLNQ EQU   *-CNTLVS                                                         
CNTTYP   DS    CL1                 M-METHOD OR B-BUDGET                         
CNTNO    DS    CL5                 METHOD CODE OR BUDGET NUMBER                 
*                                                                               
CNTENT   DS    0X                  MONTHLY AMOUNTS/REPLICATION                  
CNTASAL  DS    0PL8                SALARY                                       
CNTAPEN  DS    0PL8                PENSION                                      
CNTABEN  DS    0PL8                BENEFIT                                      
CNTAMTS  DS    PL8                 AMOUNT VALUE                                 
CNTREP   DS    XL1                 REPLICATION FACTOR                           
CNTENTL  EQU   *-CNTENT                                                         
         DS    (MOATABM-1)XL(CNTENTL)                                           
*                                                                               
CNTVALL  EQU   *-CNTVALS                                                        
*                                                                               
         ORG   OUTVALS             RESET DISPLACEMENT                           
*                                                                               
* ** PERSONNEL VALUES **                                                        
*                                                                               
PERVALS  DS    0X                  ** OUTPUT VALUES **                          
PERMTH   DS    CL(L'PLDKMTHD)      METHOD CODE                                  
PER1RL1  DS    CL(L'PLDKCACT)      PERSON 1R ACCOUNT LEVEL 1 CODE               
PER1RL2  DS    CL(L'PLDKCACT)      PERSON 1R ACCOUNT LEVEL 2 CODE               
PER1RL3  DS    CL(L'PLDKCACT)      PERSON 1R ACCOUNT LEVEL 3 CODE               
PER1RL4  DS    CL(L'PLDKCACT)      PERSON 1R ACCOUNT LEVEL 4 CODE               
PERANAL  DS    CL(L'PLDKANAL)      ANALYSIS CODE                                
PER1CL1  DS    CL(L'PLDKCACT)      PERSON 1R ACCOUNT LEVEL 1 CODE               
PER1CL2  DS    CL(L'PLDKCACT)      PERSON 1R ACCOUNT LEVEL 2 CODE               
PER1CL3  DS    CL(L'PLDKCACT)      PERSON 1R ACCOUNT LEVEL 3 CODE               
PER1CL4  DS    CL(L'PLDKCACT)      PERSON 1R ACCOUNT LEVEL 4 CODE               
PERLNQ   EQU   *-PERVALS                                                        
*                                                                               
PERENT   DS    0X                  MONTHLY AMOUNTS/REPLICATION                  
PERHOUR  DS    0PL8                HOURS                                        
PERASAL  DS    0PL8                SALARY                                       
PERAPEN  DS    0PL8                PENSION                                      
PERABEN  DS    0PL8                BENEFIT                                      
PERAMTS  DS    PL8                 AMOUNT VALUE                                 
PERREP   DS    XL1                 REPLICATION FACTOR                           
PERENTL  EQU   *-PERAMTS                                                        
         DS    (MOATABM-1)XL(PERENTL)                                           
*                                                                               
PERVALL  EQU   *-PERVALS                                                        
*                                                                               
         ORG   OUTVALS             RESET DISPLACEMENT                           
*                                                                               
* ** LEDGER VALUES **                                                           
*                                                                               
LDGVALS  DS    0X                                                               
LDGCODE  DS    CL(L'LDGKUNT+L'LDGKLDG)                                          
LDGNAME  DS    CL(L'NAMEREC)       LEDGER NAME                                  
LDGOFFP  DS    CL(L'LDGOPOS)       LEDGER OFFICE POSITION                       
LDGCLIP  DS    CL(L'LDGCPOS)       LEDGER CLIENT POSITION                       
*                                                                               
LDGDSCS  DS    0X                  LEDGER LEVEL DESCRIPTIONS                    
LDGL1DSC DS    CL(L'ACLVDESC)      LEVEL 1 DESCRIPTION                          
LDGL2DSC DS    CL(L'ACLVDESC)      LEVEL 2 DESCRIPTION                          
LDGL3DSC DS    CL(L'ACLVDESC)      LEVEL 3 DESCRIPTION                          
LDGL4DSC DS    CL(L'ACLVDESC)      LEVEL 4 DESCRIPTION                          
LDGDSCLN EQU   *-LDGDSCS                                                        
*                                                                               
LDGLNS   DS    0X                  LEDGER LEVEL LENGTHS                         
LDGL1LN  DS    XL(L'ACLVLEN)       ACCOUNT LEVEL 1 LENGTH                       
LDGL2LN  DS    XL(L'ACLVLEN)       ACCOUNT LEVEL 2 LENGTH                       
LDGL3LN  DS    XL(L'ACLVLEN)       ACCOUNT LEVEL 3 LENGTH                       
LDGL4LN  DS    XL(L'ACLVLEN)       ACCOUNT LEVEL 4 LENGTH                       
LDGLNSQ  EQU   *-LDGLNS                                                         
*                                                                               
LDGVALL  EQU   *-LDGVALS                                                        
*                                                                               
* ** FILTER VALUES **                                                           
*                                                                               
FLTVALS  DS    0X                                                               
FLTNUMB  DS    CL(L'CTVKNUM)            FILTER NUMBER                           
FLTDSC   DS    CL(L'CTVNMNAM)           FILTER DESCRIPTION                      
FLTENT   DS    0CL(L'FLTCODE+L'FLTNAME) FILTER CODE AND NAMES                   
FLTCODE  DS    CL(L'CTVVLCHR)                  FILTER CODE                      
FLTNAME  DS    CL(L'CTVVLNAM)                  FILTER NAME                      
FLTENTQ  EQU   *-FLTENT                                                         
         DS    (FLTMAX)CL(FLTENTQ)                                              
FLTVALL  EQU   *-FLTVALS                                                        
FLTMAX   EQU   60                                                               
*                                                                               
         ORG   OUTVALS             RESET DISPLACEMENT                           
*                                                                               
* ** SCRIBE DOWNLOAD VALUES **                                                  
*                                                                               
FRMVALS  DS    0X                                                               
FRMCODE  DS    CL(L'RESKFORM)      FORMAT CODE                                  
FRMNAME  DS    CL(L'NAMEREC)       FORMAT NAME                                  
FRMEXT   DS    CL10                FORMAT EXTENTION                             
*                                                                               
* ALWAYS KEEP DATE/TIME TOGETHER                                                
*                                                                               
FRMDATE  DS    XL(L'RAPKDATE)      ACTIVITY DATE (COMPRESSED)                   
FRMTIME  DS    XL(L'RAPKTIME)      ACTIVITY TIME (BINARY SECONDS)               
FRMDTLNQ EQU   *-FRMDATE                                                        
FRMVALL  EQU   *-FRMVALS                                                        
*                                                                               
         ORG   OUTVALS                                                          
*                                                                               
* ** SCRIBE DOWNLOAD VALUES **                                                  
*                                                                               
RQTVALS  DS    0X                                                               
RQTCHRFL DS    CL50                CHARACTER FIELD DESCRIPTION                  
RQTBINFL DS    XL4                 BINARY FIELD DESCRIPTION                     
RQTVALL  EQU   *-RQTVALS                                                        
*                                                                               
RQTRECNO DS    XL(L'LD_CODE)       RECORD MAP NUMBER                            
RQTFLDNO DS    XL(L'LH_MAPN)       FIELD  MAP NUMBER                            
*                                                                               
         ORG   OUTVALS                                                          
*                                                                               
* ** G/L Summary Download Values **                                             
*                                                                               
GLSVALS  DS    0X                                                               
GLSUNLA  DS    0CL14               G/L U/L/A                                    
GLSUNLD  DS    0CL2                    G/L U/L                                  
GLSUNT   DS    CL1                         G/L UNIT                             
GLSLDG   DS    CL1                         G/L LEDGER                           
GLSACT   DS    CL12                            G/L ACT                          
GLSANME  DS    CL36                G/L Acct Name                                
GLSACTE  DS    CL12                G/L Account Equivalency                      
GLSOFF   DS    CL2                 G/L Office                                   
GLSSACT  DS    CL14                G/L Subsidiary Account                       
GLSSANM  DS    CL36                G/L Subsidiary Acct Name                     
GLSSOFF  DS    CL2                 G/L Subsidiary Office                        
GLSSCUL  DS    CL2                 G/L Subsidiary Contra U/L                    
GLSSCAC  DS    CL12                G/L Subsidiary Contra Acct                   
GLSMED   DS    XL2                 G/L Subsidiary Media                         
GLSMEDNM DS    CL12                G/L SUBSIDIARY MEDIA NAME                    
GLSSYSMD DS    CL2                 G/L System/Media                             
GLSCLI   DS    CL5                 G/L Subsuduary Client                        
GLSPRO   DS    CL5                 G/L Subsuduary Product                       
GLSENT   DS    0X                  MONTHLY AMOUNTS/REPLICATION                  
GLSAMTS  DS    PL8                 AMOUNT VALUE                                 
GLSREP   DS    XL1                 REPLICATION FACTOR                           
GLSENTL  EQU   *-GLSENT                                                         
         DS    (MOATABM2-1)XL(GLSENTL)                                          
GLSVALL  EQU   *-GLSVALS                                                        
*                                                                               
TOTVALS  DS    0X                                                               
TOTRECS  DS    PL8                                                              
TOTENT   DS    0X                  MONTHLY AMOUNTS/REPLICATION                  
TOTAMTS  DS    PL8                 AMOUNT VALUE                                 
TOTREP   DS    XL1                 REPLICATION FACTOR                           
TOTENTL  EQU   *-TOTENT                                                         
         DS    (MOATABM2-1)XL(TOTENTL)                                          
TOTVALL  EQU   *-TOTVALS                                                        
*                                                                               
         ORG   OUTVALS                                                          
*                                                                               
* ** G/L Transaction Download Values **                                         
*                                                                               
GLTVALS  DS    0X                                                               
OGLTSULA DS    0CL14               G/L Subsidary U/L/A                          
OGLTSULD DS    0CL2                    G/L Subsidary U/L                        
OGLTSUNT DS    CL1                         G/L Subsidary Unit                   
OGLTSLDG DS    CL1                         G/L Subsidary Ledger                 
OGLTSACT DS    CL12                            G/L Subsidary Acct               
OGLTCULA DS    0CL14               G/L Subsidary Contra  U/L/A                  
OGLTCULD DS    0CL2                    G/L Subsidary Contra U/L                 
OGLTCUNT DS    CL1                         G/L Subsidary Contra Unit            
OGLTCLDG DS    CL1                         G/L Subsidary Contra Ledger          
OGLTCACT DS    CL12                            G/L Subsidary Contra Acc         
OGLTGULA DS    0CL14               G/L U/L/A                                    
OGLTGULD DS    0CL2                    G/L U/L                                  
OGLTGUNT DS    CL1                         G/L Unit                             
OGLTGLDG DS    CL1                         G/L Ledger                           
OGLTGACT DS    CL12                            G/L Acct                         
OGLTGDTE DS    PL3                 Date Uploaded to G/L                         
OGLTOFF  DS    CL2                 Transaction Office                           
OGLTAMNT DS    PL8                 Transaction Amount                           
OGLTREF  DS    CL6                 Transaction Reference                        
OGLTCLI  DS    CL3                 Client Code (Media Payables Only)            
OGLTSMD  DS    CL2                 Syetem/Media (SR Only)                       
OGLTDATE DS    PL3                 Transaction Date                             
OGLTANME DS    CL36                Account Name                                 
OGLTCNME DS    CL36                Contra-Account Name                          
OGLTPRO  DS    CL3                 Product                                      
OGLTPOFF DS    CL2                 Person Office                                
OGLTPDPT DS    CL3                 Person Department                            
OGLTPPER DS    CL7                 Person Code                                  
GLTVALL  EQU   *-GLTVALS                                                        
         ORG                                                                    
*                                                                               
OUTVALL  EQU   *-OUTVALS                                                        
*                                                                               
*** REGULAR STORAGE ***                                                         
*                                                                               
PL1CRNGE DS    CL(L'ACTRANGE)                                                   
         ORG   PL1CRNGE                                                         
GLSARNGE DS    CL(L'ACTRANGE)      G/L Summary Account Range                    
         ORG   PL1CRNGE                                                         
GLTARNGE DS    CL(L'ACTRANGE)      G/L Transaction Account Range                
GLTCRNGE DS    CL(L'ACTRANGE)      G/L Transaction Contra-Account Range         
         ORG   PL1CRNGE                                                         
PERRANGE DS    CL(L'ACTRANGE)      1R ACCOUNT RANGE                             
CSTRANGE DS    CL(L'ACTRANGE)      1C ACCOUNT RANGE                             
         ORG   PL1CRNGE                                                         
CACRANGE DS    CL(L'ACTRANGE)                                                   
CNTRANGE DS    CL(L'ACTRANGE)                                                   
*                                                                               
CSTACT   DS    CL(L'ACTKACT)       COSTING ACCOUNT                              
PERACT   DS    CL(L'ACTKACT)       PERSON ACCOUNT                               
ANAL14   DS    CL(L'PLDKANAL)      ANALYSIS CODE                                
*                                                                               
GLSLDRNG DS    CL(L'LDGRANGE)      G/L Summary Record Ledger Range              
*                                                                               
UNTLDG   DS    CL(L'ACTKUNT+L'ACTKLDG)  UNIT/LEDGER                             
RECLEV   DS    CL1                      ACCOUNT LEVEL                           
*                                                                               
COMPANY  DS    XL(L'LP_AGYB)       COMPANY CODE                                 
*                                                                               
CPYINDS  DS    X                   ** COMPANY INDICATORS **                     
CPYIOFF1 EQU   X'80'               ONE CHARACTER OFFICES                        
CPYIOFF2 EQU   X'40'               TWO CHARACTER OFFICES                        
*                                                                               
LIMACC   DS    CL2                 LIMITED ACCESS                               
*                                                                               
SVCPYST1 DS    XL1                 SAVED AREA FOR COMPANY STATUS BYTE 1         
SVCPYST2 DS    XL1                                COMPANY STATUS BYTE 2         
SVCPYST3 DS    XL1                                COMPANY STATUS BYTE 3         
SVCPYST4 DS    XL1                                COMPANY STATUS BYTE 4         
SVCPYLQ1 EQU   *-SVCPYST1                                                       
SVCPYST5 DS    XL1                 SAVED AREA FOR COMPANY STATUS BYTE 5         
SVCPYST6 DS    XL1                                COMPANY STATUS BYTE 6         
SVCPYST7 DS    XL1                                COMPANY STATUS BYTE 7         
SVCPYST8 DS    XL1                                COMPANY STATUS BYTE 8         
SVCPYLQ2 EQU   *-SVCPYST5                                                       
SVCPYLNQ EQU   *-SVCPYST1                                                       
*                                                                               
SVCDPTLN DS    XL1                 SAVED AREA FOR COMPANY LEVEL DEPT LN         
*                                                                               
LASTS    DS    0X                  ** LAST TIME VALUES **                       
LASTULA  DS    0CL(L'ACTKUNT+L'ACTKLDG+L'ACTKACT)                               
LASTUL   DS    0CL(L'ACTKUNT+L'ACTKLDG)                                         
LASTUNT  DS    CL(L'ACTKUNT)       LAST UNIT PASSED DOWN                        
LASTLDG  DS    CL(L'ACTKLDG)       LAST LEDGER PASSED DOWN                      
LASTACT  DS    CL(L'ACTKACT)       LAST ACCOUNT READ                            
LASTKEY  DS    XL(L'IOKEY)         LAST RECORD KEY                              
*                                                                               
LASTFLAG DS    X                   ** FLAG BYTE **                              
LASTPROC EQU   X'80'               PROCESS RECORD IN IOKEY                      
LASTLAST EQU   X'40'               LAST TIME                                    
LASTAMT  DS    PL(L'CLIAMTS)       LAST MONTHLY BUCKET                          
*                                                                               
LASTRTN  DS    0C                  START OF SPECIFIC ROUTINE STORAGE            
*                                                                               
* LAST FIELDS FOR NXTFLT ROUTINE                                                
*                                                                               
LSTFLTS  DS    0C                  LAST AREA FOR NXTFLT RTN                     
LASTFLN  DS    CL(L'FLTNUMB)       LAST FILTER NUMBER                           
LASTFDSC DS    CL(L'FLTDSC)        LAST FILTER DESCRIPTION                      
LASTFCDE DS    CL(L'FLTCODE)       LAST FILTER CODE                             
LASTFNM  DS    CL(L'FLTNAME)       LAST FILTER NAME                             
LSTFLTLN EQU   *-LSTFLTS                                                        
*                                                                               
         ORG   LASTRTN                                                          
*                                                                               
* LAST FIELDS FOR NXTACT ROUTINE                                                
*                                                                               
LSTACTS  DS    0C                  LAST AREA FOR NXTACT RTN                     
LASTLEV  DS    XL2                 LAST LEVEL NUMBER                            
LASTFLTS DS    0CL5                LAST FILTER VALUES                           
LASTFLT1 DS    CL(L'RSTFILT1)      LAST FILTER 1                                
LASTFLT2 DS    CL(L'RSTFILT1)      LAST FILTER 2                                
LASTFLT3 DS    CL(L'RSTFILT1)      LAST FILTER 3                                
LASTFLT4 DS    CL(L'RSTFILT1)      LAST FILTER 4                                
LASTFLT5 DS    CL(L'RSTFILT1)      LAST FILTER 5                                
LASTLV1C DS    CL(L'ACTLV1CD)      LAST ACCOUNT LEVEL 1 CODE                    
LASTLV2C DS    CL(L'ACTLV2CD)      LAST ACCOUNT LEVEL 2 CODE                    
LASTLV3C DS    CL(L'ACTLV3CD)      LAST ACCOUNT LEVEL 3 CODE                    
LASTLV4C DS    CL(L'ACTLV4CD)      LAST ACCOUNT LEVEL 4 CODE                    
LASTOFFV DS    CL(L'ACTOFFVL)      LAST OFFICE VALUE                            
*                                                                               
*        ALWAYS KEEP DATE/TIME TOGETHER                                         
*                                                                               
LASTDT   DS    0C                                                               
LASTDTE  DS    XL(L'ACTDATE)       LAST DATE FIELD FOR ACC DWNL                 
LASTTIM  DS    XL(L'ACTTIME)       LAST TIME FIELD FOR ACC DWNL                 
LASTDTLN EQU   *-LASTDT                                                         
*                                                                               
         ORG   LASTRTN                                                          
*                                                                               
* LAST FIELDS FOR NXTCNT ROUTINE                                                
*                                                                               
LSTCNT   DS    0X                  LAST TIME VALUES FOR CONTRA BUCKETS          
LSTCN1C  DS    0CL(L'CACKACT)      LAST   RECORD 1C                             
LSTCN1CA DS    CL(L'CACKACT)       LAST   RECORD 1C LEV A                       
LSTCN1CB DS    CL(L'CACKACT)                        LEV B                       
LSTCN1CC DS    CL(L'CACKACT)                        LEV C                       
LSTCN1CD DS    CL(L'CACKACT)                        LEV D                       
LSTCN1CL EQU   *-LSTCN1C                                                        
LSTCNLDG DS    CL(L'CACKCLDG)      LAST CONTRA ACCOUNT LEDGER                   
LSTCN14  DS    0CL(L'CACKACT)      LAST   RECORD 1C                             
LSTCN14A DS    CL(L'CACKACT)       LAST   RECORD 1C LEV A                       
LSTCN14B DS    CL(L'CACKACT)                        LEV B                       
LSTCN14C DS    CL(L'CACKACT)                        LEV C                       
LSTCN14D DS    CL(L'CACKACT)                        LEV D                       
LSTCN14L EQU   *-LSTCNLDG                                                       
LSTCNMTH DS    CL(L'CACKBTYP)      LAST METHOD CODE                             
*                                                                               
         ORG   LASTRTN                                                          
*                                                                               
* LAST FIELDS FOR NXTPLA ROUTINE                                                
*                                                                               
LSTPLAS  DS    0C                  LAST AREA FOR NXTPLA RTN                     
LASTMTH  DS    CL(L'PLCKMTHD)      LAST METHOD CODE                             
LASTCACT DS    CL(L'PLCKCACT)      LAST CONTRA ACCOUNT READ                     
LASTOFF  DS    CL(L'PLCKAGYO)      OFFICE CODE                                  
LASTLV1  DS    CL(L'LEVACDE)                                                    
LASTLV2  DS    CL(L'LEVBCDE)                                                    
LASTLV3  DS    CL(L'LEVCCDE)                                                    
LASTLV4  DS    CL(L'LEVDCDE)                                                    
LASTCLV1 DS    CL(L'LEVACDE)                                                    
LASTCLV2 DS    CL(L'LEVBCDE)                                                    
LASTCLV3 DS    CL(L'LEVCCDE)                                                    
LASTCLV4 DS    CL(L'LEVDCDE)                                                    
LASTBUDN DS    CL(L'BUDKBUDN)                                                   
*                                                                               
         ORG   LASTRTN                                                          
*                                                                               
* LAST FIELDS FOR NXTFRM ROUTINE                                                
*                                                                               
LSTFRMS  DS    0X                  ** LAST TIME VALUES FOR FORMAT **            
LSTFNME  DS    CL(L'FRMNAME)       LAST SCRIBE FORMAT NAME                      
LSTFEXT  DS    CL(L'FRMEXT)        LAST SCRIBE FORMAT EXTENTION                 
LSTFRML  EQU   *-LSTFRMS                                                        
*                                                                               
         ORG   LASTRTN                                                          
*                                                                               
* LAST FIELDS FOR NXTGLS ROUTINE                                                
*                                                                               
LSTGLS   DS    0X                  * LAST TIME VALUES FOR G/L SUMMARY *         
LSTGLULA DS    CL(L'GLSUNLA)       LAST G/L ACCOUNT CODE                        
LSTACTEQ DS    CL(L'GLSACTE)       LAST G/L ACCOUNT EQUIVALENT                  
LSTGLSL  EQU   *-LSTGLS                                                         
*                                                                               
         ORG   LASTRTN                                                          
*                                                                               
* LAST FIELDS FOR NXTPLD/NXTPER ROUTINE                                         
*                                                                               
LSTPRS   DS    0X                  ** LAST TIME VALUES FOR PERSONNEL**          
LST1C    DS    CL(L'PER1CACT)      LAST RECORD 1C                               
LST1R    DS    CL(L'PER1CACT)      LAST RECORD 1R                               
LSTANAL  DS    CL(L'PLDKANAL)      LAST RECORD ANALYSIS CODE                    
LSTPRMTH DS    CL(L'PLDKMTHD)      LAST METHOD CODE                             
LSTPR1R  DS    0CL(L'PER1CACT)     LAST   RECORD 1R                             
LSTPR1RA DS    CL(L'PER1CACT)      LAST   RECORD 1C LEV A                       
LSTPR1RB DS    CL(L'PER1CACT)                       LEV B                       
LSTPR1RC DS    CL(L'PER1CACT)                       LEV C                       
LSTPR1RD DS    CL(L'PER1CACT)                       LEV D                       
LSTPR1RL EQU   *-LSTPR1R                                                        
LSTPRAN  DS    CL(L'PLDKANAL)      LAST ANALYSIS CODE                           
LSTPR1C  DS    0CL(L'PER1CACT)     LAST   RECORD 1C                             
LSTPR1CA DS    CL(L'PER1CACT)      LAST   RECORD 1C LEV A                       
LSTPR1CB DS    CL(L'PER1CACT)                       LEV B                       
LSTPR1CC DS    CL(L'PER1CACT)                       LEV C                       
LSTPR1CD DS    CL(L'PER1CACT)                       LEV D                       
LSTPR1CL EQU   *-LSTPR1C                                                        
*                                                                               
LASTL    EQU   *-LASTS                                                          
*                                                                               
FLAG     DS    XL1                                                              
FLGREC   EQU   X'80'               MARK TO SHOW A RECORD HAS BEEN FOUND         
FLGSCR41 EQU   X'40'               SHOW THAT THERE IS A SEQ RECORD LEFT         
FLG1R    EQU   X'20'               OVERRIDE 16 TO 1R IN USE                     
FLGMPAY  EQU   X'20'               Current U/L is a Media Pay (NXTGLT)          
FLG9     EQU   X'10'               ACCOUNT IS 9 ONLY FOR 16                     
FLGEOF   EQU   X'08'               END OF TABLE                                 
FLGDTTM  EQU   X'04'               DATE/TIME HAS BEEN SENT ALREADY              
FLGFLT   EQU   X'02'               FILTERS HAVE BEEN SENT ALREADY               
FLGNOUL  EQU   X'01'               NO U/L DEFINED                               
*                                                                               
LEVS     DS    0CL(L'ACTKACT+L'NAMEREC)                                         
LEVACDE  DS    CL(L'ACTKACT)       LEVEL A CODE                                 
LEVANME  DS    CL(L'NAMEREC)       LEVEL A NAME                                 
LEVBCDE  DS    CL(L'ACTKACT)       LEVEL B CODE                                 
LEVBNME  DS    CL(L'NAMEREC)       LEVEL B NAME                                 
LEVCCDE  DS    CL(L'ACTKACT)       LEVEL C CODE                                 
LEVCNME  DS    CL(L'NAMEREC)       LEVEL C NAME                                 
LEVDCDE  DS    CL(L'ACTKACT)       LEVEL D CODE                                 
LEVDNME  DS    CL(L'NAMEREC)       LEVEL D NAME                                 
LVCDLNQ  EQU   *-LEVS                                                           
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
LEVLQ    EQU   *-LEVLNQS                                                        
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS FOR LEDGER                  
LEVEL    DS    XL1                 ACCOUNT LEVEL                                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
ACCLEVA  DS    CL(L'ACTKACT)       LEVEL A CODE                                 
ACCLEVB  DS    CL(L'ACTKACT)       LEVEL B CODE                                 
ACCLEVC  DS    CL(L'ACTKACT)       LEVEL C CODE                                 
ACCLEVD  DS    CL(L'ACTKACT)       LEVEL D CODE                                 
*                                                                               
CURRENT  DS    0C                  Current entry                                
CURACDE  DS    CL(L'ACTKACT)       Current Account Code                         
CURANME  DS    CL(L'NAMEREC)       Current Account Name                         
CURALNQ  EQU   *-CURRENT           Current Account Entry Length                 
*                                                                               
SVADDR   DS    A                   SAVED AREA FOR LAST ADDRESS                  
SVADDR2  DS    A                   SAVED AREA FOR CURR TABLE ENTRY              
SVADDR3  DS    A                   ANOTHER AREA FOR TABLE ENTRY                 
HALF     DS    H                   HALF WORD OF STORAGE                         
SVHALF   DS    H                   2ND HALF WORD OF STORAGE                     
BYTE     DS    XL1                 BYTE OF STORAGE                              
SVACCNUM DS    XL1                 SAVE AREA FOR NUMBER OF ACCOUNTS             
*                                                                               
SVIOKEY  DS    XL(L'IOKEY)         SAVED KEY FOR NESTED ROUTINES                
SVDTE    DS    XL(L'ACTDATE)       SAVED ARE FOR ACTIVITY DATE                  
SVTIM    DS    XL(L'ACTTIME)       SAVED AREA FOR ACTIVITY TIME                 
SVULA    DS    0CL(L'ACTKUNT+L'ACTKLDG+L'ACTKACT)                               
SVUL     DS    CL(L'ACTKUNT+L'ACTKLDG)                                          
SVACT    DS    XL(L'ACTKACT)                                                    
*                                                                               
CSTLDBLK DS    CL(LDGTABL2)        COSTING LEDGER BLOCK VALUES (1C)             
CNTLDBLK DS    CL(LDGTABL2)        CONTRA  LEDGER BLOCK VALUES (1-6)            
PERLDBLK DS    CL(LDGTABL2)        PERSON  LEDGER BLOCK VALUES (1R)             
         ORG   PERLDBLK                                                         
GLSLDBLK DS    CL(LDGTABL2)        G/L SUMMARY RECORDS VALUES (G*)              
*                                                                               
CSTLEVS  DS    XL4                 COSTING LEDGER LEVEL LENGTHS                 
CNTRLEVS DS    XL4                 CONTRA LEDGER LEVEL LENGTHS                  
*                                                                               
MTHLSTN  DS    AL2                 N'ENTRIES IN METHOD LIST                     
MTHLST   DS    XL10                REQUESTED METHODS                            
*                                                                               
BUD#     DS    XL2                 CURRENT BUDGET NUMBER                        
*                                                                               
PAYTYPE  DS    XL1                 PAY TYPE 0=SAL, 1=PEN, 2=BEN                 
CURUL    DS    0CL2                CURRENT UNIT/LEDGER                          
CURUNT   DS    CL1                 UNIT                                         
CURLDG   DS    CL1                 LEDGER                                       
*                                                                               
PKAMT    DS    PL(L'PLCKAMT)                                                    
*                                                                               
FLTNUM   DS    H                   NUMBER OF ENTRIES IN FLTENT                  
*                                                                               
OFFLST   DS    0C                                                               
OFFNUM   DS    XL2                                                              
OFFTAB   DS    CL240               LIST OF VALID OFFICES                        
OFFTABND DS    XL1                 EOT                                          
*                                                                               
CLINUM   DS    H                   NUMBER OF ENTRIES IN CLIAMTS                 
CLIAMTS  DS    (MOATABM*4)PL8      MONTHLY AMOUNTS                              
CLIAMLNQ EQU   (*-CLIAMTS)/L'CLIAMTS                                            
*                                                                               
         ORG   CLIAMTS+(MOATABM2*L'CLIAMTS)                                     
*                                                                               
* NXTGLT Storage                                                                
*                                                                               
GLABLK   DS    XL(TSPNEWL)         G/L Account Block                            
GLAREC   DS    0X                  G/L Account Record                           
GLAULA   DS    0CL14               G/L U/L/A                                    
GLAUL    DS    0CL2                   G/L U/L                                   
GLAUNT   DS    CL1                       G/L Unit                               
GLALDG   DS    CL1                       G/L Ledger                             
GLAACT   DS    CL12                      G/L Account Code                       
GLAKEYL  EQU   *-GLAREC                                                         
GLANME   DS    CL36                G/L Account Name                             
GLAACTE  DS    CL12                G/L Account Equivalency                      
GLARECL  EQU   *-GLAREC                                                         
*                                                                               
TSARKSAV DS    XL34                TSAR RECORD KEY SAVE AREA                    
*                                                                               
BUFFREC  DS    A                   BUFFER S/R A(RECORD)                         
BUFFRET  DS    X                   BUFFER S/R RETURN VALUES                     
*                                                                               
SVOFF    DS    CL2                 Office Code                                  
SVDEPT   DS    CL3                 Department Code                              
SVPER    DS    CL7                 Person Code                                  
SVGLDTE  DS    XL(L'GLDDATE)       Saved area for Date up to the G/L            
SVCLI    DS    CL5                 Saved area for client code                   
SVPRO    DS    CL5                 Saved area for product code                  
*                                                                               
CURCNME  DS    CL(L'NAMEREC)       Current Contra-Account Name                  
*                                                                               
GLTULRNG DS    CL(L'ULRANGE)       G/L Transaction U/L Range                    
GLTCULRG DS    CL(L'ULRANGE)       G/L Transaction Contra U/L Range             
GLTOFFRG DS    CL(L'OFFRANGE)      G/L Transaction Off Range                    
*                                                                               
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE ACLNKWRKD                                                      
       ++INCLUDE ACMSGEQUS                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACLNK13   03/18/11'                                      
         END                                                                    
