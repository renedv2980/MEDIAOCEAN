*          DATA SET SRJOB00    AT LEVEL 004 AS OF 05/12/20                      
*PHASE T15700A                                                                  
         TITLE 'SRJOB00 - DISPLAY/CHANGE JOB QUEUES'                            
JOB      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 JOBWRKX-JOBWRKD,**$JOB**,RA,CLEAR=YES,RR=RE                      
         USING JOBWRKD,RC          RC=A(W/S)                                    
         ST    RE,RELO                                                          
         MVC   SRPARAS(SRPARAL),0(R1)                                           
         L     R9,SRPASYS                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,SRPATWA                                                       
         USING JOBTWAD,R8          R8=A(TWA)                                    
         NI    JOBACTH+6,X'FF'-X'40'                                            
*                                                                               
         L     R1,VSSB                                                          
         USING SSBD,R1                                                          
         MVC   RECLEN,SSBTWAL                                                   
         MVC   TBLET,SSBTBLET                                                   
*                                                                               
         L     R1,SRPAUTL                                                       
         USING UTLD,R1                                                          
         ST    R1,AUTL             SAVE A(UTL ENTRY)                            
         MVC   TERMSTAT,TSTAT                                                   
         MVC   TERMNUM,TNUM                                                     
         MVC   TRANSNUM,TTRCNT                                                  
         MVC   TERMPID(2),TAGYPER                                               
         MVC   TERMPID+2(2),TPERSON                                             
*                                                                               
         L     R1,SRPACOM                                                       
         USING COMFACSD,R1                                                      
         MVC   VSCANNER,CSCANNER                                                
         MVC   VTERMVAL,CTERMVAL                                                
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VGETHELP,CGETHELP                                                
         MVC   VDICTATE,CDICTATE                                                
         MVC   VGETFACT,CGETFACT                                                
         DROP  R1                                                               
*                                                                               
         L     R1,=A(ACTTAB)       RELOCATE TABLE ADDRESSES                     
         A     R1,RELO                                                          
         ST    R1,AACTTAB                                                       
         L     R1,=A(OPTTAB)                                                    
         A     R1,RELO                                                          
         ST    R1,AOPTTAB                                                       
         L     R1,=A(SUBTAB)                                                    
         A     R1,RELO                                                          
         ST    R1,ASUBTAB                                                       
*                                                                               
JOBX     GOTO1 VDICTATE,DMCB,C'LU  ',DDDCLST,DDDSLST                            
         MVC   HELPKEY,HELPID                                                   
         MVI   JOBLOCK,C'N'                                                     
         MVI   CLSLOCK,C'N'                                                     
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION                                                     *         
***********************************************************************         
VALACT   L     R1,AACTTAB                                                       
         USING ACTTABD,R1                                                       
         CLI   JOBACTH+5,0                                                      
         BNE   *+10                                                             
         MVC   JOBACT(8),SR@DSP    DEFAULT TO DISPLAY                           
         MVI   HELP,1              SET HELP NUMBER                              
         GOTO1 FVAL,JOBACTH                                                     
         BE    EMIF                                                             
         CLI   FLDH+5,2            INPUT MUST BE AT LEAST 2 CHARACTERS          
         BL    EFTS                                                             
         LLC   RE,FLDH+5                                                        
         BCTR  RE,0                RE=L'INPUT-1                                 
VALACT2  L     R1,AACTTAB                                                       
VALACT4  CLI   ACTNAME,0           TEST E-O-T                                   
         BNE   VALACT6                                                          
         LA    R0,1                YES-TEST SECOND PASS DONE                    
         CR    RE,R0                                                            
         BNH   EIAC                                                             
         LR    RE,R0                                                            
         B     VALACT2                                                          
*                                                                               
VALACT6  EX    0,ACTNAME           RF=A(ACTNAME)                                
         EX    RE,*+8              MATCH INPUT TO TABLE                         
         B     *+10                                                             
         CLC   FLD(0),0(RF)                                                     
         BE    *+12                                                             
         LA    R1,ACTTABL(R1)                                                   
         B     VALACT4                                                          
         TM    ACTINDS,ACTIDDS                                                  
         BZ    *+12                                                             
         TM    TERMSTAT,TSTATDDS                                                
         BZ    EIAC                                                             
         MVC   JOBACT(8),0(RF)     DISPLAY FULL ACTION NAME                     
         MVC   ACTION,ACTTYPE                                                   
         MVC   JOBN,ACTJOBI        SAVE ACTION TABLE VALUES                     
         MVC   OPTR,ACTOPTR                                                     
         MVC   OPTX,ACTOPTX                                                     
VALACTX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE JOB KEY                                                    *         
***********************************************************************         
VALJOB   CLI   JOBJOBH+5,0         TEST ANY INPUT IN JOB FIELD                  
         BNE   *+10                                                             
         MVC   JOBJOB(4),SR4ALL    NO-PRESET TO 'ALL'                           
         MVI   HELP,2                                                           
         GOTO1 FVAL,JOBJOBH                                                     
         JE    *+2                                                              
         GOTO1 VSCANNER,DMCB,FLDH,(5,SCANBLK),C',=,='                           
         CLI   4(R1),0             TEST 1 THRU 4 FIELDS INPUT                   
         BE    EIJB                                                             
         CLI   4(R1),4                                                          
         BH    EIJB                                                             
         MVC   SCANLIN,4(R1)       SET UP FOR DATA VALIDATION                   
         LA    R2,SCANBLK-L'SCANBLK                                             
         XC    JOBKEY(JOBKEYL),JOBKEY                                           
*                                                                               
VALJOB2  LLC   R1,FNDX             BUMP TO NEXT SUB-FIELD                       
         LA    R1,1(R1)                                                         
         CLM   R1,1,SCANLIN        TEST ALL DATA VALIDATED                      
         BH    VALJOB30                                                         
         STC   R1,FNDX                                                          
         LA    R2,L'SCANBLK(R2)                                                 
         CLI   JOBN,0              TEST ANY MORE DATA EXPECTED                  
         BE    EIIF                NO-ERROR                                     
*                                                                               
         TM    2(R2),X'80'         TEST INPUT IS NUMERIC                        
         BZ    VALJOB12                                                         
         CLI   1(R2),0             TEST SECOND HALF PRESENT                     
         BNE   EIIF                                                             
         TM    JOBN,JOBNNUM        TEST NUMERIC VALUE VALID                     
         BZ    EIIF                                                             
         OC    4(2,R2),4(R2)       PRE-VALIDATE INPUT VALUE                     
         BNZ   EFVB                                                             
         OC    4(4,R2),4(R2)       TEST ZERO NUMERIC VALUE                      
         BZ    EFVS                                                             
*                                  START & END REPORT SEQUENCE NUMBERS          
VALJOB4  TM    JOBN,JOBNSTRR+JOBNENDR                                           
         BZ    VALJOB8                                                          
         CLC   6(2,R2),=H'9999'    TEST GR MAX ALLOWED                          
         BH    EFVB                                                             
         TM    JOBN,JOBNSTRR                                                    
         BZ    VALJOB6                                                          
         MVC   JOBKSTRR,6(R2)      SET START REPORT SEQ NUM                     
         MVI   JOBN,JOBNENDR       SET END REPORT SEQ NUM EXPECTED              
         B     VALJOB2                                                          
VALJOB6  MVC   JOBKENDR,6(R2)      SET END REPORT SEQ NUM                       
         CLC   JOBKENDR,JOBKSTRR   TEST END GR START                            
         BL    ESGE                                                             
         MVI   JOBN,0              SET NO MORE DATA EXPECTED                    
         B     VALJOB2                                                          
*                                  START & END JOB SEQUENCE NUMBERS             
VALJOB8  TM    JOBN,JOBNSTRD+JOBNENDD                                           
         BZ    EIIF                                                             
         OC    4(3,R2),4(R2)       TEST VALUE GR 255                            
         BNZ   EFVB                                                             
         TM    JOBN,JOBNSTRD                                                    
         BZ    VALJOB10                                                         
         MVC   JOBKSTRD,7(R2)      SET START JOB SEQ NUM                        
         MVI   JOBN,JOBNENDD       SET END JOB SEQ NUM EXPECTED                 
         B     VALJOB2                                                          
VALJOB10 MVC   JOBKENDD,7(R2)      SET END JOB SEQ NUM                          
         CLC   JOBKENDD,JOBKSTRD   TEST END GR START                            
         BL    ESGE                                                             
         MVI   JOBN,0              SET NO MORE DATA EXPECTED                    
         B     VALJOB2                                                          
*                                  'ALL' USER-ID OR REPORT SUB-ID               
VALJOB12 CLC   12(4,R2),SR4ALL     TEST INPUT IS 'ALL'                          
         BNE   VALJOB14                                                         
         CLI   1(R2),0             TEST SECOND HALF INPUT                       
         BNE   EIIF                                                             
         TM    JOBN,JOBNALL                                                     
         BZ    EIIF                                                             
         TM    JOBN,JOBNALLU       TEST 'ALL' USER-ID                           
         BZ    *+12                                                             
         MVI   JOBN,JOBNSTRD       SET START JOB SEQ NUM EXPECTED               
         B     VALJOB2                                                          
         TM    JOBN,JOBNALLR       TEST 'ALL' REPORT SUB-ID                     
         BZ    EIIF                                                             
         MVI   JOBN,JOBNSTRR       SET START REPORT SEQ NUM EXPECTED            
         B     VALJOB2                                                          
*                                  REPORT SUB-ID OR USER-ID                     
VALJOB14 TM    JOBN,JOBNUSER+JOBNSUBR                                           
         BZ    EIIF                                                             
         TM    JOBN,JOBNSUBR       TEST REPORT SUB-ID VALID                     
         BZ    VALJOB18                                                         
         CLI   0(R2),3             TEST INPUT LENGTH OF 3                       
         BL    EFTS                                                             
         BH    VALJOB18                                                         
         TM    JOBN,JOBNUSER       TEST USER-ID VALID ALSO                      
         BZ    VALJOB16            YES-IT MIGHT BE A USER-ID                    
         LA    R3,KEY                                                           
         USING CTIREC,R3           BUILD KEY OF USER-ID RECORD                  
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,12(R2)                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTIKEY,IO                            
         CLI   8(R1),0                                                          
         BE    VALJOB20            ASSUME USER-ID IF FOUND                      
VALJOB16 MVC   JOBKSUBR,12(R2)     SET REPORT SUB-ID                            
         MVI   JOBN,JOBNSTRR       SET REPORT START SEQ NUM EXPECTED            
         B     VALJOB2                                                          
*                                  USER-ID                                      
VALJOB18 TM    JOBN,JOBNUSER       TEST USER-ID EXPECTED                        
         BZ    EIIF                                                             
         CLI   1(R2),0             TEST LENGTH OF SECOND HALF                   
         BNE   EIIF                                                             
         CLI   0(R2),3             TEST LENGTH OF FIRST HALF                    
         BL    EFTS                                                             
         CLI   0(R2),7                                                          
         BH    EFTL                                                             
         LA    R3,KEY                                                           
         USING CTIREC,R3           BUILD KEY OF USER-ID RECORD                  
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,12(R2)                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTIKEY,IO                            
         CLI   8(R1),0             TEST FOR RECORD FOUND                        
         BNE   EIID                                                             
VALJOB20 SR    R0,R0               FIND ID NUMBER ELEMENT ON ID RECORD          
         LA    R1,IO+(CTIDATA-CTIREC)                                           
VALJOB22 CLI   0(R1),0             TEST E-O-R                                   
         JE    *+2                                                              
         CLI   0(R1),X'02'         TEST ID NUMBER ELEMENT                       
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VALJOB22                                                         
         MVC   JOBKUSER,2(R1)      SET USER-ID NUMBER                           
         MVI   JOBN,JOBNALLR+JOBNSUBR+JOBNSTRD                                  
         B     VALJOB2                                                          
*                                                                               
VALJOB30 DS    0H                                                               
VALJOBX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS                                                    *         
***********************************************************************         
VALOPT   MVI   HELP,3                                                           
         XC    OPTI,OPTI           CLEAR INPUT OPTIONS BITS                     
         GOTO1 FVAL,JOBOPTH                                                     
         BE    VALOPT22                                                         
*                                  SCAN THE INPUT OPTIONS FIELD                 
         GOTO1 VSCANNER,DMCB,FLDH,(6,SCANBLK),C',=,='                           
         MVC   SCANLIN,4(R1)       SAVE N'SCANNER LINES                         
         LA    R2,SCANBLK-L'SCANBLK                                             
*                                                                               
VALOPT4  LLC   R1,FNDX             INCREMENT FIELD INDEX BY ONE                 
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R2,L'SCANBLK(R2)                                                 
         CLC   FNDX,SCANLIN        COMPARE FIELD INDEX TO N'FIELDS              
         BH    VALOPT22                                                         
*                                                                               
         CLI   0(R2),0                                                          
         BE    EIIF                                                             
         CLI   0(R2),8                                                          
         BH    EIIF                                                             
         LLC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         L     R3,AOPTTAB                                                       
         USING OPTTABD,R3          R3=A(OPTIONS TABLE)                          
*                                                                               
VALOPT6  CLI   OPTNAME,0           TEST E-O-T                                   
         BE    EIKW                                                             
         EX    0,OPTSHRT           RF=A(KEYWORD)                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),12(R2)      MATCH ON OPTION SHORT NAME                   
         BE    VALOPT14                                                         
VALOPT10 CLC   0(1,R2),OPTMINKL                                                 
         BL    VALOPT12                                                         
         EX    0,OPTNAME           RF=A(KEYWORD)                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),12(R2)      MATCH ON FULL OPTION NAME                    
         BE    VALOPT14                                                         
*                                  BUMP TO NEXT OPTION TABLE ENTRY              
VALOPT12 LA    R3,OPTTABL(R3)                                                   
         B     VALOPT6                                                          
*                                                                               
VALOPT14 TM    OPTINDS,OPTIDDS     TEST IF A DDS ONLY OPTION                    
         BZ    *+12                                                             
         TM    TERMSTAT,TSTATDDS                                                
         BZ    EIKW                                                             
         MVC   DUB(2),OPTX         TEST IF THIS OPTION IS COMPATIBLE            
         NC    DUB(2),OPTOPTB                                                   
         BNZ   EKWI                                                             
         MVC   DUB(2),OPTI         TEST IF OPTION PREVIOUSLY INPUT              
         NC    DUB(2),OPTOPTB                                                   
         BNZ   EDKO                                                             
         OC    OPTI,OPTOPTB        NO-SET THIS OPTION INPUT                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,OPTIADDR                                                    
         LA    R1,JOB(R1)                                                       
         ST    R1,IADDR            SET A(TABLE OR VALIDATION ROUTINE)           
         SR    R1,R1                                                            
         ICM   R1,3,OPTOADDR                                                    
         LA    R1,JOBWRKD(R1)                                                   
         ST    R1,OADDR            SET A(OUTPUT FIELD)                          
*                                                                               
         IC    RF,1(R2)            RF=DATA LENGTH                               
         XC    FLDH,FLDH                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),22(R2)       MOVE IN THE DATA                             
         TM    3(R2),X'80'         TEST FIELD IS NUMERIC                        
         BZ    *+8                                                              
         OI    FLDH+4,X'08'        YES-SET INPUT INDICATOR                      
         LA    R0,8(RF)            SET TOTAL FIELD LENGTH                       
         STC   R0,FLDH                                                          
         STC   RF,FLDH+5                                                        
*                                  TEST DATA LENGTH                             
         CLC   FLDH+5(1),OPTMINDL                                               
         BL    EDTS                                                             
         CLC   FLDH+5(1),OPTMAXDL                                               
         BH    EDTL                                                             
*                                                                               
         L     RF,IADDR            RE=A(TABLE OR ROUTINE)                       
         MVI   FERN,FLDISOK                                                     
         TM    OPTINDS,OPTIRTN                                                  
         BZ    VALOPT16                                                         
*                                  GO TO FIELD VALIDATION ROUTINE               
         BASR  RE,RF                                                            
         CLI   FERN,FLDISOK        TEST FOR ERRORS                              
         BE    VALOPT20                                                         
         B     EXIT                YES-ERROR & CURSOR SET SO EXIT               
*                                  PROCESS DATA TABLE                           
VALOPT16 TM    OPTINDS,OPTITAB                                                  
         JZ    *+2                                                              
         TM    OPTINDS,OPTIHLP     TEST TABLE HAS HELP HEADER                   
         BZ    *+8                                                              
         LA    RF,60(RF)           YES-BUMP OVER IT                             
         LLC   R0,0(RF)            R0=L'LHS OF TABLE                            
         LLC   R1,1(RF)            R1=L'RHS OF TABLE                            
         LA    RF,2(RF)            POINT TO FIRST TABLE ENTRY                   
         AR    R0,R1               R0=L'DATA TABLE                              
         LLC   RE,FLDH+5           RF=L'DATA                                    
         BCTR  RE,0                                                             
*                                                                               
VALOPT18 CLI   0(RF),0             TEST E-O-T                                   
         BE    EIDV                                                             
         EX    0,0(RF)             R6=A(KEYWORD)                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(R6)        MATCH INPUT WITH TABLE                       
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     VALOPT18                                                         
         AR    RF,R0                                                            
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
*                                  MOVE DATA TO OUTPUT AREA                     
VALOPT20 LLC   R1,OPTOUTDL                                                      
         BCTR  R1,0                                                             
         L     RE,OADDR                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK                                                     
         B     VALOPT4                                                          
*                                                                               
VALOPT22 MVC   DUB(2),OPTI         TEST ALL REQUIRED OPTIONS INPUT              
         NC    DUB(2),OPTR                                                      
         CLC   DUB(2),OPTR                                                      
         BE    VALOPT26                                                         
         L     R3,AOPTTAB          NO-LOCATE A MISSING OPTION                   
VALOPT24 CLI   OPTNAME,0           TEST E-O-T                                   
         JE    *+2                                                              
         MVC   DUB(2),OPTR                                                      
         NC    DUB(2),OPTOPTB                                                   
         BNZ   *+12                                                             
         LA    R3,OPTTABL(R3)                                                   
         B     VALOPT24                                                         
         MVC   XTRA(L'OPTNAME),OPTNAME                                          
         B     EROM                                                             
VALOPT26 DS    0H                                                               
VALOPTX  MVI   FNDX,0                                                           
         B     GO                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE TERMINAL NUMBER                                 *         
***********************************************************************         
VALTRM   NTR1                                                                   
         GOTO1 VTERMVAL,DMCB,FLDH                                               
         ICM   R1,15,4(R1)                                                      
         BZ    EITN                                                             
         USING UTLD,R1                                                          
         SAM31                                                                  
         OC    TPRNT,TPRNT                                                      
         BNZ   EITN                                                             
         ST    R1,AUTL                                                          
         MVC   WORK(L'TNUM),TNUM                                                
VALTRMX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE STATUS (WHEN GIVEN WITHOUT ST= )                *         
* R2 POINTS TO SCANNER TABLE ENTRY                                    *         
***********************************************************************         
VALSTA   NTR1                                                                   
         LA    RF,STATAB                                                        
         LLC   R0,0(RF)            R0=L'LHS OF TABLE                            
         LLC   R1,1(RF)            R1=L'RHS OF TABLE                            
         LA    RF,2(RF)            POINT TO FIRST TABLE ENTRY                   
         AR    R0,R1               R0=L'DATA TABLE                              
         LLC   RE,0(R2)            RE=L'DATA                                    
         BCTR  RE,0                                                             
*                                                                               
VALSTA10 CLI   0(RF),0             TEST E-O-T                                   
         JE    *+2                                                              
         EX    0,0(RF)             R6=A(KEYWORD)                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),0(R6)      MATCH INPUT WITH TABLE                       
         BE    VALSTA20                                                         
         AR    RF,R0                                                            
         B     VALSTA10                                                         
*                                                                               
VALSTA20 AR    RF,R0                                                            
         SR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)       MOVE DATA TO OUTPUT AREA                     
*                                                                               
VALSTAX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS INPUT ACTION                                                *         
***********************************************************************         
GO       L     R6,SRPATIA          R6=A(SPECIAL S/R SAVE PAGE)                  
         USING SRSD,R6                                                          
         LA    R0,SRPAGENO         READ & LOCK SAVE PAGE                        
         SLL   R0,32-8                                                          
         ICM   R0,3,TERMNUM                                                     
         ST    R0,TWAPAGE                                                       
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,TWAPAGE,SRSD                
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         LA    R5,SR$JOB           R5=A($JOB SAVE AREA)                         
         USING JOBSAVED,R5                                                      
         TIME  BIN                 R0=TIME,R1=DATE(JULIAN)                      
         ST    R1,DUB                                                           
         OC    SRSDATE,SRSDATE                                                  
         BZ    *+14                                                             
         CLC   SRSDATE,DUB         TEST LAST UPDATED TODAY                      
         BNE   GO0                                                              
         OC    SRSPID,SRSPID                                                    
         BZ    GO1                                                              
         CLC   SRSPID,TERMPID      TEST USED BY SAME PERSON                     
         BE    GO1                                                              
*                                                                               
GO0      LA    R1,JOBACTH          TWA IS NOT VALID                             
         ST    R1,FADR                                                          
         LR    R0,R6               CLEAR TWA AND WRITE IT BACK                  
         LHI   R1,SRSTWAL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,TWAPAGE,SRSD                         
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         B     ENTD                NOTHING TO DISPLAY                           
*                                                                               
GO1      MVI   TWAREAD,C'Y'                                                     
         LA    R1,JOBMSGH          SET XMIT BITS IN ALL TWA FIELDS              
         SR    RE,RE                                                            
         LA    RF,JOBXXXH-1                                                     
         OI    6(R1),X'80'                                                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,*-8                                                        
*                                                                               
         CLI   ACTION,ACTTCHA      TEST CHANGE                                  
         BNE   GO2                 NO                                           
         SR    RE,RE                                                            
         ICM   RE,3,JOBSTNUM       TEST DISPLAY LAST TIME                       
         LA    RE,1(RE)                                                         
         CLM   RE,3,TRANSNUM                                                    
         BNE   GO2                                                              
         MVC   SAVEJOBN,JOBSTABN   SAVE DISPLAYED C/I TABLE                     
         MVC   SAVEJOBT(SAVEJOBL),JOBSTAB                                       
         GOTO1 DIS,0               BUILD C/I & STATUS TABLE                     
         SR    RF,RF                                                            
         ICM   RF,3,JOBSTABN                                                    
         BZ    GO2                 NOTHING IN QUEUE TO CHANGE                   
         CLM   RF,3,SAVEJOBN                                                    
         BNE   GO2                                                              
         SLL   RF,1                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8              TEST DISPLAYED QUEUE HAS CHANGED             
         B     *+10                                                             
         CLC   SAVEJOBT(0),JOBSTAB                                              
         BNE   GO2                 NO-DISPLAY CURRENT QUEUE                     
         GOTO1 DIS,DISFADTA        REDISPLAY QUEUE                              
         B     CHA                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY                                                             *         
***********************************************************************         
GO2      GOTO1 DIS,DISFADTA+DISFASEQ                                            
         LA    R1,JOBACTH          SET FADR FOR ERROR/EXIT                      
         ST    R1,FADR                                                          
         CLI   SEQDIS,0            TEST ANYTHING DISPLAYED                      
         BE    ENTD                NO                                           
         XC    WORK(10),WORK       FORMAT MESSAGE IN FLD FOR EXIT               
         MVI   WORK+0,4                                                         
         MVI   WORK+4,4                                                         
         MVI   WORK+8,4                                                         
         LLC   R0,SEQLO                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+1(3),DUB                                                    
         LLC   R0,SEQHI                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+5(3),DUB                                                    
         LLC   R0,SEQNUM                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+9(3),DUB                                                    
         CLI   ACTION,ACTTCHA      TEST QUEUE DISPLAY FOR CHANGE                
         BE    *+12                                                             
         MVI   MSG,250             JOBS DISPLAYED ENTER NEXT REQ                
         B     GOX                                                              
         MVI   MSG,251             JOBS DISPLAYED NOW ENTER CHANGES             
         LA    R1,JOBLINH                                                       
         ST    R1,FADR                                                          
*                                                                               
GOX      SR    RE,RE               ZERO ERROR FOR INFO                          
         CLI   HELP,4                                                           
         BE    HELPER                                                           
         B     ERROR                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY JOB QUEUE                                                   *         
* NTRY:  R1=DISPLAY FLAGS (SEE EQUATED VALUES FOR DISFLAG)            *         
***********************************************************************         
DIS      NTR1                                                                   
         STC   R1,DISFLAG          SAVE FORMAT/DISPLAY FLAG                     
         LA    R1,JOBLINH                                                       
         USING JOBLINED,R1                                                      
         LA    RE,JOBLINEL                                                      
         LA    RF,JOBXXXH-1                                                     
*                                                                               
DIS2     TM    DISFLAG,DISFASEQ    CLEAR & PROTECT LINE ACTION FIELDS           
         BZ    *+10                                                             
         XC    JOBLACT,JOBLACT                                                  
         OI    JOBLACTH+1,X'20'                                                 
         BXLE  R1,RE,DIS2                                                       
         DROP  R1                                                               
*                                                                               
         XC    JOBSTABN,JOBSTABN   CLEAR SAVE DISPLAY TABLE                     
         XC    JOBSTAB(JOBSTABL),JOBSTAB                                        
         XC    SEQS,SEQS           CLEAR DISPLAY SEQUENCE NUMBERS               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ       R0=N'JOB QUEUE ENTRIES                       
         BZ    DISX                                                             
*                                                                               
         LA    R2,SRJOBQ                                                        
         USING SRJOBQ,R2           R2=A(JOB QUEUE)                              
DIS4     CLI   SRJOBSTA,SRJOBERR   TEST ERROR ENTRY                             
         BE    DIS14               YES                                          
         OC    JOBKUSER,JOBKUSER   TEST USER-ID FILTER                          
         BZ    *+14                                                             
         CLC   SRJOBUSR,JOBKUSER                                                
         BNE   DIS14                                                            
         OC    JOBKSUBR,JOBKSUBR   TEST REPORT-ID FILTER                        
         BZ    *+14                                                             
         CLC   SRJOBSUB,JOBKSUBR                                                
         BNE   DIS14                                                            
         OC    JOBKSTRR,JOBKSTRR   TEST REPORT SEQ LOW FILTER                   
         BZ    *+14                                                             
         CLC   SRJOBREP,JOBKSTRR                                                
         BL    DIS14                                                            
         OC    JOBKENDR,JOBKENDR   TEST REPORT SEQ HIGH FILTER                  
         BZ    *+14                                                             
         CLC   SRJOBREP,JOBKENDR                                                
         BH    DIS14                                                            
*                                                                               
         LA    RF,NDX              READ FIRST CI REC USING REPORT ID            
         USING UKRECD,RF                                                        
         XC    NDX,NDX                                                          
         MVC   UKKEY(7),SRJOBUSR                                                
         MVI   UKFLAG,UKFLNUM+UKFLCIA+UKFLCIR                                   
         GOTO1 VDATAMGR,DMCB,INDEX,PRTQUE,NDX,WORK,CIREC                        
         CLI   8(R1),0                                                          
         BNE   DIS10                                                            
         LA    RF,NDX              EXTRACT RETURN VALUES                        
         MVC   DSKADR,UKUSRINF                                                  
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),UKUSRINF+4                                           
         DROP  RF                                                               
*                                                                               
         LA    RE,CIREC            RE=A(REPORT BUFFER)                          
         USING PQRECD,RE                                                        
         MVI   JOBFLAG,0                                                        
         MVI   JOBFLAG2,0                                                       
*                                                                               
         CLC   PQKEY,SRJOBUSR      TEST REPORT HAS SAME KEY                     
         BNE   DIS10                                                            
*                                                                               
         CLI   PQSTAT,PQSTPU       TEST REPORT PURGED                           
         BNE   *+12                                                             
         MVI   SRJOBSTA,SRJOBERR   YES-FLAG THIS ENTRY IN ERROR                 
         B     DIS14                                                            
*                                                                               
         TM    PQSTAT,PQSTTE       ESTABLISH JOB STATUS                         
         BZ    DIS6                                                             
         TM    PQATTB,PQATJOBI+PQATJOBO                                         
         BNO   DIS6                                                             
         MVI   JOBFLAG,JOBFRUN     JOB IS RUNNING                               
         B     DIS12                                                            
*                                                                               
DIS6     TM    PQSTAT,PQSTIN       INVISIBLE REPORT                             
         BZ    *+12                                                             
         MVI   JOBFLAG,JOBFRUN     JOB IS RUNNING                               
         B     DIS12                                                            
         TM    PQSTAT,PQSTDEAD                                                  
         BNZ   DIS8                                                             
         TM    PQSTAT,PQSTAC                                                    
         BZ    DIS10                                                            
         TM    PQATTB,PQATJOBI                                                  
         BZ    DIS8                                                             
         MVI   JOBFLAG,JOBFHLD     JOB IS IN HOLD STATUS                        
         TM    SRJOBSTA,SRJOBHLD                                                
         BNZ   DIS12                                                            
         MVI   JOBFLAG,JOBFSCH     JOB IS SCHEDULED FOR SUBMISSION              
         TM    SRJOBSTA,SRJOBPUT                                                
         BZ    DIS12                                                            
         MVI   JOBFLAG,JOBFSUB     JOB HAS BEEN SUBMITTED                       
         TM    SRJOBSTA,SRJOBOUT+SRJOBINV                                       
         BNO   DIS12                                                            
         MVI   JOBFLAG,JOBFAVA     JOB IS READY (NOT NOTIFIED)                  
         TM    SRJOBSTA,SRJOBNOT                                                
         BZ    *+8                                                              
         MVI   JOBFLAG,JOBFNOT     JOB HAS BEEN NOTIFIED                        
         MVI   JOBFLAG2,JOBFERR                                                 
         B     DIS12                                                            
*                                                                               
DIS8     TM    PQATTB,PQATJOBO                                                  
         BZ    DIS10                                                            
         TM    PQSTAT,PQSTPR       SET JOBFLAG2 FOR SECONDARY STATUS            
         BZ    *+8                                                              
         MVI   JOBFLAG2,JOBFPRT                                                 
         TM    PQSTAT,PQSTSE                                                    
         BZ    *+8                                                              
         MVI   JOBFLAG2,JOBFSNT                                                 
         TM    PQATTB,PQATERR                                                   
         BZ    *+8                                                              
         MVI   JOBFLAG2,JOBFERR                                                 
         MVI   JOBFLAG,JOBFAVA     JOB IS READY (NOT NOTIFIED)                  
         TM    SRJOBSTA,SRJOBNOT                                                
         BZ    DIS11                                                            
         MVI   JOBFLAG,JOBFNOT     JOB HAS BEEN NOTIFIED                        
         B     DIS11                                                            
*                                                                               
DIS10    B     DIS14               JOB IN UNKNOWN STATUS - IGNORE               
*                                                                               
DIS11    TM    PQATTB,PQATPW       TEST IF REPORT HAS SECURITY                  
         BZ    DIS12                                                            
         TM    PQTYPE,PQTYNEW                                                   
         BZ    DIS11A                                                           
         TM    PQSECF1,PQSINONO    TEST IF SECURITY FLAGS ARE VALID             
         BO    DIS12                                                            
         TM    PQSECF1,PQSIPIN     TEST IF PIN PROTECTED                        
         BZ    DIS12                                                            
         OI    JOBFLAG2,JOBFPIN                                                 
         B     DIS12                                                            
DIS11A   TM    PQSECF1,PQSINONO    TEST IF SECURITY FLAGS ARE VALID             
         BO    DIS12                                                            
         TM    PQSECF1,PQSIPIN     TEST IF PIN PROTECTED                        
         BZ    DIS12                                                            
DIS11B   OI    JOBFLAG2,JOBFPIN                                                 
*                                                                               
         DROP  RE                                                               
*                                                                               
DIS12    MVC   DUB(1),JOBFILT      APPLY STATUS FILTER                          
         NC    DUB(1),JOBFLAG                                                   
         CLC   DUB(1),JOBFILT                                                   
         BNE   DIS14                                                            
         LLC   RF,SEQNUM           BUMP SEQUENCE NUMBER                         
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         OC    JOBKSTRD,JOBKSTRD   TEST DISPLAY SEQ LOW FILTER                  
         BZ    *+14                                                             
         CLC   SEQNUM,JOBKSTRD                                                  
         BL    DIS14                                                            
         OC    JOBKENDD,JOBKENDD   TEST DISPLAY SEQ HIGH FILTER                 
         BZ    *+14                                                             
         CLC   SEQNUM,JOBKENDD                                                  
         BH    DIS14                                                            
         BAS   RE,FORMAT           FORMAT DISPLAY LINE                          
         BNE   DIS14                                                            
         LH    RF,JOBSTABN         BUMP N'ENTRIES IN JOBSTAB                    
         LR    RE,RF                                                            
         LA    RF,1(RF)                                                         
         STH   RF,JOBSTABN                                                      
         SLL   RF,1                                                             
         LA    RF,JOBSTAB-2(RF)                                                 
         MVC   0(2,RF),SRJOBREP    SAVE REPORT NUM IN TABLE                     
         LA    RE,JOBSSTAT(RE)                                                  
         MVC   0(1,RE),JOBFLAG     SAVE REPORT STATUS FLAG IN TABLE             
         CLI   SEQLO,0             SET DISPLAY SEQUENCE NUMBERS                 
         BNE   *+10                                                             
         MVC   SEQLO,SEQNUM                                                     
         MVC   SEQHI,SEQNUM                                                     
*                                                                               
DIS14    LA    R2,SRJOBQLN(R2)                                                  
         BCT   R0,DIS4                                                          
*                                                                               
DISX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT A DISPLAY LINE                                    *         
* NTRY:  R2=A(JOB TABLE ENTRY)                                        *         
* EXIT:  CC=NOT EQUAL IF JOB WON'T FIT ON SCREEN                      *         
***********************************************************************         
FORMAT   NTR1                                                                   
         USING SRJOBQ,R2                                                        
         LLC   R1,SEQDIS                                                        
         LA    R1,1(R1)                                                         
         LA    R0,JOBTTDIS                                                      
         LA    RE,JOBTLDIS                                                      
         CR    R1,R0               WILL THIS FIT ON SCREEN                      
         BH    FORMATN             NO                                           
         STC   R1,SEQDIS                                                        
         LA    R3,JOBLINH          POINT TO LEFT SIDE OF SCREEN                 
         CR    R1,RE               TEST FIELD FITS ON LEFT SIDE                 
         BNH   *+10                YES                                          
         LA    R3,JOBLINEL(R3)     POINT TO RIGHT SIDE OF SCREEN                
         SR    R1,RE                                                            
         BCTR  R1,0                                                             
         LA    R0,JOBLINEL*2                                                    
         MR    R0,R0                                                            
         AR    R3,R1                                                            
         USING JOBLINED,R3         R3=A(TWA DISPLAY LINE)                       
         NI    JOBLACTH+1,X'FF'-X'20'                                           
         TM    DISFLAG,DISFADTA    TEST TABLE BUILD ONLY                        
         BZ    FORMATY                                                          
         LA    R1,KEY                                                           
         USING CTIKEY,R1           READ USER-ID RECORD                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),SRJOBUSR                                             
         CLC   CTIKEY,IO           TEST ID RECORD IN CORE                       
         BE    FORMAT1                                                          
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0             TEST FOR ERRORS                              
         JNE   *+2                                                              
FORMAT1  LA    R1,IO+(CTIDATA-CTIREC)                                           
         SR    R0,R0                                                            
FORMAT2  CLI   0(R1),0             LOCATE ALPHA USER-ID ELEMENT                 
         JE    *+2                                                              
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     FORMAT2                                                          
         MVC   DUB,2(R1)           SAVE ALPHA USER-ID                           
         MVC   JOBLUSER,DUB                                                     
         MVC   JOBLREPT(3),SRJOBSUB                                             
         MVI   JOBLREPT+3,C','                                                  
         LA    R1,JOBLREPT+4                                                    
         SR    R0,R0                                                            
         ICM   R0,3,SRJOBREP                                                    
         EDIT  (R0),(5,0(R1)),ALIGN=LEFT                                        
*                                                                               
         TM    JOBFLAG2,JOBFPIN    TEST IF PIN PROTECTED                        
         BZ    *+8                                                              
         MVI   JOBLSTAT-1,C'>'                                                  
*                                                                               
         L     R1,=A(STATAB)                                                    
         A     R1,RELO             R1=A(REPORT STATUS TABLE)                    
         LA    R1,2(R1)                                                         
FORMAT3  CLI   0(R1),0             TEST E-O-T                                   
         BNE   *+12                                                             
         LA    R1,SR@UNKNW                                                      
         B     FORMAT4                                                          
         CLC   JOBFLAG,L'STATAB-1(R1)                                           
         BE    FORMAT4                                                          
         LA    R1,L'STATAB(R1)                                                  
         B     FORMAT3                                                          
FORMAT4  MVC   JOBLLINH+5(1),4(R1) SET COLOUR ATTRIB                            
         EX    0,0(R1)             R6=A(STATUS)                                 
*NOP     CLC   JOBSRV+4(2),=C',1'  CHECK FORMAT#1                               
*NOP     BE    FORMAT8                                                          
         MVC   JOBLSTAT,0(R6)                                                   
         MVC   BYTE,JOBFLAG2                                                    
         NI    BYTE,255-JOBFPIN                                                 
         CLI   BYTE,0              TEST PRTD/SENT/ERR STATUS SET                
         BNE   FORMAT5                                                          
         TM    TERMSTAT,TSTATDDS   TEST DDS TERMINAL                            
         BZ    FORMATA                                                          
         OC    SRJOBJES,SRJOBJES   YES-TEST JES JOB NUMBER SET                  
         BZ    FORMATA                                                          
         B     FORMATA             NOW USING JES JOB NUM FOR TIME STAMP         
         SR    R0,R0               DISPLAY JES JOB NUMBER                       
         ICM   R0,3,SRJOBJES                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   JOBLSTA2(1),SR@JOB                                               
         CLC   SRJOBJES,=X'FFFF'                                                
         BNE   *+14                                                             
         MVC   JOBLSTA2+1(4),SR@HIGH                                            
         B     *+10                                                             
         UNPK  JOBLSTA2+1(4),DUB                                                
         B     FORMATA                                                          
*                                                                               
FORMAT5  L     R1,=A(ST2TAB)       R1=A(REPORT STATUS2 TABLE)                   
         A     R1,RELO                                                          
FORMAT6  CLI   0(R1),0             TEST E-O-T                                   
         BE    FORMATA                                                          
         MVC   BYTE,JOBFLAG2                                                    
         NI    BYTE,255-JOBFPIN                                                 
         CLC   BYTE,L'ST2TAB-1(R1)                                              
         BE    FORMAT7                                                          
         LA    R1,L'ST2TAB(R1)                                                  
         B     FORMAT6                                                          
FORMAT7  EX    0,0(R1)             RF=A(TEXT)                                   
         MVC   JOBLSTAT,SPACES     CLEAR "READY"                                
         MVC   JOBLSTAT(5),0(RF)   OVERWRITE WITH SECONDARY STATUS              
*                                                                               
FORMATA  CLC   JOBSRV+4(2),=C',1'  CHECK FORMAT#1                               
         BE    FORMAT8                                                          
         B     FORMATA1                                                         
*                                                                               
FORMAT8  MVI   JOBLF1ST+6,C' '     JOB,1 - SHOW CREATION TIME STAMP             
         SR    R0,R0                                                            
         ICM   R0,3,SRJOBCTS       SHOW DAY/HOUR/MIN AS AS DD:HH:MM             
         BZ    FORMAT8X                                                         
         SRDL  R0,6                SHIFT OUT MINUTE                             
         SRL   R1,32-6                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  JOBLF1CS+6(2),DUB                                                
         MVI   JOBLF1CS+5,C':'                                                  
         SRDL  R0,5                SHIFT OUT HOUR                               
         SRL   R1,32-5                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  JOBLF1CS+3(2),DUB                                                
         MVI   JOBLF1CS+2,C':'                                                  
         SRDL  R0,5                SHIFT OUT DAY                                
         SRL   R1,32-5                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  JOBLF1CS(2),DUB                                                  
FORMAT8X EQU   *                                                                
*                                                                               
FORMATA1 TM    DISFLAG,DISFASEQ    TEST FORMAT SEQUENCE NUMBERS                 
         BZ    FORMATY                                                          
         LLC   R0,SEQNUM                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  JOBLACT+1(2),DUB                                                 
         MVI   JOBLACT,C'*'                                                     
         B     FORMATY                                                          
*                                                                               
FORMATN  LTR   RB,RB                                                            
         B     EXIT                                                             
FORMATY  CR    RB,RB                                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CHANGE JOB QUEUE                                                    *         
***********************************************************************         
CHA      MVI   HELP,4                                                           
         MVI   SEQCHA,0                                                         
         MVI   NUMCHA,0                                                         
         XC    NUMCAN,NUMCAN                                                    
         LA    R1,36                                                            
         LA    R3,JOBLIN                                                        
*                                                                               
CHA02    CLI   0(R3),C'?'          SCAN FOR ? IN LINE FIELDS                    
         BE    GO2                                                              
         LA    R3,JOBLINEL(R3)                                                  
         BCT   R1,CHA02                                                         
         MVI   HELP,0                                                           
*                                                                               
CHA04    LLC   R1,SEQCHA           BUMP SEQUENCE NUMBER                         
         LA    R1,1(R1)                                                         
         CLM   R1,1,SEQDIS                                                      
         BH    CHAX                                                             
         STC   R1,SEQCHA           SAVE NEXT NUMBER                             
*                                                                               
         LA    R0,JOBTTDIS                                                      
         LA    RE,JOBTLDIS                                                      
         LA    R3,JOBLINH          POINT TO LEFT SIDE OF SCREEN                 
         CR    R1,RE               TEST LEFT SIDE PROCESSED                     
         BNH   *+10                NO                                           
         LA    R3,JOBLINEL(R3)     POINT TO RIGHT SIDE OF SCREEN                
         SR    R1,RE                                                            
         BCTR  R1,0                                                             
         LA    R0,JOBLINEL*2                                                    
         MR    R0,R0                                                            
         AR    R3,R1                                                            
         USING JOBLINED,R3         R3=A(TWA DISPLAY LINE)                       
*                                                                               
         CLI   JOBLACTH+5,0        IGNORE LINES WITH NO INPUT                   
         BE    CHA04                                                            
         CLI   JOBLACT,C'*'        IGNORE UNCHANGED LINES                       
         BE    CHA04                                                            
*                                                                               
         GOTO1 FVAL,JOBLACTH                                                    
*                                                                               
         L     R1,ASUBTAB          R1=A(SUB-ACTION TABLE)                       
         USING SUBTABD,R1                                                       
CHA06    CLI   SUBNAME,0           SEARCH SUB-ACTION TABLE FOR ACTION           
         BE    EILA                                                             
         EX    0,SUBCODE           RF=A(SUB-ACTION TEXT)                        
         CLC   FLD(1),0(RF)                                                     
         BE    CHA07                                                            
         LA    R1,SUBTABL(R1)                                                   
         B     CHA06                                                            
*                                                                               
CHA07    TM    SUBINDS,SUBIDDS                                                  
         BZ    CHA07A                                                           
         TM    TERMSTAT,TSTATDDS                                                
         BZ    EILA                                                             
*                                                                               
CHA07A   ICM   RF,7,SUBROUT                                                     
         LA    RF,0(RF)            RF=A(SUB-ACTION ROUTINE)                     
         A     RF,RELO                                                          
         LLC   RE,SEQCHA                                                        
         BCTR  RE,0                                                             
         LR    R1,RE                                                            
         SLL   RE,1                                                             
         LA    RE,JOBSTAB(RE)      RE=A(C/I ADDRESS)                            
         LA    R1,JOBSSTAT(R1)                                                  
         MVC   JOBFLAG,0(R1)       SET JOB STATUS FLAG                          
*                                                                               
         LLC   R0,SRJOBINQ                                                      
         LA    R1,SRJOBQ                                                        
         USING SRJOBQ,R1                                                        
CHA08    CLI   SRJOBSTA,SRJOBERR   IGNORE ERROR ENTRIES                         
         BE    CHA08A                                                           
         CLC   SRJOBREP,0(RE)      LOCATE ENTRY IN SRJOBQ FOR REP NUM           
         BER   RF                  GO TO ACTION ROUTINE                         
CHA08A   LA    R1,SRJOBQLN(R1)                                                  
         BCT   R0,CHA08                                                         
         DC    H'0'                                                             
*                                                                               
CHA10    LLC   R1,NUMCHA           BUMP CHANGE COUNT                            
         LA    R1,1(R1)                                                         
         STC   R1,NUMCHA                                                        
         B     CHA04                                                            
                                                                                
***********************************************************************         
* JOB DELETE FOR NOTIFIED OR HELD ONLY                                *         
***********************************************************************         
CHADEL   CLI   JOBFLAG,JOBFNOT     DELETE ONLY IF HOLD OR NOTIFY                
         BE    CHADEL10                                                         
         LA    RF,JOBLACTH                                                      
         ST    RF,FADR                                                          
         B     ECDJ                                                             
*                                                                               
CHADEL10 MVI   SRJOBSTA,SRJOBERR   FLAG ENTRY IN ERROR                          
         MVI   JOBLACT,C'*'                                                     
         MVC   JOBLACT+1(L'JOBLACT-1),SR@DEL                                    
         B     CHA10               NEXT                                         
                                                                                
***********************************************************************         
*  JOB CANCEL FOR SUBMITTED ONLY                                      *         
***********************************************************************         
CHACAN   CLI   JOBFLAG,JOBFSUB     CANCEL ONLY IF SUBMITTED                     
         BE    CHCAN01                                                          
         LA    RF,JOBLACTH                                                      
         ST    RF,FADR                                                          
         B     ECCJ                                                             
*                                                                               
CHCAN01  DS    0H                                                               
*&&DO                                                                           
         MVI   SRJOBSTA,SRJOBERR   FLAG LOCAL ENTRY IN ERROR                    
         L     RE,VSSB                                                          
         ICM   R2,15,SSBAJOB-SSBD(RE)                                           
         XR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         ICM   RF,15,2(R2)                                                      
         LA    R2,6(R2)                                                         
*                                                                               
RUNTAB   USING JOBTABD,R2          MATCH LOCAL TO GLOBAL                        
CHCAN02  TM    RUNTAB.JOBSTAT,JOBSUSE                                           
         BZ    CHCAN04                                                          
         CLC   TERMNUM,RUNTAB.JOBTERM                                           
         BNE   CHCAN04                                                          
         CLC   SRJOBUSR,RUNTAB.JOBPQUSR                                         
         BNE   CHCAN04                                                          
         CLC   SRJOBSUB,RUNTAB.JOBPQSUB                                         
         BNE   CHCAN04                                                          
         CLC   SRJOBREP,RUNTAB.JOBPQSEQ                                         
         BNE   CHCAN04                                                          
         CLC   RUNTAB.JOBPQSEQ,SRJOBREP                                         
         BNE   CHCAN04                                                          
         MVI   RUNTAB.JOBSTAT,JOBSAVA                                           
         B     CHCAN06                                                          
*                                                                               
CHCAN04  BXLE  R2,RE,CHCAN02                                                    
         DROP  RUNTAB                                                           
*&&                                                                             
**&&US                                                                          
         BRAS  RE,LOCKCLS                                                       
         BRAS  RE,LOCKJOB                                                       
         BRAS  RE,GETJOBT          GET ENTRY FROM TABS JOBS                     
         BNE   CHCANERR                                                         
         BRAS  RE,INACLS           GET ENTRY FROM TABS CLASS                    
         BNE   CHCANERR                                                         
         BRAS  RE,FREEJOB                                                       
         BRAS  RE,FREECLS                                                       
**&&                                                                            
CHCAN06  BAS   RE,PURGEPQ          PURGE REPORT FROM PRINT QUEUE                
         BNE   ECCJ                                                             
         MVI   SRJOBSTA,SRJOBERR   FLAG LOCAL ENTRY IN ERROR                    
         MVI   JOBLACT,C'*'                                                     
         MVC   JOBLACT+1(L'JOBLACT-1),GE@CAN                                    
*                                                                               
         LLC   R1,NUMCAN+1         BUMP CANCEL COUNT                            
         LA    R1,1(R1)                                                         
         STC   R1,NUMCAN+1                                                      
         B     CHA10                                                            
*                                                                               
CHCANERR BRAS  RE,FREEJOB                                                       
         BRAS  RE,FREECLS                                                       
         B     ECCJ                                                             
*                                                                               
CHAX     CLI   NUMCHA,0            TEST ANY CHANGES MADE                        
         BE    GO2                 NO-REDISPLAY QUEUE                           
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'80',NUMCAN),F#SETUSR                            
*                                                                               
         MVI   MSG,252             SET MESSAGE & FADR FOR EXIT                  
         XC    WORK,WORK                                                        
         LA    R1,JOBACTH                                                       
         ST    R1,FADR                                                          
         XC    TRANSNUM,TRANSNUM   FORCE DISPLAY NEXT TIME                      
         B     GOX                                                              
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND ENTRY IN TABS JOB TABLE                             *         
* NTRY:  R1=SRJOBQ ENTRY TO FIND                                      *         
***********************************************************************         
JT       USING DMSPACED,DSPHD                                                   
GETJOBT  NTR1                                                                   
         LA    R4,1                SET CONDITION CODE TO NOT OK                 
         LR    R3,R1               USING R3 INSTEAD OF R1                       
         USING SRJOBQ,R3                                                        
         LAM   AR0,ARF,ARZERO                                                   
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         MVI   DUB,X'20'           SET ENQUIRE                                  
         GOTO1 VLOCKSPC,DUB        GET JOBTAB HEADER                            
         ICM   RF,15,4(R1)                                                      
         MVC   DSPHD,0(RF)                                                      
         NI    JT.DSPTFRST,X'3F'   TURN OFF X'40' BIT                           
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,TBLET       ALET TO TABS DATASPACE                       
         ICM   R2,15,JT.DSPTFRST   DISCPLACEMENT TO JOB TABLE                   
         SAC   512                                                              
*                                                                               
         USING TABJOBS,R2                                                       
         MVC   JOBHDRL,TBJLHEAD    SET SOFT HEADER LENGTH                       
         MVC   JOBTABL,TBJLNTRY    SET SOFT ENTRY  LENGTH                       
         OC    JOBHDRL,JOBHDRL     IF NOT SET THEN SET TO OLD VALUES            
         BNZ   GETJT10                                                          
         MVC   JOBHDRL,=H'32'      USE OLD HARD DEFAULTS                        
         MVC   JOBTABL,=H'48'                                                   
*                                                                               
GETJT10  MVI   CANERR#,1           JOB NOT FOUND                                
         ICM   R0,15,0(R2)         R0=NUM JOBS IN DATASPACE TABLE               
         BZ    GETJT50                                                          
         AH    R2,JOBHDRL          R2=A(START OF TABLE)                         
         USING TBJOBTAB,R2                                                      
*                                                                               
GETJT20  CLM   R2,15,JT.DSPTEND    PAST END OF TABLE?                           
         BH    GETJT50                                                          
*NOP*    CLC   TBJNTRY(4),NULLS    TEST USED ENTRY                              
*NOP*    BE    GETJT40             NEXT ENTRY IN TABS                           
*NOP*    CLC   TBJCLASS,EFFS                                                    
*NOP*    BE    GETJT40                                                          
         CLI   TBJSTAT,0                                                        
         BE    GETJT40                                                          
         CLC   TBJTERM,TERMNUM     TERMINAL NUMBER MUST MATCH                   
         BNE   GETJT40                                                          
         CLC   TBJPQKEY,SRJOBUSR                                                
         BNE   GETJT40                                                          
         MVI   CANERR#,2           CAN NOT CANCEL UPDATIVE JOB                  
         TM    TBJSTAT,TBJUPDT     UPDATIVE?                                    
         BNZ   GETJT50             EXIT NOT ALLOWED TO CANCEL                   
         MVI   TBJSTAT,0           PURGE IT                                     
         XR    R4,R4                                                            
         ST    R2,SVJOBNTY                                                      
         MVI   CANERR#,0           WE JUST SET IT TO PURGE                      
         B     GETJT50                                                          
         DROP  JT                                                               
*                                                                               
GETJT40  AH    R2,JOBTABL                                                       
         BCT   R0,GETJT20                                                       
         MVI   CANERR#,3           JOB NOT FOUND                                
*                                                                               
GETJT50  SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         LTR   R4,R4               SET CC                                       
         J     EXIT                                                             
         DROP  R3,R2                                                            
         EJECT                                                                  
***********************************************************************         
* SCAN CLASS TABLE TO CHECK IF THIS IS IN A SUBMITTED QUEUE           *         
* THEN REMOVE IT IF IT IS, WHICH IT SHOULD BE                         *         
***********************************************************************         
CLS      USING DMSPACED,DSPHD                                                   
*                                                                               
INACLS   NTR1                                                                   
         L     R3,SVJOBNTY                                                      
         LA    R6,3                ERROR 3 (NOT FOUND)                          
         LAM   AR0,ARF,ARZERO                                                   
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDCLASS)                                            
         MVI   DUB,X'20'           SET ENQUIRE                                  
         GOTO1 VLOCKSPC,DUB        GET JOBTAB HEADER                            
         ICM   RF,15,4(R1)                                                      
         MVC   DSPHD,0(RF)                                                      
         NI    CLS.DSPTFRST,X'3F'  TURN OFF X'40' BIT                           
*                                                                               
         USING JCLASSD,R2                                                       
         LAM   AR0,ARF,ARZERO                                                   
         ICM   R2,15,CLS.DSPTFRST  CLASS TABLE                                  
         LAM   AR2,AR2,TBLET                                                    
         CPYA  AR3,AR2                                                          
         SAC   512                                                              
         DROP  CLS                                                              
*                                                                               
         USING TBJOBTAB,R3                                                      
INCL02   OC    JCLASSD(JCKEYL),JCLASSD                                          
         BZ    INCLXIT             DONE                                         
         XR    R4,R4                                                            
*                                                                               
INCL04   ICM   R3,15,JCFSUB        ANY SUBMITTED JOBS HERE?                     
         BZ    INCL08              NO-TRY NEXT                                  
         LR    R4,R3               POINTING TO ITSELF TO START                  
*                                                                               
INCL06   CLM   R3,15,SVJOBNTY      MATCH MY ADDRESS                             
         BE    INCL10              FOUND ME                                     
         LR    R4,R3               R4 POINTS WILL TO PRIOR JOB ENTRY            
         ICM   R3,15,TBJNXT        TRY NEXT LINK TO JOB ENTRY                   
         BNZ   INCL06              SEE IF I'M THIS ONE                          
*                                                                               
INCL08   AHI   R2,JCLASSL          NEXT CLASS ENTRY                             
         B     INCL02                                                           
*                                                                               
INCL10   XR    R6,R6               FOUND THE ENTRY (GOOD SO FAR)                
         XR    R0,R0                                                            
         ICM   R0,3,JCNSUB         REDUCE SUBMITTED COUNT                       
         BZ    *+8                 SOMETHING A BIT STRANGE IF WE JUMP           
         AHI   R0,-1               LESS ONE IN CLASS TABLE                      
         STCM  R0,3,JCNSUB                                                      
*                                                                               
         CR    R4,R3               EQUAL THEN I'M 1ST JOB SUBMITTED             
         BNE   INCL12                                                           
         MVC   JCFSUB,TBJNXT       SET NEXT AS FIRST                            
         C     R3,JCLSUB           AM I ALSO LAST SUBMITTED JOB?                
         BNE   INCLXIT             NO. DONE SO NO TBJNXT TO SET                 
         XC    JCLSUB,JCLSUB       CLEAR LAST SUBMITTED, NOTHING SUBED          
         CLC   JCFSUB,JCLSUB       THIS SHOULD BE ZERO NOW                      
         BE    INCLXIT                                                          
         LHI   R6,1                ERROR 1. LINK IS BROKEN                      
         B     INCLXIT             NOT MY PROBLEM FOR NOW (REBUILD?)            
*                                                                               
PS       USING TBJOBTAB,R4         PS=PRIOR SUBMITTED JOB                       
INCL12   CPYA  AR4,AR3                                                          
         CLM   R3,15,PS.TBJNXT     GET JOB THAT POINTS TO ME                    
         BE    INCL16                                                           
         LHI   R6,2                ERROR 2.                                     
         B     INCLXIT             NOT MY PROBLEM FOR NOW (REBUILD?)            
*                                                                               
INCL16   MVC   PS.TBJNXT,TBJNXT    FIX UP LINKS (TAKE ME OUT)                   
         XC    TBJNXT,TBJNXT                                                    
*                                                                               
         CLM   R3,15,JCLSUB        WAS I THE LAST SUBMITTED?                    
         BNE   *+8                 NO                                           
         STCM  R4,15,JCLSUB        NOW THIS IS THE TAIL                         
         DROP  PS                                                               
*                                                                               
INCLXIT  SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         LTR   R6,R6                                                            
         J     EXIT                SET CC                                       
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PURGE A JOB FROM THE PRINT QUEUE                         *         
* NTRY:  R1=SRJOBQ ENTRY TO PURGE                                     *         
***********************************************************************         
PURGEPQ  NTR1                                                                   
         USING SRJOBQ,R1                                                        
         LA    R2,NDX              READ INDEX ENTRY USING REPORT ID             
         USING UKRECD,R2                                                        
         XC    NDX,NDX                                                          
         MVC   UKKEY(7),SRJOBUSR                                                
         MVI   UKFLAG,UKFLNUM                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,INDEX,PRTQUE,NDX,WORK,CIREC                        
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         MVC   DSKADR,UKUSRINF                                                  
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),UKUSRINF+4                                           
         TM    UKATTB,UKATJOBI+UKATJOBO                                         
         BO    PURGE020                                                         
         TM    UKTYPE,UKTYUPDT                                                  
         BO    PURGE020                                                         
         DROP  R2                                                               
*                                                                               
PURGE010 GOTO1 VDATAMGR,DMCB,PURGE,PRTQID,NDX,LINE,CIREC                        
         CLI   8(R1),0                                                          
         BE    PURGE030                                                         
         DC    H'0'                PQ ERROR                                     
*                                                                               
PURGE020 LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
*                                                                               
PURGE030 CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOCK JOB                                                            *         
***********************************************************************         
LOCKJOB  NTR1                                                                   
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         GOTOR VLOCKSPC,DUB                                                     
         MVI   JOBLOCK,C'Y'                                                     
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FREE JOB TABLE                                                      *         
***********************************************************************         
FREEJOB  NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=AL4(DTJOB)                                              
         OI    DMCB,X'10'                                                       
         GOTOR VLOCKSPC,DMCB                                                    
         MVI   JOBLOCK,C'N'                                                     
         J     EXIT                                                             
                                                                                
***********************************************************************         
* LOCK CLASS TABLE                                                    *         
***********************************************************************         
LOCKCLS  NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         GOTOR VLOCKSPC,DMCB                                                    
         MVI   CLSLOCK,C'Y'                                                     
         J     EXIT                                                             
                                                                                
***********************************************************************         
* FREE CLASS TABLE                                                    *         
***********************************************************************         
FREECLS  NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         OI    DMCB,X'10'                                                       
         GOTOR VLOCKSPC,DMCB                                                    
         MVI   CLSLOCK,C'N'                                                     
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT AND PRE-VALIDATE A TWA INPUT FIELD               *         
*                                                                     *         
* NTRY:  R1=A(TWA FIELD HEADER)                                       *         
*        HELP=HELP TYPE IF HELP SUPPORTED FOR INPUT FIELD             *         
* EXIT:  FERN=FIELD ERROR NUMBER                                      *         
*        FADR=A(INPUT FIELD HEADER)                                   *         
*        FLDH=TWA FIELD HEADER                                        *         
*        FLD =EXTRACTED & SPACE FILLED INPUT FIELD                    *         
*        CC  =EQ IF NO INPUT                                          *         
*                                                                     *         
* NOTE:  HELP INTERFACE WILL BE ENTERED DIRECTLY FROM FVAL IF 'HELP'  *         
*        OR QUESTION MARK ARE INPUT IN FIELDS THAT HAVE HELP SUPPORT  *         
***********************************************************************         
FVAL     MVI   FERN,FLDNOTI                                                     
         MVI   FNDX,0              RESET MULTIPLE FIELD INDEX                   
         XC    XTRA,XTRA                                                        
         ST    R1,FADR             SET A(TWA FIELD HEADER)                      
         MVC   FLDH,0(R1)          EXTRACT HEADER & DATA                        
         MVC   FLD,SPACES                                                       
         LLC   RF,FLDH                                                          
         TM    FLDH+1,X'02'        TEST EXTENDED HEADER                         
         BNO   *+8                                                              
         AHI   RF,-8                                                            
         LA    R0,L'FLDH+1                                                      
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)                                                     
*                                                                               
         LA    R1,FLD(RF)          CALCULATE L'INPUT DATA                       
         LA    RF,1(RF)                                                         
FVAL2    CLI   0(R1),C' '          FIND LAST INPUT CHARACTER                    
         BNL   FVAL4                                                            
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         BCT   RF,FVAL2                                                         
         B     FVALX                                                            
*                                                                               
FVAL4    STC   RF,FLDH+5           SET INPUT DATA LENGTH                        
         MVI   FERN,FLDISOK                                                     
         CLI   FLD,X'6F'           IF FIELD STARTS WITH QUESTION MARK           
         BE    FVAL6                                                            
         CLI   FLDH+5,2            OR 'HE(LP)' IS INPUT                         
         BL    FVALX                                                            
         CLI   FLDH+5,4                                                         
         BH    FVALX                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),SR@HELP                                                   
         BNE   FVALX                                                            
*                                                                               
FVAL6    CLI   HELP,0              AND HELP SUPPORTED FOR THIS FIELD            
         BNE   HELPER              GO AND GIVE SOME HELP                        
         MVI   FERN,FLDHELN                                                     
         B     ERROR                                                            
*                                                                               
FVALX    MVI   HELP,0                                                           
         CLI   FERN,FLDNOTI        SET CC=EQ IF FLD NOT INPUT                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR SETTINGS                                                      *         
***********************************************************************         
EMIF     LA    RE,FLDNOTI          MISSING INPUT FIELD                          
         B     ERROR                                                            
EIIF     LA    RE,FLDBADI          INVALID INPUT FIELD                          
         B     ERROR                                                            
EFTS     LA    RE,FLDSHRT          FIELD TOO SHORT                              
         B     ERROR                                                            
EFTL     LA    RE,FLDLONG          FIELD TOO LONG                               
         B     ERROR                                                            
EIAC     LA    RE,FLDIACT          INVALID ACTION                               
         B     ERROR                                                            
EFNN     LA    RE,FLDNOTN          FIELD NOT NUMERIC                            
         B     ERROR                                                            
EIJB     LA    RE,FLDIJOB          INVALID JOB (KEY)                            
         B     ERROR                                                            
EFVB     LA    RE,FLDVBIG          FIELD VALUE TOO BIG                          
         B     ERROR                                                            
EFVS     LA    RE,FLDVSML          FIELD VALUE TOO SMALL                        
         B     ERROR                                                            
ESGE     LA    RE,FLDSGRE          START GREATER THAN END                       
         B     ERROR                                                            
EIID     LA    RE,FLDIDNF          INVALID (USER) ID                            
         B     ERROR                                                            
EIKW     LA    RE,FLDBADK          INVALID KEYWORD                              
         B     ERROR                                                            
EKWI     LA    RE,FLDINCK          KEYWORD INCOMPATIBLE                         
         B     ERROR                                                            
EDKO     LA    RE,FLDDUPK          DUPLICATED KEYWORD OPTION                    
         B     ERROR                                                            
EIDV     LA    RE,FLDBADD          INVALID DATA VALUE                           
         B     ERROR                                                            
EROM     LA    RE,FLDREQK          REQUIRED OPTION MISSING                      
         B     ERROR                                                            
EDTS     LA    RE,FLDDSML          DATA TOO SHORT                               
         B     ERROR                                                            
EDTL     LA    RE,FLDDBIG          DATA TOO LONG                                
         B     ERROR                                                            
ENTC     LA    RE,FLDNCHA          NOTHING TO CHANGE                            
         B     ERROR                                                            
ENTD     LA    RE,FLDNDIS          NOTHING TO DISPLAY                           
         B     ERROR                                                            
EILA     LA    RE,FLDISUB          INVALID LINE ACTION                          
         B     ERROR                                                            
ECDJ     LA    RE,FLDCDEL          CAN'T DELETE JOB                             
         B     ERROR                                                            
ECCJ     LA    RE,FLDCCAN          CAN'T CANCEL JOB                             
         B     ERROR                                                            
ECHJ     LA    RE,FLDCHLD          CAN'T HOLD JOB                               
         B     ERROR                                                            
ECRJ     LA    RE,FLDCREL          CAN'T RELEASE JOB                            
         B     ERROR                                                            
ECMJ     LA    RE,FLDCMOV          CAN'T MOVE JOB (TO TOP)                      
         B     ERROR                                                            
ECSJ     LA    RE,FLDCSUB          CAN'T SUBMIT JOB                             
         B     ERROR                                                            
EITN     LA    RE,FLDITRM          INVALID TERMINAL NUMBER                      
         B     ERROR                                                            
ESNA     LA    RE,FLDJSNA          SUBMITTER NOT AVAILABLE                      
         B     ERROR                                                            
EJRE     LA    RE,FLDJOBE          CAN'T READ JOB FROM PRTQUE                   
         B     ERROR                                                            
ERRTIA   LA    RE,203              TEMPSTR READ ERROR                           
         B     ERROR                                                            
                                                                                
***********************************************************************         
* OUTPUT AN ERROR MESSAGE & EXIT                                      *         
*                                                                     *         
* NTRY:  RE=ERROR NUMBER                                              *         
*        FADR=A(TWA FIELD HEADER OF FIELD IN ERROR)                   *         
*        XTRA=EXTRA MESSAGE TO BE TACKED ON TO OUTPUT MESSAGE OR      *         
*        FNDX=MULTIPLE FIELD INDEX NUMBER                             *         
***********************************************************************         
ERROR    L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD IN ERROR                 
         ST    RE,SAVERE                                                        
*                                                                               
         CLI   TWAREAD,C'Y'        TEST TWA WAS READ                            
         BNE   ERRX                NO                                           
         MVC   JOBSTNUM,TRANSNUM   WRITE BACK S/R SAVE PAGE                     
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,TWAPAGE,SRPATIA                      
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
ERRX     L     RE,SAVERE                                                        
         LTR   RE,RE               ERROR ZERO MEANS INFO                        
         BZ    INFOX                                                            
         GOTO1 VGETTXT,DMCB,(RE),0,(C'E',0),0,0,X'00010000'                     
         MVI   FERN,FLDERR         TELL CALLER ABOUT ERROR                      
         B     EXIT                YOU MAY BE IN A SUBROUTINE                   
*                                                                               
INFOX    IC    RE,MSG                                                           
         GOTO1 VGETTXT,DMCB,(RE),0,(C'I',0),0,WORK,X'00010000'                  
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LTORG AND CONSTANTS                                                 *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
SPACES   DC    CL80' '                                                          
ARZERO   DC    16F'0'                                                           
NULLS    DC    16X'00'                                                          
*                                                                               
PUT      DC    C'PUT'                                                           
ADR      DC    C'ADR'                                                           
END      DC    C'END'                                                           
EOJ      DC    C'EOJ'                                                           
*                                                                               
DMREAD   DC    C'DMREAD  '                                                      
READ     DC    C'READ    '                                                      
DMWRT    DC    C'DMWRT   '                                                      
TEMPSTR  DC    C'TEMPSTR '                                                      
CTFILE   DC    C'CTFILE  '                                                      
PRTQUE   DC    C'PRTQU   '                                                      
INDEX    DC    C'INDEX   '                                                      
PURGE    DC    C'PURGE   '                                                      
         EJECT                                                                  
*                                                                               
DDDCLST  DS    0C                                                               
         DCDDL SR#ALL,4,L,LABEL=SR4ALL                                          
         DCDDL SR#CHNG,8,L                                                      
         DCDDL SR#DSP,8,L                                                       
         DCDDL SR#HELD,9,L                                                      
         DCDDL SR#HELP,8,L                                                      
         DCDDL SR#NTFD,9,L                                                      
         DCDDL SR#READY,9,L                                                     
         DCDDL SR#RNING,9,L                                                     
         DCDDL SR#SCHLD,9,L                                                     
         DCDDL SR#SUBTD,9,L                                                     
         DCDDL SR#STAT,8,L                                                      
         DCDDL SR#TRM,8,L                                                       
         DCDDL SR#TRM,3,L,LABEL=SR3TRM                                          
         DCDDL SR#DEL,3,L                                                       
         DCDDL SR#TOP,3,L                                                       
         DCDDL SR#HOLD,3,L                                                      
         DCDDL SR#RLEAS,3,L                                                     
         DCDDL SR#SUBMT,3,L                                                     
         DCDDL SR#PRTD,5,L                                                      
         DCDDL SR#SENT,5,L                                                      
         DCDDL SR#ERROR,5,L                                                     
         DCDDL SR#HIGH,4,L                                                      
         DCDDL SR#OK,3,L                                                        
         DCDDL SR#JOB,3,L                                                       
         DCDDL SR#UNKNW,9,L                                                     
         DCDDL GE#CAN,3,L                                                       
         EJECT                                                                  
***********************************************************************         
* ACTION TABLE - SEE ACTTABD                                          *         
***********************************************************************         
ACTTAB   DS    0H                                                               
*                                                                               
         DC    X'41F0',S(SR@DSP)                                                
         DC    AL1(ACTTDIS,0,JOBNALLU+JOBNUSER+JOBNSUBR)                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    X'0'                                                             
*                                                                               
         DC    X'41F0',S(SR@CHNG)                                               
         DC    AL1(ACTTCHA,0,JOBNALLU+JOBNUSER+JOBNSUBR)                        
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    X'0'                                                             
*                                                                               
         DC    X'41F0',S(SR@HELP)                                               
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    X'0'                                                             
*                                                                               
ACTTABX  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE - SEE OPTTABD                                          *         
***********************************************************************         
OPTTAB   DS    0H                                                               
*                                                                               
         DC    X'41F0',S(SR@STAT),X'41F0',S(SR@STAT)                            
         DC    AL1(OPTITAB,2,2,9)                                               
         DC    AL1(1),X'8000'                                                   
         DC    AL2(STATAB-JOB,JOBFILT-JOBWRKD),AL1(L'JOBFILT)                   
*                                                                               
         DC    X'41F0',S(SR@TRM),X'41F0',S(SR3TRM)                              
         DC    AL1(OPTIDDS+OPTIRTN,2,1,9)                                       
         DC    AL1(2),X'4000'                                                   
         DC    AL2(VALTRM-JOB,TERMNUM-JOBWRKD),AL1(L'TERMNUM)                   
*                                                                               
         DC    X'41F0',S(SR@RNING),X'41F0',S(SR@RNING)                          
         DC    AL1(OPTIRTN,2,0,0)                                               
         DC    AL1(3),X'2000'                                                   
         DC    AL2(VALSTA-JOB,JOBFILT-JOBWRKD),AL1(L'JOBFILT)                   
*                                                                               
         DC    X'41F0',S(SR@HELD),X'41F0',S(SR@HELD)                            
         DC    AL1(OPTIRTN,2,0,0)                                               
         DC    AL1(4),X'1000'                                                   
         DC    AL2(VALSTA-JOB,JOBFILT-JOBWRKD),AL1(L'JOBFILT)                   
*                                                                               
         DC    X'41F0',S(SR@SCHLD),X'41F0',S(SR@SCHLD)                          
         DC    AL1(OPTIRTN,2,0,0)                                               
         DC    AL1(5),X'0800'                                                   
         DC    AL2(VALSTA-JOB,JOBFILT-JOBWRKD),AL1(L'JOBFILT)                   
*                                                                               
         DC    X'41F0',S(SR@SUBTD),X'41F0',S(SR@SUBTD)                          
         DC    AL1(OPTIRTN,2,0,0)                                               
         DC    AL1(6),X'0400'                                                   
         DC    AL2(VALSTA-JOB,JOBFILT-JOBWRKD),AL1(L'JOBFILT)                   
*                                                                               
         DC    X'41F0',S(SR@READY),X'41F0',S(SR@READY)                          
         DC    AL1(OPTIRTN,2,0,0)                                               
         DC    AL1(7),X'0200'                                                   
         DC    AL2(VALSTA-JOB,JOBFILT-JOBWRKD),AL1(L'JOBFILT)                   
*                                                                               
         DC    X'41F0',S(SR@NTFD),X'41F0',S(SR@NTFD)                            
         DC    AL1(OPTIRTN,2,0,0)                                               
         DC    AL1(8),X'0100'                                                   
         DC    AL2(VALSTA-JOB,JOBFILT-JOBWRKD),AL1(L'JOBFILT)                   
*                                                                               
OPTTABX  DC    H'00'                                                            
         EJECT                                                                  
***********************************************************************         
* SUB-ACTION TABLE - SEE SUBTABD                                      *         
***********************************************************************         
SUBTAB   DS    0H                                                               
         DC    X'41F0',S(SR@DEL),AL1(0),AL3(CHADEL)                             
         DC    X'41F0',S(GE@CAN),AL1(0),AL3(CHACAN)                             
         DC    X'00'                                                            
                                                                                
***********************************************************************         
* STATUS TABLE(S)                                                     *         
***********************************************************************         
         DS    0H                                                               
STATAB   DS    0XL6                PRIMARY STATUS TABLE                         
         DC    AL1(5,1)                                                         
         DC    X'4160',S(SR@RNING),X'06',AL1(JOBFRUN)                           
         DC    X'4160',S(SR@HELD),X'02',AL1(JOBFHLD)                            
         DC    X'4160',S(SR@SCHLD),X'02',AL1(JOBFSCH)                           
         DC    X'4160',S(SR@SUBTD),X'06',AL1(JOBFSUB)                           
         DC    X'4160',S(SR@READY),X'04',AL1(JOBFAVA)                           
         DC    X'4160',S(SR@NTFD),X'04',AL1(JOBFNOT)                            
         DC    H'0'                                                             
*                                                                               
ST2TAB   DS    0XL6                SECONDARY STATUS TABLE                       
         DC    X'41F0',S(SR@PRTD),AL2(JOBFPRT)                                  
         DC    X'41F0',S(SR@SENT),AL2(JOBFSNT)                                  
         DC    X'41F0',S(SR@ERROR),AL2(JOBFERR)                                 
         DC    AL1(0)                                                           
*                                                                               
EXTOPT   DC    CL75'|DDS ONLY TERM=  OR  TRM=  To display job queue of X        
               a specified terminal'                                            
         DC    CL75'|         Format = Terminal number or line address'         
         DC    CL75' '                                                          
         DC    H'0'                                                             
EXTCHA   DC    CL75'|DDS    S(UB)     To submit a scheduled job'                
         DC    CL75' '                                                          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* HELP                                                                *         
***********************************************************************         
HELPID   DC    XL10'0157FF00000000000000'  SYS/PRG/SCRN                         
*                                                                               
HELPER   CLI   TWAREAD,C'Y'        TEST TWA WAS READ                            
         BNE   HELPER1             NO                                           
         MVC   JOBSTNUM,TRANSNUM   WRITE BACK S/R SAVE PAGE                     
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,TWAPAGE,SRPATIA                      
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
HELPER1  MVC   HELPNUM,HELP                                                     
         CLI   HELP,4                                                           
         BNE   HELPER2                                                          
         LA    R1,JOBTABH          SET TAB FLD FOR CHANGE HELP                  
         ST    R1,FADR                                                          
HELPER2  SR    RF,RF               CLEAR EXTRA HELP TEXT                        
         CLI   HELP,3                                                           
         BNE   *+8                                                              
         LA    RF,EXTOPT                                                        
         CLI   HELP,4                                                           
         BNE   *+8                                                              
         LA    RF,EXTCHA                                                        
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         GOTO1 VGETHELP,DMCB,(X'50',HELPKEY),FADR,0,(RF),0                      
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SAVE STORAGE (SR$JOB IN S/R TWA SAVE PAGE)           *         
***********************************************************************         
JOBSAVED DSECT                                                                  
*                                                                               
JOBSTABN DS    H                   N'ENTRIES IN JOBSTAB                         
JOBSMAXN EQU   40                                                               
JOBSTAB  DS    (JOBSMAXN)XL2       C/I ADDRESS OF DISPLAYED REPORT              
JOBSTABL EQU   *-JOBSTAB                                                        
JOBSSTAT DS    (JOBSMAXN)XL1       STATUS FLAG OF DISPLAYED REPORT              
JOBSTNUM DS    XL2                 LAST $JOB TRANSACTION NUMBER                 
*                                                                               
JOBSAVEL EQU   *-JOBSAVED          MUST NOT EXCEED 256 BYTES                    
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE DSECT                                         *         
***********************************************************************         
JOBWRKD  DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
SAVERE   DS    F                                                                
SVJOBNTY DS    A                                                                
DMCB     DS    6F                                                               
PARA     DS    6F                                                               
WORK     DS    XL64                                                             
WORK2    DS    XL64                                                             
*                                                                               
DSPHD    DS    XL(L'DSPHDR)        JOB DATA SPACE HEADER                        
CLSLOCK  DS    C                                                                
JOBLOCK  DS    C                                                                
JOBHDRL  DS    H                   LENGTH OF JOB HEADER                         
JOBTABL  DS    H                   LENGTH OF A TABLE ENTRY                      
*                                                                               
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
         DS    XL6                                                              
*                                                                               
RECLEN   DS    H                   TEMPSTR RECORD LEN                           
         DS    H                                                                
RELO     DS    A                   PROGRAM RELOCATION FACTOR                    
AACTTAB  DS    A                   A(ACTION TABLE)                              
AOPTTAB  DS    A                   A(OPTION TABLE)                              
ASUBTAB  DS    A                   A(SUB-ACTION TABLE)                          
TWAPAGE  DS    A                   TWA PAGE/TERMINAL NUMBER                     
TBLET    DS    A                   ALET TO TABS DATASPACE                       
AUTL     DS    A                   A(UTL ENTRY)                                 
VSCANNER DS    V                                                                
VTERMVAL DS    V                                                                
VGETTXT  DS    V                                                                
VGETHELP DS    V                                                                
VDICTATE DS    V                                                                
VGETFACT DS    V                                                                
*                                                                               
SRPARAS  DS    0F                  SERVICE REQUEST PARAMETER LIST               
SRPASYS  DS    A                   A(SYSFACS)                                   
SRPATIA  DS    A                   A(TIA)                                       
SRPAUTL  DS    A                   A(UTL ENTRY)                                 
SRPACOM  DS    A                   A(COMFACS)                                   
SRPASEL  DS    A                   A(SELIST ENTRY)                              
SRPATWA  DS    A                   A(TWA)                                       
SRPARAL  EQU   *-SRPARAS                                                        
*                                                                               
FADR     DS    A                   A(FIELD HEADER)                              
FERN     DS    X                   FIELD ERROR NUMBER                           
MSG      DS    X                                                                
FLDNOTI  EQU   1                   FIELD NOT INPUT                              
FLDBADI  EQU   2                   FIELD IS INVALID                             
FLDSHRT  EQU   59                  FIELD TOO SHORT                              
FLDLONG  EQU   60                  FIELD TOO LONG                               
FLDIACT  EQU   21                  INVALID ACTION                               
FLDNOTN  EQU   03                  FIELD NOT NUMERIC                            
FLDIJOB  EQU   250                 INVALID JOB KEY                              
FLDVBIG  EQU   9                   FIELD VALUE TOO LARGE                        
FLDVSML  EQU   8                   FIELD VALUE TOO SMALL                        
FLDSGRE  EQU   251                 START GREATER THAN END                       
FLDIDNF  EQU   31                  INVALID USER ID                              
FLDBADK  EQU   26                  INVALID OPTION KEYWORD                       
FLDINCK  EQU   61                  INCOMPATIBLE OPTION KEYWORD                  
FLDDUPK  EQU   62                  DUPLICATED OPTION KEYWORD                    
FLDBADD  EQU   13                  INVALID OPTION DATA VALUE                    
FLDREQK  EQU   63                  REQUIRED FILTER MISSING                      
FLDDBIG  EQU   65                  OPTION DATA VALUE TOO SHORT                  
FLDDSML  EQU   64                  OPTION DATA VALUE TOO LONG                   
FLDNCHA  EQU   252                 NOTHING IN QUEUE TO CHANGE                   
FLDNDIS  EQU   66                  NOTHING IN QUEUE TO DISPLAY                  
FLDISUB  EQU   253                 INVALID LINE ACTION                          
FLDCDEL  EQU   254                 CAN'T DELETE JOB                             
FLDCCAN  EQU   29                  CAN'T CANCEL JOB                             
FLDCHLD  EQU   255                 CAN'T HOLD JOB                               
FLDCREL  EQU   256                 CAN'T RELEASE JOB                            
FLDCMOV  EQU   257                 CAN'T MOVE JOB                               
FLDITRM  EQU   182                 INVALID TERMINAL NUMBER                      
FLDCSUB  EQU   258                 CAN'T SUBMIT JOB                             
FLDJSNA  EQU   259                 JES SUBMITTER NOT AVAILABLE                  
FLDJOBE  EQU   260                 JOB READING ERROR                            
FLDHELN  EQU   0                   HELP NOT SUPPORTED FOR FIELD                 
FLDERR   EQU   0                   ERROR ON THIS FIELD                          
FLDISOK  EQU   255                 FIELD IS INPUT THIS TIME                     
CANERR#  DS    X                                                                
FNDX     DS    X                   MULTIPLE FIELD INDEX                         
HELP     DS    X                   FIELD REQUIRING HELP                         
XTRA     DS    CL8                 EXTRA MESSAGE                                
FLDH     DS    XL8                 EXTRACTED TWA FIELD HEADER                   
FLD      DS    CL80                EXTRACTED TWA FIELD                          
*                                                                               
TWAREAD  DS    C                   C'Y'=SPECIAL S/R TWA PAGE READ               
TERMSTAT DS    XL2                 TERMINAL STATUS BYTES                        
TERMNUM  DS    XL2                 TERMINAL NUMBER                              
TRANSNUM DS    XL2                 CURRENT TERMINAL TRANSACTION NUMBER          
TERMPID  DS    XL4                 TERMINAL PID                                 
ACTION   DS    X                   ACTION TYPE (FROM ACTTYPE)                   
DISFLAG  DS    X                   DISPLAY FLAG BYTE                            
DISFADTA EQU   X'80'               DISPLAY JOB DATA                             
DISFASEQ EQU   X'40'               DISPLAY JOB SEQUENCE                         
CHAFLAG  DS    X                   CHANGE FLAG BYTE                             
*                                                                               
JOBFILT  DS    X                   JOB FLAG FILTER (AS FOR JOBFLAG)             
JOBFLAG  DS    X                   JOB FLAG BYTE                                
JOBFRUN  EQU   X'80'               JOB IS RUNNING                               
JOBFHLD  EQU   X'40'               JOB IN HOLD STATUS                           
JOBFSCH  EQU   X'20'               JOB SCHEDULED                                
JOBFSUB  EQU   X'10'               JOB SUBMITTED                                
JOBFAVA  EQU   X'08'               JOB READY                                    
JOBFNOT  EQU   X'04'               JOB NOTIFIED                                 
JOBFLAG2 DS    X                   JOB FLAG BYTE 2                              
JOBFPRT  EQU   X'80'               REPORT HAS BEEN PRINTED                      
JOBFSNT  EQU   X'40'               REPORT HAS BEEN SENT                         
JOBFERR  EQU   X'20'               REPORT IS INCOMPLETE (ABENDED)               
JOBFPIN  EQU   X'01'               REPORT IS PIN PROTECTED                      
JOBN     DS    X                   JOB VALIDATION INDICS (NEXT FIELD)           
JOBNALLU EQU   X'80'               ALLOW 'ALL'  (USER-ID'S)                     
JOBNALLR EQU   X'40'               ALLOW 'ALL'  (REPORT SUB-ID'S)               
JOBNUSER EQU   X'20'               ALLOW 'UUUU' (SPECIFIC USER-ID)              
JOBNSUBR EQU   X'10'               ALLOW 'RRR'  (SPECIFIC SUB-ID)               
JOBNSTRR EQU   X'08'               ALLOW 'NNNN' (REPORT SEQ NUM START)          
JOBNENDR EQU   X'04'               ALLOW 'MMMM' (REPORT SEQ NUM END)            
JOBNSTRD EQU   X'02'               ALLOW 'NNN'  (JOB SEQ NUM START)             
JOBNENDD EQU   X'01'               ALLOW 'MMM'  (JOB SEQ NUM END)               
JOBNALL  EQU   JOBNALLU+JOBNALLR                                                
JOBNALP  EQU   JOBNUSER+JOBNSUBR                                                
JOBNNUMR EQU   JOBNSTRR+JOBNENDR                                                
JOBNNUMD EQU   JOBNSTRD+JOBNENDD                                                
JOBNNUM  EQU   JOBNNUMR+JOBNNUMD                                                
*                                                                               
JOBKEY   DS    0X                  JOB KEY/FILTER                               
JOBKUSER DS    XL2                 USER-ID                                      
JOBKSUBR DS    CL3                 REPORT SUB-ID                                
JOBKSTRR DS    XL2                 REPORT SEQUENCE NUMBER START                 
JOBKENDR DS    XL2                 REPORT SEQUENCE NUMBER END                   
JOBKSTRD DS    XL1                 JOB SEQUENCE NUMBER START                    
JOBKENDD DS    XL1                 JOB SEQUENCE NUMBER END                      
JOBKEYL  EQU   *-JOBKEY                                                         
*                                                                               
SEQS     DS    0XL5                                                             
SEQLO    DS    X                   LOW JOB SEQUENCE NUMBER DISPLAYED            
SEQHI    DS    X                   HIGH JOB SEQUENCE NUMBER DISPLAYED           
SEQNUM   DS    X                   MAXIMUM NUMBER OF JOBS THAT QUALIFY          
SEQDIS   DS    X                   CURRENT DISPLAY SEQUENCE NUMBER              
SEQCHA   DS    X                   CURRENT CHANGE SEQUENCE NUMBER               
NUMCHA   DS    X                   NUMBER OF QUEUE CHANGES MADE                 
*                                                                               
NUMCAN   DS    H                   NUMBER OF QUEUE CANCELS                      
*                                                                               
OPTR     DS    XL2                 REQUIRED FILTERS                             
OPTX     DS    XL2                 INCOMPATIBLE OPTIONS                         
OPTI     DS    XL2                 OPTIONS INPUT SO FAR                         
IADDR    DS    A                   A(OPTION VALIDATION ROUTINE/TABLE)           
OADDR    DS    A                   A(OUTPUT OPTION VALUE)                       
*                                                                               
LINE     DS    XL256               FOR SUBJOB ROUTINE                           
SCANLIN  DS    X                   N'ENTRIES IN SCANNER BLOCK                   
SCANBLK  DS    6XL32               SCANNER BLOCK                                
*                                                                               
SAVEJOBN DS    H                   N'ENTRIES IN SAVEJOBT                        
SAVEJOBT DS    (JOBSMAXN)XL2       C/I ADDRESS OF DISPLAYED REPORT              
SAVEJOBL EQU   *-SAVEJOBT                                                       
*                                                                               
DDDSLST  DS    0C                                                               
         DSDDL PRINT=YES                                                        
*                                                                               
DSKADR   DS    F                                                                
PRTQID   DS    CL8                                                              
NDX      DS    XL40                                                             
         DS    XL24                                                             
KEY      DS    XL25                                                             
IO       DS    1000C                                                            
CIREC    DS    14336C                                                           
*                                                                               
JOBWRKX  EQU   *                                                                
         EJECT                                                                  
ACTTABD  DSECT                     ACTION TABLE                                 
ACTNAME  DS    CL4                 ACTION NAME (MIN CHARS INPUT)                
ACTTYPE  DS    X                   ACTION TYPE                                  
ACTTDIS  EQU   X'80'               DISPLAY JOB QUEUE                            
ACTTCHA  EQU   X'40'               CHANGE JOB QUEUE                             
ACTINDS  DS    X                   ACTION INDICATORS                            
ACTIDDS  EQU   X'80'               DDS ONLY ACTION                              
ACTJOBI  DS    X                   JOB VALIDATION INDICATORS                    
ACTOPTR  DS    XL2                 REQUIRED FILTERS                             
ACTOPTX  DS    XL2                 OPTIONS NOT ALLOWED                          
         DS    XL1                 SPARE                                        
ACTTABL  EQU   *-ACTTABD                                                        
*                                                                               
OPTTABD  DSECT                     OPTION TABLE                                 
OPTNAME  DS    CL4                 OPTION NAME (LONG)                           
OPTSHRT  DS    CL4                 OPTION NAME (SHORT) OR SPACES                
OPTINDS  DS    XL1                 OPTION INDICATORS                            
OPTIDDS  EQU   X'80'               DDS ONLY OPTION                              
OPTIRTN  EQU   X'40'               OPTIADDR IS DISPLACEMENT TO ROUTINE          
OPTITAB  EQU   X'20'               OPTIADDR IS DISPLACEMENT TO TABLE            
OPTIHLP  EQU   X'10'               VALIDATION TABLE HAS HELP HEADER             
OPTMINKL DS    XL1                 MINIMUM INPUT L'OPTNAME ALLOWED              
OPTMINDL DS    XL1                 MINIMUM L'DATA REQUIRED                      
OPTMAXDL DS    XL1                 MAXIMUM L'DATA REQUIRED                      
OPTOPTN  DS    XL1                 OPTION NUMBER (1 THRU 15)                    
OPTOPTB  DS    XL2                 OPTION BIT MASK (X'0001....8000')            
*                                  CORRESPONDING TO OPTION NUMBER               
OPTIADDR DS    AL2                 DISPLACEMENT OF TABLE/ROUTINE                
OPTOADDR DS    AL2                 DISPLACEMENT OF OUTPUT OPTION                
OPTOUTDL DS    XL1                 L'OUTPUT OPTION                              
OPTTABL  EQU   *-OPTTABD                                                        
*                                                                               
SUBTABD  DSECT                     SUB-ACTION TABLE                             
SUBCODE  DS    0C                  SUB-ACTION CODE                              
SUBNAME  DS    CL4                 SUB-ACTION NAME POINTER                      
SUBINDS  DS    XL1                 SUB-ACTION INDICATORS                        
SUBIDDS  EQU   X'80'               DDS ONLY SUB-ACTION                          
SUBROUT  DS    AL3                 SUB-ACTION ROUTINE ADDRESS                   
SUBTABL  EQU   *-SUBTABD                                                        
         EJECT                                                                  
JOBTWAD  DSECT                                                                  
         DS    XL64                                                             
       ++INCLUDE SRJOBFFD                                                       
         EJECT                                                                  
JOBLINED DSECT                     DISPLAY/CHANGE LINE                          
JOBLACTH DS    XL8                                                              
JOBLACT  DS    CL3                 JOB QUEUE ACTION                             
JOBLLINH DS    XL8                                                              
JOBLLIN  DS    0CL34               JOB QUEUE DISPLAY                            
JOBLUSER DS    CL8                 USER-ID                                      
         DS    C                                                                
JOBLREPT DS    CL9                 REPORT-ID (RRR,NNNNN)                        
         DS    C                                                                
JOBLSTAT DS    CL9                 JOB STATUS                                   
         DS    C                                                                
JOBLSTA2 DS    CL5                 REPORT STATUS                                
         ORG   JOBLSTAT                                                         
JOBLF1ST DS    CL6                 FORMAT#1 - JOB STATUS                        
         DS    C                                                                
JOBLF1CS DS    CL8                 FORMAT#1 - TIME STAMP DD:HH:MM               
JOBLINEL EQU   *-JOBLINED                                                       
*                                                                               
JOBTTDIS EQU   (JOBXXXH-JOBLINH)/JOBLINEL                                       
JOBTRDIS EQU   JOBTTDIS/2                                                       
JOBTLDIS EQU   JOBTTDIS-JOBTRDIS                                                
*                                                                               
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* FATABSJOB                                                                     
       ++INCLUDE FATABSJOB                                                      
* DMSPACED                                                                      
       ++INCLUDE DMSPACED                                                       
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
* GEDDEQUS                                                                      
       ++INCLUDE GEDDEQUS                                                       
* SRDDEQUS                                                                      
       ++INCLUDE SRDDEQUS                                                       
* DMPRTQD                                                                       
       ++INCLUDE DMPRTQD                                                        
* DMPRTQK                                                                       
       ++INCLUDE DMPRTQK                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRJOB00   05/12/20'                                      
         END                                                                    
