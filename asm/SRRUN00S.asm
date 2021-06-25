*          DATA SET SRRUN00S   AT LEVEL 078 AS OF 05/01/02                      
*PHASE T15800A                                                                  
*INCLUDE QSORT                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'SRRUN00 - DISPLAY/CHANGE SCHEDULER JOB QUEUE'                   
RUN      CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL RUNWRKX-RUNWRKD,**$RUN**,RA,CLEAR=YES,RR=RE                      
         USING RUNWRKD,RC          RC=A(W/S)                                    
         ST    RE,RELO                                                          
         MVC   SRPARAS(SRPARAL),0(R1)                                           
*                                                                               
*        XIT1                                                                   
*                                                                               
         L     R9,SRPATIOB                                                      
         USING TIOBD,R9            R9=A(TIOB)                                   
         MVC   PFKEY,TIOBAID       PF KEY NUMBER                                
         DROP  R9                                                               
*                                                                               
         L     R9,SRPASYS                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R6,SRPATIA                                                       
         USING SRSD,R6             R6=A(SPECIAL S/R SAVE PAGE)                  
         L     R1,VSSB                                                          
         MVC   RECLEN,SSBTWAL-SSBD(R1)                                          
         L     R1,SSBAJOB-SSBD(R1)                                              
         MVC   JOBTABL,0(R1)                                                    
         MVC   JOBTABX,2(R1)                                                    
         LA    R1,6(R1)                                                         
         ST    R1,AJOBTAB                                                       
         L     R8,SRPATWA                                                       
         USING RUNTWAD,R8          R8=A(TWA)                                    
         NI    RUNACTH+6,X'FF'-X'40'                                            
         L     R1,SRPAUTL                                                       
         USING UTLD,R1                                                          
         MVC   TERMSTAT,TSTAT                                                   
         MVC   TRANSNUM,TTRCNT                                                  
         L     R1,SRPACOM                                                       
         USING COMFACSD,R1         R1=A(COMFACS)                                
         MVC   VSCANNER,CSCANNER                                                
         MVC   VTERMVAL,CTERMVAL                                                
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETFACT,CGETFACT                                                
         DROP  R1                                                               
*                                                                               
         L     R1,VSSB             ARE WE RUNNING UNDER MONSOON?                
         CLI   SSBJESIO-SSBD(R1),C' '                                           
         BE    *+16                YES -- COLUMN HEADINGS ARE CORRECT           
         MVC   RUNHED+15(8),=C'JES Job '                                        
         MVC   RUNHEDU+15(8),=C'------- '                                       
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
         L     R1,=A(STATAB)                                                    
         A     R1,RELO                                                          
         ST    R1,ASTATAB                                                       
         LR    R1,RC               RUNWRKD                                      
         AH    R1,=Y(MYJOBTAB-RUNWRKD)                                          
         ST    R1,AMYJTAB                                                       
*                                                                               
         L     R0,AMYJTAB                                                       
         L     R1,=A(RUNWRKX-MYJOBTAB)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*                                                                               
RUNX     DS    0H                                                               
         EJECT                                                                  
* VALIDATE ACTION                                                               
*                                                                               
VALACT   L     R1,AACTTAB                                                       
         USING ACTTABD,R1                                                       
         CLI   RUNACTH+5,0                                                      
         BNE   *+10                                                             
         MVC   RUNACT(L'ACTNAME),ACTNAME                                        
         MVI   HELP,1              SET HELP NUMBER                              
         GOTO1 FVAL,RUNACTH                                                     
         BE    ERROR                                                            
         CLI   FLDH+5,2            INPUT MUST BE AT LEAST 2 CHARACTERS          
         BL    EFTS                                                             
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                RE=L'INPUT-1                                 
VALACT2  L     R1,AACTTAB                                                       
VALACT4  CLI   ACTNAME,0           TEST E-O-T                                   
         BNE   VALACT6                                                          
         LA    R0,1                YES - TEST SECOND PASS DONE                  
         CR    RE,R0                                                            
         BNH   EIAC                                                             
         LR    RE,R0                                                            
         B     VALACT2                                                          
VALACT6  EX    RE,*+8              MATCH INPUT TO TABLE                         
         B     *+10                                                             
         CLC   FLD(0),ACTNAME                                                   
         BE    *+12                                                             
         LA    R1,ACTTABL(R1)                                                   
         B     VALACT4                                                          
         TM    ACTINDS,ACTIDDS                                                  
         BZ    *+12                                                             
         TM    TERMSTAT,TSTATDDS                                                
         BZ    EIAC                                                             
         MVC   RUNACT,ACTNAME      DISPLAY FULL ACTION NAME                     
         MVC   ACTION,ACTTYPE                                                   
         MVC   OPTR,ACTOPTR                                                     
         MVC   OPTX,ACTOPTX                                                     
VALACTX  DS    0H                                                               
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*                                                                               
VALOPT   MVI   HELP,2                                                           
         XC    OPTI,OPTI           CLEAR INPUT OPTIONS BITS                     
         GOTO1 FVAL,RUNOPTH                                                     
         BNE   VALOPT2                                                          
         CLI   ACTION,ACTTCLR      TEST CLEAR                                   
         BNE   VALOPT22                                                         
         B     EROM                AT LEAST ONE FILTER REQUIRED                 
*                                                                               
*                                  SCAN THE INPUT OPTIONS FIELD                 
VALOPT2  GOTO1 VSCANNER,DMCB,FLDH,(6,SCANBLK),C',=,='                           
         MVC   SCANLIN,4(R1)       SAVE N'SCANNER LINES                         
         LA    R2,SCANBLK-L'SCANBLK                                             
*                                                                               
VALOPT4  ZIC   R1,FNDX             INCREMENT FIELD INDEX BY ONE                 
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R2,L'SCANBLK(R2)                                                 
         CLC   FNDX,SCANLIN        COMPARE FIELD INDEX TO N'FIELDS              
         BH    VALOPT22                                                         
*                                                                               
         CLI   0(R2),0                                                          
         BE    EIIF                                                             
         CLI   0(R2),L'OPTNAME                                                  
         BH    EIIF                                                             
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         L     R3,AOPTTAB                                                       
         USING OPTTABD,R3          R3=A(OPTIONS TABLE)                          
*                                                                               
VALOPT6  CLI   OPTNAME,0           TEST E-O-T                                   
         BE    EIKW                                                             
         CLI   OPTSHRT,C' '        TEST IF OPTION HAS A SHORT NAME              
         BE    VALOPT10                                                         
         LA    RE,1                SHORT NAME CAN BE 2 OR 3 BYTES LONG          
         CLI   OPTSHRT+2,C' '                                                   
         BE    *+8                                                              
         LA    RE,2                                                             
         CR    R1,RE               TEST L'KEYWORD = L'INPUT KEYWORD             
         BNE   VALOPT10                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   OPTSHRT(0),12(R2)   MATCH ON OPTION SHORT NAME                   
         BE    VALOPT14                                                         
VALOPT10 CLC   0(1,R2),OPTMINKL                                                 
         BL    VALOPT12                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   OPTNAME(0),12(R2)   MATCH ON FULL OPTION NAME                    
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
         OC    OPTI,OPTOPTB        NO - SET THIS OPTION INPUT                   
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,OPTIADDR                                                    
         LA    R1,RUN(R1)                                                       
         ST    R1,IADDR            SET A(TABLE OR VALIDATION ROUTINE)           
         SR    R1,R1                                                            
         ICM   R1,3,OPTOADDR                                                    
         LA    R1,RUNWRKD(R1)                                                   
         ST    R1,OADDR            SET A(OUTPUT FIELD)                          
*                                                                               
         IC    RF,1(R2)            RF=DATA LENGTH                               
         XC    FLDH,FLDH                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),22(R2)       MOVE IN THE DATA                             
         TM    3(R2),X'80'         TEST FIELD IS NUMERIC                        
         BZ    *+8                                                              
         OI    FLDH+4,X'08'        YES - SET INPUT INDICATOR                    
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
         BAS   RE,60(RF)                                                        
         CLI   FERN,FLDISOK        TEST FOR ERRORS                              
         BE    VALOPT20                                                         
         B     EXIT                YES - ERROR & CURSOR SET SO EXIT             
*                                  PROCESS DATA TABLE                           
VALOPT16 TM    OPTINDS,OPTITAB                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    OPTINDS,OPTIHLP     TEST TABLE HAS HELP HEADER                   
         BZ    *+8                                                              
         LA    RF,60(RF)           YES - BUMP OVER IT                           
         ZIC   R0,0(RF)            R0=L'LHS OF TABLE                            
         ZIC   R1,1(RF)            R1=L'RHS OF TABLE                            
         LA    RF,2(RF)            POINT TO FIRST TABLE ENTRY                   
         AR    R0,R1               R0=L'DATA TABLE                              
         ZIC   RE,FLDH+5           RF=L'DATA                                    
         BCTR  RE,0                                                             
*                                                                               
VALOPT18 CLI   0(RF),0             TEST E-O-T                                   
         BE    EIDV                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RF)        MATCH INPUT WITH TABLE                       
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
VALOPT20 ZIC   R1,OPTOUTDL                                                      
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
         L     R3,AOPTTAB          NO - LOCATE A MISSING OPTION                 
VALOPT24 CLI   OPTNAME,0           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                THAT WON'T DO                                
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
* ROUTINE TO VALIDATE TERMINAL NUMBER                                           
*                                                                               
VALTRM   DC    CL60'terminal number or line/address'                            
         NTR1  ,                                                                
         GOTO1 VTERMVAL,DMCB,FLDH                                               
         ICM   R1,15,4(R1)                                                      
         BZ    EITN                                                             
         USING UTLD,R1                                                          
         OC    TPRNT,TPRNT                                                      
         BNZ   EITN                                                             
         MVC   WORK(L'TNUM),TNUM                                                
VALTRMX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
* ROUTINE TO VALIDATE JOBTYPE                                                   
*                                                                               
VALTYP   DC    CL60'1 chr system ID +  2chr sub-ID/report'                      
         MVC   WORK(L'JOBTYPE),FLD                                              
VALTYPX  BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO VALIDATE SYSTEM                                                    
*                                                                               
VALSYS   DC    CL60'1 chr system ID'                                            
         MVC   WORK(L'JOBSYS),FLD                                               
VALSYSX  BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO VALIDATE SEQ=                                                      
*                                                                               
VALSTNO  DC    CL60'sequence number to start display at'                        
         TM    FLDH+4,X'08'        TEST FIELD NUMERIC                           
         BZ    EFNN                                                             
         CLC   =F'999',8(R2)       TEST NOT > 999                               
         BL    EFNN                                                             
         MVC   WORK(L'STARTSEQ),10(R2)                                          
VALSTNX  BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO VALIDATE CLASS                                                     
*                                                                               
VALCLS   EQU   *                                                                
*&&US*&& DC    CL60'2 chr class prefix'                                         
*&&UK*&& DC    CL60'1 chr class'                                                
         MVC   WORK(L'JOBCLS),FLD                                               
VALCLSX  BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO VALIDATE USERID FILTER                                             
*                                                                               
VALUSER  DC    CL60'userid (sign-on id)'                                        
*                                                                               
         NTR1                                                                   
*                                                                               
         LA    R3,KEY              BUILD ID RECORD KEY                          
         USING CTIKEY,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,SPACES                                                    
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTIKID(0),FLD                                                    
         CLC   CTIKEY,IO           TEST ID RECORD IN CORE                       
         BE    VALUSER1                                                         
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         BNE   EIID                ID RECORD NOT FOUND                          
*                                                                               
VALUSER1 LA    R3,IO+(CTIDATA-CTIREC)                                           
         SR    R0,R0                                                            
VALUSER2 CLI   0(R3),0             LOCATE HEX USER-ID ELEMENT                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VALUSER2                                                         
         MVC   WORK(L'JOBUSER),2(R3)                                            
*                                                                               
VALUSERX B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* ROUTINE TO VALIDATE SUB-ID FILTER                                             
*                                                                               
VALSUBID DC    CL60'2 or 3 chr report-id'                                       
         MVC   WORK(L'JOBSUBID),FLD                                             
VALSUBIX BR    RE                                                               
         EJECT                                                                  
* PROCESS INPUT ACTION                                                          
*                                                                               
GO       L     R1,SRPAUTL                                                       
         LA    R0,SRPAGENO         READ & LOCK SAVE PAGE                        
         SLL   R0,32-8                                                          
         ICM   R0,3,TNUM-UTLD(R1)                                               
         ST    R0,TWAPAGE                                                       
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,TWAPAGE,SRPATIA             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TWAREAD,C'Y'                                                     
*                                                                               
         LA    R5,SR$RUN                                                        
         USING RUNSAVED,R5         R5=A($RUN SAVE AREA)                         
*                                                                               
         CLI   ACTION,ACTTSUM      TEST SUMMARY                                 
         BE    SUMMARY             YEP                                          
*                                                                               
         LA    R1,RUNMSGH          SET XMIT BITS IN ALL TWA FIELDS              
         SR    RE,RE                                                            
         LA    RF,RUNXXXH-1                                                     
         OI    6(R1),X'80'                                                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,*-8                                                        
*                                                                               
         CLI   ACTION,ACTTCLR      TEST CLEAR                                   
         BNE   GO2                 NO                                           
*                                                                               
         GOTO1 DIS,DISFADTA+DISFASEQ    REDISPLAY                               
         BNE   ERROR4                                                           
         B     CHA                                                              
*                                                                               
GO2      GOTO1 DIS,DISFADTA+DISFASEQ    REDISPLAY                               
         BNE   ERROR4                                                           
         LA    R1,RUNACTH          SET FADR FOR ERROR/EXIT                      
         ST    R1,FADR                                                          
         OC    SEQDIS,SEQDIS       TEST ANYTHING DISPLAYED                      
         BZ    ENTD                NO                                           
         XC    FLD(L'RUNMSG),FLD   FORMAT MESSAGE IN FLD FOR EXIT               
         MVC   FLD(L'DISMSG),DISMSG                                             
         LH    R0,SEQLO                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FLD+12(3),DUB                                                    
         LH    R0,SEQHI                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FLD+19(3),DUB                                                    
         LH    R0,SEQNUM                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FLD+26(3),DUB                                                    
         MVC   FLD+L'DISMSG(L'NXTMSG),NXTMSG                                    
         CLI   RUNOPTH+5,0                                                      
         BE    GOX                                                              
         LA    R1,RUNTABH                                                       
         ST    R1,FADR                                                          
         B     GOX                                                              
*                                                                               
*                                                                               
GOX      DS   0H                                                                
         MVC   RUNSLO,SEQLO        PUT IN S/R SAVED STORAGE                     
         MVC   RUNSHI,SEQHI                                                     
         MVC   RUNSNUM,SEQNUM                                                   
         MVC   RUNSTSEQ,STARTSEQ                                                
         B     ERROR4              GO SET MESSAGE & CURSOR                      
         EJECT                                                                  
***********************************************************************         
* DISPLAY JOB QUEUE                                                   *         
*                                                                     *         
* NTRY - R1=DISPLAY FLAGS (SEE EQUATED VALUES FOR DISFLAG)            *         
***********************************************************************         
         SPACE 1                                                                
DIS      NTR1  ,                                                                
         STC   R1,DISFLAG          SAVE FORMAT/DISPLAY FLAG                     
         LA    R1,RUNLINH                                                       
         LA    RE,RUNLINEL                                                      
         LA    RF,RUNXXXH-1                                                     
         USING RUNLINED,R1                                                      
DIS2     TM    DISFLAG,DISFASEQ    CLEAR & PROTECT LINE ACTION FIELDS           
         BZ    *+10                                                             
         XC    RUNLACT,RUNLACT                                                  
         OI    RUNLACTH+1,X'20'                                                 
         BXLE  R1,RE,DIS2                                                       
         DROP  R1                                                               
         XC    RUNSTABN,RUNSTABN   CLEAR SAVE DISPLAY TABLE                     
         XC    RUNSTAB(RUNSTABL),RUNSTAB                                        
         XC    SEQS,SEQS           CLEAR DISPLAY SEQUENCE NUMBERS               
         XC    TABCOUNT,TABCOUNT   CLEAR TABLE ENTRY COUNTER                    
*                                                                               
         L     R2,AJOBTAB                                                       
         USING JOBTABD,R2          R2=A(JOB SCHEDULER TABLE)                    
         L     R3,AMYJTAB                                                       
         USING MYJOBTBD,R3         R3=A(MY JOB SCHEDULER TABLE)                 
DIS4     TM    JOBSTAT,JOBSUSE     TEST USED ENTRY                              
         BZ    DISNXT                                                           
         OC    JOBTERM,JOBTERM     THERE MUST BE A TERMINAL NUMBER. . .         
         BZ    DISNXT                                                           
         OC    JOBPQKEY,JOBPQKEY   . . . AND REPORT ID. . .                     
         BZ    DISNXT                                                           
         OC    JOBPQCIA,JOBPQCIA   . . . AND C/I ADDRESS                        
         BZ    DISNXT                                                           
*                                                                               
         OC    TERMNUM,TERMNUM     TERMINAL FILTER                              
         BZ    *+14                                                             
         CLC   JOBTERM,TERMNUM                                                  
         BNE   DISNXT                                                           
*                                                                               
         CLI   JOBSYS,0            SYSTEM FILTER                                
         BE    *+14                                                             
         CLC   JOBSYS,JOBMVSID+4                                                
         BNE   DISNXT                                                           
*                                                                               
         OC    JOBTYPE,JOBTYPE     JOBTYPE FILTER (SYS/SUB ID)                  
         BZ    *+14                                                             
         CLC   JOBTYPE,JOBMVSID+4                                               
         BNE   DISNXT                                                           
*                                                                               
         TM    JOBJESNO,X'80'      IS THIS A MONSOON JOB?                       
         BZ    DIS5                NO                                           
         OC    JOBCLS,JOBCLS       CLASS FILTER                                 
         BZ    DIS5                                                             
*&&US*&& LA    RF,2                LENGTH OF CLASS CODE                         
*&&UK*&& LA    RF,1                LENGTH OF CLASS CODE                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   JOBCLS(0),JOBCLASS                                               
         BNE   DISNXT                                                           
*                                                                               
DIS5     OC    JOBUSER,JOBUSER     USERID FILTER                                
         BZ    *+14                                                             
         CLC   JOBUSER,JOBPQUSR                                                 
         BNE   DISNXT                                                           
*                                                                               
         OC    JOBSUBID,JOBSUBID   SUB-ID FILTER                                
         BZ    *+14                                                             
         CLC   JOBSUBID,JOBPQSUB                                                
         BNE   DISNXT                                                           
*                                                                               
*                                  SAVE ENTRY IN MYJOBTAB                       
         MVC   MYJCLASS,JOBCLASS                                                
         MVC   MYJTIME,JOBSTIME                                                 
         TM    JOBSTAT,JOBSTYPB    TYPE B?                                      
         BNO   *+8                                                              
         MVI   MYJTYPB,X'FF'                                                    
         MVC   MYJTERM,JOBTERM                                                  
         MVC   MYJPQKEY,JOBPQKEY                                                
         MVC   MYJPQCIA,JOBPQCIA                                                
         MVC   MYJPQID(1),JOBPQID                                               
         MVC   MYJPRTY,JOBPRTY                                                  
         MVC   MYJJESNO,JOBJESNO                                                
         MVC   MYJMVSID,JOBMVSID                                                
*                                                                               
         AH    R3,=Y(MYJOBTL)      NEXT ENTRY                                   
         L     R1,TABCOUNT                                                      
         LA    R1,1(R1)                                                         
         ST    R1,TABCOUNT                                                      
*                                                                               
DISNXT   AH    R2,JOBTABL                                                       
         C     R2,JOBTABX                                                       
         BNH   DIS4                                                             
         DROP  R2,R3                                                            
*                                                                               
*               SORT ENTRIES IN MYJOBTAB BY CREATION TIME                       
*                                                                               
         L     R2,TABCOUNT                                                      
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,AMYJTAB,(R2),MYJOBTL,L'MYJTIME,               X        
               MYJTIME-MYJOBTBD                                                 
*                                                                               
*             LOOK THROUGH PRINT QUEUE INDEX FOR STATUS                         
*                                                                               
*                                  GET LIST OF VALID PRINT QUEUES               
         GOTO1 VDATAMGR,DMCB,=C'GLIST',=C'PRTQUE',WORK,0,CIREC                  
         ICM   RE,15,WORK+32                                                    
         LA    RE,8(RE)                                                         
         ST    RE,AFPRTQUE         A(FIRST ENTRY IN PQ LIST)                    
         ST    RE,ACPRTQUE         A(CURRENT ENTRY)                             
         MVC   PRTQN(4),=C'PRTQ'                                                
         MVC   PRTQN+4(1),1(RE)    EBCDIC PQ# OF FIRST PRINT QUEUE              
*                                                                               
DIS10    DS    0H                                                               
         LA    R3,MYPQINDX                                                      
         USING PQRECD,R3                                                        
         XC    SVPQINDX,SVPQINDX                                                
         XC    MYPQINDX,MYPQINDX     START FROM BEGINNING OF INDEX              
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'BUFFER'),PRTQN,0,0,CIREC                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CIDATA,CIREC+12     CIDATA IS HERE                               
         LH    R0,CICITOT          NUMBER OF PART 1 CI'S. . .                   
         SH    R0,CICINDX          . . .MINUS INDEX ENTRIES. . .                
         STH   R0,CIMAXRPT         . . .EQUALS NUMBER OF REPORTS                
         XC    INDCOUNT,INDCOUNT                                                
         B     DIS17                                                            
*                                                                               
DIS15    DS    0H                                                               
         CLC   MYPQINDX+5(2),SVPQINDX+5    NEXT NUMBER LOWER THAN               
         BL    DISERR2                     PREVIOUS NUMBER                      
*                                                                               
         CLC   MYPQINDX,SVPQINDX   SAME ENTRY I JUST HAD?                       
         BE    DISERR1                                                          
         MVC   SVPQINDX,MYPQINDX                                                
*                                                                               
         OC    MYPQINDX,MYPQINDX   MY INDEX SHOULD NOT BE ZERO                  
         BZ    DISERR1             (UNLESS IT'S THE FIRST CALL)                 
*                                                                               
         LH    R1,INDCOUNT                                                      
         LA    R1,1(R1)                                                         
         LH    R0,CIMAXRPT         DIE IF MORE INDEX CALLS THAN RPTS            
         CR    R1,R0                                                            
         BH    DISERR1                                                          
         STH   R1,INDCOUNT                                                      
         B     DIS17                                                            
*                                                                               
DISERR1  DS    0H                  MAX OUT NUMBER OF REPORTS                    
         XC    FLD(L'RUNMSG),FLD   FORMAT MESSAGE IN FLD FOR EXIT               
         MVC   FLD(L'ANNMSG1),ANNMSG1                                           
         B     XNO                                                              
*                                                                               
DISERR2  DS    0H               GOT REPORT NUMBER LOWER THAN PREVIOUS           
         XC    FLD(L'RUNMSG),FLD   FORMAT MESSAGE IN FLD FOR EXIT               
         MVC   FLD(L'ANNMSG2),ANNMSG2                                           
         B     XNO                                                              
*                                                                               
DIS17    DS    0H                                                               
         OI    UKFLAG-UKRECD(R3),UKFLTMP   PASS BACK TEMP EMTRIES               
         GOTO1 VDATAMGR,DMCB,(X'08',=C'INDEX'),PRTQN,(R3),R,CIREC               
         TM    DMCB+8,X'80'        END OF FILE?                                 
         BO    DIS50               YES                                          
*                                                                               
*                                                                               
*              SEARCH FOR SAME REPORT IN MYJOBTAB                               
*                                                                               
         L     R2,TABCOUNT                                                      
         LA    R6,MYJTIME-MYJOBTBD                                              
         L     RF,=V(BINSRCH)                                                   
         A     RF,RELO                                                          
         XC    WORK,WORK                                                        
         MVC   WORK+MYJTIME-MYJOBTBD(L'MYJTIME),PQAGELT                         
         GOTO1 (RF),DMCB,(0,WORK),AMYJTAB,(R2),MYJOBTL,                X        
               ((R6),L'MYJTIME),1000                                            
         CLI   DMCB,X'01'                                                       
         BE    DIS15               NO MATCH FOUND                               
         L     R2,0(R1)            A(MATCH)                                     
         USING MYJOBTBD,R2         MATCHING ENTRY IN MY JOB TABLE               
         CLC   MYJPQKEY,PQKEY      SAME KEY?                                    
         BE    DIS30               YES                                          
*                                                                               
*  NEED TO CHECK SURROUNDING ENTRIES IN CASE MORE THAN ONE MATCH                
*                                                                               
DIS20    SH    R2,=Y(MYJOBTL)      BACK UP TO FIRST MATCHING TIME               
         CLC   WORK+MYJTIME-MYJOBTBD(L'MYJTIME),MYJTIME                         
         BNE   DIS25                                                            
         B     DIS20                                                            
DIS25    AH    R2,=Y(MYJOBTL)      NEED TO GET NEXT                             
         CLC   WORK+MYJTIME-MYJOBTBD(L'MYJTIME),MYJTIME                         
         BNE   DIS15                                                            
         CLC   MYJPQKEY,PQKEY      SAME KEY?                                    
         BNE   DIS25                                                            
*                                                                               
DIS30    MVI   JOBFLAG,0                                                        
         TM    PQSTAT,PQSTTE       ESTABLISH JOB STATUS                         
         BZ    DIS32                                                            
         TM    PQATTB,PQATJOBI+PQATJOBO                                         
         BNO   DIS32                                                            
         MVI   JOBFLAG,JOBFRUN     JOB IS RUNNING                               
         B     DIS40                                                            
*                                                                               
DIS32    TM    PQSTAT,PQSTDEAD                                                  
         BNZ   DIS34                                                            
         TM    PQSTAT,PQSTAC                                                    
         BZ    DIS38                                                            
         TM    PQATTB,PQATJOBI                                                  
         BZ    DIS34                                                            
         MVI   JOBFLAG,JOBFSUB     JOB HAS BEEN SUBMITTED                       
         B     DIS40                                                            
*                                                                               
DIS34    TM    PQATTB,PQATJOBO                                                  
         BZ    DIS38                                                            
         TM    PQATTB,PQATERR                                                   
         BZ    DIS36                                                            
         MVI   JOBFLAG,JOBFERR     JOB IN ERROR                                 
         B     DIS40                                                            
DIS36    MVI   JOBFLAG,JOBFAVA     JOB IS READY                                 
         B     DIS40                                                            
*                                                                               
DIS38    MVI   JOBFLAG,JOBFUNK     JOB IN UNKNOWN STATUS                        
*                                                                               
DIS40    MVC   MYJOBFL,JOBFLAG     COMPLETE MYJOBTAB ENTRY                      
         MVC   MYJPQSTA,PQSTAT     PQ STAT                                      
         MVC   MYJPQATB,PQATTB     PQ ATTRIBUTE                                 
         MVC   MYJOBTYP,PQTYPE     PQ TYPE                                      
         B     DIS15                                                            
*                                                                               
DIS50    DS    0H                                                               
         LA    RE,CIREC                                                         
         XCEFL (RE),14336          CLEAR PQ INDEX BUFFER                        
         L     RE,ACPRTQUE                                                      
         LA    RE,8(RE)            BUMP TO NEXT PRINT QUEUE                     
         CLI   0(RE),0                                                          
         BNE   DIS55                                                            
         B     DIS60                                                            
*                                                                               
DIS55    ST    RE,ACPRTQUE                                                      
         MVC   PRTQN+4(1),1(RE)    SAVE EBCDIC PQ#                              
         B     DIS10                                                            
*                                                                               
DIS60    DS    0H                                                               
*                                                                               
* FIND WHERE TO START AND DISPLAY PAGE                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,RUNSTNUM       TEST DISPLAY LAST TIME                       
         LA    RE,1(RE)                                                         
         CLM   RE,3,TRANSNUM                                                    
         BNE   DIS77                                                            
*                                                                               
         CLI   PFKEY,12                                                         
         BNH   DIS69                                                            
         ZIC   R1,PFKEY                                                         
         SH    R1,=H'12'                                                        
         STC   R1,PFKEY                                                         
*                                                                               
DIS69    OC    RUNSTSEQ,RUNSTSEQ   IF THERE WAS SE= OPTION LAST TIME            
         BZ    DIS69A              CLEAR OLD LO NUMBER TO REDISPLAY             
         XC    RUNSLO,RUNSLO       FROM TOP IF REMOVED OPTION                   
DIS69A   OC    STARTSEQ,STARTSEQ   IF THERE'S A SE= OPTION                      
         BZ    DIS70               CLEAR OLD LO NUMBER                          
         XC    RUNSLO,RUNSLO                                                    
*                                                                               
*                                                                               
DIS70    CLI   PFKEY,0             ENTER = REFRESH                              
         BNE   DIS71                                                            
         MVC   STARTPF,RUNSLO                                                   
         B     DIS77                                                            
DIS71    CLI   PFKEY,5             PAGE UP                                      
         BNE   DIS72                                                            
         LH    R1,RUNSLO                                                        
         SH    R1,=H'18'                                                        
         CH    R1,=H'0'                                                         
         BNH   DIS77                                                            
         STH   R1,STARTPF                                                       
         B     DIS77                                                            
DIS72    CLI   PFKEY,6             PAGE DOWN                                    
         BNE   DIS74                                                            
         LH    R1,RUNSHI                                                        
         LA    R1,1(R1)                                                         
         STH   R1,STARTPF                                                       
         B     DIS77                                                            
DIS74    CLI   PFKEY,7             TOP                                          
         BNE   DIS75                                                            
         B     DIS77                                                            
DIS75    CLI   PFKEY,8             BOTTOM                                       
         BNE   DIS77                                                            
         L     R1,TABCOUNT                                                      
         SH    R1,=H'17'                                                        
         CH    R1,=H'0'                                                         
         BNH   DIS77                                                            
         STH   R1,STARTPF                                                       
         B     DIS77                                                            
*                                                                               
DIS77    DS    0H                                                               
         CLC   STARTPF,TABCOUNT+2     MORE THAN IN TABLE?                       
         BNH   *+10                                                             
         XC    STARTPF,STARTPF    THEN START AT TOP                             
*                                                                               
         XC    LISTCT,LISTCT                                                    
         L     R2,AMYJTAB          A(MYJOBTAB)                                  
         USING MYJOBTBD,R2                                                      
*                                                                               
DIS80    DS    0H                                                               
         OC    MYJCLASS(MYJOBTL),MYJCLASS                                       
         BZ    DISX                                                             
         MVC   DUB(1),JOBFILT      APPLY STATUS FILTER                          
         NC    DUB(1),MYJOBFL                                                   
         CLC   DUB(1),JOBFILT                                                   
         BNE   DIS90                                                            
         LH    RF,SEQNUM           BUMP SEQUENCE NUMBER                         
         LA    RF,1(RF)                                                         
         STH   RF,SEQNUM                                                        
         CH    RF,STARTSEQ         STARTSEQ= OPTION MAY BE ACTIVE               
         BL    DIS90                                                            
         CH    RF,STARTPF          WHERE TO START FOR PF SCROLLING              
         BL    DIS90                                                            
         BAS   RE,FORMAT           FORMAT DISPLAY LINE                          
         BNE   DIS90                                                            
         OC    SEQLO,SEQLO         SET DISPLAY SEQUENCE NUMBERS                 
         BNZ   *+10                                                             
         MVC   SEQLO,SEQNUM                                                     
         MVC   SEQHI,SEQNUM                                                     
*                                                                               
DIS90    DS    0H                                                               
         ZIC   R1,LISTCT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,LISTCT                                                        
         CH    R1,=H'18'                                                        
         BH    DISX                                                             
         AH    R2,=Y(MYJOBTL)      NEXT ENTRY                                   
         B     DIS80                                                            
*                                                                               
DISX     DS    0H                                                               
         B     XYES                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT A DISPLAY LINE                                    *         
*                                                                     *         
* NTRY - R4=A(JOB TABLE ENTRY)                                        *         
* NTRY - R2=A(MY JOB TABLE ENTRY)                                     *         
*                                                                     *         
* EXIT - CC=NOT EQUAL IF JOB WON'T FIT ON SCREEN                      *         
***********************************************************************         
         SPACE 1                                                                
FORMAT   NTR1  ,                                                                
         USING MYJOBTBD,R2                                                      
         LH    R1,SEQDIS                                                        
         LA    RE,1(R1)                                                         
         LA    R0,RUNSMAXN                                                      
         CR    RE,R0               WILL THIS FIT ON SCREEN                      
         BH    FORMATN             NO                                           
         STH   RE,SEQDIS                                                        
         MH    R1,=Y(RUNLINEL)                                                  
         LA    R3,RUNLINH(R1)      POINT TO LEFT SIDE OF SCREEN                 
         USING RUNLINED,R3         R3=A(TWA DISPLAY LINE)                       
*        NI    RUNLACTH+1,X'FF'-X'20'                                           
         TM    DISFLAG,DISFADTA    TEST TABLE BUILD ONLY                        
         BZ    FORMATY                                                          
*                                                                               
         SR    R0,R0               FORMAT SUBMIT TIME                           
         ICM   R0,3,MYJTIME        BINARY UNIT TIME (4/3 SECOND)                
         SLL   R0,2                TIMES 4                                      
         SRDL  R0,32               PREPARE FOR DIVIDE                           
         D     R0,=F'3'            R1 = TIME IN SECONDS ONLY                    
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RUNLSS,DUB                                                       
         MVI   RUNLMSD,RUNLMSDQ                                                 
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RUNLMM,DUB                                                       
         MVI   RUNLHMD,RUNLHMDQ                                                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RUNLHH,DUB                                                       
*                                                                               
         TM    MYJJESNO,X'80'      IS THIS A MONSOON JOB?                       
         BZ    *+14                NO                                           
         MVC   RUNLCLS,MYJCLASS    SOON CLASS (E.G., A3SOON)                    
         B     FORMAT0                                                          
*                                                                               
         MVC   RUNLCLS(3),=C'JOB'  EDIT JES JOB NUMBER (JOBNNNN)                
         SR    R0,R0                                                            
         ICM   R0,3,MYJJESNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RUNLCLS+3(4),DUB                                                 
*                                                                               
FORMAT0  MVC   RUNLNAME,MYJMVSID   EDIT MVS JOB ID (UUUUSSPP)                   
*                                                                               
         MVC   RUNLPRTY,MYJPRTY    JOB PRIORITY                                 
*                                                                               
         TM    MYJOBTYP,X'20'      LUNATIC REPORT?                              
         BNO   *+8                                                              
         MVI   RUNLUNA,C'*'                                                     
*                                                                               
         CLI   MYJTYPB,X'FF'       TYPE B?                                      
         BNE   *+8                                                              
         MVI   RUNLTYPB,C'*'                                                    
*                                  C/I ADDRESS                                  
         GOTO1 VHEXOUT,DMCB,MYJPQCIA,RUNLCIA,2                                  
*                                                                               
         SR    R0,R0               EDIT TERMINAL (NNNN LLLL/CUDV)               
         ICM   R0,3,MYJTERM                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RUNLTNUM,DUB                                                     
         L     RE,VUTL                                                          
         LH    R1,0(RE)                                                         
         BCTR  R0,0                                                             
         MR    R0,R0                                                            
         LA    RE,6(R1,RE)                                                      
         GETLA (RE),DUB,ADDR=ALPHA                                              
         MVC   RUNLTSYM,DUB                                                     
*                                                                               
         LA    R1,KEY              EDIT USER-ID                                 
         USING CTIKEY,R1                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),MYJPQUSR                                             
         CLC   CTIKEY,IO           TEST ID RECORD IN CORE                       
         BE    FORMAT1                                                          
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0             TEST FOR ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
FORMAT1  LA    R1,IO+(CTIDATA-CTIREC)                                           
         SR    R0,R0                                                            
FORMAT2  CLI   0(R1),0             LOCATE ALPHA USER-ID ELEMENT                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     FORMAT2                                                          
         MVC   RUNLUSER,2(R1)                                                   
*                                                                               
         MVC   RUNLREPT,MYJPQSUB                                                
         MVI   RUNLRSD,RUNLRSDQ                                                 
         SR    R0,R0                                                            
         ICM   R0,3,MYJPQSEQ                                                    
         EDIT  (R0),(5,RUNLSEQN),ALIGN=LEFT                                     
         DROP  R1                                                               
*                                                                               
         L     R1,ASTATAB                                                       
         LA    R1,2(R1)                                                         
FORMAT3  CLI   0(R1),0             TEST E-O-T                                   
         BNE   *+12                                                             
         LA    R1,=CL3'UNK'                                                     
         B     FORMAT4                                                          
         CLC   MYJOBFL,L'STATAB-1(R1)                                           
         BE    FORMAT4                                                          
         LA    R1,L'STATAB(R1)                                                  
         B     FORMAT3                                                          
FORMAT4  MVC   RUNLSTAT,0(R1)                                                   
*                                                                               
         MVC   RUNLPQNO,MYJPQID    PRINT QUEUE                                  
*                                                                               
FORMAT8  TM    DISFLAG,DISFASEQ    TEST FORMAT SEQUENCE NUMBERS                 
         BZ    FORMATY                                                          
         LH    R0,SEQNUM                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RUNLACT(3),DUB                                                   
*        MVI   RUNLACT,C'*'                                                     
         B     FORMATY                                                          
*                                                                               
FORMATN  LTR   RB,RB                                                            
         B     EXIT                                                             
FORMATY  CR    RB,RB                                                            
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
* CHANGE JOB QUEUE                                                              
*                                                                               
CHA      DS    0H                                                               
*        XC    SEQCHA,SEQCHA                                                    
         XC    CHANUM,CHANUM                                                    
         L     R2,AMYJTAB          A(MYJOBTAB)                                  
         USING MYJOBTBD,R2                                                      
*                                                                               
CHA2     DS    0H                                                               
         OC    MYJCLASS(MYJOBTL),MYJCLASS                                       
         BZ    CHAX                                                             
         MVC   WORK(2),MYJPQCIA                                                 
         L     R1,AJOBTAB                                                       
         LH    RE,JOBTABL                                                       
         L     RF,JOBTABX                                                       
         USING JOBTABD,R1          R1=A(JOB SCHEDULER TABLE)                    
CHA5     TM    JOBSTAT,JOBSUSE     TEST USED ENTRY                              
         BZ    *+14                                                             
         CLC   JOBPQCIA,WORK       LOCATE ENTRY IN JOBTAB FOR C/I ADDR          
         BE    CHA5A                                                            
         BXLE  R1,RE,CHA5                                                       
*        DC    H'0'                                                             
         B     CHA2                                                             
         DROP  R1                                                               
CHA5A    BAS   RE,CHADEL                                                        
*                                                                               
CHA6     LH    R1,CHANUM           BUMP CHANGE COUNT                            
         LA    R1,1(R1)                                                         
         STH   R1,CHANUM                                                        
CHA8     AH    R2,=Y(MYJOBTL)      NEXT ENTRY                                   
         B     CHA2                                                             
*                                                                               
CHAX     OC    CHANUM,CHANUM       TEST ANY CHANGES MADE                        
         BZ    GO2                 NO - RE-DISPLAY QUEUE                        
         XC    FLD(L'RUNMSG),FLD   SET MESSAGE & FADR FOR EXIT                  
         LA    R7,FLD                                                           
         EDIT  (B2,CHANUM),(4,0(R7)),ZERO=NOBLANK,ALIGN=LEFT                    
         AR    R7,R0                                                            
         MVC   0(L'CHAMSG,R7),CHAMSG                                            
         LA    R1,RUNACTH                                                       
         ST    R1,FADR                                                          
         XC    TRANSNUM,TRANSNUM   FORCE DISPLAY NEXT TIME                      
         B     GOX                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE JOB SCHEDULER TABLE ENTRY & SET STATUS OF JOB IN  *         
* TERMINAL QUEUE TO COMPLETED/ERROR                                   *         
*                                                                     *         
* NTRY - R1=A(JOB SCHEDULER TABLE ENTRY)                              *         
***********************************************************************         
         SPACE 1                                                                
CHADEL   NTR1                                                                   
         LR    R2,R1                                                            
         USING JOBTABD,R2          R2=A(JOB SCHEDULER TABLE ENTRY)              
         MVI   JOBSTAT,JOBSAVA     FREE JOB TABLE ENTRY                         
         OI    CHATYPE,CHATDEL     SET CHANGE TO JOBTAB MADE                    
         LA    R0,SRPAGENO         READ SPECIAL S/R SAVE PAGE                   
         SLL   R0,32-8                                                          
         ICM   R0,3,JOBTERM                                                     
         ST    R0,DUB              SAVE PAGE FOR WRITE                          
         L     R6,SRPATIA                                                       
         L     RE,SRPAUTL                                                       
         CLC   TNUM-UTLD(L'TNUM,RE),JOBTERM                                     
         BE    CHADEL2                                                          
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R0),CIREC                  
         LA    R6,CIREC                                                         
CHADEL2  SR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ                                                    
         BZ    CHADELX                                                          
         LA    R1,SRJOBQ                                                        
         USING SRJOBQ,R1                                                        
CHADEL4  CLI   SRJOBSTA,SRJOBPUT   TEST SUBMITTED                               
         BNE   CHADEL6                                                          
         CLC   JOBPQCIA,SRJOBCIA   MATCH CIADDR & USER-ID/SUB-ID                
         BNE   CHADEL6                                                          
         CLC   JOBPQUSR(JOBPQSEQ-JOBPQUSR),SRJOBUSR                             
         BNE   CHADEL6                                                          
         OI    SRJOBSTA,SRJOBOUT+SRJOBINV                                       
         L     R0,DUB                                                           
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(R0),SRSD                            
         L     RE,SRPAUTL                                                       
         CLC   TNUM-UTLD(L'TNUM,RE),JOBTERM                                     
         BE    CHADELX                                                          
         GOTO1 (RF),(R1),DMUNLK,TEMPSTR                                         
         B     CHADELX                                                          
CHADEL6  LA    R1,SRJOBQLN(R1)                                                  
         BCT   R0,CHADEL4                                                       
CHADELX  DS    0H                                                               
         XIT1                                                                   
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SUMMARY SCREEN                                             
***********************************************************************         
SUMMARY  DS    0H                                                               
         GOTO1 DIS,DISFADTA+DISFASEQ         CREATE MYJOBTAB                    
         BNE   ERROR4                                                           
*                                                                               
         GOTO1 VCALLOV,DMCB,RUNHEDH,X'D90158FD',0                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
*  SORT ON CLASS, TYPE B, AND TIME                                              
*                                                                               
         L     R2,TABCOUNT                                                      
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,AMYJTAB,(R2),MYJOBTL,L'MYJCLASS+L'MYJTYPB,    X        
               MYJCLASS-MYJOBTBD                                                
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'01',0)                                          
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   CURTIME,FATIME      CURRENT BINARY TIME                          
         DROP  R1                                                               
*                                                                               
* LOOP THROUGH TABLE TO BUILD TOTALS                                            
*                                                                               
         XC    SCRFULL,SCRFULL                                                  
         XC    YESMORE,YESMORE                                                  
         L     R2,AMYJTAB          A(MYJOBTAB)                                  
         USING MYJOBTBD,R2                                                      
         LA    R3,RNSLIN1H         FIRST FIELD                                  
         USING SUMLINED,R3                                                      
*                                                                               
SUM00    DS    0H                                                               
         MVC   OLDCLASS,MYJCLASS                                                
         MVC   OLDTYPEB,MYJTYPB                                                 
         OC    MYJCLASS(MYJOBTL),MYJCLASS                                       
         BZ    SUMX                                                             
*                                                                               
         TM    MYJPQSTA,PQSTTE       ESTABLISH JOB STATUS                       
         BZ    SUM6                                                             
         TM    MYJPQATB,PQATJOBI+PQATJOBO                                       
         BNO   SUM6                                                             
         L     R1,NUMRUN           #RUNNING                                     
         LA    R1,1(R1)                                                         
         ST    R1,NUMRUN                                                        
         B     SUM50                                                            
*                                                                               
SUM6     TM    MYJPQSTA,PQSTDEAD                                                
         BNZ   SUM8                                                             
         TM    MYJPQSTA,PQSTAC                                                  
         BZ    SUM10                                                            
         TM    MYJPQATB,PQATJOBI                                                
         BZ    SUM8                                                             
         L     R1,NUMSUB           #SUBMITTED                                   
         LA    R1,1(R1)                                                         
         ST    R1,NUMSUB                                                        
         B     SUM40                                                            
*                                                                               
SUM8     TM    MYJPQATB,PQATJOBO                                                
         BZ    SUM10                                                            
         TM    MYJPQATB,PQATERR                                                 
         BZ    SUM9                                                             
         L     R1,NUMERR           #ERROR                                       
         LA    R1,1(R1)                                                         
         ST    R1,NUMERR                                                        
         B     SUM50                                                            
SUM9     DS    0H                  #READY                                       
         L     R1,NUMRDY                                                        
         LA    R1,1(R1)                                                         
         ST    R1,NUMRDY                                                        
         B     SUM50                                                            
*                                                                               
SUM10    DS    0H                  #UNKNOWN                                     
         L     R1,NUMUNK                                                        
         LA    R1,1(R1)                                                         
         ST    R1,NUMUNK                                                        
         B     SUM50                                                            
*                                                                               
SUM40    DS    0H                                                               
         SR    R0,R0               FORMAT SUBMIT TIME                           
         ICM   R0,3,MYJTIME        BINARY UNIT TIME (4/3 SECOND)                
         SLL   R0,2                TIMES 4                                      
         SRDL  R0,32               PREPARE FOR DIVIDE                           
         D     R0,=F'3'            R1 = TIME IN SECONDS ONLY                    
         L     R0,CURTIME                                                       
         SR    R0,R1               SUBTRACT SUBMIT FROM CURRENT TIME            
         A     R0,TIMECNT          KEEP TOTAL                                   
         ST    R0,TIMECNT                                                       
*                                                                               
SUM50    DS    0H                                                               
         AH    R2,=Y(MYJOBTL)      NEXT ENTRY                                   
         CLC   MYJCLASS,OLDCLASS                                                
         BNE   SUM60                                                            
         CLC   MYJTYPB,OLDTYPEB    TYPE B?                                      
         BE    SUM00                                                            
SUM60    DS    0H                                                               
         CLI   OLDTYPEB,X'FF'                                                   
         BNE   SUM65                                                            
         L     R1,TOTSUBB          UPDATE TYPE B TOTALS                         
         A     R1,NUMSUB                                                        
         ST    R1,TOTSUBB                                                       
         L     R1,AVETIMEB         UPDATE TYPE B TOTALS                         
         A     R1,TIMECNT                                                       
         ST    R1,AVETIMEB                                                      
         B     SUM67                                                            
SUM65    L     R1,TOTSUB           UPDATE TOTALS                                
         A     R1,NUMSUB                                                        
         ST    R1,TOTSUB                                                        
         L     R1,AVETIME          UPDATE TIME TOTALS                           
         A     R1,TIMECNT                                                       
         ST    R1,AVETIME                                                       
         B     SUM67                                                            
SUM67    CLI   SCRFULL,C'Y'                                                     
         BNE   SUM70                                                            
         MVI   YESMORE,C'Y'                                                     
         XC    NUMSUB,NUMSUB                                                    
         XC    NUMRUN,NUMRUN                                                    
         XC    NUMRDY,NUMRDY                                                    
         XC    NUMERR,NUMERR                                                    
         XC    NUMUNK,NUMUNK                                                    
         B     SUM00                                                            
*                                  DISPLAY LINE                                 
SUM70    OI    5(R3),X'80'                                                      
         LA    R3,8(R3)            BUMP PAST HEADER                             
         MVC   SUMLCLS,OLDCLASS                                                 
         OC    OLDTYPEB,OLDTYPEB                                                
         BZ    SUM75                                                            
         MVC   SUMLTYPB,=C'/B'                                                  
SUM75    EDIT  (B4,NUMSUB),(4,SUMLSUB),ZERO=NOBLANK                             
         EDIT  (B4,NUMRUN),(4,SUMLRNG),ZERO=NOBLANK                             
         EDIT  (B4,NUMRDY),(4,SUMLRDY),ZERO=NOBLANK                             
         EDIT  (B4,NUMERR),(4,SUMLERR),ZERO=NOBLANK                             
         EDIT  (B4,NUMUNK),(4,SUMLUNK),ZERO=NOBLANK                             
         OC    TIMECNT,TIMECNT                                                  
         BNZ   SUM76                                                            
         MVC   SUMLHH(4),=C'NONE'                                               
         B     SUM77                                                            
SUM76    L     R1,TIMECNT                                                       
         SR    R0,R0                                                            
         D     R0,NUMSUB                                                        
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SUMLMM,DUB                                                       
         MVI   SUMLCOL,C':'                                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SUMLHH,DUB                                                       
SUM77    XC    NUMSUB,NUMSUB                                                    
         XC    NUMRUN,NUMRUN                                                    
         XC    NUMRDY,NUMRDY                                                    
         XC    NUMERR,NUMERR                                                    
         XC    NUMUNK,NUMUNK                                                    
         XC    TIMECNT,TIMECNT                                                  
*                                                                               
         AH    R3,=Y(SUMLLEN)      NEXT FIELD HEADER                            
         LA    R1,RNSMOREH         END OF LIST                                  
         CR    R3,R1                                                            
         BL    SUM00                                                            
         MVI   SCRFULL,C'Y'                                                     
         B     SUM00                                                            
         DROP  R2,R3                                                            
*                                                                               
* FORMAT MESSAGE                                                                
*                                                                               
SUMX     DS    0H                                                               
         CLI   YESMORE,C'Y'        INDICATE THERE WERE MORE                     
         BNE   SUMXA                                                            
         MVC   RNSMORE,=C'-->'                                                  
         OI    RNSMOREH+5,X'80'                                                 
*                                                                               
SUMXA    LA    R1,RUNACTH          SET FADR FOR ERROR/EXIT                      
         ST    R1,FADR                                                          
         XC    FLD(L'RUNMSG),FLD   FORMAT MESSAGE IN FLD FOR EXIT               
         MVC   FLD(L'SUMMSG),SUMMSG                                             
         LA    R7,FLD                                                           
         LA    R7,L'SUMMSG-4(R7)                                                
         EDIT  (B4,TOTSUB),(4,0(R7)),ZERO=NOBLANK,ALIGN=LEFT                    
         AR    R7,R0                                                            
         MVC   0(L'SUMMSG1,R7),SUMMSG1                                          
         LA    R7,L'SUMMSG1-5(R7)                                               
         OC    AVETIME,AVETIME                                                  
         BZ    SUM100                                                           
         L     R1,AVETIME                                                       
         SR    R0,R0                                                            
         D     R0,TOTSUB                                                        
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R7),DUB                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R7),DUB                                                      
SUM100   DS    0H                                                               
         LA    R7,5(R7)                                                         
         OC    TOTSUBB,TOTSUBB     ANY TYPE B                                   
         BZ    ERROR4                                                           
         MVC   0(L'SUMMSG2,R7),SUMMSG2                                          
         LA    R7,L'SUMMSG2-4(R7)                                               
         EDIT  (B4,TOTSUBB),(4,0(R7)),ZERO=NOBLANK,ALIGN=LEFT                   
         AR    R7,R0                                                            
         MVC   0(L'SUMMSG3,R7),SUMMSG3                                          
         LA    R7,L'SUMMSG3-5(R7)                                               
         DS    0H                  R1 = TIME IN SECONDS ONLY                    
         OC    AVETIMEB,AVETIMEB                                                
         BZ    ERROR4                                                           
         L     R1,AVETIMEB                                                      
         SR    R0,R0                                                            
         D     R0,TOTSUBB                                                       
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R7),DUB                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R7),DUB                                                      
         B     ERROR4                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT AND PRE-VALIDATE A TWA INPUT FIELD               *         
*                                                                     *         
* NTRY - R1=A(TWA FIELD HEADER)                                       *         
*        HELP=HELP TYPE IF HELP SUPPORTED FOR INPUT FIELD             *         
* EXIT - FERN=FIELD ERROR NUMBER                                      *         
*        FADR=A(INPUT FIELD HEADER)                                   *         
*        FLDH=TWA FIELD HEADER                                        *         
*        FLD =EXTRACTED & SPACE FILLED INPUT FIELD                    *         
*        CC  =EQ IF NO INPUT                                          *         
*                                                                     *         
* NOTE - HELP INTERFACE WILL BE ENTERED DIRECTLY FROM FVAL IF 'HELP'  *         
*        OR QUESTION MARK ARE INPUT IN FIELDS THAT HAVE HELP SUPPORT  *         
***********************************************************************         
         SPACE 1                                                                
FVAL     MVI   FERN,FLDNOTI                                                     
         MVI   FNDX,0              RESET MULTIPLE FIELD INDEX                   
         XC    XTRA,XTRA                                                        
         ST    R1,FADR             SET A(TWA FIELD HEADER)                      
         MVC   FLDH,0(R1)          EXTRACT HEADER & DATA                        
         MVC   FLD,SPACES                                                       
         ZIC   RF,FLDH                                                          
         LA    R0,L'FLDH+1                                                      
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)                                                     
         LA    R1,FLD(RF)          CALCULATE L'INPUT DATA                       
         LA    RF,1(RF)                                                         
FVAL2    CLI   0(R1),C' '          FIND LAST INPUT CHARACTER                    
         BNL   FVAL4                                                            
         MVI   0(R1),C' '                                                       
         BCTR  R1,0                                                             
         BCT   RF,FVAL2                                                         
         B     FVALX                                                            
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
         CLC   FLD(0),=C'HELP'                                                  
         BNE   FVALX                                                            
FVAL6    CLI   HELP,0              AND HELP SUPPORTED FOR THIS FIELD            
         BNE   HELPER              GO AND GIVE SOME HELP                        
         MVI   FERN,FLDHELN                                                     
         B     ERROR                                                            
FVALX    MVI   HELP,0                                                           
         CLI   FERN,FLDNOTI        SET CC=EQ IF FLD NOT INPUT                   
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO GIVE HELP                                                          
*                                                                               
HELPER   GOTO1 VCALLOV,DMCB,RUNHEDH,X'D90158FE',0                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACTHELP  CLI   HELP,1              TEST ACTION HELP REQUIRED                    
         BNE   OPTHELP                                                          
         MVC   RUNHED1(L'ACTHED1),ACTHED1                                       
         MVC   RUNHED2(L'ACTHED2),ACTHED2                                       
         L     R1,AACTTAB                                                       
         USING ACTTABD,R1          R1=A(ACTION TABLE)                           
         LA    RE,RUNLIN1          RE=A(TWA OUTPUT LINE)                        
ACTHELP2 CLI   ACTNAME,0                                                        
         BE    HELPX                                                            
         TM    ACTINDS,ACTIDDS     TEST DDS ONLY ACTION                         
         BZ    *+12                                                             
         TM    TERMSTAT,TSTATDDS                                                
         BZ    ACTHELP4                                                         
         MVC   0(2,RE),ACTNAME                                                  
         MVI   2(RE),C'('                                                       
         MVC   3(L'ACTNAME-2,RE),ACTNAME+2                                      
         LA    RF,L'ACTNAME+1(RE)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
         MVC   11(L'ACTINFO,RE),ACTINFO                                         
         LA    RE,L'RUNLIN1+L'RUNLIN1H(RE)                                      
ACTHELP4 LA    R1,ACTTABL(R1)                                                   
         B     ACTHELP2                                                         
*                                                                               
OPTHELP  CLI   HELP,2              TEST OPTIONS HELP REQUIRED                   
         BNE   SUBHELP                                                          
         MVC   RUNHED1(L'OPTHED1),OPTHED1                                       
         MVC   RUNHED2(L'OPTHED2),OPTHED2                                       
         L     R2,AOPTTAB                                                       
         USING OPTTABD,R2          R2=A(OPTIONS TABLE)                          
         LA    R6,RUNLIN1                                                       
OPTHELP2 CLI   OPTNAME,0           TEST E-O-T                                   
         BE    HELPX                                                            
         TM    OPTINDS,OPTIDDS     TEST DDS ONLY OPTION                         
         BZ    *+12                                                             
         TM    TERMSTAT,TSTATDDS                                                
         BZ    OPTHELPN                                                         
         MVC   DUB(2),OPTX         TEST OPTION IS COMPATIBLE                    
         NC    DUB(2),OPTOPTB                                                   
         BNZ   OPTHELPN                                                         
         SR    R1,R1                                                            
         ICM   R1,3,OPTIADDR                                                    
         LA    R1,RUN(R1)                                                       
         ST    R1,IADDR                                                         
         LA    R7,L'RUNLIN1+L'RUNLIN1H(R6)                                      
         LA    R1,OPTNAME+L'OPTNAME-1                                           
         CLI   0(R1),C' '          CALCULATE L'OPTNAME                          
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,OPTNAME                                                       
         SR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),OPTNAME     MOVE OUT FULL OPTION NAME                    
         LA    R1,1(R1)            R1=L'LONG NAME                               
         ZIC   RE,OPTMINKL         RE=MINIMUM L'KEYWORD ALLOWED                 
         CR    R1,RE               TEST SHORT VERSION ALLOWED                   
         BL    OPTHELP4                                                         
         LA    RF,0(R6,RE)         YES SHOW OPTIONAL BIT IN PARENS              
         SR    R1,RE                                                            
         LA    RE,OPTNAME(RE)                                                   
         MVI   0(RF),C'('                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),0(RE)                                                    
         LA    RF,2(RF,R1)                                                      
         MVI   0(RF),C')'                                                       
OPTHELP4 CLI   OPTSHRT,C' '        TEST SHORT KEWORD IF PRESENT                 
         BE    *+10                                                             
         MVC   1(2,R7),=C'OR'                                                   
         MVC   4(L'OPTSHRT,R7),OPTSHRT                                          
         MVC   11(L'OPTINFO,R6),OPTINFO                                         
         MVC   DUB(2),OPTR         TEST IF THIS OPTION IS REQUIRED              
         NC    DUB(2),OPTOPTB                                                   
         BZ    OPTHELP8                                                         
         LA    R1,L'OPTINFO+10(R6) YES - TACK ON REQUIRED KEYWORD               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(10,R1),=C'(REQUIRED)'                                          
OPTHELP8 TM    OPTINDS,OPTIHLP     TEST HELP INFO (AT FRONT OF TABLE)           
         BO    *+12                                                             
         TM    OPTINDS,OPTITAB     TEST IF A ROUTINE                            
         BO    OPTHELPA                                                         
         MVC   11(7,R7),=C'FORMAT='                                             
         L     R1,IADDR                                                         
         MVC   18(60,R7),0(R1)     HELP INFO IS AT A(ROUTINE)                   
         B     OPTHELPG                                                         
OPTHELPA MVC   11(8,R7),=C'VALUES='                                             
         L     R6,IADDR            R6=A(VALUES TABLE)                           
         ZIC   R3,0(R6)            R3=L'LHS OF TABLE ENTRY                      
         ZIC   R0,1(R6)                                                         
         AR    R0,R3                                                            
         ST    R0,DUB              DUB(4)=TOTAL TABLE LENGTH                    
         LA    R4,18(R7)           R4=A(OUTPUT VALUES)                          
         LA    R6,2(R6)            R6=A(FIRST TABLE ENTRY)                      
OPTHELPC CLI   0(R6),0             TEST E-O-T                                   
         BNE   *+14                                                             
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          YES - REMOVE TRAILING DELIMITER              
         B     OPTHELPG            GO ON TO NEXT OPTION                         
         LA    R1,0(R6,R3)                                                      
         BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LR    R0,R6                                                            
         SR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R6)       DISPLAY FULL VALUE                           
         LA    R1,1(R1)                                                         
         LA    RE,0(R4,R1)                                                      
         CLM   R1,1,OPTMINKL       TEST IF FULL VALUE REQUIRED                  
         BL    OPTHELPE                                                         
         ZIC   RE,OPTMINDL         NO - SHOW OPTIONAL BIT IN PARENS             
         LA    RF,0(R4,RE)                                                      
         SR    R1,RE                                                            
         LA    RE,0(R6,RE)                                                      
         MVI   0(RF),C'('                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),0(RE)                                                    
         LA    RF,2(RF,R1)                                                      
         MVI   0(RF),C')'                                                       
         LA    RE,1(RF)                                                         
OPTHELPE MVI   0(RE),C'/'          INSERT DELIMITER                             
         LA    R4,1(RE)                                                         
         A     R6,DUB              BUMP TO NEXT VALUE ENTRY                     
         B     OPTHELPC                                                         
OPTHELPG LA    R6,L'RUNLIN1+L'RUNLIN1H(R7)                                      
OPTHELPN LA    R2,OPTTABL(R2)                                                   
         B     OPTHELP2                                                         
*                                                                               
SUBHELP  CLI   HELP,4              TEST SUB-ACTION HELP REQUIRED                
         BNE   HELPX                                                            
         LA    R1,RUNTABH          SET FADR TO TAB FIELD                        
         ST    R1,FADR                                                          
         MVC   RUNHED1(L'SUBHED1),SUBHED1                                       
         MVC   RUNHED2(L'SUBHED2),SUBHED2                                       
         L     R1,ASUBTAB          R1=A(SUB-ACTION TABLE)                       
         USING SUBTABD,R1                                                       
         LA    RE,RUNLIN1          RE=A(TWA OUTPUT LINE)                        
SUBHELP2 CLI   SUBNAME,0           TEST E-O-T                                   
         BE    HELPX                                                            
         TM    SUBINDS,SUBIDDS     TEST DDS ONLY SUB-ACTION                     
         BZ    *+12                                                             
         TM    TERMSTAT,TSTATDDS                                                
         BZ    SUBHELP4                                                         
         MVC   0(L'SUBINFO,RE),SUBINFO                                          
         LA    RE,L'RUNLIN1+L'RUNLIN1H(RE)                                      
SUBHELP4 LA    R1,SUBTABL(R1)                                                   
         B     SUBHELP2                                                         
         DROP  R1                                                               
*                                                                               
HELPX    MVI   FERN,FLDHELP                                                     
         XC    TRANSNUM,TRANSNUM   FORCE DISPLAY NEXT TIME                      
         B     ERROR                                                            
         EJECT                                                                  
* ERROR SETTINGS                                                                
*                                                                               
EIIF     MVI   FERN,FLDBADI        INVALID INPUT FIELD                          
         B     ERROR                                                            
EFTS     MVI   FERN,FLDSHRT        FIELD TOO SHORT                              
         B     ERROR                                                            
EFTL     MVI   FERN,FLDLONG        FIELD TOO LONG                               
         B     ERROR                                                            
EIAC     MVI   FERN,FLDIACT        INVALID ACTION                               
         B     ERROR                                                            
EFNN     MVI   FERN,FLDNOTN        FIELD NOT NUMERIC                            
         B     ERROR                                                            
EFVB     MVI   FERN,FLDVBIG        FIELD VALUE TOO BIG                          
         B     ERROR                                                            
EFVS     MVI   FERN,FLDVSML        FIELD VALUE TOO SMALL                        
         B     ERROR                                                            
EIID     MVI   FERN,FLDIDNF        INVALID (USER) ID                            
         B     ERROR                                                            
EIKW     MVI   FERN,FLDBADK        INVALID KEYWORD                              
         B     ERROR                                                            
EKWI     MVI   FERN,FLDINCK        KEYWORD INCOMPATIBLE                         
         B     ERROR                                                            
EDKO     MVI   FERN,FLDDUPK        DUPLICATED KEYWORD OPTION                    
         B     ERROR                                                            
EIDV     MVI   FERN,FLDBADD        INVALID DATA VALUE                           
         B     ERROR                                                            
EROM     MVI   FERN,FLDREQK        REQUIRED OPTION MISSING                      
         B     ERROR                                                            
EDTS     MVI   FERN,FLDDSML        DATA TOO SHORT                               
         B     ERROR                                                            
EDTL     MVI   FERN,FLDDBIG        DATA TOO LONG                                
         B     ERROR                                                            
ENTC     MVI   FERN,FLDNCHA        NOTHING TO CHANGE                            
         B     ERROR                                                            
ENTD     MVI   FERN,FLDNDIS        NOTHING TO DISPLAY                           
         OI    DUMMY,X'80'         INFO MSG                                     
         B     ERROR                                                            
EILA     MVI   FERN,FLDISUB        INVALID LINE ACTION                          
         B     ERROR                                                            
EITN     MVI   FERN,FLDITRM        INVALID TERMINAL NUMBER                      
         B     ERROR                                                            
         EJECT                                                                  
***********************************************************************         
* OUTPUT AN ERROR MESSAGE & EXIT                                      *         
*                                                                     *         
* NTRY - FERN=ERROR NUMBER                                            *         
*        FADR=A(TWA FIELD HEADER OF FIELD IN ERROR)                   *         
*        XTRA=EXTRA MESSAGE TO BE TACKED ON TO OUTPUT MESSAGE OR      *         
*        FNDX=MULTIPLE FIELD INDEX NUMBER                             *         
***********************************************************************         
         SPACE 1                                                                
ERROR    MVC   FLD,SPACES                                                       
         L     R1,=A(ERRS)                                                      
         A     R1,RELO             R1=A(ERROR MESSAGE TABLE)                    
         SR    RE,RE                                                            
         LA    RF,FLD                                                           
         TM    FERN,X'80'          TEST ERROR MESSAGE FORMAT                    
         BNZ   ERROR2              NO                                           
         TM    DUMMY,X'80'         DON'T SAY ERROR, IT'S AN INFO MSG            
         BNZ   ERROR2              NO                                           
         MVC   FLD(10),=C'**ERROR** '                                           
         LA    RF,FLD+10                                                        
ERROR2   CLI   0(R1),0             TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   FERN,0(R1)                                                       
         BE    *+16                                                             
         IC    RE,1(R1)                                                         
         LA    R1,1(RE,R1)                                                      
         B     ERROR2                                                           
         LR    RE,R1               RE=A(MESSAGE TABLE ENTRY)                    
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),2(RE)       MOVE MESSAGE                                 
         OC    XTRA,XTRA                                                        
         BNZ   *+12                                                             
         CLI   FNDX,0                                                           
         BE    ERROR4                                                           
         LA    R1,FLD+L'RUNMSG-1                                                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    RE,1(R1)                                                         
         LA    RF,FLD                                                           
         SR    RE,RF                                                            
         LA    RF,L'RUNMSG-10                                                   
         CR    RE,RF                                                            
         BH    ERROR4                                                           
         OC    XTRA,XTRA                                                        
         BZ    ERROR3                                                           
         MVI   1(R1),C'-'                                                       
         MVC   2(L'XTRA,R1),XTRA                                                
         B     ERROR4                                                           
ERROR3   MVC   1(9,R1),=C' - FLD#NN'                                            
         ZIC   R0,FNDX                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R1),DUB                                                      
ERROR4   MVC   RUNMSG,FLD                                                       
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD IN ERROR                 
         B     DONE                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE BACK SPECIAL S/R SAVE PAGE (IF READ) & IF ANY JOBTAB ENTRIES  *         
* WERE DELETED BY TRANSACTION COMPRESS SCHEDULER JOB TABLE            *         
***********************************************************************         
         SPACE 1                                                                
DONE     CLI   TWAREAD,C'Y'        TEST TWA WAS READ                            
         BNE   DONE2               NO                                           
         MVC   RUNSTNUM,TRANSNUM   WRITE BACK S/R SAVE PAGE                     
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,TWAPAGE,SRPATIA                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DONE2    TM    CHATYPE,CHATDEL     TEST JOBTAB ENTRY DELETED                    
         BZ    EXIT                                                             
         GOTO1 VTICTOC,DUB,C'SSET'                                              
         L     R1,VSSB                                                          
         OI    SSBSTAT1-SSBD(R1),SSBSCHK1                                       
         L     R1,AJOBTAB          YES - COMPRESS JOB QUEUE                     
         LH    RE,JOBTABL                                                       
         L     RF,JOBTABX                                                       
         USING JOBTABD,R1          R1=A(JOB SCHEDULER TABLE)                    
         CLI   JOBSTAT,JOBSAVA                                                  
         BE    *+12                                                             
DONE4    BXLE  R1,RE,*-8                                                        
         B     EXIT                                                             
         LR    R2,R1               SAVE A(FREE ENTRY)                           
         BXLE  R1,RE,*+8                                                        
         B     EXIT                                                             
         CLI   JOBSTAT,JOBSAVA     TEST FREE ENTRY                              
         BE    *-12                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R1)                                                    
         LA    RE,1(RE)                                                         
         MVI   JOBSTAT,JOBSAVA                                                  
         LR    R1,R2                                                            
         B     DONE4                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
EXIT     DS    0H                                                               
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
SPACES   DC    CL80' '                                                          
         SPACE 1                                                                
PUT      DC    C'PUT'                                                           
ADR      DC    C'ADR'                                                           
END      DC    C'END'                                                           
EOJ      DC    C'EOJ'                                                           
         SPACE 1                                                                
DMREAD   DC    C'DMREAD  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
DMUNLK   DC    C'DMUNLK  '                                                      
GFILE    DC    C'GFILE   '                                                      
READ     DC    C'READ    '                                                      
         SPACE 1                                                                
TEMPSTR  DC    C'TEMPSTR '                                                      
CTFILE   DC    C'CTFILE  '                                                      
PRTQUE   DC    C'PRTQUE  '                                                      
         SPACE 1                                                                
DISMSG   DC    C'Job numbers AAA to BBB of CCC displayed'                       
NXTMSG   DC    C' - enter next request'                                         
SUMMSG   DC    C'Total waiting=####'                                            
SUMMSG1  DC    C',avg wait=00:00'                                               
SUMMSG2  DC    C' Type B waiting=####'                                          
SUMMSG3  DC    C',00:00'                                                        
CHAMSG   DC    C' entries removed from job table'                               
ANNMSG1  DC    C'** ERROR #1 ** PLEASE CALL ANN BEALS AT X5155  '               
ANNMSG2  DC    C'** ERROR #2 ** PLEASE CALL ANN BEALS AT X5155  '               
         EJECT                                                                  
ACTHED1  DC    C' Valid     Action description'                                 
ACTHED2  DC    C'actions    ------------------'                                 
         SPACE 1                                                                
OPTHED1  DC    C' Valid     Option descriptions/formats/values'                 
OPTHED2  DC    C'options    ----------------------------------'                 
         SPACE 1                                                                
SUBHED1  DC    C' Valid   Action descriptions'                                  
SUBHED2  DC    C'actions  -------------------'                                  
         SPACE 2                                                                
* ACTION TABLE (SEE ACTTABD)                                                    
*                                                                               
ACTTAB   DS    0X                                                               
*                                                                               
         DC    C'DISPLAY '                                                      
         DC    AL1(ACTTDIS,0,0)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    CL60'to display job queue entries'                               
*                                                                               
         DC    C'CLEAR   '                                                      
         DC    AL1(ACTTCLR,0,0)                                                 
         DC    AL2(0)                                                           
         DC    X'8800'                                                          
         DC    CL60'To clear job queue entries (Jobs will still run!)'          
*                                                                               
         DC    C'HELP    '                                                      
         DC    AL1(0,0,0)                                                       
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    CL60'or ? if help is required'                                   
*                                                                               
         DC    C'SUMMARY '                                                      
         DC    AL1(ACTTSUM,0,0)                                                 
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    CL60'to display summary screen'                                  
*                                                                               
ACTTABX  DC    X'00'                                                            
         EJECT                                                                  
OPTTAB   DS    0X                  ** OPTIONS TABLE (SEE OPTTABD) **            
*                                                                               
         DC    C'STATUS  ',C'   '                                               
         DC    AL1(OPTITAB,2,2,9)                                               
         DC    AL1(1),X'8000'                                                   
         DC    AL2(STATAB-RUN,JOBFILT-RUNWRKD),AL1(L'JOBFILT)                   
         DC    CL40'to display jobs with a specified status'                    
*                                                                               
         DC    C'TERMINAL',C'TRM'                                               
         DC    AL1(OPTIDDS+OPTIRTN,2,1,9)                                       
         DC    AL1(2),X'4000'                                                   
         DC    AL2(VALTRM-RUN,TERMNUM-RUNWRKD),AL1(L'TERMNUM)                   
         DC    CL40'to display jobs for a specified terminal'                   
*                                                                               
         DC    C'SYSTEM  ',C'   '                                               
         DC    AL1(OPTIDDS+OPTIRTN,2,1,1)                                       
         DC    AL1(3),X'2000'                                                   
         DC    AL2(VALSYS-RUN,JOBSYS-RUNWRKD),AL1(L'JOBSYS)                     
         DC    CL40'to display jobs for a specific system'                      
*                                                                               
         DC    C'JOBTYPE ',C'TYP'                                               
         DC    AL1(OPTIDDS+OPTIRTN,2,3,3)                                       
         DC    AL1(4),X'1000'                                                   
         DC    AL2(VALTYP-RUN,JOBTYPE-RUNWRKD),AL1(L'JOBTYPE)                   
         DC    CL40'to display jobs of a specified type'                        
*                                                                               
         DC    C'SEQUENCE',C'   '                                               
         DC    AL1(OPTIDDS+OPTIRTN,2,1,8)                                       
         DC    AL1(5),X'0800'                                                   
         DC    AL2(VALSTNO-RUN,STARTSEQ-RUNWRKD),AL1(L'STARTSEQ)                
         DC    CL40'to display from a specified seq num'                        
*                                                                               
         DC    C'CLASS   ',C'CLS'                                               
         DC    AL1(OPTIDDS+OPTIRTN,2,1,2)                                       
         DC    AL1(6),X'0400'                                                   
         DC    AL2(VALCLS-RUN,JOBCLS-RUNWRKD),AL1(L'JOBCLS)                     
         DC    CL40'to display jobs of a specified class'                       
*                                                                               
         DC    C'USERID  ',C'USR'                                               
         DC    AL1(OPTIDDS+OPTIRTN,2,3,8)                                       
         DC    AL1(7),X'0200'                                                   
         DC    AL2(VALUSER-RUN,JOBUSER-RUNWRKD),AL1(L'JOBUSER)                  
         DC    CL40'to display jobs of a specified user-id'                     
*                                                                               
         DC    C'REPORT  ',C'SUB'                                               
         DC    AL1(OPTIDDS+OPTIRTN,2,1,3)                                       
         DC    AL1(8),X'0100'                                                   
         DC    AL2(VALSUBID-RUN,JOBSUBID-RUNWRKD),AL1(L'JOBSUBID)               
         DC    CL40'to display jobs of a specified report-id'                   
*                                                                               
OPTTABX  DC    X'00'                                                            
         SPACE 2                                                                
SUBTAB   DS    0X                  ** SUB-ACTION TABLE (SEE SUBTABD) **         
         DC    C'DEL',AL1(0),AL3(CHADEL)                                        
         DC    CL50' D(EL)   to delete a job queue entry'                       
         DC    X'00'                                                            
         SPACE 1                                                                
STATAB   DS    0XL10               ** PRIMARY STATUS TABLE **                   
         DC    AL1(9,1)                                                         
         DC    C'RNG      ',AL1(JOBFRUN)                                        
         DC    C'RUNNING  ',AL1(JOBFRUN)                                        
         DC    C'SUBMITTED',AL1(JOBFSUB)                                        
         DC    C'RDY      ',AL1(JOBFAVA)                                        
         DC    C'ERROR    ',AL1(JOBFERR)                                        
         DC    C'UNKNOWN  ',AL1(JOBFUNK)                                        
         DC    AL1(0)                                                           
         EJECT                                                                  
ERRS     DS    0X                  ** ERROR MESSAGE TABLE **                    
ERR01    DC    AL1(FLDNOTI,ERR02-*)                                             
         DC    C'missing input field'                                           
ERR02    DC    AL1(FLDBADI,ERR03-*)                                             
         DC    C'invalid input field'                                           
ERR03    DC    AL1(FLDHELP,ERR04-*)                                             
         DC    C'HELP displayed - enter next request'                           
ERR04    DC    AL1(FLDSHRT,ERR05-*)                                             
         DC    C'input field too short'                                         
ERR05    DC    AL1(FLDLONG,ERR06-*)                                             
         DC    C'input field too long'                                          
ERR06    DC    AL1(FLDIACT,ERR07-*)                                             
         DC    C'invalid ACTION - enter ? for HELP'                             
ERR07    DC    AL1(FLDNOTN,ERR08-*)                                             
         DC    C'input must be numeric'                                         
ERR08    DC    AL1(FLDVBIG,ERR09-*)                                             
         DC    C'input value too large'                                         
ERR09    DC    AL1(FLDVSML,ERR10-*)                                             
         DC    C'input value too small'                                         
ERR10    DC    AL1(FLDIDNF,ERR11-*)                                             
         DC    C'invalid user-id'                                               
ERR11    DC    AL1(FLDHELN,ERR12-*)                                             
         DC    C'HELP not supported for this field - refer to manual'           
ERR12    DC    AL1(FLDBADK,ERR13-*)                                             
         DC    C'invalid OPTION keyword - enter ? for HELP'                     
ERR13    DC    AL1(FLDINCK,ERR14-*)                                             
         DC    C'incompatible OPTION keyword'                                   
ERR14    DC    AL1(FLDDUPK,ERR15-*)                                             
         DC    C'duplicated OPTION keyword'                                     
ERR15    DC    AL1(FLDBADD,ERR16-*)                                             
         DC    C'invalid OPTION data value'                                     
ERR16    DC    AL1(FLDREQK,ERR17-*)                                             
         DC    C'required filter missing'                                       
ERR17    DC    AL1(FLDDBIG,ERR18-*)                                             
         DC    C'OPTION data value too long'                                    
ERR18    DC    AL1(FLDDSML,ERR19-*)                                             
         DC    C'OPTION data value too short'                                   
ERR19    DC    AL1(FLDNCHA,ERR20-*)                                             
         DC    C'nothing in job queue to change'                                
ERR20    DC    AL1(FLDNDIS,ERR21-*)                                             
         DC    C'Nothing to display'                                            
ERR21    DC    AL1(FLDISUB,ERR22-*)                                             
         DC    C'invalid LINE ACTION - enter ? for HELP'                        
ERR22    DC    AL1(FLDITRM,ERR23-*)                                             
         DC    C'invalid terminal address or number'                            
ERR23    EQU   *                                                                
ERRSX    DC    AL1(0)                                                           
         EJECT                                                                  
* DSECT TO COVER SAVE STORAGE (SR$RUN IN S/R TWA SAVE PAGE)                     
*                                                                               
RUNSAVED DSECT                                                                  
*                                                                               
RUNSTABN DS    H                   N'ENTRIES IN RUNSTAB                         
RUNSMAXN EQU   18                                                               
RUNSTAB  DS    (RUNSMAXN)XL2       C/I ADDRESS OF DISPLAYED REPORT              
RUNSTABL EQU   *-RUNSTAB                                                        
RUNSTNUM DS    XL2                 LAST $RUN TRANSACTION NUMBER                 
RUNSLO   DS    XL2                 LOW SEQUENCE # ON SCREEN                     
RUNSHI   DS    XL2                 HIGH SEQ # ON SCREEN                         
RUNSNUM  DS    XL2                 MAX #                                        
RUNSTSEQ DS    H                   START AT #                                   
*                                                                               
RUNSAVEL EQU   *-RUNSAVED          MUST NOT EXCEED 64 BYTES                     
         EJECT                                                                  
RUNWRKD  DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
PARA     DS    6F                                                               
WORK     DS    XL64                                                             
WORK2    DS    XL64                                                             
*                                                                               
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
         DS    H                                                                
RELO     DS    A                   PROGRAM RELOCATION FACTOR                    
AJOBTAB  DS    A                   A(JOB SCHEDULER TABLE)                       
JOBTABL  DS    H                   L'JOB SCHEDULER TABLE ENTRY                  
JOBTABX  DS    A                   A(END OF JOB SCHEDULER TABLE)                
AACTTAB  DS    A                   A(ACTION TABLE)                              
AOPTTAB  DS    A                   A(OPTION TABLE)                              
ASUBTAB  DS    A                   A(SUB-ACTION TABLE)                          
ASTATAB  DS    A                   A(JOB STATUS TABLE)                          
AMYJTAB  DS    A                   A(MY JOB STATUS TABLE)                       
TWAPAGE  DS    A                   TWA PAGE/TERMINAL NUMBER                     
VSCANNER DS    V                                                                
VTERMVAL DS    V                                                                
VHEXOUT  DS    V                                                                
VGETFACT DS    V                                                                
*                                                                               
SRPARAS  DS    0F                  SERVICE REQUEST PARAMETER LIST               
SRPASYS  DS    A                   A(SYSFACS)                                   
SRPATIA  DS    A                   A(TIA)                                       
SRPAUTL  DS    A                   A(UTL ENTRY)                                 
SRPACOM  DS    A                   A(COMFACS)                                   
SRPASEL  DS    A                   A(SELIST ENTRY)                              
SRPATWA  DS    A                   A(TWA)                                       
SRPAMAP  DS    A                   A(PHASE MAP)                                 
SRPATIOB DS    A                   A(TRANSLATOR I/O BLOCK)                      
SRPARAL  EQU   *-SRPARAS                                                        
*                                                                               
FADR     DS    A                   A(FIELD HEADER)                              
FERN     DS    X                   FIELD ERROR NUMBER                           
FLDNOTI  EQU   1                   FIELD NOT INPUT                              
FLDBADI  EQU   2                   FIELD IS INVALID                             
FLDSHRT  EQU   3                   FIELD TOO SHORT                              
FLDLONG  EQU   4                   FIELD TOO LONG                               
FLDIACT  EQU   5                   INVALID ACTION                               
FLDNOTN  EQU   6                   FIELD NOT NUMERIC                            
FLDVBIG  EQU   7                   FIELD VALUE TOO LARGE                        
FLDVSML  EQU   8                   FIELD VALUE TOO SMALL                        
FLDIDNF  EQU   9                   INVALID USER ID                              
FLDBADK  EQU   10                  INVALID OPTION KEYWORD                       
FLDINCK  EQU   11                  INCOMPATIBLE OPTION KEYWORD                  
FLDDUPK  EQU   12                  DUPLICATED OPTION KEYWORD                    
FLDBADD  EQU   13                  INVALID OPTION DATA VALUE                    
FLDREQK  EQU   14                  REQUIRED FILTER MISSING                      
FLDDBIG  EQU   15                  OPTION DATA VALUE TOO SHORT                  
FLDDSML  EQU   16                  OPTION DATA VALUE TOO LONG                   
FLDNCHA  EQU   17                  NOTHING IN QUEUE TO CHANGE                   
FLDNDIS  EQU   18                  NOTHING IN QUEUE TO DISPLAY                  
FLDISUB  EQU   19                  INVALID LINE ACTION                          
FLDCDEL  EQU   20                  CAN'T DELETE JOB                             
FLDITRM  EQU   21                  INVALID TERMINAL NUMBER                      
FLDHELN  EQU   253                 HELP NOT SUPPORTED FOR FIELD                 
FLDHELP  EQU   254                 HELP DISPLAYED FOR FIELD                     
FLDISOK  EQU   255                 FIELD IS INPUT THIS TIME                     
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
ACTION   DS    X                   ACTION TYPE (FROM ACTTYPE)                   
DISFLAG  DS    X                   DISPLAY FLAG BYTE                            
DISFADTA EQU   X'80'               DISPLAY JOB DATA                             
DISFASEQ EQU   X'40'               DISPLAY JOB SEQUENCE                         
CHAFLAG  DS    X                   CHANGE FLAG BYTE                             
*                                                                               
JOBFILT  DS    X                   JOB FLAG FILTER (AS FOR JOBFLAG)             
JOBFLAG  DS    X                   JOB FLAG BYTE                                
JOBFRUN  EQU   X'80'               JOB IS RUNNING                               
JOBFSUB  EQU   X'40'               JOB SUBMITTED                                
JOBFAVA  EQU   X'20'               JOB READY                                    
JOBFERR  EQU   X'10'               JOB READY IN ERROR                           
JOBFUNK  EQU   X'08'               JOB READY IN ERROR                           
*                                                                               
JOBSYS   DS    CL1                 SYSTEM                                       
JOBTYPE  DS    CL3                 SYSTEM/PROG FILTER                           
JOBCLS   DS    CL2                 JOB CLASS FILTER                             
JOBUSER  DS    XL2                 USERID FILTER                                
JOBSUBID DS    CL3                 SUB-ID FILTER                                
*                                                                               
STARTSEQ DS    H                   STARTSEQ= OPTION                             
STARTPF  DS    H                   START FOR PF ACTION                          
*                                                                               
SEQS     DS    0XL10                                                            
SEQLO    DS    H                   LOW JOB SEQUENCE NUMBER DISPLAYED            
SEQHI    DS    H                   HIGH JOB SEQUENCE NUMBER DISPLAYED           
SEQNUM   DS    H                   MAXIMUM NUMBER OF JOBS THAT QUALIFY          
SEQDIS   DS    H                   CURRENT DISPLAY SEQUENCE NUMBER              
SEQCHA   DS    H                   CURRENT CHANGE SEQUENCE NUMBER               
CHANUM   DS    H                   NUMBER OF QUEUE CHANGES MADE                 
CHATYPE  DS    X                   TYPE OF CHANGE MADE                          
CHATDEL  EQU   X'80'               JOBTAB ENTRY DELETED                         
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
TABCOUNT DS    F                   NUMBER OF ENTRIES IN MYJOBTAB                
AFPRTQUE DS    A                   A(FIRST PQ NAME)                             
ACPRTQUE DS    A                   A(CURRENT PQ NAME)                           
LISTCT   DS    X                   COUNT OF ENTRIES ON PAGE                     
DUMMY    DS    X                                                                
PRTQN    DS    CL7                 PRINT QUEUE NAME                             
         DS    CL4                 PQ REC LEN                                   
R        DS    CL210               PQ REC DATA                                  
SUMLLEN  EQU   36                                                               
*                                                                               
*                                                                               
* DMPRTQW                                                                       
         DS    0H                  ALIGN                                        
       ++INCLUDE DMPRTQW                                                        
*                                                                               
SAVERUNN DS    H                   N'ENTRIES IN SAVERUNT                        
SAVERUNT DS    (RUNSMAXN)XL2       C/I ADDRESS OF DISPLAYED REPORT              
SAVERUNL EQU   *-SAVERUNT                                                       
*                                                                               
NUMRUN   DS    F                                                                
NUMSUB   DS    F                                                                
NUMERR   DS    F                                                                
NUMRDY   DS    F                                                                
NUMUNK   DS    F                                                                
TOTRUN   DS    F                                                                
TOTSUB   DS    F                                                                
TOTSUBB  DS    F                                                                
TOTERR   DS    F                                                                
TOTRDY   DS    F                                                                
TOTUNK   DS    F                                                                
CURTIME  DS    F                                                                
TIMECNT  DS    F                                                                
AVETIME  DS    F                                                                
AVETIMEB DS    F                                                                
PRTQID   DS    CL8                                                              
OLDCLASS DS    CL2                                                              
OLDTYPEB DS    CL1                                                              
PFKEY    DS    X                                                                
SCRFULL  DS    X                                                                
YESMORE  DS    X                                                                
CIMAXRPT DS    H                                                                
INDCOUNT DS    H                   COUNT OF INDEX CALLS PER PQ#                 
SVPQINDX DS    XL40                                                             
MYPQINDX DS    XL40                                                             
NDX      DS    XL40                                                             
KEY      DS    XL25                                                             
IO       DS    1000C                                                            
CIRECLN  EQU   14336                                                            
CIREC    DS    14336X              PRINT QUEUE INDEX BUFFER                     
*                                                                               
MYJOBTAB DS    (MYJOBTL)CL1000       MY COMPRESSED JOBTABLE                     
*                                    ROOM FOR 1000 ENTRIES                      
         DS    CL1                                                              
*                                                                               
RUNWRKX  EQU   *                                                                
         EJECT                                                                  
ACTTABD  DSECT                     ** ACTION TABLE **                           
ACTNAME  DS    CL8                 ACTION NAME (MIN CHARS INPUT)                
ACTTYPE  DS    X                   ACTION TYPE                                  
ACTTDIS  EQU   X'80'               DISPLAY JOB QUEUE                            
ACTTCLR  EQU   X'40'               CLEAR JOB QUEUE                              
ACTTSUM  EQU   X'20'               DISPLAY SUMMARY SCREEN                       
ACTINDS  DS    X                   ACTION INDICATORS                            
ACTIDDS  EQU   X'80'               DDS ONLY ACTION                              
ACTJOBI  DS    X                   JOB VALIDATION INDICATORS                    
ACTOPTR  DS    XL2                 REQUIRED FILTERS                             
ACTOPTX  DS    XL2                 OPTIONS NOT ALLOWED                          
ACTINFO  DS    XL60                HELP TEXT                                    
ACTTABL  EQU   *-ACTTABD                                                        
         SPACE 1                                                                
OPTTABD  DSECT                     ** OPTIONS TABLE **                          
OPTNAME  DS    CL8                 OPTION NAME (LONG)                           
OPTSHRT  DS    CL3                 OPTION NAME (SHORT) OR SPACES                
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
OPTINFO  DS    CL40                HELP TEXT                                    
OPTTABL  EQU   *-OPTTABD                                                        
         SPACE 1                                                                
SUBTABD  DSECT                     ** SUB-ACTION TABLE **                       
SUBCODE  DS    0C                  SUB-ACTION CODE                              
SUBNAME  DS    CL3                 SUB-ACTION NAME                              
SUBINDS  DS    XL1                 SUB-ACTION INDICATORS                        
SUBIDDS  EQU   X'80'               DDS ONLY SUB-ACTION                          
SUBROUT  DS    AL3                 SUB-ACTION ROUTINE ADDRESS                   
SUBINFO  DS    CL50                HELP TEXT                                    
SUBTABL  EQU   *-SUBTABD                                                        
        EJECT                                                                   
RUNTWAD  DSECT                                                                  
         DS    XL64                                                             
       ++INCLUDE SRRUNFFD                                                       
         ORG   RUNHEDH                                                          
       ++INCLUDE SRRUNFED                                                       
         ORG   RUNHEDH                                                          
       ++INCLUDE SRRUNFDD                                                       
         EJECT                                                                  
SUMLINED DSECT                                                                  
SUMLCLS  DS    CL2                                                              
SUMLTYPB DS    CL2                                                              
         DS    CL2                                                              
SUMLSUB  DS    CL4                                                              
         DS    CL1                                                              
SUMLRNG  DS    CL4                                                              
         DS    CL1                                                              
SUMLRDY  DS    CL4                                                              
         DS    CL1                                                              
SUMLERR  DS    CL4                                                              
         DS    CL1                                                              
SUMLUNK  DS    CL4                                                              
         DS    CL1                                                              
SUMLHH   DS    CL2                                                              
SUMLCOL  DS    CL1                                                              
SUMLMM   DS    CL2                                                              
         EJECT                                                                  
MYJOBTBD DSECT                                                                  
MYJCLASS DS    CL2                 DON'T MOVE - CHANGES SORTS/SEARCH            
MYJTYPB  DS    CL1                 "                                            
MYJTIME  DS    CL2                 "                                            
MYJTERM  DS    XL2                                                              
MYJPQKEY DS    0XL7                                                             
MYJPQUSR DS    XL2                                                              
MYJPQSUB DS    CL3                                                              
MYJPQSEQ DS    XL2                                                              
MYJPQCIA DS    XL2                                                              
MYJPQID  DS    CL2                                                              
MYJPRTY  DS    CL1                                                              
MYJJESNO DS    CL2                                                              
         ORG   MYJJESNO                                                         
MYJCLS   DS    CL2                                                              
MYJMVSID DS    CL8                                                              
*MYJREC   DS    CL28                                                            
MYJOBFL  DS    CL1                                                              
MYJOBTYP DS    CL1                                                              
MYJPQSTA DS    CL1                                                              
MYJPQATB DS    CL1                                                              
MYJOBTL  EQU   *-MYJOBTBD                                                       
         EJECT                                                                  
RUNLINED DSECT                     ** DISPLAY/CHANGE LINE **                    
RUNLACTH DS    XL8                                                              
RUNLACT  DS    CL3                 JOB QUEUE ACTION                             
RUNLLINH DS    XL8                                                              
RUNLLIN  DS    0CL74               JOB QUEUE DISPLAY                            
         DS    C                                                                
RUNLTIME DS    0CL8                HH.MM.SS                                     
RUNLHH   DS    CL2                                                              
RUNLHMD  DS    C                                                                
RUNLHMDQ EQU   C'.'                                                             
RUNLMM   DS    CL2                                                              
RUNLMSD  DS    C                                                                
RUNLMSDQ EQU   C'.'                                                             
RUNLSS   DS    CL2                                                              
         DS    C                                                                
RUNLCLS  DS    CL2                 CC                                           
         DS    CL1                                                              
RUNLUNA  DS    CL1                 LUNATIC REPORT                               
         DS    CL1                                                              
RUNLTYPB DS    CL1                 TYPE B REPORT                                
         DS    CL2                                                              
RUNLPRTY DS    C                   P                                            
         DS    C                                                                
RUNLNAME DS    CL8                 UUUUSSPP                                     
         DS    C                                                                
RUNLPQNO DS    CL2                 PRINT QUEUE NUMBER                           
         DS    C                                                                
RUNLCIA  DS    CL4                 C/I/ADDRESS                                  
         DS    C                                                                
RUNLTNUM DS    CL4                 TTTT                                         
         DS    C                                                                
RUNLTSYM DS    CL8                 LLLLCUDV                                     
         DS    CL1                                                              
RUNLUSER DS    CL8                 USER-ID                                      
         DS    C                                                                
RUNLREPT DS    CL3                 SSS                                          
RUNLRSD  DS    C                                                                
RUNLRSDQ EQU   C','                                                             
RUNLSEQN DS    CL5                 NNNNN                                        
         DS    C                                                                
RUNLSTAT DS    CL3                 JOB STATUS                                   
         DS    C                                                                
RUNLINEL EQU   *-RUNLINED                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
       ++INCLUDE DMPRTQK                                                        
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078SRRUN00S  05/01/02'                                      
         END                                                                    
