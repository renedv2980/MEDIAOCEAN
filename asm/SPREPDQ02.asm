*          DATA SET SPREPDQ02  AT LEVEL 034 AS OF 10/04/13                      
*PHASE SPDQ02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRTREC                                                                 
                                                                                
*=====================================================================          
* THIS PROGRAM READS THE OUTPUT OF 'RCVSTRIP', WHICH STRIPS AND SORTS           
* RECOVERY FILES, LEAVING ONLY THE FIRST COPY AND THE LAST CHANGE               
* FOR EACH RECORD.                                                              
*                                                                               
* THERE IS ONE REQUEST CARD FOR EACH CLIENT ACTIVE IN THE IQ/DDS                
* DATA EXCHANGE PROCESS                                                         
*                                                                               
* IN ORDER TO SORT THE RECORDS BY MASTER EST/MONTH/BUYKEY,                      
* THE LAST BYTES OF THE RECOVERY HEADER ARE OVERWRITTEN BEFORE THE              
* RECORDS ARE PASSED TO THE SORT.                                               
*                                                                               
* IQ CALLS BUYS AVAILS AND CALLS SPOTS DETAILS                                  
* ALL OF THEIR AVAILS LIVE ON THE MASTER ESTIMATE NUMBER, WHILE DDS             
* CARRIES THE DATA ON THE SUB-ESTIMATE NUMBERS.                                 
*                                                                               
* A FILE OF AVAILS AND DETAILS IS GENERATED FOR EACH MASTER EST/MONTH           
* *EOF*EOF*EOF IS USED AS A SEPARATOR                                           
*                                                                               
* THE NEXT JOB STEP (SPREPDG02) GENERATES THE ACTUAL DOWNLOAD FILES             
* IF YOU NEED TO SEE THE REAL OUTPUT FILES, CONSULT YI YUNG                     
*=====================================================================          
                                                                                
SPDQ02   TITLE 'GENERATE IQ ACTIVITY DATA FROM RECOVERY'                        
         PRINT NOGEN                                                            
SPDQ02   CSECT                                                                  
*                                                                               
QSPTFIL  EQU   X'21'                                                            
*                                                                               
QCOPY    EQU   1                                                                
QCHG     EQU   2                                                                
QADD     EQU   3                                                                
*                                                                               
         NMOD1 0,SPDQ02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
*                                                                               
         L     RC,=A(SPDQ02WK)                                                  
         USING SPDQ02WK,RC                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*===========================================================                    
* OPEN OUTPUT FILE AT RUNFRST                                                   
*===========================================================                    
                                                                                
RUNF     OPEN  (IQFILE,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
                                                                                
*===========================================================                    
* CLOSE OUTPUT FILES AT RUNLAST                                                 
*===========================================================                    
                                                                                
RUNL     CLOSE IQFILE                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
         EJECT                                                                  
CLTF     BRAS  RE,BLDEST           BUILD MASTER EST LIST                        
*                                                                               
         BRAS  RE,BLDCML                                                        
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(12),DMCB                                                    
         L     RE,=A(SORTCARD)                                                  
         ST    RE,DMCB+0                                                        
         L     RE,=A(RECCARD)                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 =V(SORTER),DMCB                                                  
*                                                                               
GETRECV  L     R1,=A(RECVIN)                                                    
         L     R0,ADBUY            READ TO ADBUY-4                              
         AHI   R0,-4                                                            
         GET   (1),(0)                                                          
*                                                                               
         L     R8,ADBUY                                                         
         USING RCVRHDRD,R8                                                      
*                                                                               
         CLI   DM$RFILTY,QSPTFIL                                                
         BNE   GETRECV                                                          
*                                                                               
         CLI   DM$RRECTY,QCOPY                                                  
         BE    GETRECV             IGNORE COPIES                                
*                                                                               
         CLI   DM$RPRG,C'Q'        TEST UPLOAD PROGRAM ACTIVITY                 
         BE    GETRECV             YES - DON'T INCLUDE IT                       
*                                                                               
         CLC   BUYKEY(3),BAGYMD    RIGHT A-M/CLT                                
         BNE   GETRECV                                                          
*                                                                               
         SR    RE,RE               SEE IF ESTIMATE ACTIVE                       
         IC    RE,BUYKEST                                                       
         LA    RE,ESTLST(RE)                                                    
         CLI   0(RE),0                                                          
         BE    GETRECV                                                          
*                                                                               
         MVC   DM$MSTR(1),0(RE)    MOVE MASTER EST NUMBER                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDEND),WORK     GET END DATE YYMMDD               
         L     RF,ADCONLST                                                      
         L     RF,VBRDMON-SPADCONS(RF)                                          
         GOTO1 (RF),DMCB,WORK,WORK+6          GET BRDMON OF END DATE            
         GOTO1 DATCON,DMCB,WORK+6,(3,WORK+12) GET 3-BYTE YMD                    
         MVC   DM$YYMM(2),WORK+12             MOVE BINARY Y/M                   
*                                                                               
         L     R0,ADBUY                                                         
         AHI   R0,-4                                                            
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R0)                                     
         B     GETRECV                                                          
         EJECT                                                                  
GETSORT  DC    0H'0'                                                            
         CLOSE (RECVIN)                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
GETSRT2  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RE,15,4(R1)                                                      
         BZ    GETSRTX                                                          
* MOVE RECORD TO ADBUY AND SET 2X'00' AT END                                    
         LH    RF,0(RE)            GET LENGTH                                   
         L     R0,ADBUY                                                         
         AHI   R0,-4                                                            
         LA    R1,2(RF)            SET TOLEN = FROMLEN+2                        
         MVCL  R0,RE                                                            
*                                                                               
         L     R8,ADBUY                                                         
         USING RCVRHDRD,R8                                                      
*                                                                               
         CLI   LASTMEST,0          TEST FIRST TIME                              
         BE    GETSRT10                                                         
         CLC   LASTMEST,DM$MSTR    SAME MASTER EST AS PREVIOUS                  
         BNE   GETSRT4                                                          
         CLC   LASTYYMM,DM$YYMM                                                 
         BE    GETSRT12                                                         
*                                                                               
GETSRT4  LA    R0,EOFLN            OUTPUT SEPARATORS                            
         PUT   IQFILE,(0)                                                       
*                                                                               
         BRAS  RE,PRTTOTS                                                       
PRTTOTS  DS    0H                                                               
*                                                                               
GETSRT10 MVI   FORCEHED,C'Y'                                                    
         MVC   LASTMEST,DM$MSTR                                                 
         MVC   LASTYYMM,DM$YYMM                                                 
         MVI   HDRSW,C'N'                                                       
*                                                                               
GETSRT12 CLI   HDRSW,C'Y'          TEST HEADER SENT YET                         
         BE    GETSRT20                                                         
*                                                                               
         BRAS  RE,BLDHDR           BUILD NEW HEADER                             
*                                                                               
         MVI   HDRSW,C'Y'                                                       
         ZAP   AVLCOUNT,=P'0'                                                   
         ZAP   DTLCOUNT,=P'0'                                                   
*                                                                               
GETSRT20 BRAS  RE,BLDAVL                                                        
*                                                                               
         TM    BUYKEY+15,X'80'     TEST RECORD DELETED                          
         BO    GETSRT2             YES - NO SPOTS TO OUTPUT                     
*                                                                               
         BRAS  RE,BLDSPOTS                                                      
*                                                                               
         B     GETSRT2                                                          
*                                                                               
GETSRTX  GOTO1 AENDREQ             GO ON TO NEXT REQUEST                        
                                                                                
         EJECT                                                                  
*==========================================================                     
* BUILD LIST OF ESTIMATES TO PROCESS FOR CLIENT IN CLTHDR                       
*==========================================================                     
                                                                                
         USING RCVRHDRD,R8                                                      
BLDEST   NTR1  BASE=*,LABEL=*                                                   
         XC    ESTLST,ESTLST                                                    
*                                                                               
         XC    KEY,KEY             READ CSO MASTER EST RECORD                   
K        USING MASRECD,KEY                                                      
         MVI   K.MASKTYPE,X'0D'                                                 
         MVI   K.MASKSTYP,X'6F'                                                 
         L     RE,ADCLT                                                         
         MVC   K.MASKAM,1(RE)                                                   
         MVC   K.MASKCLT,2(RE)                                                  
         GOTO1 HIGH                                                             
         B     BLDES4                                                           
*                                                                               
BLDES2   GOTO1 SEQ                                                              
*                                                                               
BLDES4   CLC   KEY(5),KEYSAVE                                                   
         BNE   BLDES20                                                          
         CLC   K.MASKDATE,=X'FAF4F0F5F3F1'  IQ START IS MAY31/04                
         BL    BLDES2                                                           
         DROP  K                                                                
*                                                                               
BLDES10  GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY            USE BUYREC I/O AREA                          
         LA    R6,24(R6)                                                        
         CLI   0(R6),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,2(R6)            POINT TO MASTER EST LIST ENTRY               
*                                                                               
         USING MESTLSTD,R6                                                      
BLDES12  LA    R1,MESTSUBS                                                      
*                                                                               
BLDES14  SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    RE,ESTLST(RE)                                                    
         MVC   0(1,RE),MESTNUM     MOVE MASTER EST FOR THIS SUBEST              
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   BLDES14                                                          
*                                                                               
         LA    R6,9(R6)            NEXT MASTER EST ENTRY                        
         CLI   0(R6),0                                                          
         BNE   BLDES12                                                          
         B     BLDES2                                                           
         DROP  R6                                                               
                                                                                
*==================================================================             
* CHECK IF ANY MASTER ESTIMATES HAVE EQRELO/HI SET TO REDIRECT                  
* THE DATA TO A DIFFERENT MASTER ESTIMATE                                       
* SINCE THE REDIRECT ESTIMATE IS ALWAYS PRIOR TO THE REAL MASTER                
* ESTIMATE, COMPARE THE DATES BEFORE DECIDING TO REDIRECT.                      
* NEVER REDIRECT TO AN ESTIMATE THAT STARTS SOONER.                             
*                                                                               
* AS OF 10/5/04, SAVE THE DEMOS IN THE ESTDEM TABLE                             
*==================================================================             
                                                                                
BLDES20  XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(4),0(R6)        MOVE 00/A-M/CLT                              
         MVC   KEY+4(3),=C'POL'                                                 
         LA    R4,ESTLST+1                                                      
         LHI   R5,255                                                           
*                                                                               
BLDES22  CLI   0(R4),0                                                          
         BE    BLDES26                                                          
*                                                                               
         CLC   KEY+7(1),0(R4)      TEST READ IT ALREADY                         
         BE    BLDES24             YES - CHECK FOR REDIRECTION                  
*                                                                               
         MVC   KEY+7(1),0(R4)      ELSE READ ESTIMATE NOW                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETEST                                                           
*                                                                               
BLDES24  L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
*                                                                               
         CLI   EREQLO,0            TEST ANY REDIRECTION                         
         BE    BLDES26             NO                                           
*                                                                               
         IC    R0,EREQLO                                                        
         CLC   EREQLO,KEY+7        LOW RANGE = THIS ESTIMATE                    
         BNE   *+8                                                              
         IC    R0,EREQHI                                                        
* NEED TO CHECK DATES BEFORE SUBSTITUTING                                       
         STC   R0,KEY+7                                                         
         L     RE,ADBUY                                                         
         CLC   KEY(13),0(RE)       TEST ALREADY HAVE IT                         
         BE    BLDES25                                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLDES26                                                          
*                                                                               
         GOTO1 GETBUY         <=== USE ADBUY FOR THIS ESTHDR                    
*                                                                               
RDR      USING ESTHDRD,R8                                                       
*                                                                               
BLDES25  L     R8,ADBUY                                                         
         CLC   ESTART,RDR.ESTART   OUR EST START TO REDIRECT EST START          
         BNL   BLDES26             IF IT DOESN'T START SOONER, SKIP IT          
         STC   R0,0(R4)            REDIRECT THIS DATA TO NEW MASTER EST         
         DROP  RDR                                                              
*                                                                               
BLDES26  AHI   R4,1                                                             
         BCT   R5,BLDES22                                                       
         EJECT                                                                  
*=================================================================              
* SAVE THE DEMO LISTS FOR ALL ESTIMATES                                         
*=================================================================              
                                                                                
         L     R0,=A(ESTDEMS)                                                   
         LHI   R1,ESTDEMX-ESTDEMS                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(4),0(R6)        MOVE 00/A-M/CLT                              
         MVC   KEY+4(3),=C'POL'                                                 
         MVI   KEY+7,1                                                          
         GOTO1 HIGH                                                             
         B     BLDES32                                                          
*                                                                               
BLDES30  GOTO1 SEQ                                                              
*                                                                               
BLDES32  CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   BLDES40                                                          
*                                                                               
         GOTO1 GETEST                                                           
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
*                                                                               
         SR    RE,RE                                                            
         IC    RE,KEY+7                                                         
         BCTR  RE,0                                                             
         MHI   RE,60                                                            
         A     RE,=A(ESTDEMS)                                                   
         MVC   0(60,RE),EDEMLST    SAVE DEMO LIST                               
         B     BLDES30                                                          
*                                                                               
BLDES40  J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* BUILD COMMERCIAL TABLE                                                        
* NOTE THIS CODE SUPPRESSES TRACING - IT PRINTS TOO MUCH                        
*==============================================================                 
                                                                                
BLDCML   NTR1  BASE=*,LABEL=*                                                   
         MVC   BLDSVTRC,RCTRACE    SAVE TRACE FLAG                              
         MVI   RCTRACE,C'N'        AND TURN TRACE OFF                           
*                                                                               
         L     RE,ADCLT                                                         
         CLC   CMLTBID,0(RE)       SAME A-M/CLT                                 
         JE    BLDCMLX                                                          
         MVC   CMLTBID,0(RE)       SAVE A-M/CLT                                 
*                                                                               
         L     R0,CMLPAR2          GET A(MYCMLTAB)                              
         L     R1,=A(MYCMLTBX-MYCMLTAB)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    CMLPAR3,CMLPAR3     CLEAR RECORD COUNT                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         L     RE,ADCLT                                                         
         MVC   KEY+2(3),1(RE)                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'TRFDIR',KEYSAVE,KEY                       
*                                                                               
* SKIP CMMLSEQ RECORD                                                           
*                                                                               
BLDCML10 GOTO1 DATAMGR,DMCB,DMRSEQ,=C'TRFDIR',KEYSAVE,KEY                       
*                                                                               
BLDCML20 CLC   KEY(5),KEYSAVE      0A21/A-M/CLT                                 
         JNE   BLDCMLX                                                          
*                                                                               
         L     R6,ADBUY                                                         
         USING CMLRECD,R6                                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'TRFFILE',KEY+14,(R6),DMWORK               
*                                                                               
         XC    CMLWORK,CMLWORK                                                  
MY       USING MYCMLD,CMLWORK                                                   
         MVC   MY.MYCMSEQ,CMLSEQ+1   SAVE 2 BYTES OF CMML SEQ                   
         MVC   MY.MYCMEBC,SPACES     INITIALIZE IT                              
         TM    KEY+13,CMLKSTA_PCKD   X'01' - PACKED?                            
         BO    *+10                                                             
         MVC   MY.MYCMEBC(8),KEY+5   AND COMMERCIAL CODE                        
*                                                                               
         LA    R6,24(R6)                                                        
BLDCML30 CLI   0(R6),0             EOR?                                         
         BE    BLDCML50                                                         
         CLI   0(R6),X'A0'         ADID ELEMENT?                                
         BE    BLDCML40                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     BLDCML30                                                         
*                                                                               
         USING CMLADIEL,R6                                                      
BLDCML40 OC    CMLADID,CMLADID                                                  
         BZ    BLDCML50                                                         
         MVC   MY.MYCMADID,CMLADID                                              
*                                                                               
BLDCML50 GOTO1 BINSRCH,CMLPAR1,(1,CMLWORK) INSERT IF NOT FOUND                  
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     BLDCML10                                                         
*                                                                               
BLDCMLX  MVC   RCTRACE,BLDSVTRC                                                 
         J     EXIT                                                             
BLDSVTRC DC    X'00'                                                            
         DROP  R6,MY                                                            
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* BUILD AVAIL FILE HEADER DIRECTLY IN OUTPUT AREA                               
*================================================================               
                                                                                
BLDHDR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,IQOUT                                                         
         LHI   R1,IQOUTX-IQOUT                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R5,IQOUT                                                         
*                                                                               
         MVI   0(R5),C'H'          SET HEADER RECORD ID                         
         AHI   R5,1                                                             
         MVI   0(R5),FLDDELIM                                                   
         AHI   R5,1                                                             
*                                                                               
         MVC   0(1,R5),QMED                                                     
         AHI   R5,1                                                             
         MVI   0(R5),FLDDELIM                                                   
         AHI   R5,1                                                             
*                                                                               
         MVC   0(3,R5),QCLT                                                     
         AHI   R5,3                                                             
         MVI   0(R5),FLDDELIM                                                   
         AHI   R5,1                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LASTMEST                                                      
         BAS   RE,CVD                                                           
         MVC   0(3,R5),WORK+7                                                   
         AHI   R5,3                                                             
         MVI   0(R5),FLDDELIM                                                   
         AHI   R5,1                                                             
*                                                                               
         IC    R0,LASTYYMM+1       MONTH                                        
         BAS   RE,CVD                                                           
         MVC   0(2,R5),WORK+8                                                   
         AHI   R5,2                                                             
         MVI   0(R5),FLDDELIM                                                   
         AHI   R5,1                                                             
*                                                                               
         IC    R0,LASTYYMM         YEAR                                         
         BAS   RE,CVD                                                           
         MVC   0(2,R5),=C'20'      LET'S ASSUME THIS CENTURY !                  
         MVC   2(2,R5),WORK+8                                                   
         AHI   R5,4                                                             
         MVI   0(R5),FLDDELIM                                                   
         AHI   R5,1                                                             
*                                                                               
         SR    R4,R4                                                            
         IC    R4,LASTMEST         GET MASTER EST NUMBER                        
         BCTR  R4,0                                                             
         MHI   R4,60                                                            
         A     R4,=A(ESTDEMS)      POINT TO DEMO LIST                           
*                                                                               
         L     R6,ADBLOCK                                                       
         XC    0(256,R6),0(R6)                                                  
         USING DBLOCKD,R6                                                       
         MVC   DBSELAGY,QAGY                                                    
         MVC   DBSELMED,QMED                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         DROP  R6                                                               
                                                                                
*=============================================================                  
* GET 6 CHARACTER NAMES FOR RATINGS                                             
* GET 5 CHARACTER NAMES FOR IMPRESSIONS AND PUT AN 'I' IN FRONT                 
*=============================================================                  
                                                                                
* GET 5 AND 6 CHARACTER DEMO NAMES                                              
         XC    DEMNAMES,DEMNAMES                                                
         GOTO1 DEMOCON,DMCB,(20,(R4)),(11,DEMNAMES),(R6)                        
*                                                                               
         LA    R1,DEMNAMES                                                      
         LHI   R0,20                                                            
*                                                                               
BLDH2    CLI   0(R1),C' '          MAKE SURE SOMETHING IS THERE                 
         BNH   BLDH10                                                           
         MVC   0(6,R5),0(R1)       START WITH 6 CHAR NAME                       
         CLI   0(R1),C'R'          TEST RATING                                  
         BE    BLDH4               YES                                          
         MVI   0(R5),C'I'          SET IMPRESSION                               
         MVC   1(5,R5),0(R1)       AND USE 5 CHAR NAME                          
*                                                                               
BLDH4    AHI   R5,5                POINT TO LAST CHAR                           
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         AHI   R5,1                                                             
*                                                                               
BLDH10   MVI   0(R5),FLDDELIM                                                   
         AHI   R5,1                                                             
         AHI   R1,6                NEXT NAME                                    
         BCT   R0,BLDH2                                                         
*                                                                               
         LA    R0,IQOUTLN                                                       
         SR    R5,R0                                                            
         STH   R5,IQOUTLN                                                       
*                                                                               
         PUT   IQFILE,(0)                                                       
         AP    AVLCOUNT,=P'1'                                                   
         J     EXIT                                                             
         EJECT                                                                  
*===============================================================                
* BUILD AVAIL HEADER                                                            
* BUILD ALL FIELDS IN FIXED LOCATIONS, THEN COMPRESS AND INSERT                 
* DELIMITERS                                                                    
*================================================================               
                                                                                
BLDAVL   NTR1  BASE=*,LABEL=*                                                   
         L     R8,ADBUY                                                         
         USING RCVRHDRD,R8                                                      
*                                                                               
         BRAS  RE,PRTBUY                                                        
*                                                                               
         LA    R0,IQREC                                                         
         LHI   R1,IQRECX-IQREC                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    LASTUID,LASTUID                                                  
         XC    LASTDID,LASTDID                                                  
         MVI   DDSFLAG,C'Y'        DEFAULT TO DDS ID                            
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'95'        FIND UPLOAD ELEMENT                          
         MVI   ELCDHI,X'95'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BLDAV4                                                           
*                                                                               
         USING BUPELEM,R6                                                       
         MVI   DDSFLAG,C'N'        SET NOT A DDSID                              
*                                                                               
         LHI   R0,8                REMOVE LEADING SPACES FROM ID                
         LA    R1,BUPUID                                                        
         LA    RE,IQAVLUID                                                      
*                                                                               
BLDAV2A  CLI   0(R1),C' '                                                       
         BH    BLDAV2B                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,BLDAV2A                                                       
*                                                                               
BLDAV2B  MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,BLDAV2B                                                       
*                                                                               
         MVC   LASTUID,IQAVLUID    SAVE UID                                     
*                                                                               
BLDAV4   SR    R0,R0                                                            
         IC    R0,BUYKEY+9         ESTIMATE                                     
         BAS   RE,CVD                                                           
         MVC   IQAVLDID(3),WORK+7                                               
*                                                                               
         IC    R0,BUYKEY+10        LINE                                         
         BAS   RE,CVD                                                           
         MVC   IQAVLDID+3(3),WORK+7                                             
         MVC   LASTDID,IQAVLDID    SAVE DDS ID                                  
*                                                                               
BLDAV6   GOTO1 MSUNPK,DMCB,(X'80',BUYKEY+4),WORK,BIGSTA                         
         MVC   IQAVLSTA,BIGSTA                                                  
*                                                                               
         LHI   RE,X'40'                                                         
         LA    RF,DAYTAB                                                        
         LA    R1,IQAVLROT                                                      
*                                                                               
BLDAV10  MVI   0(R1),X'82'         SET TO LOWER CASE B                          
         EX    RE,TESTDAY                                                       
         BZ    *+10                                                             
         MVC   0(1,R1),0(RF)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         SRL   RE,1                                                             
         LTR   RE,RE                                                            
         BNZ   BLDAV10                                                          
         B     BLDAV20                                                          
TESTDAY  TM    BDDAY,0                                                          
DAYTAB   DC    C'MTWTFSS'                                                       
*                                                                               
BLDAV20  SR    R0,R0                                                            
         ICM   R0,3,BDTIMST        FOR SYBASE 2400 = 0                          
         BAS   RE,CVDTIME                                                       
         MVC   IQAVLSTR(2),WORK+6                                               
         MVI   IQAVLSTR+2,C':'                                                  
         MVC   IQAVLSTR+3(2),WORK+8                                             
*                                                                               
         ICM   R0,3,BDTIMEND       USE END TIME IF PRESENT                      
         BNZ   *+8                                                              
         ICM   R0,3,BDTIMST        ELSE REPEAT START TIME                       
         BAS   RE,CVDTIME                                                       
         MVC   IQAVLEND(2),WORK+6                                               
         MVI   IQAVLEND+2,C':'                                                  
         MVC   IQAVLEND+3(2),WORK+8                                             
*                                                                               
         MVC   IQAVLDPT,BDDAYPT                                                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         EDIT  (R0),(3,IQAVLSLN),ALIGN=LEFT                                     
*                                                                               
         MVC   IQAVLPRG,BDPROGRM                                                
*                                                                               
         LA    R1,IQAVLPRG                                                      
         LHI   R0,17                                                            
*                                                                               
BLDAV30  CLI   0(R1),FLDDELIM                                                   
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,BLDAV30                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
         TM    BDCIND2,BDCNBRDQ    SET RATE IN DOLLARS                          
         BZ    *+8                                                              
         MHI   R0,100              YES, CONVERT TO PENNIES                      
         EDIT  (R0),(10,IQAVLCOS),ALIGN=LEFT                                    
*                                                                               
         CLI   BDCIND,0            TEST NTP SPOT                                
         BNE   *+8                                                              
         MVI   IQAVLCTY,C'P'       SET COST TYPE                                
*                                                                               
         LA    R4,IQAVLCOM                                                      
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
BLDAV40  BRAS  RE,NEXTEL                                                        
         BNE   BLDAV50                                                          
         CLI   2(R6),3             COMMENTS 4-5 ARE DISC RESOLUTION             
         BNH   *+8                                                              
         LA    R4,IQAVLRES         POINT TO RESOLUTION COMMENTS                 
*                                                                               
BLDAV42  SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-4               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6)                                                    
         AHI   R4,80                                                            
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    BLDAV42                                                          
*                                                                               
BLDAV50  MVC   IQAVLMG(2),BDMGDATE MAKEGOOD ID                                  
                                                                                
* PUT OUT DEMOGRAPHIC DATA ONLY IF DDSFLAG IS Y (BUY ADDED AT DDS)              
                                                                                
         CLI   DDSFLAG,C'Y'                                                     
         BNE   BLDAV60                                                          
                                                                                
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BUYKEY+9         ESTIMATE NUMBER                              
         BCTR  R1,0                                                             
         MHI   R1,60                                                            
         A     R1,=A(ESTDEMS)                                                   
         LHI   R0,20                                                            
         LA    R4,IQAVLDEM                                                      
*                                                                               
BLDAV55  BAS   RE,GETDEM           GET/MOVE DEMO VALUE                          
         AHI   R4,8                NEXT OUTPUT POSITION                         
         AHI   R1,3                NEXT DEMO IN ESTIMATE LIST                   
         CLI   1(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,BLDAV55                                                       
         B     BLDAV60                                                          
         EJECT                                                                  
CVDTIME  CHI   R0,2400                                                          
         JL    CVD                                                              
         AHI   R0,-2400                                                         
*                                                                               
CVD      CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         BR    RE                                                               
*                                                                               
GETDEM   NTR1                                                                   
         XC    DUB,DUB                                                          
         SR    R7,R7                                                            
         IC    R7,1(R6)                                                         
         AHI   R7,-24                                                           
         BNP   GETDEMX                                                          
         SRL   R7,3                                                             
         AHI   R6,24                                                            
*                                                                               
GETDEM2  CLC   0(3,R6),0(R1)                                                    
         BE    GETDEM4                                                          
         AHI   R6,8                                                             
         BCT   R7,GETDEM2                                                       
         B     GETDEMX                                                          
*                                                                               
GETDEM4  ICM   R0,15,4(R6)                                                      
         N     R0,=X'3FFFFFFF'                                                  
         TM    4(R6),X'40'         TEST 2-DEC                                   
         BO    *+8                                                              
         MHI   R0,10                                                            
         EDIT  (R0),(8,(R4)),ALIGN=LEFT                                         
*                                                                               
GETDEMX  XIT1                                                                   
         EJECT                                                                  
*=============================================================                  
* COMPRESS FIELDS AND INSERT DELIMITERS                                         
*=============================================================                  
                                                                                
BLDAV60  L     R0,=A(IQOUT)                                                     
         LHI   R1,IQOUTX-IQOUT                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,IQATAB                                                        
         L     R5,=A(IQOUT)                                                     
*                                                                               
BLDAV62  L     R1,0(R4)            GET DATA ADDRESS                             
         SR    RF,RF                                                            
         IC    RF,4(R4)            GET OUTPUT LEN                               
         TM    5(R4),X'80'         TEST FIXED LEN OUTPUT                        
         BO    BLDAV64                                                          
*                                                                               
         BRAS  RE,BACKUP           GET ACTUAL LEN TO MOVE IN RF                 
         L     R1,0(R4)            POINT TO DATA AGAIN                          
*                                                                               
BLDAV64  LTR   RF,RF                                                            
         BZ    BLDAV66                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R1)                                                    
         AHI   R5,1                RESTORE LEN                                  
*                                                                               
BLDAV66  AR    R5,RF               POINT TO NEXT OUTPUT POSN                    
         MVI   0(R5),FLDDELIM      INSERT DELIMITER                             
         AHI   R5,1                                                             
*                                                                               
         LA    R4,L'IQATAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   BLDAV62                                                          
*                                                                               
         LA    R0,IQOUTLN                                                       
         SR    R5,R0                                                            
         STH   R5,IQOUTLN                                                       
*                                                                               
         LA    R0,IQOUTLN                                                       
         PUT   IQFILE,(0)                                                       
         AP    AVLCOUNT,=P'1'                                                   
         J     EXIT                                                             
*                                                                               
BACKUP   AR    R1,RF               POINT TO END OF DATA                         
         BCTR  R1,0                POINT TO LAST CHAR                           
*                                                                               
BACKUP2  CLI   0(R1),C' '          TEST SIGNIFICANT DATA                        
         BHR   RE                                                               
         BCTR  R1,0                                                             
         BRCT  RF,BACKUP2                                                       
         BR    RE                                                               
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*===============================================================                
* BUILD SPOT RECORDS                                                            
*================================================================               
                                                                                
BLDSPOTS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING RCVRHDRD,R8                                                      
         MVI   ANYERRS,C'N'                                                     
*                                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         LA    R6,BDELEM                                                        
         XC    ELEMDATE,ELEMDATE                                                
*                                                                               
BLDSPT2  BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
*                                                                               
         TM    6(R6),X'80'         TEST MINUS                                   
         BO    BLDSPT4             YES - DON'T CHANGE SEQNUM                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R6)),WORK     GET YYMMDD                        
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         MVC   HALF,2(R6)          ASSUME IT WILL BE A MONDAY                   
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         CHI   R0,1                TEST MONDAY                                  
         BE    BLDSPT2X                                                         
*                                                                               
         BCTR  R0,0                SET DAYS BACK TO PREV MONDAY                 
         LCR   R0,R0               AND SET TO BACK UP !                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         MVC   WORK(6),WORK+6      MOVE YYMMDD TO WORK                          
         GOTO1 DATCON,DMCB,WORK,(2,HALF)                                        
*                                                                               
BLDSPT2X SR    R0,R0                                                            
         IC    R0,ELEMNO                                                        
         CLC   ELEMDATE,HALF                                                    
         BE    *+6                                                              
         SR    R0,R0                                                            
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
         MVC   ELEMDATE,HALF                                                    
*                                                                               
BLDSPT4 CLI    1(R6),10            TEST ALLOCATED                               
         BNH   BLDSPT2             NO                                           
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    BLDSPT2                                                          
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BO    BLDSPT2                                                          
*                                                                               
         XC    IQOUTLN,IQOUTLN     CLEAR OUTPUT RECORD LEN                      
         LA    R0,IQREC                                                         
         LHI   R1,IQRECX-IQREC                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   IQSPTSTA,BIGSTA                                                  
         MVC   IQSPTDID,LASTDID                                                 
*                                                                               
         L     RE,ADCLT                                                         
         AHI   RE,CLIST-CLTHDRD                                                 
*                                                                               
BLDSPT6  CLC   10(1,R6),3(RE)      MATCH PRD CODE                               
         BE    BLDSPT8                                                          
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   BLDSPT6                                                          
         DC    H'0'                                                             
*                                                                               
BLDSPT8  MVC   IQPRD,0(RE)                                                      
*                                                                               
BLDSPT10 GOTO1 DATCON,DMCB,(2,ELEMDATE),(10,IQDATE)                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ELEMNO                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  IQSEQNUM,DUB                                                     
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
         TM    BDCIND2,BDCNBRDQ    SET RATE IN DOLLARS                          
         BZ    *+8                                                              
         MHI   R0,100              YES, CONVERT TO PENNIES                      
         EDIT  (R0),(10,IQCOST),ALIGN=LEFT                                      
*                                                                               
BLDSPT12 TM    6(R6),X'20'         TEST COST OVERRIDE                           
         BZ    BLDSPT14                                                         
         SR    R0,R0                                                            
         ICM   R0,7,7(R6)                                                       
         BRAS  RE,CVD                                                           
         EDIT  (R0),(10,IQCOST),ALIGN=LEFT                                      
*                                                                               
BLDSPT14 TM    6(R6),X'40'         TEST MISSED                                  
         BZ    BLDSPT20                                                         
*                                                                               
         MVI   IQRESTYP,C'1'       SET PRE-EMPTED FLAG                          
         TM    6(R6),X'02'         TEST MAKEGOOD ON NEW LINE                    
         BZ    BLDSPT40                                                         
         MVI   IQRESTYP,C'2'       SET MADEGOOD                                 
         SR    R0,R0                                                            
         IC    R0,13(R6)           GET MAKEGOOD CODE                            
         BRAS  RE,TRANSCD                                                       
         B     BLDSPT40                                                         
*                                                                               
BLDSPT20 SR    R7,R7                                                            
         IC    R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   0(R7),X'10'         TEST AFFID FOLLOWS                           
         BNE   BLDSPT30                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R7)),(10,IQAIRDT)                               
*                                                                               
         ICM   R0,3,4(R7)                                                       
         N     R0,=X'00000FFF'                                                  
         BRAS  RE,CVDTIME                                                       
         MVC   IQAIRTIM(2),WORK+6                                               
         MVI   IQAIRTIM+2,C':'                                                  
         MVC   IQAIRTIM+3(2),WORK+8                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'12'         TEST AFFID FILM ELEM FOLLOWS                 
         BNE   BLDSPT40                                                         
*                                                                               
         GOTO1 BINSRCH,CMLPAR1,3(R7) GET FILM CODE                              
         MVC   IQISCI(8),=CL12'UNKNOWN'                                         
         MVC   IQADID,=CL12'UNKNOWN'                                            
         CLI   0(R1),0                                                          
         BNE   BLDSPT40                                                         
         L     RE,0(R1)                                                         
         USING MYCMLD,RE                                                        
         MVC   IQISCI,MYCMEBC       MOVE FILM CODE                              
         MVC   IQADID,MYCMADID      MOVE ADID                                   
         B     BLDSPT40                                                         
         DROP  RE                                                               
                                                                                
* NO AFFID - ERROR MESSAGE IF SPOT IS PAID                                      
                                                                                
BLDSPT30 OC    4(2,R6),4(R6)       TEST SPOT PAID                               
         BZ    BLDSPT40            NO - CONTINUE                                
         CLI   ANYERRS,C'Y'        TEST PREVIOUS ERROR THIS BUY                 
         BE    BLDSPT40                                                         
*                                                                               
         MVI   ANYERRS,C'Y'                                                     
         MVC   P+10(22),=C'PAID SPOTS NOT MATCHED'                              
         GOTO1 REPORT                                                           
*                                                                               
BLDSPT40 L     R4,=A(IQSTAB)                                                    
         L     R5,=A(IQOUT)                                                     
*                                                                               
BLDSPT42 L     R1,0(R4)            GET DATA ADDRESS                             
         SR    RF,RF                                                            
         IC    RF,4(R4)            GET OUTPUT LEN                               
         TM    5(R4),X'80'         TEST FIXED LEN OUTPUT                        
         BO    BLDSPT44                                                         
*                                                                               
         BRAS  RE,BACKUP           GET ACTUAL LEN TO MOVE IN RF                 
         L     R1,0(R4)            POINT TO DATA AGAIN                          
*                                                                               
BLDSPT44 LTR   RF,RF                                                            
         BZ    BLDSPT46                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R1)                                                    
         AHI   RF,1                RESTORE LEN                                  
*                                                                               
BLDSPT46 AR    R5,RF               POINT PAST END                               
         MVI   0(R5),FLDDELIM      INSERT DELIMITER                             
         AHI   R5,1                                                             
*                                                                               
         LA    R4,L'IQSTAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   BLDSPT42                                                         
*                                                                               
         LA    R0,IQOUTLN                                                       
         SR    R5,R0                                                            
         STH   R5,IQOUTLN                                                       
*                                                                               
         PUT   IQFILE,(0)                                                       
         AP    DTLCOUNT,=P'1'                                                   
         B     BLDSPT2                                                          
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
*        TRANSLATE MAKEGOOD CODE IN R0                                          
*=================================================================              
                                                                                
TRANSCD  NTR1                                                                   
         CHI   R0,X'FF'                                                         
         BNE   TC10                                                             
         MVC   IQMGCODE,=C'PR'                                                  
         B     TCX                                                              
*                                                                               
TC10     L     R5,ADBLOCK                                                       
         USING MGABLKD,R5                                                       
*                                                                               
         XC    0(MGALNQ,R5),0(R5)                                               
*                                                                               
         MVI   MGAACT,MGAQTRNS                                                  
         MVC   MGAACOM,ACOMFACS    SET A(COMFAS)                                
         LA    RE,BUYREC                                                        
         ST    RE,MGAIO                                                         
         ST    R6,MGAELEM                                                       
*                                                                               
TC15     GOTO1 VBLDMGN,MGABLKD                                                  
*                                                                               
         MVC   IQMGCODE,MGAENTRY+MGECODE-MGENTRYD                               
TCX      J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* PRINT A LINE TO IDENTIFY ACTIVITY                                             
*===========================================================                    
         SPACE 1                                                                
*                                                                               
PRTBUY   NTR1  BASE=*,LABEL=*                                                   
         L     R8,ADBUY                                                         
         USING RCVRHDRD,R8                                                      
*                                                                               
         MVC   PAGY,BUYALPHA                                                    
*                                                                               
         SR    RE,RE                                                            
         IC    RE,BUYREC                                                        
         N     RE,=X'00000007'     DROP AGY                                     
         LA    RE,MDTAB-1(RE)                                                   
         MVC   PMED,0(RE)                                                       
*                                                                               
         L     RE,ADCLT                                                         
         SR    R0,R0                                                            
         IC    R0,CPROF+6-CLTHDRD(RE)                                           
         GOTO1 CLUNPK,DMCB,((R0),BUYREC+1),PCLT                                 
*                                                                               
         BAS   RE,GETSTA                                                        
         MVC   PSTA,BIGSTA       MOVE PRINTABLE STATION                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         IC    R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         TM    BUYREC+15,X'80'                                                  
         BZ    *+10                                                             
         MVC   PERR(7),=C'DELETED'                                              
*                                                                               
PRTBUYX  XIT1                                                                   
*                                                                               
MDTAB    DC    C'TRNX'                                                          
         LTORG                                                                  
         EJECT                                                                  
GETSTA   NTR1  BASE=*,LABEL=*                                                   
         L     R8,ADBUY                                                         
         AHI   R8,24                                                            
         USING BUYREC,R8                                                        
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         XC    STAWORK,STAWORK                                                  
         MVI   STAPACT,C'U'                                                     
         MVI   STAPCTRY,C'U'                                                    
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,BUYREC+4                                                
*                                                                               
         GOTO1 VSTAPACK,STAWORK                                                 
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BIGSTA(8),STAPQSTA                                               
         J     EXIT                                                             
         DROP  R1,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'SPDQ02WK'                                                    
SPDQ02WK DS    0D                                                               
*                                                                               
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
DDSFLAG  DC    X'00'                                                            
HDRSW    DC    C'N'                                                             
ANYERRS  DS    C                                                                
FLDDELIM EQU   C'|'                                                             
RECDELIM DC    C'@&&\N'                                                         
LASTMEST DC    X'00'                                                            
LASTYYMM DC    XL2'00'                                                          
LASTUID  DC    XL8'00'                                                          
LASTDID  DC    XL8'00'                                                          
STAWORK  DS    XL32                                                             
ELEMDATE DS    XL2                                                              
ELEMNO   DS    XL1                                                              
DEMNAMES DS    CL120               20 6-BYTE DEMO NAMES                         
         DS    0D                                                               
CMLWORK  DS    XL22                                                             
CMLTBID  DS    XL4                 X'00'/A-M/CLT OF CURRENT TABLE               
*                                                                               
         DS    0D                                                               
CMLPAR1  DC    A(0)                                                             
CMLPAR2  DC    A(MYCMLTAB)           A(TABLE)                                   
CMLPAR3  DC    F'0'                RECORD COUNT                                 
CMLPAR4  DC    A(MYCMLNQ)          RECORD LENGTH                                
CMLPAR5  DC    A(2)                KEYDSPL/KEYLEN                               
CMLPAR6  DC    A((MYCMLTBX-MYCMLTAB)/L'MYCMLTAB)                                
                                                                                
         DS    0D                                                               
COUNTS   DS    0XL24                                                            
AVLCOUNT DC    PL4'0',CL20'AVAILS OUT'                                          
DTLCOUNT DC    PL4'0',CL20'DETAILS OUT'                                         
COUNTX   EQU   *                                                                
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=6100,             X        
               MACRF=GM,EODAD=GETSORT                                           
*                                                                               
IQFILE   DCB   DDNAME=IQFILE,DSORG=PS,RECFM=VB,LRECL=1024,MACRF=PM              
*                                                                               
         DS    0D                                                               
EOFLN    DC    AL2(EOFLNX-EOFLN)   FILE SEPARATOR RECORD                        
         DC    H'0'                                                             
         DC    CL16'*EOF*EOF*EOF*EOF'                                           
EOFLNX   EQU   *                                                                
*                                                                               
* SORT ON DM$MSTR/DM$YYMM/BUYKEY DM$MSTR IS IN RCVHDR+21                        
* LEN(2) 2X'00' 21 RCVHDR = 25+1 FOR COLNUM                                     
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(26,16,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=6100'                                  
*                                                                               
THISBEAH DC    C'H'                                                             
THISBEAA DC    C'A'                                                             
THISBEAD DC    C'D'                                                             
         EJECT                                                                  
*=====================================================                          
* IQ RECORD LAYOUTS                                                             
*=====================================================                          
         DS    0D                                                               
         DC    CL8'**IQREC*'                                                    
IQREC    DS    1024X                                                            
IQRECX   EQU   *                                                                
         ORG   IQREC                                                            
IQHDR    DS    0D                  FIRST RECORD ON AVAIL FILE                   
*                                  EACH FIELD FOLLOWED BY A DELIMITER           
IQTYPE   DC    C'H'                                                             
IQMED    DS    CL1                                                              
IQCLT    DS    CL3                                                              
IQEST    DS    CL3                                                              
IQMONTH  DS    CL2                                                              
IQYEAR   DS    CL4                                                              
IQHDRDEM DS    20CL6                                                            
*                                                                               
         ORG   IQREC                                                            
IQAVLHDR DS    0D                                                               
IQAVLUID DS    CL8                 IQ UID                                       
IQAVLSTA DS    CL8                                                              
IQAVLDID DS    CL8                 DDS UID                                      
IQAVLROT DS    CL7                 MTWTFSS                                      
IQAVLSTR DS    CL5                 START TIME HH:MM                             
IQAVLEND DS    CL5                 END TIME HH:MM                               
IQAVLDPT DS    CL1                 DAYPART                                      
IQAVLSLN DS    CL3                 SLN                                          
IQAVLPRG DS    CL17                PROGRAM NAME                                 
IQAVLCOS DS    CL10                COST IN PENNIES                              
IQAVLCTY DS    CL1                 COST TYPE (BLANK OR P)                       
IQAVLCOM DS    CL255               COMMENTS                                     
IQAVLRES DS    CL255               DISCREPANCY RESOLUTION COMMENTS              
IQAVLMG  DS    CL2                 MAKEGOOD ID                                  
IQAVLDEM DS    20CL8               MAX 20 8-BYTE DEMO VALUES (2 DEC)            
         ORG   IQAVLDEM                                                         
IQAVDM01 DS    CL8                                                              
IQAVDM02 DS    CL8                                                              
IQAVDM03 DS    CL8                                                              
IQAVDM04 DS    CL8                                                              
IQAVDM05 DS    CL8                                                              
IQAVDM06 DS    CL8                                                              
IQAVDM07 DS    CL8                                                              
IQAVDM08 DS    CL8                                                              
IQAVDM09 DS    CL8                                                              
IQAVDM10 DS    CL8                                                              
IQAVDM11 DS    CL8                                                              
IQAVDM12 DS    CL8                                                              
IQAVDM13 DS    CL8                                                              
IQAVDM14 DS    CL8                                                              
IQAVDM15 DS    CL8                                                              
IQAVDM16 DS    CL8                                                              
IQAVDM17 DS    CL8                                                              
IQAVDM18 DS    CL8                                                              
IQAVDM19 DS    CL8                                                              
IQAVDM20 DS    CL8                                                              
         ORG                                                                    
IQFUTURE DS    CL10                                                             
*                                                                               
* ONE RECORD FOR EACH SPOT                                                      
*                                                                               
         ORG   IQREC                                                            
IQSPTSTA DS    CL8                                                              
IQSPTDID DS    CL8                 DDS UID                                      
IQPRD    DS    CL3                 PRODUCT CODE                                 
IQDATE   DS    CL8                 MM/DD/YY                                     
IQAIRDT  DS    CL8                 MM/DD/YY AFFID DATE                          
IQAIRTIM DS    CL5                 HH:MM AFFID TIME                             
IQRESTYP DS    CL1                 1=PRE-EMPTED/2=MADEGOOD                      
IQCOST   DS    CL10                COST PER SPOT IF DDS BUY                     
IQMGCODE DS    CL2                 MAKEGOOD CODE IF IQRESTYP=2                  
IQISCI   DS    CL8                 FILM CODE                                    
IQSEQNUM DS    CL2                 SPOT SEQUENCE NUMBER THIS DATE               
IQADID   DS    CL12                AD-ID                                        
*                                                                               
         ORG                                                                    
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*IQATAB*'                                                    
* A(DATA)                                                                       
* L'DATA                                                                        
* IND 1  X'80' = FIXED LEN OUTPUT                                               
* IND 2                                                                         
* IND 3                                                                         
*                                                                               
IQATAB   DS    0XL8                                                             
         DC    A(THISBEAA),AL1(1),X'80',2X'00'   RECORD ID = AVAIL              
         DC    A(IQAVLUID),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVLSTA),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVLDID),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVLROT),AL1(7),X'80',2X'00'                                  
         DC    A(IQAVLSTR),AL1(5),X'80',2X'00'                                  
         DC    A(IQAVLEND),AL1(5),X'80',2X'00'                                  
         DC    A(IQAVLDPT),AL1(1),X'80',2X'00'                                  
         DC    A(IQAVLSLN),AL1(3),X'00',2X'00'                                  
         DC    A(IQAVLPRG),AL1(17),X'00',2X'00'                                 
         DC    A(IQAVLCOS),AL1(10),X'00',2X'00'                                 
         DC    A(IQAVLCTY),AL1(1),X'00',2X'00'                                  
         DC    A(IQAVLCOM),AL1(255),X'00',2X'00'                                
         DC    A(IQAVLRES),AL1(255),X'00',2X'00'                                
         DC    A(IQAVLMG),AL1(2),X'00',2X'00'                                   
         DC    A(IQAVDM01),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM02),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM03),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM04),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM05),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM06),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM07),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM08),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM09),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM10),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM11),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM12),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM13),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM14),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM15),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM16),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM17),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM18),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM19),AL1(8),X'00',2X'00'                                  
         DC    A(IQAVDM20),AL1(8),X'00',2X'00'                                  
         DC    A(IQFUTURE),AL1(10),X'00',2X'00'  FOR FUTURE USE                 
         DC    A(IQFUTURE),AL1(10),X'00',2X'00'  FOR FUTURE USE                 
         DC    X'FF'                                                            
*                                                                               
         DS    0D                                                               
IQSTAB   DS    0XL8                                                             
         DC    A(THISBEAD),AL1(1),X'80',2X'00' RECORD ID = DETAIL               
         DC    A(IQSPTSTA),AL1(8),X'00',2X'00'                                  
         DC    A(IQSPTDID),AL1(8),X'00',2X'00'                                  
         DC    A(IQPRD),AL1(3),X'00',2X'00'                                     
         DC    A(IQDATE),AL1(8),X'00',2X'00'                                    
         DC    A(IQAIRDT),AL1(8),X'00',2X'00'                                   
         DC    A(IQAIRTIM),AL1(5),X'00',2X'00'                                  
         DC    A(IQRESTYP),AL1(1),X'00',2X'00'                                  
         DC    A(IQCOST),AL1(10),X'00',2X'00'                                   
         DC    A(IQMGCODE),AL1(2),X'00',2X'00'                                  
         DC    A(IQISCI),AL1(8),X'00',2X'00'                                    
         DC    A(IQSEQNUM),AL1(2),X'00',2X'00'                                  
         DC    A(IQADID),AL1(12),X'00',2X'00'                                   
         DC    A(IQFUTURE),AL1(10),X'00',2X'00'  FOR FUTURE USE                 
         DC    X'FF'                                                            
*                                                                               
IQOUTLN  DS    H                                                                
         DS    H                                                                
IQOUT    DS    1024X                                                            
IQOUTX   EQU   *                                                                
*                                                                               
ESTDEMS  DS    256XL60             SAVED ESTHDR DEMOS                           
ESTDEMX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'MYCMLTAB'                                                    
MYCMLTAB DS    10000CL(MYCMLNQ)                                                 
MYCMLTBX EQU   *                                                                
*                                                                               
MYCMLD   DSECT                                                                  
MYCMSEQ  DS    XL2                 BINARY SEQUENCE NUMBER                       
MYCMEBC  DS    CL8                 EBCDIC COMMERCIAL                            
MYCMADID DS    CL12                12-CHAR ADID                                 
MYCMLNQ  EQU   *-MYCMLD                                                         
         EJECT                                                                  
RCVRHDRD DSECT                                                                  
*PREFIX=DM$                                                                     
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
         ORG   DM$RAG              ORG BACK TO CREATE SORT FIELDS               
DM$MSTR  DS    XL1       MASTER EST                                             
DM$YYMM  DS    XL2       BINARY Y/M                                             
       ++INCLUDE SPGENBUY          NOTE BUYREC CONTINUES RCVRHDRD               
         EJECT                                                                  
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CMLRECD  DSECT                                                                  
       ++INCLUDE SPTRCMML                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
       ++INCLUDE SPMGADN                                                        
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
* DSECT FOR OUTPUT PRINT LINE                                                   
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    C                                                                
PMED     DS    CL1                                                              
         DS    C                                                                
PCLT     DS    CL3                                                              
         DS    C                                                                
PPRD     DS    CL3                                                              
         DS    C                                                                
PSTA     DS    CL7                                                              
         DS    C                                                                
PEST     DS    CL3                                                              
         DS    C                                                                
PLIN     DS    CL3                                                              
         DS    C                                                                
PDATE    DS    CL7                                                              
         DS    C                                                                
PERR     DS    CL20                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034SPREPDQ02 10/04/13'                                      
         END                                                                    
