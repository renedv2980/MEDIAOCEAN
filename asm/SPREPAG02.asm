*          DATA SET SPREPAG02  AT LEVEL 027 AS OF 06/18/07                      
*PHASE SPAG02A                                                                  
*INCLUDE DDUNFLD                                                                
*INCLUDE CASHVAL                                                                
                                                                                
*======================================================================         
* 15JUN07 AHYD *B is invalid alpha for a file, change it to @B (DDSB)           
* 22JAN07 AHYD Make format name for assigned different the regular              
* 17NOV06 AHYD Added option to report assigned dollars (NETWORK)                
* 02NOV06 AHYD Added option to be able to create =NWK pointer only              
* 11SEP06 MHER WRITE DATA TO BSAM FILE                                          
*              WORKER FILE GETS 1 RECORD WITH FILE SIZE/DSN                     
* AS OF MAR/05 SEND SAME MONTH SEQNUMS FOR ALL SYSTEMS                          
*======================================================================         
                                                                                
*======================================================================         
* INPUT FILE HAS A HEADER WITH THE FOLLOWING FORMAT                             
* CL6 *A9HDR HEADER IDENTIFIER                                                  
* CL4 SYSTEM (SPOT,NET,PRNT)                                                    
* CL6 YYMMDD GENERATION DATE                                                    
* CL6 YYMMDD FIRST YY/MM                                                        
* CL6 YYMMDD LAST  YY/MM                                                        
*======================================================================         
                                                                                
*======================================================================         
* Avaliable options                                                             
*----------------------------------------------------------------------         
* QOPT1 -- C'Y' PRINT OUTBLK record                                             
* QOPT1 -- C'P' PRINT WORK RECORDS - DO NOT CREATE FILE                         
* QOPT1 -- C'S' Create =NWK file with DSN only using SHIP= in JCL               
* QOPT3 -- C'A' Report Ordered assigned for NETWORK                             
*======================================================================         
                                                                                
         TITLE 'SPAG02 - GENERATE ACCENT DOWNLOAD FILE'                         
         USING SPAGWORK,RC                                                      
         USING SPWORKD,RA,R9                                                    
         USING FILEHDRD,FILEHDR                                                 
SPAG02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPAG02                                                         
         L     RC,=A(SPAGWORK)                                                  
         L     RA,0(,R1)                                                        
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
*                                                                               
         USING MASTD,R2                                                         
         USING LOGOD,R3                                                         
         CLI   QOPT1,C'S'          Create =NWK pointer DSN only                 
         BNE   SPAG1                                                            
         L     R2,VMASTC                                                        
         L     R3,MCVLOGOC                                                      
         MVC   OUTDSN,LOGOINFO                                                  
         B     SPAG7                                                            
         DROP  R2,R3                                                            
                                                                                
SPAG1    BRAS  RE,INIT                                                          
         MVI   SENTSYS,NO          FORCE SYSTEM TO BE SENT                      
         MVI   QSYS,SPOTQ                                                       
         BRAS  RE,REQF             Set system information                       
         BRAS  RE,DATAOUT          Put out data records                         
         BRAS  RE,REQL             Close input file                             
*                                                                               
         MVI   QSYS,NETQ                                                        
         BRAS  RE,REQF             Set system information                       
         BRAS  RE,DATAOUT          Put out data records                         
         BRAS  RE,REQL             Close input file                             
*                                                                               
         MVI   QSYS,PRINTQ                                                      
         BRAS  RE,REQF             Set system information                       
         BRAS  RE,DATAOUT          Put out data records                         
         BRAS  RE,REQL             Close input file                             
*                                                                               
         MVI   EOFFLAG,YES         SET EOFFLAG                                  
         BRAS  RE,PUTFILE          WRITE LAST OUTPUT BLOCK                      
         LA    R4,FILEOUT                                                       
         CLOSE ((R4))                                                           
*                                                                               
SPAG7    BRAS  RE,NWKFILE                                                       
                                                                                
         MVC   P(11),=C'OUTPUT DSN='                                            
         MVC   P+11(35),OUTDSN                                                  
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 AENDREQ                                                          
         J     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*=====================================================================*         
* Initialize areas and tables                                                   
*=====================================================================*         
INIT     NTR1                                                                   
                                                                                
         USING SPADCONS,RE                                                      
         L     RE,ADCONLST                                                      
         MVC   OFFICER,VOFFICER                                                 
         DROP  RE                                                               
                                                                                
         L     R2,=A(OUTBLK)       INITIALIZE OUTPUT BLOCK                      
         XC    0(4,R2),0(R2)                                                    
         LHI   R0,4                                                             
         STH   R0,0(R2)                                                         
         ZAP   PKZERO,=P'0'                                                     
*                                                                               
         MVC   OUTAGYA,AGY         SET ALPHA AGY                                
         CLI   OUTAGYA,C'*'        Not valid DDSB fix                           
         JNE   *+8                                                              
         MVI   OUTAGYA,C'@'        Change the character                         
         MVI   OUTDATE,C'D'                                                     
         GOTO1 DATCON,DMCB,(5,0),(X'20',OUTDATE+1)                              
*                                                                               
         TIME  DEC                 R0 = HHMMSSTH                                
         SRL   R0,4                R0 = 0HHMMSST                                
         O     R0,=X'0000000F'     R0 = 0HHMMSSF                                
         ST    R0,FULL                                                          
         MVI   OUTTIME,C'T'                                                     
         UNPK  DUB,FULL            TIME IN HHMMSS                               
         MVC   OUTTIME+1(6),DUB+2                                               
*                                                                               
         USING AMTTABD,R2                                                       
         LA    RE,FIXAMTS                                                       
         SR    R2,R2                                                            
INIT010  CLI   0(RE),EOT           End of table                                 
         BE    INIT020                                                          
         ICM   R2,7,1(RE)                                                       
         OI    AMTIND1,AMTIHIDE    Set to hide column                           
         CLC   QOPT3,0(RE)         Match on optin                               
         BNE   *+8                                                              
         NI    AMTIND1,TURNOFF-AMTIHIDE  If matches then show                   
         LA    RE,4(,RE)                                                        
         B     INIT010                                                          
         DROP  R2                                                               
                                                                                
**********************************************************************          
* Initialize schema based on options, Net vs. Gross and Assigned$$              
**********************************************************************          
INIT020  LA    RE,FIXSCHMA         Change schema based on QOPT3                 
INIT022  CLI   0(RE),EOT           End of table                                 
         BE    INIT030                                                          
         SR    R2,R2                                                            
         ICM   R2,7,1(RE)          Get table entry                              
         MVI   2(R2),TURNOFF       Mark as inactive                             
         CLC   QOPT3,0(RE)         Support Assigned amounts ?                   
         BNE   *+8                 No, so schema okay                           
         MVI   2(R2),00            Restore this entry                           
         LA    RE,4(,RE)                                                        
         B     INIT022                                                          
                                                                                
INIT030  GOTO1 DATCON,DMCB,(5,0),(8,FE00DATE)                                   
         L     RE,=A(FE12CFMT)                                                  
         MVC   0(1,RE),QOPT3       Set to assigned or not                       
         MVC   FE0ADATE,FE00DATE                                                
         MVC   FE00$,=CL7'Net $'                                                
         MVC   FE0A$,=CL7'Net $'                                                
         MVC   OUT$TYP,=CL3'NET'                                                
         CLI   QOPT2,FILE$NET      Net amounts?                                 
         BE    INIT040             Yes                                          
         MVI   QOPT2,FILE$GRS      Set default                                  
         MVC   OUT$TYP,=CL3'GRS'   No                                           
         MVC   FE00$,=CL7'Gross $'                                              
         MVC   FE0A$,=CL7'Gross $'                                              
*                                                                               
INIT040  L     R4,=A(HDTAB)        Adjust column headings                       
INIT042  CLI   0(R4),EOT           End of table                                 
         BE    INIT050                                                          
         L     RE,0(,R4)                                                        
         MVC   0(5,RE),=CL5'Net'                                                
         CLI   QOPT2,FILE$NET      Net amounts                                  
         JE    *+10                Yes                                          
         MVC   0(5,RE),=CL5'Gross' No, gross amounts                            
         AHI   R4,L'HDTAB                                                       
         B     INIT042                                                          
                                                                                
INIT050  CLI   QOPT1,C'P'          Test printing only                           
         BE    INIT100             Yes                                          
         GOTO1 DYNALLOC,DMCB,(X'80',OUTDDNAM),(X'43',OUTSIZE),OUTDSN            
         LA    R4,FILEOUT                                                       
         OPEN  ((R4),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
INIT100  L     R4,=A(SCHTABLE)                                                  
         BRAS  RE,SCHOUT           Put out schema details                       
         L     R4,=A(REPTABLE)                                                  
         BRAS  RE,SCHOUT           Put out report details                       
         J     EXIT                                                             
         EJECT ,                                                                
*=====================================================================*         
* Set system data for system request                                            
*=====================================================================*         
         SPACE 1                                                                
REQF     NTR1                                                                   
         MVI   ASSIGNED,NO                                                      
         MVC   YYMMLEN,=AL2(THISYYM1-THISYYMM)                                  
*        MVC   DSNALPHA,QAGY                                                    
*        MVC   DSNTYPE,QOPT2       Gross or Net                                 
         CLI   QSYS,NETQ           TEST DOWNLOAD INPUT FORMAT                   
         BE    REQF010                                                          
         CLI   QSYS,SPOTQ          TEST DOWNLOAD INPUT FORMAT                   
         BE    REQF020                                                          
         CLI   QSYS,PRINTQ         TEST DOWNLOAD INPUT FORMAT                   
         BE    REQF030                                                          
         DC    H'00'                                                            
                                                                                
REQF010  LA    R4,NETIN                                                         
         LA    R5,DDSPOTIN                                                      
         MVC   DSNSYS,=CL3'NET'                                                 
         MVI   LMTTABSY,2                                                       
         CLI   QOPT3,C'A'                                                       
         BNE   REQF050                                                          
         MVC   YYMMLEN,=AL2(THISYYM2-THISYYMM)                                  
         MVI   ASSIGNED,YES                                                     
         B     REQF050                                                          
*                                                                               
REQF020  LA    R4,SPOTIN                                                        
         LA    R5,DDSPOTIN                                                      
         MVC   DSNSYS,=CL3'SPT'                                                 
         MVI   LMTTABSY,1                                                       
         B     REQF050                                                          
*                                                                               
REQF030  LA    R4,PRNTIN                                                        
         LA    R5,DDPRNTIN                                                      
         MVC   DSNSYS,=CL3'PRT'                                                 
         MVI   LMTTABSY,3                                                       
*                                                                               
REQF050  LTR   R5,R5                                                            
*        BZ    REQF052                                                          
*        GOTO1 DYNALLOC,DMCB,DDSPOTIN,(X'FE',DSNPARM),(X'80',0)                 
                                                                                
REQF052  ST    R4,AFILEIN                                                       
         OPEN  ((R4),(INPUT))                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AFILEIN                                                       
         LA    R0,FILEREC          1st, file header or blank for NET            
         GET   (1),(0)                                                          
*                                                                               
         MVC   FILEHDR(FILEHDRQ),FILEREC                                        
         CLI   QSYS,NETQ                                                        
         BNE   REQF060                                                          
         MVC   FILERCDE(5),=C'A9HDR'                                            
         MVC   FILESYS,=CL4'NET '                                               
         MVI   FILESYSC,NETQ                                                    
         MVC   FILE$TYP,QOPT2                                                   
                                                                                
REQF060  CLC   =C'A9HDR',FILERCDE  Report code type                             
         BE    *+6                 Must match                                   
         DC    H'0'                                                             
         CLI   QSYS,SPOTQ                                                       
         BNE   REQF070                                                          
         CLC   FILE$TYP,QOPT2      Must match now                               
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
*===================================================================            
* Output limited access values                                                  
*===================================================================            
REQF070  LA    RE,LMTRECL                                                       
         SR    R5,R5                                                            
         ICM   R5,3,0(RE)                                                       
         BCTR  R5,0                                                             
         EXMVC R5,WRKIOL,0(RE)                                                  
         BRAS  RE,PUTFILE                                                       
                                                                                
         LHI   R0,-18                                                           
         GOTO1 ADDAY,DMCB,(C'M',TODAY),FILEQSTR,(R0)  BACKWARDS                 
*                                                                               
         LPR   R0,R0                                                            
         GOTO1 (RF),(R1),,FILEQEND,(R0)               FORWARDS                  
                                                                                
*==============================================================                 
* SET BEFORE AND AFTER MONTH DATES AS WELL                                      
*==============================================================                 
                                                                                
         LHI   R0,-1                                                            
         GOTO1 (RF),(R1),(C'M',FILEQSTR),QBEFORE,(R0)                           
*                                                                               
         LHI   R0,1                                                             
         GOTO1 (RF),(R1),(C'M',FILEQEND),QAFTER,(R0)                            
*                                                                               
REQFX    LHI   R0,15               MAX MONTHS IN A REC FOR NET/SPOT             
         CLI   QSYS,PRINTQ         TEST PRINT                                   
         BNE   *+8                                                              
         LHI   R0,21                                                            
         STH   R0,NMONTHS                                                       
         J     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* AT END, CLOSE INPUT, OUTPUT FILES                                             
*=================================================================              
REQL     NTR1                                                                   
         CLI   QOPT1,C'P'          TEST PRINT ONLY                              
         BE    REQL2                                                            
*                                                                               
         L     R4,AFILEIN                                                       
         CLOSE ((R4))                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* PUT OUT LAST TOTALS                                                           
*                                                                               
REQL2    LA    R1,CLTORD                                                        
         BRAS  RE,PRTTOTS                                                       
*                                                                               
         MVC   TOTCLT,=C'***'                                                   
         LA    R1,MEDORD                                                        
         BRAS  RE,PRTTOTS                                                       
*                                                                               
         MVI   TOTMED,C'*'                                                      
         LA    R1,AGYTOTS                                                       
         BRAS  RE,PRTTOTS                                                       
         J     EXIT                                                             
         EJECT                                                                  
*===================================================================            
* OUTPUT SCHEMA/REPORT DESC TO WORKER FILE                                      
* ON ENTRY R4 POINTS TO APPROPRIATE TABLE                                       
*===================================================================            
                                                                                
SCHOUT   NTR1                                                                   
SCH2     SR    R5,R5                                                            
         ICM   R5,3,0(R4)                                                       
         BCTR  R5,0                                                             
         CLI   2(R4),TURNOFF       If set then skip                             
         BE    SCH4                Not zero so skip it                          
         EXMVC R5,WRKIOL,0(R4)                                                  
         BRAS  RE,PUTFILE                                                       
*                                                                               
SCH4     LA    R4,1(R5,R4)                                                      
         CLI   0(R4),EOT                                                        
         BNE   SCH2                                                             
         J     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
* New worker file to point to BSAM file                                         
*---------------------------------------------------------------------*         
NWKFILE  NTR1                                                                   
         BRAS  RE,WRKROPEN                                                      
*                                                                               
         LA    R4,FE0A             Used assigned version                        
         CLI   QOPT3,C'A'                                                       
         BE    *+8                                                              
         LA    R4,FE00             Used regular version                         
         SR    R5,R5               OUTPUT FILE DESC TO WORKER FILE              
         ICM   R5,3,0(R4)                                                       
         BCTR  R5,0                                                             
         EXMVC R5,WRKIOL,0(R4)                                                  
*                                                                               
         LH    RE,WRKIOL                                                        
         LA    RE,WRKIOL(RE)       POINT TO END OF RECORD                       
         BCTR  RE,0                BACK UP OVER X'00' REC TERMINATOR            
         MVI   0(RE),X'1A'         ELEM CODE                                    
         LHI   R0,46                                                            
         STCM  R0,3,1(RE)                                                       
         MVC   3(4,RE),OUTCNT                                                   
         MVC   7(4,RE),OUTBYTES                                                 
         MVC   11(35,RE),OUTDSN                                                 
         MVI   46(RE),0            TERMINATE BEAKUS                             
*                                                                               
         LH    R0,WRKIOL                                                        
         AHI   R0,46                                                            
         STH   R0,WRKIOL           LEN IS SAME + 46-X'00' JUST MOVED            
*                                                                               
         BRAS  RE,WRKR                                                          
         BRAS  RE,WRKRCLSE                                                      
         J     EXIT                                                             
         EJECT                                                                  
*===================================================================            
* Tables and LTORG                                                              
*===================================================================            
FIXAMTS  DC    C'A',AL3(AMTOASNG)                                               
         DC    C' ',AL3(AMTUNBD1)                                               
         DC    C'A',AL3(AMTUNBD2)                                               
         DC    AL1(EOT)                                                         
                                                                                
FIXSCHMA DC    C' ',AL3(REP24)                                                  
         DC    C'A',AL3(REP25)                                                  
         DC    C'A',AL3(REP26)                                                  
         DC    C'A',AL3(MPID19)                                                 
         DC    C' ',AL3(MPID20)                                                 
         DC    C'A',AL3(MPID21)                                                 
         DC    C' ',AL3(FE00)                                                   
         DC    C'A',AL3(FE0A)                                                   
         DC    AL1(EOT)                                                         
                                                                                
*====================================================================           
* THIS TABLE DEFINES THE MAP-IDS OF THE FIELDS USED IN LIMIT ACCESS             
*====================================================================           
                                                                                
* ENTRIES ARE AL1(DDLINK DATA TYPE CODE)                                        
*             AL2(MAPID)  >201 ARE FIELDS NOT IN DATA RECORDS                   
*                                                                               
         DS    0D                                                               
         DS    XL4                                                              
LMTRECL  DC    AL2(LMTTABX-LMTRECL+1) RECLEN INCLUDING LENGTH FIELD             
         DC    AL2(0)                                                           
*                                                                               
LMTTAB   DC    X'14',AL2(LMTTABX-LMTTAB)                                        
LMTTABSY DC    AL1(0)              LMTACCS METH 01=SP,02=NE,03=PRINT            
         DC    AL2(8)              RECORD MAP CODE                              
         DC    AL1(1),AL2(201)     AGY/MD (SPOT/NET), MED (PRINT)               
         DC    AL1(2),AL2(202)     OFFICE                                       
         DC    AL1(3),AL2(203)     QCLT                                         
         DC    AL1(4),AL2(204)     BCLT                                         
         DC    AL1(5),AL2(205)     CLT LIMIT ACCESS LIST                        
         DC    AL1(0)                                                           
LMTTABX EQU    *                                                                
         DC    AL1(0)              EOR                                          
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* Put data to BSAM file output                                                  
*===================================================================            
PUTFILE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   QOPT1,C'P'          TEST PRINT ONLY                              
         JE    EXIT                                                             
*                                                                               
         L     R2,=A(OUTBLK)       POINT TO START OF BLK                        
         CLI   EOFFLAG,YES                                                      
         BE    PUTFIL2                                                          
         LH    R0,0(R2)            GET CURRENT BLKSIZE                          
         AH    R0,WRKIOL           ADD LENGTH OF CURRENT RECORD                 
         CHI   R0,OUTBLKX-OUTBLK-4  TEST RECORD WILL FIT                        
         BL    PUTFIL10            YES                                          
                                                                                
* WRITE THIS BLOCK NOW                                                          
                                                                                
PUTFIL2  WRITE OUTWRITE,SF,FILEOUT,(R2),MF=E                                    
         WAIT  ECB=OUTECB                                                       
         CHECK OUTECB                                                           
*                                                                               
         L     R0,OUTBYTES                                                      
         AH    R0,0(R2)                                                         
         ST    R0,OUTBYTES                                                      
*                                                                               
         L     R0,OUTCNT                                                        
         AHI   R0,1                                                             
         ST    R0,OUTCNT                                                        
*                                                                               
         LHI   R0,4                                                             
         STH   R0,0(R2)            INITIALIZE NEW BLOCK                         
*                                                                               
PUTFIL10 CLI   EOFFLAG,YES                                                      
         JE    EXIT                                                             
*                                                                               
         L     RE,=A(OUTBLK)                                                    
         AH    RE,0(RE)            POINT TO NEXT RECORD POSN                    
         LH    RF,WRKIOL           GET LENGTH OF MOVE                           
         LA    R0,WRKIOL           FROM ADDRESS                                 
         LR    R1,RF               FROM LEN = TO LEN                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,=A(OUTBLK)       POINT TO OUTPUT BLOCK                        
         LH    R0,0(R2)            GET LENGTH SO FAR                            
         AH    R0,WRKIOL           ADD LENGTH OF NEW RECORD                     
         STH   R0,0(,R2)           AND STORE IN BLOCK                           
*                                                                               
         CLI   QOPT1,YES           TEST TO PRINT AS WELL                        
         JNE   EXIT                                                             
         CLI   EOFFLAG,YES                                                      
         JE    EXIT                                                             
         LA    R4,WRKIOL                                                        
         LA    R0,P                                                             
         LH    R1,0(,R4)                                                        
         CHI   R1,1000                                                          
         BNH   *+8                                                              
         LHI   R1,1000                                                          
         LH    R5,0(,R4)                                                        
         MVCL  R0,R4                                                            
*                                                                               
         GOTO1 REPORT                                                           
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* OUTPUT DATA RECORDS TO WORKER FILE                                            
* START BY BUILDING A TABLE TO ASSIGN SEQUENCE NUMBERS TO MONTHS                
*===================================================================            
DATAOUT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,DMCB,FILEQEND,(3,FULL)   SAVE 3BYTE YMD END DATE          
*                                                                               
         LA    R4,MONTAB                                                        
         LHI   R5,48               SET MAX MONTHS                               
         ZAP   HALF,=P'1'          SET SEQNUM                                   
         MVC   WORK(4),FILEQSTR    MOVE EBCDIC START DATE                       
         MVC   WORK+4(2),=C'15'                                                 
*                                                                               
DAT2     GOTO1 DATCON,DMCB,WORK,(3,WORK+6) GET 3 BYTE BINARY                    
         CLC   WORK+6(3),FULL              REACHED END OF PERIOD                
         JH    DAT4                                                             
*                                                                               
         GOTO1 (RF),(R1),(3,WORK+6),WORK+12    THEN BACK TO EBCDIC              
         MVC   WORK(6),WORK+12                                                  
*                                                                               
         OI    HALF+1,X'0F'                                                     
         UNPK  0(2,R4),HALF        SET SEQNUM IN TABLE                          
         MVC   2(4,R4),WORK+12     SET YYMM IN TABLE (Y2K FORMAT)               
*                                                                               
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,1  ADD A MONTH                     
         MVC   WORK(6),WORK+6                                                   
*                                                                               
         AP    HALF,=P'1'                                                       
         AHI   R4,6                                                             
         BRCT  R5,DAT2                                                          
*                                                                               
DAT4     MVC   0(2,R4),=C'99'      SET EOT FLAG                                 
         MVC   2(4,R4),=X'FFFFFFFF' SET HIGH END DATE                           
         LA    R0,SAVEREC                                                       
         LHI   R1,SAVERECX-SAVEREC                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SO NO RANDOM MATCHES                   
*                                                                               
         CLI   QSYS,NETQ           TEST NETPAK (DLFILE INPUT)                   
         JNE   DAT10               Yes Net                                      
         BRAS  RE,GETNET           Read first record to start                   
         B     DAT12                                                            
                                                                                
DAT10    CLI   QSYS,NETQ           TEST NETPAK (DLFILE INPUT)                   
         JE    DAT12               Yes Net                                      
         L     R1,AFILEIN          Spot and Print                               
         LA    R0,FILEREC                                                       
         GET   (1),(0)                                                          
         BRAS  RE,ANY$                                                          
         JZ    DAT10               No dollars so skip record                    
         J     DAT16               Process                                      
*                                                                               
DAT12    CLI   #FIELDS,0                                                        
         JE    EXIT                                                             
         BRAS  RE,GETDL                                                         
         BRAS  RE,ANY$                                                          
         JZ    DAT10               No dollars so skip record                    
*==================================================================             
* COMPARE EACH FIELD TO PREVIOUS AND SEND ALL CHANGED FIELDS                    
*==================================================================             
                                                                                
DAT16    BRAS  RE,GET1OFF          Get one char office code                     
         BRAS  RE,FILTER                                                        
         JZ    DAT10               Filter out office                            
         BRAS  RE,CHKLMT           CHECK LIMIT ACCESS VALUES CHANGED            
*                                                                               
         LA    R0,WRKIO            CLEAR THE OUTPUT AREA                        
         LHI   R1,WRKIOX-WRKIO                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,DATATAB                                                       
         LA    R5,WRKIO            SET OUTPUT POINTER                           
         MVC   0(5,R5),=X'0600050008'  SET DATA RECORD                          
         AHI   R5,5                                                             
*                                                                               
         CLI   SENTSYS,YES         TEST SENT SYSTEM                             
         JE    DAT18               YES                                          
                                                                                
* MOVE SYSTEM CODE TO OUTPUT DATA ONCE ONLY FOR EACH SYSTEM                     
                                                                                
         MVI   0(R5),LQ_RAWDQ      SET UNEDITED TEXT ELEMENT CODE               
         LHI   RF,MAPSYSCD         GET DATA TYPE CODE                           
         STCM  RF,3,3(R5)          SET IN RECORD                                
         MVI   5(R5),LD_CHARQ      SET UNEDITED CHAR DATA FOLLOWS               
*                                                                               
         LHI   RF,1                GET DATA LENGTH                              
         AHI   RF,6                ADD OVERHEAD                                 
         STCM  RF,3,1(R5)          SET LENGTH OF STRING                         
*                                                                               
         MVC   6(1,R5),FILESYSC    System code                                  
         AHI   R5,7                                                             
*                                                                               
         MVC   0(10,R5),SPOTMAP                                                 
         CLI   FILESYSC,SPOTQ      Spot                                         
         JE    DAT17                                                            
         MVC   0(11,R5),PRINTMAP                                                
         CLI   FILESYSC,PRINTQ     Print                                        
         JE    DAT17                                                            
         MVC   0(9,R5),NETMAP                                                   
         CLI   FILESYSC,NETQ       Net                                          
         JE    DAT17                                                            
         DC    H'00'                                                            
                                                                                
DAT17    ICM   RF,3,1(R5)          Bump to next spot                            
         AR    R5,RF                                                            
         J     DAT18                                                            
*                                                                               
DAT18    SR    R0,R0                                                            
         ICM   R0,3,0(R4)          GET DATA DSPL                                
         LA    RE,FILEREC                                                       
         AR    RE,R0                                                            
         LA    RF,SAVEREC                                                       
         AR    RF,R0                                                            
         LLC   R1,2(R4)            GET COMPARE LENGTH                           
         BCTR  R1,0                                                             
*                                                                               
         CLI   SENTSYS,YES         IF THIS IS NEW SYS, SEND ALL                 
         BNE   DAT19                                                            
         CLI   SENTLACC,YES        TEST JUST SENT LIMIT ACCS DATA               
         BE    DAT19               YES - SEND EVERYTHING                        
         EXCLC R1,0(RE),0(RF)      TEST DATA CHANGED                            
         BE    DAT20                                                            
                                                                                
* NEED TO SEND THIS FIELD                                                       
                                                                                
DAT19    MVI   0(R5),LQ_RAWDQ      SET UNEDITED TEXT ELEMENT CODE               
         LLC   RF,3(R4)            GET DATA TYPE CODE                           
         STCM  RF,3,3(R5)          SET IN RECORD                                
         MVI   5(R5),LD_CHARQ      SET UNEDITED CHAR DATA FOLLOWS               
*                                                                               
         IC    RF,2(R4)            GET DATA LENGTH                              
         SHI   RF,1                                                             
         BNP   DAT19D                                                           
         LA    R8,0(RF,RE)         Point to last character in data              
DAT19B   CLI   0(R8),C' '                                                       
         BH    DAT19D                                                           
         BCTR  R8,0                                                             
         BRCT  RF,DAT19B                                                        
*                                  At least one blank character                 
DAT19D   EXMVC RF,6(R5),0(RE)      Move data                                    
                                                                                
         AHI   RF,7                ADD OVERHEAD                                 
         STCM  RF,3,1(R5)          SET LENGTH OF STRING                         
         AR    R5,RF                                                            
*                                                                               
DAT20    LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DAT18                                                            
*                                                                               
         LA    R0,SAVEREC          SAVE WHAT WE JUST SENT                       
         LHI   R1,SAVERECX-SAVEREC                                              
         LA    RE,FILEREC                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     DAT30                                                            
                                                                                
*==============================================================                 
* NOW DEAL WITH COLUMN DATA                                                     
* FIRST DO PRIOR, THEN MONTHS, THEN SUBSEQ (three loops)                        
*==============================================================                 
                                                                                
DAT30    LA    R1,DOLLARS                                                       
         LHI   R0,DOLLAR#                                                       
         ZAP   0(8,R1),=P'0'                                                    
         AHI   R1,L'DOLORD                                                      
         BCT   R0,*-10                                                          
*======================================================================         
* NOW PROCESS PRIOR DOLLARS                                                     
*======================================================================         
                                                                                
         LA    R6,FILEREC          SUM PRIOR DOLLARS                            
         USING THISRECD,R6                                                      
         LA    R6,THISYYMM                                                      
         USING THISYYMM,R6                                                      
         LH    R7,NMONTHS                                                       
*                                                                               
DAT32    CLC   THISYYMM,SPACES     TEST DATA PRESENT                            
         BNH   DAT36               NO - DONE                                    
         CLC   THISYYMM,FILEQSTR   TEST PRIOR TO START                          
         BNL   DAT34               IF NOT LOW, IGNORE                           
*                                                                               
         AP    DOLORD,THISORD                                                   
         AP    DOLPAID,THISPAID                                                 
         AP    DOLBLLD,THISBLLD                                                 
         ZAP   DUB,THISORD                                                      
         CLI   ASSIGNED,YES        Ordered assigned                             
         BNE   DAT33                                                            
         AP    DOLOASN,THISOASN                                                 
         ZAP   DUB,THISOASN                                                     
                                                                                
DAT33    SP    DUB,THISBLLD        Figure out unbilled (billable)               
         AP    DOLUNBD,DUB                                                      
*                                                                               
DAT34    AH    R6,YYMMLEN                                                       
         BCT   R7,DAT32                                                         
*                                                                               
DAT36    MVC   MONSEQ,=C'00'                                                    
         MVC   DOLYYMM,=C'PRIOR '                                               
*                                                                               
         BAS   RE,CHKZERO          CHECK DOLLARS ALL ZERO                       
         BE    DAT38                                                            
         BAS   RE,PUTDOLS                                                       
         BAS   RE,SETR5            INITIALIZE OUTPUT POINTER                    
                                                                                
*======================================================================         
* NOW PROCESS CURRENT DOLLARS                                                   
*======================================================================         
         USING THISRECD,R6                                                      
DAT38    LA    R6,FILEREC          PROCESS MONTHS IN PERIOD                     
         LA    R6,THISYYMM                                                      
         USING THISYYMM,R6                                                      
*                                                                               
         LH    R7,NMONTHS                                                       
DAT40    CLC   THISYYMM,SPACES                                                  
         BNH   DAT44                                                            
*                                                                               
         BAS   RE,GETSEQ                                                        
         CLC   MONSEQ,=C'00'       TEST PRIOR                                   
         BE    DAT42               SKIP                                         
         CLC   MONSEQ,=C'99'       TEST SUBSEQ                                  
         BE    DAT42               SKIP                                         
*                                                                               
         MVC   WORK(4),0(R6)                 MOVE YYMM                          
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,WORK,(9,DOLYYMM)  GET MMM/YY                         
*                                                                               
         ZAP   DOLORD,THISORD                                                   
         ZAP   DOLPAID,THISPAID                                                 
         ZAP   DOLBLLD,THISBLLD                                                 
         ZAP   DOLUNBD,THISORD                                                  
         CLI   ASSIGNED,YES        Ordered assigned                             
         BNE   DAT41                                                            
         ZAP   DOLOASN,THISOASN                                                 
         ZAP   DOLUNBD,THISOASN                                                 
                                                                                
DAT41    SP    DOLUNBD,THISBLLD                                                 
*                                                                               
         BAS   RE,CHKZERO                                                       
         BE    DAT42                                                            
         BAS   RE,PUTDOLS          PUT RECORD TO WORKER FILE                    
         BAS   RE,SETR5                                                         
*                                                                               
DAT42    AH    R6,YYMMLEN                                                       
         BCT   R7,DAT40                                                         
                                                                                
*======================================================================         
* NOW PROCESS SUBSEQ DOLLARS                                                    
*======================================================================         
DAT44    LA    R1,DOLLARS                                                       
         LHI   R0,DOLLAR#                                                       
         ZAP   0(8,R1),=P'0'                                                    
         AHI   R1,L'DOLORD                                                      
         BCT   R0,*-10                                                          
                                                                                
         USING THISRECD,R6                                                      
         LA    R6,FILEREC          SUM SUBSEQ DOLLARS                           
         LA    R6,THISYYMM                                                      
         USING THISYYMM,R6                                                      
         LH    R7,NMONTHS                                                       
*                                                                               
DAT50    CLC   THISYYMM,SPACES                                                  
         BNH   DAT54                                                            
*                                                                               
         BAS   RE,GETSEQ           GET SEQNUM                                   
         CLC   MONSEQ,=C'99'       TEST SUBSEQ                                  
         BNE   DAT52                                                            
*                                                                               
         AP    DOLORD,THISORD                                                   
         AP    DOLPAID,THISPAID                                                 
         AP    DOLBLLD,THISBLLD                                                 
         ZAP   DUB,THISORD                                                      
         CLI   ASSIGNED,YES                                                     
         BNE   DAT51                                                            
         AP    DOLOASN,THISOASN                                                 
         ZAP   DUB,THISOASN                                                     
                                                                                
DAT51    SP    DUB,THISBLLD                                                     
         AP    DOLUNBD,DUB                                                      
*                                                                               
DAT52    AH    R6,YYMMLEN                                                       
         BCT   R7,DAT50                                                         
*                                                                               
DAT54    MVC   DOLYYMM,=C'SUBSEQ'                                               
         MVC   MONSEQ,=C'99'                                                    
*                                                                               
         BAS   RE,CHKZERO                                                       
         BE    *+8                                                              
         BAS   RE,PUTDOLS                                                       
*                                                                               
         MVI   SENTSYS,YES                                                      
         MVI   SENTLACC,NO         RESET 'SENT LMT ACC DATA'                    
         B     DAT10               AND CONTINUE                                 
                                                                                
**********************************************************                      
* See if we need to process prior, current or subseq     *                      
**********************************************************                      
CHKZERO  CP    DOLORD,=P'0'                                                     
         BNER  RE                  RETURN WITH CC NEQ                           
         CP    DOLPAID,=P'0'                                                    
         BNER  RE                                                               
         CP    DOLBLLD,=P'0'                                                    
         BNER  RE                                                               
         CP    DOLOASN,=P'0'                                                    
         BNER  RE                                                               
         CP    DOLUNBD,=P'0'                                                    
         BR    RE                                                               
         DROP  R6                                                               
                                                                                
**********************************************************                      
* See if record has any amounts, if not then skip record *                      
**********************************************************                      
         USING THISRECD,R6                                                      
ANY$     LA    R6,FILEREC                                                       
         LA    R6,THISYYMM                                                      
         USING THISYYMM,R6                                                      
         LH    R7,NMONTHS                                                       
                                                                                
ANY$10   CLC   THISYYMM,SPACES                                                  
         BNH   ANY$20                                                           
         CP    THISORD,=P'0'                                                    
         BNER  RE                  Done                                         
         CP    THISPAID,=P'0'                                                   
         BNER  RE                  Done                                         
         CP    THISBLLD,=P'0'                                                   
         BNER  RE                  Done                                         
         CLI   ASSIGNED,YES                                                     
         JNE   ANY$20                                                           
         CP    THISOASN,=P'0'                                                   
         BNER  RE                  Done                                         
ANY$20   AH    R6,YYMMLEN                                                       
         BRCT  R7,ANY$10                                                        
         SR    R7,R7                                                            
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================              
* MONSEQ HAS MONTH SEQUENCE NUMBER (00 FOR PRIOR, 99 FOR SUBSEQ)                
* DOLYYMM HAS MMM/YY OR PRIOR OR SUBSEQ                                         
* DOLLARS HAS DOLLAR FIELDS                                                     
*                                                                               
* NOTE THAT FIELD MAPIDS ARE ==> HARD CODED <==                                 
*=================================================================              
                                                                                
         USING THISRECD,R6                                                      
PUTDOLS  NTR1                                                                   
         LA    R6,FILEREC                                                       
         CLC   TOTMED,THISMED      TEST SAME MEDIA/CLIENT                       
         BE    PUTDOL18                                                         
*                                                                               
         LA    R1,CLTTOTS          CHANGE OF MEDIA - CLT TOTALS                 
         BRAS  RE,PRTTOTS                                                       
*                                                                               
         MVC   TOTCLT,=C'***'                                                   
         LA    R1,MEDTOTS          THEN MEDIA TOTALS                            
         BRAS  RE,PRTTOTS                                                       
         B     PUTDOL20                                                         
*                                                                               
PUTDOL18 CLC   TOTCLT,THISCLT                                                   
         BE    PUTDOL20                                                         
*                                                                               
         LA    R1,CLTTOTS          CHANGE OF CLIENT - CLT TOTALS                
         BRAS  RE,PRTTOTS                                                       
*                                                                               
PUTDOL20 MVC   TOTMED,THISMED      SAVE CURRENT VALUES                          
         MVC   TOTCLT,THISCLT                                                   
*                                                                               
         AP    MEDORD,DOLORD                                                    
         AP    MEDPAID,DOLPAID                                                  
         AP    MEDBLLD,DOLBLLD                                                  
         AP    MEDOASN,DOLOASN                                                  
         AP    MEDUNBD,DOLUNBD                                                  
*                                                                               
         AP    CLTORD,DOLORD                                                    
         AP    CLTPAID,DOLPAID                                                  
         AP    CLTBLLD,DOLBLLD                                                  
         AP    CLTOASN,DOLOASN                                                  
         AP    CLTUNBD,DOLUNBD                                                  
*                                                                               
         AP    AGYORD,DOLORD                                                    
         AP    AGYPAID,DOLPAID                                                  
         AP    AGYBLLD,DOLBLLD                                                  
         AP    AGYOASN,DOLOASN                                                  
         AP    AGYUNBD,DOLUNBD                                                  
*                                                                               
         MVC   0(6,R5),MOSCDMAP    SEND MOS CODE AS CHAR                        
         MVC   6(2,R5),MONSEQ                                                   
         AHI   R5,8                                                             
*                                                                               
         MVC   0(6,R5),MOSNMMAP    SEND MONTH/YEAR STRING                       
         MVC   6(6,R5),DOLYYMM                                                  
         AHI   R5,12                                                            
*                                                                               
         USING AMTTABD,R2                                                       
         LA    R2,AMTTAB                                                        
PUTDOL36 SR    R0,R0                                                            
         ICM   R0,1,AMTMAP#        Get column number                            
         BZ    PUTDOL50            Finished                                     
         L     RE,AMTPREV$         A(Previous amount)                           
         L     RF,AMTCURR$         A(New amount)                                
         TP    0(8,RE)             See if packed                                
         BE    *+10                No, so zero out                              
         ZAP   0(8,RE),=P'0'                                                    
         TM    AMTIND1,AMTIHIDE                                                 
         BO    PUTDOL48                                                         
*                                                                               
         CLI   SENTSYS,NO          Sent system code/name?                       
         BE    PUTDOL40            No, force resend of data                     
         CLI   SENTLACC,YES        Limited access changed                       
         BE    PUTDOL40            Yes, force resend of data                    
         CP    0(8,RE),0(8,RF)     Same as before ?                             
         BE    PUTDOL48            Yes so skip                                  
*                                                                               
PUTDOL40 ZAP   AMT,0(8,RF)                                                      
         ZAP   0(8,RE),0(8,RF)     Save value in previous                       
         BAS   RE,DOLOUT                                                        
*                                                                               
PUTDOL48 AHI   R2,AMTLNQ           Bump up in table                             
         B     PUTDOL36            Next                                         
*                                                                               
PUTDOL50 MVI   0(R5),0             SET RECORD TERMINATOR                        
         AHI   R5,1                                                             
         LA    R0,WRKIOL                                                        
         SR    R5,R0               GET REC LENGTH                               
         SLL   R5,16                                                            
         ST    R5,WRKIOL                                                        
         MVI   FIXED,NO                                                         
*                                                                               
         BRAS  RE,PUTFILE                                                       
         CR    RB,RB               SET CC EQ                                    
         J     EXIT                                                             
         EJECT ,                                                                
*======================================================================         
*                                                                               
*======================================================================         
DOLOUT   STCM  R0,3,TSTRCODE                                                    
         MVC   0(L'TSTRING,R5),TSTRING     ASSUME VALUE 0                       
         CP    AMT,PKZERO                                                       
         BNE   DOLOUT2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,TSTRL          SET AS LENGTH OF STRING                      
         BCTR  RF,0                                                             
         EXMVC RF,0(R5),TSTRING    *EXECUTED*                                   
         LA    R5,1(R5,RF)         ADVANCE OUTPUT POINTER                       
         BR    RE                                                               
*                                                                               
DOLOUT2  STCM  R0,3,PSTRCODE                                                    
         CP    AMT,PKZERO                                                       
         BL    *+8                                                              
         OI    AMT+7,X'0F'                                                      
*                                                                               
         EDIT  (P8,AMT),(15,PSTRVAL),ALIGN=LEFT,FLOAT=-                         
         LR    RF,R0               LENGTH OF DOLLARS                            
         AHI   RF,PSTRVAL-PSTRING  PLUS FIXED LENGTH                            
         STCM  RF,3,PSTRL          SET AS LENGTH OF STRING                      
         BCTR  RF,0                                                             
         EXMVC RF,0(R5),PSTRING    *EXECUTED*                                   
         LA    R5,1(R5,RF)         ADVANCE OUTPUT POINTER                       
         BR    RE                                                               
*                                                                               
SETR5    XC    WRKIO(256),WRKIO    CLEAR OUTPUT AREA                            
         XC    WRKIO+256(256),WRKIO+256                                         
         LA    R5,WRKIO            SET OUTPUT POINTER                           
         MVC   0(5,R5),=X'0600050008'  SET DATA RECORD                          
         AHI   R5,5                                                             
         BR    RE                                                               
         EJECT                                                                  
*======================================================================         
* Get sequense number for year/month                                            
*======================================================================         
GETSEQ   NTR1                                                                   
         MVC   MONSEQ,=C'00'                                                    
         CLC   0(4,R6),FILEQSTR    TEST PRIOR TO START                          
         BL    GETSEQX             IF LOW, MUST BE PRIOR                        
*                                                                               
         LA    R1,MONTAB                                                        
GETSEQ2  MVC   MONSEQ,0(R1)        SET 99 BEFORE EXIT                           
         CLC   0(2,R1),=C'99'                                                   
         BE    GETSEQX                                                          
         CLC   0(4,R6),2(R1)       MATCH YYMM                                   
         BE    GETSEQX                                                          
         AHI   R1,6                                                             
         B     GETSEQ2                                                          
*                                                                               
GETSEQX  XIT1                                                                   
         EJECT ,                                                                
*=====================================================================*         
* Map data for system and MOS code and name                                     
*=====================================================================*         
SPOTMAP  DC    AL1(LQ_RAWDQ),AL2(10,MAPSYSNM),AL1(LD_CHARQ),C'SPOT'             
PRINTMAP DC    AL1(LQ_RAWDQ),AL2(11,MAPSYSNM),AL1(LD_CHARQ),C'PRINT'            
NETMAP   DC    AL1(LQ_RAWDQ),AL2(09,MAPSYSNM),AL1(LD_CHARQ),C'NET'              
MOSCDMAP DC    AL1(LQ_DLDDQ),AL2(08,MAPMOSCD),AL1(LP_OTCHR)                     
MOSNMMAP DC    AL1(LQ_DLDDQ),AL2(12,MAPMOSNM),AL1(LP_OTCHR)                     
                                                                                
*=====================================================================*         
* These data fields are in fixed portion of record                              
*=====================================================================*         
DATATAB  DS    0D                                                               
         DC    AL2(THISMED-THISRECD),AL1(L'THISMED,MAPMEDCD)                    
         DC    AL2(THISMDNM-THISRECD),AL1(L'THISMDNM,MAPMEDNM)                  
         DC    AL2(THISMDOF-THISRECD),AL1(L'THISMDOF,MAPMOFCD)                  
         DC    AL2(THISCLT-THISRECD),AL1(L'THISCLT,MAPCLTCD)                    
         DC    AL2(THISCLNM-THISRECD),AL1(L'THISCLNM,MAPCLTNM)                  
         DC    AL2(THISACOF-THISRECD),AL1(L'THISACOF,MAPAOFCD)                  
         DC    AL2(THISPRD-THISRECD),AL1(L'THISPRD,MAPPRDCD)                    
         DC    AL2(THISPRNM-THISRECD),AL1(L'THISPRNM,MAPPRDNM)                  
         DC    AL2(THISEST-THISRECD),AL1(L'THISEST,MAPESTCD)                    
         DC    AL2(THISESNM-THISRECD),AL1(L'THISESNM,MAPESTNM)                  
         DC    AL2(THISTYPE-THISRECD),AL1(L'THISTYPE,MAPESTTY)                  
         DC    AL1(EOT)                                                         
                                                                                
*=====================================================================*         
* Amount columns to process                                                     
*=====================================================================*         
AMTTAB   DS    0D                                                               
         DC    AL1(MAPORDR,0,0,0),A(PVDOLORD),A(DOLORD)                         
         DC    AL1(MAPPAID,0,0,0),A(PVDOLPAD),A(DOLPAID)                        
         DC    AL1(MAPUNPD,AMTICALC,0,0),A(PKZERO),A(PKZERO)                    
         DC    AL1(MAPBLLD,0,0,0),A(PVDOLBIL),A(DOLBLLD)                        
AMTUNBD1 DC    AL1(MAPUNBD,AMTICALC,0,0),A(PKZERO),A(PKZERO)                    
AMTOASNG DC    AL1(MAPOASN,0,0,0),A(PVDOLOAS),A(DOLOASN)                        
AMTUNBD2 DC    AL1(MAPUNB2,0,0,0),A(PVDOLUNB),A(DOLUNBD)                        
         DC    AL1(0)                                                           
*=====================================================================*         
*                                                                               
*=====================================================================*         
TSTRING  DS    0D                                                               
         DC    X'06'                                                            
TSTRL    DC    AL2(TSTRINGX-TSTRING)                                            
TSTRCODE DC    XL2'00'                                                          
TSTRTYPE DC    C'L'                                                             
TSTRVAL  DC    C'0'                                                             
TSTRINGX EQU   *                                                                
         ORG                                                                    
*                                                                               
         DS    0D                                                               
PSTRING  DS    0XL24                                                            
         DC    X'06'                                                            
PSTRL    DC    AL2(PSTRVAL-PSTRING)   STRING LEN UP TO VAL                      
PSTRCODE DC    XL2'00'                                                          
PSTRTYPE DC    C'L'                                                             
PSTRVAL  DC    CL16'0'                                                          
PSTRINGX EQU   *                                                                
         ORG                                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* MAKE SURE TERMINATOR ON THIS RECORD OR CONCATENATE NEXT TO IT                 
*=====================================================================*         
GETNET   NTR1  BASE=*,LABEL=*                                                   
         MVI   #FIELDS,0                                                        
         L     R1,AFILEIN                                                       
         LA    R0,DLREC                                                         
         GET   (1),(0)                                                          
*                                                                               
         CLI   DLREC+1,C'"'        TEST ANY MORE DLFILE DATA                    
         JE    *+12                                                             
         CLI   DLREC+2,C'"'                                                     
         JNE   GETNETX             NO - EXIT                                    
                                                                                
         LA    R4,DLREC+197        POINT TO LAST BYTE                           
         CLI   0(R4),C' '                                                       
         JH    *+8                                                              
         BRCT  R4,*-8                                                           
*                                                                               
         CLI   0(R4),X'5E'         TEST SEMICOLON                               
         JE    GETNET10                                                         
*                                                                               
         L     R1,AFILEIN                                                       
         LA    R0,DLREC2           POINT TO SECOND IO AREA                      
         GET   (1),(0)             AND READ NEXT RECORD                         
*                                                                               
         MVI   1(R4),C' '          SET SPACE AS SEPARATOR                       
         MVC   2(132,R4),DLREC2+2  AND CONCATENATE THE REST                     
                                                                                
GETNET10 GOTOR VDDUNFLD,DMCB,DLREC,ADBUY,80                                     
         MVC   #FIELDS,8(R1)                                                    
         CR    RE,RE                                                            
*                                                                               
GETNETX  J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT ,                                                                
*===============================================================                
* PRINT TOTALS - R1 POINTS TO 4PL8 ORD/PAID/BLLD/OASN                           
*===============================================================                
PRTTOTS  NTR1  BASE=*,LABEL=*                                                   
         LR    R4,R1               R1 = A(Accumulators)                         
         LHI   R0,DOLLAR#          Number of accumulators                       
         CLI   ASSIGNED,YES        Has assigned amounts                         
         BE    *+6                                                              
         BCTR  R0,0                No, so one less                              
PRTTOT2  CP    0(L'DOLORD,R1),=P'0'                                             
         BNE   PRTTOT4                                                          
         AHI   R4,L'DOLORD                                                      
         BCT   R0,PRTTOT2                                                       
         B     PRTTOTX             No amounts to print                          
*                                                                               
PRTTOT4  MVC   P(2),QAGY           Print agency                                 
         MVC   P+3(1),TOTMED       Print media                                  
         MVC   P+5(3),TOTCLT       Print client                                 
*                                                                               
         LR    R4,R1                                                            
         LHI   R5,DOLLAR#                                                       
         CLI   ASSIGNED,YES                                                     
         BE    *+6                                                              
         BCTR  R5,0                                                             
         LA    R6,P+10                                                          
PRTTOT10 MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
         ZAP   DUB,0(L'DOLORD,R4)                                               
         ED    WORK(21),DUB                                                     
         MVC   0(18,R6),WORK+3     Print amount                                 
         AHI   R4,L'DOLORD         Next accumulator                             
         AHI   R6,21               Move up in print line                        
         BCT   R5,PRTTOT10         Next                                         
         GOTO1 REPORT                                                           
*                                                                               
         LHI   R0,DOLLAR#                                                       
         ZAP   0(8,R1),=P'0'                                                    
         AHI   R1,L'DOLORD                                                      
         BCT   R0,*-10                                                          
*                                                                               
PRTTOTX  J     EXIT                                                             
         LTORG                                                                  
         EJECT ,                                                                
*==============================================================                 
* READ AND REFORMAT NETPAK WRITER DOWNLOAD (DL) DATA                            
* DLREC CONTAINS THE FIRST RECORD TO PROCESS                                    
*==============================================================                 
GETDL    NTR1  BASE=*,LABEL=*                                                   
         L     R4,ADBUY                                                         
         LLC   R5,#FIELDS          GET NUMBER OF FIELDS                         
*                                                                               
         LA    R0,FILEREC          CLEAR INPUT RECORD AREA                      
         LHI   R1,512                                                           
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING THISRECD,R6                                                      
         LA    R6,FILEREC                                                       
         MVC   THISMED,0(R4)       FIRST LETTER IS MEDIA                        
         MVC   THISMDNM,0(R4)      BUT THE WHOLE NAME IS THERE TOO              
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    R1,THISCLT                                                       
         LA    R2,THISCLNM                                                      
         BAS   RE,GETCDNM          GET CLT CODE AND NAME                        
         BAS   RE,NEXTFLD                                                       
*                                                                               
         MVC   THISMDOF,0(R4)                                                   
         BAS   RE,NEXTFLD                                                       
*                                                                               
         MVC   THISACOF,0(R4)                                                   
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    R1,THISPRD                                                       
         LA    R2,THISPRNM                                                      
         BAS   RE,GETCDNM          GET PRD CODE AND NAME                        
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    R1,THISEST                                                       
         LA    R2,THISESNM                                                      
         BAS   RE,GETCDNM          GET EST CODE AND NAME                        
         BAS   RE,NEXTFLD                                                       
*                                                                               
         GOTO1 HEXIN,DMCB,(R4),THISAGMD,2,0  BINARY AGYMD IN EBCDIC             
         BAS   RE,NEXTFLD                                                       
*                                                                               
         XC    THISCACC,THISCACC                                                
         CLI   0(R4),C' '                                                       
         BNH   *+10                                                             
         MVC   THISCACC,0(R4)                                                   
         BAS   RE,NEXTFLD                                                       
*                                                                               
         BAS   RE,FMTYYMM          FORMAT YYMM AND DOLLAR FIELDS                
*                                                                               
         LA    R6,THISYYMM         POINT TO FIRST MONTH SLOT                    
         USING THISYYMM,R6                                                      
*                                                                               
GETDL10  BAS   RE,GETMMMYY            CONVERT MMMYY TO YYMM..                   
*                                                                               
         MVC   THISYYMM,WORK                                                    
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    R2,THISORD                                                       
         BAS   RE,GETDOLS                                                       
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    R2,THISPAID                                                      
         BAS   RE,GETDOLS                                                       
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    R2,THISBLLD                                                      
         BAS   RE,GETDOLS                                                       
         BAS   RE,NEXTFLD                                                       
*                                                                               
         CLI   ASSIGNED,YES                                                     
         BNE   GETDL12                                                          
         LA    R2,THISOASN         Ordered assigned                             
         BAS   RE,GETDOLS                                                       
                                                                                
GETDL12  AH    R6,YYMMLEN          POINT TO NEXT MONTH SLOT                     
         DROP  R6                                                               
                                                                                
*====================================================================           
* GET THE NEXT DLFILE RECORD AND TEST SAME CLT/PRD/EST                          
*====================================================================           
         BRAS  RE,GETNET                                                        
         CLI   #FIELDS,0                                                        
         JE    EXIT                                                             
*                                                                               
         L     R4,ADBUY                                                         
         LLC   R5,#FIELDS          NUMBER OF FIELDS                             
*                                                                               
         LA    R7,FILEREC                                                       
         USING THISRECD,R7                                                      
*                                                                               
         CLC   THISMED,0(R4)                                                    
         JNE   EXIT                                                             
         BAS   RE,NEXTFLD                                                       
*                                                                               
         CLC   THISMDOF,0(R4)                                                   
         JNE   EXIT                                                             
         BAS   RE,NEXTFLD                                                       
*                                                                               
         CLC   THISACOF,0(R4)                                                   
         JNE   EXIT                                                             
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    R1,WORK                                                          
         LA    R2,WORK+4                                                        
         BAS   RE,GETCDNM          GET CLT CODE AND NAME                        
         CLC   THISCLT,WORK                                                     
         JNE   EXIT                                                             
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    R1,WORK                                                          
         LA    R2,WORK+4                                                        
         BAS   RE,GETCDNM          GET PRD CODE AND NAME                        
         CLC   THISPRD,WORK                                                     
         JNE   EXIT                                                             
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    R1,WORK                                                          
         LA    R2,WORK+4                                                        
         BAS   RE,GETCDNM          GET EST CODE AND NAME                        
         CLC   THISEST,WORK                                                     
         JNE   EXIT                                                             
*                                                                               
         L     R4,ADBUY                                                         
         AHI   R4,8*80             POINT TO YYMMDD DATA (9TH FIELD)             
         LLC   R5,#FIELDS          NUMBER OF FIELDS                             
         SHI   R5,8                                                             
         B     GETDL10             AND CONTINUE TO PROCESS                      
         DROP  R7                                                               
*                                                                               
NEXTFLD  AHI   R4,80                                                            
         SHI   R5,1                                                             
         LTR   R5,R5                                                            
         BNMR  RE                                                               
         DC    H'0'                                                             
                                                                                
*=============================================================                  
* SPLIT CODE AND NAME TO R1 AND R2 ADDRESSES                                    
* NOTE THAT NAME IS ALWAYS FOLLOWED BY SPACES                                   
*=============================================================                  
                                                                                
GETCDNM  NTR1                                                                   
         LR    RE,R4                                                            
         SR    RF,RF                                                            
*                                                                               
GETCDNM2 CLI   0(RE),C' '          FIND SPACE AFTER CODE                        
         BE    GETCDNM4                                                         
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         B     GETCDNM2                                                         
*                                                                               
GETCDNM4 MVC   0(3,R1),SPACES                                                   
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EXMVC RF,0(R1),0(R4)      MOVE CODE                                    
         MVC   0(24,R2),1(RE)      MOVE NAME                                    
         J     EXIT                                                             
                                                                                
*====================================================================           
* NUMERIC FIELDS ARE 16 BYTES, RIGHT ALIGNED                                    
* LET CASHVAL SORT IT OUT                                                       
* r2 POINTS TO 8 BYTE PACKED OUTPUT FIELD                                       
*====================================================================           
                                                                                
GETDOLS  NTR1                                                                   
         LR    RE,R4                                                            
         LHI   RF,16                                                            
*                                                                               
GETDOL2  CLI   0(RE),C' '                                                       
         BNE   GETDOL4                                                          
         AHI   RE,1                                                             
         BCT   RF,GETDOL2                                                       
         DC    H'0'                                                             
*                                                                               
GETDOL4  ST    RE,DMCB             SET FIELD START ADDRESS                      
         OI    DMCB,X'80'          REQUEST PACKED OUTPUT                        
         ST    RF,DMCB+4           SET FIELD LENGTH                             
         GOTO1 =V(CASHVAL),DMCB                                                 
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   0(8,R2),DMCB+4(8)   MOVE PACKED FIELD                            
         J     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* FORMAT YYMM FIELDS AND ZAP PACKED DOLLAR FIELDS                               
* R4 HAS FIRST MMMYY FOR THIS ESTIMATE                                          
* R6 POINTS TO FILEREC                                                          
*=================================================================              
                                                                                
FMTYYMM  NTR1                                                                   
         BAS   RE,GETMMMYY                 CONVERT MMMYY TO YYMM..              
*                                                                               
         LA    R4,THISYYMM-THISRECD(R6)                                         
         LHI   R5,15                                                            
         USING THISYYMM,R4                                                      
*                                                                               
FMTYYM2  MVC   THISYYMM,WORK                                                    
         ZAP   THISORD,=P'0'                                                    
         ZAP   THISPAID,=P'0'                                                   
         ZAP   THISBLLD,=P'0'                                                   
         CLI   ASSIGNED,YES                                                     
         BNE   *+10                                                             
         ZAP   THISOASN,=P'0'                                                   
*                                                                               
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,1                                  
         MVC   WORK(6),WORK+6                                                   
*                                                                               
         AH    R4,YYMMLEN                                                       
         BCT   R5,FMTYYM2                                                       
         J     EXIT                                                             
         DROP  R4                                                               
                                                                                
*============================================================                   
* CONVERT MMMYY AT 0(R4) TO YYMM01 IN WORK                                      
* SADLY, DATVAL WILL NOT VALIDATE MMMYY                                         
*============================================================                   
GETMMMYY NTR1                                                                   
         CLC   =C'BEFORE',0(R4)                                                 
         BNE   *+14                                                             
         MVC   WORK(4),QBEFORE                                                  
         B     GETMMM10                                                         
*                                                                               
         CLC   =C'AFTER',0(R4)                                                  
         BNE   *+14                                                             
         MVC   WORK(4),QAFTER                                                   
         B     GETMMM10                                                         
*                                                                               
         MVC   WORK(2),3(R4)                                                    
         LHI   R0,12                                                            
         LA    R1,MONTHS         POINT TO JANFEB...                             
*                                                                               
GETMMM4  CLC   0(3,R1),0(R4)                                                    
         BE    GETMMM6                                                          
         AHI   R1,3                                                             
         BCT   R0,GETMMM4                                                       
         DC    H'0'                                                             
*                                                                               
GETMMM6  LHI   RF,13                                                            
         SR    RF,R0                                                            
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(2),DUB                                                    
                                                                                
GETMMM10 MVC   WORK+4(2),=C'15'   GET DDS FORMAT DATES                          
         GOTO1 DATCON,DMCB,WORK,(3,WORK+6)                                      
         GOTO1 (RF),(R1),(3,WORK+6),WORK                                        
         J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* Filter out records based on request cards                                     
*==================================================================             
                                                                                
FILTER   NTR1  BASE=*,LABEL=*                                                   
         CLI   QCLT,C'*'                                                        
         BNE   FILT020             Not filtering office                         
         CLI   QCLT+1,C' '                                                      
         BE    FILT020             NO FILTER (PSYCH)                            
         CLI   QCLT+1,C'-'         NEVER HAVE OFFICE C'-' THEN                  
         BNE   FILT010                                                          
         CLC   SV1OFF,QCLT+2                                                    
         BE    FILTNO              EXCLUDE                                      
         B     FILT020             KEEP THESE THOUGH                            
                                                                                
FILT010  CLC   SV1OFF,QCLT+1                                                    
         BE    FILT020             THIS IS OKAY                                 
         BL    FILTNO              NOT IN RANGE                                 
         CLI   QCLT+2,C' '         WAS THERE A RANGE ?                          
         BE    FILTNO              NO                                           
         CLC   SV1OFF,QCLT+2       YES, SO IS IT IN RANGE ?                     
         BH    FILTNO              NO                                           
*                                                                               
FILT020  DS    0H                                                               
         B     FILTYES                                                          
                                                                                
FILTNO   SR    RE,RE                                                            
FILTYES  LTR   RE,RE                                                            
         J     EXIT                                                             
         EJECT ,                                                                
*==================================================================             
* TEST FOR CHANGE IN LIMIT ACCESS VALUES AND SEND ELEMENT IF NEEDED             
* WHENEVER ANY LIMIT ACCESS VALUE CHANGES, SEND THEM ALL                        
*==================================================================             
                                                                                
GET1OFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING THISRECD,R6                                                      
         LA    R6,FILEREC                                                       
         MVC   SV1OFF,THISMDOF     Set/save possible 1 char office              
*                                                                               
*        CLI   QSYS,SPOTQ          TEST SPOT                                    
*        BE    GET1OF10                                                         
*        CLI   QSYS,NETQ                                                        
*        BNE   GET1OFFX                                                         
                                                                                
         USING OFFICED,R4                                                       
GET1OF10 LA    R4,OFCWORK                                                       
         XC    OFCWORK,OFCWORK     TRANSLATE 2-CHAR OFFICE TO 1-CHAR            
         MVC   OFCSYS,QSYS                                                      
         MVC   OFCAGY,QAGY                                                      
         MVC   OFCOFC2,THISMDOF    MOVE 2-CHAR OFFICE                           
*                                                                               
         GOTO1 OFFICER,DMCB,(C'2',(R4)),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   GET1OFFX                                                         
         MVC   SV1OFF,OFCOFC       Set/save possible 1 char office              
                                                                                
GET1OFFX J     EXIT                                                             
         EJECT ,                                                                
         DROP  R4,R6                                                            
*==================================================================             
* TEST FOR CHANGE IN LIMIT ACCESS VALUES AND SEND ELEMENT IF NEEDED             
* WHENEVER ANY LIMIT ACCESS VALUE CHANGES, SEND THEM ALL                        
*==================================================================             
                                                                                
         USING THISRECD,R6                                                      
CHKLMT   NTR1  BASE=*,LABEL=*                                                   
         LA    R6,FILEREC                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(1),THISAGMD    PRINT PUTS MEDIA HERE                        
         MVC   WORK+1(1),SV1OFF                                                 
         MVC   WORK+2(3),THISCLT                                                
         MVC   WORK+5(3),THISCACC                                               
*                                                                               
CHKLMT2  CLI   SENTSYS,YES         TEST NEW SYSTEM                              
         BNE   CHKLMT4             YES- SEND EVERYTHING                         
*                                                                               
         CLC   WORK(8),SVLMTACC                                                 
         JE    EXIT                                                             
                                                                                
* NEED TO SEND NEW LIMIT ACCESS VALUES                                          
                                                                                
CHKLMT4  MVI   SENTLACC,YES        SET JUST SENT LIMIT ACCESS DATA              
         MVI   SENTSYS,NO          SET THAT WE NEED SYSTEM AGAIN                
         MVC   SVLMTACC,WORK       SAVE WHAT CAUSED US TO SEND                  
         XC    BCLT,BCLT                                                        
         CLI   FILESYSC,PRINTQ                                                  
         BE    CHKLMT8                                                          
*                                                                               
         GOTO1 CLPACK,DMCB,THISCLT,BCLT                                         
*                                                                               
CHKLMT8  LA    R0,WRKIO            CLEAR THE OUTPUT AREA                        
         LHI   R1,WRKIOX-WRKIO                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING THISRECD,R6                                                      
         LA    R6,FILEREC                                                       
         LA    R5,WRKIO                                                         
*                                                                               
         MVC   3(2,R5),=Y(201)     AGYMD                                        
         MVC   5(1,R5),THISAGMD                                                 
         LHI   RF,1                                                             
         BAS   RE,CHKSET                                                        
*                                                                               
         MVC   3(2,R5),=Y(202)     MEDIA OFFICE                                 
         MVC   5(2,R5),THISMDOF                                                 
         LHI   RF,2                                                             
         BAS   RE,CHKSET                                                        
*                                                                               
         MVC   3(2,R5),=Y(203)     EBCDIC CLIENT                                
         MVC   5(3,R5),THISCLT                                                  
         LHI   RF,3                                                             
         BAS   RE,CHKSET                                                        
*                                                                               
         MVC   3(2,R5),=Y(204)     PACKED CLIENT                                
         MVC   5(2,R5),BCLT                                                     
         LHI   RF,2                                                             
         BAS   RE,CHKSET                                                        
*                                                                               
         MVC   3(2,R5),=Y(205)     CLIENT LIMIT ACCESS CODES                    
         MVC   5(3,R5),THISCACC                                                 
         LHI   RF,3                                                             
         BAS   RE,CHKSET                                                        
*                                                                               
         MVC   0(5,R5),=X'0600050008'   SET RECORD CODE FOR BEAKUS              
         AHI   R5,5                     AT THE END !                            
*                                                                               
         MVI   0(R5),0             SET RECORD TERMINATOR                        
         AHI   R5,1                                                             
*                                                                               
         LA    R0,WRKIOL                                                        
         SR    R5,R0                                                            
         SLL   R5,16               LEFT ALIGN                                   
         ST    R5,WRKIOL                                                        
         MVI   FIXED,NO                                                         
*                                                                               
         BRAS  RE,PUTFILE                                                       
         J     EXIT                                                             
         DROP  R6                                                               
                                                                                
* SET ELEMENT CODE AND LENGTH. RF HAS FIELD LEN                                 
* R5 HAS ELEMENT START ADDRESS                                                  
                                                                                
CHKSET   MVI   0(R5),X'13'                                                      
         AHI   RF,5                X'13'/ELEMLEN(2)/DATAID(2)                   
         STCM  RF,3,1(R5)                                                       
         AR    R5,RF                                                            
         BR    RE                                                               
*                                                                               
         DS    0D                                                               
SVLMTACC DS    CL8                                                              
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* OPEN WORKER FILE                                                              
*==============================================================                 
                                                                                
WRKROPEN NTR1  BASE=*,LABEL=*                                                   
         L     R0,AWKBUFF          CLEAR WKFILE BUFFER                          
         L     R1,=A(WKBUFFX-WKBUFF)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   QOPT1,C'P'          TEST PRINT ONLY                              
         JE    EXIT                                                             
                                                                                
         USING WLHDRD,R4                                                        
         LA    R4,WRKIO                                                         
         XC    SEQNUM,SEQNUM                                                    
         XC    0(256,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,RCORIGID                                                 
         MVC   WLSYSPRG(4),=C'A9**' REPORT ID                                   
*&&DO                                                                           
         B     WRKROP2                                                          
         CLI   FILESYSC,SPOTQ                                                   
         BE    WRKROP2                                                          
         MVC   WLSYSPRG(4),=C'NEA9'                                             
         CLI   FILESYSC,NETQ                                                    
         BE    WRKROP2                                                          
         MVC   WLSYSPRG(4),=C'PPA9'                                             
         CLI   FILESYSC,PRINTQ                                                  
         BE    WRKROP2                                                          
         DC    H'0'                                                             
*&&                                                                             
WRKROP2  GOTO1 DATCON,DMCB,(5,0),(1,DUB)  GET PACKED DATE                       
         MVC   WLDAY,DUB+2                                                      
         MVI   WLCLASS,C'A'        CLASS A                                      
         MVC   WLPSWD,SPACES                                                    
         OI    WLATTB,WLATOBJ                                                   
         LA    R1,24*RETDAYS                                                    
         STCM  R1,3,WLRETNL                                                     
                                                                                
         LA    R3,WRKIO                                                         
         MVI   FIXED,YES                                                        
         BAS   RE,WRKR                                                          
*                                                                               
         USING UKRECD,R5                                                        
         LA    R5,WRKRINDX                                                      
         XC    SEQNUM,SEQNUM                                                    
         XC    WRKRINDX,WRKRINDX                                                
         MVC   UKUSRID,RCORIGID                                                 
         MVC   UKSYSPRG(1),FILESYS                                              
         MVC   UKSYSPRG+2(1),=C'A9'                                             
         MVC   UKFILENO,WLREPRNO   WORKER FILE NUMBER                           
         DROP  R5                                                               
                                                                                
         MVC   WRKFNO,WLREPRNO     WORKER FILE NUMBER                           
         MVI   FIXED,NO                                                         
                                                                                
         MVC   P(16),=C'WORKER FILE ID ='                                       
         EDIT  (B2,WLUSRID),(4,P+20)                                            
         MVI   P+24,C','                                                        
         MVC   P+25(4),WLFILEID                                                 
         GOTO1 HEXOUT,DMCB,WLDAY,P+29,1,=C'TOG'                                 
         MVC   P+31(1),WLCLASS                                                  
         MVI   P+32,C','                                                        
         EDIT  WLREPRNO,(5,P+33),0,ALIGN=LEFT                                   
         GOTO1 REPORT                                                           
         DROP  R4                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* CLOSE WORKER FILE                                                             
*==============================================================                 
                                                                                
WRKRCLSE NTR1  BASE=*,LABEL=*                                                   
         CLI   QOPT1,C'P'          TEST PRINT ONLY                              
         JE    EXIT                                                             
                                                                                
         USING WLHDRD,R4                                                        
         LA    R4,WRKIO                                                         
         XC    0(256,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         MVI   FIXED,YES                                                        
         XC    SEQNUM,SEQNUM                                                    
         BAS   RE,WRKR                                                          
         GOTO1 DATAMGR,DMCB,=CL8'KEEP',WRKFILEN,WRKRINDX,(R4),AWKBUFF           
         J     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* ADD LINE TO WORKER FILE                                                       
*==============================================================                 
WRKR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,WRKIO                                                         
         CLI   FIXED,YES                                                        
         BNE   WRKR2                                                            
         CLI   QOPT1,C'P'                                                       
         BNE   WRKR10                                                           
         MVC   P(80),0(R4)                                                      
         B     WRKR4                                                            
*                                                                               
WRKR2    LA    R4,WRKIOL                                                        
         CLI   QOPT1,C'P'          TEST TO PRINT OUTPUT ONLY                    
         BNE   WRKR10                                                           
         LA    R0,P                                                             
         LH    R1,0(R4)                                                         
         CHI   R1,1000                                                          
         BNH   *+8                                                              
         LHI   R1,1000                                                          
         LH    R5,0(R4)                                                         
         MVCL  R0,R4                                                            
*                                                                               
WRKR4    GOTO1 REPORT                                                           
         J     EXIT                                                             
                                                                                
WRKR10   GOTO1 DATAMGR,DMCB,DMPRINT,WRKFILE,0,(R4),AWKBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SEQNUM                                                        
         AHI   R1,1                                                             
         ST    R1,SEQNUM                                                        
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* Code segment as storage                                                       
***********************************************************************         
         DS    0D                                                               
         DC    CL8'SPAGWORK'                                                    
SPAGWORK DS    0D                                                               
*                                                                               
OUTWRITE WRITE OUTECB,SF,,MF=L                                                  
OUTSIZE  DC    AL3(1),AL3(5)                                                    
                                                                                
DDSPOTIN DC    CL8'SPOTIN'                                                      
DDPRNTIN DC    CL8'PRNTIN'                                                      
DDNETIN  DC    CL8'NETIN'                                                       
                                                                                
DSNPARM  DS    0CL35                                                            
DSNSYS   DC    CL3'SPT'                                                         
         DC    CL5'TAPE.'                                                       
DSNCHAR  DC    CL2'SP'                                                          
         DC    CL3'0A9'                                                         
DSNALPHA DC    CL2' '                                                           
DSNTYPE  DC    C' '                                                             
         DC    CL((L'DSNPARM)-(*-DSNPARM))' '                                   
*                                                                               
* NOTE DDNAMES DYNAMICALLY MODIFIED BEFORE OPEN                                 
*                                                                               
SPOTIN   DCB   DDNAME=SPOTIN,DSORG=PS,RECFM=FB,MACRF=GM,LRECL=768,     X        
               EODAD=EXIT                                                       
*                                                                               
PRNTIN   DCB   DDNAME=PRNTIN,DSORG=PS,RECFM=FB,MACRF=GM,LRECL=768,     X        
               EODAD=EXIT                                                       
*                                                                               
NETIN    DCB   DDNAME=NETIN,DSORG=PS,RECFM=FB,MACRF=GM,LRECL=199,      X        
               EODAD=EXIT                                                       
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,BLKSIZE=08192,DSORG=PS,MACRF=(W),        X        
               LRECL=08000,RECFM=V                                              
**>>>>>>>> IF YOU CHANGE THE BLKSIZE, CHANGE OUTBLK TOO !  <<<<<<<<**           
*                                                                               
OUTREC   DS    0D                                                               
OUTCNT   DC    F'0'                                                             
OUTBYTES DC    F'0'                                                             
*                                                                               
OUTDDNAM DC    CL8'FILEOUT'                                                     
*                                                                               
OUTDSN   DC    CL35' '                                                          
         ORG   OUTDSN                                                           
         DC    C'SPTDISK.SAG'                                                   
OUTAGYA  DS    CL2                                                              
         DC    C'.'                                                             
OUTDATE  DS    CL7                                                              
         DC    C'.'                                                             
OUTTIME  DS    CL7                                                              
         DC    C'.'                                                             
OUT$TYP  DS    CL3                 NET or GRS                                   
         ORG                                                                    
*                                                                               
RETDAYS  EQU   5                   Days to retain                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
SPOTQ    EQU   C'S'                                                             
PRINTQ   EQU   C'P'                                                             
NETQ     EQU   C'N'                                                             
EOR      EQU   0                                                                
EOT      EQU   X'FF'                                                            
TURNOFF  EQU   X'FF'                                                            
AFILEIN  DC    A(0)                ADDRESS OF CURRENT INPUT FILE                
VDDUNFLD DC    V(DDUNFLD)                                                       
AWKBUFF  DC    A(WKBUFF)                                                        
SVRE     DS    A                                                                
OFFICER  DS    A                                                                
SEQNUM   DS    F                                                                
WRKFNO   DS    H                                                                
COLNUM   DS    H                                                                
NMONTHS  DS    H                                                                
YYMMLEN  DS    H                                                                
FIXED    DS    C                                                                
ASSIGNED DS    C                   Yes/No                                       
#FIELDS  DS    X                                                                
SENTSYS  DC    AL1(NO)                                                          
SENTLACC DC    AL1(NO)                                                          
MONSEQ   DC    C'00'                                                            
SV1OFF   DS    X                   Value of one character office                
QSYS     DS    C                                                                
EOFFLAG  DC    AL1(NO)                                                          
QAFTER   DS    CL6                                                              
QBEFORE  DS    CL6                                                              
         DS    0D                                                               
OFCWORK  DS    XL32                                                             
*                                                                               
         DS    0D                                                               
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
MEDNAME  DS    CL24                                                             
*                                                                               
         DS    0D                                                               
AMT      DS    PL8                                                              
DOLYYMM  DS    D                                                                
         DS    0D                                                               
DOLLARS  DS    0XL32                                                            
DOLORD   DS    PL8                                                              
DOLPAID  DS    PL8                                                              
DOLBLLD  DS    PL8                                                              
DOLOASN  DS    PL8                                                              
DOLUNBD  DS    PL8                                                              
DOLLAR#  EQU   (*-DOLLARS)/L'DOLORD                                             
*                                                                               
         DS    0D                                                               
PVDOLORD DS    PL8                 Previous value ordered                       
PVDOLPAD DS    PL8                 Previous value paid                          
PVDOLBIL DS    PL8                 Previous value billed                        
PVDOLOAS DS    PL8                 Previous value ordered assigned              
PVDOLUNB DS    PL8                 Previous value billable (unbilled)           
PKZERO   DS    PL8                 Leave as Packed length 8                     
*                                                                               
TOTS     DS    0D                                                               
TOTAGY   DC    CL2' '                                                           
TOTMED   DC    CL1' '                                                           
TOTCLT   DC    CL3' '                                                           
         DS    XL2                                                              
MEDTOTS  DS    0D                                                               
MEDORD   DC    PL8'0'                                                           
MEDPAID  DC    PL8'0'                                                           
MEDBLLD  DC    PL8'0'                                                           
MEDOASN  DC    PL8'0'                                                           
MEDUNBD  DC    PL8'0'                                                           
*                                                                               
CLTTOTS  DS    0D                                                               
CLTORD   DC    PL8'0'                                                           
CLTPAID  DC    PL8'0'                                                           
CLTBLLD  DC    PL8'0'                                                           
CLTOASN  DC    PL8'0'                                                           
CLTUNBD  DC    PL8'0'                                                           
*                                                                               
AGYTOTS  DS    0D                                                               
AGYORD   DC    PL8'0'                                                           
AGYPAID  DC    PL8'0'                                                           
AGYBLLD  DC    PL8'0'                                                           
AGYOASN  DC    PL8'0'                                                           
AGYUNBD  DC    PL8'0'                                                           
*                                                                               
MONTAB   DS    0D                  2 BYTE SEQNUM, YYMM                          
         DS    48XL6                                                            
MONTABX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DS    XL4                                                              
YYMM     DS    CL4                                                              
GORD     DS    PL8                                                              
GPAID    DS    PL8                                                              
GBLLD    DS    PL8                                                              
*                                                                               
         DS    0D                                                               
WRKRINDX DS    XL48                                                             
         DS    0D                                                               
         DC    CL8'**WRKIO*'                                                    
WRKIOL   DS    H                   LENGTH GOES HERE                             
         DS    H                                                                
WRKIO    DS    CL512                                                            
WRKIOX   EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'FILEHDR'                                                     
FILEHDR  DS    0D                                                               
         DC    CL(FILEHDRQ)'SA9HDRSPOTYYMMDDYYMMDDYYMMDDG'                      
*                                                                               
         DS    0D                                                               
         DC    CL8'**DLREC*'                                                    
DLREC    DS    CL256                                                            
*                                                                               
         DC    CL8'*DLREC2*'                                                    
DLREC2   DS    CL256                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*FILEREC'                                                    
FILEREC  DS    CL768                                                            
FILERECX EQU   *                                                                
         DS    0D                                                               
         DC    CL8'*SAVEREC'                                                    
SAVEREC  DS    CL768                                                            
SAVERECX EQU   *                                                                
*                                                                               
MAPSYSCD EQU   1                   System code                                  
MAPSYSNM EQU   2                   System name                                  
MAPMEDCD EQU   3                   Media  code                                  
MAPMEDNM EQU   4                   Media  name                                  
MAPMOFCD EQU   5                   Media   office                               
MAPCLTCD EQU   6                   Client code                                  
MAPCLTNM EQU   7                   Client name                                  
MAPAOFCD EQU   8                   Account office                               
MAPPRDCD EQU   9                   Product  code                                
MAPPRDNM EQU   10                  Product  name                                
MAPESTCD EQU   11                  Estimate code                                
MAPESTNM EQU   12                  Estimate name                                
MAPESTTY EQU   13                  Estimate type                                
MAPMOSCD EQU   14                  MOS code                                     
MAPMOSNM EQU   15                  MOS name                                     
MAPORDR  EQU   16                  Ordered                                      
MAPPAID  EQU   17                  Cleared / Paid                               
MAPUNPD  EQU   18                  Not cleared / Un-paid                        
MAPBLLD  EQU   19                  Billed                                       
MAPUNBD  EQU   20                  Billable (Unbilled)                          
MAPOASN  EQU   20                  Ordered Assigned                             
MAPUNB2  EQU   21                  Billable (Unbilled), w/Assigned              
         EJECT ,                                                                
*======================================================================         
* Macro for Schema                                                              
*======================================================================         
         MACRO                                                                  
&TAG     SCHEMA &IAMNODE=,&LABEL=,&MAPID=,&TYPE=C,&PREVNODE=,          X        
               &COLUMNOF=,&CHILDOF=,&GLOBID=,&MAXW=,&MAXTOT=                    
&TAG     DC     AL2(&TAG.Z-&TAG+1),AL2(0)                                       
.*                                                                              
&TAG.0   DC     X'06',AL2(&TAG.1-&TAG.0),X'FE02'                                
&TAG.1   DC     X'06',AL2(&TAG.2-&TAG.1),X'0001',C'C&LABEL'                     
&TAG.2   DC     X'06',AL2(&TAG.3-&TAG.2),X'0002',C'S&MAPID'                     
&TAG.3   DC     X'06',AL2(&TAG.4-&TAG.3),X'0003',C'C&TYPE'                      
&TAG.4   EQU    *                                                               
         AIF    (T'&IAMNODE NE 'O').A                                           
         AIF    (T'&CHILDOF  NE 'O').B                                          
         AIF    (T'&COLUMNOF NE 'O').C                                          
         MNOTE  1,'MISSING IAMNODE,CHILDOF,COLUMNOF'                            
.*                                                                              
.A       ANOP                                                                   
         AIF    (T'&CHILDOF NE 'O').AERR                                        
         AIF    (T'&COLUMNOF  NE 'O').AERR                                      
&TAG.5   DC     X'06',AL2(&TAG.6-&TAG.5),X'0004',C'S&IAMNODE'                   
&TAG.6   DC     X'06',AL2(&TAG.7-&TAG.6),X'0005',C'S&PREVNODE'                  
&TAG.7   EQU    *                                                               
         AGO    .X                                                              
.*                                                                              
.AERR    ANOP                                                                   
         MNOTE  1,'IAMNODE SO CANT USE CHILDOF OR COLUMNOF'                     
         MEXIT                                                                  
.*                                                                              
.B       ANOP                                                                   
         AIF    (T'&IAMNODE NE 'O').BERR                                        
         AIF    (T'&COLUMNOF NE 'O').BERR                                       
&TAG.8   DC     X'06',AL2(&TAG.9-&TAG.8),X'0006',C'S&CHILDOF'                   
&TAG.9   EQU    *                                                               
         AGO    .X                                                              
.BERR    ANOP                                                                   
         MNOTE  1,'CHILDOF SO CANT USE IAMNODE OR COLUMNOF'                     
         MEXIT                                                                  
.*                                                                              
.*                                                                              
.C       ANOP                                                                   
         AIF    (T'&IAMNODE NE 'O').CERR                                        
         AIF    (T'&CHILDOF NE 'O').CERR                                        
&TAG.10  DC     X'06',AL2(&TAG.11-&TAG.10),X'0006',C'S&COLUMNOF'                
&TAG.11  DC     X'06',AL2(&TAG.12-&TAG.11),X'0008',C'S&MAXTOT'                  
&TAG.12  DC     X'06',AL2(&TAG.13-&TAG.12),X'000B',C'C&GLOBID'                  
&TAG.13  EQU    *                                                               
         AGO    .XXX                                                            
.CERR    ANOP                                                                   
         MNOTE  1,'COLUMNOF SO CANT USE IAMNODE OR CHILDOF'                     
         MEXIT                                                                  
.*                                                                              
.X       ANOP                                                                   
&TAG.X   DC     X'06',AL2(&TAG.Y-&TAG.X),X'000B',C'C&GLOBID'                    
&TAG.Y   DC     X'06',AL2(&TAG.Z-&TAG.Y),X'000C',C'S&MAXW'                      
         AGO   .XXX                                                             
.*                                                                              
.XXX     ANOP                                                                   
&TAG.Z   DC     X'00'                                                           
         MEND                                                                   
         EJECT                                                                  
***********************************************************************         
* Actual schema                                                                 
***********************************************************************         
         PRINT GEN                                                              
SCHTABLE DS    0D                                                               
*                                                                               
FE00     DC    AL2(FE00X-*),AL2(0)                                              
FE00A    DC    X'15',AL2(FE00B-FE00A)                                           
         DC    CL8'XAGY   ',CL3'A49',C'Agency Summary - '                       
FE00$    DC    CL7'Gross $'                                                     
         DC    C' Run On '                                                      
FE00DATE DC    C'MMMDD/YY'                                                      
FE00B    DC    AL1(0)                                                           
FE00X    EQU   *                                                                
*                                                                               
FE0A     DC    AL2(FE0AX-*),AL2(0)                                              
FE0AA    DC    X'15',AL2(FE0AB-FE0AA)                                           
         DC    CL8'XAGYA  ',CL3'A49',C'Agency Summary - '                       
FE0A$    DC    CL7'Gross $'                                                     
         DC    CL11' w/Assigned'                                                
         DC    C' Run On '                                                      
FE0ADATE DC    C'MMMDD/YY'                                                      
FE0AB    DC    AL1(0)                                                           
FE0AX    EQU   *                                                                
*                                                                               
FE01     DC    AL2(FE01X-*),AL2(0)                                              
FE01A    DC    X'06',AL2(FE01B-FE01A),X'FE01'                                   
FE01B    DC    X'06',AL2(FE01C-FE01B),X'0001',C'CDoc'                           
FE01C    DC    X'06',AL2(FE01D-FE01C),X'0002',C'S9999'                          
FE01D    DC    AL1(0)                                                           
FE01X    EQU   *                                                                
*                                                                               
FE012    DC    AL2(FE012X-*),AL2(0) THIS IS A SECOND FE01(2)                    
FE012A   DC    X'06',AL2(FE012B-FE012A),X'FE01'                                 
FE012B   DC    X'06',AL2(FE012C-FE012B),X'0001',C'CS49'                         
FE012C   DC    X'06',AL2(FE012D-FE012C),X'0002',C'F8'                           
FE012D   DC    X'06',AL2(FE012E-FE012D),X'0004',C'H'                            
FE012E   DC    AL1(0)                                                           
FE012X   EQU   *                                                                
*                                  SYSTEM CODE                                  
MPID00   SCHEMA IAMNODE=100,LABEL=SYSTEM,MAPID=1,PREVNODE=8,           X        
               GLOBID=SYSC,MAXW=1,TYPE=C                                        
*                                  SYSTEM NAME                                  
MPID01   SCHEMA CHILDOF=100,LABEL=SYSTEM_NAME,MAPID=2,                 X        
               GLOBID=SYSN,MAXW=5,TYPE=C                                        
*                                  MEDIA CODE                                   
MPID02   SCHEMA IAMNODE=101,LABEL=MEDIA,MAPID=3,PREVNODE=100,          X        
               GLOBID=MDCD,MAXW=1,TYPE=C                                        
*                                  MEDIA NAME                                   
MPID03   SCHEMA CHILDOF=101,LABEL=MEDIA_NAME,MAPID=4,                  X        
               GLOBID=MDNM,MAXW=24,TYPE=C                                       
*                                  CLIENT MEDIA OFFICE                          
MPID04   SCHEMA IAMNODE=102,LABEL=MEDIA_OFFC,MAPID=5,PREVNODE=101,     X        
               GLOBID=MDOF,MAXW=1,TYPE=C                                        
*                                  CLIENT CODE                                  
MPID05   SCHEMA IAMNODE=103,LABEL=CLIENT,MAPID=6,PREVNODE=102,         X        
               GLOBID=CLCD,MAXW=3,TYPE=C                                        
*                                  CLIENT NAME                                  
MPID06   SCHEMA CHILDOF=103,LABEL=CLIENT_NAME,MAPID=7,                 X        
               GLOBID=CLNM,MAXW=24,TYPE=C                                       
*                                  CLIENT ACC OFFICE                            
MPID07   SCHEMA IAMNODE=105,LABEL=CLIENT_ACOF,MAPID=8,PREVNODE=103,    X        
               GLOBID=ACOF,MAXW=2,TYPE=C                                        
*                                  PRODUCT CODE                                 
MPID08   SCHEMA IAMNODE=106,LABEL=PRODUCT,MAPID=9,PREVNODE=105,        X        
               GLOBID=PRCD,MAXW=3,TYPE=C                                        
*                                  PRODUCT NAME                                 
MPID09   SCHEMA CHILDOF=106,LABEL=PRODUCT_NAME,MAPID=10,               X        
               GLOBID=PRNM,MAXW=24,TYPE=C                                       
*                                  ESTIMATE NUMBER                              
MPID10   SCHEMA IAMNODE=108,LABEL=ESTIMATE,MAPID=11,PREVNODE=106,      X        
               GLOBID=ESCD,MAXW=3,TYPE=C                                        
*                                  ESTIMATE NAME                                
MPID11   SCHEMA CHILDOF=108,LABEL=ESTIMATE_NAME,MAPID=12,              X        
               GLOBID=ESNM,MAXW=24,TYPE=C                                       
*                                  ESTIMATE TYPE                                
MPID12   SCHEMA CHILDOF=108,LABEL=ESTIMATE_TYPE,MAPID=13,              X        
               GLOBID=ESTY,MAXW=3,TYPE=C                                        
*                                  MOS CODE FOR SORTING                         
MPID13   SCHEMA IAMNODE=109,LABEL=MOSCODE,MAPID=14,PREVNODE=108,       X        
               GLOBID=MOSC,MAXW=2,TYPE=C                                        
*                                  MOS                                          
MPID14   SCHEMA CHILDOF=109,LABEL=MOS,MAPID=15,                        X        
               GLOBID=MOS,MAXW=10,TYPE=C                                        
*                                                                               
MPID15   SCHEMA COLUMNOF=109,LABEL=ORDERED,MAPID=16,MAXTOT=0,          X        
               GLOBID=ORDG,TYPE=L                                               
*                                                                               
MPID16   SCHEMA COLUMNOF=109,LABEL=CLEARED,MAPID=17,MAXTOT=0,          X        
               GLOBID=CLRG,TYPE=L                                               
*                                                                               
MPID17   SCHEMA COLUMNOF=109,LABEL=UNCLEARED,MAPID=18,MAXTOT=0,        X        
               GLOBID=UNCG,TYPE=L                                               
*                                                                               
MPID18   SCHEMA COLUMNOF=109,LABEL=BILLED,MAPID=19,MAXTOT=0,           X        
               GLOBID=BLLG,TYPE=L                                               
*                                                                               
MPID19   SCHEMA COLUMNOF=109,LABEL=BILLABLE,MAPID=20,MAXTOT=0,         X        
               GLOBID=UNBG,TYPE=L                                               
*                                                                               
MPID20   SCHEMA COLUMNOF=109,LABEL=ASSIGNED,MAPID=20,MAXTOT=0,         X        
               GLOBID=ASSG,TYPE=L                                               
*                                                                               
MPID21   SCHEMA COLUMNOF=109,LABEL=BILLABLE,MAPID=21,MAXTOT=0,         X        
               GLOBID=UNBG,TYPE=L                                               
*                                                                               
         DC    AL2(FE03X-*),AL2(0)                                              
FE03A    DC    AL1(6),AL2(FE03B-FE03A),X'FE03'                                  
FE03B    DC    X'00'                                                            
FE03X    EQU   *                                                                
*                                                                               
         DC    AL2(FE031X-*),AL2(0)                                             
FE031A   DC    AL1(6),AL2(FE031B-FE031A),X'FE03'                                
FE031B   DC    X'00'                                                            
FE031X   EQU   *                                                                
*                                                                               
         DC    AL1(EOT)            END OF TABLE                                 
         EJECT                                                                  
*======================================================================         
* Macro for Report definition                                                   
*======================================================================         
         MACRO                                                                  
&TAG     REPORT &NODEID=,&MAPID=,&TYPE=,&ALIGN=L,&ROW=,&COL=,&H1=,&H2=,+        
               &H3=,&DEC=2,&HIDE=,&SUBTYPE=,&GROUP=,&GLEVEL=,&NOFILT=           
         LCLA  &PROP                                                            
         GBLA  &#HEADS                                                          
         GBLC  &HEADS(8)                                                        
&DINK    SETC  ''''                                                             
                                                                                
&PROP    SETA   X'01'                             Sort Accending                
&TAG     DC     AL2(&TAG.X-&TAG),AL2(0)                                         
.*                                                                              
&TAG.0   DC     X'06',AL2(&TAG.1-&TAG.0),X'FE31'                                
&TAG.1   DC     X'06',AL2(&TAG.2-&TAG.1),X'0001',C'&NODEID'                     
&TAG.2   DC     X'06',AL2(&TAG.3-&TAG.2),X'000B',C'S&MAPID'                     
&TAG.3   DC     X'06',AL2(&TAG.4-&TAG.3),X'0002',C'C&TYPE'                      
         AIF    ('&TYPE'(1,1) EQ 'C').C                                         
&TAG.4   DC     X'06',AL2(&TAG.5-&TAG.4),X'0003',C'C&ALIGN,&ROW,&COL'           
&TAG.5   EQU    *                                                               
         AIF    (T'&H1 EQ 'O').C10                                              
&STRING  SETC   '&H1'                                                           
&LEN     SETA   K'&H1                                                           
&LPOS    SETA   ('&H1' INDEX '&DINK')                                           
         AIF    (&LPOS NE 1).TXT                                                
&LEN     SETA   &LEN-2                                                          
&STRING  SETC   '&STRING'(2,&LEN)                                               
.TXT     ANOP                                                                   
&LEN     SETA   &LEN+1                                                          
         DC     X'06',AL2(&TAG.6-&TAG.5),X'0005',CL&LEN'C&STRING'               
&TAG.6   EQU    *                                                               
         AGO   .C10                                                             
                                                                                
.C       ANOP                                                                   
         AIF    (T'&COL EQ 'O').C1                                              
&TAG.4   DC     X'06',AL2(&TAG.5-&TAG.4),X'0003',C'C&COL'                       
                                                                                
&TAG.5   EQU    *                                                               
.C1      AIF    (T'&H1 EQ 'O').C2                                               
         AIF    ('&H1'(1,1) EQ '@').C1A                                         
&CHEAD   SETC   ' '                                                             
         AIF    ('&H1'(1,5) EQ 'SPACE').SH1                                     
&CHEAD   SETC   '&H1'                                                           
.SH1     DC     X'06',AL2(&TAG.6-&TAG.5),X'0005',C'C&CHEAD'                     
&TAG.6   EQU   *                                                                
         AGO   .C2                                                              
                                                                                
&TAG.5   EQU    *                                                               
.C1A     DC     X'06',AL2(&TAG.6-&TAG.5),X'0005',C'C'                           
&H1      DC    CL5' '                                                           
&#HEADS  SETA  &#HEADS+1                                                        
&HEADS(&#HEADS) SETC '&H1'                                                      
&TAG.6   EQU   *                                                                
                                                                                
.C2      ANOP                                                                   
         AIF   (T'&H2 EQ 'O').C4                                                
&CHEAD   SETC  ' '                                                              
         AIF   ('&H2'(1,5) EQ 'SPACE').SH2                                      
&CHEAD   SETC  '&H2'                                                            
.SH2     DC    X'06',AL2(&TAG.7-&TAG.6),X'0006',C'C&CHEAD'                      
.C4      ANOP                                                                   
&TAG.7   EQU   *                                                                
         AIF   (T'&H3 EQ 'O').C6                                                
         DC    X'06',AL2(&TAG.8-&TAG.7),X'0007',C'C&H3'                         
.C6      ANOP                                                                   
&TAG.8   EQU   *                                                                
         DC    X'06',AL2(&TAG.9-&TAG.8),X'0008',C'S&DEC'                        
.C10     ANOP                                                                   
&TAG.9   EQU   *                                                                
         AIF   (T'&SUBTYPE EQ 'O').C11                                          
         DC     X'06',AL2(&TAG.10-&TAG.9),X'000A',C'C&SUBTYPE'                  
         AIF   ('&SUBTYPE' EQ 'C').C11                                          
&PROP    SETA  X'00'                           Sort only SUBTYPE=C              
                                                                                
.C11     ANOP                                                                   
         AIF   (T'&HIDE EQ 'O').C12                                             
         AIF   ('&HIDE' NE 'Y').C12                                             
&PROP    SETA  &PROP+X'80'                                                      
                                                                                
.C12     ANOP                                                                   
         AIF   (T'&NOFILT EQ 'O').C13                                           
         AIF   ('&NOFILT' NE 'Y').C13                                           
&PROP    SETA  &PROP+X'40'                     No show in quick filt            
                                                                                
.C13     ANOP                                                                   
&TAG.10  DC     X'06',AL2(&TAG.11-&TAG.10),X'000C',C'S&PROP'                    
&TAG.11  EQU   *                                                                
         AIF   (T'&GROUP EQ 'O').C14                                            
         AIF   (T'&GLEVEL EQ 'O').C14                                           
         DC     X'06',AL2(&TAG.12-&TAG.11),X'0011',C'C00&GROUP'                 
         DC    C'00&GLEVEL'                                                     
&TAG.12  EQU    *                                                               
.C14     ANOP                                                                   
.*                                                                              
.X       ANOP                                                                   
         DC    X'00'                                                            
&TAG.X   EQU    *                                                               
         MEXIT                                                                  
         MEND                                                                   
*======================================================================         
* Macro for Generated headings                                                  
*======================================================================         
         MACRO                                                                  
         HEADTAB                                                                
         LCLA  &COUNT                                                           
         GBLA  &#HEADS                                                          
         GBLC  &HEADS(8)                                                        
HDTAB    DS    0A                                                               
&COUNT   SETA  &#HEADS                                                          
.NEXTHD  AIF   (&COUNT EQ 0).HDEOF                                              
         DC    A(&HEADS(&COUNT))                                                
&COUNT   SETA  &COUNT-1                                                         
         AGO   .NEXTHD                                                          
.HDEOF   DC    AL1(EOT)                                                         
         MEXIT                                                                  
         MEND                                                                   
         EJECT ,                                                                
***********************************************************************         
* Actual Report definition                                                      
***********************************************************************         
REPTABLE DS    0D                                                               
*                                                                               
FE11     DC    AL2(FE11X-*),AL2(0)                                              
FE11A    DC    X'06',AL2(FE11B-FE11A),X'FE11'                                   
FE11B    DC    X'06',AL2(FE11C-FE11B),X'0001',C'CREPORT'                        
FE11C    DC    X'06',AL2(FE11D-FE11C),X'0002',C'CSCRIBE'  <=== KEEP !           
FE11D    DC    AL1(0)                                                           
FE11X    EQU   *                                                                
*                                                                               
FE12     DC    AL2(FE12X-*),AL2(0)                                              
FE12A    DC    X'06',AL2(FE12B-FE12A),X'FE12'                                   
FE12B    DC    X'06',AL2(FE12C-FE12B),X'0001',C'CREPORT'                        
FE12C    DC    X'06',AL2(FE12D-FE12C),X'0002',C'CXAGY'                          
FE12CFMT DC    C' '                                                             
FE12D    DC    X'06',AL2(FE12E-FE12D),X'0003',C'CAgency Summary'                
FE12E    DC    X'06',AL2(FE12F-FE12E),X'0004',C'C001Media'                      
FE12F    DC    X'06',AL2(FE12G-FE12F),X'0004',C'C002Advertiser'                 
FE12G    DC    X'06',AL2(FE12Z-FE12G),X'0004',C'C003MOS'                        
FE12Z    DC    AL1(0)                                                           
FE12X    EQU   *                                                                
*                                                                               
* THESE GENERATE FE31 OBJECTS                                                   
*                                                                               
REP00    REPORT NODEID=S8,MAPID=1,TYPE=R,ALIGN=L,ROW=1,COL=1,H1=System,+        
               SUBTYPE=C,GROUP=1,GLEVEL=1                                       
REP01    REPORT NODEID=S8,MAPID=2,TYPE=R,ALIGN=L,ROW=1,COL=2,SUBTYPE=N          
REP02    REPORT NODEID=S8,MAPID=3,TYPE=R,ALIGN=L,ROW=2,COL=1,H1=Media, +        
               SUBTYPE=C,GROUP=1,GLEVEL=2                                       
REP03    REPORT NODEID=S8,MAPID=4,TYPE=R,ALIGN=L,ROW=2,COL=2,SUBTYPE=N          
REP04    REPORT NODEID=S8,MAPID=5,TYPE=R,ALIGN=L,ROW=3,COL=1,          +        
               H1='Media Office',SUBTYPE=C                                      
REP05    REPORT NODEID=S8,MAPID=6,TYPE=R,ALIGN=L,ROW=4,COL=1,H1=Client,+        
               SUBTYPE=C,GROUP=2,GLEVEL=1                                       
REP06    REPORT NODEID=S8,MAPID=7,TYPE=R,ALIGN=L,ROW=4,COL=2,SUBTYPE=N          
REP07    REPORT NODEID=S8,MAPID=8,TYPE=R,ALIGN=L,ROW=5,COL=1,          +        
               H1='Acc Office',SUBTYPE=C                                        
REP08    REPORT NODEID=S8,MAPID=9,TYPE=R,ALIGN=L,ROW=6,COL=1,          +        
               H1=Product,SUBTYPE=C,GROUP=2,GLEVEL=2                            
REP09    REPORT NODEID=S8,MAPID=10,TYPE=R,ALIGN=L,ROW=6,COL=2,SUBTYPE=N         
REP10    REPORT NODEID=S8,MAPID=11,TYPE=R,ALIGN=L,ROW=7,COL=1,         +        
               H1=Estimate,SUBTYPE=C,GROUP=2,GLEVEL=3                           
REP11    REPORT NODEID=S8,MAPID=12,TYPE=R,ALIGN=L,ROW=7,COL=2,         +        
               SUBTYPE=N                                      EST NAME          
REP12    REPORT NODEID=S8,MAPID=13,TYPE=R,ALIGN=L,ROW=7,COL=3,SUBTYPE=O         
REP13    REPORT NODEID=S8,MAPID=14,TYPE=G,ALIGN=L,ROW=8,COL=1,H1=MOS,  +        
               HIDE=Y,SUBTYPE=C,NOFILT=Y                      MOS SORT          
REP14    REPORT NODEID=S8,MAPID=15,TYPE=R,ALIGN=L,ROW=8,COL=2,         +        
               SUBTYPE=N,GROUP=3,GLEVEL=1                                       
*                                                                               
REP20    REPORT NODEID=S8,MAPID=16,TYPE=C,COL=1,H2=Ordered,DEC=2,      +        
               SUBTYPE=A,H1=@H1ORD                                              
REP21    REPORT NODEID=S8,MAPID=17,TYPE=C,COL=2,H2=Cleared,DEC=2,      +        
               SUBTYPE=A,H1=@H1CLR                                              
REP22    REPORT NODEID=S8,MAPID=18,TYPE=C,COL=3,H2=Uncleared,DEC=2,    +        
               SUBTYPE==A16-A17,H1=@H1UNC                                       
REP23    REPORT NODEID=S8,MAPID=19,TYPE=C,COL=4,H2=Billed,DEC=2,       +        
               SUBTYPE=A,H1=@H1BIL                                              
REP24    REPORT NODEID=S8,MAPID=20,TYPE=C,COL=5,H2=Billable,DEC=2,     +        
               SUBTYPE==A16-A19,H1=@H1UB1                                       
REP25    REPORT NODEID=S8,MAPID=20,TYPE=C,COL=5,H2=Ordered,DEC=2,      +        
               SUBTYPE=A,H1=@H1OAS,H3=Assigned                                  
REP26    REPORT NODEID=S8,MAPID=21,TYPE=C,COL=5,H2=Billable,DEC=2,     +        
               SUBTYPE=A,H1=@H1UB2                                              
         DC    AL2(FE61X-*),AL2(0)                                              
FE61A    DC    X'06',AL2(FE61B-FE61A),X'FE61'                                   
FE61B    DC    X'00'                                                            
FE61X    EQU   *                                                                
         DC    AL1(EOT)            END OF TABLE                                 
                                                                                
         HEADTAB                                                                
         EJECT                                                                  
         PRINT NOGEN                                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'*OUTBLK*'                                                    
OUTBLK   DS    8192C                                                            
OUTBLKX  EQU   *                                                                
*                                                                               
         DC    CL8'*WKBUFF*'                                                    
WKBUFF   DS    0D                                                               
         DS    CL14336                                                          
WKBUFFX  EQU   *                                                                
*                                                                               
* DMWRKFL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFL                                                        
         PRINT ON                                                               
* DMWRKFK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFK                                                        
         PRINT ON                                                               
* SPREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
* SPREPMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
*=====================================================================*         
* DSECT to cover formated input                                                 
*=====================================================================*         
THISRECD DSECT                                                                  
THISAGY  DS    CL2                                                              
THISMED  DS    CL1                                                              
THISAGMD DS    XL1                                                              
THISCLT  DS    CL3                                                              
THISPRD  DS    CL3                                                              
THISEST  DS    CL3                                                              
*                                                                               
THISTYPE DS    CL1                                                              
THISMDOF DS    CL2                                                              
THISACOF DS    CL2                                                              
THISCACC DS    CL3                 CLIENT ACCESS CODE                           
THISCMB  DS    PL8                 CURRENT MONTH BILLED                         
THISNPT  DS    PL8                 NET PAID TODAY                               
*                                                                               
THISMDNM DS    CL24                                                             
THISCLNM DS    CL24                                                             
THISPRNM DS    CL24                                                             
THISESNM DS    CL24                                                             
*                                                                               
THISYYMM DS    CL4                 REPEATS 12 MORE TIMES                        
THISORD  DS    PL8                 (20 MORE TIMES FOR PRINT)                    
THISPAID DS    PL8                                                              
THISBLLD DS    PL8                                                              
THISYYM1 EQU   *                                                                
THISOASN DS    PL8                 NETWORK only if ASSIGNED=YES                 
THISYYM2 EQU   *                                                                
                                                                                
*======================================================================         
* DSECT FOR file header                                                         
*======================================================================         
FILEHDRD DSECT                                                                  
FILEID   DS    0CL6                                                             
FILESYSC DS    CL1                 System code                                  
FILERCDE DS    CL2                 Report type                                  
FILERTYP DS    CL3                 Record type                                  
FILESYS  DS    CL4                 System                                       
FILEDATE DS    CL6                 File creation date                           
FILEQSTR DS    CL6                 Request start date                           
FILEQEND DS    CL6                 Request end date                             
FILE$TYP DS    CL1                 Dollar type, gross or net                    
FILE$NET EQU   C'N'                .  Net   dollars                             
FILE$GRS EQU   C'G'                .  Gross dollars                             
FILEHDRQ EQU   *-FILEHDRD                                                       
                                                                                
*======================================================================         
* DSECT for amounts to put out                                                  
*======================================================================         
AMTTABD  DSECT                                                                  
AMTMAP#  DS    AL1                 Map code number                              
AMTIND1  DS    X                   Special indicators                           
AMTICALC EQU   X'80'               .  Calculated column                         
AMTIHIDE EQU   X'01'               .  Hide amount                               
         DS    AL1                                                              
         DS    AL1                                                              
AMTPREV$ DS    A                   A(Previous amount)                           
AMTCURR$ DS    A                   A(Current  amount)                           
AMTLNQ   EQU   *-AMTTABD                                                        
                                                                                
*======================================================================         
* DSECT FOR PRINT LINE                                                          
*======================================================================         
PLINED   DSECT                                                                  
PSYS     DS    CL1                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PMOS     DS    CL6                                                              
         DS    CL1                                                              
PORD     DS    CL10                                                             
         DS    CL1                                                              
PPAID    DS    CL10                                                             
         DS    CL1                                                              
PBILLED  DS    CL10                                                             
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDLINKD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDOFFICED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027SPREPAG02 06/18/07'                                      
         END                                                                    
