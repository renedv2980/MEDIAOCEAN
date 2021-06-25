*          DATA SET SPEZF20    AT LEVEL 022 AS OF 02/19/16                      
*PHASE T23020A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE BINSR31                                                                
***********************************************************************         
*                                                                     *         
* TITLE: T23020 - EASI CPE REPORT REQUEST PROGRAM                     *         
*                                                                     *         
* REGISTER USAGE:                                                     *         
* -------- -----                                                      *         
* R5 - CSECT STORAGE, LITERALS                                        *         
* R7 - ZF20WKD, THE OVERLAY WORKING STORAGE                           *         
* R8 - SPOOLD                                                         *         
* R9 - SYSD                                                           *         
* RA - TWA                                                            *         
* RB - BASE (ONE BASE REGISTER USED THROUGHOUT THE PROGRAM)           *         
* RC - GEND                                                           *         
*                                                                     *         
***********************************************************************         
*                                                                               
***********************************************************************         
         TITLE 'T23020 - EASI CPE REPORT REQUES PROGRAM'                        
*                                                                               
T23020   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ZF20WKDL,**3020**,RR=R2,CLEAR=YES                                
         PRINT NOGEN                                                            
*                                                                               
         LR    R7,RC                                                            
         USING ZF20WKD,R7                                                       
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         LR    R5,RB                                                            
         AHI   R5,CSTORAGE-T23020                                               
         USING CSTORAGE,R5                                                      
*                                                                               
         ST    R2,RELO                                                          
         ST    RC,SVRC                                                          
*                                                                               
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         MVC   AIO,AIO1                                                         
*                                                                               
         BRAS  RE,INIT                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VKEY                                                          
         B     EQXIT                                                            
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+12                                                             
         BRAS  RE,LIST                                                          
         B     EQXIT                                                            
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VKEY     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    FILTRS(FILTRLQ),FILTRS                                           
*                                                                               
* BY DEFAULT, FILTER UID AND AGENCY ARE SET TO LOGIN UID/AGY VALUES             
* IF USER ID FIELD IS FILLED IN, UID FILTER IS REMOVED                          
* IF AGENCY=ALL OPTION IS USED(DDS ONLY), AGY FILTER IS REMOVED                 
*                                                                               
         MVC   FILTBUID,TWAORIG    LOGIN ID                                     
         MVC   FILTAGY,TWAAGY      LOGIN AGENCY                                 
         OI    FILTFLAG,FILTUIDQ+FILTAGYQ                                       
*                                                                               
         MVC   LKAGYBID,TWAORIG    LOGIN ID                                     
         BRAS  RE,LKAGY            LOOKUP ALPHA AGY, USER ID                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TWAAGY,LKAGYAGY     LOOKED UP AGY MUST MATCH TWA AGY             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FILTUID,LKAGYUID    CHAR USER ID                                 
*                                                                               
* CHECK OPTIONS NOW                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPEOPTH                                                       
         CLI   5(R2),0             FIELD EMPTY?                                 
         BE    VK10                                                             
*                                                                               
         BRAS  RE,VOPT                                                          
         BNE   VKERR                                                            
*                                                                               
* USER ID FIELD                                                                 
*                                                                               
VK10     DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPEUIDH                                                       
*                                                                               
         CLI   5(R2),0             FIELD EMPTY?                                 
         BE    VK100               YES - USE DEFAULT UID, AGY                   
*                                                                               
         CLC   =C'ALL',8(R2)       FIRST CHECK FOR THE WORD "ALL"               
         BNE   VK20                                                             
         CLI   5(R2),3                                                          
         BNE   VK20                                                             
*                                                                               
* HAVE "ALL" UID FILTERE HERE                                                   
* NOTE: AGENCY FILTER REMAINS IN PLACE BY DEFAULT                               
         XC    FILTBUID,FILTBUID                                                
         XC    FILTUID,FILTUID                                                  
         NI    FILTFLAG,X'FF'-FILTUIDQ                                          
         B     VK100                                                            
*                                                                               
VK20     DS    0H                  SPECIFIC USER ID ENTERED                     
         MVC   LKAGYUID,8(R2)                                                   
         OC    LKAGYUID,SPACES                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,8(R2)                                                     
         OC    CTIKID,SPACES                                                    
         L     R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,(R4),DMWORK           
         CLI   8(R1),0                                                          
         BNE   VKERR                                                            
         LA    R6,CTIDATA                                                       
*                                                                               
VK30     DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    VKERR                                                            
*                                                                               
         CLI   0(R6),X'02'         DESCRIPTION ELEM                             
         BNE   *+10                                                             
         MVC   LKAGYBID,2(R6)      SAVE BINARY USER ID                          
*                                                                               
         CLI   0(R6),X'06'         AGY ID ELEM                                  
         BNE   *+14                                                             
         MVC   LKAGYAGY,2(R6)      SAVE AGENCY                                  
         B     VK40                                                             
*                                                                               
         ZIC   R0,1(R6)            ADVANCE TO NEXT ELEM                         
         AR    R6,R0                                                            
         B     VK30                                                             
*                                                                               
* MAKE SURE THIS USER ID FILTER VALUE IS ALLOWED                                
VK40     DS    0H                                                               
         TM    FILTFLAG,FILTAGYQ                                                
         BZ    *+14                NO AGY FILTER = DDS REQUEST                  
         CLC   FILTAGY,LKAGYAGY                                                 
         BNE   VKERR                                                            
*                                                                               
         MVC   FILTUID,LKAGYUID                                                 
         MVC   FILTBUID,LKAGYBID                                                
         OI    FILTFLAG,FILTUIDQ                                                
*                                                                               
* MEDIA - TAKEN AS IS (ALPHA CHARACTER ONLY)                                    
*                                                                               
VK100    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPEMEDH                                                       
         CLI   5(R2),0                                                          
         BE    VK300                                                            
*                                                                               
         CLI   5(R2),1                                                          
         BNE   VKERR                                                            
*                                                                               
         LA    R1,8(R2)                                                         
         BRAS  RE,ISALPHA                                                       
         BNE   VKERR                                                            
         MVC   FILTMED,8(R2)                                                    
         OI    FILTFLAG,FILTMEDQ                                                
*                                                                               
* STATION - TAKEN AS IS, CALL LETTERS NOT VALIDATED                             
*                                                                               
VK300    DS    0H                                                               
         LA    R2,CPESTAH                                                       
         CLI   5(R2),0                                                          
         BE    VK400                                                            
*                                                                               
* STATION PRESENT - MAKE SURE WE ALSO HAVE MEDIA                                
*                                                                               
         LA    R2,CPEMEDH                                                       
         MVI   ERROR,MISSING                                                    
         CLI   CPEMEDH+5,X'00'     MEDIA FIELD EMPTY?                           
         BE    VKERR               NO: MEDIA MUST BE PRESENT IF STA IS          
*                                                                               
         CLC   =C'ALL',CPESTA                                                   
         BE    VK400                                                            
*                                                                               
         MVC   FILTSTA,CPESTA                                                   
         OC    FILTSTA,SPACES                                                   
         OI    FILTFLAG,FILTSTAQ                                                
*                                                                               
* SOURCE                                                                        
*                                                                               
VK400    DS    0H                                                               
         LA    R2,CPESRCH                                                       
         CLI   5(R2),0                                                          
         BE    VK500                                                            
*                                                                               
         MVC   FILTSRCE,CPESRC                                                  
         OC    FILTSRCE,SPACES                                                  
         MVC   FILTSRCL,5(R2)                                                   
         OI    FIL2FLAG,FIL2SRCQ                                                
*                                                                               
* MOS                                                                           
*                                                                               
VK500    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPEMOSH                                                       
         CLI   5(R2),0                                                          
         BE    VK600                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(2,CPEMOS),WORK                                      
         CLC   =C'000000',WORK                                                  
         BE    VKERR                                                            
         GOTO1 DATCON,(R1),(0,WORK),(1,FILTMOS)                                 
         NI    FILTMOS,X'FF'-X'A0'  TURN OFF THE X'A0'                          
         OI    FILTFLAG,FILTMOSQ                                                
*                                                                               
* START DATE                                                                    
*                                                                               
VK600    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPEDATH                                                       
         CLI   5(R2),0                                                          
         BE    VK700                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         ICM   R3,15,DMCB                                                       
         BZ    VKERR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(2,FILTSD)                                  
         OI    FILTFLAG,FILTDATQ                                                
*                                                                               
         CLM   R3,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VK700                YES                                         
         LA    R3,1+8(R2,R3)                                                    
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB,DMCB                                                        
         BZ    VKERR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(2,FILTED)                                  
*                                                                               
* DOWNLOAD OPTION                                                               
*                                                                               
VK700    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPEDNLH                                                       
         CLI   5(R2),0                                                          
         BE    VK800                                                            
         CLI   CPEDNL,C'Y'                                                      
         BE    *+16                                                             
         CLI   CPEDNL,C'N'                                                      
         BNE   VKERR                                                            
         B     VK800                                                            
         OI    FIL2FLAG,FIL2DNL                                                 
*                                                                               
* VALIDATE ESTIMATES OPTION                                                     
*                                                                               
VK800    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,CPEESTH                                                       
         CLI   5(R2),0                                                          
         BE    VK900                                                            
         CLI   CPEEST,C'Y'                                                      
         BE    *+12                                                             
         CLI   CPEEST,C'N'                                                      
         BNE   VKERR                                                            
         OI    FIL2FLAG,FIL2EST                                                 
*                                                                               
VK900    DS    0H                                                               
         B     VKXIT                                                            
*                                                                               
VKERR    GOTO1 ERREX                                                            
*                                                                               
VKXIT    DS    0H                                                               
         TM    FIL2FLAG,FIL2EST                                                 
         BZ    VKX200                                                           
         BRAS  RE,FIXEST                                                        
*                                                                               
VKX200   DS    0H                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
LIST     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A2C' DDWRKIO                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VWRKIO,0(R1)                                                     
*                                                                               
* FAKE INDEX CALL TO GET WORKER FILE NAME                                       
*                                                                               
*                                                                               
         TM    FILTFLAG,FILTUIDQ                                                
         BO    LIST10                                                           
*                                                                               
         XC    WRKEZKEY,WRKEZKEY                                                
         MVC   WRKEZUID,TWAORIG                                                 
         MVI   WRKIACTN,WRKIANDX                                                
         GOTO1 VWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    *+14                                                             
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   WRKIFILE+4,C'1'                                                  
*                                                                               
LIST10   DS    0H                                                               
         TM    FIL2FLAG,FIL2DNL                                                 
         BZ    *+8                                                              
         BRAS  RE,INITDL                                                        
*                                                                               
* SET UP TO READ WORKER FILE INDICES                                            
*                                                                               
         XC    WRKEZKEY,WRKEZKEY                                                
         MVC   WRKEZUID,FILTBUID                                                
*                                                                               
LIST20   DS    0H                                                               
         MVI   WRKIACTN,WRKIANDX                                                
         TM    FILTFLAG,FILTUIDQ                                                
         BO    LIST25                                                           
         MVI   WRKINDS,WRKIWFNQ                                                 
         XC    WRKEZUID,WRKEZUID                                                
*                                                                               
LIST25   DS    0H                                                               
         XC    CURSRCE,CURSRCE     CLEAR CURRENT SOURCE                         
*                                                                               
         GOTO1 VWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,0                                                       
         BE    LIST30                                                           
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* REACHED EOF HERE                                                              
*                                                                               
         TM    FILTFLAG,FILTUIDQ   RUNNING FOR ONE UID?                         
         BO    LIST200             YES - DO NOT READ OTHER FILES                
*                                                                               
         BRAS  RE,BUMPWKRF         BUMP TO NEXT WORKER FILE                     
         BNE   LIST200             ALREADY ON THE LAST ONE                      
*                                                                               
         XC    WRKEZKEY,WRKEZKEY   CLEAR OUT THE INDEX                          
         B     LIST20              READ FIRST INDEX FOR NEW FILE                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* HERE WE HAVE A WORKER FILE INDEX IN WRKRINDX (R2)                             
* DO THE WORKER INDEX FILTERING HERE                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
LIST30   DS    0H                                                               
*        BRAS  RE,TRACEIND                                                      
*                                                                               
         MVC   LKAGYBID,WRKEZUID                                                
         BRAS  RE,LKAGY                                                         
         MVC   CURRBUID,LKAGYBID                                                
         MVC   CURRUID,LKAGYUID                                                 
         MVC   CURRAGY,LKAGYAGY                                                 
*                                                                               
         BRAS  RE,FILTIND                                                       
         BNE   LIST20                                                           
*                                                                               
         LR    R0,R0               DEBUG                                        
*                                                                               
* READ WORKER FILE RECORDS FOR THIS BATCH                                       
*                                                                               
LIST50   DS    0H                                                               
         MVI   WRKIACTN,WRKIAGET                                                
         GOTO1 VWRKIO,WRKIOB                                                    
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LIST20                                                           
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LIST60   DS    0H                                                               
         BRAS  RE,DOSRC            SEE IF SOURCE ANY GOOD                       
         BNE   LIST20              NO - WE'RE SKIPPING IT                       
*                                                                               
         L     R3,AIO                                                           
         CLC   4(2,R3),=C'31'      INVOICE RECORD                               
         BE    LIST70                                                           
*                                                                               
         B     LIST50              READ NEXT RECORD                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* PROCESS INVOICE HEADER (31) RECORD                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
LIST70   DS    0H                                                               
         L     R1,AIO                                                           
         LA    R1,7(R1)                                                         
         BRAS  RE,CHKSTAT          CHECK INV STATUS FILTERS                     
         BNE   LIST50              READ NEXT INVOICE                            
*                                                                               
         TM    FIL2FLAG,FIL2CDAT   FILTERING ON CONVERT DATE?                   
         BZ    LIST100                                                          
         L     R1,AIO                                                           
         CLC   8(3,R1),FILTCDAT    CONVERTED DATE MATCHES?                      
         BNE   LIST50              NO - SKIP IT                                 
*                                                                               
LIST100  DS    0H                                                               
         BRAS  RE,PROCINV                                                       
         B     LIST50              NEXT READ                                    
*                                                                               
*                                                                               
*                                                                               
*****************************************************************               
* DONE READING WORKER FILES - PRINT TOTALS                                      
*****************************************************************               
LIST200  DS    0H                                                               
*                                                                               
* DO WE HAVE ANYTHING IN THE BINSRCH TABLE?                                     
*                                                                               
         OC    BP3,BP3             NUMBER OF ENTRIES                            
         BZ    LIST500             NADA! CLOSE FILES AND EXIT                   
*                                                                               
* XC BSENTRY TO DO A READ HIGH FOR THE VERY FIRST TABLE ENTRY                   
*                                                                               
         XC    BSENTRY(BTENTLQ),BSENTRY                                         
         LA    R1,BSENTRY                                                       
         STCM  R1,15,BP1                                                        
         MVI   BP4,X'02'           READ HIGH                                    
         SAM31                                                                  
         GOTO1 VBINSRCH,BSPARS                                                  
         SAM24                                                                  
*                                                                               
         OC    BP1,BP1             A(TABLE ENTRY)                               
         BNZ   *+6                 FOUND ANYTHING?                              
         DC    H'0'                MUST HAVE SOMETHING                          
*                                                                               
* SET UP FOR THE LOOP                                                           
*                                                                               
         MVC   ACURCTAB,ACOLTAB                                                 
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)     SKIP A PAGE AFTER REQ DETAILS                
*                                                                               
         MVC   BTABADDR,BP1        A(FIRST ENTRY IN TABLE)                      
         ICM   R6,15,BP3           NUMBER OF ENTRIES                            
*                                                                               
* NOW LOOP THROUGH THE TABLE AND PRINT TOTALS                                   
*                                                                               
LIST210  DS    0H                                                               
         LA    R1,BSENTRY                                                       
         ICM   R1,8,=AL1(BTENTLQ)                                               
         BRAS  RE,FROM31           COPY RECORD INTO BSENTRY                     
         BRAS  RE,PRTTOT                                                        
         BNE   LIST300                                                          
*                                                                               
* ADVANCE TO NEXT ENTRY IN BINSEARCH TABLE                                      
*                                                                               
LIST300  DS    0H                                                               
         ICM   R0,15,BTABADDR                                                   
         AHI   R0,BTENTLQ                                                       
         STCM  R0,15,BTABADDR                                                   
         BCT   R6,LIST210                                                       
*                                                                               
*                                                                               
*                                                                               
* * * * * * ** * * * * * * * * * * * * * * * * * * * * * * * * * * * *          
* PRINT INDIVIDUAL INVOICES, IF ANY                                             
* * * * * * ** * * * * * * * * * * * * * * * * * * * * * * * * * * * *          
LIST500  DS    0H                                                               
         LA    R2,DLCB                                                          
         USING DLCBD,R2                                                         
         MVI   DNFIRST,C'Y'                                                     
         DROP  R2                                                               
*                                                                               
* DO WE HAVE ANYTHING IN THE INDIVIDUALS' BINSRCH TABLE?                        
*                                                                               
         OC    B2P3,B2P3           NUMBER OF ENTRIES                            
         BZ    LIST700             NONE! CLOSE FILES AND EXIT                   
*                                                                               
* SKIP A LINE BETWEEN COUNTERS AND DETAILS                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   ACURCTAB,ACOLTAB2                                                
*                                                                               
         TM    FIL2FLAG,FIL2DNL                                                 
         BO    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* XC BSENTRY2 TO DO A READ HIGH FOR THE VERY FIRST TABLE ENTRY                  
*                                                                               
         XC    BSENTRY2(B2ENTLQ),BSENTRY2                                       
         LA    R1,BSENTRY2                                                      
         STCM  R1,15,B2P1                                                       
         MVI   B2P4,X'02'          READ HIGH                                    
         SAM31                                                                  
         GOTO1 VBINSRCH,BSPARS2                                                 
         SAM24                                                                  
*                                                                               
         OC    B2P1,B2P1           A(TABLE ENTRY)                               
         BNZ   *+6                 FOUND ANYTHING?                              
         DC    H'0'                MUST HAVE SOMETHING                          
*                                                                               
* SET UP FOR THE LOOP                                                           
*                                                                               
         MVC   BTABADDR,B2P1       A(FIRST ENTRY IN TABLE)                      
         ICM   R6,15,B2P3          NUMBER OF ENTRIES                            
*                                                                               
* NOW LOOP THROUGH THE TABLE AND PRINT INDIVIDUAL INVOICES                      
*                                                                               
LIST510  DS    0H                                                               
         LA    R1,BSENTRY2                                                      
         ICM   R1,8,=AL1(B2ENTLQ)                                               
         BRAS  RE,FROM31           COPY RECORD INTO BSENTRY                     
*                                                                               
         MVI   PBPRFLAG,C'N'                                                    
*                                                                               
LIST520  DS    0H                                                               
         BRAS  RE,PRTINV                                                        
*                                                                               
         LA    R1,BSENTRY2+B2ERRTYP-BTAB2D                                      
         BRAS  RE,FINDMASK                                                      
         ZIC   RE,BYTE                                                          
         LHI   RF,X'FF'                                                         
         XR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+8                                                              
         NI    BSENTRY2+B2ERRTYP-BTAB2D,0                                       
*                                                                               
         CLI   BSENTRY2+B2ERRTYP-BTAB2D,0                                       
         BNE   LIST520             YES - PROCESS THIS INVOICE AGAIN             
*                                                                               
* ADVANCE TO NEXT ENTRY IN BINSEARCH TABLE                                      
*                                                                               
LIST530  DS    0H                                                               
         ICM   R0,15,BTABADDR                                                   
         AHI   R0,B2ENTLQ                                                       
         STCM  R0,15,BTABADDR                                                   
         BCT   R6,LIST510                                                       
*                                                                               
LIST700  DS    0H                                                               
*                                                                               
* END OF REPORT FOR DOWNLOAD VERSION                                            
*                                                                               
         TM    FIL2FLAG,FIL2DNL                                                 
         BZ    LISTX                                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVI   DLCB+DLCBACT-DLCBD,C'R'                                          
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
LISTX    DS    0H                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GENERAL INITIALIZATION                                                        
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         MVC   WRKFILE,=C'WRKF    '                                             
*                                                                               
         L     RE,=V(BINSRCH)                                                   
         ST    RE,VBINSRCH                                                      
*                                                                               
         L     R1,=A(MYHEAD)                                                    
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         L     RE,=A(COLTAB)                                                    
         A     RE,RELO                                                          
         ST    RE,ACOLTAB                                                       
*                                                                               
         L     RE,=A(COLTAB2)                                                   
         A     RE,RELO                                                          
         ST    RE,ACOLTAB2                                                      
*                                                                               
         L     RE,=A(COMTAB)                                                    
         A     RE,RELO                                                          
         ST    RE,ACOMTAB                                                       
*                                                                               
         L     RE,=A(SRCETAB)                                                   
         A     RE,RELO                                                          
         ST    RE,ASRCETAB                                                      
***                                                                             
         LA    RE,SYSD                                                          
         AHI   RE,WRKFBUFR-SYSD                                                 
         ST    RE,AWKBUFF                                                       
*                                                                               
         LA    RE,IDTAB                                                         
         LHI   RF,IDTABL                                                        
         XCEFL                                                                  
*                                                                               
         MVI   MISCFLG,X'00'                                                    
         MVI   PROBFLG,X'00'                                                    
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   INIT10                                                           
         CLI   FIRSTSW,C'Y'                                                     
         BNE   INIT10                                                           
         MVI   FIRSTSW,C'N'                                                     
         BRAS  RE,GETMEM                                                        
*                                                                               
* INITIALIZE BINSEARCH31 PARAMETERS                                             
INIT10   DS    0H                                                               
         XC    BSPARS,BSPARS                                                    
* P1                                                                            
         LA    R1,BSENTRY                                                       
         STCM  R1,15,BP1                                                        
* P2                                                                            
         MVC   BP2,ABSBUFF                                                      
* P3                                                                            
         XC    BP3,BP3             NUMBER OF RECORDS SO FAR                     
* P4                                                                            
         LHI   R1,BTENTLQ          TABLE ENTRY LENGTH                           
         STCM  R1,15,BP4                                                        
* P5                                                                            
         LHI   R1,BTKEYLQ          LENGTH OF KEY                                
         STCM  R1,15,BP5           DISP OF KEY INTO RECORD                      
* P6                                                                            
         ICM   R1,15,=AL4(BTENTNQ)                                              
         STCM  R1,15,BP6           MAX NUMBER OF RECORDS                        
*                                                                               
*                                                                               
* INITIALIZE BINSEARCH31 PARAMETERS - BLOCK 2                                   
*                                                                               
         XC    BSPARS2,BSPARS2                                                  
* P1 - 2                                                                        
         LA    R1,BSENTRY2                                                      
         STCM  R1,15,B2P1                                                       
* P2 - 2                                                                        
         L     R1,ABSBUFF          A(STORAGE) RETURNED BY GETMAIN               
* CALCULATE SIZE OF FIRST TABLE                                                 
         ICM   R0,15,=AL4(BTENTNQ)                                              
         MHI   R0,BTENTLQ                                                       
         AR    R1,R0                                                            
         STCM  R1,15,B2P2                                                       
* P3 - 2                                                                        
         XC    B2P3,B2P3           NUMBER OF RECORDS SO FAR                     
* P4 - 2                                                                        
         LHI   R1,B2ENTLQ          TABLE ENTRY LENGTH                           
         STCM  R1,15,B2P4                                                       
* P5 - 2                                                                        
         LHI   R1,B2KEYLQ          LENGTH OF KEY                                
         STCM  R1,15,B2P5          DISP OF KEY INTO RECORD                      
* P6 - 2                                                                        
         ICM   R1,15,=AL4(B2ENTNQ)                                              
         STCM  R1,15,B2P6          MAX NUMBER OF RECORDS                        
*                                                                               
* INITIALIZE WORKIO BLOCK                                                       
*                                                                               
         XC    WRKIOB(WRKIOBL),WRKIOB                                           
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO                                                     
         MVC   WRKIABUF,AWKBUFF                                                 
         MVI   WRKIFTYP,WRKIFTEZ                                                
*                                                                               
INITX    J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
ISALPHA  CLI   0(R1),C'A'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'Z'                                                       
         JH    ISNEQX                                                           
         J     ISEQX                                                            
*                                                                               
ISNUM    CLI   0(R1),C'0'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'9'                                                       
         JH    ISNEQX                                                           
*                                                                               
ISEQX    CR    RB,RB                                                            
         BR    RE                                                               
ISNEQX   LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
******                                                                          
MYHEAD   NTR1  BASE=*,LABEL=*                                                   
******                                                                          
         CLI   DOHDHOOK,C'Y'                                                    
         BE    *+12                                                             
         MVI   DOHDHOOK,C'Y'                                                    
         B     MYHX                                                             
*                                                                               
         LHI   R6,0                R6 = HEADER LINE NUM-1                       
*                                                                               
MYH05    DS    0H                                                               
         LA    R2,H1                                                            
         LR    R0,R6                                                            
         MHI   R0,L'HEAD1                                                       
         AR    R2,R0                                                            
*                                                                               
         MVC   0(L'HEAD1,R2),SPACES                                             
*                                                                               
         ST    R2,PBAOUT                                                        
         ST    R6,PBHLINE                                                       
         L     R1,=A(PTHEAD)                                                    
         A     R1,RELO                                                          
         BRAS  RE,DOCOLTAB                                                      
*                                                                               
MYH100   DS    0H                                                               
         AHI   R6,1                NEXT HEADER LINE                             
         CHI   R6,CTBHDNQ                                                       
         BL    MYH05                                                            
*                                                                               
* DO HEADER UNDERLINES                                                          
*                                                                               
         LHI   R0,CTBHDNQ                                                       
         MHI   R0,L'HEAD1                                                       
         LA    R2,H1                                                            
         AR    R2,R0                                                            
         ST    R2,PBAOUT                                                        
         L     R1,=A(PTUNDER)                                                   
         A     R1,RELO                                                          
         BRAS  RE,DOCOLTAB                                                      
*                                                                               
MYH200   DS    0H                                                               
MYHX     J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
INITDL   NTR1  BASE=*,LABEL=*                                                   
         XC    HEADHOOK,HEADHOOK                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
         MVI   DNFIRST,C'Y'                                                     
         USING DLCBD,R2                                                         
         LA    R2,DLCB                                                          
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
         MVC   P,SPACES                  JUST IN CASE                           
         MVI   DLCBACT,C'I'              START AND INTIALIZE REPORT             
         GOTO1 =V(DLFLD),DLCB                                                   
         DROP  R2                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
DNPRINT  NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'N'                                                    
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
FIXEST   NTR1  BASE=*,LABEL=*                                                   
         L     R1,ACOLTAB                                                       
         USING COLTABD,R2                                                       
*                                                                               
         LA    R2,(CTIECV-COLTAB)(R1)                                           
         MVI   CTBACTIV,X'01'                                                   
         LA    R2,(CTIECI-COLTAB)(R1)                                           
         MVI   CTBACTIV,X'01'                                                   
         LA    R2,(CTIECM-COLTAB)(R1)                                           
         MVI   CTBACTIV,X'01'                                                   
*                                                                               
         L     R1,ACOLTAB                                                       
         LA    R2,(CTICPE-COLTAB)(R1)                                           
         LA    R3,CTBHD1                                                        
         ZIC   R0,CTBWIDTH                                                      
         MHI   R0,CTICPELQ-2                                                    
         AR    R3,R0                                                            
         ZIC   RE,CTBWIDTH                                                      
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=C'CPE                  '                                
*                                                                               
         J     EQXIT                                                            
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* PRINT OUT FILTERS SUMMARY                                                     
***********************************************************************         
PRTFTR   NTR1  BASE=*,LABEL=*                                                   
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(16),=CL16'FILTERS SUMMARY:'                                    
         MVC   P2(16),=CL16'----------------'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF10   DS    0H                                                               
         TM    FILTFLAG,FILTAGYQ                                                
         BZ    PRTF20                                                           
         MVC   P(8),=CL8'AGENCY: '                                              
         MVC   P+8(2),FILTAGY                                                   
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF20   DS    0H                                                               
         TM    FILTFLAG,FILTUIDQ                                                
         BZ    PRTF30                                                           
         MVC   P(9),=CL9'USER ID: '                                             
         MVC   P+9(10),FILTUID                                                  
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF30   DS    0H                                                               
         TM    FILTFLAG,FILTMOSQ                                                
         BZ    PRTF40                                                           
         MVC   P(5),=CL5'MOS: '                                                 
         GOTO1 DATCON,DMCB,(1,FILTMOS),(6,P+5)                                  
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF40   DS    0H                                                               
         TM    FILTFLAG,FILTSTAQ                                                
         BZ    PRTF50                                                           
         MVC   P(9),=CL9'STATION: '                                             
         MVC   P+9(4),FILTSTA                                                   
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF50   DS    0H                                                               
         TM    FILTFLAG,FILTMEDQ                                                
         BZ    PRTF60                                                           
         MVC   P(7),=CL7'MEDIA: '                                               
         MVC   P+7(1),FILTMED                                                   
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF60   DS    0H                                                               
         TM    FILTFLAG,FILTDATQ                                                
         BZ    PRTF70                                                           
         MVC   P(28),=CL28'LOADED BETWEEN          AND '                        
         GOTO1 DATCON,DMCB,(2,FILTSD),(5,P+15)                                  
         GOTO1 DATCON,DMCB,(2,FILTED),(5,P+28)                                  
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF70   DS    0H                                                               
         TM    FIL2FLAG,FIL2SRCQ                                                
         BZ    PRTF80                                                           
         MVC   P(8),=CL28'SOURCE: '                                             
         MVC   P+8(4),FILTSRCE                                                  
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF80   DS    0H                                                               
*&&DO                                                                           
         TM    FIL2FLAG,FIL2STAT                                                
         BZ    PRTF90                                                           
*                                                                               
         MVC   P(10),=CL10'COUNTING '                                           
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
*                                                                               
         TM    FILTSTAT,EZIHCVQ    FILTERING ON CONVERTED?                      
         BZ    PRTF80A                                                          
         MVC   P+10(15),=CL15'CONVERTED'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF80A  DS    0H                                                               
         TM    FILTSTAT,EZIHRCVQ   FILTERING ON RECONVERTED?                    
         BZ    PRTF80B                                                          
         MVC   P+10(15),=CL15'RECONVERTED'                                      
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF80B  DS    0H                                                               
         TM    FILTSTAT,EZIHCOVR   FILTERING ON OVERRIDES?                      
         BZ    PRTF80C                                                          
         MVC   P+10(15),=CL15'WITH OVERRIDES'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF80C  DS    0H                                                               
         TM    FILTSTAT,EZIHCDEL   FILTERING ON DELETED?                        
         BZ    PRTF80D                                                          
         MVC   P+10(15),=CL15'DELETED'                                          
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF80D  DS    0H                                                               
         TM    FILTSTAT,EZIHREPI   FILTERING ON CONVERTED TO REP?               
         BZ    PRTF80E                                                          
         MVC   P+10(20),=CL20'CONVERTED TO REI'                                 
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTF80E  DS    0H                                                               
         TM    FILTSTAT,X'01'      FILTERING ON RECEIVED FROM IM?               
         BZ    PRTF90                                                           
         MVC   P+10(20),=CL20'RECEIVED FROM IM'                                 
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*&&                                                                             
*                                                                               
PRTF90   DS    0H                                                               
*                                                                               
PRTFX    DS    0H                                                               
         MVI   SKIPSPEC,C'N'                                                    
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GO THROUGH COLTAB ENTRIES, CALLING A PROCESS SUBROUTINE FOR EACH              
* COLUMN, AND PASSING ADDRESS OF COLTAB ENTRY IN R3                             
*                                                                               
* R1 EXPECTED TO ADDRESS A PROCESS SUBROUTINE                                   
***********************************************************************         
DOCOLTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LTR   R1,R1                                                            
         BZ    DOCTNQX                                                          
         CLC   0(4,R1),=X'90ECD00C'  NTR1?                                      
         BNE   DOCTNQX                                                          
*                                                                               
         L     R3,ACURCTAB                                                      
         USING COLTABD,R3                                                       
*                                                                               
DOCT10   DS    0H                                                               
         CLI   CTBLN,X'FF'                                                      
         BE    DOCTQX                                                           
*                                                                               
         CLI   CTBACTIV,X'00'                                                   
         BE    DOCT20                                                           
*                                                                               
         LR    RF,R1                                                            
         BASR  RE,RF                                                            
*                                                                               
DOCT20   SR    R0,R0                                                            
         ICM   R0,3,CTBLN                                                       
         AR    R3,R0                                                            
         B     DOCT10                                                           
*                                                                               
DOCTQX   J     EQXIT                                                            
DOCTNQX  J     NEQXIT                                                           
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* DOCOLTAB'S PROCESS SUBROUTINES                                                
* PBLOCK(4) EXPECTED TO HAVE A(OUTPUT)                                          
* R3 EXPECTED TO ADDRESS COLTAB ENTRY                                           
***********************************************************************         
* USING STATEMENT COMMON TO ALL DOCOLTAB SUBROUTINES                            
         USING COLTABD,R3                                                       
*                                                                               
* PTUNDER - PRINT UNDERLINE                                                     
PTUNDER  NTR1  BASE=*,LABEL=*                                                   
         L     R2,PBAOUT                                                        
*                                                                               
         ZIC   RE,CTBWIDTH                                                      
         MVI   0(R2),C'-'                                                       
         BCTR  RE,0                                                             
         CHI   RE,0                                                             
         BNH   PTUNX                                                            
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(R2)                                                    
*                                                                               
         ZIC   R0,CTBWIDTH                                                      
         AR    R2,R0               ADVANCE PAST COLUMN HEADING                  
         ZIC   R0,CTBSPACE                                                      
         AR    R2,R0                                                            
         ST    R2,PBAOUT                                                        
*                                                                               
PTUNX    J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
* PRINT HEADER LINES                                                            
PTHEAD   NTR1  BASE=*,LABEL=*                                                   
         L     R2,PBAOUT                                                        
*                                                                               
         L     RF,PBHLINE          RE/RF PAIR = HEADING NUMBER-1                
         ZIC   R0,CTBWIDTH         COLUMN WIDTH                                 
         MR    RE,R0                                                            
         LA    R4,CTBHD1                                                        
         AR    R4,RF                                                            
*                                                                               
         CLI   0(R4),X'40'         LOWER THAN SPACE?                            
         BNL   *+12                                                             
         BRAS  RE,GETCOMM          YES, MULTI-COLUMN HEADER                     
         B     PTH50                                                            
*                                                                               
         ZIC   RE,CTBWIDTH                                                      
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),SPACES      ANYTHING IN THERE?                           
         BE    PTH50               NO, DON'T CLOBBER THE HEADER LINE            
*                                                                               
         ZIC   RE,CTBWIDTH                                                      
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R4)                                                    
*                                                                               
PTH50    DS    0H                                                               
         ZIC   R0,CTBWIDTH                                                      
         AR    R2,R0               ADVANCE PAST COLUMN HEADING                  
         ZIC   R0,CTBSPACE                                                      
         AR    R2,R0               ADVANCE PAST COLUMN SPACE                    
         ST    R2,PBAOUT                                                        
*                                                                               
PTHEADX  J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
* PRINT INDIVIDUAL BTABD-TYPE ENTRY                                             
PTENTRY  NTR1  BASE=*,LABEL=*                                                   
         L     R2,PBAOUT                                                        
*                                                                               
         LR    R1,R3               A(COLTAB ENTRY)                              
         ICM   RF,15,CTBOUTR       A(OUTPUT SUBROUTINE)                         
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
         ZIC   R0,CTBWIDTH         COLUMN WIDTH                                 
         AR    R2,R0                                                            
         ZIC   R0,CTBSPACE         COLUMN SPACE                                 
         AR    R2,R0                                                            
         ST    R2,PBAOUT                                                        
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
* PRINT HEADER LINES - DOWNLOAD FORMAT                                          
PDHEAD   NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R6                                                         
         LA    R6,DLCB                                                          
*                                                                               
         L     RF,PBHLINE          RE/RF PAIR = HEADING NUMBER-1                
         ZIC   R0,CTBWIDTH         COLUMN WIDTH                                 
         MR    RE,R0                                                            
         LA    R4,CTBHD1                                                        
         AR    R4,RF                                                            
*                                                                               
         CLI   0(R4),C' '                                                       
         BNL   PDH10                                                            
*                                                                               
         LA    R2,DLCBFLD                                                       
         BRAS  RE,GETCOMM          YES, MULTI-COLUMN HEADER                     
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB      SEND FIELD                                   
         B     PDHEADX                                                          
*                                                                               
PDH10    DS    0H                                                               
         ZIC   RE,CTBWIDTH         COLUMN WIDTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R4)    COLUMN HEADING                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB      SEND FIELD                                   
*                                                                               
PDHEADX  J     EQXIT                                                            
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
* PRINT INDIVIDUAL BTABD-TYPE ENTRY, DOWNLOAD FORMAT                            
PDENTRY  NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R4                                                         
         LA    R4,DLCB                                                          
*                                                                               
         LR    R1,R3               A(COLTAB ENTRY)                              
         LA    R2,DLCBFLD                                                       
         ICM   RF,15,CTBOUTR       A(OUTPUT SUBROUTINE)                         
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB      SEND FIELD                                   
*                                                                               
PDNTRYX  J     EQXIT                                                            
         LTORG                                                                  
         DROP  R4                                                               
*                                                                               
* DROP R3 - STATEMENT COMMON TO ALL DOCOLTAB SUBROUTINES                        
         DROP  R3                                                               
*                                                                               
*                                                                               
***********************************************************************         
* PRINT MULTI-COLUMN HEADERS                                                    
* R4 EXPECTED TO HAVE A(COMMON HEADER NUMBER)                                   
* R2 = A(DESTINATION)                                                           
***********************************************************************         
GETCOMM  NTR1  BASE=*,LABEL=*                                                   
         L     R1,ACOMTAB                                                       
*                                                                               
GETC10   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   0(1,R4),0(R1)                                                    
         BE    GETC20                                                           
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETC10                                                           
*                                                                               
GETC20   DS    0H                                                               
         ZIC   RE,1(R1)                                                         
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         SHI   RE,3                MINUS LENGTH,NUMBER, BCTR                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),2(R1)                                                    
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* PRINT SUBROUTINES FOR COLTAB                                                  
*                                                                               
* R1 TO ADDRESS COLTAB ENTRY                                                    
* R2 TO ADDRESS DESTINATION                                                     
***********************************************************************         
* USING STATEMENT COMMON TO ALL COLTAB PRINT SUBROUTINES                        
         USING COLTABD,R1                                                       
*                                                                               
PRTBLANK NTR1  BASE=*,LABEL=*                                                   
         MVI   0(R2),C' '                                                       
         J     EQXIT                                                            
*                                                                               
PRTXT    NTR1  BASE=*,LABEL=*                                                   
         ICM   R0,15,CTBDISP                                                    
         LTR   R0,R0                                                            
         JZ    EXIT                                                             
         L     R3,ABSENTRY                                                      
         AR    R3,R0                                                            
*                                                                               
         ZIC   RE,CTBWIDTH                                                      
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R3)                                                    
*                                                                               
         ZIC   RE,CTBWIDTH                                                      
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),SPACES                                                   
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
PRERR    NTR1  BASE=*,LABEL=*                                                   
         ZIC   RE,CTBWIDTH                                                      
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SPACES                                                   
*                                                                               
         ICM   R0,15,CTBDISP                                                    
         L     R3,ABSENTRY                                                      
         AR    R3,R0                                                            
*                                                                               
         TM    0(R3),PFCMISQ                                                    
         BZ    *+14                                                             
         MVC   WORK,=C'CLT MISSING  '                                           
         B     PRERR10                                                          
*                                                                               
         TM    0(R3),PFCINVQ                                                    
         BZ    *+14                                                             
         MVC   WORK,=C'CLT INVALID   '                                          
         B     PRERR10                                                          
*                                                                               
         TM    0(R3),PFPMISQ                                                    
         BZ    *+14                                                             
         MVC   WORK,=C'PRD MISSING   '                                          
         B     PRERR10                                                          
*                                                                               
         TM    0(R3),PFPINVQ                                                    
         BZ    *+14                                                             
         MVC   WORK,=C'PRD INVALID   '                                          
         B     PRERR10                                                          
*                                                                               
         TM    0(R3),PFEMISQ                                                    
         BZ    *+14                                                             
         MVC   WORK,=C'EST MISSING   '                                          
         B     PRERR10                                                          
*                                                                               
         TM    0(R3),PFEINVQ                                                    
         BZ    *+14                                                             
         MVC   WORK,=C'EST INVALID   '                                          
         B     PRERR10                                                          
*                                                                               
         TM    0(R3),PFOVERQ                                                    
         BZ    *+14                                                             
         MVC   WORK,=C'OVERRIDE        '                                        
         B     PRERR10                                                          
*                                                                               
         DC    H'0'                                                             
*                                                                               
PRERR10  DS    0H                                                               
         ZIC   RE,CTBWIDTH                                                      
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK                                                     
*                                                                               
         ZIC   RE,CTBWIDTH                                                      
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),SPACES                                                   
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
* VERY SPECIFIC SUBROUTINE, PRINTS MOS FROM EASI BATCH INDEX                    
PRMOS    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   R0,15,CTBDISP                                                    
         L     R3,ABSENTRY                                                      
         AR    R3,R0                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),0(R3)                                                    
         GOTO1 DATCON,DMCB,(1,WORK),(6,0(R2))                                   
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
PRPACK   NTR1  BASE=*,LABEL=*                                                   
         ZIC   R4,CTBWIDTH                                                      
         ICM   R0,15,CTBDISP                                                    
         L     R3,ABSENTRY                                                      
         AR    R3,R0                                                            
*                                                                               
         ZAP   DUB,0(L'BTTOTST,R3)                                              
         MVC   WORK(17),=X'4040404040402020202020202020202020'                  
         MVI   WORK+15,X'21'                                                    
         ED    WORK(17),DUB+2                                                   
         LA    R3,WORK                                                          
         AHI   R3,17                                                            
         SR    R3,R4                                                            
         CHI   R4,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R3)                                                    
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
* DROP STATEMENT COMMON TO ALL COLTAB PRINT SUBROUTINES                         
         DROP  R1                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* AGENCY INFORMATION LOOKUP SUBROUTINE                                          
* TAKES IN 2-BYTE BINARY USER ID (IN LKAGYBLK,LKAGYBID)                         
* RETURNS 10-CHARACTER USER ID ADN 2-CHAR AGENCY POWER CODE                     
* LKAGYBLK IS FILLED IN WITH 2-CHAR AGY AND 10-CHAR USER ID                     
* LKAGYBLK WILL HAVE X'FF' IF LOOKUP UNSUCCESSFUL                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
LKAGY    NTR1  BASE=*,LABEL=*                                                   
         MVI   LKAGYAGY,X'FF'      DEFAULT TO NO LOOKUP                         
         MVC   LKAGYUID,=C'UNKNOWN  '                                           
*                                                                               
* LOOK IN THE TABLE FIRST                                                       
*                                                                               
         LHI   R0,IDTABL/L'LKAGYBLK                                             
         LA    R2,IDTAB                                                         
*                                                                               
LK100    CLI   0(R2),X'00'         EMPTY SPACE?                                 
         BE    LK200               YES - ID NOT IN TABLE                        
         CLC   LKAGYBID,0(R2)      SAME USER ID?                                
         BE    *+14                YES - FOUND IT!                              
         LA    R2,L'LKAGYBLK(R2)                                                
         BCT   R0,LK100                                                         
         DC    H'0'                TABLE FULL                                   
*                                                                               
* FOUND THE ID                                                                  
         MVC   LKAGYBLK,0(R2)                                                   
         B     LKXIT               EXIT - DO NOT SAVE INFO IN TABLE             
*                                                                               
* ID NOT IN THE TABLE                                                           
*                                                                               
LK200    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),LKAGYBID                                             
         L     R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,(R4),DMWORK           
         CLI   8(R1),0                                                          
         BNE   LKXITSV                                                          
         LA    R6,CTIDATA                                                       
*                                                                               
LK500    DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    LKXITSV                                                          
*                                                                               
         CLI   0(R6),X'02'         DESCRIPTION ELEM                             
         BNE   *+16                                                             
         MVC   LKAGYUID,2(R6)                                                   
         OC    LKAGYUID,SPACES                                                  
*                                                                               
         CLI   0(R6),X'06'         AGY ID ELEM                                  
         BE    LK540                                                            
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LK500                                                            
         USING CTAGYD,R6                                                        
*                                                                               
LK540    MVC   LKAGYAGY,CTAGYID                                                 
*                                                                               
* SAVE INFO IN THE TABLE                                                        
*                                                                               
LKXITSV  MVC   0(L'LKAGYBLK,R2),LKAGYBLK    SAVE BINARY USER ID                 
*                                                                               
LKXIT    DS    0H                                                               
         CLI   LKAGYAGY,X'FF'                                                   
         JNE   EQXIT                                                            
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* UPDATES/ZAPS TOTALS                                                           
* R1 EXPECTED TO ADDRESS TOTALS LIST                                            
* R4 EXPECTED TO ADDRESS BINSEARCH TABLE ENTRY                                  
* TOTALS LIST WILL BE UPDATED WITH VALUES ADDRESSED BY BTABADDR                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
ZAPTOT   DS    0H                                                               
         MVI   ZAPFLAG,C'Y'                                                     
         J     UPDTOT1                                                          
*                                                                               
UPDTOT   DS    0H                                                               
         MVI   ZAPFLAG,C'N'                                                     
*                                                                               
UPDTOT1  NTR1  BASE=*,LABEL=*                                                   
         LHI   R2,BTNTOTQ                                                       
         LA    R4,BTTOTST-BTABD(R4)                                             
*                                                                               
UPDTOT10 DS    0H                                                               
         CLI   ZAPFLAG,C'Y'                                                     
         BNE   *+14                                                             
         ZAP   0(8,R1),=P'0'                                                    
         B     *+10                                                             
*                                                                               
         AP    0(8,R1),0(8,R4)                                                  
*                                                                               
UPDTOT20 DS    0H                                                               
         LA    R1,8(R1)     ADVANCE IN TOTALS LIST                              
         LA    R4,8(R4)     ADVANCE IN BINSRCH TABLE                            
         BCT   R2,UPDTOT10                                                      
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* R1 EXPECTED TO ADDRESS THE RECORD                                             
* R0 EXPECTED TO HAVE THE COLUMN NUMBER                                         
* NOTE, THAT RECORD, FIELD DELIMITERS ARE HARD-CODED                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
FINDFLD  NTR1  BASE=*,LABEL=*                                                   
         CHI   R0,0                TARGET COLUMN NUMBER                         
         BNH   FINDNQX                                                          
*                                                                               
         LH    R2,0(R1)            REC LEN                                      
         AR    R2,R1               R2 POINTS TO THE END OF RECORD               
         LA    R3,4(R1)            SKIP THE 4 REC LENGTH BYTES                  
         LHI   R6,1                CURRENT COLUMN NUMBER                        
*                                                                               
FIND10   DS    0H                                                               
         CR    R0,R6                                                            
         BNH   FIND50              WE'VE ARRIVED AT THE TARGET COLUMN           
*                                                                               
* SKIP CURRENT FIELD                                                            
*                                                                               
* FIELDS 2,37 GET SPECIAL TREATMENT IN '31' RECORD                              
         CLC   =C'31',4(R1)                                                     
         BNE   FIND20                                                           
         CHI   R6,2                ARE WE AT FIELD 2?                           
         BE    *+12                YES - SEE IF WE'VE GOT '31' RECORD           
         CHI   R6,37               ARE WE AT FIELD 37?                          
         BNE   FIND20              NO - REGULAR FIELD SKIP LOGIC                
*                                                                               
         LA    R3,13(R3)           SKIP 12-BYTE SAVE FIELD+SEMI                 
         AHI   R6,1                INCREMENT CURRENT FIELD #                    
*                                                                               
         CR    R3,R2               DID WE ADVANCE PAST EOR?                     
         BNL   FINDNQX                                                          
         CLI   0(R3),X'15'                                                      
         BE    FINDNQX                                                          
         B     FIND10                                                           
*                                                                               
FIND20   DS    0H                  REGULAR FIELD SKIP LOGIC                     
         CR    R3,R2               REACHED EOR?                                 
         BNL   FINDNQX                                                          
         CLI   0(R3),X'15'         EOR?                                         
         BE    FINDNQX                                                          
         CLI   0(R3),X'5E'         EOF?                                         
         BE    *+12                                                             
*                                                                               
         LA    R3,1(R3)                                                         
         B     FIND20                                                           
*                                                                               
         LA    R3,1(R3)            SKIP THE SEMICOLON                           
         AHI   R6,1                                                             
         B     FIND10                                                           
*                                                                               
FIND50   DS    0H                                                               
         XC    GETFBLK(GETFBKLQ),GETFBLK                                        
*                                                                               
         CLC   =C'31',4(R1)                                                     
         BNE   FIND60                                                           
         CHI   R6,2                ARE WE AT FIELD 2?                           
         BE    *+12                YES - SEE IF WE'VE GOT '31' RECORD           
         CHI   R6,37               ARE WE AT FIELD 37?                          
         BNE   FIND60              NO - REGULAR FIELD SKIP LOGIC                
*                                                                               
         MVC   GETFDATA(12),0(R3)                                               
         MVI   GETFLEN,12                                                       
         B     FINDQX                                                           
*                                                                               
FIND60   DS    0H                                                               
         LR    R0,R1               A(RECORD)                                    
         LR    R1,R3               A(CURR FIELD)                                
         BRAS  RE,GETFLD                                                        
*                                                                               
FINDQX   LA    R1,1(R1)            ADVANCE TO THE FIRST CHAR OF THE FLD         
         CR    RB,RB               EXIT WITH EQUAL CONDITION CODE               
         XIT1  REGS=(R1)                                                        
*                                                                               
FINDNQX  J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FROM31/TO31                                                                   
* COPIES CONTENTS OF MEMORY 0(R1) FROM/TO ABOVE-THE-LINE BINSRCH TABLE          
* R1 TO ADDRESS THE BELOW-THE-LINE MEMORY LOCATION                              
* HIGH-ORDER BYTE OF R1 TO HAVE THE LENGTH OF DATA TO BE COPIED                 
* BTABADDR IS TO CONTAIN ADDRESS OF THE BINSRCH TABLE                           
***********************************************************************         
FROM31   NTR1  BASE=*,LABEL=*                                                   
         ICM   R2,15,BTABADDR                                                   
         LR    R3,R1                                                            
         SRL   R3,24               NOW RE HAS THE LENGTH OF DATA                
         CHI   R3,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         CHI   R3,255                                                           
         BL    *+6                                                              
         DC    H'0'                                                             
         BCTR  R3,0                                                             
         LA    R1,0(R1)            CLEAR THE HIGH ORDER BYTE                    
*                                                                               
         SAM31                                                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)                                                    
         SAM24                                                                  
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
TO31     NTR1  BASE=*,LABEL=*                                                   
         ICM   R2,15,BTABADDR                                                   
         LR    R3,R1                                                            
         SRL   R3,24                                                            
         CHI   R3,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         CHI   R3,255                                                           
         BL    *+6                                                              
         DC    H'0'                                                             
         BCTR  R3,0                                                             
         LA    R1,0(R1)            CLEAR THE HIGH-ORDER BYTE                    
*                                                                               
         SAM31                                                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R1)                                                    
         SAM24                                                                  
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* R0 EXPECTED TO HAVE ADDRESS OF THE RECORD                                     
* R1 EXPECTED TO ADDRESS THE FIELD                                              
* ON EXIT GETFBLK WILL HAVE DATA AND FIELD LENGTH                               
***********************************************************************         
GETFLD   NTR1  BASE=*,LABEL=*                                                   
         XC    GETFBLK(GETFBKLQ),GETFBLK                                        
*                                                                               
         LR    RF,R0               A(RECORD)                                    
         SR    R0,R0                                                            
         ICM   R0,3,0(RF)          L'RECORD                                     
         AR    R0,RF               R0 = A(EOR)                                  
*                                                                               
* DROP LEADING BLANKS FIRST                                                     
GETF10   DS    0H                                                               
         BRAS  RE,GETFEOF                                                       
         BNE   GETFXIT             EOF BEFORE FIRST CHAR = BLANK FIELD          
*                                                                               
         CLI   0(R1),X'00'                                                      
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   GETF20                                                           
         LA    R1,1(R1)            ADVANCE TO NEXT CHAR                         
         B     GETF10                                                           
*                                                                               
GETF20   DS    0H                  R1 = A(FIRST CHAR) HERE                      
         LR    R2,R1               SAVE A(FIRST CHAR) IN R2                     
*                                                                               
* FIND EOF NOW                                                                  
GETF21   DS    0H                                                               
         BRAS  RE,GETFEOF                                                       
         BNE   GETF30                                                           
*                                                                               
         LA    R1,1(R1)                                                         
         B     GETF21                                                           
*                                                                               
* DROP THE TRAILING BLANKS NOW                                                  
GETF30   DS    0H                  R1=EOF HERE                                  
         BCTR  R1,0                BACK UP ONE                                  
*                                                                               
GETF31   DS    0H                  R1=EOF HERE                                  
         CR    R1,R2                                                            
         BNH   GETF40              REACHED START OF RECORD HERE                 
         CLI   0(R1),X'00'         BLANK                                        
         BE    *+12                NO - LAST CHAR OF THE FIELD                  
         CLI   0(R1),C' '          SPACE?                                       
         BNE   GETF40              NO - LAST CHAR OF THE FIELD                  
         BCT   R1,GETF31                                                        
*                                                                               
* CALCULATE FIELD LENGTH AND COPY DATA                                          
GETF40   DS    0H                  R2=FIRST CHAR,R1=LAST CHAR                   
         SR    R1,R2               LENGTH OF RECORD                             
         AHI   R1,1                                                             
         STC   R1,GETFLEN                                                       
*                                                                               
         CHI   R1,0                                                             
         BH    *+6                                                              
         DC    H'0'                VERY, VERY SAD                               
         CHI   R1,L'GETFDATA                                                    
         BNH   *+8                                                              
         LHI   R1,L'GETFDATA                                                    
*                                                                               
         CHI   R1,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GETFDATA(0),0(R2)                                                
*                                                                               
GETFXIT  J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* R0 TO ADDRESS EOR                                                             
* R1 TO ADDRESS CURRENT POSITION                                                
* ON EXIT - EQUAL CONDITION IF NOT END OF FIELD                                 
*           UNEQUAL OTHERWISE                                                   
***********************************************************************         
GETFEOF  CR    R1,R0               REACHED EOR?                                 
         JNL   GEOFNEQX            YES - UNEQ                                   
         CLI   0(R1),X'15'         EOR?                                         
         JE    GEOFNEQX                                                         
         CLI   0(R1),X'5E'         END OF FIELD DELIMITER?                      
         JE    GEOFNEQX                                                         
*                                                                               
GEOFEQX  CR    RB,RB                                                            
         BR    RE                                                               
GEOFNEQX LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
***********************************************************************         
* R2 TO ADDRESS EOR                                                             
* R3 TO ADDRESS CURRENT POSITION                                                
* ON EXIT - EQUAL CONDITION IF EOF                                              
*           UNEQUAL OTHERWISE                                                   
***********************************************************************         
ISEOF    CR    R3,R2               REACHED EOR?                                 
         JNL   ISEOFQX             YES - UNEQ                                   
         CLI   0(R3),X'15'         EOR?                                         
         JE    ISEOFQX                                                          
         CLI   0(R3),X'5E'         END OF FIELD DELIMITER?                      
         JE    ISEOFQX                                                          
*                                                                               
ISEOFNQX LTR   RB,RB                                                            
         BR    RE                                                               
ISEOFQX  CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
***********************************************************************         
* R0 TO ADDRESS CLT/PRD CODE LENGTH                                             
* R1 TO ADDRESS CLT/PRD CODE                                                    
* ON EXIT EQUAL CONDITION IF CODE IS AAA/AAN/ANN                                
*         UNEQUAL OTHERWISE                                                     
***********************************************************************         
VCOD     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CHI   R0,2                                                             
         BL    VCODNEQX                                                         
         CHI   R0,3                                                             
         BH    VCODNEQX                                                         
*                                                                               
* FIRST CHAR MUST BE ALPHA                                                      
*                                                                               
         BCTR  R0,0                                                             
         BRAS  RE,ISALPHA                                                       
         BNE   VCODNEQX                                                         
         LA    R1,1(R1)                                                         
*                                                                               
VCOD10   BRAS  RE,ISALPHA                                                       
         BE    *+12                                                             
         BRAS  RE,ISNUM                                                         
         BNE   VCODNEQX                                                         
*                                                                               
         LA    R1,1(R1)                                                         
         BCT   R0,VCOD10                                                        
*                                                                               
VCODEQX  J     EQXIT                                                            
VCODNEQX J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* R1 TO ADDRESS EST CODE                                                        
* R1 HIGH ORDER BYTE CONTAINS LENGTH                                            
* ON EXIT EQUAL CONDITION IF EST CODE IS ALL NUMERIC                            
*         UNEQUAL OTHERWISE                                                     
***********************************************************************         
VEST     NTR1  BASE=*,LABEL=*                                                   
         LR    R0,R1                                                            
         SRL   R0,24                                                            
*                                                                               
VEST10   BRAS  RE,ISNUM                                                         
         BNE   VESTNEQX                                                         
*                                                                               
         AHI   R1,1                                                             
         BCT   R0,VEST10                                                        
*                                                                               
VESTEQX  J     EQXIT                                                            
VESTNEQX J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* FILTER EASI BATCH INDICES                                                     
* R2 EXPECTED TO ADDRESS THE INDEX                                              
***********************************************************************         
FILTIND  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* MAKE SURE EASI BATCH                                                          
*                                                                               
         CLI   WRKEZDAY,WRKEZDYQ                                                
         BNE   FINQX               NO - NEXT INDEX                              
*                                                                               
* CHECK BATCH DATE FILTER                                                       
*                                                                               
         TM    FILTFLAG,FILTDATQ                                                
         BZ    FI10                                                             
*                                                                               
         OC    FILTED,FILTED       SECOND DATE?                                 
         BNZ   FI05                YES - CHECK DATE RANGE                       
*                                                                               
* ONLY ONE DATE                                                                 
         CLC   FILTSD,WRKEZBDT                                                  
         BNE   FINQX                                                            
         B     FI10                                                             
*                                                                               
FI05     DS    0H                                                               
         CLC   FILTSD,WRKEZBDT                                                  
         BH    FINQX                                                            
         CLC   FILTED,WRKEZBDT                                                  
         BL    FINQX                                                            
*                                                                               
FI10     DS    0H                                                               
*                                                                               
* CHECK USER ID FILTER                                                          
*                                                                               
         TM    FILTFLAG,FILTUIDQ   UID FILTER?                                  
         BZ    FI20                NO                                           
         CLC   FILTBUID,WRKEZUID                                                
         BNE   FINQX               WRONG ID - READ NEXT INDEX                   
*                                                                               
* CHECK MOS FILTER                                                              
*                                                                               
FI20     DS    0H                                                               
         TM    FILTFLAG,FILTMOSQ   ARE WE FILTERING BY MOS?                     
         BZ    FI30                                                             
*                                                                               
         CLC   FILTMOS,WRKEZMOS                                                 
         BNE   FINQX               WRONG MOS - READ NEXT INDEX                  
*                                                                               
FI30     DS    0H                                                               
         TM    FILTFLAG,FILTUIDQ   UID FILTER PRESENT?                          
         BO    FI50                YES - NO AGY LOOKUP NEEDED                   
*                                  UID IS A MORE SPECIFIC FILTER                
*                                  AGY FILTER WILL BE IGNORED                   
* CHECK AGENCY FILTER                                                           
*                                                                               
FI40     DS    0H                                                               
         TM    FILTFLAG,FILTAGYQ                                                
         BZ    FI50                                                             
         CLC   FILTAGY,CURRAGY                                                  
         BNE   FINQX                                                            
*                                                                               
* LOOKUP EQUIVALENT STATION                                                     
*                                                                               
FI50     DS    0H                                                               
         MVC   SRCESTA(4),WRKEZSCL                                              
         MVC   SRCESTA+4(1),WRKEZMED                                            
*                                                                               
         GOTO1 VEQVSTA                                                          
*                                                                               
* CHECK MEDIA FILTER                                                            
*                                                                               
         TM    FILTFLAG,FILTMEDQ                                                
         BZ    FI60                                                             
*                                                                               
         LA    R1,EQUISTA+4                                                     
         ICM   R1,8,=AL1(EZMTBNDQ)                                              
         GOTO1 VGETMED                                                          
         BNE   FINQX                                                            
*                                                                               
         MVC   CURMED,EZMTMED-EZMEDTBD(RF)                                      
*                                                                               
         CLC   FILTMED,EQUISTA+4   MEDIA AS IS                                  
         BE    FI60                                                             
         CLC   FILTMED,CURMED      MEDIA SET TO (R,T,N,X)                       
         BNE   FINQX               NO - NEXT INDEX                              
*                                                                               
FI60     DS    0H                                                               
*                                                                               
* CHECK STATION FILTER                                                          
*                                                                               
         TM    FILTFLAG,FILTSTAQ                                                
         BZ    FI70                                                             
*                                                                               
         CLC   FILTSTA(4),EQUISTA                                               
         BNE   FINQX               WRONG STATION - READ NEXT INDEX              
*                                                                               
FI70     DS    0H                                                               
*                                                                               
FIQX     J     EQXIT                                                            
FINQX    J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INCREMENT WORKER FILE NAME                                                    
* WORKER FILE NAME EXPECTED TO BE STORED IN WRKFILE (CSECT)                     
***********************************************************************         
BUMPWKRF NTR1  BASE=*,LABEL=*                                                   
         LA    R1,WRKIFILE                                                      
         A     R1,RELO                                                          
         CLI   4(R1),C'F'      ARE WE ON THE LAST FILE?                         
         BE    BUMPNEQX        YES                                              
*                                                                               
* NO - INCREMENT THE WORKER FILE NUMBER                                         
*                                                                               
         CLI   4(R1),C'9'      GO TO NEXT FILE                                  
         BNE   *+12                                                             
         MVI   4(R1),C'A'                                                       
         B     BUMPEQX                                                          
*                                                                               
         ZIC   R0,4(R1)                                                         
         AHI   R0,1                                                             
         STC   R0,4(R1)                                                         
*                                                                               
BUMPEQX  J     EQXIT                                                            
BUMPNEQX J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* CKSRC - CHECK FOR EXCLUDED SOURCES                                            
* R1 EXPECTED TO ADDRESS THE 4-CHAR SOURCE                                      
* UNEQUAL CONDITION CODE SET IF SOURCE IS FOUND IN EXCLUDE TABLE                
***********************************************************************         
CKSRC    NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,SRCECT                                                        
         L     R2,ASRCETAB                                                      
*                                                                               
CKSRC10  DS    0H                                                               
         CLC   0(3,R1),=C'IPS'                                                  
         BE    CKSRCNQX                                                         
         CLC   0(4,R1),0(R2)                                                    
         BE    CKSRCNQX                                                         
*                                                                               
         LA    R2,4(,R2)                                                        
         BCT   R0,CKSRC10                                                       
*                                                                               
CKSRCQX  J     EQXIT                                                            
CKSRCNQX J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* OBTAIN BATCH SOURCE, CHECK SRC FILTER AND EXCLUSION TABLE                     
***********************************************************************         
DOSRC    NTR1  BASE=*,LABEL=*                                                   
         OC    CURSRCE,CURSRCE     HAVE WE OBTAINED SOURCE ALREADY?             
         BNZ   DOSRCQX             YES - DON'T DO IT AGAIN                      
*                                                                               
* OBTAIN SOURCE FROM FILE DESCRIPTION                                           
*                                                                               
         MVC   CURSRCE,WRKEZDSC+EZWCSRCE-EZWKRCMD                               
         OC    CURSRCE,SPACES                                                   
*                                                                               
DOSRC10  DS    0H                                                               
*                                                                               
* CHECK SOURCE FILTER, IF NECESSARY                                             
*                                                                               
         TM    FIL2FLAG,FIL2SRCQ   HAVE SOURCE FILTER?                          
         BZ    DOSRC20             NO - PROCEED TO EXCLUSION TABLE              
*                                                                               
         CLC   FILTSRCE,CURSRCE    SAME SOURCE AS FILTER?                       
         BNE   DOSRCNQX                                                         
         B     DOSRCQX                                                          
*                                                                               
DOSRC20  DS    0H                                                               
         LA    R1,CURSRCE                                                       
         BRAS  RE,CKSRC            CHECK SOURCE EXCLUSION TABLE                 
         BNE   DOSRCNQX                                                         
*                                                                               
DOSRCQX  J     EQXIT                                                            
DOSRCNQX J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* CHECKS INVOICE STATUS                                                         
* R1 EXPECTED TO ADDRESS EZIHCVST BYTE                                          
***********************************************************************         
CHKSTAT  NTR1  BASE=*,LABEL=*                                                   
         TM    FIL2FLAG,FIL2STAT                                                
         BZ    CHKST100                                                         
*                                                                               
         TM    FILTSTAT,EZIHCVQ    FILTERING ON CONVERTED?                      
         BZ    *+12                                                             
         TM    0(R1),EZIHCVQ       CONVERTED?                                   
         BZ    CHKSTNQX            NO - SKIP IT                                 
*                                                                               
         TM    FILTSTAT,EZIHRCVQ   FILTERING ON RECONVERTED?                    
         BZ    *+12                                                             
         TM    0(R1),EZIHRCVQ      RECONVERTED?                                 
         BZ    CHKSTNQX            NO - SKIP IT                                 
*                                                                               
         TM    FILTSTAT,EZIHCOVR   FILTERING ON OVERRIDES?                      
         BZ    *+12                                                             
         TM    0(R1),EZIHCOVR      C/P/E OVERRRIDES?                            
         BZ    CHKSTNQX            NO - SKIP IT                                 
*                                                                               
         TM    FILTSTAT,EZIHCDEL   FILTERING ON DELETED?                        
         BZ    *+12                                                             
         TM    0(R1),EZIHCDEL      DELETED?                                     
         BZ    CHKSTNQX            NO - SKIP IT                                 
*                                                                               
         TM    FILTSTAT,EZIHREPI   FILTERING ON CONVERTED TO REP?               
         BZ    *+12                                                             
         TM    0(R1),EZIHREPI      CONVERTED TO REI?                            
         BZ    CHKSTNQX            NO - SKIP IT                                 
*                                                                               
         TM    FILTSTAT,X'01'      FILTERING ON RECEIVED FROM IM?               
         BZ    *+12                                                             
         TM    0(R1),X'01'         RECEIVED FROM IM?                            
         BZ    CHKSTNQX            NO - SKIP IT                                 
*                                                                               
         B     CHKSTQX             SKIP CHECK FOR DELETES                       
*                                                                               
* DEFAULT CHECK: WE'RE SKIPPING DELETES, UNLESS FILTER IS PRESENT               
*                                                                               
CHKST100 DS    0H                                                               
         TM    0(R1),EZIHCDEL      DELETED INVOICE?                             
         BO    CHKSTNQX            IGNORE IT                                    
*                                                                               
CHKSTQX  J     EQXIT                                                            
CHKSTNQX J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* PRINT TOTALS                                                                  
* BSENTRY EXPECTED TO HAVE THE COUNTERS                                         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRTTOT   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,BSENTRY                                                       
         ST    R1,ABSENTRY                                                      
         CP    BSENTRY+BTINVCNT-BTABD(L'BTTOTST),=P'0'                          
         BE    PRTTNQX             NO - NOTHING TO PRINT                        
*                                                                               
         TM    FIL2FLAG,FIL2DNL                                                 
         BZ    PRTT50                                                           
*                                                                               
         LA    R2,DLCB                                                          
         USING DLCBD,R2                                                         
*                                                                               
         MVC   DNLINE,P           SAVE CONTENTS OF PRINTLINE                    
         MVC   P,SPACES                                                         
         MVC   DNLINE2,P2         SAVE CONTENTS OF PRINTLINE2                   
         MVC   P2,SPACES                                                        
         MVC   DNLINE3,P3         SAVE CONTENTS OF PRINTLINE2                   
         MVC   P3,SPACES                                                        
*                                                                               
         CLI   DNFIRST,C'Y'                                                     
         BNE   PRTT10                                                           
*                                                                               
* FIRST TIME CALLING DOWNLD                                                     
*                                                                               
         MVI   DNFIRST,C'N'                                                     
         LHI   R6,0                R6 = HEADER LINE NUM-1                       
*                                                                               
PRTT01   DS    0H                                                               
         ST    R6,PBHLINE                                                       
         L     R1,=A(PDHEAD)                                                    
         A     R1,RELO                                                          
         MVC   ACURCTAB,ACOLTAB                                                 
         BRAS  RE,DOCOLTAB                                                      
         MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 =V(DLFLD),DLCB                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         AHI   R6,1                NEXT HEADER LINE                             
         CHI   R6,CTBHDNQ                                                       
         BL    PRTT01                                                           
*                                                                               
PRTT10   DS    0H                                                               
         L     R1,=A(PDENTRY)                                                   
         A     R1,RELO                                                          
         MVC   ACURCTAB,ACOLTAB                                                 
         BRAS  RE,DOCOLTAB                                                      
*                                                                               
         MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 =V(DLFLD),DLCB                                                   
         B     PRTTQX                                                           
*                                                                               
PRTT50   DS    0H                                                               
         LA    R2,P                A(OUTPUT DESTINATION)                        
         ST    R2,PBAOUT                                                        
         L     R1,=A(PTENTRY)                                                   
         A     R1,RELO                                                          
         MVC   ACURCTAB,ACOLTAB                                                 
         BRAS  RE,DOCOLTAB                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTTQX   J     EQXIT                                                            
PRTTNQX  J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* PRINT INDIVIDUAL "PROBLEM "INVOICES                                           
* BSENTRY2 EXPECTED TO HAVE THE BINSRCH TABLE ENTRY                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRTINV   NTR1  BASE=*,LABEL=*                                                   
         MVI   PBPRFLAG,C'Y'                                                    
         LA    R1,BSENTRY2                                                      
         ST    R1,ABSENTRY                                                      
*                                                                               
         BRAS  RE,FIXDISP                                                       
*                                                                               
         TM    FIL2FLAG,FIL2DNL                                                 
         BZ    PRTINV50                                                         
*                                                                               
         LA    R2,DLCB                                                          
         USING DLCBD,R2                                                         
*                                                                               
         MVC   DNLINE,P           SAVE CONTENTS OF PRINTLINE                    
         MVC   P,SPACES                                                         
         MVC   DNLINE2,P2         SAVE CONTENTS OF PRINTLINE2                   
         MVC   P2,SPACES                                                        
         MVC   DNLINE3,P3         SAVE CONTENTS OF PRINTLINE2                   
         MVC   P3,SPACES                                                        
*                                                                               
         CLI   DNFIRST,C'Y'                                                     
         BNE   PRTINV10                                                         
*                                                                               
* FIRST TIME CALLING DOWNLD                                                     
         MVI   DNFIRST,C'N'                                                     
         LHI   R6,0                R6 = HEADER LINE NUM-1                       
*                                                                               
PRTINV01 DS    0H                                                               
         ST    R6,PBHLINE                                                       
         L     R1,=A(PDHEAD)                                                    
         A     R1,RELO                                                          
         MVC   ACURCTAB,ACOLTAB2                                                
         BRAS  RE,DOCOLTAB                                                      
         MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 =V(DLFLD),DLCB                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         AHI   R6,1                NEXT HEADER LINE                             
         CHI   R6,CTBHDNQ                                                       
         BL    PRTINV01                                                         
*                                                                               
*                                                                               
PRTINV10 DS    0H                                                               
         L     R1,=A(PDENTRY)                                                   
         A     R1,RELO                                                          
         MVC   ACURCTAB,ACOLTAB2                                                
         BRAS  RE,DOCOLTAB                                                      
*                                                                               
         MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 =V(DLFLD),DLCB                                                   
         B     PRTIQX                                                           
*                                                                               
PRTINV50 DS    0H                                                               
         LA    R2,P                A(OUTPUT DESTINATION)                        
         ST    R2,PBAOUT                                                        
         L     R1,=A(PTENTRY)                                                   
         A     R1,RELO                                                          
         MVC   ACURCTAB,ACOLTAB2                                                
         BRAS  RE,DOCOLTAB                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTIQX   J     EQXIT                                                            
PRTINQX  J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
***********************************************************************         
* PROCESS INVOICE HEADER ("31") RECORD                                          
***********************************************************************         
PROCINV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* HAVE A COUNTABLE INVOICE HERE                                                 
* FIRST, SEE IF THERE IS AN ENTRY FOR THIS UID/SRC/STA/MOS                      
* IN THE BINSRCH TABLE                                                          
*                                                                               
         XC    BSENTRY(BTENTLQ),BSENTRY                                         
*                                                                               
         LA    R4,BSENTRY                                                       
         USING BTABD,R4                                                         
*                                                                               
* BUILD BINSRCH TABLE KEY                                                       
*                                                                               
         MVC   BTUID,=X'FFFF'      DEFAULT TO UNKNOWN ID                        
         CLI   CURRAGY,X'FF'                                                    
         BE    *+10                UNKNOWN ID                                   
         MVC   BTUID,CURRBUID      USER ID                                      
         MVC   BTSTA,PRTSTA7C                                                   
         MVC   BTSRCE,CURSRCE                                                   
         MVC   BTMOS,WRKEZMOS                    MOS                            
         DROP  R4                                                               
*                                                                               
* SEARCH IN TABLE.  IF KEY NOT THERE - ADD IT                                   
*                                                                               
         MVI   BP4,X'01'           ADD RECORD IF NOT FOUND                      
         LA    R1,BSENTRY          RECORD TO BE LOOKED UP/ADDED                 
         STCM  R1,15,BP1           PASS A(RECORD) IN PARAM 1                    
*                                                                               
         SAM31                                                                  
         GOTO1 VBINSRCH,BSPARS                                                  
         SAM24                                                                  
*                                                                               
         OC    BP1+1(3),BP1+1      TEST TABLE FULL                              
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL - FOR NOW JUST DUMP               
*                                                                               
         MVC   BTABADDR,BP1        SAVE A(TABLE ENTRY) RIGHT AWAY               
         LA    R1,BSENTRY                                                       
         ICM   R1,8,=AL1(BTENTLQ)                                               
         BRAS  RE,FROM31        COPY REC FROM ABOVE THE LINE TO BSENTRY         
*                                                                               
         LA    R4,BSENTRY                                                       
         USING BTABD,R4                                                         
*                                                                               
* INITIALIZE THE RECORD, IF RECORD IS ADDED                                     
*                                                                               
         TM    BP1,X'80'           RECORD FOUND?                                
         BZ    PRINV10             YES - DON'T ZAP                              
*                                                                               
         LA    R1,BTTOTST                                                       
         BRAS  RE,ZAPTOT           RECORD NOT FOUND - ZAP TOTALS                
         MVC   BTSEQ,WRKEZSQN      BATCH SEQ NUMBER                             
         MVC   BTUSERID,=CL10'UNKNOWN'                                          
         CLI   CURRAGY,X'FF'                                                    
         BE    *+10                UNKNOWN ID                                   
         MVC   BTUSERID,CURRUID                                                 
*                                                                               
PRINV10  DS    0H                                                               
         AP    BTINVCNT,=P'1'      INCREMENT TOTAL INVOICE COUNT                
*                                  TURN OFF VALID/INVALID/MISSING FLAGS         
         NI    MISCFLG,X'FF'-MFVALCQ-MFVALPQ-MFVALEQ                            
         MVI   PROBFLG,X'00'                                                    
*                                                                               
* SEE IF AGENCY ADVERTISER CODE PRESENT                                         
*                                                                               
         LHI   R0,26               COLUMN 25 - AGENCY ADV CODE                  
         L     R1,AIO              A(INVOICE HEADER SAVE AREA)                  
         BRAS  RE,FINDFLD                                                       
         BNE   PRINV20             FIELD NOT FOUND - NO CLIENT                  
         CLI   GETFLEN,X'00'       ANY DATA?                                    
         BNE   PRINV25             YES - VALIDATE IT                            
*                                                                               
PRINV20  DS    0H                                                               
         OI    PROBFLG,PFCMISQ     INDICATE ADV CODE MISSING                    
         AP    BTINVCCM,=P'1'      INCREMENT MISSING CLIENT COUNT               
         B     PRINV30             DO THE PRODUCT CODE                          
*                                                                               
PRINV25  DS    0H                                                               
         CLI   GETFLEN,3                                                        
         BH    PRINV26                                                          
         ZIC   R0,GETFLEN                                                       
         LA    R1,GETFDATA                                                      
         BRAS  RE,VCOD                                                          
         BE    *+18                                                             
PRINV26  AP    BTINVCCI,=P'1'      INCREMENT INVALID CLIENT COUNT               
         OI    PROBFLG,PFCINVQ     INDICATE INVALID CLIENT CODE                 
         B     PRINV30             DO THE PRODUCT CODE                          
*                                                                               
         AP    BTINVCCV,=P'1'      INCREMENT VALID CLIENT COUNT                 
         OI    MISCFLG,MFVALCQ     INDICATE HAVE VALID CLIENT CODE              
*                                                                               
* SEE IF AGENCY PRD CODE PRESENT                                                
*                                                                               
PRINV30  DS    0H                                                               
         LHI   R0,28               COLUMN 27 - AGENCY PRD CODE                  
         L     R1,AIO              A(INVOICE HEADER SAVE AREA)                  
         BRAS  RE,FINDFLD                                                       
         BNE   PRINV40             FIELD NOT FOUND - NO PRODUCT                 
         CLI   GETFLEN,X'00'       ANY DATA?                                    
         BNE   PRINV45                                                          
*                                                                               
PRINV40  DS    0H                                                               
         OI    PROBFLG,PFPMISQ                                                  
         AP    BTINVPCM,=P'1'      INCREMENT MISSING PRD COUNT                  
         B     PRINV50                                                          
*                                                                               
PRINV45  DS    0H                                                               
         ZIC   R0,GETFLEN                                                       
         LA    R1,GETFDATA                                                      
         BRAS  RE,VCOD                                                          
         BE    *+18                                                             
         OI    PROBFLG,PFPINVQ                                                  
         AP    BTINVPCI,=P'1'      INCREMENT INVALID PRD COUNT                  
         B     PRINV50                                                          
*                                                                               
         AP    BTINVPCV,=P'1'      INCREMENT VALID PRD COUNT                    
         OI    MISCFLG,MFVALPQ                                                  
*                                                                               
* SEE IF ESTIMATE CODE IS PRESENT                                               
*                                                                               
PRINV50  DS    0H                                                               
         TM    FIL2FLAG,FIL2EST                                                 
         BZ    PRINV70                                                          
*                                                                               
         LHI   R0,9                COLUMN 8 - AGENCY EST CODE                   
         L     R1,AIO              A(INVOICE HEADER SAVE AREA)                  
         BRAS  RE,FINDFLD                                                       
         BNE   PRINV60             FIELD NOT FOUND - NO ESTIMATE                
         CLI   GETFLEN,X'00'       ANY DATA?                                    
         BNE   PRINV65                                                          
*                                                                               
PRINV60  DS    0H                  ESTIMATE MISSING                             
         OI    PROBFLG,PFEMISQ                                                  
         AP    BTINVECM,=P'1'      INCREMENT MISSING ESTIMATE COUNT             
         B     PRINV70                                                          
*                                                                               
PRINV65  DS    0H                                                               
         CLI   GETFLEN,X'03'                                                    
         BH    PRINV67                                                          
*                                                                               
         LA    R1,GETFDATA                                                      
         ICM   R1,8,GETFLEN                                                     
         BRAS  RE,VEST                                                          
         BE    PRINV68                                                          
*                                                                               
PRINV67  DS    0H                  ESTIMATE INVALID                             
         OI    PROBFLG,PFEINVQ                                                  
         AP    BTINVECI,=P'1'      INCREMENT INVALID ESTIMATE COUNT             
         B     PRINV70                                                          
*                                                                               
PRINV68  DS    0H                  ESTIMATE VALID                               
         AP    BTINVECV,=P'1'      INCREMENT VALID ESTIMATE COUNT               
         OI    MISCFLG,MFVALEQ     INDICATE HAVE VALID EST CODE                 
*                                                                               
* PROCESS OVERRIDES, IF ANY                                                     
*                                                                               
PRINV70  DS    0H                                                               
         L     R2,AIO                                                           
         TM    7(R2),EZIHCOVR      OVERRIDES?                                   
         BZ    PRINV80                                                          
*                                                                               
         AP    BTINVOVR,=P'1'      INCREMENT UNCONVERTED COUNT                  
*                                                                               
* IF C/P/E CODES ARE MISSING/INVALID, OVERRIDE IS NOT AN ERROR                  
         TM    PROBFLG,PFCMISQ+PFPMISQ+PFEMISQ+PFCINVQ+PFPINVQ+PFEINVQ          
         BNZ   PRINV80                                                          
         OI    PROBFLG,PFOVERQ                                                  
*                                                                               
* SET UP VALID CPE MASK                                                         
*                                                                               
PRINV80  DS    0H                                                               
         LHI   RE,MFVALCQ+MFVALPQ                                               
         TM    FIL2FLAG,FIL2EST    COUNTING ESTIMATES?                          
         BZ    *+8                 YES                                          
         LHI   RE,MFVALCQ+MFVALPQ+MFVALEQ                                       
*                                                                               
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    MISCFLG,0                                                        
         BNO   *+10                                                             
         AP    BTINVCPE,=P'1'                                                   
*                                                                               
         LA    R1,BSENTRY                                                       
         ICM   R1,8,=AL1(BTENTLQ)                                               
         BRAS  RE,TO31             PUT DATA BACK TO BINSRCH TABLE               
         DROP  R4                                                               
*                                                                               
* SEE IF CURRENT INVOICE IS A "PROBLEM" ONE                                     
         OC    PROBFLG,PROBFLG     ANY ERRORS?                                  
         BZ    PRINVQX             NO - JUST EXIT                               
*                                                                               
         TM    FILTFLAG,FILTNOER   SUPPRESS ERROR LISTING?                      
         BO    PRINVQX                                                          
*                                                                               
PRINV100 DS    0H                                                               
         XC    BSENTRY2(B2ENTLQ),BSENTRY2                                       
*                                                                               
         LA    R4,BSENTRY2                                                      
         USING BTAB2D,R4                                                        
*                                                                               
* BUILD BINSRCH TABLE KEY                                                       
*                                                                               
         MVC   B2UID,=X'FFFF'      DEFAULT TO UNKNOWN ID                        
         CLI   CURRAGY,X'FF'                                                    
         BE    *+10                UNKNOWN ID                                   
         MVC   B2UID,CURRBUID      USER ID                                      
         MVC   B2SRCE,CURSRCE                                                   
         MVC   B2STA,PRTSTA7C                                                   
         MVC   B2MOS,WRKEZMOS                    MOS                            
*                                                                               
* 10-CHAR USER ID                                                               
*                                                                               
         MVC   B2USERID,=CL10'UNKNOWN'                                          
         CLI   CURRAGY,X'FF'                                                    
         BE    *+10                UNKNOWN ID                                   
         MVC   B2USERID,LKAGYUID                                                
*                                                                               
* INVOICE NUMBER                                                                
*                                                                               
         LHI   R0,10               COLUMN 9 - INVOICE NUMBER                    
         L     R1,AIO              A(INVOICE HEADER SAVE AREA)                  
         BRAS  RE,FINDFLD                                                       
         BNE   PRINV120            FIELD NOT FOUND - NO INV #                   
         CLI   GETFLEN,X'00'       ANY DATA?                                    
         BE    PRINV120                                                         
         MVC   B2INVNO,GETFDATA                                                 
*                                                                               
PRINV120 DS    0H                                                               
*                                                                               
* PROBLEM FLAG                                                                  
*                                                                               
         MVC   B2ERRTYP,PROBFLG                                                 
*                                                                               
* ADV CODE                                                                      
*                                                                               
         MVC   B2ADVCOD,SPACES                                                  
         LHI   R0,26               COLUMN 25 - AGY ADV CODE                     
         L     R1,AIO              A(INVOICE HEADER SAVE AREA)                  
         BRAS  RE,FINDFLD                                                       
         BNE   PRINV140            FIELD NOT FOUND                              
         CLI   GETFLEN,X'00'       ANY DATA?                                    
         BE    PRINV140                                                         
*                                                                               
         ZIC   RE,GETFLEN                                                       
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   B2ADVCOD(0),GETFDATA                                             
*                                                                               
PRINV140 DS    0H                                                               
*                                                                               
* ADVERTISER NAME                                                               
*                                                                               
         LHI   R0,5                COLUMN 4 - AGY ADV NAME                      
         L     R1,AIO              A(INVOICE HEADER SAVE AREA)                  
         BRAS  RE,FINDFLD                                                       
         BNE   PRINV180            FIELD NOT FOUND                              
         CLI   GETFLEN,X'00'       ANY DATA?                                    
         BE    PRINV180                                                         
         MVC   B2ADVNAM,GETFDATA                                                
*                                                                               
PRINV180 DS    0H                                                               
*                                                                               
* PRODUCT CODE                                                                  
*                                                                               
         MVC   B2PRDCOD,SPACES                                                  
         LHI   R0,28               COLUMN 4 - AGY PRD CODE                      
         L     R1,AIO              A(INVOICE HEADER SAVE AREA)                  
         BRAS  RE,FINDFLD                                                       
         BNE   PRINV200            FIELD NOT FOUND                              
         CLI   GETFLEN,X'00'       ANY DATA?                                    
         BE    PRINV200                                                         
         ZIC   RE,GETFLEN                                                       
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   B2PRDCOD(0),GETFDATA                                             
*                                                                               
PRINV200 DS    0H                                                               
*                                                                               
* PRODUCT NAME                                                                  
*                                                                               
         LHI   R0,6                COLUMN 4 - AGY PRD NAME                      
         L     R1,AIO              A(INVOICE HEADER SAVE AREA)                  
         BRAS  RE,FINDFLD                                                       
         BNE   PRINV220            FIELD NOT FOUND                              
         CLI   GETFLEN,X'00'       ANY DATA?                                    
         BE    PRINV220                                                         
         MVC   B2PRDNAM,GETFDATA                                                
*                                                                               
PRINV220 DS    0H                                                               
*                                                                               
* ESTIMATTE CODE                                                                
*                                                                               
         LHI   R0,9                COLUMN 8 - ESTIMATE CODE                     
         L     R1,AIO              A(INVOICE HEADER SAVE AREA)                  
         BRAS  RE,FINDFLD                                                       
         BNE   PRINV240            FIELD NOT FOUND                              
         CLI   GETFLEN,X'00'       ANY DATA?                                    
         BE    PRINV240                                                         
         MVC   B2EST,GETFDATA                                                   
*                                                                               
PRINV240 DS    0H                                                               
         L     R2,AIO                                                           
         TM    7(R2),EZIHCOVR      OVERRIDES?                                   
         BZ    PRINV260                                                         
*                                                                               
         GOTO1 CLUNPK,DMCB,11(R2),B2ADVOVR                                      
         EDIT  (B1,15(R2)),B2ESTOVR                                             
*                                                                               
         LHI   R0,37               COLUMN 36 - 2ND SAVE FIELD                   
         L     R1,AIO              A(INVOICE HEADER SAVE AREA)                  
         BRAS  RE,FINDFLD                                                       
         BNE   PRINV260            FIELD NOT FOUND                              
         CLI   GETFLEN,X'00'       ANY DATA?                                    
         BE    PRINV260            FIELD NOT FOUND                              
*                                                                               
         MVC   B2PRDOVR,GETFDATA+3                                              
*                                                                               
PRINV260 DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
* ADD IT TO BINSRCH TABLE                                                       
*                                                                               
         MVI   B2P4,X'01'          ADD RECORD IF NOT FOUND                      
         LA    R1,BSENTRY2         RECORD TO BE LOOKED UP/ADDED                 
         STCM  R1,15,B2P1          PASS A(RECORD) IN PARAM 1                    
*                                                                               
         SAM31                                                                  
         GOTO1 VBINSRCH,BSPARS2                                                 
         SAM24                                                                  
*                                                                               
         OC    BP1+1(3),BP1+1      TEST TABLE FULL                              
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL - FOR NOW JUST DUMP               
*                                                                               
*                                                                               
PRINVQX  J     EQXIT                                                            
PRINVNQX J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* FIXES DISPLACEMENTS OF NAME/CODE/OVERRIDE IN COLTAB2                          
* DEPENDING ON THE PROBLEM FLAG                                                 
***********************************************************************         
FIXDISP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* SET UP THE BIT MASK FIRST                                                     
*                                                                               
         LA    R1,BSENTRY2+B2ERRTYP-BTAB2D PROBLEM FLAG                         
         BRAS  RE,FINDMASK                                                      
         ZIC   RE,BYTE                                                          
*                                                                               
         L     R1,ACOLTAB2                                                      
*                                                                               
* HERE LO BYTE OF RE CONTAINS PROBLEM FLAG BIT                                  
* NOW FIND CORRESPONDING ENTRY IN FIXTAB                                        
FIXD20   DS    0H                                                               
         LHI   R0,3                3 ENTRIES IN FIXTAB                          
         LA    R2,FIXTAB                                                        
*                                                                               
FIXD30   EX    RE,*+8                                                           
         B     *+8                                                              
         TM    0(R2),0                                                          
         BO    FIXD40                                                           
         LA    R2,FIXTABLQ(R2)                                                  
         BCT   R0,FIXD30                                                        
         DC    H'0'                                                             
*                                                                               
* HERE R2 POINTS TO FIXTAB ENTRY, R1 ADDRESSES COLTAB2                          
*                                                                               
FIXD40   DS    0H                                                               
         LA    R1,CT2COD-COLTAB2(R1)                                            
         LA    R2,1(R2)            ADVANCE PAST BITMASK IN FIXTAB               
         LHI   R0,3                3 DISPLACEMENTS TO FIX                       
*                                                                               
FIXD50   DS    0H                                                               
         MVC   CTBDISP-COLTABD(4,R1),0(R2)                                      
         SR    RF,RF                                                            
         ICM   RF,3,CTBLN-COLTABD(R1)                                           
         AR    R1,RF                                                            
         LA    R2,4(R2)                                                         
         BCT   R0,FIXD50                                                        
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* R1 EXPECTED TO ADDRESS FLAG BYTE                                              
* ON EXIT BYTE WILL HAVE THE LOWEST BIT VALUE FOR THE FLAG BYTE                 
FINDMASK NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,7                SEVEN PROBLEM FLAGS SO FAR                   
         LHI   RE,1                                                             
*                                                                               
FMAS10   EX    RE,*+8                                                           
         B     *+8                                                              
         TM    0(R1),0                                                          
         BO    FMASX                                                            
         SLL   RE,1                                                             
         BCT   R0,FMAS10                                                        
         DC    H'0'                                                             
*                                                                               
FMASX    STC   RE,BYTE                                                          
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
GETMEM   NTR1  BASE=*,LABEL=*                                                   
         ICM   R0,15,=AL4(BTENTNQ)                                              
         MHI   R0,BTENTLQ                                                       
         ICM   RF,15,=AL4(B2ENTNQ)                                              
         MHI   RF,B2ENTLQ                                                       
         AR    R0,RF                                                            
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         STCM  R1,15,ABSBUFF                                                    
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
* R2 EXPECTED TO ADDRESS THE OPTIONS FIELD                                      
* NUMOPTSQ = NUMBER OF OPTIONS                                                  
VOPT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
NUMOPTSQ EQU   2                                                                
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VOPT10                                                           
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   VOPTNQX             NO - NO INPUT ALLOWED IN THIS FIELD          
*                                                                               
VOPT10   DS    0H                                                               
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         LHI   R0,NUMOPTSQ                                                      
         GOTO1 SCANNER,DMCB,(R2),((R0),(R4)),0                                  
*                                                                               
         CLI   4(R1),0                                                          
         BE    VOPTNQX                                                          
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+4         GET NUMBER OF BLOCKS                         
*                                                                               
VOPT20   ZIC   R1,0(R4)            GET LENGTH                                   
         CHI   R1,2                                                             
         BL    VOPTNQX                                                          
         BCTR  R1,0                                                             
*                                                                               
* AGENCY                                                                        
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'AGENCY'                                              
         BNE   VOPT100                                                          
*                                                                               
         CLI   1(R4),3                                                          
         BNE   VOPT30                                                           
         CLC   =C'ALL',22(R4)                                                   
         BNE   VOPT30                                                           
*                                                                               
         NI    FILTFLAG,X'FF'-FILTAGYQ                                          
         B     VOPT5000                                                         
*                                                                               
VOPT30   DS    0H                                                               
         CLI   1(R4),2                                                          
         BNE   VOPTNQX                                                          
         OI    FILTFLAG,FILTAGYQ                                                
         MVC   FILTAGY,22(R4)                                                   
         B     VOPT5000                                                         
*                                                                               
* INVALID OPTION ENTERED                                                        
*                                                                               
VOPT100  DS    0H                                                               
*                                                                               
* NOERRORS                                                                      
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'NOERRORS'                                            
         BNE   VOPT200                                                          
*                                                                               
         OI    FILTFLAG,FILTNOER                                                
         B     VOPT5000                                                         
*                                                                               
VOPT200  DS    0H                                                               
         B     VOPTNQX                                                          
*                                                                               
* MOVE ON TO THE NEXT OPTION                                                    
*                                                                               
VOPT5000 DS    0H                                                               
         LA    R4,32(R4)                                                        
         BCT   R3,VOPT20                                                        
*                                                                               
VOPTQX   DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
VOPTNQX  DS    0H                                                               
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
* BYTE EXPECTED TO HAVE THE NEW MOS (X'BA' = OCT 2011)                          
* ON EXIT HALF WILL HAVE THE OLD FORMAT MOS (X'1110')                           
* DUB IS USED FOR CONVERSION                                                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
NEW2OLD  NTR1  BASE=*,LABEL=*                                                   
         ZIC   RF,BYTE                                                          
         NILL  GRF,X'000F'  ZERO OUT YEAR (GRF - GRAND REGISTER RF)             
         CVD   RF,DUB                                                           
         SR    RF,RF                                                            
         ICM   RF,3,DUB+6                                                       
         SRL   RF,4                                                             
         STC   RF,HALF+1                                                        
*                                                                               
         ZIC   RF,BYTE                                                          
         SRL   RF,4                GET RID OF THE MONTH                         
         CVD   RF,DUB                                                           
         SR    RF,RF                                                            
         ICM   RF,3,DUB+6                                                       
         SRL   RF,4                                                             
         STC   RF,HALF                                                          
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
ENDSUB   EQU   *                                                                
*                                                                               
         DS    256X                INSTRUCTION-DATA SPACE                       
*                                  ENHANCES PROGRAM EXECUTION SPEED             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* CSECT STORAGE                                                                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
CSTORAGE DS    0D                                                               
ONMASK   DC    X'80000000'                                                      
OFFMASK  DC    X'7FFFFFFF'                                                      
*                                                                               
DOHDHOOK DC    C'Y'                                                             
*                                                                               
ABSBUFF  DS    A                                                                
FIRSTSW  DC    C'Y'                                                             
*                                                                               
FILTRS   DS    0X                                                               
*                                                                               
FILTFLAG DS    X                                                                
*        EQU   X'01'                                                            
FILTAGYQ EQU   X'02'               AGY                                          
FILTUIDQ EQU   X'04'               USER ID                                      
FILTMOSQ EQU   X'08'               MOS                                          
FILTSTAQ EQU   X'10'               STATION                                      
FILTMEDQ EQU   X'20'               MEDIA                                        
FILTDATQ EQU   X'40'               BATCH DATES                                  
FILTNOER EQU   X'80'               NOERRROS                                     
*                                                                               
FIL2FLAG DS    X                                                                
FIL2SRCQ EQU   X'01'               SOURCE: COL32                                
FIL2CDAT EQU   X'02'               CONVERT DATE  QEST+2                         
FIL2STAT EQU   X'04'               STATUS (CONV, DEL, UNCONV, OVER)             
FIL2DNL  EQU   X'08'               DOWNLOAD OPTION                              
FIL2EST  EQU   X'10'               EVALUATE ESTIMATES                           
*                                                                               
FILTAGY  DS    CL2                                                              
FILTUID  DS    XL10                                                             
FILTBUID DS    XL2                                                              
FILTMOS  DS    XL2                                                              
         DS    X                   SPACE FOR PACKED DAY (IGNORED)               
FILTSTA  DS    CL4                                                              
FILTMED  DS    C                                                                
FILTSD   DS    XL2                                                              
FILTED   DS    XL2                                                              
FILTSRCE DS    CL4                                                              
FILTSRCL DS    X                                                                
FILTCDAT DS    XL3                                                              
FILTSTAT DS    X                                                                
*                                                                               
FILTRLQ  EQU   *-FILTRS                                                         
*                                                                               
* AGENCY LOOKUP  BLOCK                                                          
LKAGYBLK DS    0CL14                                                            
LKAGYBID DS    CL2                 BINARY USER ID                               
LKAGYAGY DS    CL2                 AGY ALPHA                                    
LKAGYUID DS    CL10                ALPHA USER ID                                
*                                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    C' '        FIELD DELIMITER                                      
EOTCHR   DC    C'"'        END OF TEXT FIELD DELIMITER                          
EOTALT   DC    C''''       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
FIXTAB   DC    AL1(PFCMISQ+PFCINVQ+PFOVERQ)                                     
         DC    AL4(B2ADVCOD-BTAB2D)                                             
         DC    AL4(B2ADVNAM-BTAB2D)                                             
         DC    AL4(B2ADVOVR-BTAB2D)                                             
FIXTABLQ EQU   *-FIXTAB                                                         
*                                                                               
         DC    AL1(PFPMISQ+PFPINVQ)                                             
         DC    AL4(B2PRDCOD-BTAB2D)                                             
         DC    AL4(B2PRDNAM-BTAB2D)                                             
         DC    AL4(B2PRDOVR-BTAB2D)                                             
*                                                                               
         DC    AL1(PFEMISQ+PFEINVQ)                                             
         DC    AL4(B2EST-BTAB2D)                                                
         DC    AL4(0)                                                           
         DC    AL4(B2ESTOVR-BTAB2D)                                             
         DC    X'FF'                                                            
*                                                                               
COLTAB   DS    0H                                                               
*                                                                               
* NOTE *                                                                        
* H1 LINE IS ALWAYS BLANK, FOR MULTI-COLUMN HEADERS                             
*                                                                               
CTUIDLQ  EQU   10                                                               
CTUID    DC    AL2(CTUIDX-CTUID)                                                
         DC    AL1(1)                                                           
         DC    AL1(CTUIDLQ)                                                     
         DC    AL4(PRTXT)                                                       
         DC    AL1(1)                                                           
         DC    AL4(BTUSERID-BTABD)                                              
         DC    CL(CTUIDLQ)''                                                    
         DC    CL(CTUIDLQ)'USER ID'                                             
         DC    CL(CTUIDLQ)''                                                    
CTUIDX   EQU   *                                                                
*                                                                               
CTSTALQ  EQU   7                                                                
CTSTA    DC    AL2(CTSTAX-CTSTA)                                                
         DC    AL1(1)                                                           
         DC    AL1(CTSTALQ)                                                     
         DC    AL4(PRTXT)                                                       
         DC    AL1(1)                                                           
         DC    AL4(BTSTA-BTABD)                                                 
         DC    CL(CTSTALQ)''                                                    
         DC    CL(CTSTALQ)'STA'                                                 
         DC    CL(CTSTALQ)''                                                    
CTSTAX   EQU   *                                                                
*                                                                               
CTMOSLQ  EQU   6                                                                
CTMOS    DC    AL2(CTMOSX-CTMOS)                                                
         DC    AL1(1)                                                           
         DC    AL1(CTMOSLQ)                                                     
         DC    AL4(PRMOS)                                                       
         DC    AL1(1)                                                           
         DC    AL4(BTMOS-BTABD)                                                 
         DC    CL(CTMOSLQ)''                                                    
         DC    CL(CTMOSLQ)'MOS'                                                 
         DC    CL(CTMOSLQ)''                                                    
CTMOSX   EQU   *                                                                
*                                                                               
CTSRCLQ  EQU   4                                                                
CTSRC    DC    AL2(CTSRCX-CTSRC)                                                
         DC    AL1(1)                                                           
         DC    AL1(CTSRCLQ)                                                     
         DC    AL4(PRTXT)                                                       
         DC    AL1(1)                                                           
         DC    AL4(BTSRCE-BTABD)                                                
         DC    CL(CTSRCLQ)''                                                    
         DC    CL(CTSRCLQ)'SRC'                                                 
         DC    CL(CTSRCLQ)''                                                    
CTSRCX   EQU   *                                                                
*                                                                               
CTITOTLQ EQU   3                                                                
CTITOT   DC    AL2(CTITOTX-CTITOT)                                              
         DC    AL1(1)                                                           
         DC    AL1(CTITOTLQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(1)                                                           
         DC    AL4(BTINVCNT-BTABD)                                              
         DC    CL(CTITOTLQ)''                                                   
         DC    CL(CTITOTLQ)'TOT'                                                
         DC    CL(CTITOTLQ)''                                                   
CTITOTX  EQU   *                                                                
*                                                                               
CTICPELQ EQU   3                                                                
CTICPE   DC    AL2(CTICPEX-CTICPE)                                              
         DC    AL1(1)                                                           
         DC    AL1(CTICPELQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(2)                                                           
         DC    AL4(BTINVCPE-BTABD)                                              
         DC    CL(CTICPELQ)''                                                   
         DC    CL(CTICPELQ)'CP '                                                
         DC    CL(CTICPELQ)''                                                   
CTICPEX  EQU   *                                                                
*                                                                               
CTICCVLQ EQU   3                                                                
CTICCV   DC    AL2(CTICCVX-CTICCV)                                              
         DC    AL1(1)                                                           
         DC    AL1(CTICCVLQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(1)                                                           
         DC    AL4(BTINVCCV-BTABD)                                              
         DC    (CTICCVLQ)X'01'                                                  
         DC    CL(CTICCVLQ)'  V'                                                
         DC    CL(CTICCVLQ)''                                                   
CTICCVX  EQU   *                                                                
*                                                                               
CTICCILQ EQU   3                                                                
CTICCI   DC    AL2(CTICCIX-CTICCI)                                              
         DC    AL1(1)                                                           
         DC    AL1(CTICCILQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(1)                                                           
         DC    AL4(BTINVCCI-BTABD)                                              
         DC    CL(CTICCILQ)''                                                   
         DC    CL(CTICCILQ)'  I'                                                
         DC    CL(CTICCILQ)''                                                   
CTICCIX  EQU   *                                                                
*                                                                               
CTICCMLQ EQU   3                                                                
CTICCM   DC    AL2(CTICCMX-CTICCM)                                              
         DC    AL1(1)                                                           
         DC    AL1(CTICCMLQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(2)                                                           
         DC    AL4(BTINVCCM-BTABD)                                              
         DC    CL(CTICCMLQ)''                                                   
         DC    CL(CTICCMLQ)'  M'                                                
         DC    CL(CTICCMLQ)''                                                   
CTICCMX  EQU   *                                                                
*                                                                               
CTIPCVLQ EQU   3                                                                
CTIPCV   DC    AL2(CTIPCVX-CTIPCV)                                              
         DC    AL1(1)                                                           
         DC    AL1(CTIPCVLQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(1)                                                           
         DC    AL4(BTINVPCV-BTABD)                                              
         DC    (CTIPCVLQ)X'02'                                                  
         DC    CL(CTIPCVLQ)'  V'                                                
         DC    CL(CTIPCVLQ)''                                                   
CTIPCVX  EQU   *                                                                
*                                                                               
CTIPCILQ EQU   3                                                                
CTIPCI   DC    AL2(CTIPCIX-CTIPCI)                                              
         DC    AL1(1)                                                           
         DC    AL1(CTIPCILQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(1)                                                           
         DC    AL4(BTINVPCI-BTABD)                                              
         DC    CL(CTIPCILQ)''                                                   
         DC    CL(CTIPCILQ)'  I'                                                
         DC    CL(CTIPCILQ)''                                                   
CTIPCIX  EQU   *                                                                
*                                                                               
CTIPCMLQ EQU   3                                                                
CTIPCM   DC    AL2(CTIPCMX-CTIPCM)                                              
         DC    AL1(1)                                                           
         DC    AL1(CTIPCMLQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(2)                                                           
         DC    AL4(BTINVPCM-BTABD)                                              
         DC    CL(CTIPCMLQ)''                                                   
         DC    CL(CTIPCMLQ)'  M'                                                
         DC    CL(CTIPCMLQ)''                                                   
CTIPCMX  EQU   *                                                                
*                                                                               
CTIECVLQ EQU   3                                                                
CTIECV   DC    AL2(CTIECVX-CTIECV)                                              
         DC    AL1(0)                                                           
         DC    AL1(CTIECVLQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(1)                                                           
         DC    AL4(BTINVECV-BTABD)                                              
         DC    (CTIECVLQ)X'03'                                                  
         DC    CL(CTIECVLQ)'  V'                                                
         DC    CL(CTIECVLQ)''                                                   
CTIECVX  EQU   *                                                                
*                                                                               
CTIECILQ EQU   3                                                                
CTIECI   DC    AL2(CTIECIX-CTIECI)                                              
         DC    AL1(0)                                                           
         DC    AL1(CTIECILQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(1)                                                           
         DC    AL4(BTINVECI-BTABD)                                              
         DC    CL(CTIECILQ)''                                                   
         DC    CL(CTIECILQ)'  I'                                                
         DC    CL(CTIECILQ)''                                                   
CTIECIX  EQU   *                                                                
*                                                                               
CTIECMLQ EQU   3                                                                
CTIECM   DC    AL2(CTIECMX-CTIECM)                                              
         DC    AL1(0)                                                           
         DC    AL1(CTIECMLQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(2)                                                           
         DC    AL4(BTINVECM-BTABD)                                              
         DC    CL(CTIECMLQ)''                                                   
         DC    CL(CTIECMLQ)'  M'                                                
         DC    CL(CTIECMLQ)''                                                   
CTIECMX  EQU   *                                                                
*                                                                               
CTIOVRLQ EQU   3                                                                
CTIOVR   DC    AL2(CTIOVRX-CTIOVR)                                              
         DC    AL1(1)                                                           
         DC    AL1(CTIOVRLQ)                                                    
         DC    AL4(PRPACK)                                                      
         DC    AL1(2)                                                           
         DC    AL4(BTINVOVR-BTABD)                                              
         DC    CL(CTIOVRLQ)''                                                   
         DC    CL(CTIOVRLQ)'OVR'                                                
         DC    CL(CTIOVRLQ)''                                                   
CTIOVRX  EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
COMTAB   DS    0X                                                               
CMCLT    DC    AL1(1)              NUMBER                                       
         DC    AL1(CMCLTLQ)        ENTRY LENGTH                                 
         DC    C'  CLT CODES'      TEXT                                         
CMCLTLQ  EQU   *-CMCLT                                                          
*                                                                               
CMPRD    DC    AL1(2)                                                           
         DC    AL1(CMPRDLQ)                                                     
         DC    C'  PRD CODES'                                                   
CMPRDLQ  EQU   *-CMPRD                                                          
*                                                                               
CMEST    DC    AL1(3)                                                           
         DC    AL1(CMESTLQ)                                                     
         DC    C'  EST CODES'                                                   
CMESTLQ  EQU   *-CMEST                                                          
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
COLTAB2  DS    0H                                                               
*                                                                               
* NOTE *                                                                        
* H1 LINE IS ALWAYS BLANK, FOR MULTI-COLUMN HEADERS                             
*                                                                               
CT2UIDLQ EQU   10                                                               
CT2UID   DC    AL2(CT2UIDX-CT2UID)                                              
         DC    AL1(1)                                                           
         DC    AL1(CT2UIDLQ)                                                    
         DC    AL4(PRTXT)                                                       
         DC    AL1(1)                                                           
         DC    AL4(B2USERID-BTAB2D)                                             
         DC    CL(CT2UIDLQ)''                                                   
         DC    CL(CT2UIDLQ)'USER ID'                                            
         DC    CL(CT2UIDLQ)''                                                   
CT2UIDX  EQU   *                                                                
*                                                                               
CT2STALQ EQU   7                                                                
CT2STA   DC    AL2(CT2STAX-CT2STA)                                              
         DC    AL1(1)                                                           
         DC    AL1(CT2STALQ)                                                    
         DC    AL4(PRTXT)                                                       
         DC    AL1(1)                                                           
         DC    AL4(B2STA-BTAB2D)                                                
         DC    CL(CT2STALQ)''                                                   
         DC    CL(CT2STALQ)'STA'                                                
         DC    CL(CT2STALQ)''                                                   
CT2STAX  EQU   *                                                                
*                                                                               
CT2SRCLQ EQU   4                                                                
CT2SRC   DC    AL2(CT2SRCX-CT2SRC)                                              
         DC    AL1(1)                                                           
         DC    AL1(CT2SRCLQ)                                                    
         DC    AL4(PRTXT)                                                       
         DC    AL1(1)                                                           
         DC    AL4(B2SRCE-BTAB2D)                                               
         DC    CL(CT2SRCLQ)''                                                   
         DC    CL(CT2SRCLQ)'SRC'                                                
         DC    CL(CT2SRCLQ)''                                                   
CT2SRCX  EQU   *                                                                
*                                                                               
CT2MOSLQ EQU   6                                                                
CT2MOS   DC    AL2(CT2MOSX-CT2MOS)                                              
         DC    AL1(1)                                                           
         DC    AL1(CT2MOSLQ)                                                    
         DC    AL4(PRMOS)                                                       
         DC    AL1(2)                                                           
         DC    AL4(B2MOS-BTAB2D)                                                
         DC    CL(CT2MOSLQ)''                                                   
         DC    CL(CT2MOSLQ)'MOS'                                                
         DC    CL(CT2MOSLQ)''                                                   
CT2MOSX  EQU   *                                                                
*                                                                               
CT2INVLQ EQU   10                                                               
CT2INV   DC    AL2(CT2INVX-CT2INV)                                              
         DC    AL1(1)                                                           
         DC    AL1(CT2INVLQ)                                                    
         DC    AL4(PRTXT)                                                       
         DC    AL1(1)                                                           
         DC    AL4(B2INVNO-BTAB2D)                                              
         DC    CL(CT2INVLQ)''                                                   
         DC    CL(CT2INVLQ)'INVOICE'                                            
         DC    CL(CT2INVLQ)''                                                   
CT2INVX  EQU   *                                                                
*                                                                               
CT2PRBLQ EQU   12                                                               
CT2PRB   DC    AL2(CT2PRBX-CT2PRB)                                              
         DC    AL1(1)                                                           
         DC    AL1(CT2PRBLQ)                                                    
         DC    AL4(PRERR)                                                       
         DC    AL1(1)                                                           
         DC    AL4(B2ERRTYP-BTAB2D)                                             
         DC    CL(CT2PRBLQ)''                                                   
         DC    CL(CT2PRBLQ)'PROBLEM'                                            
         DC    CL(CT2PRBLQ)''                                                   
CT2PRBX  EQU   *                                                                
*                                                                               
CT2CODLQ EQU   10                                                               
CT2COD   DC    AL2(CT2CODX-CT2COD)                                              
         DC    AL1(1)                                                           
         DC    AL1(CT2CODLQ)                                                    
         DC    AL4(PRTXT)                                                       
         DC    AL1(1)                                                           
         DC    AL4(0)              1=CODE                                       
         DC    CL(CT2CODLQ)''                                                   
         DC    CL(CT2CODLQ)'CODE'                                               
         DC    CL(CT2CODLQ)''                                                   
CT2CODX  EQU   *                                                                
*                                                                               
CT2NAMLQ EQU   25                                                               
CT2NAM   DC    AL2(CT2NAMX-CT2NAM)                                              
         DC    AL1(1)                                                           
         DC    AL1(CT2NAMLQ)                                                    
         DC    AL4(PRTXT)                                                       
         DC    AL1(1)                                                           
         DC    AL4(0)              2=NAME                                       
         DC    CL(CT2NAMLQ)''                                                   
         DC    CL(CT2NAMLQ)'NAME'                                               
         DC    CL(CT2NAMLQ)''                                                   
CT2NAMX  EQU   *                                                                
*                                                                               
CT2OVRLQ EQU   3                                                                
CT2OVR   DC    AL2(CT2OVRX-CT2OVR)                                              
         DC    AL1(1)                                                           
         DC    AL1(CT2OVRLQ)                                                    
         DC    AL4(PRTXT)                                                       
         DC    AL1(1)                                                           
         DC    AL4(0)              3=OVR                                        
         DC    CL(CT2OVRLQ)''                                                   
         DC    CL(CT2OVRLQ)'OVR'                                                
         DC    CL(CT2OVRLQ)''                                                   
CT2OVRX  EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
* EXTRA 6 BLANK COLUMNS                                                         
         DC    AL2(16),AL1(1),AL1(1),AL4(PRTBLANK),AL1(1),AL4(0),C'   '         
         DC    AL2(16),AL1(1),AL1(1),AL4(PRTBLANK),AL1(1),AL4(0),C'   '         
         DC    AL2(16),AL1(1),AL1(1),AL4(PRTBLANK),AL1(1),AL4(0),C'   '         
         DC    AL2(16),AL1(1),AL1(1),AL4(PRTBLANK),AL1(1),AL4(0),C'   '         
         DC    AL2(16),AL1(1),AL1(1),AL4(PRTBLANK),AL1(1),AL4(0),C'   '         
         DC    AL2(16),AL1(1),AL1(1),AL4(PRTBLANK),AL1(1),AL4(0),C'   '         
*                                                                               
         DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
SRCETAB  DC    CL4'APNY'                                                        
         DC    CL4'BBDO'                                                        
         DC    CL4'BVS '                                                        
         DC    CL4'CAT '                                                        
         DC    CL4'CCOL'                                                        
         DC    CL4'CFM '                                                        
         DC    CL4'CMR '                                                        
         DC    CL4'COKA'                                                        
         DC    CL4'COKE'                                                        
         DC    CL4'COKM'                                                        
         DC    CL4'COMV'                                                        
         DC    CL4'CORE'                                                        
         DC    CL4'COZE'                                                        
         DC    CL4'DTV '                                                        
         DC    CL4'GNC '                                                        
*&&DO                                                                           
         DC    CL4'IPS '                                                        
         DC    CL4'IPSD'                                                        
         DC    CL4'IPSG'                                                        
         DC    CL4'IPSM'                                                        
         DC    CL4'IPSO'                                                        
         DC    CL4'IPSP'                                                        
         DC    CL4'IPSR'                                                        
         DC    CL4'IPSS'                                                        
         DC    CL4'IPSW'                                                        
         DC    CL4'IPSY'                                                        
*&&                                                                             
         DC    CL4'IPBS'                                                        
         DC    CL4'IPCS'                                                        
         DC    CL4'IPDS'                                                        
         DC    CL4'IPGS'                                                        
         DC    CL4'IPMS'                                                        
         DC    CL4'IPOS'                                                        
         DC    CL4'IPYS'                                                        
         DC    CL4'IPZS'                                                        
         DC    CL4'IRNY'                                                        
         DC    CL4'KATZ'                                                        
         DC    CL4'LIPA'                                                        
         DC    CL4'MATT'                                                        
         DC    CL4'MHEE'                                                        
         DC    CL4'MHER'                                                        
         DC    CL4'MLO '                                                        
         DC    CL4'OMD '                                                        
         DC    CL4'PGEO'                                                        
         DC    CL4'RAPP'                                                        
         DC    CL4'RMR '                                                        
         DC    CL4'SDI '                                                        
         DC    CL4'SDIC'                                                        
         DC    CL4'SDIJ'                                                        
         DC    CL4'SDIL'                                                        
         DC    CL4'SDIM'                                                        
         DC    CL4'SDIO'                                                        
         DC    CL4'SDIT'                                                        
         DC    CL4'SDIV'                                                        
         DC    CL4'SDIZ'                                                        
         DC    CL4'SHAI'                                                        
         DC    CL4'SMV '                                                        
         DC    CL4'STRA'                                                        
         DC    CL4'WAP '                                                        
         DC    CL4'WIM '                                                        
         DC    CL4'WPRI'                                                        
         DC    CL4'WWNY'                                                        
*        DC    CL4'USA '                                                        
SRCECT   EQU   (*-SRCETAB)/4                                                    
*                                                                               
*                                                                               
TRACEIND NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,WRKEZKEY                                                      
         GOTO1 HEXOUT,DMCB,(R2),P,12,=C'N'                                      
         MVC   P+40(30),0(R2)                                                   
         MVI   DOHDHOOK,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* SHARED EASI WORKING STORAGE                                                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
       ++INCLUDE SPEZFWORKD                                                     
       ++INCLUDE SPEZFSYSD                                                      
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* SPEZF20 WORKING STORAGE                                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
ZF20WKD  DSECT                                                                  
*                                                                               
BTABADDR DS    A                                                                
ACURCTAB DS    A                   A(CURRENT COLTAB)                            
ACOLTAB  DS    A                                                                
ACOLTAB2 DS    A                                                                
ASRCETAB DS    A                                                                
ABSENTRY DS    A                                                                
ACOMTAB  DS    A                                                                
AWKBUFF  DS    A                                                                
VBINSRCH DS    A                                                                
VWRKIO   DS    A                                                                
*                                                                               
WRKFILE  DS    CL8                                                              
LOADDATE DS    CL6                                                              
CONVDATE DS    CL6                                                              
WRKRINDX DS    CL42                                                             
CURSRCE  DS    CL4                                                              
CURMED   DS    C                                                                
ZAPFLAG  DS    C                                                                
PACKED16 DS    PL16                                                             
XKEY     DS    XL64                                                             
XKEYSAVE DS    XL64                                                             
SAVSYS   DS    X                                                                
CURRBUID DS    XL2                 BINARY UID FOR CURRENT BATCH                 
CURRUID  DS    CL10                UID FOR CURRENT BATCH                        
CURRAGY  DS    CL2                 AGY FOR CURRENT BATCH                        
*                                                                               
MISCFLG  DS    X                                                                
MFVALCQ  EQU   X'01'               INVOCIE HAS VALID CLIENT CODE                
MFVALPQ  EQU   X'02'               INVOCIE HAS VALID PRODUCT CODE               
MFVALEQ  EQU   X'04'               INVOCIE HAS VALID ESTIMATE CODE              
*                                                                               
PROBFLG  DS    X                                                                
PFCMISQ  EQU   X'01'               CLIENT CODE MISSING                          
PFCINVQ  EQU   X'02'               CLIENT CODE INVALID                          
PFPMISQ  EQU   X'04'               PRODUCT CODE MISSING                         
PFPINVQ  EQU   X'08'               PRODUCT CODE INVALID                         
PFEMISQ  EQU   X'10'               ESTIMATE CODE MISSING                        
PFEINVQ  EQU   X'20'               ESTIMATE CODE INVALID                        
PFOVERQ  EQU   X'40'               CPE OVERRIDE ENTERED                         
*                                                                               
* PARAMETER BLOCK                                                               
*                                                                               
PBLOCK   DS    0F                                                               
PBAOUT   DS    F                   A(OUTPUT) FOR DOCOLTAB ROUTINES              
PBHLINE  DS    F                   NUMBER OF HEADER LINE NOW PROCESSING         
PBPROB   DS    X                   PROBLEM BYTE                                 
PBPRFLAG DS    X                   PROCESS FLAG                                 
         DS    2X                                                               
*                                                                               
* GETFIELD  BLOCK                                                               
*                                                                               
GETFBLK  DS    0X                  1-LENGTH, 30 - DATA                          
GETFLEN  DS    X                                                                
GETFDATA DS    CL30                                                             
GETFBKLQ EQU   *-GETFBLK                                                        
*                                                                               
*                                                                               
         DS    0F                                                               
BSPARS   DS    0XL32               BINSEARCH PARAMETERS                         
BP1      DS    F                                                                
BP2      DS    F                                                                
BP3      DS    F                                                                
BP4      DS    F                                                                
BP5      DS    F                                                                
BP6      DS    F                                                                
BP7      DS    F                                                                
BP8      DS    F                                                                
*                                                                               
         DS    0F                                                               
BSPARS2  DS    0XL32               BINSEARCH 2 PARAMETERS                       
B2P1     DS    F                                                                
B2P2     DS    F                                                                
B2P3     DS    F                                                                
B2P4     DS    F                                                                
B2P5     DS    F                                                                
B2P6     DS    F                                                                
B2P7     DS    F                                                                
B2P8     DS    F                                                                
*                                                                               
* BINSRCH TABLE ENTRY BUILT AND PROCESSED HERE                                  
*                                                                               
*                                                                               
BSENTRY  DS    0X                  BINSRCH TABLE ENTRY BUILT HERE               
         ORG   BSENTRY+BTENTLQ                                                  
*                                                                               
BSENTRY2 DS    0X                  BINSRCH2 TABLE ENTRY BUILT HERE              
         ORG   BSENTRY2+B2ENTLQ                                                 
*                                                                               
*                                                                               
* DLFLD PARAMETER BLOCK                                                         
*                                                                               
DLCB     DS    XL256                                                            
DNLINE   DS    CL132                                                            
DNLINE2  DS    CL132                                                            
DNLINE3  DS    CL132                                                            
DNFIRST  DS    X                                                                
*                                                                               
       ++INCLUDE DDWRKIOD                                                       
*                                                                               
* USER ID TABLE FOR FASTER LOOKUPS                                              
*                                                                               
IDTAB    DS    (500*L'LKAGYBLK)X   ID TABLE                                     
IDTABL   EQU   *-IDTAB             2-BYTE ID, 2-CHAR AGY, 10-CHAR ID            
*                                                                               
ZF20WKDL EQU   *-ZF20WKD                                                        
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* DSECTS                                                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
COLTABD  DSECT                                                                  
*                                                                               
CTBLN    DS    AL2                 TABLE ENTRY LENGTH                           
CTBACTIV DS    X                   1=ACTIVE, 0=INACTIVE                         
CTBWIDTH DS    X                   COLUMN WIDTH                                 
CTBOUTR  DS    AL4                 OUTPUT ROUTINE                               
CTBSPACE DS    X                   SPACES AFTER COLUMN                          
CTBDISP  DS    AL4                 COUNTER'S DISPLACEMENT INTO BTABD            
CTBHD1   DS    0C                  START OF HEADER 1                            
CTBHDNQ  EQU   3                   NUMBER OF HEADER LINES                       
*                                                                               
* BINSRCH TABLE COUNTERS DSECT                                                  
*                                                                               
BTABD    DSECT                                                                  
BTENTNQ  EQU   100000              NUMBER OF TABLE ENTRIES                      
*                                                                               
* BINSEARCH KEY                                                                 
BTKEY    DS    0X                                                               
*                                                                               
BTUID    DS    XL2                 USER ID                                      
BTSTA    DS    CL7                 STATION (PRINTABLE)                          
BTSRCE   DS    XL4                 SOURCE                                       
BTMOS    DS    XL4                 MOS                                          
BTKEYLQ  EQU   *-BTKEY                                                          
*                                                                               
* BINSEARCH DATA                                                                
*                                                                               
BTUSERID DS    CL10                USER ID                                      
BTSEQ    DS    XL4                 BATCH SEQ NO                                 
*                                                                               
BTTOTST  DS    0PL8                                                             
*                                                                               
BTINVCNT DS    PL8                 INVOICE COUNT - TOTAL INVOICES               
*                                                                               
BTINVCCV DS    PL8                 INVOICES WITH CLIENT CODE VALID              
BTINVCCI DS    PL8                 INVOICES WITH CLIENT CODE INVALID            
BTINVCCM DS    PL8                 INVOICES WITH CLIENT CODE MISSING            
*                                                                               
BTINVPCV DS    PL8                 =             PRODUCT        =               
BTINVPCI DS    PL8                 =             PRODUCT        =               
BTINVPCM DS    PL8                 =             PRODUCT        =               
*                                                                               
BTINVECV DS    PL8                 =             ESTIMATE       =               
BTINVECI DS    PL8                 =             ESTIMATE       =               
BTINVECM DS    PL8                 =             ESTIMATE       =               
*                                                                               
BTINVOVR DS    PL8                 INVOICES WITH C/P/E OVERRIDES                
BTINVCPE DS    PL8                 INVOICES WITH VALID CPE CODES                
*                                                                               
BTNTOTQ  EQU   (*-BTTOTST)/8       TOTAL NUMBER OF COUNTERS                     
*                                                                               
BTENTLQ  EQU   *-BTKEY             TABLE ENTRY LENGTH                           
*                                                                               
* BINSRCH TABLE PROBLEM INVOICES DSECT                                          
BTAB2D   DSECT                                                                  
B2ENTNQ  EQU   100000              NUMBER OF TABLE ENTRIES                      
*                                                                               
* BINSEARCH KEY                                                                 
B2KEY    DS    0X                                                               
*                                                                               
B2UID    DS    XL2                 USER ID                                      
B2STA    DS    CL7                 STATION (PRINTABLE)                          
B2SRCE   DS    XL4                 SOURCE                                       
B2MOS    DS    XL4                 MOS                                          
B2INVNO  DS    CL10                INVOICE NUMBER                               
B2KEYLQ  EQU   *-B2KEY                                                          
*                                                                               
* BINSEARCH DATA                                                                
B2USERID DS    CL10                USER ID                                      
*                                                                               
B2ERRTYP DS    X                   ERROR TYPE - SAME AS PROBFLG                 
*                                                                               
B2ADVCOD DS    CL10                ADV CODE                                     
B2ADVOVR DS    CL3                 ADV CODE OVERRIDE                            
B2ADVNAM DS    CL25                ADV NAME                                     
B2PRDCOD DS    CL10                PRD CODE                                     
B2PRDOVR DS    CL6                 PRD CODE OVERRIDE                            
B2PRDNAM DS    CL25                PRD NAME                                     
B2EST    DS    CL10                EST                                          
B2ESTOVR DS    CL3                 EST OVERRIDE                                 
*                                                                               
B2ENTLQ  EQU   *-B2KEY             TABLE ENTRY LENGTH                           
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* SCREEN                                                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
       ++INCLUDE SPEZFFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPEZFA2D                                                       
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* INCLUDES                                                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DMWRKFD                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPSNVRCRD                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPGENEZ                                                        
       ++INCLUDE EZBLOCK                                                        
       ++INCLUDE SPEZDSCTS                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPEZF20   02/19/16'                                      
         END                                                                    
