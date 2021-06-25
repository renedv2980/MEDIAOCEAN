*          DATA SET SPWRI20    AT LEVEL 019 AS OF 10/24/19                      
*PHASE T20420A,*                                                                
*********************************************************************           
*                                                                   *           
*          SPWRI20 (T20420) - ESTIMATE CLOSEOUTS                    *           
*                                                                   *           
*-------------------------------------------------------------------*           
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-35425  05/03/19 ADD DARE MAKEGOOD REJECTED COMMENTS RECS  *         
* AKAT ITMF-16223  05/15/17 INCREASE MONMAX                           *         
***********************************************************************         
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 19OCT15 17 AKT -- CLOSEOUT THE FOLLOWING RECORDS                  *           
*                -- XSPFIL X'0E10' DESKTOP REVISION                 *           
*                -- XSPFIL X'0E11' DESKTOP AVAIL                    *           
*                -- XSPFIL X'0E12' DESKTOP PROPOSAL                 *           
*                -- XSPFIL X'0D36' DARE CABLE MAKEGOOD NOTICE       *           
*                -- XSPFIL X'0D37' DARE CABLE MAKEGOOD OFFER        *           
*                -- XSPFIL X'0D3B' DARE MAKEGOOD REJECTED COMMENTS  *           
* 14DEC12 16 AKT -- MOVE ACTIVE DARE KEY AFTER GETREC!              *           
* 23JUL12 15 AKT -- DELETE THE ACTIVE (NOT PASSIVE) DARE KEY        *           
* 14MAY12 14 EFJ -- DON'T ABEND ON BAD PRD IN XCOM - PURGE IT       *           
* 04MAY11 13 AKT -- PASS IN SBSPPROF TO MOBILE INSTEAD OF GARBAGE!  *           
* 15MAY11 12 AKT -- RESET SBMODE AFTER SPOTIO CALL                  *           
* 02MAY11 11 AKT -- SET NETWORK WHEN PROCESSING ERROR REPORTS       *           
* 04NOV10 10 EFJ -- SUPPORT SKIPONLY OPTION                         *           
* 06OCT10 09 AKT -- CLOSEOUT DARE BATCH & FLIGHT RECORDS            *           
* 10AUG10 08 EFJ -- REMOVE TOTALSD AND CHANGE TOTALSL TO DELCTRQ    *           
*                -- DISALLOW SINGLE PRD AT REQUEST TIME             *           
* 24MAY10 07 AKT -- CLOSEOUT THE RIGHT ESTIMATE IN THE NDEF ROUTINE *           
*                -- CLOSEOUT '0D6F' MASTER/SUBEST LIST IN CHILD SPOT*           
*                -- CLOSEOUT THE SPLIT RECORDS IN SPOT/SFM          *           
* 15OCT09 06 AKT -- 2-BYTE BUYLINE SUPPORT                          *           
* 27APR07 05 AKT -- SAVE AND RESTORE SBQMED ON ERROR OVERFLOW       *           
* 09JUL07 04 EFJ -- SAVE QMED AND USE FOR ADDING I8/TR REQS (SO     *           
*                   IT WILL BE RIGHT FOR CANADA)                    *           
* 31JAN07 03 EFJ -- GENERATE I8 INVOICE CLOSEOUT REQUESTS           *           
*                -- GENERATE TRPURGE TRAFFIC CLOSEOUT REQUESTS      *           
*                -- ADD RECORD COUNTS BY AGENCY                     *           
*                -- ADD AGENCY ERROR SUMMARY REPORT                 *           
*                -- MOVE SOME W/S AROUND AND REMOVE =A() REFS       *           
*                -- DEAL W/SPOTS OUTSIDE BUY PERIOD BEFORE BDSTART! *           
*                -- MAKE SURE RUN ENDS ON BILLED/PAID THIS MONTH    *           
* 23FEB05 01 EFJ -- LEVEL REALLY RESET THIS TIME                    *           
*                -- REQUIRE END DATE IF NOT ONE ESTIMATE            *           
* 19SEP05 01 EFJ -- LEVEL RESET.  OLD HISTORY MOVED TO BOTTOM       *           
*                -- CONFIRM CLOSEOUTS LESS THAN 2 YEARS OLD         *           
*                -- MOVE CODE AND GLOBALS                           *           
*********************************************************************           
         TITLE 'T20420 - ESTIMATE CLOSEOUTS'                                    
T20420   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,T20420,RA,RR=R2,CLEAR=Y                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         USING GETTXTD,GETTXTCB                                                 
*                                                                               
         BASR  R8,0                                                             
         AHI   R8,GLOBALS-*                                                     
         USING GLOBALS,R8          RA=A(GLOBAL LITERAL POOL)                    
*                                                                               
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   ASAVE,TSPFUSER                                                   
         MVC   VDYNALLO,TDYNALLO                                                
         DROP  R1                                                               
*                                                                               
         CLI   MODE,RUNFRST        RUNFIRST                                     
         BE    FRST                                                             
*                                                                               
         CLI   MODE,VALREC         VALIDATE REQUEST                             
         BE    VREC                                                             
*                                                                               
         CLI   MODE,PRINTREP       PRINT THE REPORT                             
         BE    PREP                                                             
*                                                                               
         CLI   MODE,RUNLAST        RUNLAST                                      
         BE    LAST                                                             
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* RUNFIRST                                                            *         
***********************************************************************         
         SPACE 1                                                                
FRST     CLI   OFFLINE,C'Y'        OFFLINE ONLY                                 
         BNE   FRSTX                                                            
         MVI   TWAFIRST,2          GET RUNLAST MODE ALSO                        
*                                                                               
         L     R3,ASAVE            OPEN TEMP FILE                               
         AHI   R3,SAVVALSL         SAVE DCB IN SPFUSER AFTER SAVVALS            
         MVC   0(ECTEMPL,R3),ECTEMPLB                                           
         AHI   R3,L'ECTEMPLB                                                    
         ST    R3,AECTEMP                                                       
*                                                                               
         GOTO1 VDYNALLO,DMCB,(X'80',DDTEMP),(X'80',TMPALLOC)                    
         OPEN  ((R3),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   ACTSW,C'N'          INIT ACTIVITY SWITCH                         
*                                                                               
         AHI   R3,ECTEMPL                                                       
         MVC   0(I8RQFILL,R3),I8RQLBL                                           
         AHI   R3,L'I8RQLBL                                                     
         ST    R3,AI8RQDCB                                                      
         OPEN  ((R3),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AHI   R3,I8RQFILL                                                      
         MVC   0(TRRQFILL,R3),TRRQLBL                                           
         AHI   R3,L'TRRQLBL                                                     
         ST    R3,ATRRQDCB                                                      
         OPEN  ((R3),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,=F'250000'       GET STORAGE FOR ERROR TABLE                  
         ST    R5,DMCB+4                                                        
         ST    R5,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   R6,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,AERRTAB                                                       
         LA    R6,0(R5,R6)                                                      
         ST    R6,AERRTABX                                                      
*                                                                               
         LHI   R5,4000             ROOM FOR 1000 SKIPPED CLT'S                  
         ST    R5,DMCB+4                                                        
         ST    R5,DMCB+8                                                        
         GOTO1 (RF),(R1),C'GET'                                                 
         ICM   R6,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ACLTTAB                                                       
         LA    R6,0(R5,R6)                                                      
         ST    R6,ACLTTABX                                                      
*                                                                               
         LHI   R5,4000             ROOM FOR 1000 ERROR CLT'S                    
         ST    R5,DMCB+4                                                        
         ST    R5,DMCB+8                                                        
         GOTO1 (RF),(R1),C'GET'                                                 
         ICM   R6,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ACLTERR                                                       
         LA    R6,0(R5,R6)                                                      
         ST    R6,ACLTERRX                                                      
*                                                                               
         L     R5,=AL4(MONMAX*MONTOTL) MONTH TOTALS                             
         ST    R5,DMCB+4                                                        
         ST    R5,DMCB+8                                                        
         GOTO1 (RF),(R1),C'GET'                                                 
         ICM   R6,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,AMONTOTS                                                      
         LA    R6,0(R5,R6)                                                      
         ST    R6,AMONTOTX                                                      
*                                                                               
         LHI   R5,DELCTRQ              DELETED RECORD TOTALS                    
         ST    R5,DMCB+4                                                        
         ST    R5,DMCB+8                                                        
         GOTO1 (RF),(R1),C'GET'                                                 
         ICM   R6,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ATOTALS                                                       
         LHI   R0,DELCTRQ/8                                                     
         ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R0,*-10                                                          
*                                                                               
FRSTX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUNLAST - AGENCY SUMMARY REPORT                                     *         
***********************************************************************         
         SPACE 1                                                                
LAST     L     RE,ASAVE            RESTORE SAVED VALUES                         
         LA    RF,SAVVALSL                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   I8REQST,BLANKS      PUT END OF REQUESTS CARD                     
         MVC   I8REQST(2),=C'/*'                                                
         L     R3,AI8RQDCB                                                      
         LA    R0,I8REQST                                                       
         PUT   (R3),(R0)                                                        
*                                                                               
         CLOSE ((R3))              CLOSE I8REQ FILE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   I8REQST,BLANKS      PUT END OF REQUESTS CARD                     
         MVC   I8REQST(2),=C'/*'                                                
         L     R3,ATRRQDCB                                                      
         LA    R0,I8REQST                                                       
         PUT   (R3),(R0)                                                        
*                                                                               
         CLOSE ((R3))              CLOSE I8REQ FILE                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,DRIVINIT         INITILAIZE DRIVER                            
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINIT       CALL DRIVER FOR INITIALIZATION               
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
         L     R3,AECTEMP                                                       
         LA    R6,TOTREC                                                        
         CLOSE ((R3))              CLOSE TEMP FILE                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ACTSW,C'Y'          TEST ANY TEMP FILE RECORDS                   
         BNE   LAST5                                                            
         MVI   GLOPTS+2,6          AGENCY SUMMARY REPORT                        
*                                                                               
         DS    0H                                                               
         OPEN  ((R3),INPUT)        AND OPEN FOR INPUT                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R1,LAST5                                                         
         STCM  R1,7,33(R3)         RESET EODAD ADDRESS                          
*                                                                               
LAST2    DS    0H                                                               
         GET   (R3),(R6)                                                        
         MVC   SBQMED,TOTMED                                                    
         MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
         GOTO1 DRIVER,DMCB,(R4)                                                 
         B     LAST2                                                            
*                                                                               
LAST5    L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   SPOOLID,SVSPLID                                                  
         DROP  R1                                                               
         GOTO1 OPENPQ              INITIALIZE SPOOL                             
         CLI   ACTSW,C'Y'          TEST ANY TEMP FILE RECORDS                   
         BNE   LAST7                                                            
         MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
LAST6    DS    0H                                                               
         CLOSE ((R3))                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LAST7    BRAS  RE,GENCLER          GENERATE CLIENT ERROR SUMMARY                
         BRAS  RE,GENSKIP          GENERATE SKIPPED CLIENT REPORT               
         MVC   DUB,=C'TOTAL'                                                    
         BRAS  RE,GENDEL           GENERATE TOTAL DELETED RECORD REPORT         
*                                                                               
LASTX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
VREC     DS    0H                                                               
         OI    SBIOFLAG,SBRDDEL    READ DELETED RECS                            
         CLI   TWAFIRST,0          TEST FIRST REQUEST                           
         BNE   VREC0                                                            
*                                                                               
         MVI   FRSTLAST,C'Y'       YES-REQUEST RUNFRST/RUNLAST                  
         LA    R1,MEDNMTAB         CLEAR MEDIA TABLE                            
         LA    R0,MEDMAX                                                        
         XC    0(L'MEDNMTAB,R1),0(R1)                                           
         LA    R1,L'MEDNMTAB(R1)                                                
         BCT   R0,*-10                                                          
         B     VREC1                                                            
*                                                                               
VREC0    L     RE,ASAVE            NO-RESTORE SAVED VALUES                      
         LA    RF,SAVVALSL                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
VREC1    MVI   SVERROR,0                                                        
         MVI   ERROR,0                                                          
         LA    R2,CONOUTH          OVERRIDING THE OUTPUT TYPE INVALID           
         CLI   5(R2),0                                                          
         BNE   EINV                                                             
         LA    R2,ESTMEDH          VALIDATE MEDIA                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   *+8                                                              
         MVI   ERROPT,C'Y'         YES-RETURN IF ERROR                          
         GOTO1 VALMED                                                           
         CLI   SBQMED,C'*'         MEDIA '*' REQ?                               
         BNE   *+16                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   EMED                                                             
         B     *+16                                                             
*                                                                               
         MVI   ERROPT,0                                                         
         CLI   ERROR,INVMED        TEST INVALID MEDIA                           
         BNE   *+12                                                             
         MVI   SVERROR,INVMED      YES-SAVE ERROR CODE AND EXIT                 
         J     XIT                                                              
         MVC   SBMED,SBQMED                                                     
         MVC   SVQMED,SBQMED                                                    
         BAS   RE,ADDMED           ADD MEDIA NAME TO TABLE                      
         MVI   CANADA,C'N'                                                      
         L     R3,AIO1             TEST CANADIAN AGENCY                         
         USING AGYHDRD,R3                                                       
         CLI   AGYPROF+7,C'C'                                                   
         BNE   VREC5                                                            
         CLI   SBQMED,C'C'         YES-MEDIA C AND N ARE INVALID                
         BE    EMED                                                             
         CLI   SBQMED,C'N'                                                      
         BE    EMED                                                             
         CLI   SBQMED,C'T'         TEST MEDIA T                                 
         BNE   VREC5                                                            
         MVI   CANADA,C'Y'         YES-INDICATE CANADA, MEDIA T                 
         MVC   AGYMEDT,SBBAGYMD                                                 
         SR    R0,R0                                                            
         LA    R4,AGYEL            GET MEDIA N AND C VALUES                     
*                                                                               
VREC2    CLI   0(R4),0                                                          
         BE    VREC5                                                            
         CLI   0(R4),2                                                          
         BNE   VREC4                                                            
         CLI   2(R4),C'N'                                                       
         BNE   VREC3                                                            
         MVC   AGYMEDN,3(R4)                                                    
         MVI   SBMED,C'N'                                                       
         MVC   SBMEDNM,4(R4)                                                    
         BAS   RE,ADDMED                                                        
         B     VREC4                                                            
*                                                                               
VREC3    CLI   2(R4),C'C'                                                       
         BNE   VREC4                                                            
         MVC   AGYMEDC,3(R4)                                                    
         MVI   SBMED,C'C'                                                       
         MVC   SBMEDNM,4(R4)                                                    
         BAS   RE,ADDMED                                                        
*                                                                               
VREC4    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VREC2                                                            
         DROP  R3,R4                                                            
*                                                                               
VREC5    LA    R2,ESTCLTH          CLIENT                                       
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   VREC5A                                                           
         XC    6(2,R7),6(R7)       YES-REMOVE ANY CLIENT SECURITY               
*                                                                               
         LA    RF,SYSD                                                          
         AHI   RF,SBTWAACS-SYSD                                                 
         XC    0(L'SBTWAACS,RF),0(RF)                                           
*                                                                               
         MVI   ERROPT,C'Y'         RETURN IF ERROR                              
         MVI   DMOUTBTS,0                                                       
VREC5A   GOTO1 VALCLT                                                           
         MVI   ERROPT,0                                                         
         MVI   DMOUTBTS,X'7D'                                                   
         CLI   ERROR,INVCLT        TEST INVALID CLIENT                          
         BNE   *+12                                                             
         MVI   SVERROR,INVCLT      YES-SAVE ERROR CODE AND EXIT                 
         J     XIT                                                              
         CLI   SBQCGRD,0           NO CLIENT GROUPS                             
         BNE   EINV                                                             
*                                                                               
         LA    R2,ESTPRDH          PRODUCT                                      
         GOTO1 VALPRD                                                           
         CLI   SBQPGRD,C' '        NO PRODUCT GROUPS                            
         BH    EINV                                                             
         CLI   SBQBPRD2,0          NO PIGGYBACK                                 
         BNE   EINV                                                             
         CLC   SBQPRD,=C'POL'      TEST POL REQUEST                             
         BNE   *+14                                                             
         MVI   SBQBPRD,0           YES-MAKE SURE WE GET ALL PRODUCTS            
         MVC   SBQPRD,=C'ALL'                                                   
*                                                                               
         CLI   SBQBPRD,0           TEST SINGLE PRODUCT REQUEST                  
         BE    *+12                 NO                                          
         MVI   ERROR,INVPROD       INVALID PRODUCT                              
         B     TRAPERR                                                          
*                                                                               
         LA    R2,ESTESTH          ESTIMATE                                     
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   *+12                                                             
         MVI   ERROPT,C'Y'         YES-RETURN IF ERROR                          
         MVI   DMOUTBTS,0                                                       
         GOTO1 VALEST                                                           
         MVI   ERROPT,0                                                         
         MVI   DMOUTBTS,X'7D'                                                   
         CLI   ERROR,INVEST        TEST INVALID ESTIMATE                        
         BNE   *+12                                                             
         MVI   SVERROR,INVEST      YES-SAVE ERROR CODE                          
         B     VRECX                   AND EXIT                                 
         MVI   SBQSEPES,C'Y'       TREAT ESTIMATES SEPARATELY                   
*                                                                               
* SET/TEST END DATE                                                             
         GOTO1 DATCON,DMCB,(5,0),(0,MYDUB)             GET TODAY'S DATE         
         GOTO1 ADDAY,(R1),(C'Y',MYDUB),(X'80',MYDUB),-2  MINUS 2 YEARS          
         XC    ENDDATE,ENDDATE     END DATE                                     
         MVC   SUBTITLE,BLANKS                                                  
         LA    R2,ESTDATH                                                       
         CLI   5(R2),0             TEST NO END DATE                             
         BNE   VREC6                                                            
         CLC   SBQEST,SBQESTND     AND NOT ONE ESTIMATE                         
         BNE   EMIS                YES-INSIST ON END DATE                       
*                                                                               
         CLI   OFFLINE,C'Y'        TEST ONLINE                                  
         BE    VREC7                NO                                          
         CLI   ACTNUM,ACTREP       ONLY TEST DATES ON REPORT                    
         BNE   VREC7                                                            
         CLI   ESTDOK,C'Y'         ALREADY OK?                                  
         BE    VREC7                                                            
*                                                                               
* SBESTND NOT FILLED IN.  READ POL EST AND GET END DATE.                        
         XC    KEY,KEY             RTFE (READ EST)                              
         LA    RF,KEY                                                           
         USING ESTHDRD,RF                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,SBQEST                                                   
         DROP  RF                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,INVEST        INVALID ESTIMATE                             
         B     TRAPERR                                                          
*                                                                               
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING ESTHDRD,R1                                                       
         CLC   EEND,MYDUB                                                       
         BNL   NOKERR                                                           
         B     VREC7                                                            
         DROP  R1                                                               
*                                                                               
VREC6    GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(0,BLOCK)                              
         LA    R5,BLOCK                                                         
         USING PERVALD,R5                                                       
         CLI   PVALASSM,X'70'      ONE (AND ONLY ONE) FULL DATE GIVEN?          
         BE    VREC6A              YES - SO CONTINUE                            
         MVI   ERROR,INVDATE       ELSE - SET ERROR                             
         B     TRAPERR             AND GO TO ERROR EXIT                         
*                                                                               
VREC6A   CLI   OFFLINE,C'Y'        TEST ONLINE                                  
         BE    VREC6B               NO                                          
         CLI   ACTNUM,ACTREP       ONLY TEST DATES ON REPORT                    
         BNE   VREC6B                                                           
         CLI   ESTDOK,C'Y'         ALREADY OK?                                  
         BE    VREC6B                                                           
         CLC   PVALESTA,MYDUB                                                   
         BNL   NOKERR                                                           
*                                                                               
VREC6B   MVC   ENDDATE,PVALESTA    SAVE THE DATE                                
         DROP  R5                                                               
*                                                                               
         MVC   SUBTITLE(9),=C'PERIOD TO'                                        
         GOTO1 DATCON,(R1),(0,ENDDATE),(5,SUBTITLE+10)                          
         GOTO1 CENTER,(R1),SUBTITLE,32                                          
*                                                                               
VREC7    MVI   IGNUNPD,C'N'        IGNORE UNPAID                                
         LA    R2,ESTIG1H                                                       
         CLI   5(R2),0                                                          
         BE    VREC8                                                            
         MVC   IGNUNPD,8(R2)                                                    
         CLI   8(R2),C'Y'                                                       
         BE    VREC8                                                            
         CLI   8(R2),C'N'                                                       
         BNE   EINV                                                             
*                                                                               
VREC8    MVI   IGNUNBL,C'N'        IGNORE UNBILLED                              
         LA    R2,ESTIG2H                                                       
         CLI   5(R2),0                                                          
         BE    VREC9                                                            
         MVC   IGNUNBL,8(R2)                                                    
         CLI   8(R2),C'Y'                                                       
         BE    VREC9                                                            
         CLI   8(R2),C'N'                                                       
         BNE   EINV                                                             
*                                                                               
VREC9    MVI   ALLBUYS,C'N'        PRINT ALL BUYLINES OPTION                    
         LA    R2,ESTALLH                                                       
         CLI   5(R2),0                                                          
         BE    VREC10                                                           
         MVC   ALLBUYS,8(R2)                                                    
         CLI   8(R2),C'Y'                                                       
         BE    VREC10                                                           
         CLI   8(R2),C'N'                                                       
         BNE   EINV                                                             
*                                  OPTIONS                                      
VREC10   MVI   IOTRACE,C'N'                                                     
         MVI   TEST,C'Y'           TEST MODE IS THE DEFAULT                     
         MVI   SKIPPU,C'N'                                                      
         MVI   SKIPONLY,C'N'                                                    
         SR    R4,R4                                                            
         LA    R2,ESTOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VREC16                                                           
         GOTO1 SCANNER,DMCB,(R2),BLOCK,0                                        
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    EINV                                                             
         LA    R3,BLOCK                                                         
*                                                                               
VREC12   CLC   =C'SKIPPU',12(R3)   IGNORE PU PROFILE?                           
         BNE   *+12                                                             
         MVI   SKIPPU,C'Y'                                                      
         B     VREC14                                                           
*                                                                               
         CLC   =C'SKIPONLY',12(R3) ONLY REPORT ON SKIPPED CLIENTS?              
         BNE   *+12                                                             
         MVI   SKIPONLY,C'Y'                                                    
         B     VREC14                                                           
*                                                                               
         CLC   =C'COONLY',12(R3)   SPOT CLOSEOUT ONLY?                          
         BNE   *+12                                                             
         MVI   COONLY,C'Y'                                                      
         B     VREC14                                                           
*                                                                               
         CLC   12(5,R3),=C'TRACE'                                               
         BNE   *+12                                                             
         MVI   TRACEOPT,C'Y'                                                    
         B     VREC14                                                           
         CLC   12(7,R3),=C'IOTRACE'                                             
         BNE   *+12                                                             
         MVI   IOTRACE,C'Y'                                                     
         B     VREC14                                                           
         CLC   12(4,R3),=C'TEST'                                                
         BNE   *+12                                                             
         LA    R4,1                                                             
         B     VREC14                                                           
         CLC   12(4,R3),=C'LIVE'   LIVE MODE                                    
         BNE   VREC13                                                           
         MVI   TEST,C'N'                                                        
         CLI   OFFLINE,C'Y'        LIVE MODE ONLY OFFLINE                       
         BNE   EINV                                                             
         B     VREC14                                                           
*                                                                               
VREC13   B     EINV                                                             
*                                                                               
VREC14   LA    R3,32(R3)                                                        
         BCT   R0,VREC12                                                        
*                                                                               
VREC16   CLI   SKIPONLY,C'Y'       OPTION ONLY VALID FOR TEST MODE              
         BNE   *+16                                                             
         MVI   TEST,C'Y'                                                        
         MVI   SKIPPU,C'N'         CAN'T SKIP PU                                
         MVI   COONLY,C'Y'         DON'T GENERATE TRAF & INV CO                 
*                                                                               
         CLI   OFFLINE,C'Y'        TEST ONLINE                                  
         BE    VREC18                                                           
         CLI   ACTNUM,ACTREP       AND ACTION=REPORT                            
         BNE   VREC18                                                           
         LTR   R4,R4               AND TEST OPTION MISSING                      
         BNZ   VREC18                                                           
         LLC   RE,5(R2)            YES-PUT TEST OPTION IN THE OPTIONS           
         LA    R1,8(RE,R2)             FIELD                                    
         LTR   RE,RE                                                            
         BZ    *+16                                                             
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         MVC   0(4,R1),=C'TEST'                                                 
         LA    RE,4(RE)                                                         
         STC   RE,5(R2)                                                         
*                                                                               
VREC18   OI    ESTDOKLH+1,X'0C'    SET LOW INTENSITY                            
         OI    ESTDOKLH+6,X'80'    TRANSMIT                                     
         OI    ESTDOKH+1,X'2C'     HIDE & PROTECT                               
         OI    ESTDOKH+6,X'80'                                                  
         MVI   ESTDOK,C' '                                                      
*                                                                               
VRECX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD MEDIA NAME TO MEDIA TABLE                                       *         
***********************************************************************         
         SPACE 1                                                                
ADDMED   DS    0H                                                               
         LA    R1,MEDNMTAB                                                      
         LA    R0,MEDMAX                                                        
*                                                                               
ADDMED2  CLI   0(R1),0                                                          
         BE    ADDMED4                                                          
         CLC   SBMED,0(R1)                                                      
         BE    ADDMEDX                                                          
         LA    R1,L'MEDNMTAB(R1)                                                
         BCT   R0,ADDMED2                                                       
         DC    H'0'                                                             
*                                                                               
ADDMED4  MVC   0(1,R1),SBMED                                                    
         MVC   1(10,R1),SBMEDNM                                                 
*                                                                               
ADDMEDX  BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EINV     MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     TRAPERR                                                          
EMIS     MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     TRAPERR                                                          
EMED     MVI   ERROR,INVMED        INVALID MEDIA                                
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
NOKERR   NI    ESTDOKLH+1,X'FF'-X'04'   SHOW FIELD                              
         OI    ESTDOKLH+6,X'80'         TRANSMIT                                
         NI    ESTDOKH+1,X'FF'-X'20'-X'0C'  SHOW & UNPROTECT                    
         OI    ESTDOKH+6,X'80'                                                  
         LA    R2,ESTDOKH                                                       
         MVC   GTMSGNO,=Y(1274)                                                 
         B     MYCURSOR                                                         
*                                                                               
MYCURSOR MVI   ERROR,X'FE'         OWN ERROR MESSAGE (CONHEAD HAS MSG)          
         GOTO1 CURSERR                                                          
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
         SPACE 1                                                                
PREP     DS    0H                                                               
         L     R1,ASPOOLD                                                       
         MVC   SBPRINT,VPRINT-SPOOLD(R1)                                        
         CLI   SVERROR,0           TEST ANY REMAINING VALIDATION ERROR          
         BE    PREP4                                                            
         MVC   MYPRINT,BLANKS                                                   
         MVC   MYPRINT(21),=C'*** INVALID MEDIA ***'                            
         CLI   SVERROR,INVMED      EXIT NOW IF INVALID MEDIA, CLIENT            
         BE    PREP2                                   OR ESTIMATE              
         MVC   MYPRINT+12(10),=C'CLIENT ***'                                    
         CLI   SVERROR,INVCLT                                                   
         BE    PREP2                                                            
         MVC   MYPRINT+12(12),=C'ESTIMATE ***'                                  
         CLI   SVERROR,INVEST                                                   
         BNE   PREP4                                                            
*                                                                               
PREP2    GOTO1 SBPRINT,DMCB,MYPRINT-1,=C'BL01'                                  
         B     PREP32                                                           
*                                                                               
PREP4    BRAS  RE,DRIVINIT         INITIALIZE DRIVER                            
         MVC   SBCOMFAC,ACOMFACS   INITIALIZE SPOT BLOCK                        
         GOTO1 DATCON,DMCB,(3,BTODAY),SBQTODAY                                  
         LA    R1,IOHOOK                                                        
         ST    R1,SBIOHOOK                                                      
         LAY   R1,CLTREC                                                        
         ST    R1,SBACLTRC                                                      
         MVC   SBAIO1(12),AIO1     PASS 3 IO AREAS                              
         L     R1,SBAOFFBF         OFF-LINE BUFFER AREA                         
         L     RF,0(R1)            RF=L'AVAILABLE SPACE                         
         LA    R1,4(R1)                                                         
         ST    R1,SBASPTTB                                                      
         SR    RE,RE               L'SPTTAB=2/3 OF AVAILABLE                    
         SLDA  RE,2                                                             
         D     RE,=F'3'                                                         
         LA    RE,1(RF)                                                         
         SRL   RE,1                                                             
         ST    RE,SBLSPTTB                                                      
         AR    R1,RE                                                            
         ST    R1,SBACHUNK                                                      
         MVC   SBEPRD,SBQBPRD      PRODUCT FILTER                               
         MVI   SBEPRD2,0           BRAND 2 NOT SUPPORTED                        
         MVI   SBEUNALL,C'Y'       INCLUDE UNALLOCATED SPOTS                    
         MVI   SBEPAID,C'Y'        GET PAID DOLLARS                             
         MVI   SBEDEM,C'N'         NO DEMOS                                     
         MVI   SBQETYPE,C'*'       INCLUDE ALL ESTIMATE TYPES                   
*                                                                               
         XC    SBBCLT,SBBCLT                                                    
         OI    SBQRDOPT,SBQROSCL   READ SINGLE CLIENT AT A TIME                 
         OI    SBQRDOPT,SBQROMAB   READ MANUAL BILLING RECORDS                  
         OI    SBQREAD,SBQRDBH     READ BILL HEADERS                            
         OI    SBQSKIP,SBQSKMED    ACCOUNTING ONLY                              
*                                                                               
PREP6    MVI   PHASE,1             PHASE 1 = INITIAL READ                       
*                                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINIT       CALL DRIVER FOR INITIALIZATION               
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
         MVI   CLTSW,C'N'                                                       
         MVI   ERRFLAG,0                                                        
         MVI   CANCEL,0                                                         
         MVI   CHKCLT,C'N'                                                      
         ZAP   DELEST,=P'0'        INIT RECORD DELETE COUNTERS                  
         ZAP   DELBUY,=P'0'                                                     
         ZAP   DELGOAL,=P'0'                                                    
         ZAP   DELBILL,=P'0'                                                    
         ZAP   DELSTAB,=P'0'                                                    
         ZAP   DELBCPY,=P'0'                                                    
         L     R1,AMONTOTS         CLEAR MONTH TOTALS TABLE                     
         LA    R0,MONMAX                                                        
         XC    0(MONTOTL,R1),0(R1)                                              
         LA    R1,MONTOTL(R1)                                                   
         BCT   R0,*-10                                                          
         L     R1,AERRTAB          CLEAR BEGINNING OF ERROR TABLE               
         XC    0(2*ERRECL,R1),0(R1)                                             
         ST    R1,ACURERR          SAVE A(CURRENT ERROR TABLE ENTRY)            
*                                                                               
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BNE   PREP10                                                           
         MVC   SVQCLT,SBQCLT                                                    
         OC    SBQBCLT,SBQBCLT     YES-TEST ALL CLIENTS                         
         BNZ   PREP8                                                            
         MVC   SVBCLT,SBBCLT       YES-FIND THE NEXT CLIENT ACROSS              
         MVI   CHKCLT,C'Y'             MEDIA T, N AND C                         
         MVC   NEXTCLT,XFF                                                      
         MVI   SBQMED,C'T'                                                      
         GOTO1 SPOTIO,DMCB,SBLOCK                                               
         MVC   SBBCLT,SVBCLT                                                    
         MVI   SBQMED,C'N'                                                      
         GOTO1 (RF),(R1),SBLOCK                                                 
         MVC   SBBCLT,SVBCLT                                                    
         MVI   SBQMED,C'C'                                                      
         GOTO1 (RF),(R1),SBLOCK                                                 
         CLC   NEXTCLT,XFF         TEST FOUND A CLIENT                          
         BE    PREP30              NO-DONE                                      
         MVC   SVQCLT,SBQCLT       YES-PROCESS THIS CLIENT AS IF IT'S           
         GOTO1 CLUNPK,DMCB,NEXTCLT,SBQCLT      A SINGLE CLIENT REQUEST          
         XC    SBCLT,SBCLT                                                      
         MVI   CHKCLT,C'N'                                                      
*                                                                               
PREP8    BAS   RE,CALLCAN          CALL SPOTIO FOR CANADA                       
         MVC   SBQCLT,SVQCLT                                                    
         B     PREP12                                                           
*                                                                               
PREP10   GOTO1 SPOTIO,DMCB,SBLOCK    *** CALL SPOTIO ***                        
         CLI   CANCEL,0                                                         
         BNE   ERREXIT                                                          
         CLI   CLTSW,C'Y'          CHECK WE GOT A CLIENT                        
         BNE   PREP30                                                           
*                                                                               
PREP12   MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
         CLI   ERRFLAG,0           TEST ANY ERRORS                              
         BE    PREP14              NO-GOTO PHASE 2                              
         CLI   IGNUNPD,C'N'        YES-TEST NOT TO IGNORE UNPAID                
         BNE   *+12                                                             
         TM    ERRFLAG,ERRGRSUP+ERRNETUP    AND THERE'S UNPAID                  
         BNZ   PREP18              YES-SKIP PHASE 2                             
         CLI   IGNUNBL,C'N'        TEST NOT TO IGNORE UNBILLED                  
         BNE   *+12                                                             
         TM    ERRFLAG,ERRGRSUB+ERRNETUB    AND THERE'S UNBILLED                
         BNZ   PREP18              YES-SKIP PHASE 2                             
         TM    ERRFLAG,ERRPIG+ERRPDT+ERRBDT  ANY OTHER ERRORS?                  
         BNZ   PREP18                         YES                               
*                                                                               
PREP14   MVI   PHASE,2             CALL SPOTIO FOR DELETE PHASE                 
         MVC   SVQCLT,SBQCLT                                                    
         MVC   SBQCLT,SBCLT                                                     
         XC    SBCLT,SBCLT                                                      
         CLI   CANADA,C'Y'                                                      
         BNE   *+12                                                             
         BAS   RE,CALLCAN                                                       
         B     PREP16                                                           
         GOTO1 SPOTIO,DMCB,SBLOCK                                               
*                                                                               
* NOW CALL SPOTIO TO PASS BUY COPY RECORDS                                      
         OI    SBQSKIP,SBQSKGL+SBQSKBIL   SKIP GOALS AND BILLS                  
         NI    SBQREAD,X'FF'-SBQRDBH      AND BILL HEADERS                      
         LR    RE,R9                                                            
         AHI   RE,SBEFLAG2-SYSD        SET BUY COPY &                           
         OI    0(RE),SBEORIG+SBEPETAB     AND DON'T BUILD PE TAB                
         XC    SBCLT,SBCLT                                                      
         GOTO1 (RF),(R1),SBLOCK                                                 
         NI    SBQSKIP,X'FF'-SBQSKGL-SBQSKBIL                                   
         OI    SBQREAD,SBQRDBH                                                  
         LR    RE,R9                                                            
         AHI   RE,SBEFLAG2-SYSD                                                 
         NI    0(RE),X'FF'-SBEORIG        RESET BUY COPY                        
         NI    0(RE),X'FF'-SBEPETAB       AND BUILD PE TABLE                    
*                                                                               
                                                                                
PREP16   MVI   SBMODE,0            DONE PROCESSING RECS VIA SPOTIO              
         MVC   SBQCLT,SVQCLT                                                    
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BNE   *+10                                                             
         MVC   SBBAGYMD,AGYMEDT    YES-SET MEDIA TO T                           
         BRAS  RE,XLK              DELETE XSP STATION LOCKIN RECS               
         BRAS  RE,UPL              DELETE UPLOAD RECORDS                        
         BRAS  RE,AUT              DELETE SDE AUTHORISATION RECRODS             
         BRAS  RE,PGEST            DELETE PG ESTIMATE RECORDS                   
         BRAS  RE,CHILD            DELETE CHILD SPOT RECORDS                    
         BRAS  RE,STATUS           DELETE STATUS RECORDS                        
         BRAS  RE,BWS              DELETE BUYERS WORKSHEET RECORDS              
         BRAS  RE,BGR              DELETE BUYING GUIDELINE STATUS REC           
         BRAS  RE,MSR              DELETE MATCHING STATUS RECORDS               
         BRAS  RE,DARE             DELETE DARE RECORDS                          
         BRAS  RE,DAREBTCH         DELETE DARE BATCH RECORDS                    
         BRAS  RE,DAREFLT          DELETE DARE FLIGHT RECORDS                   
         BRAS  RE,PW               DELETE PW RECORDS                            
         BRAS  RE,BHR              DELETE BUY HISTORY RECORDS                   
         BRAS  RE,UCOM             DELETE USER COMMENT RECORDS                  
         BRAS  RE,XCOM             DELETE X COMMENT RECORDS                     
         BRAS  RE,NDEF             DELETE NETDEF/CBLPRO RECORDS                 
         BRAS  RE,BFORM            DELETE BILL FORMULA RECORDS                  
         BRAS  RE,SPBIL            DELETE SPLIT BILL RECORDS                    
         BRAS  RE,SBHOLD           DELETE SPOT BHOLD RECORDS                    
         BRAS  RE,SPLIT            DELETE SPLIT RECORDS                         
         BRAS  RE,SDREV            DELETE REVISION/WORK/PROPOSAL RECS           
         BRAS  RE,GENDEL           GENERATE DELETED RECORDS REPORT              
         BRAS  RE,ADDI8TR          ADD AN I8 REQUEST FOR THIS CLIENT            
         B     PREP20                                                           
*                                                                               
PREP18   BRAS  RE,GENERR           GENERATE ERROR REPORT                        
         B     PREP26              AND IGNORE THE MONTH TOTALS                  
*                                                                               
PREP20   MVC   TOTCLT,SBCLT        WRITE MONTH TOTALS TO TEMP FILE              
         L     R2,AECTEMP                                                       
         LA    R3,TOTREC                                                        
         L     R6,AMONTOTS                                                      
         USING MONTOTD,R6                                                       
         LA    R0,MONMAX                                                        
*                                                                               
PREP22   OC    MTMON,MTMON                                                      
         BZ    PREP26                                                           
         MVC   TOTMED,MTMED                                                     
         MVC   TOTCLTNM,SBCLTNM                                                 
         CLI   CANADA,C'Y'                                                      
         BNE   PREP24                                                           
         MVC   TOTCLTNM,CLTNMT                                                  
         CLI   TOTMED,C'T'                                                      
         BE    PREP24                                                           
         MVC   TOTCLTNM,CLTNMN                                                  
         CLI   TOTMED,C'N'                                                      
         BE    PREP24                                                           
         DC    H'0'                                                             
*                                                                               
PREP24   MVC   TOTMON,MTMON                                                     
         ZAP   TOTORD,MTORD                                                     
         ZAP   TOTNET,MTNET                                                     
         ZAP   TOTPAID,MTPAID                                                   
         ZAP   TOTNETPD,MTNETPD                                                 
         ZAP   TOTUNPD,MTUNPD                                                   
         ZAP   TOTNETUP,MTNETUP                                                 
         ZAP   TOTBILL,MTBILL                                                   
         ZAP   TOTBILLN,MTBILLN                                                 
         PUT   (R2),(R3)                                                        
         MVI   ACTSW,C'Y'                                                       
         LA    R6,MONTOTL(R6)                                                   
         BCT   R0,PREP22                                                        
*                                                                               
PREP26   OC    SBQBCLT,SBQBCLT     TEST ALL CLIENT REQUEST                      
         BNZ   PREP30                                                           
         BRAS  RE,CLRGLOB          YES-CLEAR DRIVER'S GLOBAL AREA               
         B     PREP6               PROCESS NEXT CLIENT                          
*                                                                               
PREP30   L     R3,AGLOBAL          FREE UP GLOBAL STORAGE                       
         L     R5,=A(GLOBALLN)                                                  
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
         L     R3,SBAMGTAB         FREE UP MARKET GROUP TABLE                   
         L     R5,=F'20000'                                                     
         GOTO1 (RF),(R1),C'FREE',(R3),(R5)                                      
*                                                                               
PREP32   L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   SVSPLID,SPOOLID                                                  
         DROP  R1                                                               
         L     R0,ASAVE            SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALSL                                                      
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
PREPX    J     XIT                                                              
         SPACE 2                                                                
ERREXIT  LAY   R1,CANCELTB                                                      
         SR    RF,RF                                                            
ERREXIT2 CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,1(R1)                                                         
         CLC   CANCEL,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,2(RF,R1)                                                      
         B     ERREXIT2                                                         
         MVC   MYPRINT,BLANKS                                                   
         MVC   MYPRINT(17),=C'***** ERROR *****'                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MYPRINT+18(0),2(R1)                                              
         GOTO1 SBPRINT,DMCB,MYPRINT-1,=C'BL01'                                  
         B     PREP30              CONTINUE TO NEXT REQUEST                     
         EJECT                                                                  
***********************************************************************         
* CALL SPOTIO FOR CANADA                                              *         
***********************************************************************         
         SPACE 1                                                                
CALLCAN  LR    R0,RE                                                            
         MVI   SBQMED,C'T'                                                      
         GOTO1 SPOTIO,DMCB,SBLOCK                                               
         BAS   RE,CHKCAN                                                        
         MVC   CLTNMT,SBCLTNM                                                   
         MVC   ERRFLAGT,ERRFLAG                                                 
         MVI   ERRFLAG,0                                                        
         XC    SBCLT,SBCLT                                                      
         MVI   SBQMED,C'N'                                                      
         OI    SBQCAN,SBQCBYM0     READ MARKET 0 BUYS FOR NETWORK               
         ZAP   TPAID,=P'0'                                                      
         ZAP   TNETBILL,=P'0'                                                   
         ZAP   TSTABILL,=P'0'                                                   
         GOTO1 (RF),(R1),SBLOCK                                                 
         BAS   RE,CHKCAN                                                        
         MVC   CLTNMN,SBCLTNM                                                   
         MVC   ERRFLAGN,ERRFLAG                                                 
         MVI   ERRFLAG,0                                                        
         XC    SBCLT,SBCLT                                                      
         MVI   SBQMED,C'C'                                                      
         NI    SBQCAN,255-SBQCBYM0                                              
         OI    SBQSKIP,SBQSKBUY+SBQSKBIL SKIP BUYS,GOALS,BILLS                  
         NI    SBQREAD,255-SBQRDBH       AND BILL HEADERS FOR MEDIA C           
         GOTO1 (RF),(R1),SBLOCK                                                 
         BAS   RE,CHKCAN                                                        
         MVC   ERRFLAGC,ERRFLAG                                                 
         OC    ERRFLAG,ERRFLAGT    LEAVE ERRFLAG WITH ERRORS FOR T,N,C          
         OC    ERRFLAG,ERRFLAGN                                                 
         NI    SBQSKIP,255-SBQSKBUY-SBQSKGL-SBQSKBIL                            
         OI    SBQREAD,SBQRDBH                                                  
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
CHKCAN   CLI   PHASE,1             CAN CANCEL ON FIRST PHASE                    
         BNER  RE                                                               
         CLI   CANCEL,0                                                         
         BNE   ERREXIT                                                          
         BR    RE                                                               
***********************************************************************         
* SPOTIO HOOK                                                         *         
***********************************************************************         
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCCL     CLIENT FIRST                                 
         BE    CLIENT                                                           
         CLI   SBMODE,SBPROCES     ESTIMATE FIRST                               
         BE    ESTIMATE                                                         
         CLI   SBMODE,SBPROCSP     BUY RECORD                                   
         BE    PROCBUY                                                          
         CLI   SBMODE,SBPROCGL     GOAL RECORD                                  
         BE    PROCGOAL                                                         
         CLI   SBMODE,SBPROCBL     STATION BILL RECORD                          
         BE    PROCBILL                                                         
         CLI   SBMODE,SBPROCBH     BILL HEADER RECORD                           
         BE    PROCBH                                                           
*                                                                               
HKX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CLIENT FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
CLIENT   DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0PU'    SPOT PURGE CLOSEOUT CONTROL                  
         MVC   WORK+4(2),SBAGY                                                  
         MVC   WORK+6(1),SBQMED                                                 
         MVC   WORK+7(3),SBCLT                                                  
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         GOTO1 GETPROF,DMCB,(X'90',WORK),PUPROF,DATAMGR                         
*                                                                               
         CLI   SKIPONLY,C'Y'       SKIPPED CLIENTS ONLY?                        
         BNE   CLT10                NO                                          
         CLI   PUPROF+2,C'Y'                                                    
         BE    CLT40               ONLY REPORTING ON SKIPPED CLIENTS            
         OI    SBIOFLAG,SBSKPCLT   SKIP THIS CLT                                
         B     HKX                                                              
*                                                                               
CLT10    CLI   SKIPPU,C'Y'         SKIP PU CHECKING?                            
         BE    CLT40                YES                                         
         CLI   PUPROF+2,C'Y'       SKIP CLIENT?                                 
         BNE   CLT40                NO                                          
         OI    SBIOFLAG,SBSKPCLT   SKIP THIS CLT                                
*                                                                               
         L     RF,ACLTTAB          ADD CLIENT TO SKIPPED TABLE                  
CLT20    OC    0(4,RF),0(RF)                                                    
         BZ    CLT30                                                            
         LA    RF,4(RF)                                                         
         C     RF,ACLTTABX                                                      
         BL    CLT20                                                            
         DC    H'0'                                                             
*                                                                               
CLT30    MVC   0(1,RF),SBQMED                                                   
         MVC   1(3,RF),SBCLT                                                    
         B     HKX                                                              
*                                                                               
CLT40    CLI   CHKCLT,C'Y'         TEST CHECKING CLIENTS                        
         BNE   CLT50                                                            
         CLC   SBBCLT,NEXTCLT      YES-SAVE CLIENT IF LOWER THAN LAST           
         BNL   *+10                                                             
         MVC   NEXTCLT,SBBCLT                                                   
         MVI   SBMODE,SBSTOP       STOP SPOTIO                                  
         B     HKX                                                              
*                                                                               
CLT50    MVI   CLTSW,C'Y'                                                       
         CLI   PHASE,1             ON FIRST PHASE,                              
         BNE   HKX                                                              
         GOTO1 PUTCLTNM            PUT CLIENT DETAILS TO BUFFER                 
         B     HKX                                                              
         SPACE 3                                                                
***********************************************************************         
* ESTIMATE FIRST                                                      *         
***********************************************************************         
         SPACE 1                                                                
ESTIMATE DS    0H                                                               
         L     R2,SBAIO1                                                        
         USING ESTHDRD,R2                                                       
         OC    ENDDATE,ENDDATE     TEST END DATE                                
         BZ    EST2                                                             
         CLC   EEND,ENDDATE        YES-TEST ESTIMATE END AFTER END DATE         
         BNH   EST2                                                             
         LLC   RE,EPRDCD+1         YES-BACK ESTIMATE OUT OF                     
         BCTR  RE,0                    PRODUCT/ESTIMATE TABLE                   
         SLL   RE,8                                                             
         LLC   RF,EKEYEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         MVI   0(RE),0                                                          
         B     ESTX                                                             
*                                                                               
EST2     CLI   SBQBPRD,0           TEST SINGLE PRODUCT REQUEST                  
         BE    EST4                                                             
         CLC   EKEYPRD,=C'POL'     YES-TEST THIS IS A POL ESTIMATE              
         BNE   EST4                                                             
         MVI   CANCEL,CANPOL       YES-FATAL ERROR                              
         MVI   SBMODE,SBSTOP           POL CLIENT CANNOT CLOSE BY PRD           
         B     ESTX                                                             
*                                                                               
EST4     CLI   PHASE,1                                                          
         BNE   EST6                                                             
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLOPTS+2,1          ESTIMATE SUMMARY                             
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R4)    CALL DRIVER FOR INPUT                        
         B     ESTX                                                             
*                                                                               
EST6     LR    RE,R9               DON'T DELETE ESTS FOR BUY COPIES             
         AHI   RE,SBEFLAG2-SYSD  (SHOULD ALREADY BE DONE)                       
         TM    0(RE),SBEORIG                                                    
         BNZ   ESTX                                                             
*                                                                               
         AP    DELEST,=P'1'        AUGMENT N'DELETED ESTIMATES                  
         BRAS  RE,DELETE           AND DELETE                                   
*                                                                               
ESTX     B     HKX                                                              
         EJECT                                                                  
***********************************************************************         
* BUY RECORD HOOK FROM SPOTIO                                         *         
***********************************************************************         
         SPACE 1                                                                
PROCBUY  L     R2,SBAIO1           A(BUY RECORD)                                
         USING BUYRECD,R2                                                       
         CLC   SBBMKT,BUYMSTA      REJECT SPILL                                 
         BNE   BUYX                                                             
         MVI   LINERR,0                                                         
         XC    BUYUNPD,BUYUNPD                                                  
         XC    BUYNETUP,BUYNETUP                                                
         MVC   PIGEST,BLANKS                                                    
         CLI   BDTIME,0            TEST NON-POL PIGGYBACK                       
         BE    BUY6                                                             
         LA    R3,BDELEM           YES-FIND PIGGYBACK ELEMENT                   
         SR    R0,R0                                                            
*                                                                               
BUY2     CLI   0(R3),0                                                          
         BE    BUY6                                                             
         CLI   0(R3),4                                                          
         BE    *+14                                                             
         IC    R0,1(R3)                                                         
         AR    R3,0                                                             
         B     BUY2                                                             
         USING PBELEM,R3                                                        
         MVC   PIGEST(3),PBPRD     SAVE PIGGYBACK PRODUCT/ESTIMATE              
         MVC   PIGEST+3(1),PBEST                                                
         LLC   RE,PBPROD           CHECK PIGGYBACK ESTIMATE IS VALID            
         BCTR  RE,0                FOR CLOSEOUT                                 
         SLL   RE,8                                                             
         LLC   RF,PBEST                                                         
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BNE   BUY6                                                             
         OI    ERRFLAG,ERRPIG      INVALID FOR CLOSEOUT                         
         OI    LINERR,LINERRPE                                                  
*                                                                               
BUY6     CLI   PHASE,1                                                          
         BNE   BUY30                                                            
*                                                                               
         TM    BUYKAM,X'08'        BUY COPY?                                    
         BNZ   BUYX                 YES - NO CHECKING HERE                      
*                                                                               
         CLI   SBQMED,C'N'         TEST CANADIAN NETWORK                        
         BNE   *+14                                                             
         OC    SBBMKT,SBBMKT       AND MARKET 0 NETWORK BUY                     
         BZ    BUYX                YES-DOESN'T APPEAR ON REPORT                 
         OC    BDSTART,BDSTART     CHECK BUY START IS SET                       
         BZ    BUY14                                                            
         GOTO1 DATCON,DMCB,(3,BDSTART),(2,DUB)                                  
         GOTO1 (RF),(R1),(3,BDEND),(2,HALF)                                     
         LA    R3,BDELEM                                                        
         SR    R0,R0               SCAN ELEMENTS FOR TRUE END OF BUY            
*                                                                               
BUY8     CLI   0(R3),0                                                          
         BE    BUY12                                                            
         CLI   0(R3),6                                                          
         BL    BUY10                                                            
         CLI   0(R3),13                                                         
         BH    BUY10                                                            
         USING REGELEM,R3                                                       
         CLC   RDATE,HALF                                                       
         BNH   *+10                                                             
         MVC   HALF,RDATE                                                       
         CLC   RDATE,DUB                                                        
         BNL   *+10                                                             
         MVC   DUB(2),RDATE                                                     
         OC    RPAY,RPAY           TEST PAID IN CURRENT MONTH                   
         BZ    BUY10                                                            
         GOTO1 DATCON,DMCB,(2,RPAY),(3,FULL)                                    
         CLC   FULL(2),BTODAY                                                   
         BL    BUY10                                                            
         OI    LINERR,LINERRPD     YES-ERROR                                    
         OI    ERRFLAG,ERRPDT                                                   
*                                                                               
BUY10    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BUY8                                                             
*                                                                               
BUY12    GOTO1 DATCON,DMCB,(2,DUB),(0,WORK)   REAL START DATE                   
         MVC   WORK+4(2),=C'01'    SET START TO START OF MONTH                  
         GOTO1 (RF),(R1),(2,HALF),(0,WORK+6)  REAL END DATE                     
         CLC   WORK(6),WORK+6                                                   
         BNH   *+10                                                             
         MVC   WORK(6),WORK+6                                                   
         GOTO1 ADDAY,(R1),WORK+6,WORK+12,7      FORCE A GOOD END DATE           
         MVC   WORK+6(6),WORK+12                                                
         XC    WORK+12(16),WORK+12                                              
         MVC   WORK+12(4),GETBROAD                                              
         MVC   WORK+16(4),ADDAY                                                 
         MVC   WORK+20(4),GETDAY                                                
         MVC   WORK+24(4),DATCON                                                
         LA    R1,WORK             BUILD A LIST OF MONTHS                       
         ST    R1,DMCB                                                          
         MVI   DMCB,NMONTHS-1                                                   
         L     R4,AMONTHS                                                       
***                                                                             
* WE USED TO NOT PASS IN A 4TH PARM TO MOBILE WHICH CAUSED ISSUES               
* BECAUSE KEY OR XKEY WOULD BE PASSED IN FROM THE LAST DATAMGR                  
* CALL AND CAUSED MOBILE TO BUILD THE DATE TABLES INCORRECTLY                   
***                                                                             
         GOTO1 MOBILE,DMCB,,(1,(R4)),WORK+12,SBSPPROF                           
         ST    R4,SBADATE                                                       
         LR    RE,R4                                                            
         SR    R1,R1                                                            
         CLI   0(RE),X'FF'                                                      
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    RE,4(RE)                                                         
         B     *-16                                                             
         ST    R1,SBNDATES                                                      
         XC    0(2,R4),0(R4)       FIRST MONTH START = 0                        
*                                                                               
BUY14    GOTO1 SPOTBUY,PARAS,SBLOCK    GET DOLLARS BY MONTH                     
         L     R5,SBACHUNK                                                      
         USING SCHUNKD,R5                                                       
*                                                                               
BUY16    OC    SCNEXT,SCNEXT       TEST END OF CHUNKS                           
         BZ    BUYX                                                             
         ST    R5,SBACURCH         A(CURRENT EXTRACT CHUNK)                     
         CLI   SCPRD1,254          TEST UNALLOCATED                             
         BNE   *+8                                                              
         MVI   SCPRD1,X'FF'        YES-CHANGE TO POL                            
         CLC   SBBPRD,SCPRD1       TEST CHANGE OF PRODUCT                       
         BE    BUY18                                                            
         MVC   SBBPRD,SCPRD1       YES-GET PRODUCT CODE                         
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH                                                     
         DROP  R1                                                               
*                                                                               
BUY18    L     R3,SBADATE          GET CHUNK YEAR/MONTH                         
         L     R0,SBNDATES                                                      
         CLC   SCDATE,2(R3)                                                     
         BNH   *+14                                                             
         LA    R3,4(R3)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         CLC   SCDATE,0(R3)                                                     
         BNL   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(2,2(R3)),(3,FULL)                                   
*                                                                               
         L     R1,SCGROSS          CONVERT DOLLARS TO PACKED                    
         CVD   R1,DUB                                                           
         ZAP   TOTORD,DUB                                                       
         L     R1,SCNET                                                         
         CVD   R1,DUB                                                           
         ZAP   TOTNET,DUB                                                       
         L     R1,SCPAY                                                         
         CVD   R1,DUB                                                           
         ZAP   TOTPAID,DUB                                                      
         L     R1,SCPAYN                                                        
         CVD   R1,DUB                                                           
         ZAP   TOTNETPD,DUB                                                     
         L     R1,SCUNP                                                         
         CVD   R1,DUB                                                           
         ZAP   TOTUNPD,DUB                                                      
         L     R1,SCUNPN                                                        
         CVD   R1,DUB                                                           
         ZAP   TOTNETUP,DUB                                                     
         A     R1,BUYUNPD          ACCUMULATE UNPAID                            
         ST    R1,BUYUNPD                                                       
         L     R1,SCUNPN           AND NET UNPAID                               
         A     R1,BUYNETUP                                                      
         ST    R1,BUYNETUP                                                      
*                                                                               
         L     R6,AMONTOTS         POST DOLLARS TO MONTH TOTAL TABLE            
         USING MONTOTD,R6                                                       
         LA    R0,MONMAX                                                        
*                                                                               
BUY20    OC    MTMON,MTMON                                                      
         BNZ   BUY22                                                            
         MVC   MTMED,SBQMED                                                     
         MVC   MTMON,FULL                                                       
         ZAP   MTORD,TOTORD                                                     
         ZAP   MTNET,TOTNET                                                     
         ZAP   MTPAID,TOTPAID                                                   
         ZAP   MTNETPD,TOTNETPD                                                 
         ZAP   MTUNPD,TOTUNPD                                                   
         ZAP   MTNETUP,TOTNETUP                                                 
         ZAP   MTBILL,=P'0'                                                     
         ZAP   MTBILLN,=P'0'                                                    
         B     BUY24                                                            
*                                                                               
BUY22    CLC   MTMED,SBQMED                                                     
         BNE   *+14                                                             
         CLC   MTMON,FULL                                                       
         BE    *+14                                                             
         LA    R6,MONTOTL(R6)                                                   
         BCT   R0,BUY20                                                         
         DC    H'0'                MONTH TOTAL TABLE OVERFLOW                   
         AP    MTORD,TOTORD                                                     
         AP    MTNET,TOTNET                                                     
         AP    MTPAID,TOTPAID                                                   
         AP    MTNETPD,TOTNETPD                                                 
         AP    MTUNPD,TOTUNPD                                                   
         AP    MTNETUP,TOTNETUP                                                 
*                                                                               
BUY24    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLOPTS+2,3          BUY REPORT                                   
         MVI   GLOPTS+3,0                                                       
         MVI   GLMODE,GLINPUT                                                   
         MVC   ESTLIN(1),BUYKEST   SET ESTIMATE                                 
         OC    BUYUNPD,BUYUNPD     PUT BUYLINE DETAIL IF ANY UNPAID             
         BNZ   BUY25                                                            
         OC    BUYNETUP,BUYNETUP   OR ANY NET UNPAID                            
         BNZ   BUY25                                                            
         CLI   LINERR,0            OR THERE'S AN ERROR                          
         BNE   BUY25                                                            
         CLI   ALLBUYS,C'Y'        OR ALL LINES REQUESTED                       
         BNE   BUY26                                                            
*                                                                               
BUY25    MVI   ESTLIN+1,0          CONVERT TO 2-BYTE BUYLINE                    
         MVC   ESTLIN+2(1),BUYKBUY SET BUYLINE                                  
         TM    BUYRCNTL,BUYRLN2    2-BYTE BUYLINE?                              
         BZ    *+10                NO                                           
         MVC   ESTLIN+1(2),BUYKBUY YES - SET BUYLINE                            
         MVI   MAXTOTLV,255        SUPPRESS TOTALS                              
         GOTO1 DRIVER,DMCB,(R4)    CALL DRIVER FOR INPUT                        
*                                                                               
BUY26    MVI   MAXTOTLV,0          STATION TOTAL RECORD                         
         MVC   ESTLIN+1(2),XFF                                                  
         MVC   PIGEST,BLANKS                                                    
         GOTO1 DRIVER,DMCB,(R4)    CALL DRIVER FOR INPUT                        
*                                                                               
         CLI   SBQMED,C'N'         TEST CANADIAN NETWORK                        
         BNE   BUY27                                                            
         OC    SBNETWK,SBNETWK                                                  
         BZ    BUY27                                                            
         MVC   SVBMKT,SBBMKT       YES-CALL DRIVER FOR NETWORK                  
         XC    SBBMKT,SBBMKT                                                    
         MVC   SVSTA,SBSTA                                                      
         MVC   SBSTA(4),SBNETWK                                                 
         MVI   SBSTA+4,C' '                                                     
         MVI   GLOPTS+3,C'N'                                                    
         GOTO1 (RF),(R1),(R4)                                                   
         MVC   SBBMKT,SVBMKT                                                    
         MVC   SBSTA,SVSTA                                                      
         MVI   GLOPTS+3,0                                                       
*                                                                               
BUY27    CLC   BDREP,=H'100'       TEST SPECIAL REP 100                         
         BNE   BUY29                                                            
         CLC   SBQAGY,=C'DF'       AND DANCER                                   
         BE    BUY28                                                            
         CLC   SBQAGY,=C'SF'       OR DANCER SAN FRANCISCO                      
         BE    BUY28                                                            
         CLC   SBQAGY,=C'DI'       OR DOMINANT MARKETING INC                    
         BNE   BUY29                                                            
*                                                                               
BUY28    MVC   ESTLIN,XFF          YES-GENERATE EXTRA STATION TOTALS            
         MVI   MAXTOTLV,255            FOR SPECIAL REP 100                      
         GOTO1 DRIVER,DMCB,(R4)                                                 
         MVI   MAXTOTLV,0                                                       
*                                                                               
BUY29    L     R5,SCNEXT           NEXT CHUNK                                   
         B     BUY16                                                            
*                                                                               
BUY30    TM    BUYKAM,X'08'        BUY COPY?                                    
         BZ    *+14                                                             
         AP    DELBCPY,=P'1'       AUGMENT N'DELETED BUY COPIES                 
         B     *+10                                                             
         AP    DELBUY,=P'1'        AUGMENT N'DELETED BUYS                       
         BRAS  RE,DELETE           AND DELETE                                   
*                                                                               
BUYX     B     HKX                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GOAL RECORD HOOK FROM SPOTIO                                        *         
***********************************************************************         
         SPACE 1                                                                
PROCGOAL CLI   PHASE,1             DELETE GOAL ON SECOND PASS                   
         JE    XIT                                                              
         AP    DELGOAL,=P'1'                                                    
         BRAS  RE,DELETE                                                        
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* STATION BILL RECORD HOOK FROM SPOTIO                                *         
***********************************************************************         
         SPACE 1                                                                
PROCBILL L     R2,SBAIO1                                                        
         USING STABUCKD,R2                                                      
         CLI   SBQBPRD,0           TEST REQUESTED PRD IS POL OR ALL             
         BNE   STAB1                                                            
         LA    RE,254              YES-CHECK FOR POL ESTIMATE OPEN              
         SLL   RE,8                                                             
         LLC   RF,STABKEST                                                      
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    *+14                                                             
         MVC   SBPRD,=C'POL'       YES-PRD=POL                                  
         B     STAB1                                                            
         CLC   SBQPRD,=C'ALL'      NO-IGNORE THIS REC UNLESS QPRD=ALL           
         BNE   STABX                                                            
*                                                                               
STAB1    CLI   PHASE,1                                                          
         BNE   STAB6                                                            
         CLI   STABKCUR,X'01'      SKIP PW/C2 BILL FOR REPORTING                
         BE    STABX                (STILL GETS DELETED THOUGH)                 
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLOPTS+2,3          BUY/BILL REPORT                              
         MVI   GLOPTS+3,0                                                       
         CLI   SBQMED,C'N'         TEST NETWORK LEVEL BILLING                   
         BNE   *+18                                                             
         OC    SBBMKT,SBBMKT                                                    
         BNZ   *+8                                                              
         MVI   GLOPTS+3,C'N'       YES-BILLING GOES TO NETWORK REPORT           
         MVC   PIGEST,BLANKS                                                    
         LA    R5,STABELEM         SEARCH FOR ELEMNTS IN REQUEST PERIOD         
         USING STABELEM,R5                                                      
         SR    R0,R0                                                            
*                                                                               
STAB2    CLI   0(R5),0                                                          
         BE    STABX                                                            
         CLI   0(R5),X'0E'                                                      
         BNE   STAB4                                                            
         ST    R5,SBACURCH         USE CHUNK ADDRESS FOR ELEMENT ADDR           
         MVC   ESTLIN(1),STABKEST  SET ESTIMATE                                 
         MVC   ESTLIN+1(2),XFF                                                  
         GOTO1 DRIVER,DMCB,(R4)    CALL DRIVER FOR INPUT                        
*                                                                               
STAB4    IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     STAB2                                                            
*                                                                               
STAB6    BRAS  RE,DELETE           DELETE ON SECOND PASS                        
         AP    DELSTAB,=P'1'                                                    
*                                                                               
STABX    B     HKX                                                              
         EJECT                                                                  
***********************************************************************         
* BILL HEADER HOOK FROM SPOTIO                                        *         
***********************************************************************         
         SPACE 1                                                                
PROCBH   L     R2,SBAIO1                                                        
         USING BILLRECD,R2                                                      
         CLC   SBQPRD,=C'POL'      TEST REQUESTED PRD IS POL                    
         BNE   BILL1                                                            
         LA    RE,254              YES-CHECK FOR POL ESTIMATE OPEN              
         SLL   RE,8                                                             
         LLC   RF,BKEYEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    BILLX               NO-IGNORE THIS RECORD                        
*                                                                               
BILL1    CLI   PHASE,1                                                          
         BNE   BILL8                                                            
         MVC   SBPRD,BKEYPRD       PRODUCT IS ALWAYS KEYED PRODUCT              
         MVI   LINERR,0                                                         
         CLC   BDATE(4),SBQTODAY   TEST BILLED THIS MONTH                       
         BL    *+12                                                             
         OI    LINERR,LINERRBD     YES-ERROR                                    
         OI    ERRFLAG,ERRBDT                                                   
*                                                                               
         MVC   DUB(4),BMONSERV                                                  
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL)                                     
*                                                                               
         CLI   BRETAIL,X'41'       IGNORE RETAIL SUMMARY BILLS                  
         BE    BILL6                                                            
*                                                                               
         XC    SPBVALD(SPBVALDL),SPBVALD   GET BILL VALUES                      
         GOTO1 SPBVAL,DMCB,(C'B',(R2)),SPBVALD,0                                
*                                                                               
         L     R6,AMONTOTS         POST DOLLARS TO MONTH TOTAL TABLE            
         USING MONTOTD,R6                                                       
         LA    R0,MONMAX                                                        
*                                                                               
BILL2    OC    MTMON,MTMON                                                      
         BNZ   BILL4                                                            
         MVC   MTMED,SBQMED                                                     
         MVC   MTMON,FULL                                                       
         ZAP   MTBILL,SPBVGRSP                                                  
         ZAP   MTBILLN,SPBVNETP                                                 
         ZAP   MTORD,=P'0'                                                      
         ZAP   MTNET,=P'0'                                                      
         ZAP   MTPAID,=P'0'                                                     
         ZAP   MTNETPD,=P'0'                                                    
         ZAP   MTUNPD,=P'0'                                                     
         ZAP   MTNETUP,=P'0'                                                    
         B     BILL6                                                            
*                                                                               
BILL4    CLC   MTMED,SBQMED                                                     
         BNE   BILL5                                                            
         CLC   MTMON,FULL                                                       
         BNE   BILL5                                                            
         AP    MTBILL,SPBVGRSP                                                  
         AP    MTBILLN,SPBVNETP                                                 
         B     BILL6                                                            
*                                                                               
BILL5    LA    R6,MONTOTL(R6)                                                   
         BCT   R0,BILL2                                                         
         DC    H'0'                MONTH TOTAL TABLE OVERFLOW                   
*                                                                               
BILL6    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLOPTS+2,2          BILL HEADER REPORT                           
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R4)    CALL DRIVER FOR INPUT                        
         B     BILLX                                                            
*                                                                               
BILL8    AP    DELBILL,=P'1'       DELETE ON SECOND PASS                        
         BRAS  RE,DELETE                                                        
*                                                                               
BILLX    J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK                                                         *         
***********************************************************************         
         SPACE 1                                                                
DRHOOK   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPUTSRT     PUT RECORD TO SORT                           
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         JE    HEADHK                                                           
*                                                                               
DRHOOKX  J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RESOLVE ROUTINE ADDRESSES                                           *         
***********************************************************************         
         SPACE 1                                                                
RESOLVE  LAY   R1,ROUTLIST         TEST ROUTINE IN THIS OVERLAY                 
RESOLVE2 CLI   0(R1),FF                                                         
         BE    RESOLVEX                                                         
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         J     XIT                                                              
*                                                                               
RESOLVEX B     DRHOOKX                                                          
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* EXECUTING ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXEC     LAY   R1,ROUTLIST         TEST ROUTINE IS IN THIS OVERLAY              
*                                                                               
EXEC2    CLI   0(R1),FF                                                         
         BE    EXECX                                                            
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     EXEC2                                                            
         L     R2,GLAIFLD          YES-R2=A(INPUT)                              
         L     R3,GLAOFLD              R3=A(OUTPUT)                             
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   EXEC4                                                            
         MVC   GLMAXTLV,MAXTOTLV   YES-SET DRIVER'S MAX TOTAL LEVEL             
         L     R5,SBACURCH         ADDRESS SPOTBUY CHUNK                        
         USING SCHUNKD,R5                                                       
         LR    R6,R5                                                            
         USING STABELEM,R6         OR STATION BILL ELEMENT                      
*                                                                               
EXEC4    L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BASR  RE,RF                                                            
*                                                                               
EXECX    B     DRHOOKX                                                          
         SPACE 2                                                                
***********************************************************************         
* ABOUT TO PUT A RECORD TO THE SORT                                   *         
***********************************************************************         
         SPACE 1                                                                
PUTSRT   MVI   INDATA,1            TELL SYSDRIVER THERE IS ALWAYS               
         B     DRHOOKX             COLUMN DATA                                  
         EJECT                                                                  
***********************************************************************         
* DRIVER INPUT ROUTINES                                               *         
***********************************************************************         
*                                                                               
ICLTCD   MVC   0(3,R2),SBCLT                                                    
         BR    RE                                                               
*                                                                               
IESTDT   MVC   0(1,R2),SBBEST                                                   
         MVC   1(2,R2),SBESTSTP                                                 
         MVC   3(2,R2),SBESTNDP                                                 
         BR    RE                                                               
*                                                                               
IESTNM   MVC   0(20,R2),SBESTNM                                                 
         BR    RE                                                               
*                                                                               
IESTLIN  MVC   0(3,R2),ESTLIN                                                   
         BR    RE                                                               
*                                                                               
IPIGEST  MVC   0(4,R2),PIGEST                                                   
         BR    RE                                                               
*                                                                               
IERREST  MVC   0(1,R2),SBBEST                                                   
         BR    RE                                                               
*                                                                               
ILINERR  MVC   0(1,R2),LINERR                                                   
         MVI   1(R2),X'FF'                                                      
         BR    RE                                                               
*                                                                               
IREC     MVC   0(L'RECTYPE,R2),RECTYPE                                          
         BR    RE                                                               
*                                                                               
IERR     MVC   0(1,R2),ERRCD                                                    
         BR    RE                                                               
*                                                                               
IDEL     ZAP   0(8,R2),NUMRECS                                                  
         BR    RE                                                               
*                                                                               
IORD     ZAP   0(8,R2),=P'0'       GROSS ORDERED                                
         CLI   SBMODE,SBPROCSP                                                  
         BNER  RE                                                               
         L     R1,SCGROSS                                                       
         CVD   R1,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
         BR    RE                                                               
*                                                                               
INET     ZAP   0(8,R2),=P'0'       NET ORDERED                                  
         CLI   SBMODE,SBPROCSP                                                  
         BNER  RE                                                               
         L     R1,SCNET                                                         
         CVD   R1,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
         BR    RE                                                               
*                                                                               
IPAID    ZAP   0(8,R2),=P'0'       GROSS PAID                                   
         CLI   SBMODE,SBPROCSP                                                  
         BNER  RE                                                               
         L     R1,SCPAY                                                         
         CVD   R1,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BNER  RE                                                               
         CLI   SBQMED,C'N'         AND MEDIA = N                                
         BNER  RE                                                               
         CLI   GLOPTS+3,C'N'       (AND FIRST CALL TO DRIVER)                   
         BER   RE                                                               
         AP    TPAID,DUB           YES-ACCUMULATE PAID                          
         BR    RE                                                               
*                                                                               
INETPD   ZAP   0(8,R2),=P'0'       NET PAID                                     
         CLI   SBMODE,SBPROCSP                                                  
         BNER  RE                                                               
         L     R1,SCPAYN                                                        
         CVD   R1,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
         BR    RE                                                               
*                                                                               
IBILL    LR    R0,RE               GROSS BILLED                                 
         ZAP   0(8,R2),=P'0'                                                    
         CLI   SBMODE,SBPROCBL                                                  
         BNE   IBILLX                                                           
         XC    SPBVALD(SPBVALDL),SPBVALD   GET BILL VALUES                      
         GOTO1 SPBVAL,DMCB,(C'E',(R6)),SPBVALD,0                                
         ZAP   0(8,R2),SPBVGRSP                                                 
*                                                                               
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BNE   IBILLX                                                           
         CLI   SBQMED,C'N'         TEST MEDIA = N                               
         BNE   IBILLX                                                           
         OC    SBBMKT,SBBMKT       YES-ACCUMULATE NETWORK AND STATION           
         BNZ   *+14                    BILLING                                  
         AP    TNETBILL,SPBVGRSP                                                
         B     IBILLX                                                           
         AP    TSTABILL,SPBVGRSP                                                
*                                                                               
IBILLX   LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
INETBILL LR    R0,RE               NET BILLED                                   
         ZAP   0(8,R2),=P'0'                                                    
         CLI   SBMODE,SBPROCBL                                                  
         BNE   INTBILLX                                                         
         XC    SPBVALD(SPBVALDL),SPBVALD   GET BILL VALUES                      
         GOTO1 SPBVAL,DMCB,(C'E',(R6)),SPBVALD,0                                
         ZAP   0(8,R2),SPBVNETP                                                 
*                                                                               
INTBILLX LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
IBHGRS   LR    R0,RE               BILL HEADER GROSS                            
         ZAP   0(8,R2),=P'0'                                                    
         L     R5,SBAIO1                                                        
         USING BILLRECD,R5                                                      
*                                                                               
         CLI   BRETAIL,X'41'      RETAIL SUMMARY BILL                           
         BE    IBHGRSX            DON'T ADD TO ACCUMULATORS                     
*                                                                               
         XC    SPBVALD(SPBVALDL),SPBVALD   GET BILL VALUES                      
         GOTO1 SPBVAL,DMCB,(C'B',(R5)),SPBVALD,0                                
         ZAP   0(8,R2),SPBVGRSP                                                 
*                                                                               
IBHGRSX  LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  R5                                                               
*                                 AGENCY SUMMARY INPUT ROUTINES                 
ICLT     MVC   0(3,R2),TOTCLT                                                   
         MVI   3(R2),C' '                                                       
         MVC   4(20,R2),TOTCLTNM                                                
         BR    RE                                                               
*                                                                               
IMONTH   MVC   0(2,R2),TOTMON                                                   
         BR    RE                                                               
*                                                                               
IASORD   ZAP   0(8,R2),TOTORD                                                   
         ZAP   8(8,R2),TOTNET                                                   
         BR    RE                                                               
*                                                                               
IASPAID  ZAP   0(8,R2),TOTPAID                                                  
         ZAP   8(8,R2),TOTNETPD                                                 
         BR    RE                                                               
*                                                                               
IASUNPD  ZAP   0(8,R2),TOTUNPD                                                  
         ZAP   8(8,R2),TOTNETUP                                                 
         BR    RE                                                               
*                                                                               
IASBILL  ZAP   0(8,R2),TOTBILL                                                  
         ZAP   8(8,R2),TOTBILLN                                                 
         BR    RE                                                               
*                                                                               
IASBILBL ZAP   DUB,TOTORD                                                       
         SP    DUB,TOTBILL                                                      
         ZAP   0(8,R2),DUB                                                      
         ZAP   DUB,TOTNET                                                       
         SP    DUB,TOTBILLN                                                     
         ZAP   8(8,R2),DUB                                                      
         BR    RE                                                               
*                                                                               
IASUNBIL ZAP   DUB,TOTPAID                                                      
         SP    DUB,TOTBILL                                                      
         ZAP   0(8,R2),DUB                                                      
         ZAP   DUB,TOTNETPD                                                     
         SP    DUB,TOTBILLN                                                     
         ZAP   8(8,R2),DUB                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DRIVER OUTPUT ROUTINES                                              *         
***********************************************************************         
         SPACE 1                                                                
OMED     CLI   GLARGS,C'C'         CODE ONLY?                                   
         BNE   OMED1                                                            
         MVC   SBMED,0(R2)                                                      
         MVC   0(1,R3),SBMED                                                    
         BR    RE                                                               
*                                                                               
OMED1    MVC   OUTAREA,BLANKS            MEDIA                                  
         MVC   LABLAREA(5),=C'MEDIA'                                            
         MVC   SBMED,0(R2)                                                      
         MVI   CODEAREA,C' '                                                    
         MVC   NAMEAREA(10),=C'ALL MEDIA '                                      
         CLI   0(R2),C'A'                                                       
         BE    OMED4                                                            
         MVC   CODEAREA(1),SBMED                                                
         LA    R1,MEDNMTAB         EXTRACT MEDIA NAME FROM MEDIA TABLE          
         LA    R0,MEDMAX                                                        
*                                                                               
OMED2    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SBMED,0(R1)                                                      
         BE    *+14                                                             
         LA    R1,L'MEDNMTAB(R1)                                                
         BCT   R0,OMED2                                                         
         DC    H'0'                                                             
         MVC   NAMEAREA(10),1(R1)                                               
*                                                                               
OMED4    MVC   0(L'OUTAREA,R3),OUTAREA                                          
         BR    RE                                                               
         SPACE 1                                                                
OESTDT   LR    R0,RE                                                            
         LLC   RE,0(R2)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         MVI   3(R3),C' '                                                       
         GOTO1 DATCON,DMCB,(2,1(R2)),(5,4(R3))                                  
         MVI   12(R3),C'-'                                                      
         GOTO1 (RF),(R1),(2,3(R2)),(5,13(R3))                                   
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
OMKT     MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0                                          
         MVC   0(4,R3),SBMKT                                                    
         BR    RE                                                               
         SPACE 1                                                                
OMONTH   LR    R0,RE                                                            
         MVC   FULL(2),0(R2)                                                    
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),(6,(R3))                                    
         MVC   7(5,R3),=C'GROSS'                                                
         MVC   198+7(3,R3),=C'NET'                                              
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
OESTLIN  MVC   ESTLIN,0(R2)        ESTIMATE/LINE                                
         CLC   ESTLIN,XFF                                                       
         BNE   *+12                                                             
         MVC   0(7,R3),=C'REP 100'                                              
         BR    RE                                                               
         LLC   R1,0(R2)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         MVI   3(R3),C'-'                                                       
         CLC   1(2,R2),XFF                                                      
         BNE   *+12                                                             
         MVC   4(3,R3),=C'***'                                                  
         BR    RE                                                               
         ICM   R1,3,1(R2)                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(5,R3),DUB                                                      
         BR    RE                                                               
         SPACE 2                                                                
OPIGEST  CLC   0(4,R2),BLANKS      PARTNER PRODUCT/ESTIMATE                     
         BNHR  RE                                                               
         MVC   0(3,R3),0(R2)                                                    
         LLC   R1,3(R2)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R3),DUB                                                      
         BR    RE                                                               
         SPACE 1                                                                
OERREST  LLC   R1,0(R2)            ESTIMATE FOR ERROR REPORT                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         BR    RE                                                               
         SPACE 2                                                                
OLINERR  TM    GLINDS,GLTOTLIN     NO LINE ERRORS FOR TOTAL LINES               
         BOR   RE                                                               
         MVI   ERRCD,0                                                          
         LA    R3,0(R3)                                                         
         CLI   GLARGS,3            IF BUY/BILL REPORT,                          
         BNE   OLIN8                                                            
         CLC   ESTLIN,XFF          TEST SPECIAL REP 100 LINE                    
         BE    *+14                                                             
         CLC   ESTLIN+1(2),XFF     OR BUY DETAIL LINE                           
         BE    OLIN4                                                            
         ZAP   DUB,ORD             YES-TEST ANY UNPAID                          
         SP    DUB,PAID                                                         
         BZ    OLIN2                                                            
         OI    ERRFLAG,ERRGRSUP    YES-ERROR                                    
         OI    ERRCD,ERRGRSUP                                                   
         LR    R1,R3                                                            
         EDIT  (P8,DUB),(14,(R1)),2,MINUS=YES,ALIGN=LEFT,COMMAS=YES             
         AR    R1,R0                                                            
         MVC   1(6,R1),=C'UNPAID'                                               
         LA    R3,198(R3)                                                       
*                                                                               
OLIN2    ZAP   DUB,NET             TEST ANY NET UNPAID                          
         SP    DUB,NETPD                                                        
         BZ    OLIN8                                                            
         OI    ERRFLAG,ERRNETUP    YES-ERROR                                    
         OI    ERRCD,ERRNETUP                                                   
         LR    R1,R3                                                            
         EDIT  (P8,DUB),(14,(R1)),2,MINUS=YES,ALIGN=LEFT,COMMAS=YES             
         AR    R1,R0                                                            
         MVC   1(10,R1),=C'NET UNPAID'                                          
         LA    R3,198(R3)                                                       
         B     OLIN8                                                            
*                                                                               
*                                  ESTIMATE TOTAL LINE -                        
OLIN4    CLI   GLARGS+1,C'N'       TEST CANADIAN NETWORK LINE                   
         BNE   OLIN4A                                                           
         CLI   CANADA,C'Y'                                                      
         BNE   OLIN4A                                                           
         CP    TNETBILL,=P'0'      YES-IF NETWORK BILLING = 0,                  
         BER   RE                      IGNORE PAID NE BILLED                    
*                                                                               
OLIN4A   CLI   GLARGS+1,C'S'       TEST CANADIAN NETWORK LOCAL STATION          
         BNE   OLIN5               LINE                                         
         CLI   CANADA,C'Y'                                                      
         BNE   OLIN5                                                            
         CLI   SBMED,C'N'                                                       
         BNE   OLIN5                                                            
         CP    TSTABILL,=P'0'      YES-IF STATION BILLING = 0,                  
         BER   RE                      IGNORE PAID NE BILLED                    
*                                                                               
OLIN5    ZAP   DUB,PAID                                                         
         SP    DUB,BILL            TEST PAID=BILLED                             
         BZ    OLIN6                                                            
         MVC   WORK+32(10),=C'PD NOT BLD'                                       
         BP    *+10                                                             
         MVC   WORK+32(10),=C'BLD NOT PD'                                       
         OI    ERRFLAG,ERRGRSUB    NO-ERROR                                     
         OI    ERRCD,ERRGRSUB                                                   
         LR    R1,R3                                                            
         EDIT  (P8,DUB),(14,(R1)),2,ALIGN=LEFT,COMMAS=YES                       
         AR    R1,R0                                                            
         MVC   1(10,R1),WORK+32                                                 
         LA    R3,198(R3)                                                       
*                                                                               
OLIN6    ZAP   DUB,NETPD                                                        
         SP    DUB,NETBILL         TEST NET PAID = NET BILLED                   
         BZ    OLIN20                                                           
         MVC   WORK+32(14),=C'NET PD NOT BLD'                                   
         BP    *+10                                                             
         MVC   WORK+32(14),=C'NET BLD NOT PD'                                   
         OI    ERRFLAG,ERRNETUB    NO-ERROR                                     
         OI    ERRCD,ERRNETUB                                                   
         LR    R1,R3                                                            
         EDIT  (P8,DUB),(14,(R1)),2,ALIGN=LEFT,COMMAS=YES                       
         AR    R1,R0                                                            
         MVC   1(14,R1),WORK+32                                                 
         B     OLIN20                                                           
*                                                                               
OLIN8    CLC   ESTLIN,XFF          EXCEPT FOR SPECIAL REP 100 LINE,             
         BE    OLIN20                                                           
         MVC   LINERR,0(R2)                                                     
         OC    ERRCD,LINERR                                                     
         CLI   LINERR,0            TEST ANY ERRORS                              
         BE    OLIN20                                                           
         LAY   R1,LINERRTB         YES-PRINT APPROPRIATE MESSAGES               
         SR    RF,RF                                                            
*                                                                               
OLIN10   CLI   0(R1),0                                                          
         BE    OLIN20                                                           
         IC    RF,1(R1)                                                         
         MVC   BYTE,0(R1)                                                       
         NC    BYTE,LINERR                                                      
         BZ    OLIN12                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R1)                                                    
         LA    R3,198(R3)                                                       
         LA    RF,1(RF)                                                         
*                                                                               
OLIN12   LA    R1,2(RF,R1)                                                      
         B     OLIN10                                                           
*                                                                               
OLIN20   CLI   ERRCD,0             TEST ANY ERRORS                              
         BER   RE                                                               
         CLI   GLARGS,2                                                         
         BNE   *+16                                                             
         XC    SBBMKT,SBBMKT                                                    
         XC    SBSTA,SBSTA                                                      
         LR    R0,RE                                                            
         BRAS  RE,ADDERR           YES-ADD ERRORS TO ERROR TABLE                
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
OERR     CLI   0(R2),0             ERRORS FOR ERROR REPORT                      
         BER   RE                                                               
         LAY   R5,ERRTABLE                                                      
         SR    R6,R6                                                            
OERR2    CLI   0(R5),0                                                          
         BER   RE                                                               
         IC    R6,1(R5)                                                         
         MVC   BYTE,0(R5)                                                       
         NC    BYTE,0(R2)                                                       
         BZ    OERR4                                                            
         MVC   0(30,R3),BLANKS                                                  
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R5)                                                    
         LA    R3,198(R3)                                                       
         LA    R6,1(R6)                                                         
OERR4    LA    R5,2(R6,R5)                                                      
         B     OERR2                                                            
         SPACE 2                                                                
OORD     ZAP   ORD,0(8,R2)                                                      
         MVI   GLHOOK,GLEDIT                                                    
         BR    RE                                                               
*                                                                               
OPAID    ZAP   PAID,0(8,R2)                                                     
         MVI   GLHOOK,GLEDIT                                                    
         BR    RE                                                               
*                                                                               
OBILL    ZAP   BILL,0(8,R2)                                                     
         MVI   GLHOOK,GLEDIT                                                    
         BR    RE                                                               
*                                                                               
ONET     ZAP   NET,0(8,R2)                                                      
         MVI   GLHOOK,GLEDIT                                                    
         BR    RE                                                               
*                                                                               
ONETPD   ZAP   NETPD,0(8,R2)                                                    
         MVI   GLHOOK,GLEDIT                                                    
         BR    RE                                                               
*                                                                               
ONETBILL ZAP   NETBILL,0(8,R2)                                                  
         MVI   GLHOOK,GLEDIT                                                    
         BR    RE                                                               
*                                                                               
ODOLS    LR    R0,RE                                                            
         LA    R6,2                                                             
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBDECS,2                                                         
         MVI   EBLOUT,18                                                        
         MVI   EBLIN,8                                                          
         MVI   EBTIN,C'P'                                                       
         OI    EBOPT,X'C0'                                                      
*                                                                               
ODOLS2   ST    R2,EBAIN                                                         
         ST    R3,EBAOUT                                                        
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
         LA    R2,8(R2)                                                         
         LA    R3,198(R3)                                                       
         BCT   R6,ODOLS2                                                        
*                                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TGRSNET  CLI   GLARGS,C'A'                                                      
         BNE   *+18                                                             
         MVC   0(21,R3),=C'*** AGENCY TOTALS ***'                               
         LA    R3,25(R3)                                                        
         B     TGRSNET4                                                         
         CLI   GLARGS,C'M'                                                      
         BNE   TGRSNET2                                                         
         MVC   0(12,R3),=C'MEDIA TOTALS'                                        
         MVC   25(11,R3),=C'GROSS TOTAL'                                        
         MVC   198+25(9,R3),=C'NET TOTAL'                                       
         BR    RE                                                               
TGRSNET2 LA    R3,25(R3)                                                        
         MVC   0(5,R3),=C'*ALL*'                                                
TGRSNET4 MVC   7(5,R3),=C'GROSS'                                                
         MVC   198+7(3,R3),=C'NET'                                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST TIME CONTROLS                                                 *         
***********************************************************************         
         SPACE 1                                                                
FIRSTS   CLI   GLARGS,0            TEST LEVEL 0 BREAK                           
         JNE   XIT                                                              
         MVC   TITLE,BLANKS        YES-SET THE APPROPRIATE TITLE                
*                                                                               
         LAY   R1,RPT1TITL                                                      
         CLI   GLRECNO,1                                                        
         BNE   *+14                                                             
         MVC   TITLE(L'RPT1TITL),0(R1)                                          
         B     FIRST2                                                           
*                                                                               
         LAY   R1,RPT2TITL                                                      
         CLI   GLRECNO,2                                                        
         BNE   *+14                                                             
         MVC   TITLE(L'RPT2TITL),0(R1)                                          
         B     FIRST2                                                           
*                                                                               
         LAY   R1,RPT3TITL                                                      
         CLI   GLRECNO,3                                                        
         BE    *+12                                                             
         CLI   GLRECNO,4                                                        
         BNE   *+14                                                             
         MVC   TITLE(L'RPT3TITL),0(R1)                                          
         B     FIRST2                                                           
*                                                                               
         LAY   R1,RPT5TITL                                                      
         CLI   GLRECNO,5                                                        
         BE    *+12                                                             
         CLI   GLRECNO,9                                                        
         BNE   *+14                                                             
         MVC   TITLE(L'RPT5TITL),0(R1)                                          
         B     FIRST2                                                           
*                                                                               
         LAY   R1,RPT6TITL                                                      
         CLI   GLRECNO,6                                                        
         BNE   *+14                                                             
         MVC   TITLE(L'RPT6TITL),0(R1)                                          
         B     FIRST2                                                           
*                                                                               
         LAY   R1,RPT7TITL                                                      
         CLI   GLRECNO,7                                                        
         BNE   *+20                                                             
         MVC   TITLE(L'RPT7TITL),0(R1)                                          
         MVC   SUBTITLE,BLANKS                                                  
         B     FIRST2                                                           
*                                                                               
         LAY   R1,RPT8TITL                                                      
         CLI   GLRECNO,8                                                        
         BNE   *+20                                                             
         MVC   TITLE(L'RPT8TITL),0(R1)                                          
         MVC   SUBTITLE,BLANKS                                                  
         B     FIRST2                                                           
*                                                                               
         LAY   R1,RPT10TITL                                                     
         CLI   GLRECNO,10                                                       
         JNE   XIT                                                              
         MVC   TITLE(L'RPT10TITL),0(R1)                                         
*                                                                               
FIRST2   GOTO1 CENTER,DMCB,TITLE,63                                             
         J     XIT                                                              
         EJECT                                                                  
         DROP  RA,RB                                                            
***********************************************************************         
* DELETE BGR RECORDS                                                            
***********************************************************************         
         SPACE 1                                                                
BGR      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   DELBGR,=P'0'                                                     
         XC    XKEY,XKEY                                                        
         LA    R2,XKEY                                                          
         USING BGRKEYD,R2                                                       
         MVI   BGRKTYPE,BGRKTYPQ                                                
         MVI   BGRKSUB,BGRKSUBQ                                                 
         MVC   BGRKAM,SBBAGYMD                                                  
         MVC   BGRKCLT,SBBCLT                                                   
         MVC   XKEYSAVE,XKEY                                                    
*                                                                               
BGR1     GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     BGR4                                                             
*                                                                               
BGR2     DS    0H                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
BGR4     CLC   XKEY(BGRKPRD-BGRKEY),XKEYSAVE                                    
         BNE   BGRX                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,BGRKPRD        TEST ESTIMATE VALID FOR CLOSEOUT             
         BZ    BGR2                                                             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,BGRKEST                                                     
         BZ    BGR2                DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    BGR2                                                             
BGR8     L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
BGR9     GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',BGRDDA,AIO,DMWORK             
         L     R6,AIO                                                           
         CLI   15(R6),X'FF'        LAST RECORD OF THE SET?                      
         BE    BGR10                                                            
         BRAS  RE,XDEL                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
         MVC   XKEYSAVE,XKEY                                                    
         B     BGR9                                                             
*                                                                               
BGR10    BRAS  RE,XDEL             DELETE THE LAST RECORD                       
         AP    DELBGR,=P'1'                                                     
         B     BGR2                NEXT SET                                     
*                                                                               
BGRX     J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE MSR RECORDS                                                            
***********************************************************************         
         SPACE 1                                                                
MSR      NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELMSR,=P'0'                                                     
         XC    XKEY,XKEY                                                        
         LA    R2,XKEY                                                          
         USING MSRKEYD,R2                                                       
         MVI   MSRKTYPE,MSRKTYPQ                                                
         MVI   MSRKSUB,MSRKSUBQ                                                 
         MVC   MSRKAM,SBBAGYMD                                                  
         MVC   MSRKCLT,SBBCLT                                                   
         MVC   XKEYSAVE,XKEY                                                    
*                                                                               
MSR1     GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     MSR4                                                             
*                                                                               
MSR2     DS    0H                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
MSR4     CLC   XKEY(MSRKPRD-MSRKEY),XKEYSAVE                                    
         BNE   MSRX                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,MSRKPRD        TEST ESTIMATE VALID FOR CLOSEOUT             
         BZ    MSR2                                                             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,MSRKEST                                                     
         BZ    MSR2                DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    MSR2                                                             
*                                                                               
MSR8     L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
MSR9     GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',MSRDDA,AIO,DMWORK             
         L     R6,AIO                                                           
         CLI   15(R6),X'FF'        LAST RECORD OF THE SET?                      
         BE    MSR10                                                            
         BRAS  RE,XDEL                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
         B     MSR9                                                             
*                                                                               
MSR10    BRAS  RE,XDEL             DELETE THE LAST RECORD                       
         AP    DELMSR,=P'1'                                                     
         B     MSR2                NEXT SET                                     
*                                                                               
MSRX     J     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DELETE BUY HISTORY RECORDS                                                    
***********************************************************************         
         SPACE 1                                                                
BHR      NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELBHR,=P'0'                                                     
         XC    XKEY,XKEY                                                        
         LA    R2,XKEY                                                          
         USING HISTRECD,R2                                                      
         MVI   HISTTYP,HISTTYQ                                                  
         MVI   HISTSTYP,HISTSTYQ                                                
         MVC   HISTBKAM,SBBAGYMD                                                
         MVC   HISTBKCL,SBBCLT                                                  
         LA    R3,HISTBKPR-HISTRECD-1                                           
*                                                                               
         CLI   SBQBPRD,0                                                        
         BE    BHR1                                                             
         MVC   HISTBKPR,SBQBPRD                                                 
         LA    R3,L'HISTBKPR(R3)                                                
*                                                                               
*                                                                               
BHR1     DS    0H                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     BHR4                                                             
*                                                                               
BHR2     DS    0H                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
BHR4     DS    0H                                                               
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   XKEY(0),XKEYSAVE                                                 
         BNE   BHRX                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,HISTBKPR       TEST ESTIMATE VALID FOR CLOSEOUT             
         BZ    BHR2                                                             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,HISTBKES                                                    
         BZ    BHR2                DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    BHR2                                                             
*                                                                               
BHR8     L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',HISDDA,AIO,DMWORK             
*                                                                               
         BRAS  RE,XDEL             DELETE THE LAST RECORD                       
         AP    DELBHR,=P'1'                                                     
         B     BHR2                NEXT SET                                     
*                                                                               
BHRX     J     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DELETE ALL COMMENT RECORDS IN SPOTDIR/FIL                                     
***********************************************************************         
         SPACE 1                                                                
UCOM     NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELUCOM,=P'0'                                                    
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING UCOMHDR,R2                                                       
         MVC   UCOMKTYP,=X'0D0C'                                                
         MVC   UCOMKAGY,SBBAGYMD                                                
*                                                                               
UCOM10   DS    0H                                                               
         MVC   UCOMKCLT,=X'FFFF'                                                
         GOTO1 HIGH                 GET TO NEXT COMMENT TYPE LETTER             
         CLC   KEY(3),KEYSAVE      COMMENT REC FOR SAME A/M?                    
         BNE   UCOMX                                                            
*                                                                               
         XC    KEY+4(L'KEY-4),KEY+4                                             
         MVC   UCOMKCLT,SBBCLT          ADD CLIENT TO THE KEY                   
         LA    R3,UCOMKPRD-UCOMHDR-1    LENGTH OF KEY                           
*                                                                               
         CLI   SBQBPRD,0                IS THERE PRODUCT?                       
         BE    UCOM20                                                           
         MVC   UCOMKPRD,SBQPRD          ADD PRODUCT TO KEY                      
         LA    R3,L'UCOMKPRD(R3)        ADJUST KEY LENGTH                       
*                                                                               
UCOM20   DS    0H                                                               
         SHI   R3,4                L'KEY FROM CLIENT UP                         
         GOTO1 HIGH        TRY COMMENT RECORD FOR THIS CLT+PRD                  
*                                                                               
* KEY IS CHECKED IN TWO STEPS, TO INCLUDE ALL POSSIBLE COMMENTS                 
*                                                                               
UCOM30   DS    0H                                                               
         CLC   KEY(3),KEYSAVE      COMMENT REC FOR SAME A/M?                    
         BNE   UCOMX                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY+4(0),KEYSAVE+4  KEY FROM CLIENT UP                           
         BNE   UCOM10              READHI UP TO NEXT COMM TYPE LETTER           
*                                                                               
         CLI   UCOMKEST,X'0'                                                    
         BNE   UCOM50              ESTIMATE PRESENT - PROCESS                   
*                                                                               
* NO ESTIMATE HERE, SEQ TO NEXT RECORD                                          
*                                                                               
UCOM40   DS    0H                                                               
         GOTO1 SEQ                 NEXT COMMENT RECORD                          
         B     UCOM30              CHECK THE KEY                                
*                                                                               
UCOM50   DS    0H                  FINDING 1-BYTE PRODUCT CODE                  
         LAY   R1,CLTREC           A(CLIENT RECORD)                             
         LA    R1,CLIST-CLTHDR(R1) A(PRODUCT TABLE)                             
*                                                                               
         CLC   UCOMKPRD(2),=2X'00' PRODUCT NUMBER?                              
         BNE   UCOM55                                                           
         LLC   RE,UCOMKPRD+2  INSERT PRODUCT NUMBER                             
         B     UCOM65                                                           
*                                                                               
UCOM55   DS    0H             HERE IT'S EITHER PRODUCT CODE OR GROUP            
         CLI   UCOMCTYP,C'U'                                                    
         BE    UCOM60         UCOMS CAN ONLY HAVE PRODUCT CODES, SO             
*                             FIND IT IN THE CLIST                              
         LHI   RE,X'FF'       NOT A UCOM - THIS IS PROD GROUP, USE              
         B     UCOM65         X'FF' AS PRODUCT NUMBER                           
*                                                                               
UCOM60   DS    0H                                                               
         CLI   0(R1),C' '                                                       
         BH    *+6                                                              
         DC    H'0'                PRODUCT NOT FOUND                            
*                                                                               
         CLC   UCOMKPRD,0(R1)      IS THIS OUR PRODUCT?                         
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     UCOM60                                                           
*                                                                               
         SR    RE,RE                                                            
         LLC   RE,3(R1)            GET 1-BYTE PRODUCT CODE                      
UCOM65   BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,UCOMKEST                                                    
         BZ    UCOM40              DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    UCOM40                                                           
*                                                                               
UCOM80   L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELUCOM,=P'1'                                                    
         B     UCOM40              NEXT SET                                     
*                                                                               
UCOMX    J     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DELETE ALL COMMENT RECORDS IN XSPOTDIR/FIL                                    
***********************************************************************         
         SPACE 1                                                                
XCOM     NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELXCOM,=P'0'                                                    
         XC    XKEY,XKEY                                                        
         LA    R2,XKEY                                                          
         USING XCOMKEY,R2                                                       
         MVI   COMI2KR,COMI2KRQ                                                 
         MVI   COMI2KS,COMI2KSQ                                                 
         MVC   COMI2KAM,SBBAGYMD                                                
*                                                                               
XCOM10   DS    0H                                                               
         MVC   COMI2KCL,=X'FFFF'                                                
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         CLC   XKEY(3),XKEYSAVE      COMMENT REC FOR SAME A/M?                  
         BNE   XCOMX                                                            
*                                                                               
         XC    XKEY+4(L'XKEY-4),XKEY+4                                          
         MVC   COMI2KCL,SBBCLT          ADD CLIENT TO THE KEY                   
         LA    R3,COMI2KPR-XCOMKEY-1    LENGTH OF KEY                           
*                                                                               
         CLI   SBQBPRD,0                IS THERE PRODUCT?                       
         BE    XCOM20                                                           
         MVC   COMI2KPR,SBQPRD          ADD PRODUCT TO KEY                      
         LA    R3,L'COMI2KPR(R3)        ADJUST KEY LENGTH                       
*                                                                               
XCOM20   DS    0H                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         SHI   R3,4                L'KEY FROM CLIENT UP                         
*                                                                               
* KEY IS CHECKED IN TWO STEPS, TO INCLUDE ALL POSSIBLE COMMENTS                 
*                                                                               
XCOM30   DS    0H                                                               
         CLC   XKEY(3),XKEYSAVE      COMMENT REC FOR SAME A/M?                  
         BNE   XCOMX                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   XKEY+4(0),XKEYSAVE+4  KEY FROM CLIENT UP                         
         BNE   XCOM10               READHI UP TO NEXT COMM TYPE LETTER          
*                                                                               
         CLI   COMI2KES,X'0'                                                    
         BNE   XCOM50              ESTIMATE PRESENT - PROCESS                   
*                                                                               
* NO ESTIMATE HERE, SEQ TO NEXT RECORD                                          
*                                                                               
XCOM40   DS    0H                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
         B     XCOM30              CHECK THE KEY                                
*                                                                               
XCOM50   DS    0H                  FINDING 1-BYTE PRODUCT CODE                  
         LAY   R1,CLTREC           A(CLIENT RECORD)                             
         LA    R1,CLIST-CLTHDR(R1) A(PRODUCT TABLE)                             
*                                                                               
         CLC   COMI2KPR(2),=2X'00' PRODUCT NUMBER?                              
         BNE   XCOM55                                                           
         LLC   RE,COMI2KPR+2                                                    
         B     XCOM65                                                           
XCOM55   DS    0H                                                               
         CLI   COMI2KPR+1,C'A'                                                  
         BNL   XCOM60              PRODUCT CODE, FIND IT IN CLIST               
         LHI   RE,X'FF'            OTHERWISE-PROD GROUP, USE X'FF'              
         B     XCOM65                                                           
*                                                                               
XCOM60   DS    0H                                                               
         CLI   0(R1),C' '                                                       
**NOP    BH    *+6                                                              
**NOP    DC    H'0'                PRODUCT NOT FOUND                            
         BNH   XCOM80              PRODUCT NOT FOUND - JUST DELETE IT           
*                                                                               
         CLC   COMI2KPR,0(R1)      IS THIS OUR PRODUCT?                         
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     XCOM60                                                           
*                                                                               
         SR    RE,RE                                                            
         LLC   RE,3(R1)            GET 1-BYTE PRODUCT CODE                      
XCOM65   BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,COMI2KES                                                    
         BZ    XCOM40              DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    XCOM40                                                           
*                                                                               
XCOM80   L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',XCOMDA,AIO,DMWORK             
         BRAS  RE,XDEL             AND DELETE                                   
         AP    DELXCOM,=P'1'                                                    
         B     XCOM40              NEXT SET                                     
*                                                                               
XCOMX    J     XIT                                                              
*                                                                               
***********************************************************************         
* DELETE XLK RECORDS  (XSP STA LOCKINS)                                         
***********************************************************************         
         SPACE 1                                                                
XLK      NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELXLK,=P'0'                                                     
         XC    XKEY,XKEY                                                        
         LA    R2,XKEY                                                          
         USING SSLKRECD,R2                                                      
         MVI   SSLKKTYP,SSLKKTYPQ                                               
         MVI   SSLKKSUB,SSLKKSUBQ                                               
         MVC   SSLKKAGMD,SBBAGYMD                                               
         MVC   SSLKKCLT,SBBCLT                                                  
         MVC   XKEYSAVE,XKEY                                                    
*                                                                               
XLK1     GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     XLK4                                                             
*                                                                               
XLK2     DS    0H                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
XLK4     CLC   XKEY(SSLKKMKT-SSLKKEY),XKEYSAVE                                  
         BNE   XLKX                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,SSLKKPRD        TEST ESTIMATE VALID FOR CLOSEOUT            
         BZ    XLK2                                                             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,SSLKKEST                                                    
         BZ    XLK2                DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    XLK2                                                             
*                                                                               
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',SSLKDDA,AIO,DMWORK            
*                                                                               
         BRAS  RE,XDEL             DELETE THE LAST RECORD                       
         AP    DELXLK,=P'1'                                                     
         B     XLK2                NEXT SET                                     
*                                                                               
XLKX     J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE AUTHORIZATION RECORDS                                                  
***********************************************************************         
         SPACE 1                                                                
AUT      NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELAUT,=P'0'                                                     
         XC    XKEY,XKEY                                                        
         LA    R2,XKEY                                                          
         USING AUTRECD,R2                                                       
         MVI   AUTKTYP,AUTKTYQQ                                                 
         MVI   AUTKSUB,AUTKSUBQ                                                 
         MVC   AUTKAM,SBBAGYMD                                                  
         MVC   AUTKCLT,SBBCLT                                                   
         MVC   XKEYSAVE,XKEY                                                    
*                                                                               
AUT1     GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     AUT4                                                             
*                                                                               
AUT2     DS    0H                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
AUT4     CLC   XKEY(AUTKPRD-AUTKEY),XKEYSAVE                                    
         BNE   AUTX                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,AUTKPRD        TEST ESTIMATE VALID FOR CLOSEOUT             
         BZ    AUT2                                                             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,AUTKEST                                                     
         BZ    AUT2                DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    AUT2                                                             
*                                                                               
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',AUTKDA,AIO,DMWORK             
*                                                                               
         BRAS  RE,XDEL             DELETE THE LAST RECORD                       
         AP    DELAUT,=P'1'                                                     
         B     AUT2                NEXT SET                                     
*                                                                               
AUTX     J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* XFILE RECORD DELETE ROUTINE                                         *         
***********************************************************************         
         SPACE 1                                                                
XDEL     NTR1  BASE=*,LABEL=*                                                   
         CLI   TEST,C'Y'           TEST MODE SKIPS WRITING TO FILE              
         BE    XDEL12                                                           
         MVC   XKEYSAVE,XKEY                                                    
*                                                                               
XDEL10   LA    R1,XKEY             THEN DELETE                                  
*                                                                               
         OI    32(R1),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR',XKEYSAVE,XKEY                  
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,SBAIO1                                                        
         OI    34(R1),X'C0'        X'C0' MARKS RECORD AS CLOSED OUT             
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',XKEY,SBAIO1,DMWORK            
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XDEL12   CLI   IOTRACE,C'Y'                                                     
         BNE   XDELX                                                            
         MVC   MYPRINT,BLANKS                                                   
         GOTO1 HEXOUT,DMCB,SBAIO1,MYPRINT,40,0                                  
         GOTO1 SBPRINT,(R1),MYPRINT-1,=C'BL01'                                  
         B     XDELX                                                            
*                                                                               
XDELX    J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE DARE RECORDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
DARE     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   DELDARE,=P'0'                                                    
         ZAP   DELDARC,=P'0'                                                    
         ZAP   DELDMG,=P'0'                                                     
         ZAP   DELDMGN,=P'0'                                                    
         ZAP   DELORH,=P'0'                                                     
         ZAP   DELDXMG,=P'0'                                                    
         ZAP   DELDXMGN,=P'0'                                                   
         ZAP   DELDMGRC,=P'0'                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING DAREORDD,R2                                                      
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,SBBAGYMD                                                 
         MVC   DCKCLT,SBBCLT                                                    
         GOTO1 HIGH                                                             
         B     DARE4                                                            
*                                                                               
DARE2    GOTO1 SEQ                                                              
*                                                                               
DARE4    CLC   KEY(DCKPRD-DOKEY),KEYSAVE                                        
         BNE   DAREX                                                            
*                                                                               
         SR    RE,RE               TEST ESTIMATE VALID FOR CLOSEOUT             
         ICM   RE,1,DCKPRD                                                      
         BZ    DARE2                                                            
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,DCKEST                                                      
         BZ    DARE2                                                            
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    DARE2                                                            
*                                                                               
         MVC   SVKEY,KEY           SAVE DARE KEY WHICH IS BY CLIENT             
         L     R1,SBAIO1                                                        
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         L     R2,AIO              DARE ORDER IN AIO1                           
         MVC   SVORDER,DOKORDER-DOKEY(R2)                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
DARE4D   USING DAREORDD,R3         ALSO CLOSEOUT DARE COMMENT RECORDS           
         MVC   KEY(L'DOKEY),0(R2)                                               
         MVI   DARE4D.DOKCMT,1     LOOK FOR OTHER RECORDS FIRST                 
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(DOKCMT-DOKEY),KEYSAVE  SAME ORDER NUMBER?                    
         BE    DARE5B10            YES                                          
         B     DARE8               NO, SET MKN KEY USING ORDER IN AIO1          
*                                                                               
DARE5A   GOTO1 SEQ                                                              
*                                                                               
DARE5B   CLC   KEY(DOKCMT-DOKEY),KEYSAVE     SAME ORDER NUMBER                  
         BNE   DARE5C                        NO, DONE WITH THE COMMENTS         
DARE5B10 L     R1,SBAIO1                                                        
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2                                                       
         AP    DELDARC,=P'1'                                                    
         B     DARE5A                                                           
         DROP  DARE4D                                                           
*                                                                               
DARE5C   MVC   KEY,SVKEY           GET BACK OUR RECORD                          
         LA    R2,KEY                                                           
         USING DAREORDD,R2                                                      
         GOTO1 HIGH                                                             
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 GETREC              NEED TO GET ORDER BACK FOR BYR CODE          
         L     R2,AIO                                                           
*                                                                               
DARE8    XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING DAREMGND,R3         ALSO CLOSEOUT DARE MAKE GOODS NOTICE         
         MVI   MNKTYPE,MNKTYPQ     0D                                           
         MVI   MNKSUBTY,MNKSTYPQ   36                                           
         MVC   MNKAGMD,DOKAGMD     AGY/MD                                       
         MVC   MNKORDER,DOKORDER   ORDER NUMBER                                 
         LA    R6,DORFRST                                                       
         USING DOIDELD,R6                                                       
         MVC   MNKBYR,DOIDBYR      BUYER ID                                     
         DROP  R6                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(10),KEYSAVE     SAME ORDER NUMBER, BUYER CODE?               
         BE    DARE8B10                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING DAREMGOD,R3         ALSO CLOSEOUT DARE MAKE GOODS                
         MVI   MOKTYPE,MOKTYPQ     NO, SET MKO KEY USING ORDER IN AIO1          
         MVI   MOKSUBTY,MOKSTYPQ   X'0D37'                                      
         MVC   MOKAGMD,DOKAGMD                                                  
         MVC   MOKORDER,DOKORDER   ORDER NUMBER                                 
         B     DARE9A                                                           
*                                                                               
DARE8A   GOTO1 SEQ                                                              
*                                                                               
DARE8B   CLC   KEY(10),KEYSAVE     SAME ORDER NUMBER, BUYER CODE?               
         BNE   DARE9               DONE? CLOSE THE DARE RECORD                  
DARE8B10 L     R1,SBAIO1                                                        
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2                                                       
         AP    DELDMGN,=P'1'                                                    
         B     DARE8A                                                           
         DROP  R2                                                               
         DROP  R3                                                               
*                                                                               
DARE9    XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING DAREMGOD,R3         ALSO CLOSEOUT DARE MAKE GOODS                
         L     R2,AIO                                                           
         USING DAREMGND,R2                                                      
*                                                                               
         MVI   MOKTYPE,MOKTYPQ     0D                                           
         MVI   MOKSUBTY,MOKSTYPQ   37                                           
         MVC   MOKAGMD,MNKAGMD     AGY/MD                                       
         MVC   MOKORDER,MNKORDER   ORDER NUMBER                                 
         DROP  R2,R3                                                            
DARE9A   GOTO1 HIGH                                                             
         B     DARE9C                                                           
*                                                                               
DARE9B   GOTO1 SEQ                                                              
*                                                                               
DARE9C   CLC   KEY(7),KEYSAVE      SAME ORDER NUMBER?                           
         BNE   DARE20              DONE? CLOSE DARE MAKE GOOD NOTICE            
         L     R1,SBAIO1                                                        
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2                                                       
         AP    DELDMG,=P'1'                                                     
         B     DARE9B                                                           
*                                                                               
DARE20   XC    XKEY,XKEY           CLOSE OUT DARE MAKEGOOD NOTICE RECS          
         LA    R2,XKEY                                                          
         USING MNXKEY,R2           DARE MAKEGOOD NOTICE DSECT                   
         MVI   MNXKTYPE,MNXKTYPQ   X'0D'                                        
         MVI   MNXKSBTY,MNXKSBTQ   X'36'                                        
         MVC   MNXKAGMD,SBBAGYMD   AGY/MD                                       
         MVC   MNXKORDR,SVORDER    ORDER NUMBER                                 
         MVC   XKEYSAVE,XKEY       SAVE OFF THE KEY                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     DARE20B                                                          
*                                                                               
DARE20A  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
DARE20B  CLC   XKEY(MNXKGRP-MNXKEY),XKEYSAVE                                    
         BNE   DARE21                                                           
*                                                                               
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',MNXKDA,AIO,DMWORK             
*                                                                               
         BRAS  RE,XDEL             DELETE THE RECORD                            
         AP    DELDXMGN,=P'1'                                                   
         B     DARE20A             NEXT SET                                     
         DROP  R2                  DROP USING                                   
*                                                                               
DARE21   XC    XKEY,XKEY           CLOSE OUT DARE MAKEGOOD OFFER RECS           
         LA    R2,XKEY                                                          
         USING MOXKEY,R2           DARE MAKEGOOD OFFER DSECT                    
         MVI   MOXKTYPE,MOXKTYPQ   X'0D'                                        
         MVI   MOXKSBTY,MOXKSBTQ   X'37'                                        
         MVC   MOXKAGMD,SBBAGYMD   AGY/MD                                       
         MVC   MOXKORDR,SVORDER    ORDER NUMBER                                 
         MVC   XKEYSAVE,XKEY       SAVE OFF THE KEY                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     DARE21B                                                          
*                                                                               
DARE21A  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
DARE21B  CLC   XKEY(MOXKMGCD-MOXKEY),XKEYSAVE                                   
         BNE   DARE22                                                           
*                                                                               
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',MOXKDA,AIO,DMWORK             
*                                                                               
         BRAS  RE,XDEL             DELETE THE RECORD                            
         AP    DELDXMG,=P'1'                                                    
         B     DARE21A             NEXT SET                                     
         DROP  R2                  DROP USING                                   
*                                                                               
DARE22   XC    XKEY,XKEY           CLOSE OUT MAKEGOOD REJECTED COMMENT          
         LA    R2,XKEY                                                          
         USING DAREMGCD,R2         DARE MAKEGOOD REJECTED COMMENT               
         MVI   MCXKTYPE,MCXKTYPQ   X'0D'                                        
         MVI   MCXKSBTY,MCXKSBTQ   X'3B'                                        
         MVC   MCXKAGMD,SBBAGYMD   AGY/MD                                       
         MVC   MCXKORDR,SVORDER    ORDER NUMBER                                 
         MVC   XKEYSAVE,XKEY       SAVE OFF THE KEY                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     DARE22B                                                          
*                                                                               
DARE22A  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
DARE22B  CLC   XKEY(MCXKGRP-MCXKEY),XKEYSAVE                                    
         BNE   DARE30                                                           
*                                                                               
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',MCXKDA,AIO,DMWORK             
*                                                                               
         BRAS  RE,XDEL             DELETE THE RECORD                            
         AP    DELDMGRC,=P'1'                                                   
         B     DARE22A             NEXT SET                                     
         DROP  R2                  DROP USING                                   
*                                                                               
DARE30   XC    XKEY,XKEY           CLOSE OUT OM HISTORY RECS                    
         LA    R2,XKEY                                                          
         USING OHISRECD,R2                                                      
         MVI   OHISTYP,OHISTYQ                                                  
         MVI   OHISSTYP,OHISSTYQ                                                
         MVC   OHISORD,SVORDER                                                  
         MVC   OHISBKAM,SBBAGYMD                                                
         MVC   OHISBKCL,SBBCLT                                                  
         LA    R3,OHISBKPR-OHISRECD-1                                           
*                                                                               
DARE40   MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     DARE60                                                           
*                                                                               
DARE50   MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
DARE60   EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   XKEY(0),XKEYSAVE                                                 
         BNE   DARE70                                                           
*                                                                               
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',OHISDDA,AIO,DMWORK            
*                                                                               
         BRAS  RE,XDEL             DELETE THE RECORD                            
         AP    DELORH,=P'1'                                                     
         B     DARE50              NEXT SET                                     
*                                                                               
DARE70   MVC   KEY,SVKEY                                                        
         LA    R2,KEY                                                           
         USING DAREORDD,R2                                                      
         GOTO1 HIGH                                                             
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         L     R1,SBAIO1           DARE RECORD                                  
         MVC   KEY(13),0(R1)       DELETE THE ACTIVE KEY!                       
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELDARE,=P'1'                                                    
         MVC   KEY,SVKEY           LAST PASSIVE KEY WE READ                     
         GOTO1 HIGH                                                             
         B     DARE2               NEXT RECORD                                  
*                                                                               
DAREX    J     XIT                                                              
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE DARE BATCH RECORDS                                           *         
***********************************************************************         
         SPACE 1                                                                
DAREBTCH NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   DELDARBT,=P'0'      CLEAR DELETED COUNTER                        
         MVC   AIO,SBAIO1          A(IO AREA)                                   
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R2,KEY              R2 = KEY                                     
         USING DARBTCHD,R2         DARE BATCH RECORD DSECT                      
         MVI   DBTKTYP,DBTKTYPQ    X'0D'                                        
         MVI   DBTKSTYP,DBTKSTYQ   X'35'                                        
         MVC   DBTKAGMD,SBBAGYMD   AGENCY/MEDIA                                 
*                                                                               
DRBT00   GOTO1 HIGH                READ HIGH                                    
         B     DRBT10              GO TEST KEY                                  
*                                                                               
DRBT05   GOTO1 SEQ                 READ SEQ                                     
*                                                                               
DRBT10   CLC   KEY(3),KEYSAVE      X'0D35' A/M?                                 
         BNE   DRBTX               NO - DONE                                    
*                                                                               
         CLC   DBTKCLT,SBBCLT      MATCH ON CLIENT?                             
         BE    DRBT15              YES                                          
         MVC   DBTKPRD(3),XFF      BUMP TO NEXT CLIENT                          
         B     DRBT00              GO READ HIGH                                 
*                                                                               
DRBT15   SR    RE,RE               TEST ESTIMATE VALID FOR CLOSEOUT             
         ICM   RE,1,DBTKPRD        HAVE PRODUCT?                                
         BZ    DRBT05              NO - READ SEQ                                
         BCTR  RE,0                -1                                           
         SLL   RE,8                X 256                                        
         SR    RF,RF               CLEAR RF                                     
         ICM   RF,1,DBTKEST        HAVE ESTIMATE?                               
         BZ    DRBT05              NO - READ SEQ                                
         BCTR  RF,0                -1                                           
         AR    RE,RF               ADD PRD/EST INDEXES                          
         L     R1,SBAESTTB         A(ESTIMATE TABLE)                            
         AR    RE,R1               ADD INDEX                                    
         CLI   0(RE),0             ESTIMATE VALID FOR CLOSEOUT?                 
         BE    DRBT05              NO - READ SEQ                                
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELDARBT,=P'1'      BUMP DELETED RECORD COUNTER                  
         B     DRBT05              NEXT RECORD                                  
*                                                                               
DRBTX    J     XIT                 EXIT                                         
         DROP  R2                  DROP R2                                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE DARE FLIGHT RECORDS                                          *         
***********************************************************************         
         SPACE 1                                                                
DAREFLT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   DELDARFL,=P'0'      CLEAR DELETED COUNTER                        
         MVC   AIO,SBAIO1          A(IO AREA)                                   
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R2,KEY              R2 = KEY                                     
         USING DFLRECD,R2          DARE BATCH RECORD DSECT                      
         MVI   DFLKTYP,DFLKTYPQ    X'0D'                                        
         MVI   DFLKSUB,DFLKSUBQ    X'38'                                        
         MVC   DFLKAGMD,SBBAGYMD   AGENCY/MEDIA                                 
         MVC   DFLKCLT,SBBCLT      CLIENT                                       
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         B     DRFL10              GO TEST KEY                                  
*                                                                               
DRFL05   GOTO1 SEQ                 READ SEQ                                     
*                                                                               
DRFL10   CLC   KEY(5),KEYSAVE      X'0D38' A/M CLIENT?                          
         BNE   DRFLX               NO - DONE                                    
*                                                                               
         LAY   R1,CLTREC           A(CLIENT RECORD)                             
         LA    R1,CLIST-CLTHDR(R1) A(PRODUCT TABLE)                             
*                                                                               
DRFL15   CLI   0(R1),C' '          END OF PRODUCT TABLE?                        
         BNH   DRFL05              YES - READ SEQ                               
*                                                                               
         CLC   DFLKPRD,0(R1)       MATCH ON PRODUCT?                            
         BE    *+12                YES                                          
         LA    R1,4(R1)            NO - BUMP TO NEXT CLIST ENTRY                
         B     DRFL15              CHECK NEXT PRODUCT                           
*                                                                               
         LLC   RE,3(R1)            BINARY PRODUCT?                              
         BCTR  RE,0                -1                                           
         SLL   RE,8                X 256                                        
         SR    RF,RF               CLEAR RF                                     
         ICM   RF,1,DFLKEST        HAVE ESTIMATE?                               
         BZ    DRFL05              NO - READ SEQ                                
         BCTR  RF,0                -1                                           
         AR    RE,RF               ADD PRD/EST INDEXES                          
         L     R1,SBAESTTB         A(ESTIMATE TABLE)                            
         AR    RE,R1               ADD INDEX                                    
         CLI   0(RE),0             ESTIMATE VALID FOR CLOSEOUT?                 
         BE    DRFL05              NO - READ SEQ                                
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELDARFL,=P'1'      BUMP DELETED RECORD COUNTER                  
         B     DRFL05              NEXT RECORD                                  
*                                                                               
DRFLX    J     XIT                 EXIT                                         
         DROP  R2                  DROP R2                                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE PW RECORDS                                                   *         
***********************************************************************         
         SPACE 1                                                                
PW       NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELPW,=P'0'                                                      
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PWRECD,R2                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,SBBAGYMD                                                 
         MVC   PWKCLT,SBBCLT                                                    
         LA    R3,PWKPRD-PWFKEY-1                                               
         CLI   SBQBPRD,0                                                        
         BE    PW1                                                              
         MVC   PWKPRD,SBQPRD                                                    
         LA    R3,L'PWKPRD(R3)                                                  
         MVC   PWKEST,SBQEST                                                    
*                                                                               
PW1      GOTO1 HIGH                                                             
         B     PW4                                                              
*                                                                               
PW2      GOTO1 SEQ                                                              
*                                                                               
PW4      EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   PWX                                                              
*                                                                               
         LLC   RE,PWKPRD           TEST ESTIMATE VALID FOR CLOSEOUT             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,PWKEST                                                      
         BZ    PW2                 DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    PW2                                                              
*                                                                               
PW8      L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELPW,=P'1'                                                      
         B     PW2                 NEXT RECORD                                  
*                                                                               
PWX      J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE BILL FORMULA RECORDS                                         *         
***********************************************************************         
         SPACE 1                                                                
BFORM    NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELBFORM,=P'0'                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING BFREC,R2                                                         
         MVI   BFKTYPE,BFKTYPEQ                                                 
         MVI   BFKSTYPE,BFKSTYPQ                                                
         MVC   BFKAGYMD,SBBAGYMD                                                
         MVC   BFKCLT,SBBCLT                                                    
         LHI   R3,BFKPRD-BFKEY-1                                                
         CLI   SBQBPRD,0                                                        
         BE    BF1                                                              
         MVC   BFKPRD,SBQPRD                                                    
         AHI   R3,L'BFKPRD                                                      
         MVC   BFKEST,SBQEST                                                    
*                                                                               
BF1      GOTO1 HIGH                                                             
         B     BF4                                                              
*                                                                               
BF2      GOTO1 SEQ                                                              
*                                                                               
BF4      EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   BFX                                                              
         CLC   BFKPRD,=C'AAA'                                                   
         BE    BF2                 NEVER DELETE 'ALL' PRD RECS                  
*                                                                               
         LAY   R1,CLTREC           FIND PRODUCT CODE                            
         LA    R1,CLIST-CLTHDR(R1)                                              
*                                                                               
BF6      CLI   0(R1),C' '                                                       
         BNH   BF8                                                              
         CLC   BFKPRD,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     BF6                                                              
*                                                                               
         LLC   RE,3(R1)            TEST ESTIMATE VALID FOR CLOSEOUT             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,BFKEST                                                      
         BZ    BF2                 DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    BF2                                                              
*                                                                               
BF8      L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELBFORM,=P'1'                                                   
         B     BF2                 NEXT RECORD                                  
*                                                                               
BFX      J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE SPLIT BILL RECORDS                                           *         
***********************************************************************         
         SPACE 1                                                                
SPBIL    NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELSPBL,=P'0'                                                    
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING SPBRECD,R2                                                       
         MVI   SPBKTYP,SPBKTYPQ                                                 
         MVI   SPBKSUB,SPBKSUBQ                                                 
         MVC   SPBKAGMD,SBBAGYMD                                                
         MVC   SPBKCLT,SBBCLT                                                   
         LHI   R3,SPBKPRD-SPBKEY-1                                              
         CLI   SBQBPRD,0                                                        
         BE    SPB1                                                             
         MVC   SPBKPRD,SBQPRD                                                   
         AHI   R3,L'SPBKPRD                                                     
         MVC   SPBKEST,SBQEST                                                   
*                                                                               
SPB1     GOTO1 HIGH                                                             
         B     SPB4                                                             
*                                                                               
SPB2     GOTO1 SEQ                                                              
*                                                                               
SPB4     EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   SPBX                                                             
*                                                                               
         LAY   R1,CLTREC           FIND PRODUCT CODE                            
         LA    R1,CLIST-CLTHDR(R1)                                              
*                                                                               
SPB6     CLI   0(R1),C' '                                                       
         BNH   SPB8                                                             
         CLC   SPBKPRD,0(R1)                                                    
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     SPB6                                                             
*                                                                               
         LLC   RE,3(R1)            TEST ESTIMATE VALID FOR CLOSEOUT             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,SPBKEST                                                     
         BZ    SPB2                DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    SPB2                                                             
*                                                                               
SPB8     L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELSPBL,=P'1'                                                    
         B     SPB2                NEXT RECORD                                  
*                                                                               
SPBX     J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE SPLIT RECORDS                                                *         
***********************************************************************         
         SPACE 1                                                                
SPLIT    NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELSPLIT,=P'0'      INIT DELETED SPLIT RECORD COUNTER            
         MVC   AIO,SBAIO1          USE AIO1                                     
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R2,KEY              R2 = KEY                                     
         USING SPLTRECD,R2         SPLIT RECORD DSECT                           
         MVC   SPLTKTYP,=X'0D40'   X'0D40'                                      
         MVC   SPLTKAM,SBBAGYMD    A/M                                          
         MVC   SPLTKCLT,SBBCLT     CLIENT                                       
         LA    R3,4                X'0D40' + A/M + CLIENT - 1 FOR EX            
         CLI   SBQBPRD,0           HAVE A REQUESTED PRODUCT?                    
         BE    SP1                 NO                                           
         AHI   R3,3                ALSO TEST KEY FOR PRODUCT                    
         MVC   SPLTKPRD,SBQPRD     PRODUCT                                      
         MVC   SPLTKEST,SBQEST     ESTIMATE START                               
*                                                                               
SP1      GOTO1 HIGH                READ HIGH                                    
         B     SP4                 GO TEST KEY                                  
*                                                                               
SP2      GOTO1 SEQ                 READ SEQ                                     
*                                                                               
SP4      EX    R3,*+8              KEY MATCHES CRITERIA?                        
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE      ** EXECUTED **                               
         BNE   SPX                 NO - DONE                                    
*                                                                               
         LAY   R1,CLTREC           FIND PRODUCT CODE                            
         LA    R1,CLIST-CLTHDR(R1) R1 = CLIST                                   
*                                                                               
SP6      CLI   0(R1),C' '          PRODUCT FOUND?                               
         BNH   SP8                 NO - MUST BE OLD - DELETE!                   
         CLC   SPLTKPRD,0(R1)      PRODUCT MATCHES CLIST ENTRY?                 
         BE    *+12                YES                                          
         LA    R1,4(R1)            NO - BUMP TO NEXT CLIST ENTRY                
         B     SP6                 AND TRY AGAIN                                
*                                                                               
         LLC   RE,3(R1)            BINARY PRODUCT                               
         BCTR  RE,0                -1 FOR INDEXING                              
         SLL   RE,8                X 246                                        
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,1,SPLTKEST       ESTIMATE 0?                                  
         BZ    SP2                 NO                                           
         BCTR  RF,0                -1 FOR INDEXING                              
         AR    RE,RF               ADD ESTIMATE INDEX TO PRODUCT INDEX          
         L     R1,SBAESTTB         ESTIMATE TABLE                               
         AR    RE,R1               INDEX INTO ESTIMATE TABLE?                   
         CLI   0(RE),0             ESTIMATE ELIGIBLE FOR CLOSEOUT?              
         BE    SP2                 NO - READ SEQ                                
*                                                                               
SP8      GOTO1 GETREC              YES - GET THE RECORD                         
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELSPLIT,=P'1'      BUMP DELETED SPLIT RECORD COUNTER            
         B     SP2                 AND READ SEQ                                 
*                                                                               
SPX      J     XIT                 EXIT                                         
         DROP  R2                                                               
***********************************************************************         
* DELETE PG ESTIMATE RECORDS                                          *         
***********************************************************************         
         SPACE 1                                                                
PGEST    NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELPGEST,=P'0'                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PGESTD,R2                                                        
         MVI   PGKRID,PGKNDIRQ     READ PG EST KEYS                             
         MVI   PGKSID,PGKNDISQ                                                  
         MVC   PGKAM,SBBAGYMD                                                   
         MVC   PGKCLT,SBBCLT                                                    
         LA    R3,PGKPRD-PGESTD-1                                               
         CLI   SBQBPRD,0                                                        
         BE    PGEST1                                                           
         MVC   PGKPRD,SBQPRD                                                    
         LA    R3,L'PGKPRD(R3)                                                  
         MVC   PGKEST,SBQEST                                                    
*                                                                               
PGEST1   GOTO1 HIGH                                                             
         B     PGEST4                                                           
*                                                                               
PGEST2   GOTO1 SEQ                                                              
*                                                                               
PGEST4   EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   PGESTX                                                           
         LAY   R1,CLTREC           FIND PRODUCT CODE                            
         LA    R1,CLIST-CLTHDR(R1)                                              
*                                                                               
PGEST6   CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         B     PGEST8                                                           
*                                                                               
         CLC   PGKPRD,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     PGEST6                                                           
         LLC   RE,3(R1)            TEST ESTIMATE VALID FOR CLOSEOUT             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,PGKEST                                                      
         BZ    PGEST2              DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    PGEST2                                                           
PGEST8   L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELPGEST,=P'1'                                                   
         B     PGEST2              NEXT RECORD                                  
*                                                                               
PGESTX   J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE CHILD SPOT RECORDS                                           *         
***********************************************************************         
         SPACE 1                                                                
CHILD    NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELCHILD,=P'0'                                                   
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CSORECD,R2                                                       
         MVI   CSOKTYPE,CSOKTYPQ   PROGRAM RECORD KEY                           
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,SBBAGYMD                                                  
         MVC   CSOKCLT,SBBCLT                                                   
         LA    R3,CSOKMKT-CSOKEY-1                                              
         LA    R4,CSOKEST                                                       
         BRAS  RE,CSDEL            DELETE ALL PROGRAM RECORDS                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING COMRECD,R2                                                       
         MVI   COMKTYPE,COMKTYPQ   HEAD/FOOTLINE COMMENTS                       
         MVI   COMKSTYP,COMKSTPQ                                                
         MVC   COMKAM,SBBAGYMD                                                  
         MVC   COMKCLT,SBBCLT                                                   
         LA    R3,COMKMKT-COMKEY-1                                              
         LA    R4,COMKEST                                                       
         BRAS  RE,CSDEL                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING DEMRECD,R2                                                       
         MVI   DEMKTYPE,DEMKTYPQ   DEMO OVERRIDE RECORDS                        
         MVI   DEMKSTYP,DEMKSTPQ                                                
         MVC   DEMKAM,SBBAGYMD                                                  
         MVC   DEMKCLT,SBBCLT                                                   
         LA    R3,DEMKMKT-DEMKEY-1                                              
         LA    R4,DEMKEST                                                       
         BRAS  RE,CSDEL                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING NTPRECD,R2                                                       
         MVI   NTPKTYPE,NTPKTYPQ   NTPCALC RECORDS                              
         MVI   NTPKSTYP,NTPKSTPQ                                                
         MVC   NTPKAM,SBBAGYMD                                                  
         MVC   NTPKCLT,SBBCLT                                                   
         LA    R3,NTPKMKT-NTPKEY-1                                              
         LA    R4,NTPKEST                                                       
         BRAS  RE,CSDEL                                                         
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R2,KEY              R2 = KEY                                     
         USING MASRECD,R2          CHILD SPOT MASTER ESTIMATE LIST              
         MVI   MASKTYPE,MASKTYPQ   X'0D'                                        
         MVI   MASKSTYP,MASKSTPQ   X'0F'                                        
         MVC   MASKAM,SBBAGYMD     A/M                                          
         MVC   MASKCLT,SBBCLT      CLIENT                                       
         MVC   AIO,SBAIO1          USE SBAIO1                                   
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         B     CHILD10             CHECK KEY                                    
*                                                                               
CHILD05  GOTO1 SEQ                 READ SEQ                                     
*                                                                               
CHILD10  CLC   KEY(5),KEYSAVE      SAME X'0D6F' A/M CLIENT?                     
         BNE   CHILDX              NO - DONE                                    
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R2,AIO              A(MASTER ESTIMATE LIST RECORD)               
         LA    R1,MELLIST          MASTER LIST ENTRIES                          
         USING MESTLSTD,R1         DSECT TO COVER MASTER ESTIMATE LIST          
         LA    R3,ELEM             BUILD NEW X'01' ELEMENT HERE                 
         XC    ELEM,ELEM           CLEAR THE ELEMENT                            
         LA    R4,DUB              LIST OF 7 ESTIMATES TO CHECK IN DUB          
         LA    R6,28               28 MASTER ESTIMATE ENTRIES                   
         L     R2,SBAESTTB         A(ESTIMATE TABLE)                            
         XR    RF,RF               CLEAR RF                                     
*                                                                               
CHILD12  CLI   MESTNUM,0           HAVE ANY MORE MASTER LIST ENTRIES?           
         BE    CHILD25             NO - DONE CHECKING ESTIMATES                 
         MVC   DUB(1),MESTNUM      MASTER ESTIMATE                              
         MVC   DUB+1(6),MESTSUBS   SUB-ESTIMATE LIST                            
         LA    R5,7                FOR BCT LOOP                                 
*                                                                               
CHILD15  LA    RE,254              CHECK FOR POL ESTIMATE OPEN                  
         SLL   RE,8                X 256                                        
         ICM   RF,1,0(R4)          HAVE AN ESTIMATE NUMBER?                     
         BZ    CHILD16             NO - LIST IS DONE - ALL EST PASSED           
         BCTR  RF,0                -1 FOR INDEXING                              
         AR    RE,RF               PRODUCT + ESTIMATE                           
         AR    RE,R2               INDEX INTO ESTIMATE TABLE                    
         CLI   0(RE),0             ESTIMATE ELIGABLE FOR CLOSEOUT?              
         BNE   CHILD20             YES - DON'T COPY THESE TO ELEM               
         LA    R4,1(R4)            BUMP TO NEXT ESTIMATE                        
         BCT   R5,CHILD15          LOOP BACK AND TEST NEXT ESTIMATE             
*                                                                               
CHILD16  MVC   0(9,R3),MESTNUM     COPY MASTER/SUB-ESTIMATE LIST                
         LA    R3,MESTLSTL(R3)     BUMP EST LIST IN ELEM                        
CHILD20  LA    R1,MESTLSTL(R1)     BUMP EST LIST IN RECORD                      
         BCT   R6,CHILD12          LOOP BACK AND PROCESS NEXT EST LIST          
         DROP  R1                                                               
*                                                                               
CHILD25  L     R2,AIO              A(MASTER ESTIMATE LIST RECORD)               
         LA    R1,MELLIST          MASTER LIST ENTRIES                          
         CLC   MELLIST,ELEM        ANY ESTIMATE LIST ENTRIES DELETED?           
         BE    CHILD05             NO - LEAVE RECORD AS IS & READ SEQ           
         MVC   MELLIST,ELEM        REPLACE THE X'01' ESTIMATE LISTS             
         OC    MELLIST,MELLIST     ANY MASTER ESTIMATE LISTS LEFT?              
         BNZ   CHILD30             YES                                          
         OI    15(R2),X'C0'        NO - JUST MARK IT AS CLOSED OUT              
         OI    KEY+13,X'C0'        MARK THE KEY AS CLOSED OUT AS WELL           
         AP    DELCHILD,=P'1'      ADD 1 TO DELETED CHILD SPOT COUNTER          
CHILD30  CLI   TEST,C'Y'           TEST MODE                                    
         BE    CHILD05             YES - DONT WRITE TO FILE & READ SEQ          
         GOTO1 PUTREC              WRITE THE RECORD BACK                        
         TM    KEY+13,X'C0'        IS THE KEY MARKED FOR CLOSEOUT?              
         BZ    CHILD05             NO                                           
         GOTO1 WRITE               YES - WRITE THE KEY BACK                     
         B     CHILD05             GO READ SEQ                                  
         DROP  R2                                                               
*                                                                               
CHILDX   J     XIT                 EXIT                                         
*                                                                               
         SPACE 2                                                                
CSDEL    NTR1  BASE=*,LABEL=*      ROUTINE TO DELETE A SET OF                   
         GOTO1 HIGH                CHILD SPOT RECORDS                           
         B     CSDEL4              R3=LENGTH FOR EXECUTED KEY COMPARE           
*                                  R4=A(ESTIMATE IN KEY)                        
CSDEL2   GOTO1 SEQ                                                              
*                                                                               
CSDEL4   EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   CSDELX                                                           
         L     R1,SBAESTTB         TEST THIS ESTIMATE IS ELIBIBLE FOR           
         LA    RE,254              CLOSEOUT UNDER PRODUCT POL                   
         SLL   RE,8                                                             
         LLC   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         LA    RE,0(RE,R1)                                                      
         CLI   0(RE),0                                                          
         BE    CSDEL2                                                           
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELCHILD,=P'1'                                                   
         B     CSDEL2              NEXT RECORD                                  
*                                                                               
CSDELX   J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE SP UPLOAD RECORDS                                            *         
***********************************************************************         
         SPACE 1                                                                
UPL      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   DELUPL,=P'0'                                                     
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING SPUPREC,R2                                                       
         MVC   SPUPTYPE,=X'0D78'                                                
         MVC   SPUPAM,SBBAGYMD                                                  
         MVC   SPUPCLT,SBBCLT                                                   
         GOTO1 HIGH                                                             
         B     UPL4                                                             
*                                                                               
UPL2     GOTO1 SEQ                                                              
*                                                                               
UPL4     CLC   KEY(SPUPPRD-SPUPKEY),KEYSAVE                                     
         BNE   UPLX                                                             
*                                                                               
         SR    RE,RE               TEST ESTIMATE VALID FOR CLOSEOUT             
         ICM   RE,1,SPUPPRD                                                     
         BZ    UPL2                                                             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,SPUPEST                                                     
         BZ    UPL2                                                             
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    UPL2                                                             
*                                                                               
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
UPL9     GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   7(R6),X'FF'         LAST RECORD OF THE SET?                      
         BE    UPL10                                                            
         BRAS  RE,DELETE2                                                       
         GOTO1 SEQ                                                              
         B     UPL9                                                             
*                                                                               
UPL10    BRAS  RE,DELETE2          DELETE THE LAST RECORD                       
         AP    DELUPL,=P'1'                                                     
         B     UPL2                NEXT SET                                     
*                                                                               
UPLX     J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE NETDEF/CBLPRO RECORDS (CANADA ONLY)                          *         
***********************************************************************         
         SPACE 1                                                                
NDEF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   DELNETD,=P'0'                                                    
         ZAP   DELCBLP,=P'0'                                                    
         CLI   CANADA,C'Y'         CANADA HAS THESE EXTRA RECORD TYPES          
         BNE   NDEFX                                                            
         CLI   SBQBPRD,0           TEST SINGLE PRODUCT REQUEST                  
         BNE   NDEFX               YES - LEAVE THESE EST SPECIFIC RECS          
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING NDEFRECD,R2                                                      
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,SBAGY                                                   
         GOTO1 HIGH                                                             
         B     NDEF4                                                            
*                                                                               
NDEF2    GOTO1 SEQ                                                              
*                                                                               
NDEF4    CLC   KEY(NDEFKNET-NDEFRECD),KEYSAVE                                   
         BNE   NDEFX                                                            
         CLC   NDEFKCLT,SBBCLT     CLIENT BEING CLOSED OUT?                     
         BNE   NDEF2               NO                                           
         CLI   NDEFKEST,0          ESTIMATE SPECIFIC RECORD?                    
         BE    NDEF2               NO - DON'T DELETE ESTIMATE ZERO              
         LA    RE,254              CHECK FOR POL ESTIMATE OPEN                  
         SLL   RE,8                X 256                                        
         LLC   RF,NDEFKEST         NETDEF ESTIMATE                              
         BCTR  RF,0                MINUS ONE FOR INDEXING                       
         AR    RE,RF               INDEXED ESTIMATE IN TABLE                    
         L     R1,SBAESTTB         ESTIMATE TABLE                               
         AR    RE,R1               ADD THE PRD/EST INDEX                        
         CLI   0(RE),0             ESTIMATE VALID FOR CLOSEOUT?                 
         BE    NDEF2               NO-IGNORE THIS RECORD                        
*                                                                               
         MVC   AIO,SBAIO1          A(RECORD AREA)                               
         GOTO1 GETREC              GET THE RECORD                               
         BRAS  RE,DELETE2          AND DELETE                                   
*                                                                               
         L     R1,SBAIO1           READDRESS THE RECORD                         
         LA    R3,NDEFEL-NDEFRECD(R1)                                           
         SR    R0,R0                                                            
NDEF6    CLI   0(R3),0                                                          
         BE    NDEF8               ASSUME NETDEF                                
         CLI   0(R3),NDEFNELQ                                                   
         BE    NDEF7                                                            
         IC    R0,1(R3)                                                         
         AR    R3,0                                                             
         B     NDEF6                                                            
NDEF7    CLI   2(R3),NDEFCABQ      CABLE RECORD ?                               
         BNE   NDEF8                                                            
         AP    DELCBLP,=P'1'                                                    
         B     *+10                                                             
NDEF8    AP    DELNETD,=P'1'                                                    
         B     NDEF2               NEXT RECORD                                  
*                                                                               
NDEFX    J     XIT                                                              
*                                                                               
***********************************************************************         
* DELETE SPOT BHOLD RECORDS                                                     
***********************************************************************         
         SPACE 1                                                                
SBHOLD   NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELSBH,=P'0'                                                     
         XC    XKEY,XKEY                                                        
         LA    R2,XKEY                                                          
         USING BHLDRECD,R2                                                      
         MVI   BHLDKSYS,BHLDKSYQ                                                
         MVI   BHLDKSTP,BHLDKSTQ                                                
         MVC   BHLDKAM,SBBAGYMD                                                 
         MVC   BHLDKCLI,SBBCLT                                                  
         MVC   XKEYSAVE,XKEY                                                    
*                                                                               
SBH1     GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     SBH4                                                             
*                                                                               
SBH2     DS    0H                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
SBH4     CLC   XKEY(BHLDKPRD-BHLDKEY),XKEYSAVE                                  
         BNE   SBHX                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,BHLDKPRD       TEST ESTIMATE VALID FOR CLOSEOUT             
         BZ    SBH2                                                             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,BHLDKEST                                                    
         BZ    SBH2                DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    SBH2                                                             
*                                                                               
         L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',BHLDDA,AIO,DMWORK             
*                                                                               
         BRAS  RE,XDEL             DELETE THE LAST RECORD                       
         AP    DELSBH,=P'1'                                                     
         B     SBH2                NEXT SET                                     
*                                                                               
SBHX     J     XIT                                                              
*                                                                               
***********************************************************************         
* DELETE SPOT DESKTOP REVLINE/AVAIL/PROPOSAL RECORDS                            
*                     REVLINE  = X'0E10' DRVKTYPQ / DRVKSUBQ                    
*                     AVAIL    = X'0E11' DRVKTYPQ / DWKKSUBQ                    
*                     PROPOSAL = X'0E12' DRVKTYPQ / DPRKSUBQ                    
***********************************************************************         
         SPACE 1                                                                
SDREV    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,DELDREVL         START WITH REVLINES                          
         LA    R4,DRVKSUBQ         START WITH REVLINES (X'0E10')                
*                                                                               
SDREV00  ZAP   0(8,R3),=P'0'       ZAP RECORD COUNTER                           
         XC    XKEY,XKEY           CLEAR THE KEY                                
         LA    R2,XKEY             R2 = KEY                                     
         USING DRVRECD,R2          REVLINE DSECT                                
         MVI   DRVKTYP,DRVKTYPQ    X'0E'                                        
         STC   R4,DRVKSUB          X'10'/X'11'/X'12'                            
         MVC   DRVKAM,SBBAGYMD     A/M                                          
         MVC   DRVKCLT,SBBCLT      CLIENT                                       
         MVC   XKEYSAVE,XKEY       SAVE OFF THE KEY                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     SDREV10             TEST KEY                                     
*                                                                               
SDREV5   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
SDREV10  CLC   XKEY(DRVKPRD-DRVKEY),XKEYSAVE                                    
         BNE   SDREV20                                                          
*                                                                               
         LAY   R1,CLTREC           A(CLIENT RECORD)                             
         LA    R1,CLIST-CLTHDR(R1) A(PRODUCT TABLE)                             
*                                                                               
SDREV15  CLI   0(R1),C' '          END OF PRODUCT TABLE?                        
         BNH   SDREV5              YES - READ SEQ                               
*                                                                               
         CLC   DRVKPRD,0(R1)       MATCH ON 3-CHARACTER PRODUCT?                
         BE    *+12                YES                                          
         LA    R1,4(R1)            NO - BUMP TO NEXT CLIST ENTRY                
         B     SDREV15             CHECK NEXT PRODUCT                           
*                                                                               
         LLC   RE,3(R1)            BINARY PRODUCT?                              
         BCTR  RE,0                -1                                           
         SLL   RE,8                X 256                                        
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,1,DRVKEST        ESTIMATE                                     
         BZ    SDREV5              DON'T TRY TO CLOSE OUT EST 0                 
         BCTR  RF,0                -1                                           
         AR    RE,RF               PRD+EST INDEX                                
         L     R1,SBAESTTB         A(ESTIMATE TABLE)                            
         AR    RE,R1               INDEX INTO ESTIMATE TABLE                    
         CLI   0(RE),0             ESTIMATE ELIGIBLE FOR CLOSEOUT?              
         BE    SDREV5              NO - READ SEQ                                
*                                                                               
         MVC   AIO,SBAIO1          A(AIO AREA)                                  
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',DRVKDA,AIO,DMWORK             
*                                                                               
         BRAS  RE,XDEL             CLOSEOUT RECORD                              
         AP    0(8,R3),=P'1'       INCRIMENT CLOSED OUT COUNTER                 
         B     SDREV5              READ SEQ                                     
*                                                                               
SDREV20  CLC   =X'0E12',XKEYSAVE   JUST PROCESSED PROPOSALS?                    
         BE    SDREVX              YES - DONE                                   
         LA    R3,8(R3)            BUMP TO DELDAVAL / DELDPROL                  
         AHI   R4,1                BUMP TO X'11'/X'12'                          
         B     SDREV00             PROCESS AVAILS/PROPOSALS                     
*                                                                               
SDREVX   J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION FOR DRIVER                                           *         
***********************************************************************         
         SPACE 1                                                                
DRIVINIT NTR1  BASE=*,LABEL=*                                                   
         GOTO1 CALLOV,DMCB,(X'60',0),0    LOAD DPG PHASE                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADPGPROG,0(R1)                                                   
         MVI   WIDTHOPT,C'W'       WIDE REPORT                                  
         MVI   MYFIRSTH,10         FIRST HEADING ON LINE 10                     
         GOTO1 INITDRIV            INITIALIZE DRIVER                            
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVC   ASYSDRV,GLASYSDR    SAVE A(SYSTEM DRIVER)                        
         OI    GLINDS,GLPALTOT     PRINT ALL TOTALS                             
         OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         BASR  R1,0                                                             
         AHI   R1,DRHOOK-*         DRIVER'S APPLICATION HOOK                    
         ST    R1,GLAHOOK                                                       
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLEAR DRIVER'S GLOBAL AREA                               *         
***********************************************************************         
         SPACE 1                                                                
CLRGLOB  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AGLOBAL          CLEAR DRIVER'S GLOBAL AREA                   
         L     RF,GLSIZE                                                        
         LA    RE,GLSIZE+4                                                      
         XCEFL ,                                                                
         MVC   GLASYSDR,ASYSDRV    AND INITIALIZE                               
         ST    RC,GLAWORKD                                                      
         MVC   GLAPROG,ADPGPROG                                                 
         MVI   GLTWORKD,GLTSPOOL                                                
         MVI   GLFHEADL,10                                                      
         MVI   GLLHEADL,13                                                      
         MVI   GLDETHED,C'Y'                                                    
         CLI   TRACEOPT,C'Y'                                                    
         BNE   *+8                                                              
         MVI   GLTRACE,C'Y'                                                     
         OI    GLINDS,GLPALTOT     PRINT ALL TOTALS                             
         BASR  R1,0                                                             
         AHI   R1,DRHOOK-*         DRIVER'S APPLICATION HOOK                    
         ST    R1,GLAHOOK                                                       
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* GENERATE DELETED RECORDS REPORT                                     *         
***********************************************************************         
         SPACE 1                                                                
GENDEL   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRGLOB                                                       
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINIT       CALL DRIVER FOR INITIALIZATION               
         GOTO1 DRIVER,DMCB,(R4)                                                 
         MVI   GLOPTS+2,4                                                       
         MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT FOR EACH               
         CLI   CANADA,C'Y'         TEST CANADA                                  
         BNE   *+8                                                              
         MVI   SBQMED,C'A'         YES-ALL MEDIA                                
*                                                                               
         CLC   =C'TOTAL',DUB                                                    
         BNE   GEND10                                                           
         MVI   GLOPTS+2,9                                                       
***                                                                             
* L'DELCTRQ EXCEEDED 256 SO WE NEED A MVCL                                      
***                                                                             
*        L     R3,ATOTALS                                                       
*        MVC   DELEST(DELCTRQ),0(R3)                                            
*                                                                               
         L     R0,ATOTALS          MOVE FROM A(ATOTALS)                         
         LHI   R1,DELCTRQ          LENGTH OF ALL DELETED REC COUNTERS           
         LA    R2,DELEST           MOVE TO DELEST                               
         LR    R3,R1               LENGTH OF ALL DELETED REC COUNTERS           
         MVCL  R2,R0               DELETED REC COUNTERS MOVE TO DELEST          
*                                                                               
GEND10   MVC   RECTYPE,BLANKS      RECORD TYPE                                  
         MVC   RECTYPE(7),=C'EST HDR'                                           
         ZAP   NUMRECS,DELEST                                                   
         GOTO1 (RF),DMCB,(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(4),=C'GOAL'                                              
         ZAP   NUMRECS,DELGOAL                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(3),=C'BUY'                                               
         ZAP   NUMRECS,DELBUY                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(8),=C'BUY COPY'                                          
         ZAP   NUMRECS,DELBCPY                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(4),=C'BILL'                                              
         ZAP   NUMRECS,DELBILL                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(12),=C'STATION BILL'                                     
         ZAP   NUMRECS,DELSTAB                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(12),=C'BILL FORMULA'                                     
         ZAP   NUMRECS,DELBFORM                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(13),=C'SPLIT BILLING'                                    
         ZAP   NUMRECS,DELSPBL                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(6),=C'PG EST'                                            
         ZAP   NUMRECS,DELPGEST                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(10),=C'CHILD SPOT'                                       
         ZAP   NUMRECS,DELCHILD                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(6),=C'STATUS'                                            
         ZAP   NUMRECS,DELSTAT                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(9),=C'WORKSHEET'                                         
         ZAP   NUMRECS,DELBWS                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(3),=C'MSR'                                               
         ZAP   NUMRECS,DELMSR                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(3),=C'BGR'                                               
         ZAP   NUMRECS,DELBGR                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(3),=C'XLK'                                               
         ZAP   NUMRECS,DELXLK                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(3),=C'UPL'                                               
         ZAP   NUMRECS,DELUPL                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(3),=C'AUT'                                               
         ZAP   NUMRECS,DELAUT                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(4),=C'DARE'                                              
         ZAP   NUMRECS,DELDARE                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(13),=C'DARE COMMENTS'                                    
         ZAP   NUMRECS,DELDARC                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(7),=C'DARE MG'                                           
         ZAP   NUMRECS,DELDMG                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(8),=C'DARE MGN'                                          
         ZAP   NUMRECS,DELDMGN                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(14),=C'DARE XMG CABLE'                                   
         ZAP   NUMRECS,DELDXMG                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(15),=C'DARE XMGN CABLE'                                  
         ZAP   NUMRECS,DELDXMGN                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(15),=C'DARE MG REJ COM'                                  
         ZAP   NUMRECS,DELDMGRC                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(10),=C'OM HISTORY'                                       
         ZAP   NUMRECS,DELORH                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(2),=C'PW'                                                
         ZAP   NUMRECS,DELPW                                                    
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(11),=C'BUY HISTORY'                                      
         ZAP   NUMRECS,DELBHR                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(7),=C'COMMENT'                                           
         ZAP   NUMRECS,DELUCOM                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(9),=C'X COMMENT'                                         
         ZAP   NUMRECS,DELXCOM                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(6),=C'NETDEF'                                            
         ZAP   NUMRECS,DELNETD                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(6),=C'CBLPRO'                                            
         ZAP   NUMRECS,DELCBLP                                                  
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(10),=C'SPOT BHOLD'                                       
         ZAP   NUMRECS,DELSBH                                                   
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(13),=C'SPLIT RECORDS'                                    
         ZAP   NUMRECS,DELSPLIT                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(18),=C'DARE BATCH RECORDS'                               
         ZAP   NUMRECS,DELDARBT                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(19),=C'DARE FLIGHT RECORDS'                              
         ZAP   NUMRECS,DELDARFL                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(19),=C'DESKTOP REVLINE RECORDS'                          
         ZAP   NUMRECS,DELDREVL                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(19),=C'DESKTOP AVAIL RECORDS'                            
         ZAP   NUMRECS,DELDAVAL                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVC   RECTYPE,BLANKS                                                   
         MVC   RECTYPE(19),=C'DESKTOP PROPOSAL RECORDS'                         
         ZAP   NUMRECS,DELDPROL                                                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         CLC   =C'TOTAL',DUB                                                    
         BE    GENDX                                                            
         L     RF,ATOTALS                                                       
         LA    RE,DELEST                                                        
         LHI   R0,DELCTRQ/8                                                     
*                                                                               
         AP    0(8,RF),0(8,RE)                                                  
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
*                                                                               
GENDX    J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ADD I8 &TR REQUESTS FOR ONE CLIENT IF REQ IS ALL PRD & ALL EST      *         
***********************************************************************         
         SPACE 1                                                                
ADDI8TR  NTR1  BASE=*,LABEL=*                                                   
         CLI   COONLY,C'Y'         OPT TO SKIP I8 & TR?                         
         BE    ADDI8X                                                           
         CLI   SBQBPRD,0           TEST ALL PRD REQ                             
         BNE   ADDI8X               NO                                          
         CLI   SBQEST,1            TEST ALL EST REQ                             
         BNE   ADDI8X                                                           
         CLI   SBQESTND,255                                                     
         BNE   ADDI8X                                                           
*                                                                               
         MVC   I8REQST,BLANKS                                                   
         USING R$QRECORD,R3                                                     
         LA    R3,I8REQST                                                       
         MVC   R$QCODE,=C'I8'                                                   
         MVC   R$QAGY,SBQAGY       AGENCY                                       
         MVC   R$QMED,SVQMED       MEDIA                                        
         MVC   R$QCLT,SBCLT        CLIENT                                       
         MVC   R$QSTART(4),=C'8601' START DATE                                  
         MVC   R$QEND,ENDDATE      END DATE                                     
         MVI   R$QOPT2,C'P'        PRINT ALL DETAILS                            
         MVC   R$QOPT4,SKIPPU      SET PU OPTION                                
         MVC   R$QOPT5,TEST        SET TEST OPTION                              
         DROP  R3                                                               
*                                                                               
         L     R3,AI8RQDCB                                                      
         LA    R0,I8REQST                                                       
         PUT   (R3),(R0)                                                        
*                                                                               
         MVC   I8REQST(80),BLANKS                                               
         MVC   I8REQST(2),=C'PR'                                                
         MVC   I8REQST+2(2),SBQAGY                                              
         MVC   I8REQST+4(26),=CL26'*.PURGE.PURGE..DDS,T/A....'                  
         MVC   I8REQST+30(1),SVQMED                                             
         MVI   I8REQST+31,C'.'                                                  
         MVC   I8REQST+32(3),SBCLT                                              
         MVC   I8REQST+35(3),=C'...'                                            
         GOTO1 DATCON,DMCB,(0,ENDDATE),(5,I8REQST+38)                           
         LA    R1,I8REQST+46                                                    
         MVI   0(R1),C'.'                                                       
         AHI   R1,1                                                             
*                                                                               
         CLI   SKIPPU,C'Y'                                                      
         BNE   *+14                                                             
         MVC   0(7,R1),=C'SKIPPU,'                                              
         AHI   R1,7                                                             
*                                                                               
         CLI   TEST,C'Y'                                                        
         BNE   *+14                                                             
         MVC   0(5,R1),=C'TEST.'                                                
         AHI   R1,5                                                             
*                                                                               
         BCTR  R1,0                LAST CHAR WILL BE . OR ,                     
         MVC   0(2,R1),=C'.*'      SO FORCE TO .                                
*                                                                               
         L     R3,ATRRQDCB                                                      
         LA    R0,I8REQST                                                       
         PUT   (R3),(R0)                                                        
*                                                                               
ADDI8X   J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* GENERATE ERROR REPORT                                               *         
***********************************************************************         
         SPACE 1                                                                
GENERR   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRGLOB                                                       
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINIT       CALL DRIVER FOR INITIALIZATION               
         GOTO1 DRIVER,DMCB,(R4)                                                 
         MVI   GLOPTS+2,5                                                       
         MVI   GLMODE,GLINPUT                                                   
         L     R6,AERRTAB                                                       
         USING ERRECD,R6                                                        
*                                                                               
* IF GENERATING AN ERROR REPORT FOR THIS CLT, ADD CLT TO CLTERR TABLE           
* FOR AGENCY-WIDE ERROR SUMMARY                                                 
*                                                                               
         L     RF,ACLTERR                                                       
GENERR10 OC    0(4,RF),0(RF)       FIND FREE ENTRY                              
         BZ    GENERR20                                                         
         AHI   RF,4                                                             
         C     RF,ACLTERRX                                                      
         BNL   GENERR60            TABLE FULL                                   
         B     GENERR10                                                         
*                                                                               
GENERR20 MVC   0(1,RF),ERMED                                                    
         MVC   1(3,RF),ERCLT                                                    
*                                                                               
GENERR60 OC    0(ERRECL,R6),0(R6)  TEST END OF TABLE                            
         BZ    GENERR80            YES-CALL DRIVER FOR OUTPUT NOW               
         LA    R3,ERRECL(R6)       NO-TEST LAST POSSIBLE TABLE ENTRY            
         C     R3,AERRTABX                                                      
         BL    GENERR70            NO-CALL DRIVER FOR INPUT                     
         MVC   SVQMED2,SBQMED                                                   
         MVI   SBQMED,C'*'         YES-PRINT 'OVERFLOW' LINE                    
         MVC   SBMEDNM,=C'*OVERFLOW*'                                           
         MVC   SBCLT,XFF                                                        
         MVC   SBPRD,XFF                                                        
         MVC   SBBMKT,XFF                                                       
         MVC   SBSTA,XFF                                                        
         MVI   SBBEST,0                                                         
         MVI   ERRCD,0                                                          
         GOTO1 DRIVER,DMCB,(R4)                                                 
         MVC   SBQMED,SVQMED2                                                   
         B     GENERR80                                                         
*                                                                               
GENERR70 MVC   SBQMED,ERMED                                                     
         MVC   SBCLT,ERCLT                                                      
         MVC   SBPRD,ERPRD                                                      
         XC    SBPRD2,SBPRD2                                                    
         MVC   SBBMKT,ERMKT                                                     
         MVC   SBSTA,ERSTA                                                      
         MVC   SBCBLNET,ERNET                                                   
         MVC   SBBEST,EREST                                                     
         MVC   ERRCD,ERERR                                                      
         GOTO1 DRIVER,DMCB,(R4)                                                 
         LR    R6,R3                                                            
         B     GENERR60                                                         
*                                                                               
GENERR80 MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
GENERRX  J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* GENERATE SKIPPED CLIENT REPORT                                      *         
***********************************************************************         
         SPACE 1                                                                
GENSKIP  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRGLOB                                                       
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINIT       CALL DRIVER FOR INITIALIZATION               
         GOTO1 DRIVER,DMCB,(R4)                                                 
         MVI   GLOPTS+2,7                                                       
         MVI   GLMODE,GLINPUT                                                   
         L     R6,ACLTTAB                                                       
*                                                                               
GENSKIP1 OC    0(4,R6),0(R6)       TEST END OF TABLE                            
         BZ    GENSKIP4            YES-CALL DRIVER FOR OUTPUT NOW               
         MVC   SBQMED,0(R6)                                                     
         MVC   SBCLT,1(R6)                                                      
         GOTO1 DRIVER,DMCB,(R4)                                                 
         LA    R6,4(R6)                                                         
         C     R6,ACLTTABX                                                      
         BL    GENSKIP1                                                         
*                                                                               
GENSKIP4 MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
GENSKIPX J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* GENERATE CLIENT ERROR SUMMARY                                       *         
***********************************************************************         
         SPACE 1                                                                
GENCLER  NTR1  BASE=*,LABEL=*                                                   
         OC    SBQBCLT,SBQBCLT     IS THIS A SINGLE CLT REQ?                    
         BNZ   GENCLERX             YES - NO ERROR SUMMARY                      
         BRAS  RE,CLRGLOB                                                       
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         MVI   GLMODE,GLINIT       CALL DRIVER FOR INITIALIZATION               
         GOTO1 DRIVER,DMCB,(R4)                                                 
         MVI   GLOPTS+2,10                                                      
         MVI   GLMODE,GLINPUT                                                   
         L     R6,ACLTERR                                                       
*                                                                               
GENCLER1 OC    0(4,R6),0(R6)       TEST END OF TABLE                            
         BZ    GENCLER4            YES-CALL DRIVER FOR OUTPUT NOW               
         MVC   SBQMED,0(R6)                                                     
         MVC   SBCLT,1(R6)                                                      
         GOTO1 DRIVER,DMCB,(R4)                                                 
         AHI   R6,4                                                             
         C     R6,ACLTERRX                                                      
         BL    GENCLER1                                                         
         DC    H'0'                MORE THAN 1000 CLTS IN ERROR!                
*                                                                               
GENCLER4 MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
GENCLERX J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* RECORD DELETE ROUTINE                                               *         
***********************************************************************         
         SPACE 1                                                                
DELETE   NTR1  BASE=*,LABEL=*                                                   
         CLI   TEST,C'Y'           TEST MODE SKIPS WRITING TO FILE              
         BE    DELETE12                                                         
         MVC   AIO,SBAIO1          FIRST GET THE RECORD                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SBIOKEY),SBIOKEY                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R1,SBAIO1           THEN DELETE                                  
*                                                                               
         CLI   0(R1),X'06'         IS THIS A FUCKING AGY REC?                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SBIOKEY,X'06'                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    15(R1),X'40'        X'40' MARKS RECORD AS CLOSED OUT             
         GOTO1 DATAMGR,DMCB,(X'08',DMDEL),SYSFIL,SBIOKEY,SBAIO1,DMWORK          
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELETE12 CLI   IOTRACE,C'Y'                                                     
         BNE   DELETEX                                                          
         MVC   MYPRINT,BLANKS                                                   
         GOTO1 HEXOUT,DMCB,SBAIO1,MYPRINT,24,0                                  
         GOTO1 SBPRINT,(R1),MYPRINT-1,=C'BL01'                                  
DELETEX  J     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
*                                  ** ENTRY POINT WHEN RECORD ALREADY           
DELETE2  NTR1  BASE=*,LABEL=*      ** IN SBAIO1                                 
         CLI   TEST,C'Y'           TEST MODE SKIPS WRITING TO FILE              
         BE    DELETE22                                                         
         MVC   SBIOKEY(13),KEY                                                  
*                                                                               
         L     R1,SBAIO1           THEN DELETE                                  
*                                                                               
         CLI   0(R1),X'06'         IS THIS A FUCKING AGY REC?                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SBIOKEY,X'06'                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    15(R1),X'40'        X'40' MARKS RECORD AS CLOSED OUT             
         GOTO1 DATAMGR,DMCB,(X'08',DMDEL),SYSFIL,SBIOKEY,SBAIO1,DMWORK          
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELETE22 CLI   IOTRACE,C'Y'                                                     
         BNE   DELETE2X                                                         
         MVC   MYPRINT,BLANKS                                                   
         GOTO1 HEXOUT,DMCB,SBAIO1,MYPRINT,24,0                                  
         GOTO1 SBPRINT,(R1),MYPRINT-1,=C'BL01'                                  
DELETE2X J     XIT                                                              
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ERRORS TO ERROR TABLE                                           *         
***********************************************************************         
         SPACE 1                                                                
ADDERR   NTR1  BASE=*,LABEL=*                                                   
         LA    R6,ERREC            BUILD ERROR RECORD                           
         USING ERRECD,R6                                                        
         MVC   ERMED,SBMED                                                      
         MVC   ERCLT,SBCLT                                                      
         MVC   ERPRD,SBPRD                                                      
         MVC   ERMKT,SBBMKT                                                     
         MVC   ERSTA,SBSTA                                                      
         MVC   ERNET,SBCBLNET                                                   
         MVC   EREST,ESTLIN                                                     
ADDERR1  L     R5,ACURERR                                                       
         C     R5,AERRTABX                                                      
         JNL   XIT                                                              
         CLC   ERREC(ERKEYL),0(R5) TEST KEY CHANGE                              
         BE    ADDERR2                                                          
         OC    0(ERRECL,R5),0(R5)  TEST FIRST TIME                              
         BZ    ADDERR2                                                          
         LA    R5,ERRECL(R5)                                                    
         ST    R5,ACURERR                                                       
         C     R5,AERRTABX                                                      
         JNL   XIT                                                              
         LA    R1,ERRECL(R5)       CLEAR THE ENTRY AHEAD                        
         C     R1,AERRTABX                                                      
         BNL   ADDERR2                                                          
         XC    0(ERRECL,R1),0(R1)                                               
*                                                                               
ADDERR2  MVC   0(ERKEYL,R5),ERREC                                               
         LR    R6,R5                                                            
         OC    ERERR,ERRCD         OR IN CURRENT ERROR BITS                     
         J     XIT                                                              
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE STATUS RECORDS                                               *         
***********************************************************************         
         SPACE 1                                                                
STATUS   NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELSTAT,=P'0'                                                    
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING STATD,R2                                                         
         MVC   STKTYPE,=X'0D71'    READ STATUS RECORD KEYS                      
         MVC   STKAGMD,SBBAGYMD                                                 
         MVC   STKCLT,SBBCLT                                                    
         GOTO1 HIGH                                                             
         B     STAT4                                                            
*                                                                               
STAT2    GOTO1 SEQ                                                              
*                                                                               
STAT4    CLC   KEY(STKPRD-STATKEY),KEYSAVE                                      
         BNE   STATX                                                            
*                                                                               
         SR    RE,RE               TEST ESTIMATE VALID FOR CLOSEOUT             
         ICM   RE,1,STKPRD                                                      
         BZ    STAT2                                                            
         B     *+8                                                              
         LA    RE,255              PRODUCT POL                                  
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,STKEST                                                      
         BZ    STAT2                                                            
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    STAT2                                                            
STAT6    L     R1,SBAIO1           YES-GET THE RECORD                           
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2          AND DELETE                                   
         AP    DELSTAT,=P'1'                                                    
         B     STAT2               NEXT RECORD                                  
*                                                                               
STATX    J     XIT                                                              
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE BUYER'S WORKSHEET RECORDS                                    *         
***********************************************************************         
         SPACE 1                                                                
BWS      NTR1  BASE=*,LABEL=*                                                   
         ZAP   DELBWS,=P'0'                                                     
*                                                                               
BWS1     XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CAMRECD,R2                                                       
         MVI   CAMPKTYP,CAMPKTYQ   READ CAMPAIGN RECORD PASSIVE KEYS            
         MVI   CAMPKSUB,CAMPKSBQ                                                
         MVC   CAMPKAM,SBBAGYMD                                                 
         MVC   CAMPKCLT,SBBCLT                                                  
         GOTO1 HIGH                                                             
         B     BWS4                                                             
*                                                                               
BWS2     GOTO1 SEQ                                                              
*                                                                               
BWS4     CLC   KEY(CAMPKPRD-CAMPKEY),KEYSAVE                                    
         BNE   BWSX                                                             
         SR    RE,RE               TEST ESTIMATE VALID FOR CLOSEOUT             
         ICM   RE,1,CAMPKPRD                                                    
         BZ    BWS2                                                             
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,CAMPKEST                                                    
         BZ    BWS2                                                             
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    BWS2                                                             
         L     R3,SBAIO1           YES-GET THE RECORD                           
         ST    R3,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2          DELETE CAMPAIGN RECORD                       
         AP    DELBWS,=P'1'                                                     
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         USING BWHRECD,R2                                                       
         MVI   BWHKTYP,BWHKTYPQ    READ CAMPAIGN'S MARKET HEADER RECS           
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,SBBAGYMD                                                
         MVC   BWHKBYR,CAMKBYR-CAMKEY(R3)                                       
         MVC   BWHKCAM,CAMKCAM-CAMKEY(R3)                                       
         GOTO1 HIGH                                                             
         B     BWS8                                                             
*                                                                               
BWS6     GOTO1 SEQ                                                              
*                                                                               
BWS8     CLC   KEY(BWHKMKT-BWHKEY),KEYSAVE                                      
         BNE   BWS16                                                            
         L     R3,SBAIO1           GET THE RECORD                               
         ST    R3,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2          DELETE CAMPAIGN MARKET HEADER                
         AP    DELBWS,=P'1'                                                     
         MVC   SVKEY2,KEY                                                       
         XC    KEY,KEY                                                          
         USING BWDRECD,R2                                                       
         MVI   BWDKTYP,BWDKTYPQ    READ ALL DETAIL RECORDS                      
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,SBBAGYMD                                                
         MVC   BWDKBYR,BWHKBYR-BWHKEY(R3)                                       
         MVC   BWDKSEQ,BWHKSEQ-BWHKEY(R3)                                       
         GOTO1 HIGH                                                             
         B     BWS12                                                            
*                                                                               
BWS10    GOTO1 SEQ                                                              
*                                                                               
BWS12    CLC   KEY(BWDKEL-BWDKEY),KEYSAVE                                       
         BNE   BWS14                                                            
         L     R1,SBAIO1           GET THE RECORD                               
         ST    R1,AIO                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,DELETE2          DELETE DETAIL RECORD                         
         AP    DELBWS,=P'1'                                                     
         B     BWS10                                                            
*                                                                               
BWS14    MVC   KEY,SVKEY2                                                       
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         B     BWS6                NEXT CAMPAIGN MARKET HEADER RECORD           
*                                                                               
BWS16    MVC   KEY,SVKEY                                                        
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         B     BWS2                NEXT CAMPAIGN                                
*                                                                               
BWSX     TM    SBBAGYMD,X'08'      BWS RECS USE HOB OF MEDIA NIBBLE...          
         BNZ   *+12                ...TO INDICATE BYRCODE > 255                 
         OI    SBBAGYMD,X'08'                                                   
         B     BWS1                                                             
         NI    SBBAGYMD,X'FF'-X'08'                                             
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
         DROP  RB                                                               
***********************************************************************         
* DRIVER HEADHOOK                                                     *         
***********************************************************************         
         SPACE 1                                                                
HEADHK   CLI   GLRECNO,8           TEST SKIPPED CLIENT REPORT                   
         JE    HEADHK10             YES - NO SPECIAL HEADS                      
         CLI   GLRECNO,9           TEST TOTAL DELETED REC REPORT                
         JE    HEADHK10             YES - NO SPECIAL HEADS                      
         CLI   GLRECNO,10          TEST CLT ERROR SUMMARY                       
         JE    HEADHK10             YES - NO SPECIAL HEADS                      
         CLI   GLRECNO,7           TEST AGENCY SUMMARY                          
         JNE   HEADHK2                                                          
         TM    GLINDS,GLTOTLIN     AND LEVEL 1 TOTAL                            
         JZ    HEADHK10                                                         
         CLI   GLLEVEL,1                                                        
         JNE   HEADHK10                                                         
         L     R1,AH4              YES-REMOVE MEDIA FROM HEADLINES              
         MVC   0(L'OUTAREA+2,R1),BLANKS                                         
         J     HEADHK10                                                         
*                                                                               
HEADHK2  L     R1,AH4              PUT ESTIMATE TO HEAD 6                       
         A     R1,PWIDTH                                                        
         A     R1,PWIDTH                                                        
         CLI   GLRECNO,3           OR HEAD 7                                    
         JL    *+16                                                             
         CLI   GLRECNO,6                                                        
         JE    *+8                                                              
         A     R1,PWIDTH                                                        
         MVC   1(65,R1),BLANKS                                                  
         MVC   1(8,R1),=C'ESTIMATE'                                             
         CLI   SBQEST,1                                                         
         JNE   HEADHK8                                                          
         CLI   SBQESTND,255                                                     
         JNE   HEADHK8                                                          
         MVC   17(3,R1),=C'ALL'                                                 
         CLC   SBQESFLT,BLANKS     ESTIMATE FILTERING                           
         JNH   HEADHK10                                                         
         MVC   17(10,R1),=C'FILTERED ('                                         
         LA    R0,L'SBQESFLT                                                    
         LA    R2,SBQESFLT                                                      
         LA    RE,C'1'                                                          
         LA    RF,27(R1)                                                        
*                                                                               
HEADHK4  CLI   0(R2),C'*'                                                       
         JE    HEADHK6                                                          
         CLI   0(R2),C' '                                                       
         JE    HEADHK6                                                          
         STC   RE,0(RF)                                                         
         MVI   1(RF),C'='                                                       
         TM    0(R2),X'40'         TEST NEGATIVE FILTER                         
         JO    *+12                                                             
         MVI   2(RF),C'-'                                                       
         LA    RF,1(RF)                                                         
         MVC   2(1,RF),0(R2)                                                    
         OI    2(RF),X'40'         INSURE UPPER CASE                            
         MVI   3(RF),C','                                                       
         LA    RF,4(RF)                                                         
*                                                                               
HEADHK6  LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         JCT   R0,HEADHK4                                                       
         BCTR  RF,0                                                             
         MVI   0(RF),C')'                                                       
         J     HEADHK10                                                         
*                                                                               
HEADHK8  LLC   RE,SBQEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  17(3,R1),DUB                                                     
         CLC   SBQEST,SBQESTND                                                  
         JE    HEADHK10                                                         
         MVI   20(R1),C'-'                                                      
         IC    RE,SBQESTND                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  21(3,R1),DUB                                                     
*                                                                               
HEADHK10 DS    0H                                                               
         GOTO1 GENHEAD             LINE UP HEADLINES AND FORMAT TITLES          
*                                                                               
         CLI   GLRECNO,5           TEST DELETED RECORD REPORT                   
         JNE   HEADHKX                                                          
         CLI   TEST,C'Y'           AND TEST MODE                                
         JNE   HEADHKX                                                          
         L     R1,AH4              YES-PUT MSG TO HEAD 5                        
         A     R1,PWIDTH                                                        
         MVC   72(17,R1),=C'*** TEST MODE ***'                                  
*                                                                               
HEADHKX  J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS, VARIALBLES, STORAGE AREAS, ETC.                          *         
***********************************************************************         
         SPACE 1                                                                
GLOBALS  DS    0D                                                               
*                                                                               
DELEST   DS    PL8                                                              
DELBUY   DS    PL8                                                              
DELGOAL  DS    PL8                                                              
DELBILL  DS    PL8                                                              
DELSTAB  DS    PL8                                                              
DELPGEST DS    PL8                                                              
DELCHILD DS    PL8                                                              
DELSTAT  DS    PL8                                                              
DELBWS   DS    PL8                                                              
DELBCPY  DS    PL8                                                              
DELMSR   DS    PL8                                                              
DELBHR   DS    PL8                                                              
DELBFORM DS    PL8                                                              
DELSPBL  DS    PL8                                                              
DELORH   DS    PL8                                                              
DELUCOM  DS    PL8                                                              
DELXCOM  DS    PL8                                                              
DELBGR   DS    PL8                                                              
DELDARE  DS    PL8                                                              
DELDARC  DS    PL8                                                              
DELDMG   DS    PL8                                                              
DELDMGN  DS    PL8                                                              
DELPW    DS    PL8                                                              
DELXLK   DS    PL8                                                              
DELUPL   DS    PL8                                                              
DELAUT   DS    PL8                                                              
DELNETD  DS    PL8                                                              
DELCBLP  DS    PL8                                                              
DELSBH   DS    PL8                                                              
DELSPLIT DS    PL8                                                              
DELDARBT DS    PL8                                                              
DELDARFL DS    PL8                                                              
DELDREVL DS    PL8                                                              
DELDAVAL DS    PL8                                                              
DELDPROL DS    PL8                                                              
DELDXMG  DS    PL8                                                              
DELDXMGN DS    PL8                                                              
DELDMGRC DS    PL8                                                              
DELCTRQ  EQU   *-DELEST                                                         
*                                                                               
NUMRECS  DS    PL8                                                              
*                                                                               
ORD      DS    PL8                                                              
NET      DS    PL8                                                              
PAID     DS    PL8                                                              
NETPD    DS    PL8                                                              
BILL     DS    PL8                                                              
NETBILL  DS    PL8                                                              
*                                                                               
TPAID    DS    PL8                                                              
TNETBILL DS    PL8                                                              
TSTABILL DS    PL8                                                              
*                                                                               
MYDUB    DS    D                                                                
ASAVE    DS    A                                                                
VDYNALLO DS    V                                                                
ASYSDRV  DS    A                                                                
ACURERR  DS    A                                                                
*>>>AMONTOTS DC    A(MONTOTS)                                                   
BUYUNPD  DS    F                                                                
BUYNETUP DS    F                                                                
NEXTCLT  DS    H                                                                
*                                                                               
SKIPPU   DS    C                   Y=IGNORE PU PROFILE                          
SKIPONLY DS    C                   Y=ONLY REPORT SKIPPED CLIENTS                
COONLY   DS    C                   Y=SPOT CLOSEOUT ONLY (NO I8 OR TR)           
PUPROF   DS    CL16                PURGE CONTROL PROFILE                        
*                                                                               
ENDDATE  DS    CL6                                                              
CANADA   DS    CL1                                                              
IGNUNPD  DS    CL1                                                              
IGNUNBL  DS    CL1                                                              
ALLBUYS  DS    CL1                                                              
IOTRACE  DS    CL1                                                              
TEST     DS    CL1                                                              
PHASE    DS    XL1                                                              
CLTSW    DS    CL1                                                              
CHKCLT   DS    CL1                                                              
ESTLIN   DS    XL3                                                              
PIGEST   DS    CL4                                                              
SVQMED   DS    CL1                                                              
SVQMED2  DS    CL1                                                              
SVQCLT   DS    CL3                                                              
SVBCLT   DS    XL2                                                              
SVBMKT   DS    XL2                                                              
SVSTA    DS    CL5                                                              
SVKEY    DS    CL(L'KEY)                                                        
SVKEY2   DS    CL(L'KEY)                                                        
XKEY     DS    CL64                KEYS USED FOR XFILES                         
XKEYSAVE DS    CL64                                                             
SVERROR  DS    XL1                                                              
RECTYPE  DS    CL12                                                             
ERRMSG   DS    CL30                                                             
MYPRINT  DS    CL132                                                            
AGYMEDT  DS    XL1                                                              
AGYMEDN  DS    XL1                                                              
AGYMEDC  DS    XL1                                                              
CLTNMT   DS    CL(L'SBCLTNM)                                                    
CLTNMN   DS    CL(L'SBCLTNM)                                                    
*                                                                               
SVORDER  DS    0XL4                ORDER NUMBER (FF COMPLEMENT)                 
SVORDDT  DS    XL2                 (YEAR-90)*1000 + JULIAN DAY                  
SVORDSQ  DS    XL2                 SEQUENCE NUMBER (0-9999)                     
*                                                                               
ERRFLAG  DS    XL1                                                              
ERRPIG   EQU   X'80'                                                            
ERRPDT   EQU   X'40'                                                            
ERRBDT   EQU   X'20'                                                            
ERRGRSUP EQU   X'10'                                                            
ERRGRSUB EQU   X'08'                                                            
ERRNETUP EQU   X'04'                                                            
ERRNETUB EQU   X'02'                                                            
ERRCD    DS    XL1                                                              
ERRFLAGT DS    XL1                                                              
ERRFLAGN DS    XL1                                                              
ERRFLAGC DS    XL1                                                              
*                                                                               
LINERR   DS    XL1                                                              
LINERRPE EQU   X'80'                                                            
LINERRPD EQU   X'40'                                                            
LINERRBD EQU   X'20'                                                            
*                                                                               
ERREC    DS    CL(ERRECL)          ERROR RECORD                                 
I8REQST  DS    CL(L'R$QAREA)                                                    
*                                                                               
CANCEL   DS    XL1                                                              
CANPOL   EQU   1                                                                
*                                                                               
FF       EQU   X'FF'                                                            
XFF      DC    16X'FF'                                                          
BLANKS   DC    132C' '                                                          
DMDEL    DC    CL8'DMDEL'                                                       
DDTEMP   DC    CL8'ECTEMP'                                                      
TMPALLOC DC    XL6'000003000003'                                                
*                                                                               
       ++INCLUDE DDEBLOCK                                                       
*                                                                               
         SPACE 2                                                                
* VALUES SAVED BETWEEN REQUESTS                                                 
*                                                                               
SAVVALS  DS    0X                                                               
*                                                                               
AECTEMP  DS    A                                                                
AI8RQDCB DS    A                                                                
ATRRQDCB DS    A                                                                
AERRTAB  DS    A                                                                
AERRTABX DS    A                                                                
ACLTTAB  DS    A                                                                
ACLTTABX DS    A                                                                
ACLTERR  DS    A                                                                
ACLTERRX DS    A                                                                
AMONTOTS DS    A                                                                
AMONTOTX DS    A                                                                
ATOTALS  DS    A                                                                
SVSPLID  DS    CL3                                                              
ACTSW    DS    CL1                                                              
*                                                                               
MEDNMTAB DS    (MEDMAX)CL11                                                     
MEDMAX   EQU   8                                                                
*                                                                               
SAVVALSL EQU   *-SAVVALS                                                        
*                                                                               
         DS    0D                                                               
ECTEMPLB DC    CL8'*ECTEMP*'                                                    
ECTEMP   DCB   DDNAME=ECTEMP,DSORG=PS,LRECL=90,BLKSIZE=900,            X        
               MACRF=(GM,PM),RECFM=FB,EODAD=LAST5                               
ECTEMPL  EQU   *-ECTEMPLB                                                       
*                                                                               
         DS    0D                                                               
I8RQLBL  DC    CL8'*I8RQFL*'                                                    
I8RQFIL  DCB   DDNAME=I8RQFIL,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=80,MACRF=PM                                                
I8RQFILL EQU   *-I8RQLBL                                                        
*                                                                               
         DS    0D                                                               
TRRQLBL  DC    CL8'*TRRQFL*'                                                    
TRRQFIL  DCB   DDNAME=TRRQFIL,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=80,MACRF=PM                                                
TRRQFILL EQU   *-TRRQLBL                                                        
*                                                                               
         SPACE 2                                                                
TOTREC   DS    0CL90               TEMPORARY FILE RECORD LAYOUT                 
*                                                                               
TOTKEY   DS    0CL26                                                            
TOTMED   DS    CL1                                                              
TOTCLT   DS    CL3                                                              
TOTCLTNM DS    CL20                                                             
TOTMON   DS    XL2                                                              
*                                                                               
TOTDATA  DS    0CL64                                                            
TOTORD   DS    PL8                                                              
TOTNET   DS    PL8                                                              
TOTPAID  DS    PL8                                                              
TOTNETPD DS    PL8                                                              
TOTUNPD  DS    PL8                                                              
TOTNETUP DS    PL8                                                              
TOTBILL  DS    PL8                                                              
TOTBILLN DS    PL8                                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*>>>MONTOTS  DS    (MONMAX)CL(MONTOTL)     MONTH TOTAL TABLE                    
*MONMAX   EQU   240                 ROOM FOR 20 MEDIA/YEARS                     
*                         * NOTE * MEDIA T CAN PROCESS MEDIA T & N,             
*                                  SO IT'S REALLY 20 MEDIA/YEARS                
MONMAX   EQU   480                 ROOM FOR 40 MEDIA/YEARS                      
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
*                                                                               
CLTREC   DS    XL(CLTHDRL)         CLIENT RECORD                                
*                                                                               
LINERRTB DC    AL1(LINERRPE,12),CL12'PTNR EST ERR'                              
         DC    AL1(LINERRPD,11),CL11'PAY=CURR MO'                               
         DC    AL1(LINERRBD,15),CL15'BILL MO=CURR MO'                           
         DC    AL1(0)                                                           
*                                                                               
CANCELTB DC    AL1(CANPOL,34),CL34'POL CLIENT CANNOT CLOSE BY PRODUCT'          
         DC    AL1(0)                                                           
*                                                                               
RPT1TITL DC    C'SPOTPAK CLOSEOUT ESTIMATE SUMMARY'                             
RPT2TITL DC    C'SPOTPAK CLOSEOUT BILL REPORT'                                  
RPT3TITL DC    C'SPOTPAK CLOSEOUT BUY REPORT'                                   
RPT5TITL DC    C'SPOTPAK CLOSEOUT DELETED RECORD SUMMARY'                       
RPT6TITL DC    C'SPOTPAK CLOSEOUT ERROR SUMMARY'                                
RPT7TITL DC    C'SPOTPAK CLOSEOUT AGENCY SUMMARY'                               
RPT8TITL DC    C'SPOTPAK CLOSEOUT SKIPPED CLIENT REPORT'                        
RPT10TITL DC    C'SPOTPAK CLOSEOUT CLIENT ERROR SUMMARY'                        
         EJECT                                                                  
ROUTLIST DS    0F                  ROUTINE ADDRESS LIST                         
         DC    CL8'OMED    ',AL4(OMED)                                          
         DC    CL8'ICLTCODE',AL4(ICLTCD)                                        
         DC    CL8'ICLIENT ',AL4(ICLT)                                          
         DC    CL8'OMKT    ',AL4(OMKT)                                          
         DC    CL8'IESTDT  ',AL4(IESTDT)                                        
         DC    CL8'OESTDT  ',AL4(OESTDT)                                        
         DC    CL8'IESTNM  ',AL4(IESTNM)                                        
         DC    CL8'IMONTH  ',AL4(IMONTH)                                        
         DC    CL8'OMONTH  ',AL4(OMONTH)                                        
         DC    CL8'IESTLIN ',AL4(IESTLIN)                                       
         DC    CL8'OESTLIN ',AL4(OESTLIN)                                       
         DC    CL8'IPIGEST ',AL4(IPIGEST)                                       
         DC    CL8'OPIGEST ',AL4(OPIGEST)                                       
         DC    CL8'IERREST ',AL4(IERREST)                                       
         DC    CL8'OERREST ',AL4(OERREST)                                       
         DC    CL8'ILINERR ',AL4(ILINERR)                                       
         DC    CL8'OLINERR ',AL4(OLINERR)                                       
         DC    CL8'IREC    ',AL4(IREC)                                          
         DC    CL8'IERR    ',AL4(IERR)                                          
         DC    CL8'OERR    ',AL4(OERR)                                          
         DC    CL8'IDEL    ',AL4(IDEL)                                          
         DC    CL8'IORD    ',AL4(IORD)                                          
         DC    CL8'OORD    ',AL4(OORD)                                          
         DC    CL8'IPAID   ',AL4(IPAID)                                         
         DC    CL8'OPAID   ',AL4(OPAID)                                         
         DC    CL8'IBILL   ',AL4(IBILL)                                         
         DC    CL8'OBILL   ',AL4(OBILL)                                         
         DC    CL8'INET    ',AL4(INET)                                          
         DC    CL8'ONET    ',AL4(ONET)                                          
         DC    CL8'INETPD  ',AL4(INETPD)                                        
         DC    CL8'ONETPD  ',AL4(ONETPD)                                        
         DC    CL8'INETBILL',AL4(INETBILL)                                      
         DC    CL8'ONETBILL',AL4(ONETBILL)                                      
         DC    CL8'IBHGRS  ',AL4(IBHGRS)                                        
         DC    CL8'IASORD  ',AL4(IASORD)                                        
         DC    CL8'IASPAID ',AL4(IASPAID)                                       
         DC    CL8'IASUNPD ',AL4(IASUNPD)                                       
         DC    CL8'IASBILL ',AL4(IASBILL)                                       
         DC    CL8'IASBILBL',AL4(IASBILBL)                                      
         DC    CL8'IASUNBIL',AL4(IASUNBIL)                                      
         DC    CL8'ODOLS   ',AL4(ODOLS)                                         
         DC    CL8'TGRSNET ',AL4(TGRSNET)                                       
         DC    X'FF'                                                            
*                                                                               
ERRTABLE DC    AL1(ERRPIG,12),CL12'PTNR EST ERR'                                
         DC    AL1(ERRPDT,11),CL11'PAY=CURR MO'                                 
         DC    AL1(ERRBDT,17),CL17'BILL MO = CURR MO'                           
         DC    AL1(ERRGRSUP,29),CL29'GROSS ORDERED NOT= GROSS PAID'             
         DC    AL1(ERRNETUP,25),CL25'NET ORDERED NOT= NET PAID'                 
         DC    AL1(ERRGRSUB,28),CL28'GROSS PAID NOT= GROSS BILLED'              
         DC    AL1(ERRNETUB,24),CL24'NET PAID NOT= NET BILLED'                  
         DC    AL1(0)                                                           
*                                                                               
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
         SPACE 1                                                                
MONTOTD  DSECT                     MONTH TOTAL TABLE DSECT                      
MTMED    DS    CL1                                                              
MTMON    DS    XL2                                                              
MTORD    DS    PL8                                                              
MTNET    DS    PL8                                                              
MTPAID   DS    PL8                                                              
MTNETPD  DS    PL8                                                              
MTUNPD   DS    PL8                                                              
MTNETUP  DS    PL8                                                              
MTBILL   DS    PL8                                                              
MTBILLN  DS    PL8                                                              
MONTOTL  EQU   *-MONTOTD                                                        
*                                                                               
ERRECD   DSECT                     ERROR RECORD TABLE DSECT                     
ERKEY    DS    0C                                                               
ERMED    DS    CL1                                                              
ERCLT    DS    CL3                                                              
ERPRD    DS    CL3                                                              
ERMKT    DS    XL2                                                              
ERSTA    DS    CL5                                                              
ERNET    DS    CL3                                                              
EREST    DS    XL1                                                              
ERKEYL   EQU   *-ERKEY                                                          
ERERR    DS    XL1                                                              
ERRECL   EQU   *-ERRECD                                                         
         SPACE 1                                                                
*                                                                               
*PREFIX=R$                                                                      
QRECORD  DSECT                      SPONSOR STYLE REQUEST CARD                  
QAREA    DS    0CL80   COLUMN                                                   
QPROG    DS    0CL2    ------                                                   
QCODE    DS    CL2        1        PROGRAM CODE                                 
QAGY     DS    CL2        3        AGENCY CODE                                  
QMED     DS    CL1        5        MEDIA CODE (R/T)                             
QCLT     DS    CL3        6        CLIENT CODE                                  
QPGR     DS    CL1        9        PROCESS BY DIVISION                          
QMGR     DS    CL1       10        PROCESS BY DISTRICT                          
QCLOFFC  DS    CL1       11        CLIENT OFFICE FILTER                         
QBYID    EQU   QCLOFFC             C'Y' IF BUYS PROCESSED BY ID                 
QPRD     DS    CL3       12        PRODUCT MNEMONIC                             
QMKT     DS    CL4       15        MARKET NUMBER                                
QSTA     DS    CL5       19        STATION CALL LETTERS                         
QEST     DS    CL3       24        ESTIMATE NUMBER                              
QESTEND  DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
QDEMOVRD DS    CL1       30        Y=DEMO OVERRIDE ACTIVE                       
QCONTREQ DS    CL1       31        C'*' ==> DATA IN QAREA2                      
QSTAUTO  DS    CL3       32        AUTO REQUEST START DATE                      
QENDAUTO DS    CL3       35        AUTO REQUEST END DATE                        
         ORG   QSTAUTO+2                                                        
QDEMNOS  DS    CL4                 DEMO OVERRIDE NUMBERS                        
QSTART   DS    CL6       38        REQUEST START DATE                           
QEND     DS    0CL6      44        REQUEST END DATE                             
QTODAY   DS    CL6       44                                                     
QBOOK1   DS    CL4       50        RATING BOOK (YYMM) GOAL/ESTD DATA            
QHUT1    DS    CL2       54        HUT ADJUSTMENT MONTH                         
QRERATE  DS    CL1       56        RERATE TYPE  I=INVOICE                       
*                                               P=PURCHASED                     
*                                               A=ADJUST ONLY                   
*                                               U=UPGRADE (+Q2BOOK2)            
QCOMPARE DS    CL1       57        DATA COMPARE OPTION                          
*                                  A=GOAL V PURCHASED                           
*                                  B=GOAL V AFFIDAVIT                           
*                                  C=PURCHASED V PURCHASED (RERATED)            
*                                  D=PURCHASED V AFFIDAVIT                      
*                                  E=LOCKIN V PURCHASED                         
*                                  F=LOCKIN V AFFIDAVIT                         
*                                  L=GOAL V PURCHASED, LOCKIN PURCHASED         
QAFFIL   DS    CL1       58        AFFILIATION FILTER                           
QPRGTYPE DS    CL1       59        PROGRAM TYPE FILTER                          
QDPTDET  DS    CL1       60        DAYPART DETAIL CONTROL                       
*                                  A=SHOW FULL DETAIL (DEFAULT)                 
*                                  B=SUPPRESS SPOT-LENGTH                       
*                                  C=SUPPRESS SPOT-LENGTH & DAY-PART            
QDPTMENU DS    CL1       61        DAYPART MENU OVERRIDE                        
QOPT1    DS    CL1       62        OPTION 1                                     
QOPT2    DS    CL1       63        OPTION 2                                     
QOPT3    DS    CL1       64        OPTION 3                                     
QOPT4    DS    CL1       65        OPTION 4                                     
QOPT5    DS    CL1       66        OPTION 5                                     
QGRP     DS    CL2       67        GROUP                                        
QFILTER  EQU   QGRP                FILTER TYPE/VALUE                            
QUESTOR  DS    CL12      69        REQUESTOR NAME                               
*PREFIX=                                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENHIST                                                      
       ++INCLUDE SPGENORHIS                                                     
       ++INCLUDE SPGENUCOM                                                      
       ++INCLUDE SPGENXCOM                                                      
       ++INCLUDE SPGENSTAB                                                      
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENPGEST                                                     
       ++INCLUDE SPGENCSO                                                       
       ++INCLUDE SPGENSTAT                                                      
       ++INCLUDE SPGENMSR                                                       
       ++INCLUDE SPGENBGR                                                       
*PREFIX=S                                                                       
       ++INCLUDE SPGENXLK                                                       
*PREFIX=                                                                        
       ++INCLUDE SPGENUPL                                                       
       ++INCLUDE SPGENAUTH                                                      
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPGENDRMKN                                                     
       ++INCLUDE SPGENDRMKO                                                     
       ++INCLUDE SPGENDRMRC                                                     
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE SPNWSCAM                                                       
       ++INCLUDE SPNWSHDR                                                       
       ++INCLUDE SPNWSDTL                                                       
       ++INCLUDE SPGENSPBL                                                      
       ++INCLUDE SPGENBFML                                                      
       ++INCLUDE SPGENSPLT                                                      
       ++INCLUDE SPGENDRBTC                                                     
       ++INCLUDE SPGENDRFLT                                                     
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF2D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE SPGENBHOLD                                                     
       ++INCLUDE SPGENDREV                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPWRI20   10/24/19'                                      
         END                                                                    
*********************************************************************           
*                                                                   *           
*          SPWRI20 (T20420) - ESTIMATE CLOSEOUTS                    *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 21APR94 00 EFJ -- HISTORY LOST.  LEVEL RESET.  'A' ADDED TO PHASE *           
* 21APR94 02 EFJ -- DON'T DUMP ON PROD CODE NOT FOUND (PER MEL)     *           
* 03OCT94 03 EFJ -- DON'T TRY TO CLOSEOUT EST 0 FOR PGEST (PER TIM) *           
* 08DEC94 04 EFJ -- DMCB USED BY OTHER WRI MODULES... DON'T RELY    *           
*                   ON IT NOT TO CHANGE                             *           
* 17JAN95 05 EFJ -- SET FLAG TO PASS DELETED STAB RECS              *           
* 07SEP95 06 EFJ -- SUPPORT LARGER AGLOBAL                          *           
* 06FEB96 07 ??? -- ???                                             *           
* 05APR96 08 EFJ -- FIX WHATEVER GOT FUCKED UP                      *           
* 29APR96 09 EFJ -- DON'T ALLOW MEDIA '*' REQUESTS                  *           
* 29MAY96 10 EFJ -- SUPPORT READING OF PU PROFILE TO SKIP CLT'S     *           
*                -- STEAL SOME SPACE                                *           
* 11JUN96 11 EFJ -- ADD SKIPPED CLT REPORT                          *           
* 14JAN97 12 EFJ -- MAKE SURE DON'T DEL AGY REC (SOMETHING DID...)  *           
*                -- FIX USING WARNING                               *           
* 28JAN97 13 EFJ -- MAKE SURE KEY ISN'T X'06' AGY REC EITHER...     *           
* 29JAN97 14 EFJ -- MOVE KEY TO SBIOKEY BEFORE DMDEL ON DELETE2!    *           
* 04FEB97 15 EFJ -- NAIL SBTWAACS WITH TWAAUTH                      *           
* 05FEB97 16 EFJ -- CHANGE SF B  *+18...                            *           
* 11AUG98 17 NRK -- CHANGE VALIPER CALLS TO PERVAL.                 *           
* 08DEC98 18 EFJ -- DELETE BUY COPY RECORDS (BUYS W/MED OI X'08')   *           
* 11JAN99 19 EFJ -- INCREASE SKIPPED CLT REC SPACE                  *           
* 08FEB99 20 EFJ -- FIX L18 CHANGES                                 *           
* 08FEB99 21 EFJ -- GET MORE ADDRESSABILITY                         *           
* 14APR99 22 EFJ -- INCREASE SKIPPED CLT REC SPACE                  *           
* 24JUN99 23 SEAN-- CLOSE MSR, BGR, DARE RECORDS ALSO               *           
* 14DEC99 24 SEAN-- CLOSE PW RECORDS ALSO                           *           
* XXJAN01 25 YKVA-- CLOSE XLK, UPL, AUTH RECORDS ALSO               *           
*-------------------------------------------------------------------*           
*--------------------  LEVEL RESET SOMEWHERE  ----------------------*           
*-------------------------------------------------------------------*           
* 12JUN00 09 EFJ -- OPTION TO SKIP PU PROFILE                       *           
* 18OCT00 10 BPLA-- IGNORE RETAIL SUMMARY AND AOR BILLS             *           
* 06AUG01 12 EFJ -- GET DYNALLOC FROM TWADCONS                      *           
*                -- MOVE DCB TO SPFUSER!                            *           
* 22AUG01 13 EFJ -- FIX CLOSING FILE                                *           
* 07SEP01 14 EFJ -- L NOT LA YSFI!                                  *           
* 13NOV01 16 TZC -- CLOSE UCOM, XCOM, BUY HISTORY RECORDS           *           
* 08AUG02 17 EFJ -- INCREASE CLTSKIP TABLE                          *           
* 01APR03 19 ??? -- ???                                             *           
*   OCT03 20 WHO -- DARE                                            *           
* 14OCT03 21 PWE -- CLOSE CANADIAN ONLY RECS - NETDEF/CBLPRO        *           
* 17OCT03 22 PWE -- REFINE LEVEL 21 CODE - ALWAYS INIT COUNTERS     *           
* 17OCT03 23 PWE -- COVAIL MONTOTS - LVLS 20-22 MADE PHASE TOO BIG  *           
* 10NOV03 24 EFJ -- FIX NWS MKT REC DELETES (SET RD FOR DELETE)     *           
* 16JAN04 25 EFJ -- DON'T REPORT ON PW/C2 BILL RECS                 *           
* 25MAY04 26 EFJ -- USE BVAL FOR BILL $                             *           
*                -- MAKE SURE TO INCLUDE ETYPE ESTIMATES            *           
* 06JUL04 27 EFJ -- USE BVAL FOR BILL $  (MISSED A SPOT!)           *           
* 01DEC04 28 AKT -- COMMENT RECS NEVER LOOKED AT EST TABLE PROPERLY *           
*                   SINCE THEY WERE ADDED ON 13NOV01 (LEVEL 16)     *           
* 04FEB05 29 EFJ -- CLOSE OUT MEDIA 'C' GOALS                       *           
* 06JUN05 30 EFJ -- FIX BWS BUG (BYR > X'FF')                       *           
*                -- CLOSE OUT ORDER HISTORY RECS                    *           
*                -- CLOSE OUT BILL FORMULA RECORDS                  *           
*                -- CLOSE OUT SPLIT BILL RECORDS                    *           
*********************************************************************           
