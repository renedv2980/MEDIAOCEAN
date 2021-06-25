*          DATA SET SPREPX302  AT LEVEL 069 AS OF 03/24/15                      
*PHASE SPX302T                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPREPX302 - CHILD SPOT PERFORMANCE'                             
***********************************************************************         
* PROGRAM CHANGES                                                               
* LEV 54    APR08/87 SET PRODUCT SUMMARIES ONLY SWITCH                          
* LEV 55    APR08/87 FIX ONLY 1 STATION BUG                                     
* LEV 56    SEP03/87 FIX ONLY 1 MARKET BUG                                      
* LEV 57    APR04/89 SUPPRESS TOTAL LINE IF ONLY ONE DPT/SLN                    
*                                                                               
***********************************************************************         
* QOPT1 = Y REQUESTS REPORTS BY MONTHS                                          
* QOPT2 = Y REQUESTS REPORTS BY FLIGHTS                                         
* QOPT3 = Y REQUESTS REPORTS BY QUARTERS                                        
* QOPT4 = Y REQUESTS REPORTS BY YEAR                                            
* QOPT5 = Y MEANS 'PRINT ALL BRANDS'                                            
* QFILTER Y MEANS ONLY PRINT PRODUCT SUMMARY                                    
         SPACE 1                                                                
* USRSW1 = N MEANS A NEW PRODUCT WHEN BUILDING THE FLIGHT TABLE                 
* USRSW2 = Y MEANS THAT THIS IS A DUMMY FLIGHT TO THE HEADHOOK ROUTINE          
         SPACE 1                                                                
* MEDIA SUMMARY BUFFALO LEVELS                                                  
*    COLUMN DEFINITION 1  (DETAIL)                                              
*        LEVEL 1 = DETAIL ITEMS                                                 
*        LEVEL 2 = MARKET GROUP 3  (MGR3)                                       
*        LEVEL 3 = MARKET GROUP 2  (MGR2)                                       
*        LEVEL 4 = MARKET GROUP 1  (MGR1)                                       
*        LEVEL 5 = PRODUCT TOTALS                                               
*    COLUMN DEFINITION 2  (PRIMARY DEMO)                                        
*        LEVEL 1 = PRODUCT GROUP 3                                              
*        LEVEL 2 = PRODUCT GROUP 2                                              
*        LEVEL 3 = PRODUCT GROUP 1                                              
*        LEVEL 4 = CLIENT                                                       
*    COLUMN DEFINITION 3  (CLIENT)                                              
*        LEVEL 1 = PRODUCT GROUP 3                                              
*        LEVEL 2 = PRODUCT GROUP 2                                              
*        LEVEL 3 = PRODUCT GROUP 1                                              
*        LEVEL 4 = CLIENT                                                       
*    RECORD TYPE 1 = DETAIL , 2 = PRIMARY DEMO                                  
         SPACE 1                                                                
* SPROG SETTINGS                                                                
*        1 = PRIMARY DEMO = RATING                                              
*        2 = PRIMARY DEMO = IMPRESSIONS                                         
*        3 = PRODUCT SUMMARY   = RATING                                         
*        4 = PRODUCT SUMMARY   = IMPRESSIONS                                    
*        5 = PRD GROUP SUMMARY - PRIMARY DEMO = RATING                          
*        6 = PRD GROUP SUMMARY - PRIMARY DEMO = IMPRESSIONS                     
*        7 = CLIENT SUMMARY                                                     
*        8 = CLIENT SUMMARY                                                     
         SPACE 2                                                                
SPX302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPX302                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPX302+4096,RC                                                   
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         STM   R9,RC,SP12R9                                                     
         SPACE 1                                                                
         XC    SRREC,SRREC                                                      
         MVC   SRMODE,MODE                                                      
         EJECT                                                                  
* CONTROL SECTION *                                                             
         SPACE 1                                                                
         CLI   MODE,PROCBUY                                                     
         BE    SP700               BUILD BUFFALO RECORDS                        
         BH    CNTRL10             CHECK 'LAST' BREAKS                          
         SPACE 1                                                                
* 'FRST' BREAKS *                                                               
         SPACE 1                                                                
         CLI   MODE,MKTFRST                                                     
         BH    SP600               WRITE STATION INFO RECORDS                   
         BE    SP500               WRITE MARKET INFO RECORDS                    
         CLI   MODE,ESTFRST                                                     
         BH    SP400               WRITE MKT GRP INFO RECORDS                   
         BE    SP200               BUILD PRDBUFF, DATES AND TABLES              
         CLI   MODE,CLTFRST                                                     
         BH    EXIT                                                             
         BE    SP100               BUILD PRD GRP TABLE (PGRTBL)                 
         CLI   MODE,RUNFRST                                                     
         BH    SP050               INITIALIZATION                               
         B     SP000               INITIALIZE THE MEDBLOCK                      
         SPACE 1                                                                
* 'LAST' BREAKS *                                                               
         SPACE 1                                                                
CNTRL10  DS    0H                                                               
         CLI   MODE,STALAST                                                     
         BL    EXIT                                                             
         BE    SP800               PUT DATA RECORDS TO SORTER                   
         CLI   MODE,MKTLAST                                                     
         BE    SP900               PUT MARKET LEVEL TOTALS TO SORTER            
         CLI   MODE,ESTLAST                                                     
         BL    SP1000              PUT MKT GRP LEVEL TOTALS TO SORTER           
         CLI   MODE,PRDLAST                                                     
         BE    SP1100              PUT PRODUCT LEVEL TOTALS TO SORTER           
         CLI   MODE,CLTLAST                                                     
         BE    SP1500              PRINT THE REPORT                             
         CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         MVI   SPUT00+1,0          PREPARE SORTER TO BE RE-INITIALIZED          
         GOTO1 VSORTER,DMCB,=C'END'                                             
EXIT     XIT1                                                                   
         SPACE 1                                                                
PATCH1   DS    CL16                                                             
         EJECT                                                                  
* SP000 - AT RUNFRST WE INITIALIZE THE MEDBLOCK AND SET UP TABLES *             
         SPACE 1                                                                
SP000    DS    0H                                                               
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMQT,=F'5'                                                   
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   MEDEXTCS,C'Y'                                                    
         MVI   MEDEXTDM,4                                                       
         DROP  RE                                                               
         SPACE 1                                                                
         RELOC RELO                                                             
         LA    RF,MYHEAD                                                        
         ST    RF,HEADHOOK                                                      
         L     RF,=V(SORTBUF)                                                   
         ST    RF,VSORTBUF                                                      
         L     RF,=V(FLTTBL)                                                    
         ST    RF,VFLTTBL                                                       
         L     RF,=V(PGRTBL)                                                    
         ST    RF,VPGRTBL                                                       
         MVC   VSORTER,=V(SORTER)                                               
         LA    RF,MYBUFIO                                                       
         ST    RF,BUFFIO                                                        
         L     R2,=V(BUFFALOC)                                                  
         ST    R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',(R2)                                        
         L     RF,=V(MPTBL)                                                     
         ST    RF,VMPTBL                                                        
         L     RF,=V(SPTBL)                                                     
         ST    RF,VSPTBL                                                        
         L     RF,=V(MGPTBL)                                                    
         ST    RF,VMGPTBL                                                       
         L     RF,=V(SETPRMY)                                                   
         ST    RF,VSETPRMY                                                      
         L     RF,=V(EIEQU)                                                     
         ST    RF,VEIEQU                                                        
         L     RF,=V(IEEQU)                                                     
         ST    RF,VIEEQU                                                        
         L     RF,=V(NEWDNAM)                                                   
         ST    RF,VNEWDNAM                                                      
         SPACE 1                                                                
         GOTO1 MEDSEED,DMCB,(RA)   SET UP REPORT TABLES                         
         L     RE,MEDTABLE         SET UP CPP ADDRESSES                         
         LA    R4,CPPCTRL                                                       
SP010    DS    0H                                                               
         ZIC   R5,0(R4)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         AR    R5,RE               R5 POINTS TO REPORT                          
         L     R6,0(R5)            R6 POINTS TO REPORT DEFN                     
         MVC   1(3,R4),5(R6)       SAVE ORIGINAL COLUMN DEFN                    
         LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   SP010                                                            
         MVI   CPPSW,0             DEFAULT IS NO CROSS-DAYPART CPP/M            
         B     EXIT                                                             
         SPACE 2                                                                
* SP050 - REQFRST INITIALIZATION *                                              
         SPACE 1                                                                
SP050    DS    0H                                                               
         MVI   MYDSCNTR,0                                                       
         CLC   =C'ACT NOA',QBOOK1  IS THIS A 'NO ADJUSTMENT' ADJUSTMENT         
         BNE   *+8                  NO.                                         
         MVI   QRERATE,C'P'         YES. MAKE REQUEST REASONABLE                
         MVC   PGRACTSW,QPGR       SET PRD GRP ACTIVITY SWITCH                  
         MVI   QPGR,C' '           CLEAR QPGR FOR SPONSOR                       
         MVI   ESTSW,C'N'          NEW REQUEST                                  
         MVI   SUPTOTSW,C'N'                                                    
         MVC   RQDPOVRD,QDPTMENU   OVERRIDE DAYPART MENU                        
         MVC   BRANDSW,QOPT5       SET 'PRINT ALL BRANDS' SWITCH                
         MVI   QOPT5,C' '                                                       
         MVI   PUTSW,C'N'          NO RECORDS PUT TO SORTER YET                 
         XC    SPOTHOOK,SPOTHOOK                                                
         CLC   QPRD,=C'POL'                                                     
         BE    SP070                                                            
         CLC   QPRD,=C'ALL'                                                     
         BE    SP060                                                            
         CLI   QPRD,C'0'           IS THIS A PRDGRP NUMBER?                     
         BL    SP070                NO.                                         
         PACK  DUB,QPRD                                                         
         MVC   SAVPGR,DUB+6                                                     
         NI    SAVPGR+1,X'F0'                                                   
SP060    DS    0H                                                               
         MVC   QPRD,=C'POL'                                                     
SP070    DS    0H                                                               
         LA    R0,10                                                            
         LA    R2,CNTRS                                                         
         ZAP   0(4,R2),=P'0'                                                    
         LA    R2,4(R2)                                                         
         BCT   R0,*-10                                                          
         EJECT                                                                  
* SET THE DEMO TYPE *                                                           
         SPACE 1                                                                
         LA    R4,2                                                             
         CLI   QRERATE,C' '                                                     
         BE    SP090                                                            
         CLI   QRERATE,C'A'                                                     
         BNE   SP080                                                            
         LA    R4,5                ADJUST ONLY                                  
         B     SP090                                                            
SP080    DS    0H                                                               
         LA    R4,3                SET FOR PURCHASE RERATED                     
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    R4,1(R4)            SET FOR ADJUSTMENT                           
         CLI   QRERATE,C'I'                                                     
         BNE   SP090                                                            
         LA    R4,3(R4)            RERATE BASED ON INVOICE                      
SP090    DS    0H                                                               
         ST    R4,DEMOTYPE                                                      
         CLI   QOPT1,C'Y'                                                       
         BE    SP095                                                            
         CLI   QOPT2,C'Y'                                                       
         BE    SP095                                                            
         CLI   QOPT3,C'Y'                                                       
         BE    SP095                                                            
         MVI   QOPT4,C'Y'                                                       
SP095    DS    0H                                                               
         MVI   GREYSW,C'N'                                                      
         CLC   =CL4'GREY',QUESTOR  IS THE REQUESTOR = GREY?                     
         BNE   EXIT                 NO                                          
         MVI   GREYSW,C'Y'          YES.                                        
         B     EXIT                                                             
         SPACE 2                                                                
* SP100 - AT CLTFRST, INITIALIZE PGRTBL *                                       
         SPACE 1                                                                
SP100    DS    0H                                                               
         L     R3,VPGRTBL          START OF THE PRD GRP TABLE                   
         LA    R4,2                INCREMENT                                    
         LA    R5,434(R3)          END OF TABLE (MINUS UNA & POL)               
         MVC   0(2,R3),=X'9990'    DEFAULT PRD GRP ID IS 999                    
         BXLE  R3,R4,*-6                                                        
         MVC   0(4,R3),=4X'FF'     FORCE UNA & POL RECORDS HIGH                 
         B     EXIT                                                             
         EJECT                                                                  
* SP200 - AT ESTFRST BUILD THE PRDBUFF, THE MEDBLOCK DATES, *                   
*  FLTTBL AND PGRTBL AND WRITE THE PRODUCT INFO RECORDS     *                   
         SPACE 1                                                                
SP200    DS    0H                                                               
         CLI   ESTSW,C'Y'          IS THIS AN OLD REQUEST?                      
         BE    EXIT                 NO.                                         
         MVI   ESTSW,C'Y'           YES. LOCK MEDBLOCK DATES FOR REQ.           
         MVC   RQSTAFLT,QAFFIL     AFFILIATE FILTER                             
         MVC   RQPRGTYP,QPRGTYPE   PROGRAM TYPE FILTER                          
         MVC   PAGE,=H'1'                                                       
         SPACE 1                                                                
         L     R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R2)                                      
         SPACE 1                                                                
         GOTO1 MEDCLEAR,DMCB,MEDTABLE                                           
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         SPACE 1                                                                
* BUILD THE PRODUCT GROUP TABLE *                                               
         SPACE 1                                                                
         CLI   PGRACTSW,C' '       DO WE NEED TO UPDATE PGRTBL?                 
         BE    SP260                NO.                                         
         LA    RF,MYSPOTHK                                                      
         ST    RF,SPOTHOOK                                                      
         MVC   SAVPRD,=3X'FF'                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'     PASSIVE PRDGRP KEYS                          
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         MVC   KEY+3(2),SVCLT      CLT                                          
         MVC   KEY+5(1),PGRACTSW   PRD GRP ID                                   
         MVC   KEY+6(2),SAVPGR     PRDGRP NUMBER                                
         GOTO1 HIGH                                                             
SP210    DS    0H                                                               
         CLC   KEY(8),KEYSAVE      SAME 0D81/A-M/CLT/PGRP ID/PGRP NUM           
         BNE   SP250                NO.                                         
         LA    R0,218                                                           
         L     R2,PRDBUFF                                                       
SP220    DS    0H                                                               
         CLC   KEY+8(3),1(R2)      FIND PRD IN PRDBUFF                          
         BE    SP230                                                            
         AH    R2,PRDBUFLN         POINT TO NEXT PRDBUFF ENTRY                  
         BCT   R0,SP220                                                         
         B     SP240                                                            
         SPACE 1                                                                
SP230    DS    0H                                                               
         CLC   SAVPRD,1(R2)        GET THE FIRST PRD ALPHABETICALLY             
*********BL    *+14                                                             
         BL    *+10                DEIS: MAR20/2015                             
         MVC   SAVPRD,1(R2)                                                     
         MVC   MYBYTE,0(R2)                                                     
         ZIC   RF,0(R2)                                                         
         BCTR  RF,0                                                             
         AR    RF,RF                                                            
         A     RF,VPGRTBL          GET THE DISP INTO PRD GRP TABLE              
         MVC   0(2,RF),KEY+6       UPDATE PRDGRP NUMBER                         
SP240    DS    0H                                                               
         GOTO1 SEQ                                                              
         B     SP210                                                            
         SPACE 1                                                                
SP250    DS    0H                                                               
         ZIC   RF,MYBYTE                                                        
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN         POINT RF TO 1ST (ALPHA) PRODUCT              
         A     RF,PRDBUFF                                                       
         LA    RE,219                                                           
         MH    RE,PRDBUFLN         POINT RE TO POL                              
         A     RE,PRDBUFF                                                       
         MVC   28(12,RE),28(RF)    USE THE 1ST (ALPHA) PRODUCT'S DEMOS          
         SPACE 1                                                                
* BUILD THE FLIGHT TABLE *                                                      
         SPACE 1                                                                
SP260    DS    0H                                                               
         CLI   QOPT2,C'Y'          DO WE WANT REPORT BY FLIGHTS?                
         BNE   SP340                                                            
         MVI   QOPT2,C'N'          CANCEL FLT REQ UNLESS FLT REC FND            
         MVI   USRSW1,C'N'         SET PRODUCT SWITCH = NEW                     
         BAS   RE,SPMONDAY         MODIFY REQ TO START ON MONDAY, ETC           
         MVC   AREC,ADBUY                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D0D'     FLIGHT RECORDS                               
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         MVC   KEY+3(2),SVCLT      CLIENT                                       
         PACK  DUB,QSTART(2)                                                    
         CVB   RF,DUB                                                           
         STC   RF,SAVYR            SAVE THE YEAR                                
         LA    R0,218              NO POL OR UNA FLIGHTS                        
         L     R2,PRDBUFF                                                       
         L     R3,VFLTTBL          R3 POINTS TO FLTTBL                          
SP270    DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    SP330               TRY THE NEXT PRODUCT                         
         MVC   KEY+5(3),1(R2)      EBCDIC PRD CODE                              
         MVC   KEY+8(1),SAVYR      YEAR                                         
         XC    KEY+9(4),KEY+9      (EST NOT USED)                               
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   SP330               TRY THE NEXT PRODUCT                         
         GOTO1 GET                                                              
         L     R4,AREC                                                          
         LA    R4,24(R4)                                                        
         CLI   0(R4),1                                                          
         BE    SP280                                                            
         DC    H'0'                MUST FIND AN 01 ELEMENT                      
SP280    DS    0H                                                               
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         CLI   0(R4),5                                                          
         BE    SP290                                                            
         CLI   0(R4),0                                                          
         BNE   SP280                                                            
         B     SP320               CHECK IF 'LAST' DUMMY FLT IS NEEDED          
         SPACE 1                                                                
         USING FLTEL05,R4                                                       
SP290    DS    0H                                                               
         CLI   USRSW1,C'N'         IS THIS A NEW PRODUCT?                       
         BNE   SP300                NO. CHECK FOR GAPS BETWEEN FLIGHTS          
         MVI   QOPT2,C'Y'          REINSTATE THE FLIGHT REQ                     
         MVI   BRANDSW,C'Y'        SET 'PRINT ALL BRANDS' SWITCH                
         MVI   USRSW1,C'Y'                                                      
         CLC   CMPST,FLTST         DOES 1ST FLT COVER BEGINNING OF REQ?         
         BNL   SP310                YES. BUILD THE 1ST REAL FLIGHT              
         MVC   DATE,CMPST                                                       
         BAS   RE,SPSUBDAY          NO. SUBTRACT A DAY FROM FLTST               
         BAS   RE,SPDUMMY          BUILD A DUMMY FLIGHT                         
         LA    R3,9(R3)                                                         
         B     SP310               BUILD NEXT REAL FLIGHT                       
         SPACE 1                                                                
SP300    DS    0H                                                               
         BAS   RE,SPADDAY          ADD A DAY TO COMPRESSED DATE                 
         CLC   DATE,FLTST          IS THERE A GAP BETWEEN FLIGHTS               
         BE    SP310                NO. BUILD THE NEXT REAL FLIGHT              
         BAS   RE,SPSUBDAY          YES. SUBTRACT A DAY FROM FLTST              
         BAS   RE,SPDUMMY          BUILD A DUMMY FLIGHT                         
         LA    R3,9(R3)                                                         
SP310    DS    0H                                                               
         MVC   0(4,R3),0(R2)       PRD CODE/EBCDIC PRD CODE                     
         MVC   4(5,R3),FLTNO       FLT NUMBER/FLT START & END DATES             
         MVC   DATE,FLTEND                                                      
         LA    R3,9(R3)            POINT TO NEXT FLTTBL SLOT                    
         B     SP280               CHECK THE NEXT ELEMENT                       
         SPACE 1                                                                
SP320    DS    0H                                                               
         MVI   USRSW1,C'N'         SET PRODUCT SWITCH = NEW                     
         CLC   DATE,LASTSUN        IS REQ END DATE COVERED BY LAST FLT?         
         BNL   SP330                YES.                                        
         BAS   RE,SPADDAY          ADD A DAY TO COMPRESSED DATE                 
         MVC   HALF,CMPEND         USE THE REQUEST END DATE                     
         BAS   RE,SPDUMMY                                                       
         LA    R3,9(R3)                                                         
SP330    DS    0H                                                               
         AH    R2,PRDBUFLN         POINT TO NEXT PRDBUFF ENTRY                  
         BCT   R0,SP270                                                         
         MVI   0(R3),X'FF'         END OF FLTTBL - NO POOL FLTS ALLOWED         
         MVC   KEY,SVKEY                                                        
         DROP  R4                                                               
         CLI   QOPT2,C'Y'                                                       
         BE    SP340                                                            
         XC    P,P                                                              
         MVC   P+10(16),=C'NO FLIGHTS FOUND'                                    
         GOTO1 REPORT                                                           
         EJECT                                                                  
* BUILD THE PRODUCT INFO RECORD *                                               
         SPACE 1                                                                
SP340    DS    0H                                                               
         MVI   SRMODE,PRDFRST      MODE                                         
         ZAP   NUMREC,=P'0'                                                     
         LA    R0,220                                                           
         L     R2,PRDBUFF                                                       
SP350    DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    SP370                                                            
         MVC   SRPRD,1(R2)         PRD - EBCDIC                                 
         ZIC   RF,0(R2)                                                         
         STC   RF,SRPRDN           PRD - BINARY                                 
         CLI   SRPRDN,X'FF'                                                     
         BNE   *+8                                                              
         LA    RF,220              POL IS THE 220TH ENTRY                       
         BCTR  RF,0                                                             
         AR    RF,RF                                                            
         A     RF,VPGRTBL          DISP INTO PGR TBL                            
         CLI   PGRACTSW,C' '       ARE WE PROCESSING BY PRD GRP?                
         BE    SP360                NO.                                         
         CLI   SRPRDN,X'FF'        IS THE PRODUCT POL?                          
         BE    SP360                YES                                         
         CLC   SAVPGR,0(RF)        IS THIS PRD IN THE RIGHT PRD GRP?            
         BNE   SP370                NO. DO THE NEXT PRODUCT                     
SP360    DS    0H                                                               
         MVC   SRPGR,0(RF)         PRD GRP                                      
         CLI   SRMODE,PRDFRST                                                   
         BNE   SP370                                                            
         MVC   SRDATA(28),0(R2)    PRD/EBCDIC PRD CODE/PRD NAME                 
         BAS   RE,SRRPTSET         SET SRRPT, SRST & SREND                      
SP370    DS    0H                                                               
         AH    R2,PRDBUFLN         POINT TO NEXT PRDBUFF ENTRY                  
         BCT   R0,SP350                                                         
         AP    PREC,NUMREC                                                      
         SPACE 1                                                                
* SET UP CROSS DAYPART CPP/M OPTIONS                                            
         SPACE 1                                                                
         L     RE,MEDTABLE                                                      
         LA    R4,CPPCTRL                                                       
SETCPP   SR    R5,R5                                                            
         IC    R5,4(R4)            GET CONDITIONAL REPORT NUMBER                
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         AR    R5,RE               POINT TO CONDITIONAL REPORT                  
         L     R6,0(R5)            POINT TO CONDITIONAL ROW DEF                 
         L     RF,4(R4)                                                         
         CLI   CPPSW,0                                                          
         BE    *+8                                                              
         L     RF,0(R4)            GET CPP COL DEFN                             
         LA    RF,0(RF)            CLEAR CONTROL BYTE                           
         ST    RF,4(R6)            SET COL DEFN ADDR                            
         LA    R4,8(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   SETCPP                                                           
         L     RE,MEDTABLE                                                      
RESETB   NI    0(RE),X'7F'         RESET MEDTABLE                               
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RESETB                                                           
         SPACE 1                                                                
* OPTIMIZE BUFFALO                                                              
         SPACE 1                                                                
         L     RE,MEDTABLE                                                      
         LA    RE,16(RE)           BYPASS DETAIL LINES                          
RESETPC  OI    0(RE),X'80'         RESET PRODUCT AND CLIENT CODES               
         CLI   0(RE),X'FF'                                                      
         BE    CHKSL                                                            
         LA    RE,4(RE)                                                         
         B     RESETPC                                                          
         SPACE 1                                                                
* DAYPART SPOT LENGTH SUPPRESSION ROUTINES                                      
         SPACE 1                                                                
CHKSL    CLI   QDPTDET,C' '                                                     
         BNE   *+8                                                              
         MVI   QDPTDET,C'Y'        GIVE A TOTAL LINE                            
*                                                                               
         CLI   QDPTDET,C'B'        SUPPRESS SPOT LENGTH                         
         BE    *+12                                                             
         CLI   QDPTDET,C'C'                                                     
         BNE   SP380                                                            
         L     RE,MEDTABLE         DEACTIVATE SPOT LENGTH REPORTS               
         OI    0(RE),X'80'                                                      
         LA    RE,16(RE)                                                        
         OI    0(RE),X'80'                                                      
         LA    RE,16(RE)                                                        
         OI    0(RE),X'80'                                                      
         SPACE 1                                                                
CHKDP    CLI   QDPTDET,C'C'        SUPPRESS DAYPART                             
         BNE   SP380                                                            
         LA    RF,3                DEACTIVATE DAYPART REPORTS                   
         L     RE,MEDTABLE                                                      
DPT1     OI    4(RE),X'80'                                                      
         OI    8(RE),X'80'                                                      
         LA    RE,16(RE)                                                        
         BCT   RF,DPT1                                                          
         SPACE 1                                                                
SP380    DS    0H                                                               
         CLI   RQDPOVRD,C' '                                                    
         BNE   M21EX                                                            
         CLI   QDPTDET,C' '                                                     
         BNE   M21EX                                                            
         MVI   SUPTOTSW,C'Y'                                                    
         LA    R0,3                                                             
         L     RE,MEDTABLE                                                      
SP390    DS    0H                                                               
         OI    4(RE),X'80'         SUPPRESS DAYPART TOTAL                       
         OI    8(RE),X'80'         SUPPRESS DAYPART GROUP TOTAL                 
         OI    12(RE),X'80'        SUPPRESS TOTAL                               
         LA    RE,16(RE)                                                        
         BCT   R0,SP390                                                         
         SPACE 1                                                                
M21EX    GOTO1 =V(SETBUF),DMCB,(RA),LCODE,LVCNTRL,RR=RELO                       
         B     EXIT                                                             
         EJECT                                                                  
* SP400 - AT MGR1FRST, RESET MKTGRP ACTIVITY SWITCH & ZERO MGPTBL *             
         SPACE 1                                                                
SP400    DS    0H                                                               
         CLI   MODE,MGR1FRST                                                    
         BNE   EXIT                                                             
         MVI   MGPACTSW,0                                                       
         L     RF,VMGPTBL                                                       
         XC    0(220,RF),0(RF)                                                  
         B     EXIT                                                             
         SPACE 2                                                                
* SP500 - AT MKTFRST, RESET MKT ACTIVITY SWITCH AND ZERO MPTBL *                
         SPACE 1                                                                
SP500    DS    0H                                                               
         MVI   MKTACTSW,0                                                       
         L     RF,VMPTBL                                                        
         XC    0(220,RF),0(RF)                                                  
         B     EXIT                                                             
         SPACE 2                                                                
* SP600 - AT STAFRST, RESET STATION ACTIVITY SWITCH AND ZERO SPTBL *            
         SPACE 1                                                                
SP600    DS    0H                                                               
         MVI   STAACTSW,0                                                       
         L     RF,VSPTBL                                                        
         XC    0(220,RF),0(RF)                                                  
         B     EXIT                                                             
         EJECT                                                                  
* SP700 - AT PROCBUY BUILD THE BUFFALO RECORDS *                                
         SPACE 1                                                                
SP700    DS    0H                                                               
         XC    PSLIST,PSLIST                                                    
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
         LA    R2,PSLIST                                                        
         CLI   KEY+3,X'FF'         IS THIS A POOL BUY?                          
         BE    SP720                YES.                                        
         SPACE 1                                                                
* BRAND BUY *                                                                   
         SPACE 1                                                                
SP710    DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    EXIT                                                             
         CLC   0(1,R2),KEY+3                                                    
         BE    SP740                                                            
         LA    R2,2(R2)                                                         
         B     SP710                                                            
         SPACE 1                                                                
* POOL BUY *                                                                    
         SPACE 1                                                                
SP720    DS    0H                                                               
         CLI   BRANDSW,C'Y'        IS THIS A 'PRINT ALL BRANDS' REQUEST         
         BNE   SP730                NO. JUST DO POL                             
SP725    DS    0H                                                               
         CLI   0(R2),0             ARE WE DONE WITH BRANDS YET?                 
         BE    SP730                YES. DO POL                                 
         CLI   0(R2),X'FF'         HAVE WE DONE THIS PRODUCT ALREADY?           
         BNE   SP740                NO.                                         
         LA    R2,2(R2)                                                         
         B     SP725                                                            
         SPACE 1                                                                
SP730    DS    0H                                                               
         MVI   USRSW2,C'Y'                                                      
         MVI   0(R2),220                                                        
         L     RF,ADBUY                                                         
         USING BUYREC,RF                                                        
         MVC   1(1,R2),BDSEC                                                    
         B     SP750                                                            
         DROP  RF                                                               
         SPACE 1                                                                
SP740    DS    0H                                                               
         CLI   PGRACTSW,C' '       ARE WE PROCESSING BY PRDGRP?                 
         BE    SP750                NO.                                         
         ZIC   RF,0(R2)                                                         
         BCTR  RF,0                                                             
         AR    RF,RF                                                            
         A     RF,VPGRTBL                                                       
         CLC   SAVPGR,0(RF)        IS THIS THE RIGHT PRDGRP ID?                 
         BE    SP750                YES.                                        
         MVI   0(R2),X'FF'          NO. SKIP THIS PRODUCT                       
         B     SP780                                                            
         EJECT                                                                  
SP750    DS    0H                                                               
         L     RE,PRDBUFF                                                       
         ZIC   RF,0(R2)                                                         
         STC   RF,SRPRDN           PRD - BINARY                                 
         CLI   SRPRDN,220                                                       
         BNE   *+8                                                              
         MVI   SRPRDN,X'FF'        POL IS BINARY PRODUCT CODE X'FF'             
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         AR    RE,RF                                                            
         MVC   SRPRD,1(RE)         PRD - EBCDIC                                 
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         CLI   MEDBRAND,220                                                     
         BNE   *+12                                                             
         MVI   MEDBRAND,X'FF'                                                   
         MVI   MEDSPTLN,0                                                       
         MVI   0(R2),X'FF'                                                      
         L     R4,DEMOTYPE                                                      
         GOTO1 MEDGETBY,DMCB,(RA),(R4)                                          
         MVC   MEDSPTLN,1(R2)                                                   
         GOTO1 VSETPRMY,DMCB,(RA)                                               
         L     R4,MEDPERD+4                                                     
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   IS THERE ANY ACTIVITY                        
         BZ    SP780                NO. SKIP THIS BUY                           
         ZIC   RF,MEDBRAND         GET ACTIVE BRAND                             
         CLI   MEDBRAND,X'FF'                                                   
         BNE   *+8                                                              
         LA    RF,220                                                           
         BCTR  RF,0                                                             
         L     RE,VMPTBL                                                        
         AR    RE,RF                                                            
         MVI   0(RE),1             MARK THIS PRODUCT ACTIVE IN MPTBL            
         L     RE,VSPTBL                                                        
         AR    RE,RF                                                            
         MVI   0(RE),1             MARK THIS PRODUCT ACTIVE IN SPTBL            
         L     RE,VMGPTBL                                                       
         AR    RE,RF                                                            
         MVI   0(RE),1             MARK THIS PRODUCT ACTIVE IN MGPTBL           
         DROP  R4                                                               
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         CLI   QOPT2,C'Y'          DO WE WANT REPORTS BY FLIGHTS?               
         BNE   *+8                  NO.                                         
         BAS   RE,SPUTFLT                                                       
         CLI   QOPT1,C'Y'          DO WE WANT REPORTS BY MONTHS?                
         BNE   SP760                NO.                                         
         MVI   SRRPT,X'20'         REPORT BY MONTH                              
         LA    R4,MEDMON01                                                      
         BAS   RE,SPUTPER                                                       
SP760    DS    0H                                                               
         CLI   QOPT3,C'Y'          DO WE WANT REPORTS BY QUARTERS?              
         BNE   SP770                NO.                                         
         MVI   SRRPT,X'60'         REPORT BY QUARTER                            
         LA    R4,MEDQRT01                                                      
         BAS   RE,SPUTPER                                                       
SP770    DS    0H                                                               
         CLI   QOPT4,C'Y'          DO WE WANT REPORTS BY YEAR?                  
         BNE   SP780                NO.                                         
         MVI   SRRPT,X'80'         REPORT BY YEAR                               
         LA    R4,MEDPERD                                                       
         BAS   RE,SPUTPER                                                       
SP780    DS    0H                                                               
         CLI   KEY+3,X'FF'         IS THIS A POOL BUY                           
         BNE   EXIT                 NO.                                         
         XC    SRREC,SRREC                                                      
         MVI   SRMODE,PROCBUY                                                   
         CLI   USRSW2,C'Y'                                                      
         BNE   SP725                                                            
         MVI   USRSW2,C'N'                                                      
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* SP800 - AT STALAST, PUT (BUFFALOED) DATA RECORDS OUT TO SORTER *              
         SPACE 1                                                                
SP800    DS    0H                                                               
         CLI   STAACTSW,0                                                       
         BE    EXIT                                                             
         CLI   STA+4,C' '                                                       
         BNE   *+8                                                              
         MVI   STA+4,C'T'                                                       
         GOTO1 MSPACK,DMCB,MKT,STA,SVMSTA                                       
         MVC   SRMGR,BMGR+1        MKT GRP NUMBER                               
         MVC   SRMKT(5),SVMSTA     MKT/STA                                      
         MVC   SRDATA(12),STA      STATION CALL LETTERS/EXPANDED CALL           
         MVI   SRMODE,STAFRST                                                   
         BAS   RE,SRRECSET         SET SRPGR, SRPRD, SRPRDN & SRRPT             
         AP    SREC,NUMREC                                                      
         SPACE 1                                                                
         XC    MYBUFIO,MYBUFIO                                                  
         L     R3,BUFFBUFF                                                      
         L     R4,BUFFIO                                                        
         XC    0(20,R4),0(R4)                                                   
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R3),(R4),1                                
         B     SP820                                                            
         SPACE 1                                                                
SP810    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),1                                 
SP820    DS    0H                                                               
         TM    DMCB+8,X'80'                                                     
         BO    SP830                                                            
         MVC   SRREC,0(R4)                                                      
         ZIC   RF,SRPRDN                                                        
         CLI   SRPRDN,X'FF'                                                     
         BNE   *+8                                                              
         LA    RF,220                                                           
         BCTR  RF,0                                                             
         AR    RF,RF                                                            
         A     RF,VPGRTBL                                                       
         MVC   SRPGR,0(RF)                                                      
         MVC   SRMGR,BMGR+1        MARKET GROUP                                 
         MVC   SRMKT(5),SVMSTA     MKT/STA                                      
         GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         AP    BREC,=P'1'                                                       
         MVI   PUTSW,C'Y'                                                       
         B     SP810                                                            
         SPACE 1                                                                
SP830    DS    0H                                                               
         MVC   DMCB+8(24),LVCNTRL                                               
         GOTO1 BUFFALO,DMCB,=C'ADD',(R3)                                        
         LA    R5,1                                                             
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(R3),(X'80',(R5))                         
         B     EXIT                                                             
         EJECT                                                                  
* SP900 - AT MKTLAST, PUT MKT INFO & MKT TOTAL RECORDS TO SORTER *              
         SPACE 1                                                                
SP900    DS    0H                                                               
         CLI   MKTACTSW,0          IS THERE ANY ACTIVITY FOR THIS MKT?          
         BE    SP920                NO.                                         
         MVC   SRMGR,BMGR+1        MKT GRP NUMBER                               
         PACK  DUB,MKT                                                          
         CVB   RF,DUB                                                           
         STCM  RF,3,SRMKT          MKT                                          
         MVI   SRMODE,MKTFRST                                                   
         MVC   SRDATA(28),MKT      MKT/MKT NAME                                 
         CLI   SPOTPROF+1,C'N'                                                  
         BE    SP910                                                            
         L     RE,ADMARKET                                                      
         USING MKTREC,RE                                                        
         CLC   MKTWT,SPACES                                                     
         BE    SP910                                                            
         OC    MKTWT,MKTWT                                                      
         BZ    SP910                                                            
         PACK  DUB,MKTWT                                                        
         CVB   RF,DUB                                                           
         LTR   RF,RF                                                            
         BZ    SP910                                                            
         ST    RF,SRDATA+28        MKT WT - DATA                                
         MVC   SRDATA+32(4),MKTWT  EBCDIC MKT WT                                
         DROP  RE                                                               
SP910    DS    0H                                                               
         BAS   RE,SRRECSET         SET SRPGR, SRPRD, SRPRDN & SRRPT             
         AP    MREC,NUMREC                                                      
         SPACE 1                                                                
         XC    SRKEY(10),SRKEY                                                  
         XC    SRSTA,SRSTA                                                      
         XC    SRDATA,SRDATA                                                    
         MVI   SRSTA,X'FF'                                                      
         MVI   SRMODE,MKTLAST                                                   
         MVI   LCODE,2             MKT TOTALS                                   
         BAS   RE,SPRDTOT          GET LEVEL TOTALS & PUT TO SORTER             
         AP    MLREC,NUMREC                                                     
SP920    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* SP1000 - AT MKT GRP LAST BREAKS, PUT MKT GROUP  *                             
*  INFO AND MKT GRP LEVEL TOTAL RECORDS TO SORTER *                             
         SPACE 1                                                                
SP1000   DS    0H                                                               
         CLI   MGPACTSW,0                                                       
         BE    EXIT                                                             
         MVC   SRMGR,BMGR+1        MKT GRP NUMBER                               
         MVI   SRMODE,MGR1FRST                                                  
         LA    RF,MGR1                                                          
         CLI   MODE,MGR2LAST                                                    
         BH    SP1010                                                           
         MVI   SRMODE,MGR2FRST                                                  
         LA    RF,MGR2                                                          
         CLI   MODE,MGR3LAST                                                    
         BH    SP1010                                                           
         MVI   SRMODE,MGR3FRST                                                  
         LA    RF,MGR3                                                          
SP1010   DS    0H                                                               
         MVC   SRDATA(42),0(RF)    MOVE MKT GRP HEADER INFO                     
         BAS   RE,SRRECSET         SET SRPGR, SRPRD, SRPRDN & SRRPT             
         AP    MGREC,NUMREC                                                     
         SPACE 1                                                                
         XC    SRREC,SRREC                                                      
         MVC   SRMGR,BMGR+1        MKT GRP NUMBER                               
         MVI   SRMKT,X'FF'                                                      
         MVC   SRMODE,MODE                                                      
         MVI   LCODE,6             MGR3 TOTALS                                  
         CLI   MODE,MGR2LAST                                                    
         BL    SP1020                                                           
         MVI   LCODE,5             MGR2 TOTALS                                  
         CLI   MODE,MGR1LAST                                                    
         BL    SP1020                                                           
         MVI   LCODE,4             MGR1 TOTALS                                  
SP1020   DS    0H                                                               
         BAS   RE,SPRDTOT          GET LEVEL TOTALS & PUT TO SORTER             
         AP    MGLREC,NUMREC                                                    
         B     EXIT                                                             
         SPACE 2                                                                
* SP1100 - AT PRDLAST, PUT PRODUCT TOTALS TO SORTER *                           
         SPACE 1                                                                
SP1100   DS    0H                                                               
         MVI   SRMGR,X'FF'                                                      
         MVI   LCODE,3                                                          
         BAS   RE,SPRDTOT          GET LEVEL TOTALS & PUT TO SORTER             
         AP    PLREC,NUMREC                                                     
         B     EXIT                                                             
         EJECT                                                                  
* SP1500 - AT CLTLAST, PRINT THE REPORT *                                       
         SPACE 1                                                                
SP1500   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   PUTSW,C'Y'          ANY RECORDS PUT TO SORTER?                   
         BE    SP2005               YES                                         
         MVC   P(24),=CL24'NO BUYS FOR THIS REQUEST'                            
         GOTO1 REPORT                                                           
         MVI   SPUT00+1,0          PREPARE SORTER TO BE RE-INITIALIZED          
         GOTO1 VSORTER,DMCB,=C'END'                                             
         GOTO1 AENDREQ                                                          
         SPACE 1                                                                
SP2000   DS    0H                                                               
         CLI   SUPTOTSW,C'Y'       SHOULD I SUPPRESS DAYPART TOTALS?            
         BNE   *+16                 NO.                                         
SP2005   DS    0H                                                               
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVI   MEDSLCNT,0                                                       
         MVI   MEDDPCNT,0                                                       
         DROP  RF                                                               
         MVI   SPSUPMKT,C'Y'                                                    
         MVC   SAVMODE,MODE                                                     
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     RE,4(R1)                                                         
         LTR   RE,RE                                                            
         BZ    EXIT                                                             
         MVC   SRREC,0(RE)                                                      
         MVC   MYBUFIO,SRREC                                                    
         MVC   MODE,SRMODE         RESET MODE FOR HEADHOOK                      
         BC    0,SPTRACE                                                        
         CLI   SRMODE,PROCBUY                                                   
         BE    SP3000              DATA RECORD                                  
         BH    SP2060              TOTALS RECORDS                               
         SPACE 1                                                                
* BREAK INFORMATION LOGIC *                                                     
         SPACE 1                                                                
         CLI   SRMODE,PRDFRST                                                   
         BH    SP2020                                                           
         MVC   MID1(132),SPACES                                                 
         MVC   PRD(27),SRDATA+1    RESTORE PRODUCT INFORMATION                  
         MVC   BPRD,SRDATA                                                      
         ZIC   RE,BPRD                                                          
         CLI   BPRD,X'FF'          GET DEMO NAMES FOR PRODUCT                   
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    R5,28(RE,RF)        R5 POINTS TO DEMO LIST                       
         XC    DNAME1(28),DNAME1                                                
         LA    R0,4                                                             
         LA    R1,4                                                             
         LR    R4,R5                                                            
SP2010   DS    0H                                                               
         CLI   1(R4),0             ANY MORE DEMOS                               
         BZ    SP2015                                                           
         LA    R4,3(R4)                                                         
         BCT   R0,SP2010                                                        
         SPACE 1                                                                
SP2015   DS    0H                                                               
         SR    R0,R1                                                            
         LPR   R0,R0               R0 HAS NUMBER OF DEMOS                       
         CH    R0,=H'0'                                                         
         BE    SP2016                                                           
         GOTO1 VNEWDNAM,DMCB,(RA),((R0),(R5)),DNAME1                            
         SPACE 1                                                                
SP2016   DS    0H                                                               
         MVI   RTGSW,0                                                          
         CLI   DNAME1,C'R'                                                      
         BNE   *+8                                                              
         MVI   RTGSW,1                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     SP2000                                                           
         SPACE 1                                                                
SP2020   DS    0H                                                               
         CLI   SRMODE,MGR3FRST                                                  
         BH    SP2030                                                           
         LA    RF,MGR1                                                          
         CLI   SRMODE,MGR2FRST                                                  
         BL    SP2025                                                           
         LA    RF,MGR2                                                          
         CLI   SRMODE,MGR3FRST                                                  
         BL    SP2025                                                           
         LA    RF,MGR3                                                          
SP2025   DS    0H                                                               
         MVC   0(42,RF),SRDATA     RESTORE MARKET GROUP INFORMATION             
         MVC   BMGR+1(2),SRMGR                                                  
         MVI   FORCEHED,C'Y'                                                    
         B     SP2000                                                           
         SPACE 1                                                                
SP2030   DS    0H                                                               
         CLI   SRMODE,MKTFRST                                                   
         BH    SP2040                                                           
         MVC   MID1(132),SPACES                                                 
         MVC   MKT(28),SRDATA      RESTORE MARKET INFORMATION                   
         MVC   BMKT,SRMKT                                                       
         MVC   SPWEIGHT,SRDATA+28                                               
         L     RE,ADMARKET                                                      
         USING MKTREC,RE                                                        
         MVC   MKTNAME,MKTNM                                                    
         MVC   MKTWT,SRDATA+32                                                  
         DROP  RE                                                               
         MVC   MID1(4),MKT                                                      
         MVC   MID2(4),=CL4'----'                                               
         MVI   MID1+5,C'-'                                                      
         MVC   MID1+7(24),MKTNM                                                 
         LA    R0,24                                                            
         LA    RE,MID1+30                                                       
         LA    RF,MID2+30                                                       
SP2035   DS    0H                                                               
         CLI   0(RE),C'A'          UNDERLINE ALL LETTERS IN MKT NAME            
         BL    *+16                                                             
         CLI   0(RE),C'Z'                                                       
         BH    *+8                                                              
         MVI   0(RF),C'-'                                                       
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         BCT   R0,SP2035                                                        
         MVI   FORCEMID,C'Y'                                                    
         CLI   LINE,55                                                          
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   MID1+32(9),=C'COVERAGE='                                         
         EDIT  SPWEIGHT,(5,MID1+41),2,ALIGN=LEFT                                
         XC    STACNT,STACNT                                                    
         B     SP2000                                                           
         SPACE 1                                                                
SP2040   DS    0H                                                               
         CLI   SRMODE,STAFRST                                                   
         BNE   SP2000                                                           
         MVC   STA(12),SRDATA      RESTORE STATION INFORMATION                  
         MVC   BSTA,SRSTA                                                       
         MVC   STAHOLD,STAPRINT                                                 
         L     RE,STACNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,STACNT                                                        
         B     SP2000                                                           
         SPACE 2                                                                
* BREAK TOTALS LOGIC *                                                          
         SPACE 1                                                                
SP2060   DS    0H                                                               
         CLC   SAVMODE,MODE                                                     
         BE    SP2065                                                           
         CLI   MODE,MKTLAST                                                     
         BNE   SP2063                                                           
         MVI   MODE,STALAST        SET THE MEDADDWT ACTIVITY SWITCH             
         GOTO1 MEDADDWT,DMCB,(RA)                                               
SP2063   DS    0H                                                               
         MVC   MODE,SRMODE         INCREMENT COUNTERS                           
         GOTO1 MEDADDWT,DMCB,(RA)                                               
SP2065   DS    0H                                                               
         CLI   SRMODE,MKTLAST                                                   
         BH    SP2070                                                           
         CLI   QFILTER,C'Y'                                                     
         BNE   SP2067                                                           
         OI    DOSUMNOP+1,X'F0'                                                 
         B     SP2069                                                           
SP2067   CLC   STACNT,=F'1'                                                     
         BE    SP2000                                                           
SP2069   CLC   SAVMODE,MODE                                                     
         BE    *+10                                                             
         MVC   STAHOLD,=C'*TOTAL*'                                              
         BAS   RE,DOSUM                                                         
         B     SP2000                                                           
         SPACE 1                                                                
SP2070   DS    0H                                                               
         MVI   SPSUPMKT,C'N'                                                    
         MVC   MID1(132),SPACES                                                 
         MVC   MID2,SPACES                                                      
         CLC   SAVMODE,MODE                                                     
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   STAHOLD,=C'MGR TOT'                                              
         CLI   SRMODE,MGR3LAST                                                  
         BH    SP2080                                                           
         MVI   STAHOLD+3,C'3'                                                   
         BAS   RE,DOSUM                                                         
         B     SP2000                                                           
         SPACE 1                                                                
SP2080   DS    0H                                                               
         CLI   SRMODE,MGR2LAST                                                  
         BH    SP2090                                                           
         MVI   STAHOLD+3,C'2'                                                   
         BAS   RE,DOSUM                                                         
         B     SP2000                                                           
         SPACE 1                                                                
SP2090   DS    0H                                                               
         CLI   SRMODE,MGR1LAST                                                  
         BH    SP2100                                                           
         CLI   QFILTER,C'Y'                                                     
         BNE   *+8                                                              
         OI    DOSUMNOP+1,X'F0'                                                 
         BAS   RE,DOSUM                                                         
         B     SP2000                                                           
         SPACE 1                                                                
SP2100   DS    0H                                                               
         CLI   SRMODE,PRDLAST                                                   
         BNE   SP2000                                                           
         SPACE 1                                                                
         TM    QMKT,X'F0'                                                       
         BO    SP2000                                                           
         MVI   RCSUBPRG,3                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,4                                                       
         MVC   STAHOLD(3),=C'PRD'                                               
         BAS   RE,DOSUM                                                         
         MVI   RCSUBPRG,5                                                       
         B     SP2000                                                           
         SPACE 2                                                                
* DATA RECORD LOGIC *                                                           
         SPACE 1                                                                
SP3000   DS    0H                                                               
         CLC   SRST(4),SAVDATES                                                 
         BE    SP3005                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   SAVDATES,SRST                                                    
         GOTO1 DATCON,DMCB,(2,SRST),(5,STDATE)                                  
         GOTO1 (RF),(R1),(2,SREND),(5,ENDDATE)                                  
SP3005   DS    0H                                                               
         CLI   QFILTER,C'Y'        ONLY WANT PRODUCT SUMMARIES                  
         BE    SP2000               YES                                         
         MVI   RCSUBPRG,1                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
         CLI   LINE,56                                                          
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   CURRLN,LINE                                                      
         SR    RE,RE                                                            
         IC    RE,CURRLN                                                        
         LA    RE,3(RE)                                                         
         STC   RE,CURRLN                                                        
         CLI   SRDATA,1                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    SRDATA+12(56),SRDATA+12                                          
         BZ    SP2000                                                           
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVC   MEDBRAND,SRPRDN     SET MEDBRAND FOR MEDEDIT                     
         CLI   SUPTOTSW,C'Y'       ARE WE SUPPRESSING DAYPART TOTALS?           
         BNE   *+12                 NO.                                         
         MVI   MEDDPCNT,0                                                       
         MVI   MEDSLCNT,0                                                       
         DROP  RF                                                               
         GOTO1 MEDEDIT,DMCB,(RA)                                                
         CLI   DMCB,0              DONT PRINT THIS LINE                         
         BE    SP2000                                                           
*                                                                               
         ZIC   RE,MYDSCNTR         COUNT THE LINE                               
         LA    RE,1(RE)                                                         
         STC   RE,MYDSCNTR                                                      
*                                                                               
         CLC   P+11(5),=C'* TOT'   TOTAL LINE                                   
         BNE   SP3007                                                           
         MVC   BYTE,MYDSCNTR       SAVE NUMBER OF DETAILS                       
         MVI   MYDSCNTR,0                                                       
         CLI   BYTE,2              ONE PLUS THIS ONE                            
         BE    SP2000              MEANS ONLY ONE DETAIL - PURGE TOT            
*                                                                               
SP3007   MVC   P1(7),STAHOLD                                                    
         XC    STAHOLD,STAHOLD                                                  
         MVC   SPACING,DMCB                                                     
         CLI   SUPTOTSW,C'Y'                                                    
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         CLI   GREYSW,C'Y'         IS THE REQUESTOR = GREY?                     
         BE    SP3030               YES. SUPPRESS NTP AND DOLLARS               
         MVC   FULL,SRDATA+12                                                   
         TM    FULL,X'C0'                                                       
         BNM   SP3010                                                           
         BAS   RE,EDITBIG                                                       
         B     SP3020                                                           
SP3010   DS    0H                                                               
         EDIT  FULL,(10,P1+28),2                                                
SP3020   DS    0H                                                               
         MVC   FULL,SRDATA+16                                                   
         EDIT  FULL,(10,P1+38),2                                                
SP3030   DS    0H                                                               
         GOTO1 REPORT                                                           
         B     SP2000                                                           
         EJECT                                                                  
* DOSUM - DO SUMMARY REPORTS *                                                  
         SPACE 1                                                                
DOSUM    NTR1                                                                   
         MVI   SW1,0                                                            
         CLI   QFILTER,C'Y'                                                     
         BE    *+12                                                             
         CLI   SPDUPTOT,C'Y'                                                    
         BE    EXIT                                                             
         CLI   SRDATA+1,0          PRD CODE MAY NOT BE ZERO                     
         BE    DOSUM30                                                          
         CLI   SRPRDN,X'FF'                                                     
         BE    DOSUM25             CLIENT TOTAL                                 
         GOTO1 VIEEQU,DMCB,(RA),(1,SRDATA+1),WORD                               
         L     RE,PRDBUFF                                                       
         LA    RF,220                                                           
DOSUM10  DS    0H                                                               
         CLC   28(3,RE),WORD       IS THIS RIGHT PRIMARY DEMO?                  
         BE    DOSUM20                                                          
         AH    RE,PRDBUFLN                                                      
         BCT   RF,DOSUM10                                                       
         DC    H'0'                                                             
DOSUM20  DS    0H                                                               
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVC   MEDBRAND,0(RE)      SET PRIMARY DEMO BRAND                       
         DROP  RF                                                               
         XC    DNAME1(7),DNAME1                                                 
         GOTO1 VNEWDNAM,DMCB,(RA),(1,FULL),DNAME1                               
         MVI   RTGSW,0                                                          
         MVI   RCSUBPRG,6                                                       
         CLI   DNAME1,C'R'                                                      
         BNE   *+12                                                             
         MVI   RTGSW,1                                                          
         MVI   RCSUBPRG,5                                                       
         MVC   MID1(7),DNAME1                                                   
         B     DOSUM30                                                          
DOSUM25  DS    0H                                                               
         MVI   RCSUBPRG,7                                                       
         MVC   MID1(10),=C'ALL BRANDS'                                          
DOSUM30  DS    0H                                                               
         MVI   FORCEMID,C'Y'                                                    
         MVI   SW1,1                                                            
         GOTO1 MEDEDIT,DMCB,(RA)                                                
         CLI   DMCB,0                                                           
         BE    EXIT                                                             
         MVC   P1(7),STAHOLD                                                    
         XC    STAHOLD,STAHOLD                                                  
         MVC   SPACING,DMCB                                                     
         CLI   SUPTOTSW,C'Y'       AM I SUPPRESSING DAYPART TOTALS?             
         BNE   *+8                  NO.                                         
         MVI   SPACING,2                                                        
         CLI   GREYSW,C'Y'         IS REQUESTOR = GREY?                         
         BE    DOSUM50              YES. SUPPRESS NTP AND DOLLARS               
         MVC   FULL,SRDATA+12                                                   
         TM    FULL,X'C0'                                                       
         BNM   DOSUM40                                                          
         BAS   RE,EDITBIG                                                       
         B     DOSUM45                                                          
DOSUM40  DS    0H                                                               
         EDIT  FULL,(11,P1+27),2                                                
DOSUM45  DS    0H                                                               
         MVC   FULL,SRDATA+16                                                   
         EDIT  FULL,(10,P1+38),2                                                
         CLI   P1+38,C' '                                                       
         BE    DOSUM50                                                          
         MVC   P1+38(10),SPACES                                                 
         EDIT  FULL,(11,P2+37),2                                                
DOSUM50  DS    0H                                                               
DOSUMNOP BC    0,DOSUMCLR                                                       
         GOTO1 REPORT                                                           
         B     EXIT                                                             
DOSUMCLR NI    DOSUMNOP+1,X'0F'                                                 
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         B     EXIT                                                             
         SPACE 2                                                                
* EDITBIG - EDIT THE VERY LARGE NUMBERS *                                       
         SPACE 1                                                                
EDITBIG  NTR1                                                                   
         CVDX  DUB,FULL                                                         
         AP    DUB,=P'50'                                                       
         EDIT  (P8,DUB),(10,PSLIST)                                             
         MVC   P1+28(8),PSLIST                                                  
         B     EXIT                                                             
         SPACE 1                                                                
* SPTRACE - TRACE ROUTINE *                                                     
         SPACE 1                                                                
SPTRACE  NTR1                                                                   
         GOTO1 HEXOUT,DMCB,SRREC,P+2,96,=C'MIX',0                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 1                                                                
* ERROR - PRINT ERROR MESSAGE *                                                 
         SPACE 1                                                                
ERROR    DS    0H                                                               
         MVC   P(35),=C'NO FLIGHT RECORD FOUND FOR PRODUCT '                    
         MVC   P+34(3),SRPRD                                                    
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* SPUTPER - BUILD DATA RECORD - MONTHS, QUARTERS OR YEARS *                     
         SPACE 1                                                                
SPUTPER  NTR1                                                                   
         USING MEDBLOCK,R3                                                      
         USING MEDDATA,R5                                                       
SPPER10  DS    0H                                                               
         L     R5,4(R4)                                                         
         OC    MEDBYD(12),MEDBYD   ANY ACTIVITY?                                
         BZ    SPPER20              NO.                                         
         MVC   SRST(4),0(R4)       START AND END DATES                          
         BAS   RE,SPPOST           POST BUFFALO RECORD                          
SPPER20  DS    0H                                                               
         TM    SRRPT,X'80'          YES. WE ARE DONE                            
         BO    EXIT                                                             
         LA    R4,12(R4)                                                        
         OC    0(4,R4),0(R4)       ANY MORE DATES?                              
         BNZ   SPPER10                                                          
         B     EXIT                                                             
         SPACE 1                                                                
* SPUTFLT - BUILD DATA RECORD - FLIGHTS *                                       
         SPACE 1                                                                
SPUTFLT  NTR1                                                                   
         CLI   SRPRDN,219                                                       
         BNL   EXIT                                                             
         MVI   SRRPT,X'40'         REPORT BY FLIGHTS                            
         LA    R4,MEDQ1WKS                                                      
SPFLT10  DS    0H                                                               
         L     R5,4(R4)                                                         
         L     R6,VFLTTBL                                                       
         OC    MEDBYD(12),MEDBYD   ANY ACTIVITY?                                
         BZ    SPFLT40              NO.                                         
SPFLT20  DS    0H                                                               
         CLC   SRPRD,1(R6)         IS THIS THE RIGHT PRODUCT?                   
         BNE   SPFLT25              NO.                                         
         CLC   0(2,R4),5(R6)       DOES THIS BUY FALL WITHIN THIS FLT?          
         BL    SPFLT25              NO.                                         
         CLC   2(2,R4),7(R6)                                                    
         BNH   SPFLT30              YES.                                        
SPFLT25  DS    0H                                                               
         LA    R6,9(R6)                                                         
         CLI   0(R6),X'FF'         IS THIS END OF FLTTBL?                       
         BNE   SPFLT20                                                          
         B     ERROR                                                            
SPFLT30  DS    0H                                                               
         MVC   SRST(4),5(R6)       GET THE START AND END DATES                  
         BAS   RE,SPPOST           POST THE BUFFALO RECORD                      
SPFLT40  DS    0H                                                               
         LA    R4,12(R4)                                                        
         C     R4,MEDALAST         HAVE WE CHECKED ALL RELEVANT WEEKS?          
         BH    EXIT                 YES.                                        
         OC    0(4,R4),0(R4)                                                    
         BNZ   SPFLT10                                                          
         B     SPFLT40                                                          
         EJECT                                                                  
* SPPOST - POST BUFFALO RECORD *                                                
         SPACE 1                                                                
SPPOST   NTR1                                                                   
         L     R2,MEDLCHNK                                                      
         MVC   WORK(20),SRKEY                                                   
         MVC   MEDBYD,MEDCSTPT                                                  
         XC    MEDCSTPT,MEDCSTPT                                                
         L     RE,MEDBYD           SET EQUIVELENCED COST                        
         MH    RE,MEDEQFAC                                                      
         SRDA  RE,32                                                            
         SLA   RF,1                                                             
         D     RE,=F'1000'                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,MEDBYDEQ                                                      
         L     RF,MEDWEEKS+4                                                    
         L     RE,4(R4)                                                         
         EX    R2,MVCHUNK          MOVE DETAILS TO WEEK1                        
         MVI   MGPACTSW,1                                                       
         MVI   MKTACTSW,1                                                       
         MVI   STAACTSW,1                                                       
         GOTO1 MEDPOST,DMCB,(RA)                                                
         B     EXIT                                                             
         DROP  R3                                                               
         DROP  R5                                                               
         SPACE 1                                                                
MVCHUNK  MVC   0(0,RF),0(RE)                                                    
         EJECT                                                                  
* SRRECSET - SET SRPGR, SRPRD, SRPRDN AND SRRPT FOR INFO RECORDS *              
         SPACE 1                                                                
SRRECSET NTR1                                                                   
         ZAP   NUMREC,=P'0'                                                     
         CLI   SRMODE,STAFRST                                                   
         BNE   *+12                                                             
         L     R2,VSPTBL                                                        
         B     SPS10                                                            
         CLI   SRMODE,MKTFRST                                                   
         BNE   *+12                                                             
         L     R2,VMPTBL                                                        
         B     SPS10                                                            
         L     R2,VMGPTBL                                                       
SPS10    DS    0H                                                               
         LA    R0,220                                                           
         AR    R2,R0                                                            
         BCTR  R2,0                                                             
SPS20    DS    0H                                                               
         CLI   0(R2),1             IS THIS AN ACTIVE BRAND?                     
         BNE   SPS40                NO.                                         
         LR    RF,R0                                                            
         BCTR  RF,0                                                             
         MH    RF,PRDBUFLN                                                      
         A     RF,PRDBUFF                                                       
         MVC   SRPRD,1(RF)         PRD - EBCDIC                                 
         MVC   SRPRDN,0(RF)        PRD - BINARY                                 
         LR    RF,R0                                                            
         BCTR  RF,0                                                             
         AR    RF,RF                                                            
         A     RF,VPGRTBL                                                       
         CLI   PGRACTSW,C' '       ARE WE PROCESSING BY PRODUCT GROUP?          
         BE    SPS30                NO.                                         
         CLI   SRPRDN,X'FF'                                                     
         BE    SPS30                                                            
         CLC   SAVPGR,0(RF)        IS THIS PRODUCT IN RIGHT PRD GRP?            
         BNE   SPS40                NO.                                         
SPS30    DS    0H                                                               
         MVC   SRPGR,0(RF)         PRD GRP                                      
         BAS   RE,SRRPTSET         SET SRRPT, SRST & SREND                      
SPS40    DS    0H                                                               
         BCTR  R2,0                                                             
         BCT   R0,SPS20                                                         
         B     EXIT                                                             
         EJECT                                                                  
* SRRPTSET - SET SRRPT, SRST & SREND, THEN PUTS INFO RECORDS TO SORTER          
         SPACE 1                                                                
SRRPTSET NTR1                                                                   
         L     R8,MEDBUFF                                                       
         USING MEDBLOCK,R8                                                      
         CLI   QOPT1,C'Y'                                                       
         BNE   SRR20                                                            
         MVI   SRRPT,X'20'         REPORT BY MONTHS                             
         LA    R0,13                                                            
         LA    R2,MEDMON01                                                      
SRR10    DS    0H                                                               
         OC    0(4,R2),0(R2)                                                    
         BZ    SRR20                                                            
         MVC   SRST(4),0(R2)       START AND END DATES                          
         BAS   RE,SPUTSORT         PUT RECORD TO SORTER                         
         LA    R2,12(R2)                                                        
         BCT   R0,SRR10                                                         
SRR20    DS    0H                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   SRR40                                                            
         MVI   SRRPT,X'60'         REPORT BY QUARTERS                           
         LA    R0,5                                                             
         LA    R2,MEDQRT01                                                      
SRR30    DS    0H                                                               
         OC    0(4,R2),0(R2)                                                    
         BZ    SRR40                                                            
         MVC   SRST(4),0(R2)       START AND END DATES                          
         BAS   RE,SPUTSORT         PUT RECORD TO SORTER                         
         LA    R2,12(R2)                                                        
         BCT   R0,SRR30                                                         
SRR40    DS    0H                                                               
         CLI   QOPT4,C'Y'                                                       
         BNE   SRR50                                                            
         MVI   SRRPT,X'80'         REPORT BY YEAR                               
         MVC   SRST(4),MEDPERD     START AND END DATES                          
         BAS   RE,SPUTSORT         PUT RECORD TO SORTER                         
SRR50    DS    0H                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   SRR80                                                            
         MVI   SRRPT,X'40'         REPORT BY FLIGHT                             
         L     R2,VFLTTBL                                                       
SRR60    DS    0H                                                               
         CLI   0(R2),X'FF'         IS THIS THE END OF FLT TBL                   
         BE    SRR80                YES.                                        
         CLC   SRPRD,1(R2)                                                      
         BNE   SRR70                                                            
         MVC   SRST(4),5(R2)       START AND END DATES                          
         BAS   RE,SPUTSORT         PUT RECORD TO SORTER                         
SRR70    DS    0H                                                               
         LA    R2,9(R2)                                                         
         B     SRR60                                                            
SRR80    DS    0H                                                               
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
* SPRDTOT - THIS ROUTINE GETS THE LEVEL TOTALS & PUTS THEM IN SRDATA *          
         SPACE 1                                                                
SPRDTOT  NTR1                                                                   
         ZAP   NUMREC,=P'0'                                                     
         XC    MYBUFIO,MYBUFIO                                                  
         L     R3,BUFFBUFF                                                      
         L     R4,BUFFIO                                                        
         ZIC   R5,LCODE                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R3),(R4),(R5)                             
         B     SPR20                                                            
         SPACE 1                                                                
SPR10    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),(R5)                              
SPR20    DS    0H                                                               
         TM    DMCB+8,X'80'                                                     
         BO    SPR40               NO MORE TOTALS AT THIS LEVEL                 
         XC    SRDATA(68),SRDATA                                                
         MVC   SRDATA(68),20(R4)                                                
         MVC   SRRPT(5),6(R4)      REPORT TYPE, START & END DATES               
         CLI   MODE,PRDLAST                                                     
         BH    SPR30               PRD & PRD GRP ARE ALREADY SET                
         MVC   SRPRD,2(R4)         PRD - EBCDIC                                 
         MVC   SRPRDN,5(R4)        PRD - BINARY                                 
         ZIC   RF,SRPRDN                                                        
         CLI   SRPRDN,X'FF'                                                     
         BNE   *+8                                                              
         LA    RF,220              POL IS THE 220TH ENTRY                       
         BCTR  RF,0                                                             
         AR    RF,RF                                                            
         A     RF,VPGRTBL          DISP INTO PGR TBL                            
         MVC   SRPGR,0(RF)         PRD GRP                                      
SPR30    DS    0H                                                               
         BAS   RE,SPUTSORT         PUT RECORD TO SORTER                         
         B     SPR10                                                            
         SPACE 1                                                                
SPR40    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(R3),(X'80',(R5))                         
         B     EXIT                                                             
         EJECT                                                                  
* SPDUMMY - THIS ROUTINE CREATES A DUMMY FLIGHT TO FILL IN THE GAPS *           
         SPACE 1                                                                
SPDUMMY  NTR1                                                                   
         MVC   0(4,R3),0(R2)       PRD/EBCDIC PRD                               
         MVI   4(R3),X'FF'         DUMMY FLIGHT NUMBER                          
         MVC   5(2,R3),DATE        DUMMY FLIGHT START DATE                      
         MVC   7(2,R3),HALF        DUMMY FLIGHT END DATE                        
         B     EXIT                                                             
         SPACE 2                                                                
* SPADDAY - ADD A DAY TO THE COMPRESSED DATE *                                  
         SPACE 1                                                                
SPADDAY  NTR1                                                                   
         GOTO1 DATCON,DMCB,(2,DATE),DATE2                                       
         GOTO1 ADDAY,(R1),DATE2,DATE2,1                                         
         GOTO1 DATCON,(R1),DATE2,(2,DATE)                                       
         B     EXIT                                                             
         SPACE 2                                                                
* SPSUBDAY - SUBTRACT A DAY FROM THE COMPRESSED FLTST *                         
         SPACE 1                                                                
SPSUBDAY NTR1                                                                   
         GOTO1 DATCON,DMCB,(2,3(R4)),DATE2                                      
         L     R3,=F'1'                                                         
         LNR   R3,R3                                                            
         GOTO1 ADDAY,(R1),DATE2,DATE2,(R3)                                      
         GOTO1 DATCON,(R1),DATE2,(2,HALF)                                       
         B     EXIT                                                             
         SPACE 1                                                                
* SPMONDAY - THIS ROUTINE EXPANDS REQUEST PERIOD *                              
*  TO BEGIN ON A MONDAY AND END ON A SUNDAY      *                              
         SPACE 1                                                                
SPMONDAY NTR1                                                                   
         MVC   FRSTMON(12),QSTART                                               
         GOTO1 GETDAY,DMCB,QSTART,WORK                                          
         CLI   DMCB,1              IS QSTART A MONDAY?                          
         BE    SPM10                YES                                         
         ZIC   R3,DMCB                                                          
         BCTR  R3,0                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,(R1),QSTART,FRSTMON,(R3)                                   
SPM10    DS    0H                                                               
         GOTO1 GETDAY,(R1),QEND,WORK                                            
         CLI   DMCB,7              IS QEND A SUNDAY?                            
         BE    SPM20                                                            
         ZIC   RF,DMCB                                                          
         LA    R3,7                                                             
         SR    R3,RF                                                            
         GOTO1 ADDAY,(R1),QEND,LASTSUN,(R3)                                     
SPM20    DS    0H                                                               
         GOTO1 DATCON,(R1),FRSTMON,(2,CMPST)                                    
         GOTO1 (RF),(R1),LASTSUN,(2,CMPEND)                                     
         B     EXIT                                                             
         EJECT                                                                  
* SPUTSORT - PUT OUT A RECORD TO SORTER *                                       
         SPACE 1                                                                
SPUTSORT NTR1                                                                   
SPUT00   BC    0,SPUT10                                                         
         MVI   *-3,X'F0'                                                        
         GOTO1 VSORTER,DMCB,SORT,RECCARD,(64,VSORTBUF)                          
SPUT10   DS    0H                                                               
         CLI   SRMODE,PROCBUY                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         AP    TREC,=P'1'                                                       
         AP    NUMREC,=P'1'                                                     
         B     EXIT                                                             
         SPACE 2                                                                
* SPTOTAL - PRINT SORTER RECORD TOTALS *                                        
         SPACE 1                                                                
SPTOTAL  DS    0H                                                               
         LA    R0,9                                                             
         LA    R2,CNTRS                                                         
SPT10    DS    0H                                                               
         EDIT  (P4,0(R2)),(5,P+10)                                              
         GOTO1 REPORT                                                           
         LA    R2,4(R2)                                                         
         BCT   R0,SPT10                                                         
         B     EXIT                                                             
         SPACE 2                                                                
* MYSPOTHK - PRODUCT FILTER FOR MEDGETBY *                                      
         SPACE 1                                                                
         USING *,RF                                                             
MYSPOTHK NTR1                                                                   
         LM    R9,RC,SP12R9                                                     
         DROP  RF                                                               
         L     R2,SPOTADDR                                                      
         USING REGELEM,R2                                                       
         CLI   RLEN,14             IS THIS A POL ELEMENT?                       
         BNE   NEXIT                NO. IGNORE IT.                              
         ZIC   RF,RPPRD            GET THE PRODUCT CODE                         
         BCTR  RF,0                                                             
         AR    RF,RF                                                            
         A     RF,VPGRTBL                                                       
         CLC   SAVPGR,0(RF)        IS THIS THE RIGHT PRDGRP ID?                 
         BNE   NEXIT                NO. IGNORE IT                               
         MVI   SPOTYORN,C'Y'        YES. PROCESS IT                             
         B     EXIT                                                             
         SPACE 1                                                                
NEXIT    DS    0H                                                               
         MVI   SPOTYORN,C'N'                                                    
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
* MYHEAD - THIS ROUTINE BUILD HEADER *                                          
         SPACE 1                                                                
         USING *,RF                                                             
MYHEAD   NTR1                                                                   
         LM    R9,RC,SP12R9                                                     
         DROP  RF                                                               
         MVC   H1+52(10),=CL10'CHILD SPOT'                                      
         CLC   QCLT,=C'AO '        IS THIS AN ALL-FAMILY REQUEST?               
         BNE   MYH05                NO.                                         
         MVC   H1+52(10),=CL10'ALL FAMILY'                                      
         SPACE 1                                                                
MYH05    DS    0H                                                               
         CLI   SRRPT,X'40'         IS THIS A REPORT BY FLIGHT?                  
         BNE   MYH10                NO.                                         
         MVC   H3+51(31),=C'FLIGHT FROM MMMDD/YY - MMMDD/YY'                    
         B     MYH40                                                            
MYH10    DS    0H                                                               
         CLI   SRRPT,X'20'         IS THIS A REPORT BY MONTHS?                  
         BNE   MYH20                NO.                                         
         MVC   H3+52(30),=C'MONTH FROM MMMDD/YY - MMMDD/YY'                     
         B     MYH40                                                            
MYH20    DS    0H                                                               
         CLI   SRRPT,X'60'         IS THIS A REPORT BY QUARTERS?                
         BNE   MYH30                NO.                                         
         MVC   H3+50(32),=C'QUARTER FROM MMMDD/YY - MMMDD/YY'                   
         B     MYH40                                                            
MYH30    DS    0H                  REPORT BY YEAR                               
         MVC   H3+51(31),=C'PERIOD FROM MMMDD/YY - MMMDD/YY'                    
MYH40    DS    0H                                                               
         MVC   H3+63(8),STDATE                                                  
         MVC   H3+74(8),ENDDATE                                                 
MYH50    DS    0H                                                               
         CLI   SRMODE,MGR3LAST                                                  
         BL    MYHPGRPX                                                         
         CLI   SRMODE,MGR1LAST                                                  
         BH    MYHPGRP                                                          
         MVC   H9+54(19),=C'******SUMMARY******'                                
MYHPGRP  CLI   SRMODE,PGR3LAST                                                  
         BL    MYHPGRPX                                                         
         CLI   SRMODE,PGR1LAST                                                  
         BH    MYHPGRPX                                                         
         MVC   H7+50(32),H6+50                                                  
         XC    H6+50(32),H6+50                                                  
         CLI   SRMODE,PGR1LAST                                                  
         BNE   *+10                                                             
         MVC   H6+60(12),PGR1BK                                                 
         CLI   SRMODE,PGR2LAST                                                  
         BNE   *+10                                                             
         MVC   H6+60(12),PGR2BK                                                 
         CLI   SRMODE,PGR3LAST                                                  
         BNE   *+10                                                             
         MVC   H6+60(12),PGR3BK                                                 
MYHPGRPX DS    0H                                                               
         CLI   SRMODE,PRDLAST                                                   
         BH    MYHEADX                                                          
         MVC   H13+53(7),DNAME1                                                 
         CLI   DNAME1,C'R'                                                      
         BNE   *+10                                                             
         MVC   H13+74(3),=C'CPP'                                                
         CLI   DNAME2,0                                                         
         BE    MYHEADX                                                          
         MVC   H12+86(7),DNAME2                                                 
         MVC   H12+97(3),=C'CPP'                                                
         CLI   DNAME2,C'R'                                                      
         BE    *+10                                                             
         MVC   H12+97(3),=C'CPM'                                                
         CLI   DNAME3,0                                                         
         BE    MYHEADX                                                          
         MVC   H12+101(7),DNAME3                                                
         MVC   H12+112(3),=C'CPP'                                               
         CLI   DNAME3,C'R'                                                      
         BE    *+10                                                             
         MVC   H12+112(3),=C'CPM'                                               
         CLI   DNAME4,0                                                         
         BE    MYHEADX                                                          
         MVC   H12+116(7),DNAME4                                                
         MVC   H12+127(3),=C'CPP'                                               
         CLI   DNAME4,C'R'                                                      
         BE    *+10                                                             
         MVC   H12+127(3),=C'CPM'                                               
MYHEADX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
         DS    0F                                                               
STACNT   DS    F'0'                                                             
DEMOTYPE DC    F'0'                DEMO TYPE                                    
SP12R9   DC    F'0'                                                             
SP12RA   DC    F'0'                                                             
SP12RB   DC    F'0'                                                             
SP12RC   DC    F'0'                                                             
RELO     DC    F'0'                                                             
CNTRS    DS    0F                                                               
PREC     DS    PL4                                                              
MGREC    DS    PL4                                                              
MREC     DS    PL4                                                              
SREC     DS    PL4                                                              
BREC     DS    PL4                                                              
MLREC    DS    PL4                                                              
MGLREC   DS    PL4                                                              
PLREC    DS    PL4                                                              
TREC     DS    PL4                                                              
NUMREC   DS    PL4                                                              
VFLTTBL  DS    A                                                                
VSORTER  DS    A                                                                
VPGRTBL  DS    A                                                                
VSORTBUF DS    V                   SORTER WORK AREA                             
VMPTBL   DS    V                                                                
VSPTBL   DS    V                                                                
VMGPTBL  DS    V                                                                
VSETPRMY DS    V                                                                
VEIEQU   DS    V                                                                
VIEEQU   DS    V                                                                
VNEWDNAM DS    V                                                                
DATE     DS    H                                                                
CMPST    DS    H                   COMPRESSED EXTENDED REQ. START DATE          
CMPEND   DS    H                   COMPRESSED EXTENDED REQ. END DATE            
SAVPGR   DS    CL2                                                              
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5,6)                                                     
CPPCTRL  DC    X'01',AL3(0),X'04',AL3(0)                                        
         DC    X'05',AL3(0),X'08',AL3(0)                                        
         DC    X'00'                                                            
CPPSW    DC    X'00'               CROSS DAYPART CPP SW                         
         SPACE 1                                                                
BRANDSW  DS    C                   'PRINT ALL BRANDS' SWITCH                    
CURRLN   DS    C                                                                
GREYSW   DS    C                   REQUESTOR = GREY SWITCH                      
LCODE    DS    C                   LEVEL CODE                                   
MGPACTSW DS    C                                                                
MKTACTSW DS    C                                                                
PUTSW    DS    C                   'RECORD PUT TO SORTER' SWITCH                
RTGSW    DS    C                   RATING SWITCH                                
STAACTSW DS    C                                                                
SUPTOTSW DS    C                   SUPPRESS DAYPART TOTALS SWITCH               
MYDSCNTR DS    C                                                                
SW1      DS    C                   DATA SWITCH                                  
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
STAHOLD  DS    CL7                                                              
MYBUFIO  DS    CL96                BUFFALO WORK AREA                            
SVKEY    DS    CL17                                                             
SAVMODE  DS    C                                                                
SVMSTA   DS    CL5                                                              
PGRACTSW DS    CL1                                                              
SAVDATES DS    CL4                 SAVE COMPRESSED START & END DATES            
STDATE   DS    CL8                 MMMDD/YY                                     
ENDDATE  DS    CL8                 MMMDD/YY                                     
FRSTMON  DS    CL6                 EBCDIC START DATE                            
LASTSUN  DS    CL6                 EBCDIC END DATE                              
DATE2    DS    CL6                                                              
SAVPRD   DS    CL3                                                              
MYBYTE   DS    C                                                                
SAVYR    DS    C                                                                
         EJECT                                                                  
* SRREC - SORT RECORDS *                                                        
         SPACE 1                                                                
SORT     DC    C'SORT FIELDS=(1,32,BI,A) '                                      
RECCARD  DC    C'RECORD TYPE=F,LENGTH=96 '                                      
         DS    0F                                                               
SRREC    DS    0CL96                                                            
SRKEY    DS    0CL20                                                            
SRPGR    DS    CL2                 PRODUCT GROUP NUMBER                         
SRPRD    DS    CL3                 PRODUCT CODE - EBCDIC                        
SRPRDN   DS    CL1                 PRODUCT CODE - BINARY                        
SRRPT    DS    CL1                 REPORT CODE                                  
*                                  X'20'  - BY MONTH                            
*                                  X'40'  - BY FLIGHT                           
*                                  X'60' - BY QUARTER                           
*                                  X'80' - BY YEAR                              
SRST     DS    CL2                 COMPRESSED START DATE                        
SREND    DS    CL2                 COMPRESSED END DATE                          
SRMGR    DS    CL2                 MARKET GROUP NUMBER                          
SRMKT    DS    CL2                 MKT                                          
SRSTA    DS    CL3                 STA                                          
SRMODE   DS    CL1                 MODE                                         
         DS    CL1                 SPARE                                        
         SPACE 1                                                                
SRDATA   DS    0CL76                                                            
SRDATA1  DS    CL16                BUFFALO KEY                                  
SRDATA2  DS    CL52                BUFFALO ACCUMULATORS                         
         DS    CL8                 SPARE                                        
         SPACE 1                                                                
PSLIST   DS    CL200               PRODUCT SPOT LENGTH LIST                     
         EJECT                                                                  
SETBUF   CSECT                                                                  
         NMOD1 0,SETBUF                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R4,4(R1)            LOCODE                                       
         L     R5,8(R1)            LVCNTRL                                      
         MVI   0(R4),3             DETERMINE NUMBER OF LEVELS REQUIRED          
         TM    QMKT,X'F0'                                                       
         BNO   *+8                                                              
         MVI   0(R4),2                                                          
         CLI   MGR1LEN,0                                                        
         BNE   *+8                                                              
         CLI   PGR1LEN,0                                                        
         BE    M2A                                                              
         MVI   0(R4),4                                                          
         CLC   MGR1LEN,MGR2LEN                                                  
         BNE   *+10                                                             
         CLC   PGR1LEN,PGR2LEN                                                  
         BE    M2A                                                              
         MVI   0(R4),5                                                          
         CLC   MGR2LEN,MGR3LEN                                                  
         BNE   *+10                                                             
         CLC   PGR2LEN,PGR3LEN                                                  
         BE    M2A                                                              
         MVI   0(R4),6                                                          
*                                                                               
M2A      LA    RE,6                SET BUFFALO CONTROLS                         
         LR    RF,R5                                                            
         NI    0(RF),X'7F'         CLEAR STOP CHARACTER                         
         LA    RF,4(RF)                                                         
         BCT   RE,*-8                                                           
         IC    RE,0(R4)                                                         
         BCTR  RE,0                                                             
         SLL   RE,2                TIMES 4                                      
         AR    RE,R5                                                            
         OI    0(RE),X'80'                                                      
*                                                                               
* SET BUFFALO LEVELS                                                            
         L     R2,BUFFBUFF                                                      
         USING BUFFALOD,R2                                                      
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),0(R4)                                                  
         MVC   BUFFROWS+2(2),HALF                                               
         L     R9,BUFFLALL         GET MAXIMUM CORE AVILABLE                    
         SR    R8,R8                                                            
         M     R8,BUFFCRMX                                                      
         L     R7,BUFFWROW         GET NEW DATA LENGTH                          
         MH    R7,HALF                                                          
         ST    R7,BUFFLDTA                                                      
         A     R7,BUFFLKEY         GET NEW RECORD LENGTH                        
         ST    R7,BUFFLALL                                                      
         DR    R8,R7               GET NEW MAXIMUM RECORDS                      
         ST    R9,BUFFCRMX                                                      
         XMOD1 1                                                                
         EJECT                                                                  
* NEWDNAM - THIS ROUTINE GETS THE DEMO NAME *                                   
         SPACE 1                                                                
NEWDNAM  CSECT                                                                  
         NMOD1 0,NEWDNAM                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         ZIC   R9,4(R1)            NUMBER OF DEMOS                              
         L     R8,8(R1)            OUTPUT AREA                                  
         MH    R9,=H'7'                                                         
         BCTR  R9,0                                                             
         EX    R9,MVCSPACE         CLEAR THE OUTPUT AREA                        
         CLC   PRDBUFLN,=H'56'     IS THIS OLD OR NEW FORMAT                    
         BE    OLDNAME              OLD                                         
         L     R7,ADBLOCK                                                       
         USING DBLOCK,R7                                                        
         XC    0(256,R7),0(R7)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         DROP  R7                                                               
         ZIC   R9,4(R1)            NUMBER OF DEMOS                              
         L     R7,4(R1)            START OF INPUT                               
         L     R8,8(R1)                                                         
         L     R6,ADEST            R6 @ EST HDR                                 
         USING ESTHDR,R6                                                        
         LA    R6,EUSRNMS                                                       
         DROP  R6                                                               
         GOTO1 DEMOCON,DMCB,((R9),(R7)),(2,(R8)),(C'S',ADBLOCK),       X        
               (SPOTPROF+9,(R6))                                                
         B     EXIT00                                                           
         SPACE 1                                                                
OLDNAME  DS    0H                                                               
         ZIC   R9,4(R1)            NUMBER OF DEMOS                              
         L     R7,4(R1)            START OF INPUT                               
         L     R8,8(R1)            START OF OUTPUT                              
OLDNAME2 DS    0H                                                               
         ZIC   R6,0(R7)                                                         
         LTR   R6,R6                                                            
         BZ    EXIT00                                                           
         BCTR  R6,0                                                             
         MH    R6,=H'7'                                                         
         A     R6,DEMTABLE                                                      
         MVC   0(7,R8),0(R6)                                                    
         LA    R7,1(R7)                                                         
         LA    R8,7(R8)                                                         
         BCT   R9,OLDNAME2                                                      
EXIT00   XMOD1 1                                                                
         SPACE 1                                                                
MVCSPACE MVC   0(0,R8),SPACES                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* SETPRMY - THIS ROUTINE SETS PRIMARY DEMO IN MEDPRIMY *                        
         SPACE 1                                                                
SETPRMY  CSECT                                                                  
         NMOD1 0,SETPRMY                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         ZIC   RE,MEDBRAND         GET PRODUCT SLOT                             
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         MVC   MEDPRIMY,28(RE)     EXTRACT OLD PRIMARY DEMO                     
         CLC   PRDBUFLN,=H'56'     IS THIS OLD OR NEW FORMAT                    
         BE    EXIT10               OLD                                         
         MVC   DEMFULL(3),28(RE)   EXTRACT NEW PRIMARY DEMO                     
         SPACE 1                                                                
         LA    R9,PRMYTAB                                                       
         LA    R1,1                                                             
SETPRMY2 DS    0H                                                               
         CLC   DEMFULL(3),0(R9)    SAVE NEW PRIMARY DEMO IN TABLE               
         BE    SETPRMY4                                                         
         CLI   1(R9),0                                                          
         BE    SETPRMY3                                                         
         LA    R9,3(R9)                                                         
         LA    R1,1(R1)                                                         
         B     SETPRMY2                                                         
         SPACE 1                                                                
SETPRMY3 DS    0H                                                               
         MVC   0(3,R9),DEMFULL                                                  
SETPRMY4 DS    0H                                                               
         STC   R1,MEDPRIMY         EQUATE NEW PRIMARY DEMO TO SLOT              
EXIT10   XMOD1 1                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* IEEQU - EQUATE DEMOS FROM INTERNAL TO EXTERNAL FORMAT *                       
         SPACE 1                                                                
IEEQU    CSECT                                                                  
         NMOD1 0,IEEQU                                                          
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         ZIC   R9,4(R1)            NUMBER OF DEMOS                              
         L     R8,4(R1)                                                         
         L     R7,8(R1)                                                         
         CLC   PRDBUFLN,=H'56'     IS THIS OLD OR NEW FORMAT                    
         BE    IEOLD                OLD                                         
IEEQU10  DS    0H                                                               
         ZIC   R1,0(R8)            SET TO SLOT IN EQUATE TABLE                  
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,PRMYTAB(R1)                                                   
         MVC   0(3,R7),0(R1)                                                    
         LA    R8,1(R8)                                                         
         LA    R7,3(R7)                                                         
         BCT   R9,IEEQU10                                                       
         B     EXIT20                                                           
         SPACE 1                                                                
IEOLD    DS    0H                                                               
         MVC   0(1,R7),0(R8)       SET TO OLD DEMO                              
         LA    R7,1(R7)                                                         
         LA    R8,1(R8)                                                         
         BCT   R9,IEOLD                                                         
EXIT20   XMOD1 1                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* EIEQU - EQUATE DEMOS FROM EXTERNAL TO INTERNAL FORMAT *                       
*         CURRENTLY NOT USED BY THE X3 PROGRAM          *                       
         SPACE 1                                                                
EIEQU    CSECT                                                                  
         NMOD1 0,EIEQU                                                          
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,RC                                                    
         ZIC   R6,4(R1)            NUMBER OF DEMOS                              
         L     R7,4(R1)                                                         
         L     R8,8(R1)                                                         
         CLC   PRDBUFLN,=H'56'     IS THIS OLD OR NEW FORMAT                    
         BE    EIOLD                OLD                                         
         BCTR  R6,0                                                             
         EX    R6,CLEAROUT         CLEAR OUTPUT AREA                            
         LA    R6,1(R6)            RESTORE COUNT OF DEMOS                       
         LA    R1,1                                                             
EIEQU10  DS    0H                                                               
         MVC   DEMFULL(3),0(R7)                                                 
         CLI   1(R7),0                                                          
         BE    EIOLDX              NO MORE DEMOS TO CONVERT                     
         LA    R5,PRMYTAB                                                       
EIEQU20  DS    0H                                                               
         CLC   DEMFULL(3),0(R5)                                                 
         BE    EIEQU40                                                          
         CLI   1(R5),0                                                          
         BE    EIEQU30                                                          
         LA    R5,3(R5)                                                         
         LA    R1,1(R1)                                                         
         B     EIEQU20                                                          
         SPACE 1                                                                
EIEQU30  DS    0H                                                               
         MVC   0(3,R5),DEMFULL                                                  
EIEQU40  DS    0H                                                               
         STC   R1,0(R8)                                                         
         LA    R7,3(R7)                                                         
         BCT   R6,EIEQU10                                                       
         SPACE 1                                                                
EIOLD    DS    0H                                                               
         MVC   0(1,R8),0(R7)                                                    
         LA    R8,1(R8)                                                         
         LA    R7,1(R7)                                                         
         BCT   R6,EIOLD                                                         
EIOLDX   DS    0H                                                               
         XMOD1 1                                                                
         SPACE 1                                                                
CLEAROUT XC    0(0,R8),0(R8)       CLEAR OUTPUT AREA                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* PRODUCT GROUP (DISPLACEMENT) TABLE - 2 BYTE ENTRIES *                         
         SPACE 1                                                                
PGRTBL   CSECT                                                                  
         DS    440C                                                             
         SPACE 2                                                                
* FLIGHT TABLE - 9 BYTE ENTRIES *                                               
         SPACE 1                                                                
* BYTE 00    - PRODUCT CODE                                                     
* BYTE 01-03 - EBCDIC PRODUCT                                                   
* BYTE 04    - FLIGHT NUMBER                                                    
* BYTE 05-06 - FLIGHT START DATE                                                
* BYTE 07-08 - FLIGHT END DATE                                                  
         SPACE 1                                                                
FLTTBL   CSECT                                                                  
         DS    29565C              15 FLIGHTS OF 219 PRODUCTS                   
         SPACE 2                                                                
         BUFF  LINES=300,ROWS=6,COLUMNS=15,FLAVOR=BINARY,KEYLIST=(32,A)         
         SPACE 2                                                                
* SORTER WORK AREA *                                                            
         SPACE 1                                                                
SORTBUF  CSECT                                                                  
         DS    65536C                                                           
         SPACE 2                                                                
* PRODUCT ACTIVITY TABLES - MARKET GROUP, MARKET AND STATION *                  
         SPACE 1                                                                
MGPTBL   CSECT                                                                  
         DS    220C                                                             
         SPACE 1                                                                
MPTBL    CSECT                                                                  
         DS    220C                                                             
         SPACE 1                                                                
SPTBL    CSECT                                                                  
         DS    220C                                                             
         EJECT                                                                  
* FLIGHT RECORD DSECT *                                                         
         SPACE 1                                                                
       ++INCLUDE SPGENFLT                                                       
         SPACE 2                                                                
* PRDBUFF DSECT *                                                               
         SPACE 1                                                                
       ++INCLUDE SPREPPTBUF                                                     
         EJECT                                                                  
* DEDBLOCK                                                                      
       ++INCLUDE DEDBLOCK                                                       
         PRINT OFF                                                              
* ESTHDR                                                                        
       ++INCLUDE SPGENEST                                                       
* SPREPWORKD                                                                    
       ++INCLUDE SPREPWORKD                                                     
         DS    0D                                                               
DEMFULL  DS    F                                                                
PRMYTAB  DS    CL150                                                            
         EJECT                                                                  
* MEDPRTOPT                                                                     
         EJECT                                                                  
* SPREPMODES                                                                    
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
* SPMEDBLOCK                                                                    
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE DDBUFFALOD                                                     
         EJECT                                                                  
* SPGENBUY                                                                      
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
* SPGENGOAL                                                                     
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069SPREPX302 03/24/15'                                      
         END                                                                    
