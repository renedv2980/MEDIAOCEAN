*          DATA SET PPREPAI02  AT LEVEL 006 AS OF 10/24/16                      
*PHASE PPAI02A                                                                  
*INCLUDE PPFMTINO                                                               
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE PUBFLOAT                                                               
*INCLUDE DDUCOM                                                                 
*                                                                               
*                                                                               
         TITLE 'PPAI02 - AT&&T BILLING PRINT INTERFACE'                         
*                                                                               
*QOPT6   Y=TESTING ONLY - DO NOT WRITE SORTED OUTPUT TO FILE                    
*QOPT7   P=DISPLAY OUTPUT RECORDS CHAR + HEX                                    
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------   -------- ---------------------------------------- *         
* AKAT CUSTENH-3314 07/21/16  IMPROVE AUDIT REPORT                    *         
* AKAT DSSUP-7578   06/08/16  FIX ESTIMATE UCOMM BUG                  *         
* AKAT CUSTENH-2964 03/21/16  CHANGE COLUMN HEADING FOR CMA/DMA CD    *         
* AKAT CUSTENH-2964 03/17/16  INCLUDE CASH DISCOUNT WHEN CALC NET     *         
* AKAT CUSTENH-2964 02/04/16  FIX BINVALS BUG                         *         
*                                                                     *         
***********************************************************************         
PPAI02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPAI02,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'      2ND DSECT                                     
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         L     RC,PPFILEC                                                       
         LA    R6,1(RC)                                                         
         LA    R6,4095(R6)                                                      
         USING PPFILED,RC,R6                                                    
         LA    R7,PPAI02+4095                                                   
         LA    R7,1(R7)                                                         
         USING PPAI02+4096,R7     **NOTE USE OF R7 AS BASE REG*                 
         LA    R8,SPACEND                                                       
         USING PPAIWRKD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,PROCREQ                                                     
         BE    FIRSTB                                                           
         CLI   MODE,RUNLAST                                                     
         BE    FINAL                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
INITIAL  DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(X'14',TODAYY)   YYYYMMDD                      
         MVC   MYTODAY(2),TODAYY+4    MTH                                       
         MVI   MYTODAY+2,C'/'                                                   
         MVC   MYTODAY+3(2),TODAYY+6  DAY                                       
         MVI   MYTODAY+5,C'/'                                                   
         MVC   MYTODAY+6(4),TODAYY    YEAR YYYY                                 
*                                                                               
         MVI   FRSTSW,C' '         CLEAR FIRST TIME SWITCH                      
         MVI   TRLSW,0                                                          
         ZAP   TOTCNT,=P'0'                                                     
         LA    R2,HDRCNT           ZAP ACCUMS                                   
         LA    R3,4                                                             
INIT2    ZAP   0(4,R2),=P'0'                                                    
         LA    R2,4(R2)                                                         
         BCT   R3,INIT2                                                         
         ZAP   BILTOT,=P'0'        ZAP BILLING $ ACCUMULATOR                    
         ZAP   DTLTOT,=P'0'        ZAP DETAIL $ ACCUMULATOR                     
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
         L     R0,=V(PPFMTINO)                                                  
*                                                                               
         A     R0,RELO                                                          
         ST    R0,VFMTINO                                                       
         L     R0,=V(SORTER)                                                    
         A     R0,RELO                                                          
         ST    R0,VSORTER                                                       
         L     R0,=V(BINSRCH)                                                   
         A     R0,RELO                                                          
         ST    R0,VBINS2                                                        
         L     R0,=V(PUBFLOAT)                                                  
         ST    R0,VPUBFLOT                                                      
         A     R0,RELO                                                          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FIRSTB   DS    0H                                                               
*                                                                               
FB02     MVI   FORCEHED,C'Y'                                                    
*****    MVI   FCRDACTV,C'Y'                                                    
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   WRITESW,0           CLEAR WRITE-TO-FILE SWITCH                   
*                                                                               
         CLI   FRSTSW,C'Y'         FIRST TIME ?                                 
         BE    FBC10               NO                                           
*                                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD       INITIALIZE SORT              
*                                                                               
         CLI   QOPT6,C'Y'          TEST RUN ?                                   
         BE    FBC10               YES - NO OPEN                                
*                                                                               
         MVC   PPDYNDSN+13(L'QAGENCY),QAGENCY                                   
         GOTO1 DYNALLOC,DMCB,(0,=C'PAITAPE '),(0,PPDYNDSN)                      
         OPEN  (PAITAPE,OUTPUT)                                                 
*                                                                               
FBC10    DS    0H                                                               
         MVI   FRSTSW,C'Y'         SET FIRST TIME SWITCH                        
         MVC   ATTRECNT,=F'0'         SO ATT TABLE GETS REBUILT                 
*                                     FOR EACH REQUEST                          
*                                                                               
***                                                                             
* RESET BINVALS FOR EACH REQUEST OR IT MAY BE POINTING TO AND ENTRY             
* IN ATTTABLE FROM THE PREVIOUS REQUEST!                                        
***                                                                             
         MVC   BINVALS,=A(ATTMED) RESET BINVALS FOR EACH REQUEST!               
         MVI   ONEPRD,C'N'                                                      
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    FBC10C                                                           
         CLC   QPRODUCT,SPACES                                                  
         BE    FBC10C                                                           
         MVI   ONEPRD,C'Y'        SET FOR ONE PRODUCT                           
*                                                                               
FBC10C   DS    0H                                                               
         MVC   SVQELOW,=X'0000'                                                 
         MVC   SVQEHI,=X'FFFF'                                                  
         CLC   QEST,SPACES                                                      
         BE    FBC10C5                                                          
         CLC   QEST,=C'ALL'                                                     
         BE    FBC10C5                                                          
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,SVQELOW                                                       
         STH   R0,SVQEHI                                                        
         CLC   QESTEND,SPACES                                                   
         BE    FBC10C5                                                          
         PACK  DUB,QESTEND                                                      
         CVB   R0,DUB                                                           
         STH   R0,SVQEHI                                                        
*                                                                               
FBC10C5  GOTO1 DATCON,DMCB,QSTART,(3,BQS)                                       
         GOTO1 (RF),(R1),QEND,(3,BQE)                                           
*                                                                               
         MVC   SVQSTART(12),QSTART                                              
*                                                                               
         MVC   SVQOPT6,QOPT6                                                    
         MVC   SVQOPT7,QOPT7                                                    
*                                                                               
         XC    KEY,KEY             GET CLIENT                                   
         LA    R5,KEY                                                           
         USING PCLTKEY,R5                                                       
         MVC   PCLTKAGY(3),QAGENCY    AGENCY/MEDIA                              
         MVI   PCLTKRCD,2             CLIENT RECORD CODE                        
         MVC   PCLTKCLT,QCLIENT       CLIENT CODE                               
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                  GET PROFILES                                 
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'POB1'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   FBC80                                                            
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
FBC80    DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE                          
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         EJECT                                                                  
* READ AND OUTPUT TO BINSRCH ALL RELEVANT INVOICE RECORDS FOR CLIENT            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING PBILLKEY,R5                                                      
         MVC   PBILKAGY(3),QAGENCY    AGENCY/MEDIA                              
         MVI   PBILKRCD,8             RECORD CODE                               
         MVC   PBILKCLT,PCLTKCLT      CLIENT CODE                               
         CLI   ONEPRD,C'Y'            ONE PRODUCT REQUEST?                      
         BNE   BILL03                                                           
         MVC   PBILKPRD,QPRODUCT     PUT INTO KEY                               
         MVC   PBILKEST,SVQELOW                                                 
*                                                                               
BILL03   GOTO1 HIGH                                                             
         B     BILL10                                                           
*                                                                               
BILL05   GOTO1 SEQ                                                              
*                                                                               
BILL10   CLC   KEY(7),KEYSAVE      SAME AGY/MED/RCD/CLIENT ?                    
         BNE   BUYPROC             NO - BILLS DONE - GO DO BUYS                 
         CLI   ONEPRD,C'Y'         ONE PRODUCT REQUEST?                         
         BNE   BILL12                                                           
         CLC   PBILKPRD,QPRODUCT                                                
         BNE   BUYPROC                                                          
*                                                                               
BILL12   TM    PBILLKEY+25,X'80'   DELETED ?                                    
         BNZ   BILL05              YES - NEXT RECORD                            
         CLC   PBILKEST,SVQELOW    CHECK EST - MUST BE WITHIN RANGE             
         BL    BILL05                                                           
         CLC   PBILKEST,SVQEHI                                                  
         BH    BILL05                                                           
         OC    PBILKEST,PBILKEST       IGNORE BILLS WITH NO EST                 
         BZ    BILL05                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R0,PBILLREC                                                      
         ST    R0,AREC                                                          
         GOTO1 GETPRT              READ BILL RECORD                             
*                                                                               
         CLC   PBILLDAT,SVQSTART                                                
         BL    BILL05              NEXT RECORD                                  
         CLC   PBILLDAT,SVQEND                                                  
         BH    BILL05              NEXT RECORD                                  
*                                                                               
BILL20   DS    0H                  PROCESS BILL RECORD                          
*                                                                               
         MVC   SVEUCOM1,SPACES   INIT IN CASE OF ERROR                          
         MVC   SVEUCOM2,SPACES   INIT IN CASE OF ERROR                          
         MVC   SVEUCOM3,SPACES   INIT IN CASE OF ERROR                          
         MVC   SVEUCOM4,SPACES   INIT IN CASE OF ERROR                          
         MVC   SVEUCOM5,SPACES   INIT IN CASE OF ERROR                          
*                                                                               
         LA    R3,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R3                                                       
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         MVC   UCACOMF,VCOMFACS     COMFACS                                     
         MVI   UCSYS,C'P'        SYSTEM TO PRINT                                
         MVC   UCAGY,PBILKAGY                                                   
         MVC   UCMED,PBILKMED                                                   
         MVC   UCCLT,PBILKCLT      CLIENT                                       
         MVC   UCPRD,PBILKPRD      PRODUCT                                      
         MVC   UCEST,PBILKEST      ESTIMATE                                     
         OI    UCOPT,UCO8EST       RETURN 8 EST UCOMMS                          
*                                  NEXT 4 RETURNED IN PRODUCT AREA              
         GOTO1 =V(DDUCOM),UCOMBLK                                               
         CLI   UCERROR,0                                                        
         BNE   BILL21E      ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BO    BILL21E      NO EST DATA                                         
*                                                                               
         L     R1,UCEDATA     EST DATA                                          
         LA    RF,UCELENS     LENGTHS                                           
         LA    RE,SVEUCOM1    START OF 1ST EST UCOMM                            
         LHI   R0,4           JUST 4                                            
BILL21C  CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    *+16                                                             
         MVC   0(32,RE),0(R1)                                                   
         OC    0(32,RE),=32C' '                                                 
         LA    RE,32(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,BILL21C                                                       
*                             NEXT 4 IN PRODUCT AREA                            
         L     R1,UCPDATA     DATA FOR EST UCOMM 5-8                            
         LA    RF,UCPLENS     LENGTHS                                           
         LA    RE,SVEUCOM5    START AT SVECOM5                                  
         LHI   R0,1           JUST 1                                            
BILL21D  CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    *+16                                                             
         MVC   0(32,RE),0(R1)                                                   
         OC    0(32,RE),=32C' '                                                 
         LA    RE,32(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,BILL21D                                                       
         B     BILL21X                                                          
*                                                                               
         DROP  R3                                                               
*                                                                               
BILL21E  DS    0H                                                               
*                                                                               
BILL21X  DS    0H                                                               
         CLC   SVEUCOM4,SPACES                                                  
         BH    BILL21X5                                                         
         MVC   P1(23),=C'*** MISSING EST. UCOMM4'                               
         MVC   P1+26(1),PBILKMED                                                
         MVC   P1+28(3),PBILKCLT                                                
         MVC   P1+32(3),PBILKPRD                                                
         LH    R0,PBILKEST                                                      
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+36(3),DUB+6(2)                                                
         MVI   RCSUBPRG,1                                                       
         BAS   RE,MYRPT                                                         
*                                  USE BINSRCH TO ADD TO INVTAB                 
BILL21X5 DS    0H                                                               
         CLI   PBILKMED,C'M'       MAGAZINE                                     
         BE    BILL21X7                                                         
         CLI   PBILKMED,C'N'       NEWSPAPERS                                   
         BE    BILL21X7                                                         
         CLI   PBILKMED,C'T'       TRADE                                        
         BE    BILL21X7                                                         
*                                                                               
*        OTHER MEDIA NEED EST UCOMM5                                            
*                                                                               
BILL21X6 CLC   SVEUCOM5,SPACES                                                  
         BH    BILL21X7                                                         
         MVC   P1(23),=C'*** MISSING EST. UCOMM5'                               
         MVC   P1+26(1),PBILKMED                                                
         MVC   P1+28(3),PBILKCLT                                                
         MVC   P1+32(3),PBILKPRD                                                
         LH    R0,PBILKEST                                                      
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+36(3),DUB+6(2)                                                
         MVI   RCSUBPRG,1                                                       
         BAS   RE,MYRPT                                                         
*                                                                               
BILL21X7 DS    0H                                                               
*                                  USE BINSRCH TO ADD TO INVTAB                 
ATTAR1   DS    0H                                                               
*                                                                               
*  AT THIS POINT MUST ADD INVOICES TO TABLE AND IF THERE IS A                   
*  DUPLICATE ADD THEM TOGETHER                                                  
*       CREATE KEY                                                              
*                                                                               
         MVC    ATTMED,PBILKMED                                                 
         MVC    ATTCLI,PBILKCLT                                                 
         MVC    ATTPRO,PBILKPRD                                                 
         MVC    ATTEST,PBILKEST                                                 
         MVC    ATTINVMO,PBILKBMN+1    BILLING MONTH                            
         MVC    ATTINVN,PBILKBNO                                                
*                                                                               
*                                                                               
*        CODE BELOW REFELECTS THE DATA IN THE FILE                              
*        THAT APPLYS TO ALL OUTPUT RECS FOR AN INVOICE                          
*                                                                               
         MVI   ATTREC,C' '         SPACE-FILL OUTPUT RECORD                     
         MVC   ATTREC+1(L'ATTREC-1),ATTREC                                      
         MVC   ATTREC(6),PBILLRCV  STORE AMOUNT DUE (NET FOR THEM)              
*                                                                               
*                                                                               
BILL22   DS    0H                                                               
         GOTO1 VFMTINO,DMCB,PBILLDAT,(2,PBILKBNO),                     X        
               (PBILKMED,B1PROF),B1XPROF                                        
*                                                                               
         L     RF,DMCB                                                          
         MVC   ATTINV,0(RF)          FULL INVOICE NUMBER                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBILINVD),(20,WORK)                               
         LA    R4,ATTINVD                                                       
         MVC   0(2,R4),WORK+4        MM                                         
         MVI   2(R4),C'/'                                                       
         MVC   3(2,R4),WORK+6        DD                                         
         MVI   5(R4),C'/'                                                       
         MVC   6(4,R4),WORK          YYYY                                       
         MVC   ATTECOM4,SVEUCOM4      4TH ESTIMATE UCOMM                        
         MVC   ATTECOM5,SVEUCOM5      5TH ESTIMATE UCOMM                        
*                                                                               
BILL25   DS    0H                                                               
         MVC   WORK(12),PBILLREC     MUST READ ESTIMATE FOR NAME                
         MVI   WORK+3,X'07'                                                     
         CLC   PESTREC(12),WORK                                                 
         BE    BILL45                HAVE ESTIMATE NAME                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(12),PBILLREC                                                 
         MVI   KEY+3,X'07'         ESTIMATE                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
BILL45   XC    KEY,KEY               UCOMM READ OR EST READ                     
*                                    RESTORE SEQ BILL READ                      
         MVC   KEY(18),PBILLREC                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
BILL50   DS    0H                                                               
*                                                                               
         ZAP   DUB,PBILLRCV                                                     
*                                                                               
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) SUM                 
*                                                                               
         L      R2,AOFATTT        ADDRESS OF ATTTABLE                           
         PRINT  GEN                                                             
         MVI    BINVALS,1        RESET TO INSERT RECORD IF NOT FOUND            
         GOTO1  VBINS2,BINVALS                                                  
         PRINT  NOGEN                                                           
*                                                                               
         CLI    BINVALS,1          RECORD INSERTED                              
         BE     GOTOXIT                                                         
         OC     BINVALS+1(3),BINVALS+1 IF ZERO TABLE IS FULL                    
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
*                                                                               
         L       RF,BINVALS               ADDRESS OF FOUND RECORD               
         LA      RF,L'ATTKEY(RF)          PAST KEY                              
         AP      0(6,RF),PBILLRCV         ADD AMOUNT DUE                        
*                                         NET FOR THEM                          
GOTOXIT  DS    0H                                                               
         MVC   BINVALS,=A(ATTMED)                                               
         MVI   BINVALS,1                                                        
         B     BILL05                                                           
*                                       SET KEY                                 
         EJECT                                                                  
*                                                                               
BUYPROC  DS    0H          OUTPUT A REC FOR EACH DATED BILLING ELEM             
*                          OF ALL RELEVANT BUY RECORDS FOR CLIENT               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING PBUYKEY,R5                                                       
         MVC   PBUYKAGY(3),PCLTKAGY   AGENCY/MEDIA                              
         MVI   PBUYKRCD,X'20'         RECORD CODE                               
         MVC   PBUYKCLT,PCLTKCLT      CLIENT CODE                               
         CLI   ONEPRD,C'Y'      ONE PRODUCT REQUEST?                            
         BNE   *+10                                                             
         MVC   PBUYKPRD,QPRODUCT                                                
*                                                                               
         GOTO1 HIGH                                                             
         B     PROC04                                                           
*                                                                               
PROC02   GOTO1 SEQ                                                              
*                                                                               
PROC04   CLC   KEY(7),KEYSAVE      SAME AGY/MED/RCD/CLIENT ?                    
         BNE   EXIT                NO - DONE WITH THESE BUYS                    
*                                                                               
         CLI   ONEPRD,C'Y'         ONE PRODUCT REQUEST?                         
         BNE   PROC04C                                                          
         CLC   PBUYKPRD,QPRODUCT                                                
         BNE   EXIT                                                             
*                                                                               
PROC04C  DS    0H                                                               
         CLC   PBUYKACT,=C'ZZZ'    PASSIVE ?                                    
         BE    PROC02              YES - SKIP                                   
         CLC   PBUYKEST,SVQELOW    CHECK EST IN RANGE                           
         BL    PROC02                                                           
         CLC   PBUYKEST,SVQEHI                                                  
         BH    PROC02                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT              READ BUY RECORD                              
*                                                                               
         LA    R2,PBUYREC+33                                                    
PROC10   MVI   ELCODE,X'26'        BILL ELEMENT (PBILELEM)                      
         BAS   RE,NEXTEL                                                        
         BNE   PROC02              NEXT BUY RECORD                              
*                                                                               
         USING PPDUMD02,R2                                                      
         OC    PBLDATE,PBLDATE     SEE IF BILLED                                
         BZ    PROC10              NO - TRY FOR ANOTHER                         
         CLC   PBLDATE,BQS         TEST AGAINST BILL DATE                       
         BL    PROC10                                                           
         CLC   PBLDATE,BQE         TEST AGAINST BILL DATE                       
         BH    PROC10                                                           
*                                                                               
PROC20   DS    0H                                                               
         ST    R2,SVBILEL          SAVE ADDRESS OF PBILELEM                     
         MVI   SORTREC,C' '        SPACE-FILL RECORD                            
         MVC   SORTREC+1(L'SORTREC-1),SORTREC                                   
*                                                                               
         MVC   WORK(10),PBUYREC      MUST READ ESTIMATE FOR UDEF INFO           
         MVI   WORK+3,X'07'                                                     
         MVC   WORK+10(2),PBUYKEST                                              
         CLC   PESTREC(12),WORK                                                 
         BE    PROC30              ALREADY HAVE ESTIMATE REC                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(12),WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PBUYREC                                                  
         GOTO1 HIGH                  RESTORE BUY SEQ                            
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
PROC30   LA    R2,PESTREC+33                                                    
         MVI   ELCODE,X'08'        ESTIMATE USER DESCRIPTION ELEMENT            
         BAS   RE,NEXTEL                                                        
         BNE   PROC40              NOT FOUND                                    
*                                                                               
*        IF ESTIMATE USER DATA NEEDED GET IT HERE?                              
*                                                                               
PROC40   DS    0H                                                               
         L     R2,SVBILEL          RESTORE ADDRESS OF PBILELEM                  
*                                                                               
         MVC   WORK(10),PBUYREC      MUST READ PRODUCT FOR UDEF INFO            
         MVI   WORK+3,X'06'                                                     
         CLC   PPRDREC(10),WORK                                                 
         BE    PROC50                ALREADY HAVE PRODUCT REC                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PBUYREC                                                  
         GOTO1 HIGH                  RESTORE BUY SEQ                            
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
PROC50   LA    R2,PPRDREC+33                                                    
         MVI   ELCODE,X'08'        PRODUCT USER DESCRIPTION ELEMENT             
         BAS   RE,NEXTEL                                                        
         BNE   PROC52              NOT FOUND -                                  
*                                                                               
*        IF PROD USER DATA NEEDED FIND IT HERE                                  
*                                                                               
*                                                                               
PROC52   MVC   WORK(1),PBUYKMED      MUST READ PUBREC FOR DESC INFO             
         MVC   WORK+1(6),PBUYKPUB                                               
         MVC   WORK+7(2),PBUYKAGY                                               
         MVI   WORK+09,X'81'                                                    
         CLC   PUBREC(10),WORK                                                  
         BE    PROC60                ALREADY HAVE PUB RECORD                    
*                                    SAVDMA SHOULD STILL BE RIGHT               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),WORK                                                     
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         GOTO1 GETNAME                                                          
         CLI   PBUYKMED,C'I'                                                    
         BE    PROC55                                                           
         CLI   PBUYKMED,C'L'                                                    
         BE    PROC55                                                           
         CLI   PBUYKMED,C'S'                                                    
         BE    PROC55                                                           
*                                                                               
*        FOR OTHER MEDIA SEARCH FOR PUBGROUP                                    
*                                                                               
         XC    SAVDMA,SAVDMA        CLEAR SAVED PUB GROUP                       
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING GRPRECD,R2                                                       
         MVC   GRPKEY(3),PBUYREC    AGY/MED FROM BUY                            
         MVI   GRPKRCOD,GRPPBGQ      X'3C'                                      
         MVC   GRPPVAL,PBUYKPUB                                                 
         MVI   GRPPID,C'M'          LOOK UNDER SCHEME M                         
         GOTO1 HIGH                                                             
         CLC   KEY(14),KEYSAVE                                                  
         BE    PROC53                                                           
         MVC   P1(21),=C'*** MISSING PUB GROUP'                                 
         MVC   P1+26(1),PBUYKMED                                                
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PBUYKPUB),P1+28                               
         MVI   RCSUBPRG,1                                                       
         BAS   RE,MYRPT                                                         
         MVC   SORTDMA,SPACES      LEAVE EMPTY                                  
         MVC   SAVDMA(4),SPACES                                                 
         B     PROC59                                                           
*                                  USE BINSRCH TO ADD TO INVTAB                 
*        GRPPCODE  IS PWOS                                                      
*                                                                               
PROC53   ICM   R1,B'1100',GRPPCODE                                              
         SRL   R1,12                  DD DD ?? ??  =>  00 0D DD D?              
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'           00 0D DD DS                               
         UNPK  CODECHAR(5),FULL+1(3)               =>  Z0 ZD ZD ZD ZD           
         LH    R1,=H'3'               THEY USE 3                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SORTDMA(0),CODECHAR+1  CODE TO SCREEN LINE BLANK PADDED          
         MVI   SORTDMA+3,C' '                                                   
         MVC   SAVDMA,SORTDMA                                                   
         B     PROC59                                                           
*                                                                               
PROC55   MVC   SORTDMA,SVEUCOM5                                                 
*                                                                               
PROC59   XC    KEY,KEY                                                          
         MVC   KEY(25),PBUYREC                                                  
         GOTO1 HIGH                  RESTORE BUY SEQ                            
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
PROC60   DS    0H                                                               
         MVC   SORTMED,PBUYKMED                                                 
         MVC   SORTCLT,PBUYKCLT                                                 
         MVC   SORTCNAM,PCLTNAME                                                
*                                                                               
         L     R2,SVBILEL         ADDRESS OF BILLING ELEMENT                    
         USING PBILELEM,R2                                                      
         MVC   SORTPRD,PBPRD      PRODUCT                                       
         MVC   SORTINV,PBINVNO    INVOICE #                                     
         MVC   SORTBDAT,PBLDATE                                                 
         MVC   SORTDMA,SAVDMA                                                   
*                                                                               
PROC65   DS    0H       NOW TRY AND FINF INVOICE IN BINSRCH TABLE               
*                                                                               
         MVC    ATTMED,PBUYKMED                                                 
         MVC    ATTCLI,PBUYKCLT                                                 
         MVC    ATTPRO,PBPRD       FROM BILLING ELEMENT                         
         MVC    ATTEST,PBUYKEST                                                 
         MVC    ATTINVMO,PBLDATE+1     BILLING MONTH FROM ELEMENT               
         MVC    ATTINVN,PBINVNO        INVOICE # FROM ELEMENT                   
*                                                                               
         LA     RE,ATTMED                                                       
         ST     RE,BINVALS                                                      
         MVI    BINVALS,0          SEARCH ONLY - DON'T ADD                      
         GOTO1  VBINS2,BINVALS                                                  
         CLI    BINVALS,X'01'       NOT FOUND - MUST DIE                        
         BNE    *+6                                                             
         DC     H'0'                SOMETHING VERY WRONG                        
*                                                                               
***TEMP         USE FOR CHECKING FOR A SPECIFIC INVNO                           
***TEMP  CLC    PBINVNO,=X'169A'    5786                                        
***TEMP  BNE    *+6                                                             
***TEMP  DC     H'0'                                                            
***TEMP                                                                         
         L      RF,BINVALS               ADDRESS OF FOUND RECORD                
         LA     RF,L'ATTKEY(RF)          PAST KEY                               
         MVI    BINVALS,1           RESET TO ADD IF NOT FOUND                   
*                                                                               
         MVC    SORTINV$,0(RF)                                                  
         MVC    SORTINVF,6(RF)                                                  
         MVC    SORTINVD,16(RF)    6+10                                         
         MVC    SORTCOM4,26(RF)    6+10+10                                      
         MVC    SORTCOM5,58(RF)    6+10+10+32                                   
*                                                                               
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTENAM(3),DUB+6(2)                                             
         MVC   SORTENAM+4(20),PESTNAME                                          
         OC    SORTENAM+4(20),SPACES                                            
         MVC   SORTPNAM(3),PBPRD                                                
**NO-OP  MVC   SORTPNAM+4(20),PPRDNAME      NOW ONLY CODE                       
         OC    SORTPNAM+4(20),SPACES                                            
         MVC   SORTMEDN,SPACES                                                  
         MVC   SORTMEDN(10),PAGYMED                                             
         CLI   PBUYKMED,C'I'       ALTER TO 11 CHARACTERS                       
         BNE   *+10                                                             
         MVC   SORTMEDN(11),=C'INTERACTIVE'                                     
*                                                                               
         MVC   WORK(3),PBDBDATE     BILLABLE DATE                               
         MVI   WORK+2,X'01'         SET DAY TO 1                                
         GOTO1 DATCON,DMCB,(3,WORK),(X'14',WORK+3)                              
         MVC   SORTMOS(2),WORK+7      MTH                                       
         MVI   SORTMOS+2,C'/'                                                   
         MVC   SORTMOS+3(2),WORK+9    DAY                                       
         MVI   SORTMOS+5,C'/'                                                   
         MVC   SORTMOS+6(4),WORK+3    YEAR YYYY                                 
*                                                                               
*                                                                               
         GOTO1 VPUBFLOT,DMCB,PUBREC,SORTPUB                                     
         LA   R4,SORTPUB+59                                                     
PROC68   CLI  0(R4),C' '           SCAN BACKWARDS                               
         BH   PROC70                                                            
         BCT  R4,PROC68                                                         
*                                                                               
PROC70   LA   R4,2(R4)                                                          
         MVI  0(R4),C'('                                                        
         IC   R0,PAGYPROF+12                                                    
         GOTO1 PUBEDIT,DMCB,((R0),PUBREC+1),1(R4)                               
         LA   R4,SORTPUB+59                                                     
PROC73   CLI  0(R4),C' '           SCAN BACKWARDS                               
         BH   PROC75                                                            
         BCT  R4,PROC73                                                         
PROC75   MVI  1(R4),C')'                                                        
*                                                                               
         MVC  FULL,PBGROSS                                                      
         L    R0,FULL                                                           
         MVC  FULL,PBAGYCOM                                                     
         S    R0,FULL                                                           
         MVC  FULL,PBCSHDSC                                                     
         S    R0,FULL                                                           
*                                                                               
         ST   R0,SORTBNET                                                       
*                                                                               
         CVD  R0,DUB                                                            
         AP   DTLTOT,DUB              TOTAL OF DETAILS                          
*                                                                               
         DROP  R2                                                               
*        PASS RECORDS TO SORT HERE                                              
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         B     PROC10              NEXT BILL ELEMENT                            
*                                   (LATER ADD NUMBER)                          
         EJECT                                                                  
FINAL    DS    0H                  OUTPUT CONTROL RECORD                        
         LA    RE,OUTREC           CLEAR OUTREC                                 
         LH    RF,=H'400'                                                       
         XCEF                                                                   
*                                                                               
*        FIRST OUTPUT COLUMN HEADINGS                                           
*                                                                               
         L     RF,=A(COLHDS)                                                    
         MVC   OUTREC(250),0(RF)                                                
         MVC   OUTREC+250(COLHDLEN-250),250(RF)                                 
         MVC   OUTREC-4(2),=H'4'                                                
         LH    RF,OUTREC-4                                                      
         LA    RF,COLHDLEN(RF)                                                  
         STH   RF,OUTREC-4                                                      
*                                                                               
         AP    HDRCNT,=P'1'        HEADERS                                      
         BAS   RE,MWRITE           PUT TO FILE                                  
*                                                                               
*                                                                               
FINLUP   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R5,15,4(R1)                                                      
         BZ    TOTALS              NO MORE RECS - FINISH UP                     
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
         MVC   SORTREC,0(R5)       MOVE SORTED BACK TO SORTREC                  
         CLC   LASTINV,SORTKEY     SEE IF NEW INVOICE #                         
         BNE   FIN5                MED/CLT/PRD/INV#                             
**NO-OP  AP    SEQNO,=P'1'         LEAVE AS 1                                   
         B     FIN10                                                            
*                                                                               
FIN5     ZAP   SEQNO,=P'1'         RESET TO ONE                                 
         AP    INVCNT,=P'1'       COUNTING INVOICES                             
         MVC   LASTINV,SORTKEY     MED/CLT/PRD/INV#                             
*                                                                               
FIN10    LA    RE,OUTREC           CLEAR OUTREC                                 
         LH    RF,=H'400'                                                       
         XCEF                                                                   
*                                                                               
FIN10A   LA    R2,P1               POINT TO PRINT LINE                          
         MVI   FORCEHED,C'N'       TURN OFF FORCE HEADLINES TO PRINT            
         CLI   RCSUBPRG,100        DID WE ALREADY PRINT THIS ONCE?              
         BE    FIN10AA             YES                                          
         MVI   FORCEHED,C'Y'       FORCE HEADLINES TO PRINT                     
         MVI   RCSUBPRG,100        RCSUBPRG = 100 FOR REPORT                    
*                                                                               
         USING BLINED,R2           BLINED DSECT TO COVER PRINT LINE             
FIN10AA  MVC   BLMEDIA,SORTMED     MEDIA                                        
         MVC   BLCLT,SORTCLT       CLIENT                                       
         MVC   BLCLNAME,SORTCNAM   CLIENT NAME                                  
         OC    BLCLNAME,SPACES     CLIENT NAME                                  
         MVC   BLPRD,SORTPRD       PRODUCT                                      
         MVC   BLEST,SORTENAM      ESTIMATE                                     
         MVC   BLINVNO,SORTINVF    FULL INVOICE NUMBER                          
         MVC   BLINVD,SORTINVD     INVOICE DATE                                 
         EDIT  SORTINV$,(13,BLGROSS),2,MINUS=YES                                
         MVC   BLUCOMM,SORTCOM4    4TH ESTIMATE UCOMM                           
         MVC   BLCMADMA,SORTDMA    CMA/DMA CODE                                 
         CLI   SORTMED,C'M'        MAGAZINES?                                   
         BE    *+8                 YES                                          
         CLI   SORTMED,C'N'        NEWSPAPERS                                   
         BE    *+8                 YES                                          
         CLI   SORTMED,C'T'        TRADE                                        
         BE    *+10                YES                                          
         MVC   BLCMADMA,SORTCOM5   FOR OTHER MEDIA USE EST UCOMM5               
         BRAS  RE,PRNTLINE         SEE IF WE SHOULD PRINT THIS LINE             
         BNE   FIN10B              NO - IT'S A DUPLICATE                        
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R2                  DROP USING                                   
*                                                                               
FIN10B   LA    R4,OUTREC                                                        
         MVC   0(06,R4),=C'"INV",'                                              
         LA    R4,6(R4)                                                         
         MVC   0(8,R4),=C'"95732",'                                             
         LA    R4,8(R4)                                                         
*                                                                               
         MVI   0(R4),C'"'                                                       
         EDIT  SEQNO,(3,1(R4)),0,ALIGN=LEFT                                     
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVC   1(2,R4),=C'",'                                                   
         LA    R4,3(R4)                                                         
*                                                                               
         MVI   0(R4),C'"'                                                       
         MVC   1(10,R4),MYTODAY                                                 
         MVC   11(2,R4),=C'",'                                                  
         LA    R4,13(R4)                                                        
         MVC   0(05,R4),=C'"MM",'                                               
         LA    R4,5(R4)                                                         
         MVI   0(R4),C'"'                                                       
         MVC   1(10,R4),SORTINVF                                                
         MVC   11(2,R4),=C'",'                                                  
         LA    R4,13(R4)                                                        
         MVI   0(R4),C'"'                                                       
         MVC   1(10,R4),SORTINVD                                                
         MVC   11(2,R4),=C'",'                                                  
         LA    R4,13(R4)                                                        
*                                                                               
         CP    SORTINV$,=P'0'                                                   
         BL    FIN10N                                                           
         MVC   0(2,R4),=C'"$'                                                   
         LA    R4,2(R4)                                                         
         EDIT  SORTINV$,(13,0(R4)),2,ALIGN=LEFT                                 
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R4),=C'",'                                                   
         LA    R4,2(R4)                                                         
         B     FIN10X                                                           
*                                                                               
*        BRACKET NEGATIVE AMOUNTS                                               
*                                                                               
FIN10N   MVC   0(3,R4),=C'"($'                                                  
         LA    R4,3(R4)                                                         
         EDIT  SORTINV$,(13,0(R4)),2,ALIGN=LEFT                                 
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(3,R4),=C')",'                                                  
         LA    R4,3(R4)                                                         
*                                                                               
FIN10X   MVI   0(R4),C'"'                                                       
         MVC   1(32,R4),SORTCOM4    4TH EST UCOMM                               
         LA    R4,32(R4)                                                        
FIN12    CLI   0(R4),C' '    SCAN BACKWARD FOR NON-SPACE                        
         BH    FIN13                                                            
         BCT   R4,FIN12                                                         
*                                                                               
FIN13    MVC   1(2,R4),=C'",'                                                   
         LA    R4,3(R4)                                                         
*                                                                               
         MVI   0(R4),C'"'                                                       
         MVC   1(24,R4),SORTENAM                                                
         LA    R4,24(R4)                                                        
FIN15    CLI   0(R4),C' '    SCAN BACKWARD FOR NON-SPACE                        
         BH    FIN20                                                            
         BCT   R4,FIN15                                                         
*                                                                               
FIN20    MVC   1(2,R4),=C'",'                                                   
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'"'                                                       
         MVC   1(3,R4),SORTCLT                                                  
         MVC   4(2,R4),=C'",'                                                   
         LA    R4,6(R4)                                                         
         MVI   0(R4),C'"'                                                       
         MVC   1(24,R4),SORTPNAM                                                
         LA    R4,24(R4)                                                        
FIN25    CLI   0(R4),C' '    SCAN BACKWARD FOR NON-SPACE                        
         BH    FIN30                                                            
         BCT   R4,FIN25                                                         
*                                                                               
FIN30    MVC   1(2,R4),=C'",'                                                   
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'"'                                                       
         MVC   1(11,R4),SORTMEDN                                                
         LA    R4,11(R4)                                                        
FIN35    CLI   0(R4),C' '    SCAN BACKWARD FOR NON-SPACE                        
         BH    FIN40                                                            
         BCT   R4,FIN35                                                         
*                                                                               
FIN40    MVC   1(2,R4),=C'",'                                                   
         LA    R4,3(R4)                                                         
*                                                                               
         MVI   0(R4),C'"'                                                       
         MVC   1(10,R4),SORTMOS                                                 
         MVC   11(2,R4),=C'",'                                                  
         LA    R4,13(R4)                                                        
         MVI   0(R4),C'"'                                                       
         MVC   1(4,R4),SORTDMA     (PUBGROUP)                                   
         CLI   SORTMED,C'M'        MAGAZINES                                    
         BE    FIN42                                                            
         CLI   SORTMED,C'N'        NEWSPAPERS                                   
         BE    FIN42                                                            
         CLI   SORTMED,C'T'        TRADE                                        
         BE    FIN42                                                            
         MVC   1(4,R4),SORTCOM5    FOR OTHER MEDIA USE EST UCOMM5               
*                                                                               
FIN42    LA    R4,4(R4)                                                         
FIN42A   CLI   0(R4),C' '          SCAN BACKWARDS FOR NON-SPACE                 
         BH    FIN42X                                                           
         BCT   R4,FIN42A                                                        
FIN42X   MVC   1(2,R4),=C'",'                                                   
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'"'                                                       
         MVC   1(60,R4),SORTPUB                                                 
         LA    R4,60(R4)                                                        
FIN45    CLI   0(R4),C' '    SCAN BACKWARD FOR NON-SPACE                        
         BH    FIN50                                                            
         BCT   R4,FIN45                                                         
*                                                                               
FIN50    MVC   1(2,R4),=C'",'                                                   
         LA    R4,3(R4)                                                         
*                                                                               
         L     R0,SORTBNET                                                      
         C     R0,=F'0'                                                         
         BL    FIN50N                                                           
         MVC   0(2,R4),=C'"$'                                                   
         LA    R4,2(R4)                                                         
         EDIT  (B4,SORTBNET),(13,0(R4)),2,ALIGN=LEFT                            
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R4),=C'",'                                                   
         LA    R4,2(R4)                                                         
         B     FIN50P                                                           
*                                                                               
FIN50N   MVC   0(3,R4),=C'"($'                                                  
         LA    R4,3(R4)                                                         
         EDIT  (B4,SORTBNET),(13,0(R4)),2,ALIGN=LEFT                            
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(3,R4),=C')",'                                                  
         LA    R4,3(R4)                                                         
FIN50P   MVC   0(04,R4),=C'"1",'                                                
         LA    R4,4(R4)                                                         
         MVC   0(02,R4),=C'""'  EMPTY ATT REGION CLUSTERS (NO COMMA)            
         LA    R4,2(R4)                                                         
*                                                                               
FIN50X   DS    0H                                                               
*                                                                               
BLDREND  DS    0H                                                               
         LA    RE,OUTREC-4                                                      
         LR    RF,R4                                                            
         SR    RF,RE      DIFFERENCE SHOULD BE RECORD LENGTH                    
         STH   RF,OUTREC-4                                                      
         BAS   RE,MWRITE                                                        
*                                                                               
         CLI   TRLSW,1                 DID I JUST DO TRAILER?                   
         BE    TOT30                                                            
*                                                                               
         B     FINLUP     ELSE GO DO NEXT SORT RECORD                           
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   WRITESW,1           WRITE TO FILE                                
         MVI   RCSUBPRG,10                                                      
*                                                                               
TOTALS   DS    0H                                                               
         LA    RE,OUTREC           CLEAR OUTREC                                 
         LH    RF,=H'400'                                                       
         XCEF                                                                   
*                                                                               
         LA    R4,OUTREC                                                        
         MVI   0(R4),C'"'                                                       
         MVC   1(5,R4),=C'TRL",'                                                
         LA    R4,6(R4)                                                         
         MVC   0(4,R4),=C'"",'     EMPTY FIELD                                  
         LA    R4,3(R4)                                                         
*                                                                               
         CP    BILTOT,=P'0'                                                     
         BL    TOT05N                                                           
         MVC   0(2,R4),=C'"$'                                                   
         LA    R4,2(R4)                                                         
         EDIT  (P8,BILTOT),(14,0(R4)),2,ALIGN=LEFT                              
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(2,R4),=C'",'                                                   
         LA    R4,2(R4)                                                         
         B     TOT05X                                                           
*                                                                               
TOT05N   MVC   0(3,R4),=C'"($'                                                  
         LA    R4,3(R4)                                                         
         EDIT  (P8,BILTOT),(14,0(R4)),2,ALIGN=LEFT                              
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVC   0(3,R4),=C')",'                                                  
         LA    R4,3(R4)                                                         
*                                                                               
TOT05X   MVC   0(3,R4),=C'"",'    EMPTY FIELD                                   
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'"'                                                       
         EDIT  SORTCNT,(5,1(R4)),0,ALIGN=LEFT                                   
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'"'        NO COMMA ON FINAL ENTRY                        
         LA    R4,1(R4)                                                         
         MVI   TRLSW,1             SET DOING TRAILER RECORD                     
         B     BLDREND                                                          
*                                                                               
TOT30    MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,90                                                      
         AP    TRLCNT,=P'1'        TRAILER COUNT - ALWAYS 1                     
         MVI   P1,C' '             MAY BE UNPRINTED LEFTOVER IN P               
         MVC   P1+1(L'P1-1),P1     INIT TO SPACES                               
         LA    R4,TITLES                                                        
         LA    R5,HDRCNT                                                        
         LA    R3,4                FOR BCT                                      
*                                                                               
TOT50    MVC   P1+7(17),0(R4)                                                   
         EDIT  (P4,0(R5)),(9,P1+26),0,COMMAS=YES                                
         BAS   RE,MYRPT                                                         
         LA    R4,17(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R3,TOT50                                                         
*                                                                               
         BAS   RE,MYRPT            SKIP A LINE                                  
         MVC   P1+7(19),=C'FILE RECORDS OUTPUT'                                 
         EDIT  TOTCNT,(9,P1+26),0,COMMAS=YES                                    
         MVI   P1+35,C'*'                                                       
         BAS   RE,MYRPT                                                         
         BAS   RE,MYRPT   SKIP A LINE                                           
         MVC   P1+7(08),=C'INVOICES'                                            
*                                                                               
         LA    R4,P1+18                                                         
         CP    BILTOT,=P'0'                                                     
         BL    TOT50N                                                           
         MVI   0(R4),C'$'                                                       
         LA    R4,1(R4)                                                         
         EDIT  (P8,BILTOT),(14,0(R4)),2,COMMAS=YES,ALIGN=LEFT                   
         B     TOT50X                                                           
*                                                                               
TOT50N   MVC   0(2,R4),=C'($'                                                   
         LA    R4,2(R4)                                                         
         EDIT  (P8,BILTOT),(14,0(R4)),2,COMMAS=YES,ALIGN=LEFT                   
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVI   0(R4),C')'                                                       
TOT50X   DS    0H                                                               
         BAS   RE,MYRPT                                                         
*                                                                               
TOT60    MVC   P1+7(07),=C'DETAILS'                                             
*                                                                               
         LA    R4,P1+18                                                         
         CP    DTLTOT,=P'0'                                                     
         BL    TOT60N                                                           
         MVI   0(R4),C'$'                                                       
         LA    R4,1(R4)                                                         
         EDIT  (P8,DTLTOT),(14,0(R4)),2,COMMAS=YES,ALIGN=LEFT                   
         B     TOT60X                                                           
*                                                                               
TOT60N   MVC   0(2,R4),=C'($'                                                   
         LA    R4,2(R4)                                                         
         EDIT  (P8,DTLTOT),(14,0(R4)),2,COMMAS=YES,ALIGN=LEFT                   
         AR    R4,R0        ADD LENGTH OF OUTPUT                                
         MVI   0(R4),C')'                                                       
TOT60X   BAS   RE,MYRPT                                                         
*                                                                               
         CP    BILTOT,DTLTOT                                                    
         BE    TOT70                                                            
         BAS   RE,MYRPT                                                         
         MVC   P1+1(41),=C'*****************************************'           
         MVC   P2+1(41),=C'*** WARNING-INVOICE AND DETAIL $ MISMATCH'           
         MVC   P3+1(41),=C'*****************************************'           
         BAS   RE,MYRPT                                                         
TOT70    DS    0H                                                               
*                                                                               
         CLI   SVQOPT6,C'Y'        TEST RUN ?                                   
         BE    EXIT                YES - NO CLOSE                               
         CLOSE PAITAPE                                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
*                                                                               
MYRPT    NTR1                                                                   
*                                                                               
         CLI   QOPT6,C'Y'              SEE IF TEST RUN                          
         BE    MYRPT2                                                           
         CLI   SVQOPT6,C'Y'            SEE IF TEST RUN                          
         BNE   *+10                                                             
*                                                                               
MYRPT2   MVC   HEAD1+04(12),=C'**TEST RUN**'                                    
*****    CLI   MODE,RUNLAST                                                     
*****    BNE   *+10                                                             
         MVC   QSTART(12),SVQSTART                                              
*                                                                               
MYRPT5   GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
MWRITE   NTR1                      FIND RECORD LENGHT IN LENTAB                 
         CLI   SVQOPT7,C'P'                                                     
         BE    WRIT1                                                            
         CLI   QOPT7,C'P'                                                       
         BNE   WRIT2                                                            
WRIT1    MVI   RCSUBPRG,10                                                      
         MVC   P1+1(125),OUTREC                                                 
         MVC   P2+1(125),OUTREC+125                                             
         MVC   P3+1(125),OUTREC+250                                             
         MVC   P4+1(25),OUTREC+375                                              
         MVI   P2,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P3,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P4,0       SO LINE WILL ALWAYS PRINT                             
*                                                                               
WRIT1A   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
         MVI   RCSUBPRG,10                                                      
         GOTO1 HEXOUT,DMCB,OUTREC-4,P1+10,54,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+50,P2+18,50,=C'N'                               
         GOTO1 (RF),(R1),OUTREC+100,P3+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+150,P4+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+200,P5+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+250,P6+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+300,P7+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+350,P8+18,50,=C'N'                              
WRIT1C   MVC   P1+1(7),=C'001-050'                                              
         MVC   P2+1(7),=C'051-100'                                              
         MVC   P3+1(7),=C'101-150'                                              
         MVC   P4+1(7),=C'151-200'                                              
         MVC   P5+1(7),=C'201-250'                                              
         MVC   P6+1(7),=C'251-300'                                              
         MVC   P7+1(7),=C'301-350'                                              
         MVC   P8+1(7),=C'351-400'                                              
WRIT1E   MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
WRIT2    DS    0H                                                               
         CLI   SVQOPT6,C'Y'     SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         LA    R1,PAITAPE                                                       
*                                                                               
WRIT2B   LA    R0,OUTREC-4                                                      
         PRINT GEN                                                              
         PUT   (1),(0)                                                          
         PRINT NOGEN                                                            
WRIT3    AP    TOTCNT,=P'1'                                                     
         J     EXIT                                                             
*                                                                               
PRNTLINE NTR1                                                                   
*                                                                               
         USING BLINED,R2           BLINED DSECT TO COVER PRINT LINE             
         CLC   SVBLINE,BLINE       SAME AS PREVIOUS RECORD PRINTED?             
         BNE   PL50                NO                                           
*                                                                               
         LA    RE,SVCMADMA         LIST OF SAVED CMA/DMA CODES                  
         LA    RF,MAXCMA           MAX CMA/DMA CODES IN THE LIST                
*                                                                               
PL10     OC    0(4,RE),0(RE)       EMPTY SLOT?                                  
         BZ    PL20                YES                                          
         CLC   0(4,RE),BLCMADMA    MATCH ON CMA/DMA CODE?                       
         BE    PLNEQ               YES - SET CC NEQ TO NOT PRINT LINE           
         LA    RE,4(RE)            BUMP TO NEXT SLOT                            
         BCT   RF,PL10             TRY NEXT SLOT                                
         DC    H'0'                EXPAND MAXCMA                                
*                                                                               
PL20     MVC   0(4,RE),BLCMADMA    SAVE THE CMA/DMA CODE                        
         B     PLEQU               PRINT IT                                     
*                                                                               
PL50     MVC   SVBLINE,BLINE       SAVE OFF LAST RECORD                         
         XC    SVCMADMA,SVCMADMA   CLEAR CMA/DMA CODES                          
         MVC   SVCMADMA(4),BLCMADMA                                             
*                                                                               
PLEQU    CR    RE,RE               SET CC EQU                                   
         B     *+6                 RETURN                                       
PLNEQ    LTR   RE,RE               SET CC NOT EQ                                
         J     EXIT                RETURN                                       
         DROP  R2                  DROP USING                                   
*                                                                               
         LTORG                                                                  
*                                                                               
TITLES   DS    0C                                                               
         DC    CL17'COLUMN HEADERS'                                             
         DC    CL17'INVOICES'                                                   
         DC    CL17'LINE ITEMS'                                                 
         DC    CL17'TRAILERS'                                                   
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,12,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=232'                                   
*                                                                               
         LTORG                                                                  
BINVALS  DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(ATTMED)        RECORD TO BE ADDED                            
         DC    A(ATTTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
ATTRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    F'90'              LEN OF RECORD                                 
         DC    F'13'              KEY SIZE                                      
         DC    F'2000'            MAX NUMBER OF RECORDS                         
*                                                                               
AOFATTT  DC    A(ATTTABLE)                                                      
ATTKEY   DS    0XL13                                                            
ATTMED   DS    CL1                                                              
ATTCLI   DS    CL3                                                              
ATTPRO   DS    CL3                                                              
ATTEST   DS    XL2                                                              
ATTINUMB DS    0XL3                                                             
ATTINVMO DS    XL1           INVOICE MONTH                                      
ATTINVN  DS    XL2           INVOICE NUMBER                                     
         DS    XL1                                                              
*            ______                                                             
*              13                                                               
ATTREC   DS    0CL90                                                            
ATTNET   DS    PL6           AMOUNT DUE (NET FOR THEM)                          
ATTINV   DS    CL10          INVOICE # AS ON BILL                               
ATTINVD  DS    CL10          INVOICE DATE AS ON BILL                            
ATTECOM4 DS    CL32                                                             
ATTECOM5 DS    CL32                                                             
ENDATTR  DS    0C                                                               
***********************                                                         
         EJECT                                                                  
*                                                                               
PPDYNDSN DC    CL20'PRTTAPE.PP0AIAG1'                                           
*                                                                               
PAITAPE  DCB   DDNAME=PAITAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=400,                                              X        
               BLKSIZE=4000,                                           X        
               MACRF=PM                                                         
         EJECT                                                                  
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
*                                                                               
*                                                                               
COLHDS   CSECT                                                                  
*                                                                               
*        HEADER COLUMN HEADINGS                                                 
*                                                                               
         DC    C'"REC TYPE",'                                                   
         DC    C'"VENDOR RECID(Provided by AT&&T)",'                            
         DC    C'"FILE SEQUENCE #",'                                            
         DC    C'"FILE DATE",'                                                  
         DC    C'"IMPORT TYPE",'                                                
         DC    C'"INVOICE NUMBER",'                                             
         DC    C'"INV DATE",'                                                   
         DC    C'"INV AMOUNT",'                                                 
         DC    C'"ESTIMATE #",'                                                 
         DC    C'"MEC ESTIMATE #",'                                             
         DC    C'"CLIENT",'                                                     
         DC    C'"PRODUCT",'                                                    
         DC    C'"MEDIA TYPE",'                                                 
         DC    C'"DATE OF SERVICE",'                                            
         DC    C'"CMA/DMA CODE",'                                               
         DC    C'"LINE DESCRIPTION",'                                           
         DC    C'"LINE AMOUNT",'                                                
         DC    C'"QTY",'                                                        
         DC    C'"ATT REGION CLUSTERS"'    NO FINAL COMMA                       
COLHDX   EQU   *                                                                
COLHDLEN EQU   *-COLHDS                                                         
*                                                                               
BLINED   DSECT                                                                  
BLINE    DS    0CL132                                                           
         DS    CL4                                                              
BLMEDIA  DS    CL1                                                              
         DS    CL2                                                              
BLCLT    DS    CL3                                                              
         DS    CL1                                                              
BLCLNAME DS    CL20                                                             
         DS    CL1                                                              
BLPRD    DS    CL3                                                              
         DS    CL1                                                              
BLEST    DS    CL3                                                              
         DS    CL1                                                              
BLINVNO  DS    CL10                                                             
         DS    CL1                                                              
BLINVD   DS    CL10                                                             
         DS    CL1                                                              
BLGROSS  DS    CL14                                                             
         DS    CL1                                                              
BLUCOMM  DS    CL32                                                             
BKEYLENQ EQU   *-BLINE                                                          
         DS    CL1                                                              
BLCMADMA DS    CL4                                                              
*                                                                               
PPAIWRKD DSECT                                                                  
*                                                                               
INVPARS  DS    6F               FOR INVTAB BINSRCH                              
*                                                                               
INVMAX   EQU   10000            ALLOW 10000 INVOICES PER CLIENT                 
TODAYY   DS    CL8              YYYYMMDD                                        
MYTODAY  DS    CL10             MM/DD/YYYY                                      
TRLSW    DS    XL1              1 = DOING TRAILER RECORD                        
LASTINV  DS    CL9              MED/CLT/PRD/INV                                 
SEQNO    DS    PL3                                                              
*                                                                               
HDRCNT   DS    PL4'0'                                                           
INVCNT   DS    PL4'0'              INVOICE HEADER COUNT                         
SORTCNT  DS    PL4'0'              RECORDS READ FROM SORTER                     
TRLCNT   DS    PL4'0'              TRAILER REORDS                               
*                                                                               
TOTCNT   DS    PL4'0'              TOTAL FILE RECORDS                           
*                                                                               
BILTOT   DS    D                   BILLING AMOUNT $ TOTAL                       
DTLTOT   DS    D                   SUM OF BILLING ON DETAIL RECS                
*                                                                               
ONEPRD   DS    CL1                 SET TO Y IF DOING ONE PRODUCT                
*                                                                               
VFMTINO  DS    A                                                                
VSORTER  DS    A                                                                
VBINS2   DS    A                                                                
VPUBFLOT DS    A                                                                
*                                                                               
*        DEFAULT TO X'0000' AND X'FFFF'                                         
*        IF EST=ALL OR MISSING                                                  
SVQELOW  DS    H        EST LOW                                                 
SVQEHI   DS    H        EST HIGH - EQUAL TO LOW IF ONE EST                      
*                                                                               
SVBILEL  DS    F                                                                
SVQSTART DS    CL6                                                              
SVQEND   DS    CL6                                                              
BQS      DS    XL3                                                              
BQE      DS    XL3                                                              
ZEROS    DS    CL30                                                             
*                                                                               
SVBLINE  DS    CL(BKEYLENQ)                                                     
SVCMADMA DS    CL(4*MAXCMA) ROOM FOR UP TO 50 UNIQUE CMA CODES PER KEY          
MAXCMA   EQU   50                                                               
*                                                                               
SVEUCOM1 DS    CL32         SAVED FIRST ESTIMATE UCOMM                          
SVEUCOM2 DS    CL32         SAVED 2ND ESTIMATE UCOMM                            
SVEUCOM3 DS    CL32         SAVED 3RD ESTIMATE UCOMM                            
SVEUCOM4 DS    CL32         SAVED 4TH ESTIMATE UCOMM                            
SVEUCOM5 DS    CL32         SAVED 5TH ESTIMATE UCOMM                            
*                                                                               
B1PROF   DS    XL16                                                             
B1XPROF  DS    XL16                                                             
TSTINVC  DS    CL10                INVOICE NUMBER FROM ZERO $ INVOICE           
SVQOPT6  DS    CL1                                                              
SVQOPT7  DS    CL1                                                              
SAVDMA   DS    CL4                                                              
CODECHAR DS    CL5                                                              
WRITESW  DS    X                   1=WRITE RECORDS TO OUTPUT FILE               
FRSTSW   DS    X                   Y=FIRST TIME THROUGH HAS HAPPENED            
         DS    F                                                                
OUTREC   DS    CL400                                                            
         DS    0D                                                               
SORTREC  DS    0XL232                                                           
SORTKEY  DS    0XL12                                                            
SORTMED  DS    CL1                                                              
SORTCLT  DS    CL3                                                              
SORTPRD  DS    CL3                                                              
SORTINV  DS    XL2              INVOICE NO                                      
SORTBDAT DS    XL3              BILLED DATE (MAY NOT NEED)                      
*                                                                               
*  NEXT SEVERAL FIELDS FROM BINSRCH INVOICE DATA                                
*                                                                               
SORTINV$ DS    PL6              FULL INVOICE $                                  
SORTINVF DS    CL10             FULL INVOICE #                                  
SORTINVD DS    CL10             DATE ON INVOICE                                 
SORTCOM4 DS    CL32             4TH ESTIMATE UCOMM                              
SORTCOM5 DS    CL4              5TH ESTIMATE UCOMM                              
*                                                                               
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
*                                                                               
SORTENAM DS    CL24             EST # AND NAME                                  
SORTPNAM DS    CL24             PRD CODE AND NAME                               
SORTCNAM DS    CL20             CLT CODE AND NAME                               
SORTMEDN DS    CL11             MEDIA NAME                                      
SORTMOS  DS    CL10             BUY BILLABLE DATE (DAY SET TO 1)                
SORTDMA  DS    CL4              PUB GROUP (SCHEME M)                            
*                               FOR MEDIA I,S,L UCOMM5                          
SORTPUB  DS    CL60             PUB NAME, ZONE, NUMBER                          
         DS    CL1              FOR ALIGNMENT                                   
SORTBNET DS    XL4              BILLED NET FROM ELEMENT                         
*                                                                               
ATTTABLE CSECT                                                                  
         DS    2000CL103                                                        
*                                                                               
       ++INCLUDE PGENGRP                                                        
       ++INCLUDE DDUCOMD                                                        
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREPAI02 10/24/16'                                      
         END                                                                    
