*          DATA SET SPWRI29    AT LEVEL 032 AS OF 04/12/10                      
*PHASE T20429A,*                                                                
*INCLUDE SPFMTINO                                                               
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI29 (T20429) - HOME DEPOT BILLING REPORT             *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 12APR10 32 AKT -- CML STORAGE MOVE TO SPWRIWORKD                  *           
* 26AUG04 31 AKT -- SBCMLCD GOT RELOCATED                           *           
* 17AUG04 31 EFJ -- USE SPFMTINO TO OUTPUT INVOICE NUMBER           *           
* 02DEC03 30 PWE -- REMOVE LVL29 & PREVENT REQUESTS FOR MEDIA N/C/* *           
* 27NOV03 29 PWE -- FIX STATION FIRST FOR CANADIAN CABLE            *           
* 03NOV03 28 AKT -- FIX MGROUP X'40' BUGS                           *           
*                                                                   *           
* 03NOV03 HISTORY LOST                                              *           
*                -- NOT REALLY LOST, JUST A CHANGE IN DIRECTION! EJ *           
*                                                                   *           
* 04JAN95 12 EFJ -- DON'T INCLUDE GST ON MKT & VENDOR RECAP         *           
* 12MAY95 13 EFJ -- FIX BUG CAUSING DUMP ON 4 CMML CLASSES          *           
* 25JUN96 14 EFJ -- BILL ADDR MAY BE ON SPECIFIC PRD REC            *           
* 20OCT97 15 EFJ -- TREAT DAYPART Q AS BONUS LIKE DAYPART H         *           
* 17JUN98 16 NRK -- Y2K COMPLIANCE                                  *           
* 10NOV98 17 EFJ -- CHANGE ADDR IF CAN AGY REQ                      *           
* 24NOV98 18 EFJ -- CHANGE ADDR IF CAN AGY REQ AGAIN                *           
* 25NOV98 19 EFJ -- NOW CHANGE ADDR FOR WESTERN                     *           
* 03MAY99 20 EFJ -- NO MGRP TOTALS IF ONLY 1 MARKET                 *           
*                -- FIX BROKEN BOXES                                *           
* 13JUN99 21 EFJ -- ALL SECTIONS NOW BY MGROUP                      *           
*                -- ALWAYS MGROUP TOTAL (UNDO L20)                  *           
* 01JUL99 22 EFJ -- MORE MGRP STUFF                                 *           
* 120CT99 23 BOB -- PRINT GST NUMBER & DIVISIONAL RECAP             *           
* 13DEC00 24 EFJ -- CHANGE WESTERN TO INITIATIVE                    *           
* 24APR01 26 EFJ -- CHANGE WESTERN TO INITIATIVE                    *           
*                                                                   *           
*********************************************************************           
T20429   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20429,RA                                                  
         LR    R8,RC                                                            
         USING WORKD,R8                                                         
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPINPUT      SPOTIO HOOK                                  
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     LA    R1,SBAGYREC         TEST CANADA                                  
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   INIT1                                                            
         LA    R2,HDBMEDH          NOT DESIGNED FOR CANADIAN NETWORK            
         CLI   8(R2),C'N'          TRAP HERE SO NOT OVERLOAD STANDARD           
         BE    EINV                     MEDIA VALIDATION ROUTINE WITH           
         CLI   8(R2),C'C'               REPORT SPECIFIC CRAP                    
         BE    EINV                                                             
         CLI   8(R2),C'*'          THEY MAY TRY THIS TOO!                       
         BE    EINV                                                             
INIT1    MVI   CPSOPT,C'Y'         PRINT AVERAGE COST/SPOT OPTION               
         LA    R2,HDBCPSH                                                       
         CLI   5(R2),0                                                          
         BE    INIT2                                                            
         MVC   CPSOPT,8(R2)                                                     
         CLI   CPSOPT,C'Y'                                                      
         BE    INIT2                                                            
         CLI   CPSOPT,C'N'                                                      
         BNE   EINV                                                             
*                                                                               
INIT2    MVI   DUBOPT,C'N'         DOUBLE SPACE MARKETS OPTION                  
         LA    R2,HDBDUBH                                                       
         CLI   5(R2),0                                                          
         BE    INIT3                                                            
         MVC   DUBOPT,8(R2)                                                     
         CLI   DUBOPT,C'Y'                                                      
         BE    INIT3                                                            
         CLI   DUBOPT,C'N'                                                      
         BNE   EINV                                                             
*                                                                               
INIT3    MVI   SUPOPT,C'N'         SUPPRESS DETAIL COSTS OPTION                 
         LA    R2,HDBSDCH                                                       
         CLI   5(R2),0                                                          
         BE    INIT4                                                            
         MVC   SUPOPT,8(R2)                                                     
         CLI   SUPOPT,C'Y'                                                      
         BE    INIT4                                                            
         CLI   SUPOPT,C'N'                                                      
         BNE   EINV                                                             
*                                                                               
INIT4    MVI   RECOPT,C'N'         MARKET RECAP OPTION                          
         LA    R2,HDBPMRH                                                       
         CLI   5(R2),0                                                          
         BE    INIT4X                                                           
         MVC   RECOPT,8(R2)                                                     
         CLI   RECOPT,C'N'                                                      
         BE    INIT4X                                                           
         CLI   RECOPT,C'Y'                                                      
         BNE   EINV                                                             
*                                                                               
INIT4X   DS    0H                                                               
*                                                                               
INITDIV  MVI   DIVOPT,C'N'         DIV PRINT OPTION                             
         LA    R2,HDBPDVH                                                       
         CLI   5(R2),0                                                          
         BE    INITDIVX                                                         
         MVC   DIVOPT,8(R2)                                                     
         CLI   DIVOPT,C'N'                                                      
         BE    INITDIVX                                                         
         CLI   DIVOPT,C'Y'                                                      
         BNE   EINV                                                             
         OI    SBEUDEF,X'10'       INDICATE UDEF E2 BEING USED                  
*                                                                               
INITDIVX DS    0H                                                               
*                                                                               
INIT5    MVI   INVOPT,C'N'         INVOICE REPORT OPTION                        
         MVI   INVRPT,0                                                         
         MVI   TOTRPT,1                                                         
         MVI   VENRPT,2                                                         
         LA    R2,HDBPIRH                                                       
         CLI   5(R2),0                                                          
         BE    INIT6                                                            
         MVC   INVOPT,8(R2)                                                     
         CLI   INVOPT,C'N'                                                      
         BE    INIT6                                                            
         CLI   INVOPT,C'Y'                                                      
         BNE   EINV                                                             
         MVI   INVRPT,1            INVOICE = 1ST REPORT                         
         MVI   TOTRPT,2            MARKET TOTAL = 2ND REPORT                    
         MVI   VENRPT,3            VENDOR = 3RD REPORT                          
         CLI   SBQMGRD,0           MARKET GROUPS REQUIRED                       
         BE    EMGRP                                                            
*                                                                               
INIT6    MVI   RECRPT,0                                                         
         CLI   RECOPT,C'Y'                                                      
         BNE   INIT6X                                                           
         MVI   RECRPT,3            RECAP = 3RD OR 4TH REPORT                    
         CLI   INVOPT,C'Y'                                                      
         BNE   INIT6X                                                           
         MVI   RECRPT,4                                                         
*                                                                               
INIT6X   DS    0H                                                               
*                                                                               
INITDVR  MVI   DIVRPT,0            ASSUME NO DIVISIONAL RECAP                   
*                                                                               
         CLI   DIVOPT,C'Y'         DONE IF NO DIVISIONAL RECAP WANTED           
         BNE   INITDVRX                                                         
*                                                                               
         LA    RF,3                ASSUME DIVISION RECAP = 3RD REPORT           
*                                                                               
         CLI   INVOPT,C'Y'         IF THERE IS INVOICE REPORT                   
         BNE   *+8                                                              
         LA    RF,4                   DIVISION RECAP = 4TH REPORT               
*                                                                               
         CLI   RECOPT,C'Y'         IF THERE IS RECAP   REPORT                   
         BNE   *+8                                                              
         LA    RF,5                   DIVISION RECAP = 5TH REPORT               
*                                                                               
         STC   RF,DIVRPT           SET DIVISION RECAP RPT NUMBER                
*                                                                               
INITDVRX DS    0H                                                               
*                                                                               
INIT7    MVI   PBLOPT,C'N'         PRE-BILL OPTION                              
         LA    R2,HDBPBLH                                                       
         CLI   5(R2),0                                                          
         BE    INIT7X                                                           
         MVC   PBLOPT,8(R2)                                                     
         CLI   PBLOPT,C'N'                                                      
         BE    INIT7X                                                           
         CLI   PBLOPT,C'Y'                                                      
         BNE   EINV                                                             
*                                                                               
INIT7X   DS    0H                                                               
*                                                                               
INITGST  MVI   GSTOPT,C'N'         GST PRINT OPTION                             
         LA    R2,HDBGSTH                                                       
         CLI   5(R2),0                                                          
         BE    INITGSTX                                                         
         MVC   GSTOPT,8(R2)                                                     
         CLI   GSTOPT,C'N'                                                      
         BE    INITGSTX                                                         
         CLI   GSTOPT,C'Y'                                                      
         BNE   EINV                                                             
*                                                                               
INITGSTX DS    0H                                                               
*                                                                               
INIT8    XC    DUEDATE,DUEDATE     UNBILLED DUE/PAYABLE DATE                    
         LA    R2,HDBDPDH                                                       
         CLI   5(R2),0                                                          
         BE    INIT9                                                            
         GOTO1 DATVAL,DMCB,8(R2),DUB                                            
         OC    0(4,R1),0(R1)                                                    
         BZ    EINV                                                             
         MVC   DUEDATE,8(R2)                                                    
*                                                                               
INIT9    XC    INVDTE,INVDTE       INVOICE DATE                                 
         LA    R2,HDBINDH                                                       
         CLI   5(R2),0                                                          
         BE    INIT10                                                           
         GOTO1 DATVAL,DMCB,8(R2),DUB                                            
         OC    0(4,R1),0(R1)                                                    
         BZ    EINV                                                             
         MVC   INVDTE,8(R2)                                                     
*                                                                               
INIT10   OI    SBQSKIP,SBQSKGL     TELL SPOTIO NOT TO READ GOALS                
         CLI   SBQBPRD,X'FF'       TRANSLATE PRD=POL TO PRD=ALL                 
         BNE   *+8                                                              
         MVI   SBQBPRD,0                                                        
         MVI   SBQSEPES,C'Y'       ENSURE EST=ALL                               
         OI    ROWIND,ROWICML      GET COMMERCIALS                              
         OI    DATAIND5,DICML                                                   
         MVI   MYFIRSTH,12         FIRST HEADING ON HEAD12                      
         MVC   TITLE,BLANKS                                                     
         MVC   TITLE(16),=C'INITIATIVE MEDIA'                                   
         GOTO1 CENTER,DMCB,TITLE,63                                             
         XC    SVKEY,SVKEY                                                      
         XC    SVSTA,SVSTA                                                      
         XC    PSTRATE,PSTRATE                                                  
*                                                                               
* I'M NOT SURE WHY THIS CODE IS HERE, BUT IT SURE MAKES SOME UGLY BOXES         
* NOP'ED 04MAY99 EJOR                                                           
*&&DO                                                                           
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   INITX                                                            
         L     R1,TWAMASTC         YES-SKIP 2 EXTRA LINES FOR FORCEHED          
         L     R1,MCBXAREA-MASTD(R1)                                            
         MVI   BOXHDSKP-BOXD(R1),2                                              
*&&                                                                             
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
* ERRORS                                                                        
*                                                                               
EMGRP    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'MARKET GROUPS REQUIRED'                           
         B     MYCURSER                                                         
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSER MVI   ERROR,X'FE'         OWN ERROR MESSAGE                            
*                                                                               
CURSOR   DS    0H                                                               
         GOTO1 CURSERR                                                          
         EJECT                                                                  
* SPOTIO INPUT HOOK                                                             
*                                                                               
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCCL     CLIENT FIRST                                 
         BE    CLIENT                                                           
         CLI   SBMODE,SBPROCST     STATION FIRST                                
         BE    STATION                                                          
         CLI   SBMODE,SBPROCSP     PROCBUY                                      
         BE    PROCBUY                                                          
         B     XIT                                                              
         EJECT                                                                  
* CLIENT FIRST                                                                  
*                                                                               
CLIENT   XC    KEY,KEY             GET PRODUCT AAA RECORD                       
         LA    R2,KEY                                                           
         USING PRDHDRD,R2                                                       
         MVC   PKEYAM,SBBAGYMD                                                  
         MVC   PKEYCLT,SBBCLT                                                   
         MVC   PKEYPRD,=C'AAA'                                                  
         MVC   ADDR1,BLANKS                                                     
         MVC   ADDR2,BLANKS                                                     
         MVC   ADDR3,BLANKS                                                     
         MVC   ADDR4,BLANKS                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'PKEY),KEYSAVE                                              
         BNE   CLIENTX                                                          
         L     R2,SBAIO2                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   ADDR1,PADDR1        SAVE BILL ADDRESS                            
         MVC   ADDR2,PADDR2                                                     
         MVC   ADDR3,PADDR3                                                     
         MVC   ADDR4,PADDR4                                                     
*                                                                               
*        READ VA PROFILE IF NEEDED                                              
*                                                                               
         CLI   GSTOPT,C'Y'         SKIP IF NOT PRINTING GST NUMBER              
         BNE   CLTGSTX                                                          
*                                                                               
         XC    GPRFPRMS,GPRFPRMS   PARAMETERS FOR GETPROF                       
*                                                                               
         MVI   GPRFPRMS,C'S'       SET SYSTEM                                   
         MVC   GPRFPRMS+2(2),=C'VA' VA - GST NUMBER PROFILE                     
         MVC   GPRFPRMS+4(2),SBQAGY AGENCY                                      
         MVC   GPRFPRMS+6(1),SBMED MEDIA                                        
         MVC   GPRFPRMS+7(3),SBCLT CLIENT                                       
*                                                                               
         GOTO1 GETPROF,DMCB,GPRFPRMS,CLTPRFVA,DATAMGR                           
*                                                                               
CLTGSTX  DS    0H                                                               
*                                                                               
CLIENTX  B     XIT                                                              
         EJECT                                                                  
* STATION FIRST                                                                 
*                                                                               
         SPACE 1                                                                
STATION  LA    R1,SBAGYREC         TEST CANADA                                  
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   XIT                                                              
         CLC   SBBSTA,SVSTA        YES-TEST STATION HAS CHANGED                 
         BE    XIT                                                              
         MVC   SVSTA,SBBSTA        YES-                                         
         LA    R2,KEY              READ STATION RECORD                          
         USING STARECD,R2                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(16),STAKEY                                              
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,SBQMED                                                   
         MVC   STAKCALL,SBSTA                                                   
         CLI   STAKCALL+4,C' '                                                  
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,SBAGY                                                    
         MVC   STAKCLT,SBCLT       TRY CLIENT SPECIFIC                          
         L     R1,AIO3                                                          
         ST    R1,AIO                                                           
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    STA2                                                             
         MVC   KEY,KEYSAVE                                                      
         MVC   STAKCLT,=C'000'     TRY NON CLIENT SPECIFIC                      
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    STA2                                                             
         DC    H'0'                                                             
*                                                                               
STA2     L     R2,AIO                                                           
         XC    PSTRATE,PSTRATE                                                  
         CLI   SPST+5,C'S'         TEST FOR PQ PST RATE                         
         BNE   XIT                                                              
         LA    R1,PSTTAB           YES-LOOKUP PST RATE                          
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PSTRATE,3(R1)       ACCEPT FIRST RATE FOUND (FOR NOW)            
         B     XIT                                                              
         EJECT                                                                  
* PROCESS A BUY RECORD                                                          
*                                                                               
         SPACE 1                                                                
PROCBUY  L     R3,SBAIO1           A(BUY RECORD)                                
         USING BUYREC,R3                                                        
         MVI   MKTIND,X'FF'                                                     
         MVC   SBEMKT,SBBMKT       EXTRACT MARKET IN DIRECTORY POINTER          
         CLC   SBBMKT,BUYMSTA      REJECT SPILL MARKETS                         
         BNE   BY20                                                             
         MVI   SBBPRD,0            INITIALIZE PRODUCTS                          
         MVI   SBBPRD2,0                                                        
         XC    SBPRD,SBPRD                                                      
         XC    SBPRD2,SBPRD2                                                    
         MVI   SVPRD,0             INIT SAVED PRODUCT                           
         MVI   SVEST,0             INIT SAVED ESTIMATE                          
*                                                                               
         GOTO1 SPOTBUY,PARAS,SBLOCK   ** CALL SPOTBUY **                        
*                                                                               
         L     R5,SBACHUNK                                                      
         USING SCHUNKD,R5                                                       
*                                                                               
BY2      OC    SCNEXT,SCNEXT       TEST END OF CHUNKS                           
         BZ    BY20                                                             
         L     RE,SCNEXT           TEST DATA PART OF CHUNK IS ZERO              
         SR    RE,R5                                                            
         SH    RE,=Y(SCDATA-SCHUNKD)                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    SCDATA(0),SCDATA                                                 
         BZ    BY10                YES-SKIP TO NEXT CHUNK                       
         CLI   SCPRD1,254          REJECT UNALLOCATED                           
         BE    BY10                                                             
         CLI   SBQBPRD,0           TEST PRD=ALL                                 
         BNE   *+16                                                             
         CLI   SCPRD1,X'FF'        YES-REJECT PRD=POL                           
         BE    BY10                                                             
         B     *+14                                                             
         CLC   SCPRD1,SBQBPRD      CHECK THE PRODUCT                            
         BNE   BY10                                                             
         CLC   SBBPRD,SCPRD1       TEST CHANGE OF PRODUCT                       
         BE    BY4                                                              
         LLC   RE,SCPRD1           YES - SET PRODUCT DETAILS                    
         STC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH        ALPHA PRODUCT                                
         MVC   SBPRDNM,PBNAME      PRODUCT NAME                                 
*                                                                               
BY4      CLC   SBBPRD,SVPRD        TEST CHANGE OF PRODUCT/ESTIMATE              
         BNE   *+14                                                             
         CLC   BUYKEST,SVEST                                                    
         BE    BY6                                                              
         MVC   SVPRD,SBBPRD        YES                                          
         MVC   SVEST,BUYKEST                                                    
         CLI   SBEFILT,C'Y'        TEST EST FILTERS REQUIRED                    
         BNE   BY6                                                              
         LLC   RE,SBBPRD           YES-FIND ENTRY IN PRD/EST TABLE              
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         LLC   RF,SBBEST                                                        
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         A     RE,SBAESTTB                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(ESTBUFFL)                                                  
         A     R1,SBAESTBF                                                      
         USING ESTBUFFD,R1                                                      
         MVC   SBESTFLT,EBFILT     EXTRACT ESTIMATE FILTERS                     
*                                                                               
BY6      MVC   SBCMLSQ,SCML1       COMMERCIAL SEQUENCE NUMBER                   
         BAS   RE,GETCMML          GET COMMERCIAL DETAILS                       
         ST    R5,SBACURCH         STORE A(CHUNK)                               
         MVI   GLMODE,GLINPUT                                                   
         MVI   GLOPTS+7,C'Y'                                                    
         OC    CLSSPLIT,CLSSPLIT   TEST COMML CLASS SPLIT                       
         BNZ   BY7                                                              
         L     RF,SCSPOTS          NO-100 PCT GOES TO PRIMARY CML CLASS         
         M     RE,=F'10000'                                                     
         ST    RF,ACTSPOTS                                                      
         BAS   RE,GODRIVER         CALL DRIVER FOR INPUT                        
         B     BY10                                                             
*                                                                               
BY7      LA    R6,CLSSPLIT         CALL DRIVER FOR INPUT FOR EACH               
         LA    R0,4                COMMERCIAL CLASS IN THE SPLIT                
*                                                                               
BY8      OC    0(6,R6),0(R6)                                                    
         BZ    BY10                                                             
         MVC   CMLCLAS,0(R6)       SET THE CLASS                                
         L     RF,SCSPOTS          MULTIPLY SPOTS BY PCT SPLIT                  
         SR    R1,R1                                                            
         ICM   R1,3,4(R6)                                                       
         MR    RE,R1                                                            
         ST    RF,ACTSPOTS                                                      
         BAS   RE,GODRIVER                                                      
         MVI   GLOPTS+7,C'N'       ONLY WRITE VENDOR AND MKT RECAP              
         LA    R6,6(R6)            RECORDS FOR THE OTHER SPLITS                 
         BCT   R0,BY8                                                           
*                                                                               
BY10     MVI   GLOPTS+7,C'Y'       RESTORE DRIVER OPTIONS                       
         L     R5,SCNEXT           NEXT CHUNK                                   
         B     BY2                                                              
*                                                                               
BY20     XC    SBEMKT,SBEMKT                                                    
*                                                                               
BYX      MVI   RPMODE,RPSKIOHK     SKIP SPWRI01'S PROCESSING                    
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                   *         
*                                                                               
         SPACE 1                                                                
GODRIVER NTR1  ,                                                                
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         EJECT                                                                  
* ROUTINE TO GET COMMERCIAL RECORD INFORMATION                                  
*                                                                               
         SPACE 1                                                                
GETCMML  NTR1  ,                                                                
         BAS   RE,SETTRF           SET SYSTEM TO TRAFFIC                        
         MVC   CMLCODE(8),=CL8'UNKNOWN'                                         
         MVC   CMLCLAS,=C'ZZZZ'                                                 
         XC    CLSSPLIT,CLSSPLIT                                                
         ICM   R4,15,SBACMLTB      'UNKNOWN' IF NO COMMERCIAL TABLE             
         BZ    GETCMMLX                                                         
         OC    SBCMLSQ,SBCMLSQ     OR CML=0                                     
         BZ    GETCMMLX                                                         
         LM    R2,R3,0(R4)         N'RECORDS SO FAR & MAX RECORDS               
         LA    R4,8(R4)                                                         
         ST    R4,DMCB+4                                                        
         LA    R4,WORK                                                          
         USING CMLTABD,R4                                                       
         MVC   CTCLT,SBBCLT                                                     
         MVC   CTSEQ,SBCMLSQ                                                    
         LA    R5,CMLTABL                                                       
         GOTO1 BINSRCH,DMCB,(0,WORK),,(R2),(R5),(0,4),(R3)                      
         L     R4,0(R1)                                                         
         CLI   0(R1),0             TEST RECORD FOUND                            
         BE    GETCMML6            YES                                          
         LA    R4,WORK             NO-BUILD TABLE ENTRY                         
         LA    R5,KEY              READ COMMERCIAL RECORD                       
         USING CMLRECD,R5                                                       
         XC    CMLKEY,CMLKEY                                                    
         MVC   CMLPID(2),=X'0AA1'                                               
         MVC   CMLPAM,SBBAGYMD                                                  
         MVC   CMLPCLT,SBBCLT                                                   
         OC    SBBCMCLT,SBBCMCLT   TEST MASTER TRAFFIC CLIENT CODE              
         BZ    *+10                                                             
         MVC   CMLPCLT,SBBCMCLT    YES-USE THAT                                 
*                                                                               
         MVC   CMLPSEQ+1(2),SBCMLSQ                                             
         GOTO1 HIGH                                                             
         CLC   CMLKEY,KEYSAVE      TEST RECORD FOUND                            
         BNE   GETCMML5                                                         
         L     R5,AIO3             YES-GET THE RECORD                           
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   CMLCODE(8),CMLKCML  COMMERCIAL CODE                              
         LA    R7,CLSSPLIT                                                      
         LA    R8,5                ALLOW MAX OF 5 CMML CLASSES                  
         LA    R5,24(R5)                                                        
         SR    R0,R0                                                            
*                                                                               
GETCMML2 CLI   0(R5),0             LOOK FOR COMMERCIAL CLASS                    
         BE    GETCMML5                                                         
         CLI   0(R5),X'10'                                                      
         BNE   GETCMML3                                                         
         USING CMLDTAEL,R5                                                      
         CLC   CMLCLASS,BLANKS     TEST CLASS DEFINED                           
         BNH   GETCMML4            NO-CLASS=ZZZZ                                
         MVC   CMLCLAS,CMLCLASS                                                 
         B     GETCMML4                                                         
*                                                                               
GETCMML3 CLI   0(R5),X'21'         COMMERCIAL CLASS ELEMENT?                    
         BNE   GETCMML4                                                         
         USING CMLCLSEL,R5                                                      
         MVC   0(4,R7),CMLCLS      YES-SAVE CLASSES AND THEIR PCT'S             
         MVC   4(2,R7),CMLCLSPC                                                 
         LA    R7,6(R7)                                                         
         BCT   R8,GETCMML4                                                      
         DC    H'0'                TOO MANY COMMERCIAL CLASSES                  
*                                                                               
GETCMML4 IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETCMML2                                                         
         DROP  R5                                                               
*                                                                               
GETCMML5 DS    0H                                                               
         MVC   CTCODE(8),CMLCODE                                                
         XC    CTNAME,CTNAME                                                    
         MVC   CTNAME(12),CLSSPLIT PUT 1ST 2 CML CLS SPLITS IN CML NAME         
         MVC   CTCLASS,CMLCLAS                                                  
         XC    CTNUM,CTNUM                                                      
         MVC   CTNUM(12),CLSSPLIT+12   PUT OTHER 2 IN CML NUM                   
         L     R4,SBACMLTB         ADD TO TABLE                                 
         LA    R4,8(R4)                                                         
         LA    R5,CMLTABL                                                       
         GOTO1 BINSRCH,DMCB,(1,WORK),(R4),(R2),(R5),(0,4),(R3)                  
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         L     R1,8(R1)                                                         
         L     RE,SBACMLTB                                                      
         ST    R1,0(RE)            UPDATE N' RECORDS SO FAR                     
         LA    R4,WORK                                                          
*                                                                               
GETCMML6 MVC   CMLCODE(8),CTCODE   COMMERCIAL CODE                              
         MVC   CMLCLAS,CTCLASS     COMMERCIAL CLASS                             
         MVC   CLSSPLIT,CTNAME     COMMERCIAL CLASS SPLITS                      
         MVC   CLSSPLIT+12(12),CTNUM                                            
*                                                                               
GETCMMLX BAS   RE,SETSPT           SET SYSTEM BACK TO SPOT                      
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINREC      PROCESS A REPORT FOR INPUT                   
         BE    INREC                                                            
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLFIRST      FIRST ROUTINES                               
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEDHK                                                            
         CLI   GLHOOK,GLOUTPUT     OUTPUT                                       
         BE    OUTPUT                                                           
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  MVC   GLOPTS+2(1),CPSOPT  PRINT AVERAGE COST/SPOT OPTION               
         MVC   GLOPTS+3(1),DUBOPT  DOUBLE SPACE MARKETS OPTION                  
         MVC   GLOPTS+4(1),RECOPT  MARKET RECAP OPTION                          
         MVC   GLOPTS+5(1),INVOPT  INVOICE REPORT OPTION                        
         MVC   GLOPTS+6(1),PBLOPT  PRE-BILL OPTION                              
         MVC   GLOPTS+8(1),GSTOPT  GST PRINT OPTION                             
         MVC   GLOPTS+9(1),DIVOPT DIVISONAL RECAP PRINT OPTION                  
         MVI   GLAUTOCH,C'N'       NO AUTO CHUNKING OF COLUMN HEADERS           
         OI    GLINDS,GLPALTOT     PRINT ALL TOTALS                             
         B     DRHOOKX                                                          
         SPACE 2                                                                
*                                                                               
OUTPUT   XC    SBBMKT,SBBMKT                                                    
         B     DRHOOKX                                                          
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'OESTIM  ',A(OESTIM)                                          
         DC    CL8'OMKT    ',A(OMKT)                                            
         DC    CL8'IINV    ',A(IINV)                                            
         DC    CL8'OINV1   ',A(OINV1)                                           
         DC    CL8'OINV2   ',A(OINV2)                                           
         DC    CL8'IGST    ',A(IGST)                                            
         DC    CL8'OGST    ',A(OGST)                                            
         DC    CL8'OGST2   ',A(OGST2)                                           
         DC    CL8'ICOMCLS ',A(ICOMCLS)                                         
         DC    CL8'OCOMCLS ',A(OCOMCLS)                                         
         DC    CL8'ICML    ',A(ICML)                                            
         DC    CL8'ISPOTS  ',A(ISPOTS)                                          
         DC    CL8'OSPOTS  ',A(OSPOTS)                                          
         DC    CL8'INBSPOTS',A(INBSPOTS)                                        
         DC    CL8'ONBSPOTS',A(ONBSPOTS)                                        
         DC    CL8'OBYCST  ',A(OBYCST)                                          
         DC    CL8'ODIVCST ',A(ODIVCST)                                         
         DC    CL8'OSPTCST ',A(OSPTCST)                                         
         DC    CL8'ODUE    ',A(ODUE)                                            
         DC    CL8'OFEE    ',A(OFEE)                                            
         DC    CL8'OBYDOL  ',A(OBYDOL)                                          
         DC    CL8'FEST    ',A(FEST)                                            
         DC    CL8'FCMLCLS ',A(FCMLCLS)                                         
         DC    CL8'CLEAR   ',A(CLEAR)                                           
         DC    CL8'FINV    ',A(FINV)                                            
         DC    CL8'FMKT1   ',A(FMKT1)                                           
         DC    CL8'FMKT2   ',A(FMKT2)                                           
         DC    CL8'LMKT    ',A(LMKT)                                            
         DC    CL8'TINV    ',A(TINV)                                            
         DC    CL8'TOTINV  ',A(TOTINV)                                          
         DC    CL8'TMKT    ',A(TMKT)                                            
         DC    CL8'TOTDIV  ',A(TOTDIV)                                          
         DC    CL8'TMGRP   ',A(TMGRP)                                           
         DC    CL8'FMGRP   ',A(FMGRP)                                           
         DC    CL8'FINV2   ',A(FINV2)                                           
         DC    CL8'IUDEF   ',A(IUDEF)                                           
         DC    CL8'OUDEF   ',A(OUDEF)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER ABOUT TO PROCESS A REPORT FOR INPUT                                    
*                                                                               
INREC    CLI   GLARGS,1            TEST THIS IS THE INVOICE REPORT              
         BNE   XIT                                                              
         CLI   INVOPT,C'Y'                                                      
         BNE   XIT                                                              
         CLI   GLOPTS+7,C'N'       IT SHOULD BE SUPPRESSED THIS TIME            
         BNE   XIT                                                              
         MVI   GLHOOK,GLDONT                                                    
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   *+8                                                              
         MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
                                                                                
OESTIM   MVC   LABLAREA(8),=C'ESTIMATE'                                         
         LLC   R1,1(R2)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CODEAREA(3),DUB                                                  
         MVC   SBBPRD,2(R2)                                                     
         GOTO1 GETESTNM                                                         
         MVC   NAMEAREA(L'SBESTNM),SBESTNM                                      
         MVC   0(L'OUTAREA,R3),OUTAREA                                          
         B     XIT                                                              
                                                                                
                                                                                
OMKT     CLC   SBBMKT,0(R2)                                                     
         BE    OMKT2                                                            
         MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0                                          
         GOTO1 GETMKTNM                                                         
*                                                                               
OMKT2    MVC   0(4,R3),SBMKT                                                    
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         LLC   RE,DROLEN                                                        
         DROP  R1                                                               
         SHI   RE,6                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R3),SBMKTNM                                                  
         LA    RF,SBMKTNM+1(RE)                                                 
         LA    R1,L'SBMKTNM-2                                                   
         SR    R1,RE                                                            
         BM    XIT                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BLANKS                                                   
         BNH   XIT                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   198(0,R3),0(RF)                                                  
         B     XIT                                                              
                                                                                
                                                                                
IINV     MVC   THISKEY(1),SBBAGYMD INVOICE INPUT                                
         MVC   THISKEY+1(2),SBBCLT                                              
         MVC   THISKEY+3(1),SBBPRD                                              
         MVC   THISKEY+4(1),SBBEST                                              
         MVC   THISKEY+5(2),SBBMKT                                              
         CLC   SVKEY(5),THISKEY    TEST NEW CLT/PRD/EST                         
         BE    IINV5                                                            
         XC    KEY,KEY             YES-GET INVOICE NUM FROM BILL HDR            
         LA    R5,KEY                                                           
         USING BILLRECD,R5                                                      
         MVC   BKEYAM,SBBAGYMD                                                  
         MVC   BKEYCLT,SBBCLT                                                   
         MVC   BKEYPRD,SBPRD                                                    
         MVC   BKEYEST,SBBEST                                                   
******** MVC   BKEYYSRV,SBBQEND                                                 
******** MVC   BKEYMSRV,SBBQEND+1                                               
*Y2K         PACK  DUB,SBQREQND(2)                                              
*Y2K         CVB   R1,DUB                                                       
*Y2K         STC   R1,BKEYYSRV                                                  
*Y2K         PACK  DUB,SBQREQND+2(2)                                            
*Y2K         CVB   R1,DUB                                                       
*Y2K         STC   R1,BKEYMSRV                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(0,SBQREQND),(3,FULL) CNVT TO YMD BINARY             
         MVC   BKEYYSRV(2),FULL    BKEYYSRV AND BKEYMSRV                        
*                                                                               
         MVC   BHINV,=C'999999'                                                 
         GOTO1 HIGH                                                             
         B     IINV4                                                            
*                                                                               
IINV2    LA    R5,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
IINV4    CLC   KEY(BKEYMBIL-BKEY),KEYSAVE                                       
         BNE   IINV5                                                            
         OC    BKEYYSRV(5),BKEYYSRV  SKIP ESTIMATE RECORD                       
         BZ    IINV2                                                            
         L     R5,AIO3             GET THE RECORD                               
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         TM    BILSTAT,BSTTAORQ    SKIP TRUE AOR BILLS (CREDIT INVOICE          
         BO    IINV2               FOR AOR COMMISSION REBATE)                   
*&&DO                                                                           
         LLC   RE,BKEYMBIL                                                      
         LR    RF,RE                                                            
         SRL   RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BHINV(1),DUB                                                     
         SLL   RF,28                                                            
         SRL   RF,28                                                            
         CVD   RF,DUB                                                           
         SHI   RF,9                                                             
         BNP   *+16                                                             
         LA    RF,X'C0'(RF)                                                     
         STC   RF,BHINV+1                                                       
         B     *+14                                                             
         OI    DUB+7,X'0F'                                                      
         UNPK  BHINV+1(1),DUB      BILL MONTH                                   
         SR    RE,RE                                                            
         ICM   RE,3,BKEYINV                                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BHINV+2(4),DUB      INVOICE NUMBER                               
*&&                                                                             
         LR    R6,R9                                                            
         AHI   R6,4096                                                          
         USING SYSD+4096,R6                                                     
         GOTO1 =V(SPFMTINO),DMCB,BDATE,(X'02',BKEYINV),                X        
               (SBMED,SBB1PROF),SBB1XPRF                                        
         DROP  R6                                                               
         L     RF,DMCB+4                                                        
         MVC   BHINV(2),0(RF)      NEED TO SKIP '-' IN FORMAT                   
         MVC   BHINV+2(4),3(RF)                                                 
         B     IINV2               KEEP READING TO GET THE LATEST               
*                                                                               
IINV5    CLC   SVKEY,THISKEY       TEST NEW CLT/PRD/EST/MKT                     
         BE    IINV6                                                            
         LA    R5,KEY              YES-SEE IF BILLING EXISTS FOR MKT            
         XC    KEY,KEY                                                          
         USING STABUCKD,R5                                                      
         MVC   STABKCOD,=X'0E01'                                                
         MVC   STABKAM,SBBAGYMD                                                 
         MVC   STABKCLT,SBBCLT                                                  
         MVC   STABKPRD,SBBPRD                                                  
         MVC   STABKEST,SBBEST                                                  
         MVC   STABKMKT,SBBMKT                                                  
         MVC   INVOICE,BHINV       SET INVOICE FROM BILL HEADER                 
         GOTO1 HIGH                                                             
         CLC   KEY(STABKSTA-STABUCK),KEYSAVE   TEST ANY BILLS                   
         BE    IINV6                                                            
         MVC   INVOICE,=C'999999'  NO-SET INVOICE TO 'UNKNOWN'                  
*                                                                               
IINV6    MVC   0(6,R2),BHINV                                                    
         CLI   GLARGS,1                                                         
         BNE   IINV7                                                            
         MVC   0(6,R2),INVOICE                                                  
*                                                                               
IINV7    MVC   SVKEY,THISKEY                                                    
         B     XIT                                                              
                                                                                
                                                                                
OINV1    MVC   OUTAREA,BLANKS      INVOICE OUTPUT                               
         CLC   0(6,R2),=C'999999'  DON'T PRINT IF UNKNOWN                       
         BE    OINV12                                                           
         MVC   LABLAREA(7),=C'INVOICE'                                          
         LA    RF,NAMEAREA                                                      
*                                                                               
         LA    R1,SBAGYREC         TEST CANADA                                  
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   *+12                                                             
         MVI   0(RF),C'C'          PROCEED MEDIA BY C                           
         LA    RF,1(RF)                                                         
*                                                                               
         MVC   0(1,RF),SBQMED                                                   
         MVI   1(RF),C'-'                                                       
         MVC   2(6,RF),0(R2)                                                    
OINV12   MVC   0(L'OUTAREA,R3),OUTAREA                                          
         B     XIT                                                              
                                                                                
                                                                                
OINV2    MVC   INVOICE,0(R2)       INVOICE FOR INVOICE REPORT                   
         B     XIT                                                              
                                                                                
*                                                                               
*        EXTRACT UDEF E2                                                        
*                                                                               
IUDEF    DS    0H                                                               
*                                                                               
         LA    RE,SBLOCK           ESTABLISH SPOTBLOCK                          
         USING SBLOCK,RE                                                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,SBUE2LEN       GET UDEF E2 LENGTH                           
         BZ    *+10                IN CASE THERE IS NONE                        
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SBUE2FLD    PASS UDEF E2 TO DRIVER                       
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  RE                                                               
*                                                                               
*        SAVE UDEF E2 FIELD FOR HEADHK                                          
*                                                                               
OUDEF    DS    0H                                                               
         MVC   UDEFE2,0(R2)        SAVE UDEF E2                                 
         B     XIT                                                              
*                                                                               
                                                                                
*        GST NUMBER INPUT                                                       
*                                                                               
IGST     DS    0H                                                               
         MVC   0(10,R2),CLTPRFVA   PASS GST NUMBER                              
IGSTX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
*        GST NUMBER OUTPUT                                                      
*                                                                               
OGST     DS    0H                                                               
*                                                                               
         LTR   R3,R3               DROP IF NOT PRINTING                         
         BZ    OGSTX                                                            
*                                                                               
         CLC   0(10,R2),BLANKS     DROP IF NO GST# AVAILABLE                    
         BNH   OGSTX                                                            
*                                                                               
         MVC   OUTAREA,BLANKS      INIT PRINT AREA                              
*                                                                               
         MVC   LABLAREA(4),=CL4'GST#'                                           
         MVC   NAMEAREA(20),=CL20'R123456789 (7.00%)'                           
         MVC   NAMEAREA(10),0(R2)      ADD IN GST#                              
*                                                                               
         MVC   0(L'OUTAREA,R3),OUTAREA MOVE TO OUTPUT                           
*                                                                               
OGSTX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
*        GST NUMBER FOR INVOICE REPORT                                          
*                                                                               
OGST2    DS    0H                                                               
*                                                                               
         MVC   GST#,0(R2)          SAVE GST NUMBER FOR HEDHK                    
*                                                                               
OGST2X   DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
ICML     MVC   0(8,R2),CMLCODE     COMMERCIAL CODE INPUT                        
         B     XIT                                                              
*                                                                               
ICOMCLS  MVC   0(4,R2),CMLCLAS     COMMERCIAL CLASS INPUT                       
         OC    0(4,R2),BLANKS                                                   
         B     XIT                                                              
*                                                                               
OCOMCLS  MVC   COMCLASS,0(R2)      COMMERCIAL CLASS OUTPUT                      
         MVC   CLASSNM,BLANKS                                                   
         BAS   RE,GETCLS                                                        
         BE    *+10                                                             
         MVC   CLASSNM(9),=C'*UNKNOWN*'                                         
         CLI   GLARGS,C'H'         GETS PRINTED LATER IF IN HEADLINES           
         BE    XIT                                                              
         MVC   0(4,R3),COMCLASS                                                 
         MVC   5(17,R3),CLASSNM                                                 
         B     XIT                                                              
*                                                                               
INBSPOTS XC    0(4,R2),0(R2)       NON-BONUS SPOTS                              
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         L     R5,SBAIO1                                                        
         USING BUYRECD,R5                                                       
         CLI   BDDAYPT,C'H'        DAYPART H = BONUS                            
         BE    XIT                                                              
         CLI   BDDAYPT,C'Q'        DAYPART Q = BONUS                            
         BE    XIT                                                              
         L     R6,SBACURCH                                                      
         USING SCHUNKD,R6                                                       
         MVC   0(4,R2),SCSPOTS                                                  
         B     XIT                                                              
                                                                                
                                                                                
ONBSPOTS MVC   NSPOTS,0(R2)        SAVE N'BONUS SPOTS                           
         B     XIT                                                              
                                                                                
                                                                                
ISPOTS   XC    0(8,R2),0(R2)                                                    
         CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         LR    R3,R2                                                            
         L     R5,SBAIO1                                                        
         USING BUYRECD,R5                                                       
         CLI   BDDAYPT,C'H'        TEST BONUS SPOT                              
         BE    *+12                                                             
         CLI   BDDAYPT,C'Q'        TEST BONUS SPOT                              
         BNE   *+8                                                              
         LA    R3,4(R3)            YES-GOES IN 2ND POSITION                     
         L     R6,SBACURCH                                                      
         USING SCHUNKD,R6                                                       
         MVC   0(4,R3),SCSPOTS                                                  
         CLI   GLARGS,2            TEST NEED FRACTIONAL SPOTS                   
         BNE   XIT                                                              
         CLI   BDDAYPT,C'H'                                                     
         BE    XIT                                                              
         CLI   BDDAYPT,C'Q'                                                     
         BE    XIT                                                              
         MVC   8(4,R2),ACTSPOTS    YES                                          
         B     XIT                                                              
                                                                                
                                                                                
OSPOTS   L     R5,0(R2)            SAVE N'NON-BONUS SPOTS                       
         ST    R5,NSPOTS                                                        
         CLI   GLARGS,2            TEST FRACTIONAL SPOTS NEEDED                 
         BNE   OSPOTS2                                                          
         MVC   NSPOTS,8(R2)        YES-SAVE THEM                                
         TM    GLINDS,GLTOTLIN     TEST TOTAL LINE FOR MKT RECAP RPT            
         BZ    OSPOTS2                                                          
         CLC   GLRECNO,RECRPT                                                   
         BNE   OSPOTS2                                                          
         L     RF,8(R2)            YES-PRINT REAL SPOT TOTAL = SUM OF           
         SR    RE,RE                   ALL FRACTIONAL SPOTS                     
         SLDA  RE,1                                                             
         D     RE,=F'10000'                                                     
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         LR    R5,RF                                                            
*                                                                               
OSPOTS2  MVC   MGRPSPT,NSPOTS                                                   
         MVC   INVSPT,NSPOTS                                                    
         CLI   GLARGS+1,13                                                      
         BNE   OSPOTS4                                                          
         EDIT  (R5),(13,(R3))                                                   
         B     OSPOTS6                                                          
*                                                                               
OSPOTS4  DS    0H                                                               
         EDIT  (R5),(7,(R3))                                                    
*                                                                               
OSPOTS6  MVI   BONUSSW,C'N'                                                     
         ICM   R5,15,4(R2)                                                      
         BZ    XIT                                                              
         MVI   BONUSSW,C'Y'                                                     
         LA    R3,198(R3)                                                       
         CLI   GLARGS+1,13                                                      
         BNE   OSPOTS8                                                          
         EDIT  (R5),(13,(R3))                                                   
         MVC   3(5,R3),=C'BONUS'                                                
         B     XIT                                                              
*                                                                               
OSPOTS8  DS    0H                                                               
         EDIT  (R5),(7,(R3))                                                    
         SHI   R3,7                                                             
         MVC   0(5,R3),=C'BONUS'                                                
         B     XIT                                                              
                                                                                
                                                                                
                                                                                
OSPTCST  TM    GLINDS,GLTOTLIN     TEST TOTAL LINE                              
         BZ    OSPTCST1                                                         
*                                                                               
         CLC   GLRECNO,RECRPT      IS THIS THE MKT RECAP REPORT?                
         BE    *+14                 YES                                         
         CLC   GLRECNO,VENRPT      IS THIS THE VENDOR REPORT?                   
         BNE   OSPTCSTB             NO                                          
         CLC   GLLEVEL,MIDLEV      IS THIS MGRP LEVEL                           
         BNE   OSPTCSTA             NO                                          
         SR    R6,R6                                                            
         ICM   R1,15,MGRPSPT       USE MGRP SPOTS/COST TO GET AVG               
         BZ    OSPTCST4                                                         
         L     RF,MGRPDUE                                                       
         B     OSPTCSTC                                                         
*                                                                               
OSPTCSTA CLC   GLRECNO,VENRPT      VENDOR REPT?                                 
         BNE   OSPTCSTB             NO                                          
         CLC   GLLEVEL,LSTHEDLV    IS THIS INVOICE LEVEL?                       
         BNE   OSPTCSTB             NO                                          
         SR    R6,R6                                                            
         ICM   R1,15,INVSPT        USE INV SPOTS/COST TO GET AVG                
         BZ    OSPTCST4                                                         
         L     RF,INVDUE                                                        
         B     OSPTCSTC                                                         
*                                                                               
OSPTCSTB CLC   GLRECNO,RECRPT      YES-ALREADY HAVE COST/SPOT FOR               
         BE    *+14                    MARKET RECAP                             
         CLC   GLLEVEL,MKTLEV      TEST MARKET TOTAL                            
         BNE   *+12                                                             
         L     R6,AVGCOST          YES-ALREADY HAVE COST PER SPOT               
         B     OSPTCST4                                                         
*                                                                               
* CML CLASSS TOTAL-GET AVERAGE COST                                             
* PER SPOT BY DIVIDING TOTAL DUE BY                                             
* N'SPOTS                                                                       
         ICM   R1,15,NSPOTS                                                     
         BZ    OSPTCST4                                                         
         L     RF,TOTDUE                                                        
OSPTCSTC SR    R6,R6                                                            
         SR    RE,RE                                                            
         M     RE,=F'20000'                                                     
         DR    RE,R1                                                            
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         LR    R6,RF                                                            
         B     OSPTCST4                                                         
*                                                                               
OSPTCST1 LA    R5,MKTTAB           AVERAGE COST PER SPOT                        
         LA    R0,MAXMKT                                                        
         SR    R6,R6                                                            
*                                                                               
OSPTCST2 OC    0(6,R5),0(R5)       LOOK UP MARKET IN MARKET TABLE               
         BZ    OSPTCST3                                                         
         CLC   SBBMKT,0(R5)                                                     
         BE    *+16                                                             
         LA    R5,6(R5)                                                         
         BCT   R0,OSPTCST2                                                      
         B     OSPTCST3                                                         
         ICM   R6,15,2(R5)         FOUND-EXTRACT THE AVERAGE COST               
OSPTCST3 ST    R6,AVGCOST                                                       
*                                                                               
OSPTCST4 LTR   R3,R3               TEST WHETHER TO PRINT                        
         BZ    XIT                                                              
         EDIT  (R6),(17,1(R3)),2,MINUS=YES     YES                              
         CLI   BONUSSW,C'Y'                                                     
         BNE   XIT                                                              
         MVC   198+13(4,R3),=C'0.00'                                            
         B     XIT                                                              
                                                                                
                                                                                
ODUE     TM    GLINDS,GLTOTLIN     TEST TOTAL LINE                              
         BZ    ODUE2                                                            
         CLC   GLLEVEL,MKTLEV      YES-TEST NOT MARKET LEVEL                    
         BE    ODUE2                                                            
         CLC   GLLEVEL,MIDLEV      TEST MGRP LEVEL                              
         BNE   *+12                                                             
         L     R7,MGRPDUE          ALREADY HAVE MGRP TOTAL                      
         B     ODUE3                                                            
*                                                                               
         CLC   GLRECNO,VENRPT      ONLY FOR VENDOR REPORT                       
         BNE   *+22                                                             
         CLC   GLLEVEL,LSTHEDLV    TEST INV LEVEL                               
         BNE   *+12                                                             
         L     R7,INVDUE           ALREADY HAVE INV TOTAL                       
         B     ODUE3                                                            
*                                                                               
         L     R7,TOTDUE           YES-THEN ALREADY HAVE TOTAL DUE              
         B     ODUE3                                                            
*                                                                               
ODUE2    L     R7,AVGCOST          AMOUNT DUE = AVERAGE COST PER SPOT           
         M     R6,NSPOTS                        * N'SPOTS                       
         SLDA  R6,1                DIVIDE BY 10000 (SPOTS ARE IN                
         D     R6,=F'10000'        SPOTS*10000)                                 
         AHI   R7,1                                                             
         SRA   R7,1                                                             
         CLC   GLRECNO,RECRPT      TEST NOT RECAP REPORT                        
         BE    ODUE3                                                            
         TM    GLINDS,GLTOTLIN     AND IT'S A DETAIL LINE                       
         BO    ODUE3                                                            
         CLI   SUPOPT,C'Y'         YES-TEST TO SUPPRESS DETAIL COSTS            
         BE    ODUE6                                                            
*                                                                               
ODUE3    LR    R1,R7                                                            
         BAS   RE,EDDUE                                                         
         CLI   BONUSSW,C'Y'                                                     
         BNE   ODUE6                                                            
         LA    R3,198(R3)                                                       
         CLI   GLARGS,13                                                        
         BNE   *+14                                                             
         MVC   9(3,R3),=C'.00'                                                  
         B     ODUE6                                                            
         MVC   14(3,R3),=C'.00'                                                 
*                                                                               
ODUE6    TM    GLINDS,GLTOTLIN     TEST MARKET TOTAL LINE                       
         BZ    ODUE7                                                            
         CLC   GLLEVEL,MKTLEV                                                   
         BNE   ODUE8                                                            
         A     R7,TOTDUE           YES-                                         
         ST    R7,TOTDUE           ACCUMULATE TOTAL DUE FOR INVOICE             
         B     XIT                                                              
*                                                                               
ODUE7    LR    RF,R7                                                            
         A     RF,MGRPDUE          ACCUM TOTAL FOR MGRP                         
         ST    RF,MGRPDUE                                                       
         LR    RF,R7                                                            
         A     RF,INVDUE           ACCUM TOTAL FOR INVOICE                      
         ST    RF,INVDUE                                                        
*                                                                               
ODUE8    B     XIT                 CODE DISABLED (EFJ - 04JAN95)                
*&&UK                                                                           
         LA    R1,SBAGYREC         INVOICE TOTAL - FOR CANADA,                  
         CLI   AGYPROF+7-AGYHDR(R1),C'C'  PRINT GST AND TOTAL WITH GST          
         BNE   XIT                                                              
         LR    R1,R7                                                            
         BAS   RE,GSTCALC                                                       
         LA    R3,198+198-17(R3)                                                
         MVC   0(3,R3),=C'GST'                                                  
         LA    R3,17(R3)                                                        
         BAS   RE,EDDUE                                                         
         AR    R1,R7                                                            
         LA    R3,198+198-17(R3)                                                
         MVC   0(16,R3),=C'TOTAL AMOUNT DUE'                                    
         LA    R3,17(R3)                                                        
         BAS   RE,EDDUE                                                         
         B     XIT                                                              
*&&                                                                             
*                                                                               
EDDUE    CLI   GLARGS,13                                                        
         BNE   EDDUE2                                                           
         EDIT  (R1),(13,(R3)),2,MINUS=YES                                       
         BR    RE                                                               
EDDUE2   DS    0H                                                               
         EDIT  (R1),(17,1(R3)),2,MINUS=YES                                      
         BR    RE                                                               
                                                                                
                                                                                
OBYCST   L     RF,0(R2)            CALCULATE AVERAGE COST PER SPOT              
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
*                                                                               
         OC    NSPOTS,NSPOTS       CHECK FOR ZERO DIVISOR                       
         BNZ   *+10                                                             
         SR    RF,RF               ZERO CAST PER SPOT                           
         B     OBYCST1                                                          
*                                                                               
         D     RE,NSPOTS                                                        
         LTR   RF,RF                                                            
         BM    *+8                                                              
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
OBYCST1  L     R5,ACURMKT          R5=A(CURRENT POSITION IN MKT TABLE)          
         MVC   0(2,R5),SBBMKT      MARKET NUMBER                                
         STCM  RF,15,2(R5)         STORE COST/SPOT IN MKT TABLE                 
         LA    R5,6(R5)                                                         
         ST    R5,ACURMKT                                                       
         C     R5,=A(MKTTABX)                                                   
         BL    XIT                                                              
         DC    H'0'                INCREASE MAXMKT                              
                                                                                
*                                                                               
*        PRINT DIVISION RECAP COSTS                                             
*              CHECK FOR BONUS SPOTS AND THEN LET DRIVER TAKE OVER              
*                                                                               
ODIVCST  DS    0H                                                               
*                                                                               
         CLI   BONUSSW,C'Y'        IF THERE ARE BONUS SPOTS                     
         BNE   *+10                                                             
         MVC   198+12(4,R3),=C'0.00'   PRINT COST                               
*                                                                               
         MVI   GLHOOK,GLEDIT       LET DRIVER PRINT COST                        
*                                                                               
ODIVCSTX DS    0H                                                               
         B     XIT                                                              
                                                                                
*                                  NET & FEE                                    
OFEE     CLI   PBLOPT,C'Y'         UNLESS REPORTING PRE-BILLED,                 
         BE    OFEE2                                                            
         OC    0(4,R2),0(R2)       TEST ANY BILLED DOLLARS                      
         BZ    OFEE2                                                            
         MVI   ANYBILL,C'Y'        YES                                          
*                                                                               
OFEE2    TM    GLINDS,GLTOTLIN     SUPPRESS PRINTING DETAIL LEVEL               
         BZ    XIT                                                              
         CLI   GLARGS,1            TEST FIRST 'NET & FEE' COLUMN                
         BNE   *+16                                                             
         CLI   GLLEVEL,7           YES-SUPPRESS INVOICE TOTAL                   
         BE    XIT                                                              
         B     OFEE4                                                            
         CLI   GLLEVEL,7           SECOND - ONLY PRINT INVOICE TOTAL            
         BNE   XIT                                                              
         LA    R1,SBAGYREC         FOR CANADA, WE'LL EDIT SO THAT               
         CLI   AGYPROF+7-AGYHDR(R1),C'C'  WE CAN INCLUDE GST AND                
         BNE   OFEE4                      THE TOTAL WITH GST                    
         L     R1,0(R2)                                                         
         BAS   RE,EDFEE                                                         
         LR    R5,R1                                                            
         BAS   RE,GSTCALC                                                       
         LA    R3,198+198-17(R3)                                                
         MVC   0(3,R3),=C'GST'                                                  
         LA    R3,17(R3)                                                        
         BAS   RE,EDFEE                                                         
         AR    R1,R5                                                            
         LA    R3,198+198-17(R3)                                                
         MVC   0(16,R3),=C'TOTAL AMOUNT DUE'                                    
         LA    R3,17(R3)                                                        
         BAS   RE,EDFEE                                                         
         B     XIT                                                              
*                                                                               
OFEE4    MVI   GLHOOK,GLEDIT       LET DRIVER EDIT                              
         B     XIT                                                              
*                                                                               
EDFEE    DS    0H                                                               
         EDIT  (R1),(13,0(R3)),2,MINUS=YES                                      
         BR    RE                                                               
                                                                                
*                                  BUY DOLLARS                                  
OBYDOL   OC    0(4,R2),0(R2)       TEST ANY BOUGHT                              
         BZ    XIT                                                              
         MVI   ANYBUY,C'Y'         YES                                          
         B     XIT                                                              
                                                                                
                                                                                
FEST     XC    MKTTAB,MKTTAB       EST FRST - CLEAR 1ST ENTRY OF MKTTAB         
         LA    R1,MKTTAB                                                        
         ST    R1,ACURMKT          POINT TO BEGINNING OF TABLE                  
         B     XIT                                                              
                                                                                
                                                                                
FCMLCLS  XC    TOTDUE,TOTDUE       COMMERCIAL CLASS FIRST -                     
         B     XIT                 CLEAR TOTAL DUE                              
*                                                                               
FMGRP    XC    MGRPDUE,MGRPDUE                                                  
         XC    MGRPSPT,MGRPSPT                                                  
         B     XIT                                                              
*                                                                               
FINV2    XC    INVDUE,INVDUE                                                    
         XC    INVSPT,INVSPT                                                    
         B     XIT                                                              
*                                                                               
CLEAR    XC    TOTDUE,TOTDUE       CLEAR TOTAL DUE                              
         B     XIT                                                              
                                                                                
                                                                                
FINV     L     RE,=A(MKTNMTAB)     INVOICE FIRST -                              
         LA    RF,L'MKTNMTAB       CLEAR MARKET NAMES TABLE                     
         XCEFL ,                                                                
         B     XIT                                                              
*                                                                               
FMKT1    XC    NSPOTS,NSPOTS       MARKET FIRST - CLEAR SPOTS AND COST          
         XC    AVGCOST,AVGCOST                                                  
         B     XIT                                                              
*                                                                               
FMKT2    MVI   ANYBILL,C'N'        MKT FIRST FOR INVOICE REPORT                 
         MVI   ANYBUY,C'N'         NO BUYS OR BILLS YET                         
         B     XIT                                                              
*                                                                               
LMKT     CLI   ANYBUY,C'Y'         MARKET LAST                                  
         BNE   XIT                                                              
         CLI   PBLOPT,C'Y'         EXCEPT FOR PRE-BILL,                         
         BE    XIT                                                              
         CLI   ANYBILL,C'Y'        TEST BUYS BUT NO BILLS                       
         BE    XIT                                                              
         L     R1,=A(MKTNMTAB)     YES-ADD MARKET NAME TO TABLE                 
         LA    R0,NMKTNMS                                                       
*                                                                               
LMKT2    OC    0(24,R1),0(R1)                                                   
         BNZ   *+14                                                             
         MVC   0(24,R1),SBMKTNM                                                 
         B     XIT                                                              
         CLC   SBMKTNM,0(R1)                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,24(R1)                                                        
         BCT   R0,LMKT2                                                         
         DC    H'0'                                                             
                                                                                
*                                  INVOICE TOTAL FOR INVOICE REPORT             
TINV     MVC   0(24,R3),=C'** INVOICE AMOUNT DUE **'                            
         XC    CMTLEN,CMTLEN                                                    
         L     R1,=A(MKTNMTAB)                                                  
         OC    0(24,R1),0(R1)      TEST ANY MKTS BOUGHT NOT BILLED              
         BNZ   *+12                                                             
         LA    RE,198(R3)                                                       
         B     TINV6                                                            
         LA    RF,NMKTNMS          YES-PRINT THEM                               
         L     R5,=A(WORKAREA)                                                  
         MVC   0(18,R5),=C'NOTE: BILLING FOR '                                  
         MVC   18(24,R5),0(R1)                                                  
         LA    R1,24(R1)                                                        
         LA    R5,42(R5)                                                        
*                                                                               
TINV2    MVC   0(5,R5),=C' AND '                                                
         BCT   RF,*+8                                                           
         B     TINV4                                                            
         OC    0(24,R1),0(R1)                                                   
         BZ    TINV4                                                            
         MVC   5(24,R5),0(R1)                                                   
         LA    R1,24(R1)                                                        
         LA    R5,29(R5)                                                        
         B     TINV2                                                            
*                                                                               
TINV4    SHI   R5,4                                                             
         MVC   0(9,R5),=C'TO FOLLOW'                                            
         LA    R5,9(R5)                                                         
         L     R2,=A(WORKAREA)                                                  
         SR    R5,R2                                                            
         GOTO1 SQUASHER,DMCB,(R2),(R5)                                          
         L     RE,4(R1)                                                         
         ST    RE,CMTLEN                                                        
         ST    RE,DMCB+16          TOTAL LENGTH                                 
         LA    R0,4                                                             
         LA    R3,198(R3)                                                       
         MVI   0(R3),0                                                          
         BCT   R0,*-8                                                           
         GOTO1 CHOPPER,DMCB,(R2),(80,(R3)),(198,10),C'LEN='                     
         ICM   RE,15,8(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MHI   RE,198                                                           
         LA    RE,0(RE,R3)                                                      
*                                                                               
TINV6    MVI   0(RE),0                                                          
         LA    RE,198(RE)                                                       
         MVC   0(28,RE),=C'DUE AND PAYABLE ON OR BEFORE'                        
         OC    DUEDATE,DUEDATE                                                  
         BZ    XIT                                                              
         MVC   29(8,RE),DUEDATE                                                 
         B     XIT                                                              
                                                                                
                                                                                
TOTINV   MVC   0(5,R3),=C'*ALL*'   INVOICE TOTAL FOR VENDOR AND RECAP           
         CLI   INVOPT,C'Y'         REPORTS.                                     
         BNE   XIT                                                              
         ICM   RE,15,CMTLEN        IF THE INVOICE REPORT PRINTED AND            
         BZ    XIT                 THERE WAS A COMMENT,                         
         CLI   GLARGS,1            PRINT IT HERE ALSO                           
         BNE   TOTINV2                                                          
         MVI   198(R3),0                                                        
         MVI   198+198(R3),0                                                    
         LA    R3,198+198+198(R3)                                               
*                                                                               
TOTINV2  ST    RE,DMCB+16          TOTAL COMMENT LENGTH                         
         L     R2,=A(WORKAREA)                                                  
         GOTO1 CHOPPER,DMCB,(R2),(80,(R3)),(198,10),C'LEN='                     
         OC    8(4,R1),8(R1)                                                    
         BNZ   XIT                                                              
         DC    H'0'                                                             
                                                                                
                                                                                
TMKT     MVC   0(29,R3),BLANKS                                                  
         MVC   0(12,R3),=C'MARKET TOTAL'                                        
         B     XIT                                                              
*                                                                               
TMGRP    MVC   0(29,R3),BLANKS                                                  
         MVC   0(18,R3),=C'MARKET GROUP TOTAL'                                  
         B     XIT                                                              
*                                                                               
TOTDIV   CLC   GLRECNO,DIVRPT      IF DIVISION REPORT                           
         BNE   TOTDIVX                                                          
         MVC   0(20,R3),=C'TOTALS FOR DIVISION '                                
         MVC   20(16,R3),UDEFE2                                                 
TOTDIVX  DS    0H                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* MISC ROUTINES                                                                 
*                                                                               
         SPACE 1                                                                
GSTCALC  M     R0,=F'14'           COMPUTE GST AMOUNT                           
         D     R0,=F'100'          IN: R1=DOLLAR AMOUNT                         
         LTR   R1,R1               OUT: R1=GST (=7%)                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO READ A COMMERCIAL CLASS RECORD                                     
* CONDITION CODE RETURNED: EQ - CLASS NAME RETURNED IN CLASSNM                  
*                          NE - NAME NOT FOUND                                  
                                                                                
GETCLS   LR    R0,RE                                                            
         BAS   RE,SETTRF                                                        
         CLC   COMCLASS,BLANKS                                                  
         BNH   GETCLSN                                                          
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CLSRECD,R2                                                       
         MVC   CLSKID,=X'0A44'                                                  
         MVC   CLSKAM,SBBAGYMD                                                  
         MVC   CLSKCLAS,COMCLASS                                                
         OC    CLSKCLAS,BLANKS                                                  
         MVC   CLSKCLT,SBBCLT                                                   
         MVC   CLSKPROD,SBPRD      TRY PRODUCT SPECIFIC                         
*                                                                               
GETCLS2  DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'CLSKEY),KEYSAVE                                            
         BE    GETCLS4                                                          
         MVC   KEY,KEYSAVE         NOT FOUND - TRY CLIENT LEVEL                 
         OC    CLSKPROD,CLSKPROD                                                
         BZ    GETCLSN                                                          
         XC    CLSKPROD,CLSKPROD                                                
         B     GETCLS2                                                          
*                                                                               
GETCLS4  L     R2,SBAIO2                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R1,24(R2)                                                        
         SR    RF,RF                                                            
*                                                                               
GETCLS6  CLI   0(R1),0                                                          
         BE    GETCLSN                                                          
         CLI   0(R1),X'10'                                                      
         BNE   GETCLS8                                                          
         USING CLSDSCEL,R1                                                      
         MVC   CLASSNM,CLSDESC                                                  
         B     GETCLSY                                                          
         DROP  R1                                                               
*                                                                               
GETCLS8  IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GETCLS6                                                          
*                                                                               
GETCLSN  BAS   RE,SETSPT                                                        
         LTR   RE,R0                                                            
         BR    RE                                                               
*                                                                               
GETCLSY  BAS   RE,SETSPT                                                        
         LR    RE,R0                                                            
         CR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SETTRF   MVC   SYSDIR,=CL8'TRFDIR'                                              
         MVC   SYSFIL,=CL8'TRFFILE'                                             
         BR    RE                                                               
*                                                                               
SETSPT   MVC   SYSDIR,=CL8'SPTDIR'                                              
         MVC   SYSFIL,=CL8'SPTFILE'                                             
         BR    RE                                                               
         EJECT                                                                  
* DRIVER'S FIRST HOOK                                                           
*                                                                               
FIRSTS   CLI   GLARGS,0            TEST FIRST TIME FOR THIS REPORT              
         BNE   XIT                                                              
         MVI   GLLFTOPT,C'N'                                                    
         CLC   GLRECNO,INVRPT      TEST INVOICE REPORT                          
         BNE   *+8                                                              
         MVI   GLLFTOPT,C'Y'       YES-SET LEFT OPTION                          
         NI    GLINDS,255-GLPALTOT                                              
         CLC   GLRECNO,INVRPT      TEST INVOICE REPORT                          
         BE    FIRSTS0             YES-MAKE SURE TOTALS PRINT                   
         CLC   GLRECNO,RECRPT      TEST MARKET RECAP REPORT                     
         BE    FIRSTS0             YES-MAKE SURE TOTALS PRINT                   
         CLC   GLRECNO,VENRPT      TEST VENDOR REPORT                           
         BNE   FIRSTS1                                                          
         CLI   SUPOPT,C'Y'         AND SUPPRESSING DETAIL COSTS                 
         BNE   FIRSTS1                                                          
*                                                                               
FIRSTS0  OI    GLINDS,GLPALTOT     YES-MAKE SURE TOTALS PRINT                   
*                                                                               
FIRSTS1  XC    LEVELS,LEVELS                                                    
         LA    R1,LEVELS           SET THE LEVELS                               
         LA    R5,RPTLEVS2         LEVELS FOR VENDOR REPORT                     
         CLC   GLRECNO,RECRPT                                                   
         BNE   *+8                                                              
         LA    R5,RPTLEVS3         LEVELS FOR RECAP REPORT                      
         CLC   GLRECNO,INVRPT                                                   
         BNE   *+8                                                              
         LA    R5,RPTLEVS1         LEVELS FOR INVOICE REPORT                    
         CLC   GLRECNO,DIVRPT                                                   
         BNE   *+8                                                              
         LA    R5,RPTLEVS4         LEVELS FOR DIVISIONAL RECAP REPORT           
         LA    RF,1                                                             
         MVI   MIDLEV,0                                                         
*                                                                               
FIRSTS2  CLI   0(R5),X'FF'                                                      
         BE    FIRSTSX                                                          
         MVC   0(1,R1),0(R5)                                                    
         CLI   1(R5),1                                                          
         BNE   *+8                                                              
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL                          
         CLI   1(R5),2                                                          
         BNE   *+8                                                              
         STC   RF,MKTLEV           MARKET LEVEL                                 
         CLI   1(R5),3                                                          
         BNE   *+8                                                              
         STC   RF,MIDLEV           MIDLINE LEVEL                                
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         LA    R5,2(R5)                                                         
         B     FIRSTS2                                                          
*                                                                               
FIRSTSX  B     XIT                                                              
         SPACE 2                                                                
RPTLEVS1 DC    AL1(QMED,0)         LEVELS FOR INVOICE REPORT                    
         DC    AL1(QCLT,0)                                                      
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,0)                                                      
         DC    AL1(QEST,0)         GST# LEVEL                                   
         DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QBHINV,1)                                                    
         DC    AL1(QMKTGR1,3)      MIDLINE                                      
         DC    AL1(QMKT,2)         ROWS                                         
         DC    AL1(QSTA,0)                                                      
         DC    X'FF'                                                            
*                                                                               
RPTLEVS2 DC    AL1(QMED,0)         LEVELS FOR VENDOR REPORT                     
         DC    AL1(QCLT,0)                                                      
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,0)                                                      
         DC    AL1(QEST,0)         GST# LEVEL                                   
         DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QCMLCLS,0)                                                   
         DC    AL1(QBHINV,1)                                                    
         DC    AL1(QMKTGR1,3)      MIDLINE                                      
         DC    AL1(QMKT,2)         ROWS                                         
         DC    AL1(QCML,0)                                                      
         DC    X'FF'                                                            
*                                                                               
RPTLEVS3 DC    AL1(QMED,0)         LEVELS FOR RECAP REPORT                      
         DC    AL1(QCLT,0)                                                      
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,0)                                                      
         DC    AL1(QEST,0)         GST#                                         
         DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QBHINV,1)                                                    
         DC    AL1(QMKTGR1,3)      MIDLINE                                      
         DC    AL1(QMKT,2)         ROWS                                         
         DC    AL1(QCMLCLS,0)                                                   
         DC    X'FF'                                                            
*                                                                               
RPTLEVS4 DC    AL1(QMED,0)         LEVELS FOR DIVISIONAL RECAP REPORT           
         DC    AL1(QCLT,0)                                                      
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,0)                                                      
         DC    AL1(QEST,0)         GST#                                         
         DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QBHINV,1)                                                    
         DC    AL1(QMKTGR1,3)      MIDLINE                                      
         DC    AL1(QMKT,2)         ROWS                                         
         DC    AL1(QCMLCLS,0)                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER'S ABOUT TO PRINT A LINE                                                
*                                                                               
PRINT    CLC   GLRECNO,TOTRPT      TEST FIRST REPORT                            
         BE    PRINT2              YES-DON'T PRINT                              
         CLC   GLRECNO,INVRPT      TEST INVOICE REPORT                          
         BNE   XIT                                                              
         CLI   PBLOPT,C'Y'         AND NOT PRE-BILL                             
         BE    XIT                                                              
         TM    GLINDS,GLTOTLIN     YES-SUPPRESS PRINTING MARKET TOTAL           
         BZ    XIT                     THERE'S NO BILLING                       
******** BZ    *+14                                                             
         CLC   GLLEVEL,MKTLEV                                                   
         BNE   XIT                                                              
         CLI   ANYBILL,C'Y'                                                     
         BE    XIT                                                              
*                                                                               
PRINT2   MVI   GLHOOK,GLDONT       DON'T PRINT                                  
         B     XIT                                                              
         EJECT                                                                  
* HEADHOOK                                                                      
*                                                                               
HEDHK    CLC   GLRECNO,INVRPT      TEST INVOICE REPORT                          
         BNE   HEDHK2                                                           
         L     R2,AH4                                                           
         A     R2,PWIDTH           YES-PRINT ESTIMATE DATES                     
         A     R2,PWIDTH                                                        
         A     R2,PWIDTH                                                        
         A     R2,PWIDTH                                                        
         GOTO1 DATCON,DMCB,(2,SBESTSTP),(5,16(R2))                              
         MVI   24(R2),C'-'                                                      
         GOTO1 DATCON,DMCB,(2,SBESTNDP),(5,25(R2))                              
         A     R2,PWIDTH                                                        
         LA    R1,SUBTITLE         PRINT PERIOD ON HEAD9                        
         LA    RE,L'SUBTITLE-1                                                  
         CLI   0(R1),C' '                                                       
         BH    *+14                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
         DC    H'0'                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(R1)                                                    
*                                                                               
         A     R2,PWIDTH           COMMENT ON HEAD10                            
         MVC   1(39,R2),=C'**ORDERED AMOUNTS ARE BASED ON STATION '             
         MVC   40(20,R2),=C'INVOICE CLEARANCES**'                               
*                                                                               
         L     R2,AH1              TITLES                                       
         MVC   29(101,R2),BLANKS                                                
         MVC   29(30,R2),=C'INITIATIVE MEDIA NORTH AMERICA'                     
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   *+10                                                             
         MVC   59(2,R2),=C'-C'                                                  
         A     R2,PWIDTH                                                        
         MVC   29(103,R2),BLANKS                                                
*                                                                               
         LA    R1,SBAGYREC         TEST CANADA                                  
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BE    *+14                                                             
         MVC   30(30,R2),=C'15 PIEDMONT CTR. ATL, GA 30305'                     
         B     *+10                                                             
         MVC   29(31,R2),=C'175 BLOOR ST. TOR. ONT. M4W 3R8'                    
*                                                                               
         A     R2,PWIDTH                                                        
         MVC   29(103,R2),BLANKS                                                
         A     R2,PWIDTH                                                        
         MVC   33(99,R2),BLANKS                                                 
         MVC   35(19,R2),=C'RADIO MEDIA BILLING'                                
         CLI   SBQMED,C'R'                                                      
         BE    *+10                                                             
         MVC   33(24,R2),=C'TELEVISION MEDIA BILLING'                           
         OC    INVDTE,INVDTE                                                    
         BZ    HEDHK4                                                           
         A     R2,PWIDTH                                                        
         A     R2,PWIDTH                                                        
         MVC   34(12,R2),=C'INVOICE DATE'                                       
         MVC   47(L'INVDTE,R2),INVDTE                                           
*                                                                               
*        PRINT GST # IN MIDDLE                                                  
*                                                                               
         CLI   GLOPTS+8,C'Y'       SKIP IF NO GST# OPTION                       
         BNE   HEDHKGSX                                                         
*                                                                               
         OC    GST#,GST#                                                        
         BZ    HEDHKGSX            SKIP IF NO GST # AVAILABLE                   
*                                                                               
         A     R2,PWIDTH                                                        
         MVC   33(23,R2),=CL23'GST# R123456789 (7.00%)'                         
         MVC   38(10,R2),GST#            FILL IN GST#                           
*                                                                               
HEDHKGSX DS    0H                                                               
         B     HEDHK4                                                           
*                                                                               
HEDHK2   L     R2,AH1                                                           
         MVC   31(101,R2),BLANKS                                                
         MVC   31(16,R2),=C'INITIATIVE MEDIA'                                   
         A     R2,PWIDTH                                                        
         MVC   31(101,R2),BLANKS                                                
         MVI   31(R2),C'-'                                                      
         MVC   32(26,R2),31(R2)                                                 
         A     R2,PWIDTH                                                        
         MVC   29(103,R2),BLANKS                                                
         MVC   29(32,R2),SUBTITLE                                               
         L     R2,AH4                                                           
         MVC   29(103,R2),BLANKS                                                
         A     R2,PWIDTH           R2=A(LINE 5)                                 
         CLC   GLRECNO,RECRPT      TEST MARKET RECAP REPORT                     
         BNE   *+18                                                             
         A     R2,PWIDTH                                                        
         MVC   37(20,R2),=C'*** MARKET RECAP ***'                               
         B     HEDHK4                                                           
*                                                                               
         CLC   GLRECNO,DIVRPT      TEST DIVISIONAL RECAP REPORT                 
         BNE   HEDHKDVN                                                         
*                                                                               
*        DIVISION RECAP TITLE AND DIVISION NAME                                 
*                                                                               
         A     R2,PWIDTH                                                        
         MVC   36(22,R2),=C'*** DIVISION RECAP ***'                             
         A     R2,PWIDTH                                                        
*                                                                               
*        CENTER DIVISION NAME                                                   
*                                                                               
         LA    R0,16               MAX LENGTH OF DIVISION NAME                  
         LA    RF,UDEFE2+15        POINT TO LAST BYTE                           
*                                                                               
         CLI   0(RF),C' '          FIND LAST CHARACTER                          
         BH    *+10                                                             
         BCTR  RF,0                BACK UP A BYTE                               
         BCT   R0,*-10                                                          
*                                                                               
*                                  R0 HAS NAME LENGTH                           
         LA    RE,16                                                            
         SR    RE,R0               UNUSED PORTION OF DIVISION NAME              
*                                                                               
         LTR   RF,R0               LENGTH OF DIVISION NAME                      
         BNP   *+6                                                              
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         SRL   RE,1                LGTH LEADING SPACES FOR CENTERING            
         LA    RE,39(RE,R2)        START OF DIVISION NAME PRINT AREA            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),UDEFE2      PRINT DIVISION NAME                          
         B     HEDHK4                                                           
*                                                                               
HEDHKDVN DS    0H                                                               
*                                                                               
         LA    R2,37(R2)                                                        
         MVC   0(7,R2),=C'VENDOR:' FORMAT CLASS NAME TO HEADLINE                
         MVC   8(4,R2),COMCLASS                                                 
         MVC   13(24,R2),CLASSNM                                                
*                                                                               
HEDHK4   L     R2,AH1              FORMAT BILLING ADDRESS                       
         BAS   RE,CKADDR           IS THERE AN OVERRIDE ADDR?                   
         BE    *+8                  YES - R3 WILL POINT TO IT                   
         LA    R3,ADDR1                                                         
         LA    R0,4                                                             
         LR    R1,R2               REMOVE RUN AND REQUESTOR INFO                
         MVC   1(24,R1),BLANKS                                                  
         A     R1,PWIDTH                                                        
         MVC   1(24,R1),BLANKS                                                  
         CLC   GLRECNO,INVRPT      TEST INVOICE REPORT                          
         BNE   HEDHK8                                                           
         MVC   62(7,R2),=C'INVOICE'   YES-PRINT INVOICE NUM FIRST               
         LA    RF,70(R2)                                                        
*                                                                               
         LA    R1,SBAGYREC         TEST CANADA                                  
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   *+12                                                             
         MVI   0(RF),C'C'          PROCEED MEDIA BY C                           
         LA    RF,1(RF)                                                         
*                                                                               
         MVC   0(1,RF),SBQMED                                                   
         MVI   1(RF),C'-'                                                       
         MVC   2(6,RF),INVOICE                                                  
         CLC   INVOICE,BLANKS                                                   
         BNH   *+14                                                             
         CLC   INVOICE,=C'999999'                                               
         BNE   *+10                                                             
         MVC   0(7,RF),=C'UNKNOWN'                                              
         XC    INVOICE,INVOICE                                                  
         A     R2,PWIDTH                                                        
         A     R2,PWIDTH                                                        
*                                                                               
HEDHK8   MVC   62(18,R2),0(R3)                                                  
         A     R2,PWIDTH                                                        
         LA    R3,L'ADDR1(R3)                                                   
         BCT   R0,HEDHK8                                                        
*                                                                               
HEDHKX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* CKADDR: CHECK FOR BILLING ADDRESS OVERRIDES                         *         
* INPUT:  SBPRD (SET BY OPRD)                                         *         
* OUTPUT: R3=A(PADDR1) AND CC EQ FOR OVERRIDE                         *         
*         CC NEQ FOR NO OVERRIDE                                      *         
*                                                                     *         
***********************************************************************         
CKADDR   LR    R0,RE                                                            
         L     R3,SBAIO2                                                        
         CLC   SBPRD,PKEYPRD-PRDHDRD(R3)                                        
         BE    CKA10               ALREADY HAVE REC                             
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PRDHDRD,R3                                                       
         MVC   PKEYAM,SBBAGYMD                                                  
         MVC   PKEYCLT,SBBCLT                                                   
         MVC   PKEYPRD,SBPRD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'PKEY),KEYSAVE                                              
         BNE   CKANO                                                            
         L     R3,SBAIO2                                                        
         ST    R3,AIO                                                           
         GOTO1 GETREC                                                           
CKA10    OC    PADDR2(L'PADDR2+L'PADDR3+L'PADDR4),PADDR2                        
         BZ    CKANO               NO OVERRIDE                                  
         LA    R3,PADDR1                                                        
*                                                                               
CKAYES   LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
CKANO    LTR   RE,R0                                                            
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
* PST TABLE                                                                     
*                                                                               
PSTTAB   DC    CL2'PQ',C'S',AL3(06500),X'BCBE'  PQ MAY30/94                     
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
NSPOTS   DS    F                                                                
ACTSPOTS DS    F                                                                
AVGCOST  DS    F                                                                
TOTDUE   DS    F                                                                
MGRPDUE  DS    F                                                                
MGRPSPT  DS    F                                                                
INVDUE   DS    F                                                                
INVSPT   DS    F                                                                
CMTLEN   DS    F                                                                
ACURMKT  DS    A                                                                
*                                                                               
CPSOPT   DS    CL1                                                              
DUBOPT   DS    CL1                                                              
SUPOPT   DS    CL1                                                              
RECOPT   DS    CL1                                                              
INVOPT   DS    CL1                                                              
PBLOPT   DS    CL1                                                              
GSTOPT   DS    CL1                                                              
DIVOPT   DS    CL1                                                              
BONUSSW  DS    CL1                                                              
ANYBILL  DS    CL1                                                              
ANYBUY   DS    CL1                                                              
INVOICE  DS    CL6                                                              
BHINV    DS    CL6                                                              
DUEDATE  DS    CL8                                                              
INVDTE   DS    CL8                                                              
COMCLASS DS    CL4                                                              
CLASSNM  DS    CL24                                                             
ADDR1    DS    CL30                                                             
ADDR2    DS    CL30                                                             
ADDR3    DS    CL30                                                             
ADDR4    DS    CL30                                                             
CLSSPLIT DS    XL24                                                             
VENRPT   DS    XL1                                                              
TOTRPT   DS    XL1                                                              
RECRPT   DS    XL1                                                              
INVRPT   DS    XL1                                                              
DIVRPT   DS    XL1                                                              
PSTRATE  DS    XL3                                                              
THISKEY  DS    XL7                                                              
SVKEY    DS    XL7                                                              
SVPRD    DS    XL1                                                              
SVEST    DS    XL1                                                              
SVSTA    DS    XL3                                                              
GST#     DS    CL10                                                             
UDEFE2   DS    CL32                                                             
*                                                                               
BLANKS   DC    CL132' '                                                         
*                                                                               
GPRFPRMS DS    CL16                GETPROF PARAMETERS                           
CLTPRFVA DS    CL16                CLIENT VA PROFILE                            
*                                                                               
MKTTAB   DS    (MAXMKT)XL6                                                      
MKTTABX  EQU   *                                                                
MAXMKT   EQU   500                                                              
*                                                                               
MKTNMTAB DS    CL(NMKTNMS*24)                                                   
NMKTNMS  EQU   50                                                               
*                                                                               
WORKAREA DS    CL2000                                                           
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    F                                                                
         DS    F                                                                
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DDMASTD                                                                        
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPTRCMML                                                                       
*SPTRCMLCLS                                                                     
*SPGENAGY                                                                       
*SPGENPRD                                                                       
*SPGENSTA                                                                       
*SPGENBUY                                                                       
*SPGENBILL                                                                      
*SPGENSTAB                                                                      
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPTRCMLCLS                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRID9D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPWRI29   04/12/10'                                      
         END                                                                    
