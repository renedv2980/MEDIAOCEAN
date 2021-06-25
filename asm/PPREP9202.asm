*          DATA SET PPREP9202  AT LEVEL 098 AS OF 01/24/11                      
*PHASE PP9202A                                                                  
*INCLUDE BINSRCH                                                                
         TITLE 'PRINTPAK ESTIMATE CLOSEOUT PROGRAM'                             
*_____________*________________________*______________________________*         
* MODE        I       PASS 1           I         PASS 2               *         
*             I** PASS SET TO 0 FOR    I                              *         
*             I**  PASS 1              I                              *         
*_____________*________________________*______________________________*         
* RUNFRST     *CLEAR WORK AREAS        *N/A                           *         
*=============*========================*============================= *         
* FBUYCLI     *BUILD A LIST OF CLOSABLE*EXIT                          *         
*             * ESTIMATES              *                              *         
*=============*========================*============================= *         
* PROCBIL     *PRINT BILL REPORT AND   *CLOSE BILL RECORDS            *         
*             * ADD BILL DETAILS TO    *                              *         
*             * TEMP BUCKETS           *                              *         
*=============*========================*============================= *         
* LBILCLI     *PRINT BILL REPORT TOTALS*EXIT                          *         
*=============*========================*============================= *         
* PROCBUY     *PRINT BUY REPORT AND    *CLOSE BUY RECORDS             *         
*             * ADD BUY DETAILS TO     *                              *         
*             * TEMP BUCKETS           *                              *         
*=============*========================*============================= *         
* LBUYCLT     *PRINT BUY REPORT TOTALS *PRINT REQ. RECORD COUNTS. ADD *         
*             *CHECK TO SEE IF REQ.    * TO TOTAL RECORD COUNTS       *         
*             * IS CLOSEABLE.          *CLOSE ESTIMATE HEADERS        *         
*             *RESET MODE TO FBUYCLT   *CLOSE BUCKET RECORDS          *         
*             * IF REQ. CLOSEABLE.     *CLOSE UPLOAD RECORDS          *         
*=============*========================*============================= *         
* RUNLAST     * N/A                    *PRINT ESTIMATE SUMMARY        *         
*             *                        *PRINT AGENCY SUMMARY          *         
*=============*========================*============================= *         
         SPACE 3                                                                
***********************************************************************         
*                                                                               
*        QOPT1 F= FORCE CLOSE                                                   
*        QOPT2 T= TEST RUN - DON'T MARK FILE                                    
*        QOPT3 B= NO BUY DETAIL                                                 
*        QOPT3 P= NO BUY OR PUB DETAIL                                          
*        QOPT4 Y= IGNORE P92 PROFILE                                            
*              R= REVERSE P92 PROFILE WORKINGS                                  
*                 REPORT ONLY THOS CLIENTS INSTEAD                              
*                 OF SKIPPING THEM                                              
*                                                                               
***                                                                             
***      NOTE: IF PAGYPROF+14 = C'1'                                            
***               PAID, UNPAID, BILLED, UNBILLED AMOUNTS ARE GROSS              
***               OTHERWISE THEY ARE GROSS-CD                                   
***                                                                             
**************************                                                      
********* CHANGE LOG                                                            
**************************                                                      
*                                                                               
* BPLA  01/11   NEW VALUE (R) FOR QOPT4 TO ONLY REPORT                          
*               CLIENTS NORMALLY SKIPPED (CONTROLLED                            
*               BY THE P92 PROFILE)                                             
*                                                                               
* SMYE 04/06    DISPLAY "S" BEFORE DATE FOR STEWARDSHIP BUYS                    
*                                                                               
*        SMYE 04/14/05 IF QOPT4=Y, IGNORE P92 PROFILE (PROGPROF)                
*                                                                               
*        KWAN 08/00    NEW PBILLREC (IN PNNEWFILE)                              
*                                                                               
*        BPLA 6/4/90   FIXED BILLREC PRINTING TO SHOW NEW BILLS                 
*                      B4-7,M4-7,R4-7,RETAIL,AOR                                
*                      DON'T TOTAL RETAIL SUMMARY AND AOR BILLS                 
*                      LEVEL=179-182                                            
*                                                                               
*       BPLA 1/28/93   ADD ESTIMATE FILTERING LOGIC TO PP92ES                   
*                                                                               
*       BPLA 2/1/93    ACTIVITY CHECK - SUPPRESS CLIENT TOTALS                  
*                      IF NO BUYS OR BILLS ARE PROCESSED FOR THE                
*                      CLIENT (CLTACT)                                          
*       BPLA 3/9/93    ESTLIST AND ETAB EXPANDED                                
*       BPLA 3/11/93   ETAB EXPANDED AGAIN                                      
*                                                                               
*       BPLA 6/22/93   CLEAR UNPAID AND UNBILLED AMOUNTS FOR TEST BUYS          
*                      AT BUYR4                                                 
*                                                                               
*       BPLA 3/11/94   NEW RCSUBPRG (21) FOR CLIENT TOTALS                      
*                                                                               
*       BPLA 3/29/94   AGY SUMMARY TABLES EXPANDED                              
*                                                                               
*       BPLA 4/18/94   SOME ACCUMULATORS CHANGED TO PACKED                      
*                      TO ACCOMDATE LARGE NUMBERS                               
*                                                                               
*       BPLA 5/9/95    EXPAND ATAB FROM 8500 CLT/MTHS TO 10000                  
*                                                                               
*       SMYE 12/14/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                 
*                                                                               
*        BPLA   1/29/97   IF PROCESSING ONE CLIENT                              
*                         ALWAYS CLTACT TO 'Y'                                  
*                                                                               
*        BPLA   7/99     EXPAND ATAB FROM 10000 CLT/MTHS TO 15000               
*                        EXPAND ATABR FROM 1000 CLT/MTHS TO 1500                
*                        NOTE THAT I CAN'T USE ATABMAX IN BINSRCH               
*                        CALL - IT DOESN'T ASSEMBLE                             
*                                                                               
***********************************************************************         
         EJECT                                                                  
PP9202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP9202                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
*                                                                               
         LA    R7,PP9202+4095    ** NOTE USE OF R7 AS SECOND BASE               
         LA    R7,1(R7)          ** REGISTER                                    
         USING PP9202+4096,R7                                                   
*                                                                               
         RELOC (R3)                                                             
         L     R2,=A(PP92WKC)                                                   
         AR    R2,R3                                                            
         USING PP92WORK,R2                                                      
         ST    R3,RELO                                                          
*                                                                               
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         CLI   MODE,RUNFRST ------> FIRST TIME FOR PROGRAM                      
         BNE   CM                   YES - CLEAR ALL WORK AREAS                  
         GOTO1 =A(CLEARW),RR=RELO                                               
         B     PP9202EX                                                         
*                                                                               
CM       CLI   MODE,RUNLAST                                                     
         BE    CM5                                                              
*                                                                               
         CLI   MODE,FBUYREQ ------> FIRST BUY FOR THIS REQUEST                  
         BNE   CMA                                                              
         OI    DMINBTS,X'08'        PASS DELETES                                
*                                                                               
         MVI   FCRDTEST,C'Y'        SET TO READ TEST BUYS                       
         MVI   FCOAPRD,C'Y'         SET TO PROCESS * PRDS ALSO                  
*                                   BILLED/PAID STATUS WILL NOT BE              
*                                   CHECKED AS THESE BUYS ARE NEVER             
*                                   BILLED/PAID                                 
*                                                                               
         CLC   QEST,=C'ALL'                                                     
         BNE   *+10                                                             
         MVC   QEST,SPACES                                                      
         LA    R1,RCDATE                                                        
         MVC   WORK(2),6(R1)                                                    
         MVC   WORK+2(2),0(R1)                                                  
         MVC   WORK+4(2),3(R1)                                                  
         GOTO1 DATCON,DUB,(0,WORK),(3,TODAY)                                    
*                                                                               
CMA      DS    0H                                                               
         CLI   SAVAGM,0  --------> FIRST TIME THRU                              
         BE    CMB                                                              
         CLC   SAVAGM,PAGYKAGY ----- SAME AGENCY MEDIA                          
         BE    CMB                                                              
         XC    SAVAGYR,PAGYREC                                                  
         XC    PAGYREC(L'SAVAGYR),SAVAGYR                                       
         XC    SAVAGYR,PAGYREC                                                  
*                                                                               
         GOTO1 VPESTSUM          ----> PRINT ESTIMATE AND                       
         GOTO1 VPAGYSUM                          AGENCY SUMMARY                 
         MVC   PAGYREC(L'SAVAGYR),SAVAGYR                                       
CMB      DS    0H                                                               
         MVC   SAVAGYR,PAGYREC                                                  
         CLI   MODE,FBUYCLI ----------> FIRST BUY FOR CLIENT                    
         BNE   CM1                                                              
*                                                                               
*---------> BUILD LIST OF CLOSEABLE ESTIMATES                                   
*                                                                               
         CLI   QOPT4,C'R'          SEE IF REVERSING USE OF                      
         BNE   CMB0                P92 PROFILE                                  
         CLI   PROGPROF,C'Y'       NORMALLY SKIPPED                             
         BE    CMB1                                                             
         B     CMB0X               SKIP OTHERS                                  
*                                                                               
CMB0     CLI   PROGPROF,C'Y'       SKIP THIS CLT                                
         BNE   CMB1                                                             
         CLI   QOPT4,C'Y'          IGNORE ABOVE PROFILE                         
         BE    CMB1                                                             
*                                                                               
CMB0X    MVI   MODE,LBUYCLI ----------> LAST BUY FOR CLIENT                     
         B     PP9202EX                                                         
CMB1     DS    0H                                                               
         CLI   PASS,2                                                           
         BE    PP9202EX                                                         
*                                                                               
*------>  PASS ONE FOR CLIENT PROCESSING                                        
*                                                                               
         MVI   CLTACT,C'N'          SET OFF CLT ACTIVITY SWITCH                 
         CLC   QCLIENT,=C'ALL'      ALL CLIENTS                                 
         BE    CMB3                                                             
         CLI   QCLIENT,C'*'         OFFICE REQUEST                              
         BE    CMB3                                                             
         CLI   QCLIENT,C'&&'        BILLING GROUP                               
         BE    CMB3                                                             
         CLI   QCLIENT,C'$'         OFFICE LIST                                 
         BE    CMB3                                                             
         MVI   CLTACT,C'Y'          SINGLE CLIENT REQUEST                       
*                                   ALWAYS SET ON CLTACT                        
*                                   THIS IS NEEDED SINCE                        
*                                   THE ESTIMATES                               
*                                   MAY HAVE NO BUYS OR BILLS                   
*                                                                               
*                                                                               
CMB3     DS    0H                                                               
         LA    RE,BYTOT                                                         
         LA    R0,7                                                             
CMB5     ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   R0,CMB5                                                          
*                                                                               
         ZAP   BLRAMTT,=P'0'     BLLTOT  TOTALS                                 
         ZAP   BLRUNRT,=P'0'                                                    
*                                                                               
         XC    RRECCNT,RRECCNT                                                  
         XC    WATAB,WATAB                                                      
         XC    PREVPUB,PREVPUB                                                  
         GOTO1 VPP92ES             BUILD A LIST OF CLOSEABLE EST.               
         CLI   ERROR,0                                                          
         BE    CME                                                              
         GOTO1 VPROGERR          -------.> PROCESS ERROR PASS 1                 
         B     PP9202EX                                                         
*                                                                               
CME      MVI   FORCEHED,C'Y'  ------> GO TO SUB PGM 10                          
         MVI   RCSUBPRG,10            CLOSEOUT BILL REPORT                      
         B     PP9202EX                                                         
         SPACE 3                                                                
*                                                                               
*---------> PRINT BILL REPORT AND ADD BILL DETAILS TO TEMP BUCKETS              
*                                                                               
CM1      CLI   MODE,PROCBIL --------> PROCESS BILLS                             
         BNE   CM2                                                              
         XC    PBUYTOT,PBUYTOT     CLEAR BUY TOTALS                L01          
         XC    PREVPUB,PREVPUB     CLEAR PREVIOUS PUB              L01          
         TM    KEY+25,X'80'                                                     
         BNZ   PP9202EX                                                         
         BAS   R9,FLTRBIL          FILTER BILLS AGAINST CLOSEABLE EST.          
         CLI   BYTE,0               PASS FILTER                                 
         BE    PP9202EX              NO - EXIT                                  
         MVI   CLTACT,C'Y'         SET ON CLIENT ACTIVITY                       
         LA    RF,PBILLREC                                                      
         CLI   PASS,2                                                           
         BNE   *+12                                                             
         LA    RE,RBLRCNT                                                       
         B     CLOSREC                                                          
*                                                                               
*---------> BILLING PROCESSING PASS 1                                           
*                                                                               
         CLI   RCSUBPRG,10                                                      
         BE    CM1A                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,10           BILL CLOSEOUT REPORT                       
CM1A     GOTO1 VPP92BLR                                                         
         B     PP9202EX                                                         
*                                                                               
CM2      CLI   MODE,LBILPRO  ---------> LAST BILL PROCESSED                     
         BNE   CM3                                                              
         CLI   RCSUBPRG,10            BILL CLOSEOUT REPORT SECTION              
         BNE   PP9202EX                                                         
         CLI   PASS,2                                                           
         BE    PP9202EX                                                         
         B     BILLTOT                                                          
*                                                                               
         SPACE 3                                                                
*                                                                               
*---------> PRINT BUY REPORT AND ADD BUY DETAILS TO TEMP BUCKETS                
*                                                                               
CM3      CLI   MODE,PROCBUY ----------> PROCESS BUY RECORDS                     
         BNE   CM3B                                                             
*                                                                               
         TM    KEY+25,X'C0'        ALREADY CLOSED OUT                           
         BO    PP9202EX                                                         
         TM    PBUYCNTL,X'80'      TEST DELETED                                 
         BZ    *+10                 NO                                          
         XC    GROSS(20),GROSS      YES - ZERO ORDERED                          
         BAS   R9,FLTRBUY          FILTER BUYS AGAINST CLOSEABLE EST.           
         CLI   BYTE,0               PASS FILTER                                 
         BE    PP9202EX              NO - EXIT                                  
*                                                                               
         MVI   CLTACT,C'Y'         SET ON CLIENT ACTIVITY                       
         LA    RF,PBUYREC                                                       
         CLI   PASS,2                                                           
         BNE   CM3A90                                                           
*                              ADD TO CLIENT TOTALS                             
*                                                                               
         CLI   PBDBFD,C'T'  '      SEE IF TEST BUY                              
         BNE   *+10                 NO                                          
         XC    GROSS(20),GROSS      YES - ZERO ORDERED                          
*                                                                               
         CLI   PBUYKPRD,C'*'       SEE IF * PRD BUY                             
         BNE   *+10                 NO                                          
         XC    GROSS(20),GROSS      YES - ZERO ORDERED                          
*                                                                               
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         AP    CBGO,DUB        GROSS ORDERED                                    
         L     R0,PGROSS                                                        
         CVD   R0,DUB                                                           
         AP    CBGP,DUB        GROSS PAID                                       
         L     R0,BGROSS                                                        
         CVD   R0,DUB                                                           
         AP    CBGB,DUB        GROSS BILLED                                     
*                                                                               
         L     R0,GROSS                                                         
         S     R0,AGYCOM                                                        
         CVD   R0,DUB                                                           
         AP    CBNO,DUB        NET ORDERED                                      
         L     R0,PGROSS                                                        
         S     R0,PAGYCOM                                                       
         CVD   R0,DUB                                                           
         AP    CBNP,DUB        NET PAID                                         
         L     R0,BGROSS                                                        
         S     R0,BAGYCOM                                                       
         CVD   R0,DUB                                                           
         AP    CBNB,DUB        NET BILLED                                       
*                                                                               
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         AP    CBCDO,DUB       CD ORDERED                                       
         L     R0,PCSHDSC                                                       
         CVD   R0,DUB                                                           
         AP    CBCDP,DUB       CD PAID                                          
         L     R0,BCSHDSC                                                       
         CVD   R0,DUB                                                           
         AP    CBCDB,DUB       CD BILLED                                        
*                              ADD TO RUN TOTALS                                
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         AP    RBGO,DUB        GROSS ORDERED                                    
         L     R0,PGROSS                                                        
         CVD   R0,DUB                                                           
         AP    RBGP,DUB        GROSS PAID                                       
         L     R0,BGROSS                                                        
         CVD   R0,DUB                                                           
         AP    RBGB,DUB        GROSS BILLED                                     
*                                                                               
         L     R0,GROSS                                                         
         S     R0,AGYCOM                                                        
         CVD   R0,DUB                                                           
         AP    RBNO,DUB        NET ORDERED                                      
         L     R0,PGROSS                                                        
         S     R0,PAGYCOM                                                       
         CVD   R0,DUB                                                           
         AP    RBNP,DUB        NET PAID                                         
         L     R0,BGROSS                                                        
         S     R0,BAGYCOM                                                       
         CVD   R0,DUB                                                           
         AP    RBNB,DUB        NET BILLED                                       
*                                                                               
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         AP    RBCDO,DUB       CD ORDERED                                       
         L     R0,PCSHDSC                                                       
         CVD   R0,DUB                                                           
         AP    RBCDP,DUB       CD PAID                                          
         L     R0,BCSHDSC                                                       
         CVD   R0,DUB                                                           
         AP    RBCDB,DUB       CD BILLED                                        
*                                                                               
         LA    RE,RBYRCNT                                                       
         B     CLOSREC                                                          
*                                                                               
*---------> PASS 1 BUY PROCESSING                                               
*                                                                               
CM3A90   CLI   RCSUBPRG,20              CLOSEOUT BUY REPORT PROCESS             
         BE    CM3AX                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,20               FORCE BUY REPORT                       
CM3AX    GOTO1 VPP92BYR                                                         
         B     PP9202EX                                                         
*                                                                               
CM3B     CLI   MODE,FBUYPRO ------------> PROCESS FIRST BUY FOR PRODUCT         
         BNE   CM3C                                                             
         CLI   PASS,2                                                           
         BE    PP9202EX                                                         
*                                                                               
*---------> PASS 1 BUY PROCESSING                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,20               FORCE BUY REPORT                       
CM3C     CLI   MODE,LBUYPRO  -------> LAST BUY FOR PRODUCT                      
         BNE   CM4                                                              
         BAS   R9,BUYTOT           DO BUY REPORT TOTALS                         
         XC    PREVPUB,PREVPUB     XXXXXXXXXX                                   
         XC    PBUYTOT,PBUYTOT                                                  
*                                                                               
         SPACE 3                                                                
*                                                                               
*---------> PASS 2                                                              
*                                                                               
CM4      CLI   MODE,LBUYCLI  -------> LAST BUY FOR CLIENT                       
         BNE   CM5                                                              
         CLI   PASS,2                                                           
         BE    CM4A                                                             
         MVC   BYTOTE(56),BYTOT  -------> PASS 1                                
         BAS   R9,BUYTOT           DO BUY REPORT TOTALS                         
         GOTO1 VSETCLOS         CHECK IF REQUEST CLOSEABLE                      
         CLI   ERROR,0                                                          
         BNE   CM4A                 NO CLOSABLE - SAVE ESTIMATES                
         CLI   QOPT2,C'T' ---------->  TEST --NO CLOSEOUT                       
         BE    CM41                                                             
         CLI   RCWRITE,C'Y'                                                     
         BE    *+12                                                             
         MVI   PASS,0              RESET FOR NEXT REQUEST                       
         B     CM4A                                                             
*                                                                               
*---------> BEGIN PASS 2 PROCESSING                                             
*                                                                               
CM41     MVI   PASS,2                                                           
         MVI   FCRDACTV,C'Y'       FOR PASS 2 - READ AND MARK ACTIVE            
         MVI   MODE,FBUYCLI ------> SET FIRST BUY FOR CLIENT                    
         OI    DMINBTS,X'08'          PASS DELETES                              
         B     PP9202EX                                                         
*                                                                               
CM4A     CLI   PASS,2                                                           
         BNE   CM4B                                                             
         GOTO1 VSAVEST                                                          
         BAS   R9,CLOSEST          CLOSE ESTIMATE HEADERS                       
         BAS   R9,CLTTOT                                                        
         CLI   CLTACT,C'Y'                                                      
         BNE   CM4A5                                                            
         BAS   R9,PRECCNT          PRINT REQUEST RECORD COUNTS                  
CM4A5    MVI   CLTACT,C'N'         JUST IN CASE                                 
         B     CM4C                                                             
*                                                                               
CM4B     GOTO1 VSAVEST           TRANS. REQ EST TO SUM EST                      
*                                                                               
CM4C     MVI   ERROR,0                                                          
         MVI   PASS,0                                                           
         MVI   FCRDACTV,C'N'       RESET ACTIVE READ SW                         
         B     PP9202EX                                                         
*                                                                               
CM5      CLI   MODE,RUNLAST  -------> LAST TIME HOOK FOR REQ                    
         BNE   PP9202EX                                                         
         GOTO1 VPESTSUM                                                         
         GOTO1 VPAGYSUM                                                         
*                                                                               
         MVI   RCSUBPRG,90                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+10(16),=C'** RUN TOTALS **'                                    
         MVI   SPACING,2                                                        
         GOTO1 VRPRT                                                            
         MVC   P+10(5),=C'GROSS'                                                
         EDIT  (P8,RBGO),(14,P+18),2,MINUS=YES                                  
         EDIT  (P8,RBGP),(14,P+33),2,MINUS=YES                                  
         EDIT  (P8,RBGB),(14,P+48),2,MINUS=YES                                  
         SP    RBGO,RBGP                       PAYABLE                          
         EDIT  (P8,RBGO),(14,P+63),2,MINUS=YES                                  
         AP    RBGO,RBGP                                                        
         SP    RBGO,RBGB                       BILLABLE                         
         EDIT  (P8,RBGO),(14,P+78),2,MINUS=YES                                  
         AP    RBGO,RBGB                       RESTORE                          
*                                                                               
         GOTO1 VRPRT                                                            
         MVC   P+12(3),=C'NET'                                                  
         EDIT  (P8,RBNO),(14,P+18),2,MINUS=YES                                  
         EDIT  (P8,RBNP),(14,P+33),2,MINUS=YES                                  
         EDIT  (P8,RBNB),(14,P+48),2,MINUS=YES                                  
         SP    RBNO,RBNP                       PAYABLE                          
         EDIT  (P8,RBNO),(14,P+63),2,MINUS=YES                                  
         AP    RBNO,RBNP                                                        
         SP    RBNO,RBNB                       BILLABLE                         
         EDIT  (P8,RBNO),(14,P+78),2,MINUS=YES                                  
         AP    RBNO,RBNB                       RESTORE                          
*                                                                               
         GOTO1 VRPRT                                                            
         MVC   P+13(2),=C'CD'                                                   
         EDIT  (P8,RBCDO),(14,P+18),2,MINUS=YES                                 
         EDIT  (P8,RBCDP),(14,P+33),2,MINUS=YES                                 
         EDIT  (P8,RBCDB),(14,P+48),2,MINUS=YES                                 
         SP    RBCDO,RBCDP                     PAYABLE                          
         EDIT  (P8,RBCDO),(14,P+63),2,MINUS=YES                                 
         AP    RBCDO,RBCDP                                                      
         SP    RBCDO,RBCDB                     BILLABLE                         
         EDIT  (P8,RBCDO),(14,P+78),2,MINUS=YES                                 
         AP    RBCDO,RBCDB                     RESTORE                          
*                                                                               
         GOTO1 VRPRT                                                            
*****                                                                           
         MVI   SPACING,4                                                        
         GOTO1 VRPRT                                                            
         MVI   SPACING,2                                                        
         MVC   P+5(28),=C'YEAR TO DATE CURRENT BILLING'                         
         GOTO1 VRPRT                                                            
         MVI   SPACING,2                                                        
         MVC   P+10(8),=C'GROSS = '                                             
         EDIT  (P8,NGRSAMT),(13,P+18),2,MINUS=YES,ALIGN=LEFT                    
         GOTO1 VRPRT                                                            
         MVC   P+10(8),=C'NET   = '                                             
         EDIT  (P8,NNETAMT),(13,P+18),2,MINUS=YES,ALIGN=LEFT                    
         GOTO1 VRPRT                                                            
*                                                                               
*        PRINT RECORD COUNTS                                                    
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 VRPRT                                                            
*                                                                               
         LA    R4,P                                                             
         USING ESRLINE,R4                                                       
*                                                                               
         MVC   ESRCOM1,=C'ESTIMATE RECORDS DELETED'                             
         L     R5,TERCNT                                                        
         EDIT  (R5),(8,ESRCNT)                                                  
         GOTO1 VRPRT                                                            
         MVC   ESRCOM1,=C'    BILL RECORDS DELETED'                             
         L     R5,TBLRCNT                                                       
         EDIT  (R5),(8,ESRCNT)                                                  
         GOTO1 VRPRT                                                            
         MVC   ESRCOM1,=C'     BUY RECORDS DELETED'                             
         L     R5,TBYRCNT                                                       
         EDIT  (R5),(8,ESRCNT)                                                  
         GOTO1 VRPRT                                                            
         DROP  R4                                                               
*****                                                                           
PP9202EX XMOD1 1                                                                
         TITLE 'FILTER BILL RECORDS ON CLOSEABLE ESTIMATES'                     
FLTRBIL  MVI   BYTE,1                                                           
         LA    RE,ESTLST                                                        
FLTRBIL2 CLI   0(RE),0                                                          
         BNE   *+10                                                             
         MVI   BYTE,0                                                           
         BR    R9                                                               
         CLC   PBILKPRD(5),0(RE)                                                
         BER   R9                                                               
         LA    RE,7(RE)                                                         
         B     FLTRBIL2                                                         
         TITLE 'FILTER BUYS ON CLOSEABLE ESTIMATES'                             
FLTRBUY  MVI   BYTE,1                                                           
         LA    RE,ESTLST                                                        
FLTRBUY2 CLI   0(RE),0                                                          
         BNE   *+10                                                             
         MVI   BYTE,0                                                           
         BR    R9                                                               
         CLC   PBUYKPRD,0(RE)                                                   
         BE    *+12                                                             
         LA    RE,7(RE)                                                         
         B     FLTRBUY2                                                         
         CLC   PBUYKEST,3(RE)                                                   
         BER   R9                                                               
         LA    RE,7(RE)                                                         
         B     FLTRBUY2                                                         
         TITLE 'CLOSE RECORDS AND ADD TO RECORD COUNTS'                         
CLOSREC  L     R8,0(RE)            A(RECORD COUNTER)                            
         LA    R8,1(R8)                                                         
         ST    R8,0(RE)                                                         
         CLI   QOPT2,C'T' -------> TEST // DO  NOT DELETE                       
         BE    PP9202EX                                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   PP9202EX                                                         
         MVC   KEY(25),0(RF)                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),PRTDIR,KEY,KEY                     
         TM    KEY+25,X'C0'        ALREADY CLOSED                               
         BO    PP9202EX                                                         
         TM    KEY+25,X'80'                                                     
         BZ    *+8                                                              
         OI    KEY+25,X'20'                                                     
         OI    KEY+25,X'C0'                                                     
*****                                                                           
         CLI   KEY+3,X'08'                     SEE IF BILLREC                   
         BNE   CLOSREC3                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TODAYC)      GET TODAY'S DATE               
         CLC   TODAYC(2),PBILLDAT                IS TODAY > BILLDATE            
         BH    CLOSREC3                                                         
         AP    NGRSAMT,PBILLGRS                  ACCUM BILL GROSS               
         AP    NNETAMT,PBILLNET                  AND BILL NET                   
*****                                                                           
CLOSREC3 GOTO1 DATAMGR,DMCB,DMWRT,PRTDIR,KEY,KEY                                
         B     PP9202EX                                                         
         TITLE 'PRINT BILL REPORT TOTAL LINES'                                  
BILLTOT  LA    R8,P                                                             
         USING BLRLINE,R8                                                       
         MVC   BLRMON(14),=C'*PRODUCT TOTAL'                                    
         EDIT  BLRAMTE,(15,BLRAMT),2,COMMAS=YES,MINUS=YES                       
         EDIT  BLRUNRE,(15,BLRUNR),2,COMMAS=YES,MINUS=YES                       
         GOTO1 VRPRT                                                            
*                                                                               
*    ADD BILL CONTROL TOTALS                                                    
*                                                                               
         AP    BLRAMTT,BLRAMTE      ROLL BILL TOTALS                            
         AP    BLRUNRT,BLRUNRE                                                  
*                                                                               
         ZAP   BLRAMTE,=P'0'          BLLETOT TOTALS                            
         ZAP   BLRUNRE,=P'0'                                                    
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,20            FORCE BUY REPORT SECTION                  
         B     PP9202EX                                                         
         DROP  R8                                                               
         TITLE 'PRINT BUY REPORT TOTAL LINES'                                   
BUYTOT   LA    R8,P                                                             
         USING BYRLINE,R8                                                       
         CLI   MODE,LBUYCLI  --------> LAST BUY FOR CLIENT                      
         BE    BUYTOTA                                                          
         OC    PREVPUB,PREVPUB     ANY ACTIVITY                                 
         BZR   R9                                                               
         MVI   PREVPUB,X'FF'                                                    
         GOTO1 VPP92BYR                                                         
         MVC   BYRIDTE(14),=C'*PRODUCT TOTAL'                                   
         B     BUYTOTB                                                          
*                                                                               
BUYTOTA  DS    0H                                                               
         CLI   CLTACT,C'Y'         SEE IF ANY CLIENT ACTIVITY                   
         BNER  R9                                                               
         LA    R8,PSECOND                                                       
         MVI   FORCEHED,C'Y'       CLIENT TOTALS ON NEW PAGE                    
         MVI   RCSUBPRG,21         CLIENT TOTALS - NO PRODUCT                   
*                                                                               
         MVC   BYRIDTE(14),=C'*CLT BUY TOTAL'                                   
BUYTOTB  DS    0H                                                               
         EDIT  BYRGOE,(15,BYRGO),2,COMMAS=YES,MINUS=YES                         
         EDIT  BYRCDE,(15,BYRCD),2,COMMAS=YES,MINUS=YES                         
         EDIT  BYRGLCDE,(15,BYRGLCD),2,COMMAS=YES,MINUS=YES                     
         EDIT  BYRPE,(15,BYRP),2,COMMAS=YES,MINUS=YES                           
         EDIT  BYRNPE,(15,BYRNP),2,COMMAS=YES,MINUS=YES                         
         EDIT  BYRBE,(15,BYRB),2,COMMAS=YES,MINUS=YES                           
         EDIT  BYRNBE,(15,BYRNB),2,COMMAS=YES,MINUS=YES                         
         GOTO1 VRPRT                                                            
*                                                                               
         CLI   MODE,LBUYCLI  ----------> LAST BUY FOR CLIENT                    
         BNE   BUYTOTC                                                          
         LA    R8,P                                                             
         MVC   BYRIDTE(15),=C'*CLT BILL TOTAL'                                  
         EDIT  BLRAMTT,(15,BYRGO),2,COMMAS=YES,MINUS=YES                        
         LA    R8,PSECOND                                                       
         MVC   BYRIDTE(17),=C'*CLT UNRVSD TOTAL'                                
         EDIT  BLRUNRT,(15,BYRGO),2,COMMAS=YES,MINUS=YES                        
         MVI   SPACING,3                                                        
         GOTO1 VRPRT                                                            
         LA    R0,7                                                             
         LA    RE,BYTOTE                                                        
BUYTOTB5 ZAP   0(8,RE),=P'0'      CLEAR TOTALS                                  
         LA    RE,8(RE)                                                         
         BCT   R0,BUYTOTB5                                                      
         BR    R9                                                               
*                                                                               
*  ROLL TOTALS FOR REPORT                                                       
*                                                                               
BUYTOTC  LA    RE,BYTOTE                                                        
         LA    RF,BYTOT                                                         
         LA    R0,7                                                             
BUYTOT1  AP    0(8,RF),0(8,RE)                                                  
         ZAP   0(8,RE),=P'0'         CLEAR AFTER ROLLED                         
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,BUYTOT1                                                       
         BR    R9                                                               
         DROP  R8                                                               
*                                                                               
         EJECT                                                                  
CLTTOT   DS    0H                                                               
         CLI   CLTACT,C'Y'       ANY ACTIVITY                                   
         BNE   CLTTOT8                                                          
         MVC   P+10(19),=C'** CLIENT TOTALS **'                                 
         GOTO1 VRPRT                                                            
         MVC   P,MYP1                                                           
         MVC   PSECOND,MYP2                                                     
         GOTO1 VRPRT                                                            
         MVC   P+10(5),=C'GROSS'                                                
         EDIT  (P8,CBGO),(14,P+18),2,MINUS=YES                                  
         EDIT  (P8,CBGP),(14,P+33),2,MINUS=YES                                  
         EDIT  (P8,CBGB),(14,P+48),2,MINUS=YES                                  
         SP    CBGO,CBGP                       PAYABLE                          
         EDIT  (P8,CBGO),(14,P+63),2,MINUS=YES                                  
         AP    CBGO,CBGP                                                        
         SP    CBGO,CBGB                       BILLABLE                         
         EDIT  (P8,CBGO),(14,P+78),2,MINUS=YES                                  
         AP    CBGO,CBGB                       RESTORE                          
*                                                                               
         GOTO1 VRPRT                                                            
         MVC   P+12(3),=C'NET'                                                  
         EDIT  (P8,CBNO),(14,P+18),2,MINUS=YES                                  
         EDIT  (P8,CBNP),(14,P+33),2,MINUS=YES                                  
         EDIT  (P8,CBNB),(14,P+48),2,MINUS=YES                                  
         SP    CBNO,CBNP                       PAYABLE                          
         EDIT  (P8,CBNO),(14,P+63),2,MINUS=YES                                  
         AP    CBNO,CBNP                                                        
         SP    CBNO,CBNB                       BILLABLE                         
         EDIT  (P8,CBNO),(14,P+78),2,MINUS=YES                                  
         AP    CBNO,CBNB                       RESTORE                          
*                                                                               
         GOTO1 VRPRT                                                            
         MVC   P+13(2),=C'CD'                                                   
         EDIT  (P8,CBCDO),(14,P+18),2,MINUS=YES                                 
         EDIT  (P8,CBCDP),(14,P+33),2,MINUS=YES                                 
         EDIT  (P8,CBCDB),(14,P+48),2,MINUS=YES                                 
         SP    CBCDO,CBCDP                     PAYABLE                          
         EDIT  (P8,CBCDO),(14,P+63),2,MINUS=YES                                 
         AP    CBCDO,CBCDP                                                      
         SP    CBCDO,CBCDB                     BILLABLE                         
         EDIT  (P8,CBCDO),(14,P+78),2,MINUS=YES                                 
         AP    CBCDO,CBCDB                     RESTORE                          
*                                                                               
         GOTO1 VRPRT                                                            
*                                                                               
CLTTOT8  ZAP   CBGO,=P'0'      CLIENT TOTALS GROSS ORDERED                      
         ZAP   CBGP,=P'0'                    GROSS PAID                         
         ZAP   CBGB,=P'0'                    GROSS BILLED                       
         ZAP   CBNO,=P'0'                      NET ORDERED                      
         ZAP   CBNP,=P'0'                      NET PAID                         
         ZAP   CBNB,=P'0'                      NET BILLED                       
         ZAP   CBCDO,=P'0'                      CD ORDERED                      
         ZAP   CBCDP,=P'0'                      CD PAID                         
         ZAP   CBCDB,=P'0'                      CD BILLED                       
*                                                                               
         BR    R9                                                               
*****                                                                           
         TITLE 'CLOSE ESTIMATE HEADERS'                                         
         DS    F                                                                
CLOSEST  ST    R9,*-4                                                           
         L     R4,VETAB                                                         
         USING ETABD,R4                                                         
CE1      CLI   0(R4),0             END OF ESTIMATES                             
         BE    CEEXIT               YES - EXIT                                  
         TM    ETSTAT,X'80'        ALREADY LOOKED AT                            
         BNZ   CE8                                                              
         TM    ETSTAT,X'04'        ABSOLUTELY NOT CLOSEABLE                     
         BNZ   CE8                                                              
         CLI   ETCOM,0                                                          
         BE    CE1A0                                                            
         CLI   QOPT1,C'F'          TEST FORCE CLOSE                             
         BNE   CE8                 NO                                           
CE1A0    DS    0H                                                               
         XC    PESTKEY,PESTKEY                                                  
         MVC   PESTKAGY,RCSVAGY                                                 
         MVC   PESTKMED,ETMED                                                   
         MVI   PESTKRCD,7                                                       
         MVC   PESTKCLT(8),ETCLT   MOVE REST OF KEY                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),PRTDIR,PESTKEY,PESTREC             
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    CE2                                                              
         MVI   ERROR,2                                                          
CE1A     GOTO1 VPROGERR                                                         
         LA    R4,ETLEN(R4)                                                     
         B     CE1                                                              
CE2      CLI   QOPT2,C'T'                                                       
         BE    CE3                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   CE3                                                              
         OI    PESTKEY+25,X'C0'                                                 
         GOTO1 DATAMGR,DMCB,DMWRT,PRTDIR,PESTREC,PESTREC                        
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+12                                                             
         MVI   ERROR,3                                                          
         B     CE1A                                                             
CE3      L     R5,RERCNT           ADD TO DELETED RECORD COUNTER                
         LA    R5,1(R5)                                                         
         ST    R5,RERCNT                                                        
         OI    ETSTAT,X'02'                                                     
*                                  CLOSE OUT BUCKETS                            
*                                                                               
         MVC   KEY(25),PESTKEY                                                  
         MVI   KEY+3,X'09'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   CE4                                                              
*                                                                               
         CLI   QOPT2,C'T'                                                       
         BE    CE4                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   CE4                                                              
*****                                                                           
CE3A     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),PRTDIR,KEY,IOAREA                  
*                                                                               
*        GOTO1 HEXOUT,DMCB,IOAREA,P+10,40,=C'N'                                 
*        GOTO1 VRPRT                                CLI  QOPTW,C'T'             
*                                                                               
         OI    IOAREA+25,X'C0'                      TO BRANCH HERE              
         CLI   QOPT2,C'T'                                                       
         BE    CE4                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   CE4                                                              
         GOTO1 DATAMGR,DMCB,DMWRT,PRTDIR,IOAREA,IOAREA                          
*                                                                               
*        GOTO1 HEXOUT,DMCB,IOAREA,P+10,40,=C'N'                                 
*        GOTO1 VRPRT                                                            
*****                                                                           
CE4      DS    0H                                                               
*                                                                               
*                 FIND AND CLOSE-OUT ESTIMATE UPLOAD RECORDS                    
         MVC   KEY(25),PESTKEY                                                  
         MVI   KEY+3,X'90'                                                      
         GOTO1 HIGH                                                             
         B     CE6                                                              
CE5      GOTO1 SEQ                                                              
*                                                                               
CE6      CLC   KEY(12),KEYSAVE   CHECK THROUGH EST                              
         BNE   CE8                                                              
*                                                                               
         CLI   QOPT2,C'T'                                                       
         BE    CE8                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   CE8                                                              
*****                                                                           
CE6A     DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),PRTDIR,KEY,IOAREA                  
*                                                                               
         GOTO1 HEXOUT,DMCB,IOAREA,P+10,40,=C'N'                                 
         GOTO1 VRPRT                                CLI  QOPTW,C'T'             
*                                                                               
         OI    IOAREA+25,X'C0'                      TO BRANCH HERE              
         CLI   QOPT2,C'T'                                                       
         BE    CE8                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   CE8                                                              
         GOTO1 DATAMGR,DMCB,DMWRT,PRTDIR,IOAREA,IOAREA                          
*                                                                               
         GOTO1 HEXOUT,DMCB,IOAREA,P+10,40,=C'N'                                 
         GOTO1 VRPRT                                                            
         B     CE5                 GO CHECK FOR MORE                            
*****                                                                           
*                                                                               
CE8      DS    0H                                                               
         OI    ETSTAT,X'80'        SET ALREADY LOOKED AT                        
         LA    R4,ETLEN(R4)        SET STATUS TO CLOSED                         
         B     CE1                                                              
CEEXIT   L     R9,CLOSEST-4                                                     
         BR    R9                                                               
         DROP  R4                                                               
         TITLE 'PRINT RECORD COUNTS FOR REQUEST'                                
PRECCNT  LA    R4,PSECOND                                                       
         USING ESRLINE,R4                                                       
         MVC   ESRCOM1,=C'ESTIMATE RECORDS DELETED'                             
         EDIT  RERCNT,(8,ESRCNT)                                                
         GOTO1 VRPRT                                                            
         MVC   ESRCOM1,=C'    BILL RECORDS DELETED'                             
         EDIT  RBLRCNT,(8,ESRCNT)                                               
         GOTO1 VRPRT                                                            
         EDIT  RBYRCNT,(8,ESRCNT)                                               
         MVC   ESRCOM1,=C'     BUY RECORDS DELETED'                             
         GOTO1 VRPRT                                                            
         LA    RE,ERCNT            ADD REQUEST RECORD COUNTS TO                 
         LA    RF,RERCNT            REPORT RECORD COUNTS                        
         LA    R5,3                                                             
*                                                                               
*  ROLL TOTALS FOR REPORT                                                       
*                                                                               
PRECCNT1 L     R6,0(RF)                                                         
         A     R6,0(RE)                                                         
         ST    R6,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R5,PRECCNT1                                                      
         XC    RRECCNT,RRECCNT                                                  
         BR    R9                                                               
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         DS    0D                                                               
IOAREA   DS    CL1000                                                           
         DROP  RC                                                               
         EJECT                                                                  
         TITLE 'CLEAR WORK AREA'                                                
CLEARW   CSECT                                                                  
         NMOD1 0,CLEARW                                                         
         LA    RE,PP92WORK                                                      
         L     RF,=A(MYEND-PP92WORK)                                            
         XCEF                                                                   
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,X'FD'                                                   
         ZAP   NGRSAMT,=P'0'                                                    
         ZAP   NNETAMT,=P'0'                                                    
*                                                                               
         ZAP   BLRAMTE,=P'0'     BLLETOT TOTALS                                 
         ZAP   BLRUNRE,=P'0'                                                    
         ZAP   BLRAMTT,=P'0'     BLLTOT  TOTALS                                 
         ZAP   BLRUNRT,=P'0'                                                    
*                                                                               
*                                                                               
         LA    RE,PBUYTOT                                                       
         LA    R0,21            PBUYTOT, BYTOTE, BYTOT                          
CLEARW5  ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   R0,CLEARW5                                                       
*                                                                               
         ZAP   CBGO,=P'0'      CLIENT TOTALS GROSS ORDERED                      
         ZAP   CBGP,=P'0'                    GROSS PAID                         
         ZAP   CBGB,=P'0'                    GROSS BILLED                       
         ZAP   CBNO,=P'0'                      NET ORDERED                      
         ZAP   CBNP,=P'0'                      NET PAID                         
         ZAP   CBNB,=P'0'                      NET BILLED                       
         ZAP   CBCDO,=P'0'                      CD ORDERED                      
         ZAP   CBCDP,=P'0'                      CD PAID                         
         ZAP   CBCDB,=P'0'                      CD BILLED                       
*                                                                               
*                                                                               
         ZAP   RBGO,=P'0'         RUN TOTALS GROSS ORDERED                      
         ZAP   RBGP,=P'0'                    GROSS PAID                         
         ZAP   RBGB,=P'0'                    GROSS BILLED                       
         ZAP   RBNO,=P'0'                      NET ORDERED                      
         ZAP   RBNP,=P'0'                      NET PAID                         
         ZAP   RBNB,=P'0'                      NET BILLED                       
         ZAP   RBCDO,=P'0'                      CD ORDERED                      
         ZAP   RBCDP,=P'0'                      CD PAID                         
         ZAP   RBCDB,=P'0'                      CD BILLED                       
*                                                                               
         L     R3,=V(LOGERR)                                                    
         L     R4,=V(SYSERR)                                                    
         L     R5,=V(ATABR)                                                     
         L     R6,=V(ATAB)                                                      
         STM   R3,R6,VLOGERR                                                    
         L     R3,=V(ETAB)                                                      
         L     R4,=V(MYWEND)                                                    
         L     R5,=V(PP92ES)                                                    
         L     R6,=V(PP92BYR)                                                   
         STM   R3,R6,VETAB                                                      
         L     R3,=V(PP92BLR)                                                   
         L     R4,=V(SETCLOS)                                                   
         L     R5,=V(SAVEST)                                                    
         L     R6,=V(PESTSUM)                                                   
         STM   R3,R6,VPP92BLR                                                   
         L     R3,=V(PAGYSUM)                                                   
         L     R4,=V(PROGERR)                                                   
         L     R5,=A(RPRT)                                                      
         L     R6,=V(BINSRCH)                                                   
         STM   R3,R6,VPAGYSUM                                                   
*                                                                               
         LA    R3,VLOGERR                                                       
         LA    R4,16                                                            
CLEARW1  L     R5,0(R3)            RELOCATE                                     
         A     R5,RELO                                                          
         ST    R5,0(R3)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,CLEARW1                                                       
         L     RE,VATABR                                                        
         L     RF,VMYWEND                                                       
         SR    RF,RE                                                            
         XCEF                                                                   
         MVC   PAGE,=H'1'                                                       
         MVC   MYP1,SPACES                                                      
         MVC   MYP2,SPACES                                                      
         MVC   MYP1+24(7),=C'ORDERED'                                           
         MVC   MYP1+42(4),=C'PAID'                                              
         MVC   MYP1+55(6),=C'BILLED'                                            
         MVC   MYP1+69(7),=C'PAYABLE'                                           
         MVC   MYP1+83(8),=C'BILLABLE'                                          
*                                                                               
         MVC   MYP2+24(7),=C'-------'                                           
         MVC   MYP2+42(4),=C'----'                                              
         MVC   MYP2+55(6),=C'------'                                            
         MVC   MYP2+69(7),=C'-------'                                           
         MVC   MYP2+83(8),=C'--------'                                          
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'FILTER OUT REQUIRED ESTIMATE - CHECK FOR POL'                   
PP92ES   CSECT                                                                  
         NMOD1 0,ESTFLTR                                                        
         LA    RE,ESTLST           CLEAR ESTIMATE LIST                          
         L     RF,=A(ESTLSTX-ESTLST)                                            
         XCEF                                                                   
         MVC   SAVK,KEY                                                         
         XC    KEY(160),KEY        CLEAR KEY AREA                               
         L     R3,PPFILEC                                                       
         USING PPFILED,R3                                                       
         XC    PESTKEY,PESTKEY     BUILD ESTIMATE KEY FROM REQUEST              
         MVC   PESTKAGY,QAGENCY                                                 
         MVC   PESTKMED,QMEDIA                                                  
         MVI   PESTKRCD,7                                                       
         MVC   PESTKCLT,PCLTKCLT                                                
         MVC   PESTKPRD,QPRODUCT                                                
         CLC   PESTKPRD,=C'ALL'    ALL PRODUCTS REQUESTED                       
         BNE   *+14                                                             
ESALLPRD XC    PESTKPRD,PESTKPRD    YES - FIND FIRST                            
         B     ESCNVES                                                          
         CLC   PESTKPRD,=C'ZZZ'    POL PRODUCT REQUESTED                        
         BE    ESALLPRD             YES - FIND FIRST                            
ESCNVES  CLC   QEST(3),=C'ALL'                                                  
         BE    *+14                                                             
         CLC   QEST,=C'   '                                                     
         BNE   CNVREQ                                                           
         MVC   BESTFRST(4),=X'0000FFFF'                                         
         B     ESRDHI                                                           
CNVREQ   PACK  DUB,QEST                                                         
         CVB   RE,DUB                                                           
         STH   RE,BESTFRST                                                      
         MVC   PESTKEST,BESTFRST                                                
         XC    BESTLAST,BESTLAST                                                
         CLC   QESTEND,=C'   '                                                  
         BE    ESRDHI                                                           
         PACK  DUB,QESTEND                                                      
         CVB   RE,DUB                                                           
         STH   RE,BESTLAST                                                      
*                                                                               
         TITLE 'SCAN FOR VALID ESTIMATES'                                       
ESRDHI   MVC   KEYSAVE(25),PESTKEY                                              
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),PRTDIR,KEYSAVE,KEY                 
         MVC   PESTKEY,KEY                                                      
         B     ESCHKKEY                                                         
ESRDSEQ  MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),PRTDIR,KEYSAVE,KEY                 
         MVC   PESTKEY,KEY                                                      
ESCHKKEY CLC   KEY(7),KEYSAVE      CHECK FOR VALID ESTIMATE                     
         BNE   ESEXIT              AG,MD,CLT NE - EXIT                          
         CLC   QPRODUCT,=C'ZZZ'    POL OR ALL - BYPASS PRODUCT TEST             
         BE    ESBYPPRD                                                         
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    ESBYPPRD                                                         
         CLC   PESTKPRD,QPRODUCT   PRODUCT OK                                   
         BNE   ESEXIT               NO - EXIT                                   
*                                                                               
ESBYPPRD CLC   PESTKEST,BESTFRST   CHECK ESTIMATE                               
         BL    ESRDSEQ                                                          
         BE    ESRDREC                                                          
         CLC   PESTKEST,BESTLAST                                                
         BH    ESRDSEQ                                                          
*                                                                               
ESRDREC  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),PRTFILE,KEY+27,PESTREC,   X        
               DMWORK                                                           
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   PESTEND,QSTART      CHECK ESTIMATE DATES                         
         BL    ESRDSEQ                                                          
         CLC   PESTST,QEND                                                      
         BH    ESRDSEQ                                                          
         CLC   PESTST,QSTART                                                    
         BL    ESRDSEQ                                                          
         CLC   PESTEND,QEND                                                     
         BH    ESRDSEQ                                                          
*                                                                               
*                                                                               
         CLC   QESTEND,SPACES     SEE IF USING EST FILTERS                      
         BE    FE20               NO                                            
         CLI   QEST,C'0'                                                        
         BNL   FE20               NO - QESTEND IS END OF SERIES                 
* FILTER ESTIMATE *                                                             
         LA    RE,QESTEND                                                       
         LA    RF,PESTGRPS                                                      
         LA    R0,3                                                             
*                                                                               
FE10     DS    0H                                                               
         CLI   0(RE),C'*'                                                       
         BE    FE14                                                             
         CLI   0(RE),C' '                                                       
         BE    FE14                                                             
*                                                                               
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    FE12                YES                                          
*                                                                               
         CLC   0(1,RE),0(RF)       POSITIVE FILTER                              
         BNE   FENO                                                             
         B     FE14                                                             
*                                                                               
FE12     DS    0H                                                               
         MVC   DUB(1),0(RE)                                                     
         OI    DUB,X'40'                                                        
         CLC   DUB(1),0(RF)        NEGATIVE FILTER                              
         BE    FENO                                                             
*                                                                               
FE14     DS    0H                                                               
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FE10                                                          
         B     FE20                                                             
*                                                                               
FENO     B    ESRDSEQ              ESTIMATE DOESN'T PASS FILTER                 
*                                                                               
FE20     DS    0H                                                               
         MVI   BYTE,1              IF PRODUCT REQUEST MAKE SURE THAT            
         CLC   QPRODUCT,=C'ALL'     NO POL ESTIMATE OPEN FOR THIS               
         BE    ESSAVES              ESTIMATE                                    
         CLC   QPRODUCT,=C'ZZZ'                                                 
         BE    ESSAVES                                                          
*                                                                               
         MVC   PESTKPRD,=C'ZZZ'                                                 
         MVC   KEYSAVE(25),PESTKEY                                              
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),PRTDIR,KEYSAVE,KEYSAVE             
         CLC   PESTKEY,KEYSAVE                                                  
         BNE   *+8                                                              
         MVI   BYTE,0              POOL REQUEST REQUIRED                        
         MVC   PESTKPRD,QPRODUCT                                                
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),PRTDIR,PESTKEY,KEY                 
*                                                                               
*        BUILD A LIST OF QUALIFYING ESTIMATES                                   
*                                                                               
ESSAVES  LA    RE,ESTLST                                                        
         LA    RF,ESTLST                                                        
         A     RF,=A(ESTLSTX-ESTLST)                                            
ESFOPEN  CR    RE,RF               FIND AN OPEN SLOT                            
         BL    *+12                                                             
         MVI   ERROR,1             ESTIMATE TABLE OVERFLOW                      
         B     ESEXIT                                                           
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,7(RE)                                                         
         B     ESFOPEN                                                          
         MVC   0(3,RE),PESTKPRD                                                 
         MVC   3(2,RE),PESTKEST                                                 
         MVC   5(1,RE),BYTE        X'01'=ESTIMATE IS CLOSABLE                   
         CLI   BYTE,0                                                           
         BNE   *+8                                                              
         MVI   6(RE),X'40'         POL - CANNOT CLOSE BY PRODUCT                
         B     ESRDSEQ                                                          
*                                                                               
ESEXIT   CLC   QPRODUCT,=C'ZZZ'                                                 
         BNE   ESEXIT1                                                          
         XC    DMWORK,DMWORK       FILTER OUT BRAND ESTIMATES                   
         LA    RE,ESTLST                                                        
         LA    RF,DMWORK                                                        
ESE1     CLI   0(RE),0                                                          
         BE    ESE2                                                             
         CLC   0(3,RE),=C'ZZZ'                                                  
         BE    *+12                                                             
         LA    RE,7(RE)                                                         
         B     ESE1                                                             
         OC    0(2,RF),0(RF)                                                    
         BZ    *+12                                                             
         LA    RF,2(RF)                                                         
         B    *-14                                                              
         MVC   0(2,RF),3(RE)                                                    
         LA    RE,7(RE)                                                         
         B     ESE1                                                             
ESE2     LA    RE,ESTLST                                                        
ESE3     LA    RF,DMWORK                                                        
         CLI   0(RE),0                                                          
         BE    ESEXIT1                                                          
ESE4     OC    0(2,RF),0(RF)                                                    
         BNZ   *+16                                                             
         MVI   0(RE),X'FF'                                                      
         LA    RE,7(RE)                                                         
         B     ESE3                                                             
         CLC   3(2,RE),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     ESE4                                                             
         LA    RE,7(RE)                                                         
         B     ESE3                                                             
ESEXIT1  XMOD1 1                                                                
         LTORG                                                                  
         TITLE 'PRINT BUY RECORD REPORT'                                        
PP92BYR  CSECT                                                                  
         NMOD1 0,PP92BYR                                                        
         LA    R4,P                                                             
         USING BYRLINE,R4                                                       
         L     R3,PPFILEC                                                       
         USING PPFILED,R3                                                       
         CLI   PREVPUB,X'FF'       PUBLICATION TOTALS ONLY                      
         BNE   BUYR1                                                            
         BAS   R9,PUBTOT                                                        
         XC    PREVPUB,PREVPUB                                                  
         B     BYREXX                                                           
*                                                                               
BUYR1    CLC   PREVPUB,PBUYKPUB    PUB = PREV                                   
         BE    BUYR1D                NO - DO PUB TOTALS                         
         BAS   R9,PUBTOT                                                        
         CLI   QOPT3,C'P'          SEE IF NOT SHOWING PUBS                      
         BE    BUYR1D              YES - SKIP NAME                              
         BAS   R9,PUBNAM                                                        
BUYR1D   DS    0H                                                               
         LA    R8,PBUYREC+33                                                    
BUYR1F   DS    0H                                                               
         CLI   0(R8),X'25'         LOOP THRU PAID ELEMENTS                      
         BE    BUYR1H              CHECK TO SEE IF PAID THIS MONTH              
         CLI   0(R8),0                                                          
         BE    BUYR1J                                                           
BUYR1G   DS    0H                                                               
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     BUYR1F                                                           
*                                                                               
BUYR1H   DS    0H                                                               
         CLC   2(2,R8),TODAY       TEST PAID THIS MONTH                         
         BNE   BUYR1G                                                           
         BAS   RE,PAYMERR7                                         L01          
*                                                                               
BUYR1J   DS    0H                                                               
BUYR2    CLI   PAGYPROF+14,C'1'    REFLECT CASH DISCOUNT                        
         BNE   BUYR3                YES                                         
         L     R5,PGROSS                                                        
         CVD   R5,DUB                                                           
         ZAP   ZPAID,DUB                                                        
         L     R5,GROSS                                                         
         S     R5,PGROSS                                                        
         CVD   R5,DUB                                                           
         ZAP   ZUNPAID,DUB                                                      
         L     R5,BGROSS                                                        
         CVD   R5,DUB                                                           
         ZAP   ZBLLED,DUB                                                       
         L     R5,GROSS                                                         
         S     R5,BGROSS                                                        
         CVD   R5,DUB                                                           
         ZAP   ZUNBLLED,DUB                                                     
         B     BUYR4                                                            
*                                                                               
BUYR3    DS    0H                                                               
         L     R5,PGROSS                                                        
         S     R5,PCSHDSC                                                       
         CVD   R5,DUB                                                           
         ZAP   ZPAID,DUB                                                        
         LCR   R5,R5                                                            
         A     R5,BLABLE                                                        
         CVD   R5,DUB                                                           
         ZAP   ZUNPAID,DUB                                                      
*                                                                               
         L     R5,BGROSS                                                        
         S     R5,BCSHDSC                                                       
         CVD   R5,DUB                                                           
         ZAP   ZBLLED,DUB                                                       
*                                                                               
         LCR   R5,R5                                                            
         A     R5,BLABLE                                                        
         CVD   R5,DUB                                                           
         ZAP   ZUNBLLED,DUB                                                     
*                                                                               
*                                                                               
BUYR4    DS    0H                                                               
         CLI   PBUYKPRD,C'*'       SEE IF OTHER AGENCY BUY                      
         BE    BUYR4C                                                           
         CLI   PBDBFD,C'T'         SEE IF TEST BUY                              
         BNE   BUYR4G                                                           
*                                                                               
BUYR4C   DS    0H                                                               
         ZAP   ZUNPAID,=P'0'       CLEAR SINCE THESE BUYS ARE NEVER             
         ZAP   ZUNBLLED,=P'0'      BILLED NOR PAID                              
*                                                                               
BUYR4G   DS    0H                                                               
         CLI   QOPT3,C'B'          SEE IF NOT SHOWING BUYS                      
         BE    BUYR5                                                            
         CLI   QOPT3,C'P'          OR NOT SHOWING PUBS                          
         BE    BUYR5                                                            
*                                                                               
         EDIT  GROSS,(15,BYRGO),2,COMMAS=YES,MINUS=YES                          
         EDIT  CSHDSC,(15,BYRCD),2,COMMAS=YES,MINUS=YES                         
         EDIT  BLABLE,(15,BYRGLCD),2,COMMAS=YES,MINUS=YES                       
         EDIT  ZPAID,(15,BYRP),2,COMMAS=YES,MINUS=YES                           
         EDIT  ZUNPAID,(15,BYRNP),2,COMMAS=YES,MINUS=YES                        
         EDIT  ZBLLED,(15,BYRB),2,COMMAS=YES,MINUS=YES                          
         EDIT  ZUNBLLED,(15,BYRNB),2,COMMAS=YES,MINUS=YES                       
         EDIT  PBUYKEST,(3,BYREST)                                              
         MVI   BYRELD,C'-'                                                      
         EDIT  PBUYKLIN,(3,BYRLIN),ALIGN=LEFT                                   
         GOTO1 DATCON,DUB,(3,PBUYKDAT),(5,BYRIDTE)                              
         CLI   PBDBFD,C'T'                                                      
         BNE   BUYR4J                                                           
         MVI   BYRIDTE+8,C'T'      TEST BUY                                     
         TM    PBDSTAT2,X'40'      STEWARD BUY ?                                
         BNO   BUYR4J              NO                                           
         MVI   BYRIDTE+8,C'S'                                                   
*                                                                               
BUYR4J   DS    0H                                                               
*                                                                               
         CLC   BYRP+15(2),=C'XX'   PREVIOUS ERROR                  L01          
         BE    BYREX1                                              L01          
*                                                                               
*  AT THIS POINT DETERMINE THAT THIS BUY IS TOTALLY PAID AND BILLED             
*                                                                               
BUYR5    DS    0H                                                               
         CLI   PBUYKPRD,C'*'       THESE BUYS ARE NEVER BILLED/PAID             
         BE    BUYR5C                                                           
*                                                                               
         CLI   PBDBFD,C'T'  TEST   THESE BUYS ARE NEVER BILLED/PAID             
         BNE   BUYR5F                                                           
*                                                                               
BUYR5C   DS    0H                                                               
         XC    GROSS(20),GROSS     ZERO ORDERED AMOUNTS                         
*                                                                               
BUYR5F   LA    R1,4                UNPAID INSERTIONS ERROR         L01          
         CLC   GROSS,PGROSS                                                     
         BNE   PAYMERRX                                                         
         CLC   AGYCOM,PAGYCOM      ORDERED COMMISSION VS PAID      L01          
         BNE   PAYMERRX            ERROR PROCESSING                L01          
         CLC   CSHDSC,PCSHDSC      CASH DIS VS. PAID CD            L01          
         BNE   PAYMERRX                                            L01          
         LA    R1,8                UNBILLED ERROR                  L01          
         CLC   GROSS,BGROSS                                                     
         BNE   PAYMERRX                                                         
         CLC   AGYCOM,BAGYCOM      VS. BILLED                      L01          
         BNE   PAYMERRX                                            L01          
         CLC   CSHDSC,BCSHDSC      CD ORDERED VS BILLED CD         L01          
         BE    BYREX1                                              L01          
PAYMERRX BAS   RE,PAYMERR                                          L01          
BYREX1   DS    0H                                                               
         CLI   QOPT3,C'B'          SEE IF NOT SHOWING BUYS                      
         BE    BUYR6                                                            
         CLI   QOPT3,C'P'          OR NOT SHOWING PUBS                          
         BE    BUYR6                                                            
*                                  THEN SKIP GOTO1 VRPRT                        
*                                                                               
         GOTO1 VRPRT                                                            
*                                                                               
*        ACCUMULATE TOTALS FOR BUYS                                             
*                                                                               
BUYR6    L     R5,GROSS            GROSS                                        
         CVD   R5,DUB                                                           
         AP    PBYRGO,DUB                                                       
*                                                                               
         L     R5,CSHDSC           CD                                           
         CVD   R5,DUB                                                           
         AP    PBYRCD,DUB                                                       
*                                                                               
         L     R5,BLABLE                                                        
         CVD   R5,DUB                                                           
         AP    PBYRGLCD,DUB        GROSS -CD                                    
*                                                                               
         AP    PBYRP,ZPAID         PAID                                         
         AP    PBYRNP,ZUNPAID      UNPAID                                       
         AP    PBYRB,ZBLLED        BILLED                                       
         AP    PBYRNB,ZUNBLLED     UNBILLED                                     
*                                                                               
         MVC   PREVPUB,PBUYKPUB                                                 
*                                                                               
*        ADD TOTALS TO APPROPORIATE AGENCY SUMMARY BUCKETS                      
*                                                                               
         MVC   WATCLT,PBUYKCLT                                                  
         MVC   WATMON,PBUYKDAT -----------> BY INSERT YYMM                      
         MVC   WATGOIM,=PL6'0' INITIALIZE                                       
         MVC   WATGOBM(42),WATGOIM        PACKED FIELDS                         
         L     R5,GROSS                                                         
         CVD   R5,DUB                                                           
         MVC   WATGOIM,DUB+2                                                    
         L     R5,ASNOR                                                         
         LA    R1,DMCB                                                          
         USING BSPARA,R1                                                        
****************                                                                
* LOOK FOR ENTRY FOR CLIENT AND INSERT MONTH                                    
* ------------------------------!!!!!!!!!!!!                                    
*   IF NOT FOUND ADD IT TO TABLE                                                
****************                                                                
         GOTO1 VBINSRCH,DMCB,(X'01',WATAB),VATABR,(R5),ATLEN,ATKLEN,   X        
               ATABRMAX                                                         
         CLI   BSPNF,1             FOUND                                        
         BE    BYR3                 NO - PUT BILL MONTH IN TABLE                
*   RECORD FOUND                                                                
         L     RF,BSPAREC                                                       
         USING ATABD,RF                                                         
         AP    ATGOIM,WATGOIM      ADD GROSS DOLLARS TO INSERT  MO              
         MVI   BSPNF,0             CLEAR RECORD FOUND                           
*                                                                               
BYR3     LA    R5,WATAB                                                         
         ST    R5,BSPAREC                                                       
         MVC   WATGOIM,=PL6'0'                                                  
         MVC   WATB,=PL6'0'                                                     
*                                                                               
         L     R5,GROSS                                                         
         CVD   R5,DUB                                                           
         MVC   WATGOBM,DUB+2       GROSS $ FOR BILLING MONTH                    
         MVC   WATBT,DUB+2         GROSS $ FOR ??                               
*                                                                               
         L     R5,BLABLE           BILLABLE                                     
         CVD   R5,DUB                                                           
         MVC   WATGLCD,DUB+2                                                    
*                                                                               
         CLI   PAGYPROF+14,C'1'    NO CD OPT                                    
         BE    *+10                                                             
         MVC   WATBT,DUB+2                                                      
*                                                                               
         L     R5,CSHDSC           CASH DISCOUNT                                
         CVD   R5,DUB                                                           
         MVC   WATCD,DUB+2                                                      
*                                                                               
         L     R6,PGROSS           TOTAL PAID                                   
         CLI   PAGYPROF+14,C'1'    NO CD OPT                                    
         BE    *+8                                                              
         S     R6,PCSHDSC                                                       
         CVD   R6,DUB                                                           
         MVC   WATP,DUB+2          TOTAL PAID                                   
*                                                                               
         L     R5,GROSS                                                         
         CLI   PAGYPROF+14,C'1'    NO CD OPT                                    
         BE    *+8                                                              
         S     R5,CSHDSC                                                        
         SR    R5,R6                                                            
         CVD   R5,DUB                                                           
         MVC   WATNP,DUB+2         PAYABLE                                      
*                                                                               
         MVC   WATMON,PBDBDATE     BILLABLE DATE                                
         L     R5,BSPNOR                                                        
****************                                                                
* LOOK FOR ENTRY FOR CLIENT AND BILLABLE MONTH                                  
* ------------------------------!!!!!!!!                                        
*     IF NOT FOUND ADD IT TO BILLABLE TABLE                                     
****************                                                                
         GOTO1 VBINSRCH,DMCB,(X'01',WATAB),VATABR,(R5)                          
         CLI   BSPNF,1             FOUND                                        
         BE    BYREX                NO - EXIT                                   
*                                                                               
         L     RF,BSPAREC          ADD TO TABLE ENTRY                           
         AP    ATGOBM,WATGOBM                                                   
         AP    ATCD,WATCD                                                       
         AP    ATGLCD,WATGLCD                                                   
         AP    ATP,WATP                                                         
         AP    ATNP,WATNP                                                       
         AP    ATBT,WATBT                                                       
*                                                                               
BYREX    MVC   ASNOR,BSPNOR                                                     
BYREXX   XMOD1 1                                                                
         EJECT                                                                  
*------------------------------------                                           
PAYMERR7 LA    R1,2                                                L01          
PAYMERR  DS    0H                                                               
         CH    R1,=H'2'                                            L01          
         BNE   *+14                                                L01          
         MVC   BYRP+15(2),=C'XX'        FLAG BUY                   L01          
         B     PAYMAA                                              L01          
         CH    R1,=H'4'            UNPAID                          L01          
         BNE   *+14                                                L01          
         MVC   BYRP+15(2),=C'PA'        FLAG BUY                   L01          
         B     PAYMAA                                              L01          
         CH    R1,=H'8'            BILLABLE                        L01          
         BNE   *+10                                                L01          
         MVC   BYRP+15(2),=C'BL'        FLAG BUY                   L10          
*                                                                               
PAYMAA   MVC   WORK(3),PBUYKPRD                                                 
         MVC   WORK+3(2),PBUYKEST                                               
*------------------------------------> AND ENTER ERROR IN ESTLST                
         LA    RF,ESTLST                                                        
PAYME2   DS    0H                                                               
         CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(5),0(RF)            GET PROPER PRODUCT/EST                  
         BE    *+12                                                             
         LA    RF,7(RF)                                                         
         B     PAYME2                                                           
         CH    R1,=H'2'            SEE IF PAID IN CURRENT MONTH                 
         BNE   *+8                                                              
         MVI   5(RF),X'04'         ABSOLUTELY NOT CLOSEABLE                     
         STC   R1,HALF                                                          
         OC    6(1,RF),HALF        OR ERRORS                                    
******** STC   R1,6(RF)                                            L01          
         BR    RE                                                               
         EJECT                                                                  
PUBTOT   OC    PREVPUB,PREVPUB                                                  
         BZR   R9                                                               
         CLI   QOPT3,C'P'          SEE IF NOT SHOWING PUBS                      
         BE    PUBTOT0             YES - SKIP                                   
         MVC   BYRIDTE(10),=C'*PUB TOTAL'                                       
******   CLC   PBYRGO,=F'10000'                                                 
******   BNE   *+6                                                              
******   DC    H'00'                                                            
         EDIT  PBYRGO,(15,BYRGO),2,COMMAS=YES,MINUS=YES                         
         EDIT  PBYRCD,(15,BYRCD),2,COMMAS=YES,MINUS=YES                         
         EDIT  PBYRGLCD,(15,BYRGLCD),2,COMMAS=YES,MINUS=YES                     
         EDIT  PBYRP,(15,BYRP),2,COMMAS=YES,MINUS=YES                           
         EDIT  PBYRNP,(15,BYRNP),2,COMMAS=YES,MINUS=YES                         
         EDIT  PBYRB,(15,BYRB),2,COMMAS=YES,MINUS=YES                           
         EDIT  PBYRNB,(15,BYRNB),2,COMMAS=YES,MINUS=YES                         
*                                                                               
*   ROLL PUB TOTALS (PBUYTOT) TO ESTIMATE BUY TOTALS (BYTOTE)                   
*                                                                               
PUBTOT0  LA    RE,BYTOTE                                                        
         LA    RF,PBUYTOT                                                       
         LA    R1,7                                                             
PUBTOT1  AP    0(8,RE),0(8,RF)                                                  
         ZAP   0(8,RF),=P'0'           CLEAR AFTER ROLL                         
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R1,PUBTOT1                                                       
*                                                                               
         CLI   QOPT3,C'P'          SEE IF NOT SHOWING PUBS                      
         BER   R9                  YES - SKIP                                   
         GOTO1 VRPRT                                                            
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
*        GET PUBLICATION NAME                                                   
PUBNAM   LA    R5,4095(R3)                                                      
         USING PPFILED,R3                                                       
         USING PPFILED+4095,R5                                                  
         LA    R4,PSECOND                                                       
         MVC   BYRPNAM,PUBNAME                                                  
         MVC   BYRPZON,PUBZNAME                                                 
         GOTO1 PUBEDIT,DMCB,PUBKPUB,BYRPNO                                      
         GOTO1 VRPRT                                                            
         LA    R4,P                                                             
         BR    R9                                                               
         LTORG                                                                  
         TITLE 'ESTIMATE CLOSEOUT BILL REPORT'                                  
PP92BLR  CSECT                                                                  
         NMOD1 0,PP92BLR                                                        
         L     R3,PPFILEC                                                       
         USING PPFILED,R3                                                       
         LA    R4,P                                                             
         USING BLRLINE,R4                                                       
         MVC   BLRPROD,PBILKPRD                                                 
         EDIT  PBILKEST,(3,BLREST)                                              
         CLI   PBILLTYP,C'0'           SEE IF NEW BILLING                       
         BNL   BLR1X                                                            
         MVC   BLRTYP(2),PBILLTYP                                               
         CLI   PBRETAIL,X'81'                                                   
         BNE   BLR1C                                                            
         MVC   BLRTYP+3(5),=C'RET-C'                                            
         B     BLR1X5                                                           
BLR1C    CLI   PBRETAIL,X'41'                                                   
         BNE   BLR1D                                                            
         MVC   BLRTYP+2(6),=C'*RET-S'                                           
         B     BLR1X5                                                           
BLR1D    TM    PBILCMSW,X'20'            AOR BILL                               
         BNO   BLR1X5                                                           
         MVC   BLRTYP+2(4),=C'*AOR'                                             
         B     BLR1X5                                                           
*                                                                               
BLR1X    MVC   BLRTYP,=C'ORIGINAL'                                              
         CLI   PBILLTYP,C'4'                                                    
         BNE   *+10                                                             
         MVC   BLRTYP,=C'DETAIL  '                                              
BLR1X5   MVC   WORK(2),PBILKMOS                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DUB,(3,WORK),(9,BLRMON)                                   
         EDIT  (B2,PBILKBNO),(4,BLRINO+2)                                       
         CLC   TODAY(2),PBILKBMN   BILL MONTH = CURRENT MONTH                   
         BNE   *+8                                                              
         B     BLRBMECM            YES-FLAG AS ERROR                            
BLR2     CLI   PAGYPROF+14,C'1'    REFLECT CASH DISCOUNT                        
         BE    *+10                                                             
         MVC   PBILLGRS,PBILLBIL    YES                                         
         EDIT  (P6,PBILLGRS),(15,BLRAMT),2,COMMAS=YES,MINUS=YES                 
         CLI   PBILLTYP,C'B'         NEW BILLING                                
         BE    BLR3                  CAN'T BE CANCELED                          
*                                                                               
         CLC   PBILLCAN,=C'000000'                                              
         BNE   BLR3                                                             
         CLI   PBILLTYP,C'4'                                                    
         BE    BLR3                                                             
         EDIT  (P6,PBILLGRS),(15,BLRUNR),2,COMMAS=YES,MINUS=YES                 
BLR3     GOTO1 VRPRT                                                            
*                                                                               
*        ADD REQUEST TOTALS                                                     
*                                                                               
         CLI   PBRETAIL,X'41'    DON'T TOTAL RETAILS SUMMARY BILLS              
         BE    BLR4                                                             
         TM    PBILCMSW,X'20'    DON'T TOTAL AOR BILLS                          
         BO    BLR4                                                             
*                                                                               
         AP    BLRAMTE,PBILLGRS                                                 
         CLI   PBILLTYP,C'B'           NEW BILLING                              
         BE    BLR3B                   CAN'T BE CANCELED                        
         CLC   PBILLCAN,=C'000000'                                              
         BNE   BLR3B                                                            
         CLI   PBILLTYP,C'4'                                                    
         BE    BLR3B                                                            
         AP    BLRUNRE,PBILLGRS                                                 
*                                                                               
BLR3B    DS    0H                                                               
*                                                                               
*        ADD BILLS TO AGENCY SUMMARY TABLE                                      
*                                                                               
         MVC   WATGOIM,=PL6'0'     CLEAR WORK AREA                              
         MVC   WATGOBM(42),WATGOIM                                              
         MVC   WATCLT,PBILKCLT                                                  
         MVC   WATMON,PBILKMOS                                                  
         ZAP   WATB,PBILLGRS                                                    
         L     R5,ASNOR                                                         
         LA    R1,DMCB                                                          
         USING BSPARA,R1                                                        
         GOTO1 VBINSRCH,DMCB,(X'01',WATAB),VATABR,(R5),ATLEN,ATKLEN,   X        
               ATABRMAX                                                         
         MVC   ASNOR,BSPNOR                                                     
         CLI   BSPNF,1             FOUND                                        
         BE    BLR4                 NO - EXIT                                   
         L     RF,BSPAREC           YES - ADD AMOUNT                            
         USING ATABD,RF                                                         
         AP    ATB,WATB                                                         
BLR4     XMOD1 1                                                                
*                                                                               
*        BILL MONTH = CURRENT SET ERROR                                         
*                                                                               
BLRBMECM MVC   BLRCOM(26),=C'BILL MONTH = CURRENT MONTH'                        
         MVC   WORK(5),PBILKPRD                                                 
         LA    RF,ESTLST                                                        
BMECM1   CLI   0(RF),0                                                          
         BNE   BMECM2                                                           
         MVI   ERROR,4                                                          
         GOTO1 VPROGERR                                                         
         B     BLR4                                                             
BMECM2   CLC   WORK(5),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,7(RF)                                                         
         B     BMECM1                                                           
         MVI   5(RF),X'04'         ABSOLUTELY NOT CLOSEABLE                     
         OI    6(RF),1             OR ERRORS (BILLED IN CURRENT MTH)            
         B     BLR2                                                             
         DROP  RF                                                               
         DROP  R3                                                               
         LTORG                                                                  
         TITLE 'CHECK TO SEE IF ENTIRE REQUEST IS CLOSEABLE'                    
SETCLOS  CSECT                                                                  
         NMOD1 0,SETCLOS                                                        
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
         MVI   NOCLOSE,C'N'                                                     
*                                                                               
         LA    RF,ESTLST                                                        
SC04     DS    0H                                                               
         CLI   0(RF),0                                                          
         BE    SC08                                                             
         TM    6(RF),X'03'         BILLED OR PAID IN CURRENT MONTH              
         BNZ   SC06                                                             
         CLI   6(RF),0             ANY OTHER ERROR                              
         BNE   SC07                                                             
SC05     LA    RF,7(RF)                                                         
         B     SC04                                                             
*                                                                               
SC06     MVI   NOCLOSE,C'Y'        CURRENT MONTH ERRORS - NO CLOSE              
****                               SETCLOS WILL SET X'04' ON FOR                
****                               ALL ESTIMATES                                
SC07     OC    ERROR,6(RF)                                                      
         B     SC05                                                             
*                                                                               
SC08     DS    0H                                                               
         CP    BYRPT,=P'0'         TEST ANY PAYMENTS                            
         BNE   SC09                YES THEN BILLING MUST MATCH ORDERED          
*                                  EVEN IF THERE WAS NO BILLING                 
         CP    BLRAMTT,=P'0'                                                    
         BE    SCA                                                              
SC09     LA    RF,BYRGOT           GROSS ORDERED                                
         CLI   PAGYPROF+14,C'1'    REFLECT CD                                   
         BE    *+8                 NO                                           
         LA    RF,BYRGLCDT         GROSS LESS CD                                
         CP    BLRAMTT,0(8,RF)       GROSS BILLED TO GROSS ORDERED              
         BE    SCA                                                              
         OI    ERROR,X'10'         BILLS NE ORDERED                             
SCA      DS    0H                                                               
         CP    BLRUNRT,=P'0'       ANY UNREVERSED?                              
         BE    SCA1                                                             
         CP    BLRUNRT,BLRAMTT     UNREVERSED AND BILLED                        
         BE    SCA1                                                             
         OI    ERROR,X'20'         UNREVERSED NE ZERO                           
SCA1     DS    0H                                                               
*                                                                               
**NO-OP  OC    BYRPT,BYRPT         ANY PAYMENTS                                 
**NO-OP  BZ    SC1                  NO                                          
*                                                                               
*  ROLL TOTALS FOR REPORT                                                       
*                                                                               
         CP    BYRNPT,=P'0'         YES - MUST ALL BE PAID                      
         BE    SC1                                                              
         OI    ERROR,X'04'          SOMETHING PAYABLE                           
*                                                                               
**NO-OP  OC    BYRBT,BYRBT   WAS SC1 TEST ANY BILLINGS                          
**NO-OP  BZ    SC3                  NO                                          
*                                                                               
SC1      CP    BYRNBT,=P'0'        YES - MUST ALL BE BILLED                     
         BE    SC1X                                                             
         OI    ERROR,X'08'         SOMETHING BILLABLE                           
*                                                                               
SC1X     DS    0H                                                               
         CLI   ERROR,0             SEE IF ANY ERROR FOUND                       
         BE    SC3                                                              
         B     SCSETERR            WILL SET ERROR FOR ALL ESTIMATES             
*                                  SINCE PROGRAM CAN ONLY CLOSE-OUT             
*                                  ALL OR NO ESTIMATES FOR THE REQUEST          
*                                                                               
SC3      LA    RF,ESTLST                                                        
SC4      CLI   0(RF),0                                                          
         BE    SC5                                                              
         CLI   6(RF),0                                                          
         BNE   *+12               ERROR FOUND                                   
         LA    RF,7(RF)                                                         
         B     SC4                                                              
         MVC   ERROR,6(RF)                                                      
         L     R6,VLOGERR                                                       
         B     SCERR1A                                                          
*                                                                               
SC5      MVI   ERROR,0             BUILD SEARCH PARAMETER LIST                  
         LA    R1,DMCB                                                          
         USING BSPARA,R1                                                        
         OC    WATCLT,WATCLT                                                    
         BZ    SC8                                                              
         L     R8,VATABR                                                        
         MVC   BSPNOR,ASNORT                                                    
SC6      MVC   WATAB,0(R8)                                                      
         L     R5,BSPNOR                                                        
         GOTO1 VBINSRCH,DMCB,(X'01',WATAB),VATAB,(R5),ATLEN,ATKLEN,    X        
               15000                                                            
*                                                                               
         MVC   ASNORT,BSPNOR                                                    
         CLI   BSPNF,1             NOT FOUND                                    
         BE    SC7                  DO NEXT                                     
         L     RF,BSPAREC                                                       
         USING ATABD,RF                                                         
         AP    ATGOIM,WATGOIM      FOUND - ADD                                  
         AP    ATGOBM,WATGOBM                                                   
         AP    ATCD,WATCD                                                       
         AP    ATGLCD,WATGLCD                                                   
         AP    ATP,WATP                                                         
         AP    ATNP,WATNP                                                       
         AP    ATB,WATB                                                         
         AP    ATBT,WATBT                                                       
SC7      LA    R8,ATLEN(R8)                                                     
         CLI   0(R8),0             END OF TABLE                                 
         BNE   SC6                  NO - NEXT                                   
SC7B     DS    0H                                                               
         L     RE,VATABR                                                        
         L     RF,=A(ATABRE-ATABRS)                                             
         XCEF                                                                   
         XC    ASNOR,ASNOR                                                      
SC8      XMOD1 1                                                                
         DROP  RF                                                               
         EJECT                                                                  
SCSETERR LA    RF,ESTLST                                                        
SCERR1   OC    6(1,RF),ERROR                                                    
         CLI   NOCLOSE,C'Y'                                                     
         BNE   *+8                                                              
         OI    5(RF),X'04'         SET ABSOLUTELY NOT CLOSEABLE                 
         LA    RF,7(RF)                                                         
         CLI   0(RF),0                                                          
         BNE   SCERR1                                                           
*                                                                               
         L     R6,VLOGERR                                                       
SCERR1A  MVC   HALF(1),0(R6)                                                    
         NC    HALF(1),ERROR                                                    
         BNZ   SCERR2                                                           
*                                                                               
         CLI   0(R6),0               END OF TABLE                               
         BE    SCERR3                                                           
SCERR1C  SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     SCERR1A                                                          
*                                                                               
SCERR2   MVI   P,C'*'                                                           
         MVC   P+1(120),P                                                       
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND(1),2(R6)                                                 
         GOTO1 VRPRT                                                            
         B     SCERR1C                 TRY FOR ANOTHER ERROR                    
*                                                                               
SCERR3   DS    0H                                                               
         B     SCERR4                                                           
******   MVI   ERROR,5                                                          
******   GOTO1 VPROGERR                                                         
SCERR4   DS    0H                                                               
         CLI   NOCLOSE,C'Y'                                                     
         BE    SC7B                                                             
         CLI   QOPT1,C'F'          TEST FORCE CLOSE                             
         BE    SC5                 YES                                          
         B     SC7B                                                             
         LTORG                                                                  
         DROP  RC                                                               
         TITLE 'TRANSFER EST. INTO EST. SUMMARY TABLE'                          
SAVEST   CSECT                                                                  
         NMOD1 0,SAVEST                                                         
         L     R3,PPFILEC                                                       
         USING PPFILED,R3                                                       
         CLI   ESTLST,0                                                         
         BE    SVE3                                                             
         L     RC,VETAB                                                         
         USING ETABD,RC                                                         
         LA    R1,ESTLST                                                        
         LR    RE,RC                                                            
         A     RE,=A(ETABE-ETABS)                                               
SVE1     DS    0H                                                               
         MVC   WORK(1),QMEDIA                                                   
         MVC   WORK+1(3),PCLTKCLT                                               
         MVC   WORK+4(ETLEN),0(R1)                                              
         CLI   0(R1),X'FF'                                                      
         BE    SVE2A                                                            
SVE2     CR    RC,RE                                                            
         BH    SVERR                                                            
         CLI   0(RC),0                                                          
         BE    *+12                                                             
         LA    RC,ETLEN(RC)                                                     
         B     SVE2                                                             
         MVC   0(ETLEN,RC),WORK                                                 
SVE2A    LA    R1,7(R1)                                                         
         L     RC,VETAB                                                         
         CLI   0(R1),0                                                          
         BNE   SVE1                                                             
SVE3     LA    RE,ESTLST                                                        
         L     RF,=A(ESTLSTX-ESTLST)                                            
         XCEF                                                                   
         XMOD1 1                                                                
SVERR    MVI   ERROR,6                                                          
         GOTO1 VPROGERR                                                         
         B     SVE3                                                             
         LTORG                                                                  
         DROP  R3                                                               
         TITLE 'PRINT ESTIMATE SUMMARY REPORT'                                  
PESTSUM  CSECT                                                                  
         NMOD1 0,PESTSUM                                                        
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         MVI   ERROR,0                                                          
         L     R3,VETAB                                                         
         USING ETABD,R3                                                         
         CLI   0(R3),0                                                          
         BE    PRTESTEX                                                         
         LA    R4,P                                                             
         USING ESRLINE,R4                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,30             FORCE ESTIMATE SUMMARY HEADINGS          
PRTEST   DS    0H                                                               
         XC    PESTKEY,PESTKEY                                                  
         MVC   PESTKAGY,PAGYKAGY                                                
         MVC   PESTKMED,ETMED                                                   
         MVI   PESTKRCD,7                                                       
         MVC   PESTKCLT,ETCLT                                                   
         MVC   PESTKPRD,ETPRD                                                   
         MVC   PESTKEST,ETEST                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),PRTDIR,PESTKEY,PESTKEY             
         TM    8(R1),X'FD'                                                      
         BZ    PRTEST1                                                          
         MVI   ERROR,2                                                          
         GOTO1 VPROGERR                                                         
         B     PRTESTP                                                          
PRTEST1  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),PRTFILE,PESTKEY+27,       X        
               PESTREC,DMWORK                                                   
         MVC   ESRPRD,ETPRD                                                     
         MVC   ESRCLT,ETCLT                                                     
         MVC   HALF,ETEST                                                       
         LH    RF,HALF                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESREST,DUB                                                       
         MVC   ESRNAM,PESTNAME                                                  
         GOTO1 DATCON,DMCB,(0,PESTST),(5,ESRSDTE)                               
         GOTO1 DATCON,DMCB,(0,PESTEND),(5,ESREDTE)                              
         TM    ETSTAT,X'01'                                                     
         BZ    *+10                                                             
         MVC   ESRSTAT(9),=C'CLOSEABLE'                                         
         TM    ETSTAT,X'02'                                                     
         BZ    *+10                                                             
         MVC   ESRSTAT(9),=CL9'CLOSED'                                          
         CLI   ETCOM,0                                                          
         BE    PRTESTP                                                          
         L     RF,VLOGERR                                                       
PGETEC   DS    0H                                                               
         CLI   0(RF),0          END OF TABLE                                    
         BE    PESTNXT                                                          
*                                                                               
         MVC   HALF(1),0(RF)                                                    
         NC    HALF(1),ETCOM                                                    
         BNZ   PESTCM                                                           
*                                                                               
PESTNERR SR    R1,R1                                                            
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     PGETEC                                                           
*                                                                               
PESTCM  SR    R1,R1                                                             
         IC    R1,1(RF)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ESRSTAT(1),2(RF)                                                 
PRTESTP  ST    RF,FULL                                                          
         GOTO1 VRPRT                                                            
         L     RF,FULL                                                          
         CLI   ERROR,2                                                          
         BE    PESTNXT                                                          
         CLI   ETCOM,0                                                          
         BE    PESTNXT                                                          
         B     PESTNERR                                                         
*                                                                               
PESTNXT  LA    R3,ETLEN(R3)                                                     
         CLI   0(R3),0                                                          
         BNE   PRTEST                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
*        PRINT RECORD COUNTS                                                    
*                                                                               
         LA    R4,PSECOND                                                       
         MVC   ESRCOM1,=C'ESTIMATE RECORDS DELETED'                             
         L     R5,ERCNT                                                         
         EDIT  (R5),(8,ESRCNT)                                                  
         GOTO1 VRPRT                                                            
         MVC   ESRCOM1,=C'    BILL RECORDS DELETED'                             
         L     R5,BLRCNT                                                        
         EDIT  (R5),(8,ESRCNT)                                                  
         GOTO1 VRPRT                                                            
         MVC   ESRCOM1,=C'     BUY RECORDS DELETED'                             
         L     R5,BYRCNT                                                        
         EDIT  (R5),(8,ESRCNT)                                                  
         GOTO1 VRPRT                                                            
         LA    RE,TERCNT           ADD  RECORD COUNTS TO                        
         LA    RF,ERCNT           RUN TOTAL COUNTS                              
         LA    R5,3                                                             
*                                                                               
*  ROLL TO TOTALS FOR RUN                                                       
*                                                                               
PEST25   L     R6,0(RF)                                                         
         A     R6,0(RE)                                                         
         ST    R6,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R5,PEST25                                                        
*                                                                               
         XC    RECCNT,RECCNT                                                    
*                                                                               
         LA    RE,ESTLST                                                        
         L     RF,=A(ESTLSTX-ESTLST)                                            
         XCEF                                                                   
         OC    SYSECNT,SYSECNT                                                  
         BZ    PRTESTEX                                                         
         EDIT  SYSECNT,(8,ESRCNT)                                               
         MVC   ESRCOM1,=C'     PROGRAM ERROR COUNT'                             
         GOTO1 VRPRT                                                            
PRTESTEX XMOD1 1                                                                
         LTORG                                                                  
         TITLE 'PRINT AGENCY SUMMARY REPORT FROM TABLE'                         
PAGYSUM  CSECT                                                                  
         NMOD1 0,PAGYSUM                                                        
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R3,VATAB                                                         
         USING ATABD,R3                                                         
         LA    R4,P                                                             
         USING ASRLINE,R4                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,40       FORCE AGENCY SUMMARY HEADINGS                  
         XC    PREVCLT,PREVCLT                                                  
         MVC   ATGOIMT,=PL8'0'                                                  
         MVC   ATGOBMT(56),ATGOIMT                                              
PASTAB   CLI   0(R3),0                                                          
         BE    ASEXIT                                                           
         CLC   PREVCLT,ATCLT                                                    
         BE    ASDET                                                            
ASTOT    OC    PREVCLT,PREVCLT     FIRST TIME                                   
         BZ    ASRDCLT              YES - READ CLIENT NAME                      
         MVC   ASRMON,=C'*TOTAL'                                                
         EDIT  (P8,ATGOIMT),(13,ASRIMO),2,MINUS=YES                             
         EDIT  (P8,ATGOBMT),(13,ASRBMO),2,MINUS=YES                             
         EDIT  (P8,ATCDT),(13,ASRCD),2,MINUS=YES                                
         EDIT  (P8,ATGLCDT),(13,ASRGLCD),2,MINUS=YES                            
         EDIT  (P8,ATPT),(13,ASRP),2,MINUS=YES                                  
         EDIT  (P8,ATNPT),(13,ASRNP),2,MINUS=YES                                
         EDIT  (P8,ATBT1),(13,ASRB),2,MINUS=YES                                 
         EDIT  (P8,ATNBT),(13,ASRNB),2,MINUS=YES                                
         GOTO1 VRPRT                                                            
         MVC   ATGOIMT,=PL8'0'                                                  
         MVC   ATGOBMT(56),ATGOIMT                                              
ASRDCLT  LA    R4,PSECOND                                                       
         CLI   0(R3),0                                                          
         BE    ASEXIT                                                           
         CLI   0(R3),X'FF'                                                      
         BNE   *+14                                                             
         MVC   ASRCNAME(13),=C'AGENCY TOTALS'                                   
         B     ASPCLT                                                           
         MVC   PCLTKAGY(3),PAGYKAGY                                             
         MVI   PCLTKRCD,2                                                       
         MVC   PCLTKCLT,ATCLT                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,PRTDIR,PCLTREC,PCLTREC,DMWORK                
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,PCLTREC+27,PCLTREC,DMWORK            
         MVC   ASRCCODE,PCLTKCLT                                                
         MVC   ASRCNAME,PCLTNAME                                                
ASPCLT   GOTO1 VRPRT                                                            
         LA    R4,P                                                             
         MVC   PREVCLT,ATCLT                                                    
         B     PASTAB                                                           
         EJECT                                                                  
ASDET    GOTO1 DATCON,DMCB,(3,ATMON),(9,ASRMON)                                 
         AP    ATGOIMT,ATGOIM                                                   
         EDIT  (P6,ATGOIM),(13,ASRIMO),2,MINUS=YES                              
         AP    ATGOBMT,ATGOBM                                                   
         EDIT  (P6,ATGOBM),(13,ASRBMO),2,MINUS=YES                              
         AP    ATCDT,ATCD                                                       
         EDIT  (P6,ATCD),(13,ASRCD),2,MINUS=YES                                 
         AP    ATGLCDT,ATGLCD                                                   
         EDIT  (P6,ATGLCD),(13,ASRGLCD),2,MINUS=YES                             
         AP    ATPT,ATP                                                         
         EDIT  (P6,ATP),(13,ASRP),2,MINUS=YES                                   
         AP    ATNPT,ATNP                                                       
         EDIT  (P6,ATNP),(13,ASRNP),2,MINUS=YES                                 
         AP    ATBT1,ATB                                                        
         EDIT  (P6,ATB),(13,ASRB),2,MINUS=YES                                   
         SP    ATBT,ATB                                                         
         AP    ATNBT,ATBT                                                       
         EDIT  (P6,ATBT),(13,ASRNB),2,MINUS=YES                                 
         AP    ATBT,ATB                                                         
         GOTO1 VRPRT                                                            
         CLC   ATCLT,=X'FFFFFF'                                                 
         BE    ASNXT                                                            
         EJECT                                                                  
*        ADD CLIENT TOTALS TO AGENCY TOTALS'                                    
*                                                                               
         LA    R1,DMCB                                                          
         USING BSPARA,R1                                                        
         MVC   WORK(ATLEN),ATCLT                                                
         MVC   WORK(3),=3X'FF'                                                  
         L     R5,ASNORT                                                        
         GOTO1 VBINSRCH,DMCB,(1,WORK),VATAB,(R5),ATLEN,ATKLEN,15000             
*                                                                               
         MVC   ASNORT,BSPNOR                                                    
         CLI   BSPNF,1             FOUND                                        
         BE    ASNXT                NO - NEXT                                   
         L     RF,BSPAREC           YES - ADD CURR REC INTO TABLE               
         MVC   WATAB,0(RF)                                                      
         AP    WATGOIM,ATGOIM                                                   
         AP    WATGOBM,ATGOBM                                                   
         AP    WATCD,ATCD                                                       
         AP    WATGLCD,ATGLCD                                                   
         AP    WATP,ATP                                                         
         AP    WATNP,ATNP                                                       
         AP    WATB,ATB                                                         
         AP    WATBT,ATBT                                                       
         MVC   0(ATLEN,RF),WATAB                                                
ASNXT    LA    R3,ATLEN(R3)                                                     
         CLI   0(R3),0                                                          
         BE    ASTOT                                                            
         B     PASTAB                                                           
ASEXIT   XC    ASNORT,ASNORT                                                    
         L     RE,VATAB                                                         
         L     RF,=A(ATABE-ATABS)                                               
         XCEF                                                                   
         L     RE,VETAB                                                         
         L     RF,=A(ETABE-ETABS)                                               
         XCEF                                                                   
         XMOD1 1                                                                
         LTORG                                                                  
         TITLE 'PRINT PROGRAM ERROR MESSAGE'                                    
PROGERR  CSECT                                                                  
         NMOD1 0,PROGERR                                                        
         L     R3,VSYSERR                                                       
         ST    R3,FULL                                                          
PROGERR1 CLC   0(1,R3),ERROR                                                    
         BE    PROGERR3                                                         
         CLI   0(R3),0                                                          
         BE    PROGERR2                                                         
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     PROGERR1                                                         
PROGERR2 EDIT  ERROR,(3,PSECOND)                                                
         MVC   PSECOND+4(23),=C'ERROR MESSAGE NOT FOUND'                        
         B     PROGERR4                                                         
PROGERR3 EDIT  ERROR,(3,PSECOND)                                                
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+4(1),2(R3)                                               
PROGERR4 MVI   P,C'*'                                                           
         MVC   P+1(120),P                                                       
         GOTO1 VRPRT                                                            
         L     R1,SYSECNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,SYSECNT                                                       
         DC    H'0'                                                             
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
SYSERR   CSECT                     SYSTEM ERRORS                                
         DC    AL1(1),AL1(37)                                                   
         DC    C'ESTIMATE TABLE OVERFLOW FOR REQUEST'                           
         DC    AL1(2),AL1(32)                                                   
         DC    C'ESTIMATE IN TABLE BUT NOT READ'                                
         DC    AL1(3),AL1(24)                                                   
         DC    C'ERROR AFTER WRITE -EST'                                        
         DC    AL1(4),AL1(30)                                                   
         DC    C'EST NOT IN TABLE-BILL REPORT'                                  
         DC    AL1(5),AL1(25)                                                   
         DC    C'ERROR MESSAGE NOT FOUND'                                       
         DC    AL1(6),AL1(29)                                                   
         DC    C'ESTIMATE SAVE AREA OVERFLOW'                                   
         DC    50X'00'                                                          
         EJECT                                                                  
LOGERR   CSECT                     ESTIMATE COMMENTS(LOGICAL ERROR)             
         DC    X'01',AL1(28)                                                    
         DC    C'BILL MONTH = CURRENT MONTH'                                    
         DC    X'02',AL1(26)                                                    
         DC    C'PAY DATE = CURRENT MONTH'                                      
         DC    X'04',AL1(30)                                                    
         DC    C'UNPAID INSERTIONS FOR CLIENT'                                  
         DC    X'08',AL1(32)                                                    
         DC    C'UNBILLED INSERTIONS FOR CLIENT'                                
         DC    X'10',AL1(39)                                                    
         DC    C'BILLS NOT EQUAL TO ORDERED FOR CLIENT'                         
         DC    X'20',AL1(37)                                                    
         DC    C'UNREVERSED AMOUNT NOT EQUAL TO ZERO'                           
         DC    X'40',AL1(43)                                                    
         DC    C'ZZZ ESTIMATE OPEN-CANNOT CLOSE BY PRODUCT'                     
         DC    50X'00'                                                          
         EJECT                                                                  
*                                  LINK TO REPORT                               
         SPACE 2                                                                
RPRT     CSECT                                                                  
         NMOD1 0,RPRT                                                           
*                                                                               
*                                                                               
         MVC   HEAD4+50(25),=C'TEST RUN- FILE NOT MARKED'                       
         CLI   RCWRITE,C'Y'                                                     
         BNE   RPRT2                                                            
         CLI   QOPT2,C'T'                                                       
         BE    RPRT2                                                            
         MVC   HEAD4+50(25),=C'LIVE RUN - FILE MARKED   '                       
RPRT2    DS    0H                                                               
         CLI   MODE,RUNLAST                                                     
         BE    RPRTX                                                            
         CLI   QOPT1,C'F'                                                       
         BNE   RPRTX                                                            
         MVC   HEAD6+50(22),=C'** CLOSE-OUT FORCED **'                          
*                                                                               
RPRTX    GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
         PRINT ON                                                               
PP92WORK DSECT                                                                  
VLOGERR  DS    V                                                                
VSYSERR  DS    V                                                                
VATABR   DS    V                                                                
VATAB    DS    V                                                                
VETAB    DS    V                                                                
VMYWEND  DS    V                                                                
VPP92ES  DS    V                                                                
VPP92BYR DS    V                                                                
VPP92BLR DS    V                                                                
VSETCLOS DS    V                                                                
VSAVEST  DS    V                                                                
VPESTSUM DS    V                                                                
VPAGYSUM DS    V                                                                
VPROGERR DS    V                                                                
VRPRT    DS    V                                                                
VBINSRCH DS    V                                                                
RELO     DS    F                                                                
*******                   *****                                                 
NGRSAMT  DS    D                                                                
NNETAMT  DS    D                                                                
*******                   *****                                                 
*********                         CLOSE-OUT CLIENT TOTALS FROM BUYS             
CBGO     DS    PL8'0'                        GROSS ORDERED                      
CBGP     DS    PL8'0'                        GROSS PAID                         
CBGB     DS    PL8'0'                        GROSS BILLED                       
CBNO     DS    PL8'0'                          NET ORDERED                      
CBNP     DS    PL8'0'                          NET PAID                         
CBNB     DS    PL8'0'                          NET BILLED                       
CBCDO    DS    PL8'0'                           CD ORDERED                      
CBCDP    DS    PL8'0'                           CD PAID                         
CBCDB    DS    PL8'0'                           CD BILLED                       
*                                                                               
*********                         CLOSE-OUT RUN TOTALS FROM BUYS                
RBGO     DS    PL8'0'                        GROSS ORDERED                      
RBGP     DS    PL8'0'                        GROSS PAID                         
RBGB     DS    PL8'0'                        GROSS BILLED                       
RBNO     DS    PL8'0'                          NET ORDERED                      
RBNP     DS    PL8'0'                          NET PAID                         
RBNB     DS    PL8'0'                          NET BILLED                       
RBCDO    DS    PL8'0'                           CD ORDERED                      
RBCDP    DS    PL8'0'                           CD PAID                         
RBCDB    DS    PL8'0'                           CD BILLED                       
*                                                                               
RRECCNT  DS    0CL12               REQUEST RECORD COUNTERS                      
RERCNT   DS    F                    ESTIMATE                                    
RBLRCNT  DS    F                    BILL                                        
RBYRCNT  DS    F                    BUY                                         
RECCNT   DS    0CL12               REPORT RECORD COUNTERS                       
ERCNT    DS    F                    ESTIMATE                                    
BLRCNT   DS    F                    BILL                                        
BYRCNT   DS    F                    BUY                                         
TRECCNT  DS    0CL12           RUN TOTAL RECORD COUNTERS                        
TERCNT   DS    F                    ESTIMATE                                    
TBLRCNT  DS    F                    BILL                                        
TBYRCNT  DS    F                    BUY                                         
BESTFRST DS    H                   FIRST POSSIBLE ESTIMATE                      
BESTLAST DS    H                   LAST POSSIBLE ESTIMATE                       
*                                                                               
BLLETOT  DS    0F                  ESTIMATE BILL TOTALS                         
BLRAMTE  DS    PL8                  BILL AMOUNT                                 
BLRUNRE  DS    PL8                  UNREVERSED AMOUNT                           
*                                                                               
BLLTOT   DS    0F                  REQUEST BILL TOTALS                          
BLRAMTT  DS    PL8                  BILL AMOUNT                                 
BLRUNRT  DS    PL8                  UNREVERSED AMOUNT                           
*                                                                               
PBUYTOT  DS    0F                  PUB PUB REPORT TOTALS                        
PBYRGO   DS    PL8                  GROSS ORDERED                               
PBYRCD   DS    PL8                  CASH DISCOUNT                               
PBYRGLCD DS    PL8                  GROSS-CASH DISCOUNT                         
PBYRP    DS    PL8                  PAID                                        
PBYRNP   DS    PL8                  NOT PAID                                    
PBYRB    DS    PL8                  BILLED                                      
PBYRNB   DS    PL8                  NOT BILLED                                  
*                                                                               
BYTOTE   DS    0F                  ESTIMATE BUY TOTALS                          
BYRGOE   DS    PL8                  GROSS ORDERED                               
BYRCDE   DS    PL8                  CASH DISCOUNT                               
BYRGLCDE DS    PL8                  GROSS-CASH DISCOUNT                         
BYRPE    DS    PL8                  PAID                                        
BYRNPE   DS    PL8                  NOT PAID                                    
BYRBE    DS    PL8                  BILLED                                      
BYRNBE   DS    PL8                  NOT BILLED                                  
*                                                                               
BYTOT    DS    0F                  BUY REPORT TOTALS                            
BYRGOT   DS    PL8                  GROSS ORDERED                               
BYRCDT   DS    PL8                  CASH DISCOUNT                               
BYRGLCDT DS    PL8                  GROSS-CASH DISCOUNT                         
BYRPT    DS    PL8                  PAID                                        
BYRNPT   DS    PL8                  NOT PAID                                    
BYRBT    DS    PL8                  BILLED                                      
BYRNBT   DS    PL8                  NOT BILLED                                  
*                                                                               
ASNOR    DS    F                   NO. REC. IN REQ. AGY SUM TABLE               
ASNORT   DS    F                   NO. REC. IN REPORT AGY SUM TABLE             
*                                                                               
ZPAID    DS    PL8                                                              
ZUNPAID  DS    PL8                                                              
ZBLLED   DS    PL8                                                              
ZUNBLLED DS    PL8                                                              
*                                                                               
SYSECNT  DS    F                   SYSTEM ERROR COUNT                           
ERROR    DS    C                   ERROR CODE                                   
PASS     DS    C                   PROGRAM PASS                                 
*                                                                               
CLTACT   DS    CL1                 GETS SET TO "N" AT FBUYCLI                   
*                                  GETS SET TO "Y" AT PROCBIL                   
*                                  AND PROCBUY                                  
*                                  IS CHECKED AT BUYTOT                         
NOCLOSE  DS    CL1                                                              
PREVPUB  DS    CL6                 PREVIOUS PUBLICATION                         
WATAB    DS    0CL53               AGENCY SUMMARY WORK RECORD                   
WATCLT   DS    CL3                                                              
WATMON   DS    CL2                                                              
WATGOIM  DS    CL6                                                              
WATGOBM  DS    CL6                                                              
WATCD    DS    CL6                                                              
WATGLCD  DS    CL6                                                              
WATP     DS    CL6                                                              
WATNP    DS    CL6                                                              
WATB     DS    CL6                                                              
WATBT    DS    CL6                                                              
TODAY    DS    CL3                 TODAYS DATE                                  
TODAYC   DS    CL6                                                              
PREVCLT  DS    CL3                 PREVIOUS CLIENT                              
SAVAGYR  DS    0CL250                                                           
SAVAGM   DS    CL3                                                              
         DS    CL247                                                            
ATGOIMT  DS    CL8                 AGENCY SUMMARY TOTAL COUNTERS                
ATGOBMT  DS    CL8                                                              
ATCDT    DS    CL8                                                              
ATGLCDT  DS    CL8                                                              
ATPT     DS    CL8                                                              
ATNPT    DS    CL8                                                              
ATBT1    DS    CL8                                                              
ATNBT    DS    CL8                                                              
*                                                                               
MYP1     DS    CL132                                                            
MYP2     DS    CL132                                                            
*                                                                               
SAVK     DS    CL160               SAVE KEY,KEYSAVE,DMWORK                      
**                                                                              
*-------->  TABLE OF ESTIMATES THAT MEET CLOSEOUT REQUEST PARAMETERS            
**                                                                              
AESTLST  EQU   *                                                                
ESTLST   DS    140000C              REQ ESTIMATE LIST                           
EESTLST  EQU   *       --> 20,000 ESTIMATES (PER CLIENT)                        
*                                   0-2 = PRODUCT                               
*                                   3-4 = ESTIMATE                              
*                                   5   = X'01'=CLOSEABLE X'02'=CLOSED          
*                                  X'04' = ABSOLUTELY NOT TO BE CLOSED          
*                                   6   = COMMENT NUMBER                        
************************                                                        
**************** NEW VALUES OR'ED TOGETHER                                      
************************                                                        
*                                       X'01' = BILLED IN CURRENT MTH           
*                                       X'02' = PIAD IN CUURENT MTH             
*                                       X'04' = UNPAID INSERTIONS               
*                                       X'08' = UNBILLED INSERTIONS             
*                                                                               
*                                       X'10' = ORDERED NOT EQU BILLS           
*                                       X'20' = UNREVERSED BILLS                
*********************************                                               
*                                       X'02 = BILLED IN CURRENT MONTH          
*                                       X'03' = ORDERED NOT EQ BILLS            
*                                       X'04' = UNREVERSED BILLS                
*                                       X'05' = UNBILLED INSERTIONS             
*                                               OR NON-ZERO UNBILLED            
*                                       X'06' = UNPAID INSERTIONS               
*                                               OR NON-ZERO UNPAID              
*                                       X'07 = PAID IN CURRENT MONTH            
*** NOTE : IF COMMENT = X'02' OR X'07' SETCLOS WILL SET ABSOLUTELY              
***        NOT TO BE CLOSED (X'04') ON IN 5TH BYTE                              
*                                                                               
*                                                                               
         ORG   ESTLST                                                           
ESTLSTPR DS    CL3                 EST PRODUCT                                  
ESTLSTES DS    CL2                  ESTIMATE NUMBER                             
ESTLSTCL DS    XL1                 ESTMATE STATUS                               
ESTLSTCM DS    XL1                 COMMENT FOR EST(X'01' POL EST CANNOT         
         ORG                                                                    
ESTLSTX  DS    X                                                                
ESTENTLN EQU   L'ESTLSTPR+L'ESTLSTES+L'ESTLSTCL+L'ESTLSTCM                      
ESTNENTR EQU   (EESTLST-AESTLST)/ESTENTLN                                       
MYEND    DS    0C                                                               
         SPACE 3                                                                
**                                                                              
*-------->  TABLE OF AGENCY SUMMARY DATA                                        
**                                                                              
ATKLEN   EQU   5                   AGENCY TABLE KEY LENGTH                      
ATLEN    EQU   53                  AGENCY TABLE ENTRY LENGTH                    
ATABR    CSECT     -------> 1500 ENTRIES ENTRY LEN =53                          
ATABRS   EQU   *   -*-*-*-> SEE DSECT "ATABD"                                   
         DS    79500C                                                           
ATABRE   EQU   *                                                                
ATABRMAX EQU   (ATABRE-ATABRS)/ATLEN                                            
         SPACE 3                                                                
**                                                                              
*-------->  AGENCY SUMMARY TABLE                                                
**                                                                              
ATAB     CSECT                                                                  
ATABS    EQU   *                                                                
         DS    795000C            15000 CLT/MTHS  X 53 BYTES EACH               
ATABE    EQU   *                                                                
ATABMAX  EQU   (ATABE-ATABS)/ATLEN                                              
         SPACE 3                                                                
**                                                                              
*-------->  ESTIMATE SUMMARY TABLE                                              
**                                                                              
ETAB     CSECT          -------->ENTRY LENGTH 11  ROOM FOR 50,000               
ETABS    EQU   *        --------> CONSISTS OF MEDIA(1) CLT(3) PLUS              
         DS    550000C            ENTRY FROM ESTLST (PRD(3) EST(2)              
ETABE    EQU   *                  STATUS(1) AND COMMENT #(1)                    
*                                                                               
MYWEND   CSECT                                                                  
         DS    C                                                                
         SPACE 3                                                                
**                                                                              
*-------->  TABLE OF                                                            
**                                                                              
PP92WKC  CSECT                                                                  
         DC    144000X'00'                                                      
*                                                                               
ETABD    DSECT                     ESTIMATE TABLE LAYOUT                        
ETMED    DS    CL1                 MEDIA                                        
ETCLT    DS    CL3                 CLIENT                                       
ETPRD    DS    CL3                 PRODUCT                                      
ETEST    DS    CL2                 ESTIMATE NUMBER                              
ETSTAT   DS    CL1                 ESTIMATE STATUS 1=CLOSABLE 2=CLOSED          
*                                  4 = ABSOLUTELY NOT TO BE CLOSED              
ETCOM    DS    CL1                 COMMENT NUMBER                               
ETKLEN   EQU   9                   EST. TABLE KEY LENGTH                        
ETLEN    EQU   11                  EST. TABLE ENTRY LENGTH                      
*                                                                               
*        AGENCY SUMMARY TABLE LAYOUT                                            
ATABD    DSECT                                                                  
ATKEY    DS    0CL5                                                             
ATCLT    DS    CL3                 CLIENT                                       
ATMON    DS    CL2            YEAR MONTH (FOR INSERT DATE) (BILLED)             
ATGOIM   DS    CL6                 GROSS ORDERED INSERT MO.                     
ATGOBM   DS    CL6                 GROSS ORDERED BILL MO.                       
ATCD     DS    CL6                 CASH DISCOUNT                                
ATGLCD   DS    CL6                 GROSS LESS CASH DISCOUNT                     
ATP      DS    CL6                 PAID                                         
ATNP     DS    CL6                 NOT PAID                                     
ATB      DS    CL6                 BILLED                                       
ATBT     DS    CL6                 BILLABLE TOTAL                               
*                                                                               
*        BILL REPORT DETAIL LINE                                                
*                                                                               
BLRLINE  DSECT                                                                  
         DS    CL2                                                              
BLRPROD  DS    CL3                 PRODUCT                                      
         DS    CL6                                                              
BLREST   DS    CL3                 ESTIMATE                                     
         DS    CL2                                                              
BLRMON   DS    CL6                 BILL MONTH                                   
         DS    CL3                                                              
BLRINO   DS    CL6                 INVOICE NUMBER                               
         DS    CL3                                                              
BLRAMT   DS    CL15                BILLED AMOUNT                                
         DS    CL3                                                              
BLRUNR   DS    CL15                UNREVERSED AMOUNT                            
         DS    CL2                                                              
BLRTYP   DS    CL8                 BILL TYPE                                    
         DS    CL2                                                              
BLRCOM   DS    CL20                COMMENTS                                     
*                                                                               
*        BUY REPORT DETAIL LINES                                                
*                                                                               
BYRLINE  DSECT                                                                  
         DS    CL1                                                              
BYRPNO   DS    CL17                PUB NUMBER                                   
         DS    CL1                                                              
BYRPNAM  DS    CL20                PUB NAME                                     
         DS    CL1                                                              
BYRPZON  DS    CL20                PUB ZONE                                     
         ORG   BYRPNO                                                           
BYRIDTE  DS    CL8                 INSERT DATE                                  
         DS    CL3                                                              
BYREST   DS    CL3                 ESTIMATE                                     
BYRELD   DS    CL1                 DASH                                         
BYRLIN   DS    CL3                 LINE                                         
BYRGO    DS    CL15                GROSS ORDERED                                
         DS    CL1                                                              
BYRCD    DS    CL15                CASH DISCOUNT                                
         DS    CL1                                                              
BYRGLCD  DS    CL15                GROSS LESS CASH DISCOUNT                     
         DS    CL2                                                              
BYRP     DS    CL15                PAID                                         
         DS    CL2                                                              
BYRNP    DS    CL15                NOT PAID                                     
         DS    CL1                                                              
BYRB     DS    CL15                BILLED                                       
         DS    CL1                                                              
BYRNB    DS    CL15                NOT BILLED                                   
*                                                                               
*        ESTIMATE SUMMARY DETAIL LINE                                           
*                                                                               
ESRLINE  DSECT                                                                  
         DS    CL2                                                              
ESRCOM1  DS    CL24                RECORD COUNT COMMENT                         
         DS    CL1                                                              
ESRCNT   DS    CL8                 RECORD COUNT                                 
         ORG   ESRCOM1                                                          
         DS    CL1                                                              
ESRCLT   DS    CL3                 CLIENT                                       
         DS    CL6                                                              
ESRPRD   DS    CL3                 PRODUCT                                      
         DS    CL6                                                              
ESREST   DS    CL3                 ESTIMATE NUMBER                              
         DS    CL5                                                              
ESRNAM   DS    CL20                ESTIMATE NAME                                
         DS    CL3                                                              
ESRSDTE  DS    CL8                 ESTIMATE START DATE                          
         DS    CL2                                                              
ESREDTE  DS    CL8                 ESTIMATE END DATE                            
         DS    CL3                                                              
ESRSTAT  DS    CL10                                                             
         DS    CL3                                                              
ESRCOM   DS    CL25                COMMENT                                      
*                                                                               
*        AGENCY SUMMARY DETAIL LINES                                            
ASRLINE  DSECT                                                                  
         DS    CL1                                                              
ASRCCODE DS    CL3                                                              
         DS    CL1                                                              
ASRCNAME DS    CL20                CLIENT NAME                                  
         ORG   ASRCCODE                                                         
ASRMON   DS    CL6                 MONTH                                        
         DS    CL3                                                              
ASRIMO   DS    CL13                GROSS ORDERED INSERT MONTHS                  
         DS    CL1                                                              
ASRBMO   DS    CL13                GROSS ORDERED BILL MONTHS                    
         DS    CL1                                                              
ASRCD    DS    CL13                CASH DISCOUNT                                
         DS    CL1                                                              
ASRGLCD  DS    CL13                GROSS LESS CASH DISCOUNT                     
         DS    CL1                                                              
ASRP     DS    CL13                PAID                                         
         DS    CL1                                                              
ASRNP    DS    CL13                UNPAID                                       
         DS    CL1                                                              
ASRB     DS    CL13                BILLED                                       
         DS    CL1                                                              
         DS    CL1                                                              
ASRNB    DS    CL13                                                             
       ++INCLUDE DDBSPARA                                                       
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE         HAVE NEW PBILLREC DSECT                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098PPREP9202 01/24/11'                                      
         END                                                                    
