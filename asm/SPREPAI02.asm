*          DATA SET SPREPAI02  AT LEVEL 007 AS OF 11/08/16                      
*PHASE SPAI02A                                                                  
*INCLUDE SPFMTINO                                                               
*INCLUDE GETUSER                                                                
*INCLUDE NETACC                                                                 
*INCLUDE NETNET                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE DDUCOM                                                                 
         TITLE 'SPREPAI02 - AT&&T INTERFACE'                                    
***********************************************************************         
*                                                                               
*QOPT6   Y=TESTING ONLY - DO NOT WRITE SORTED OUTPUT TO FILE                    
*QOPT7   P=DISPLAY OUTPUT RECORDS CHAR + HEX                                    
*                                                                               
***********************************************************************         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT CUSTENH-3314 07/06/16  IMPROVE AUDIT REPORT                    *         
* AKAT DSSUP-7578   06/08/16  FIX ESTIMATE UCOMM BUG                  *         
* AKAT CUSTENH-2964 04/14/16  FIX SORT COUNT BUG FOR NET              *         
* AKAT CUSTENH-2964 02/01/16  MODIFICATIONS FOR SPOT AND NET          *         
*                                                                     *         
***********************************************************************         
SPAI02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPAI02,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DS    F                                                                
*                                                                               
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R8,SPACEND                                                       
         USING SPAIWRKD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCBILL                                                    
         BE    PROCBL                                                           
         CLI   MODE,CLTFRST                                                     
         BE    CLTF                                                             
         CLI   MODE,CLTLAST                                                     
         BE    CLTL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         SPACE 2                                                                
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        RUNFRST                                                                
         SPACE 2                                                                
RUNF     DS    0H                                                               
*                                  RELOCATE ADDRESSES                           
         LA    R0,(ACONSX-ACONS)/4      NO. OF ADDRS                            
         LA    R2,ACONS                                                         
         LA    RE,RCONS                                                         
RUNF2    DS    0H                                                               
         L     RF,0(R2)                                                         
         A     RF,RELO                                                          
         ST    RF,0(RE)                                                         
         LA    R2,4(R2)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,RUNF2                                                         
*                                                                               
INIT5    MVI   NETOPT,C'N'         SET FOR NEW NETPAK                           
*                                                                               
         L     R1,VMASTC                                                        
         USING MASTD,R1                                                         
         CLI   MCNETPAK,C'Y'       SEE IF NETPAK                                
         BE    INIT6                                                            
         DROP  R1                                                               
*                                                                               
         MVI   NETOPT,0                                                         
         B     INIT7                                                            
*                                                                               
INIT6    DS    0H                                                               
*                                                                               
         CLI   NETOPT,C'N'         SEE IF DOING NEW NETWORK                     
         BNE   INIT7                                                            
         BC    0,INIT6X                                                         
         OI    *-3,X'F0'           ONLY DO ONCE                                 
         L     R4,ADBUY                                                         
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',NUFLIST,(R4)                      
         B     INIT6X                                                           
*                                                                               
NUFLIST  DC    CL8'NUNTFIL'                                                     
         DC    CL8'NUNTDIR'                                                     
         DC    CL10'X'                                                          
*                                                                               
INIT6X   DS    0H                                                               
*                                                                               
INIT7    DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(X'14',TODAYY)   YYYYMMDD                      
         MVC   MYTODAY(2),TODAYY+4    MTH                                       
         MVI   MYTODAY+2,C'/'                                                   
         MVC   MYTODAY+3(2),TODAYY+6  DAY                                       
         MVI   MYTODAY+5,C'/'                                                   
         MVC   MYTODAY+6(4),TODAYY    YEAR YYYY                                 
*                                                                               
         MVI   FRSTSW,C' '         CLEAR FIRST TIME SWITCH                      
         ZAP   TOTCNT,=P'0'                                                     
         LA    R2,HDRCNT           ZAP ACCUMS                                   
         LA    R3,4                                                             
INIT2    ZAP   0(4,R2),=P'0'                                                    
         LA    R2,4(R2)                                                         
         BCT   R3,INIT2                                                         
         ZAP   BILTOT,=P'0'        ZAP BILLING $ ACCUMULATOR                    
         ZAP   DTLTOT,=P'0'        ZAP DETAIL $ ACCUMULATOR                     
         B     EXIT                                                             
*                                                                               
REQF     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*****    MVI   FCRDACTV,C'Y'                                                    
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         MVC   SVQOPT6,QOPT6                                                    
         MVC   SVQOPT7,QOPT7                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         CLI   FRSTSW,C'Y'         FIRST TIME ?                                 
         BE    FBC10               NO                                           
*                                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD       INITIALIZE SORT              
*                                                                               
         CLI   QOPT6,C'Y'          TEST RUN ?                                   
         BE    FBC10               YES - NO OPEN                                
*                                                                               
         MVC   SPDYNDSN+13(L'QAGY),QAGY                                         
         GOTO1 DYNALLOC,DMCB,(0,=C'SAITAPE '),(0,SPDYNDSN)                      
         OPEN  (SAITAPE,OUTPUT)                                                 
*                                                                               
FBC10    DS    0H                                                               
         MVI   FRSTSW,C'Y'         SET FIRST TIME SWITCH                        
** CAN'T SET HERE                                                               
*******  MVC   ATTRECNT,=F'0'         SO ATT TABLE GETS REBUILT                 
*                                     FOR EACH REQUEST                          
*                                                                               
         CLC   =C'ALL',QEST                                                     
         BNE   *+10                                                             
         MVC   QEST(6),=C'001255'                                               
*                                                                               
         CLC   QEST(3),SPACES                                                   
         BNE   *+10                                                             
         MVC   QEST(6),=C'001255'                                               
*                                                                               
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         MVC   MYSTART,QSTART                                                   
         MVC   MYEND,QEND                                                       
         GOTO1 DATCON,DMCB,QSTART,(3,MYSTRTB)                                   
         GOTO1 DATCON,DMCB,QEND,(3,MYENDB)                                      
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QEND,(2,BQENDP)                                      
*                                                                               
         MVC   QSTART,SPACES                                                    
         MVC   QEND,SPACES                                                      
         XC    BQSTART,BQSTART                                                  
         MVC   BQEND,=X'FFFFFF'                                                 
*                                                                               
REQF20   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
PROCBL   GOTO1 APRBILL,DMCB,(RC)                                                
         B     EXIT                                                             
*        CLTFRST                                                                
CLTF     DS    0H                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SOB1'                                                 
         MVC   WORK+4(2),QAGY                                                   
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
*        USED TO CHECK FOR ANY OFFICE CODE                                      
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  RF                                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
         B     EXIT                                                             
*        CLT LAST                                                               
CLTL     DS    0H                                                               
         CLI   NETOPT,C'N'   NETPAK?                                            
         BE    CLTL5         YES - THEN GO READ UNITS                           
         MVC   REQPRD,BPRD   SAVE SPONSOR'S BPRD                                
         MVI   BPRD,0        CLEAR FOR RNBILL'S READING                         
         CLC   QPRD,=C'ALL'                                                     
         BE    CLTL2                                                            
         CLC   QPRD,=C'   '                                                     
         BE    CLTL2                                                            
         MVC   BPRD,REQPRD   RESTORE FOR SINGLE PRD                             
*                                                                               
CLTL2    GOTO1 ARNBILL,DMCB,(RC)   GO READ 0E01 BILLING BACK-UP RECS            
         B     CLTL10                                                           
*                                                                               
CLTL5    GOTO1 VPROCNET,DMCB,(RC)  UNIT PROCESSING                              
*                                                                               
CLTL10   DS    0H             FILE CREATED AT RUNLAST                           
         B     EXIT                                                             
*                                                                               
RUNL     DS    0H                  OUTPUT CONTROL RECORD                        
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
         MVI   FIRSTNET,C'Y'       INIT FIRST NET REC FLAG                      
         XC    SVBLINE,SVBLINE     CLEAR SVBLINE                                
*                                                                               
         LA    RE,SVCMADMA         CLEAR SVCMADMA                               
         LH    RF,=AL2(MAXCMA*3)   LENGTH OF SVCMADMA                           
         XCEF                                                                   
*                                                                               
FINLUP   GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   R5,15,4(R1)                                                      
         BZ    FIN50X              NO MORE RECS - FINISH UP                     
         CLI   NETOPT,C'N'         NETWORK?                                     
         BE    *+10                YES - WE COUNT RECORDS LATER                 
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
         MVC   SORTREC,0(R5)       MOVE SORTED BACK TO SORTREC                  
         CLC   LASTINV,SORTREC     SEE IF NEW INVOICE #                         
         BNE   FIN5                MED/CLT/PRD/INV#                             
**NO-OPT AP    SEQNO,=P'1'        LEAVE AS 1 FOR ALL                            
         B     FIN10                                                            
*                                                                               
FIN5     ZAP   SEQNO,=P'1'         RESET TO ONE                                 
         AP    INVCNT,=P'1'       COUNTING INVOICES                             
         MVC   LASTINV,SORTREC     MED/CLT/PRD/INV#                             
*                                                                               
FIN10    LA    RE,OUTREC           CLEAR OUTREC                                 
         LH    RF,=H'400'                                                       
         XCEF                                                                   
*                                                                               
         LA    R5,SORTREC                                                       
         USING SORTRECD,R5                                                      
*                                                                               
         CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   FIN10A              NO - REPORT SPOT AS-IS                       
         CLI   FIRSTNET,C'Y'       PROCESSING FIRST SORT RECORD?                
         BNE   FIN10AAA            NO                                           
         MVC   SORTKYSV,SORTKEY    YES - SET SORTKYSV FIRST TIME IN             
         MVC   SORTRCSV,SORTREC    SAVE THE SORT RECORD                         
         MVI   LASTNET,C'N'        INIT LAST NET REC FLAG                       
         MVI   FIRSTNET,C'N'       PROCESSED FIRST NET SORT RECORD              
*                                                                               
FIN10AAA CLC   SORTKYSV,SORTKEY    SAME SORT KEY AS LAST?                       
         BNE   FIN10AA             NO - REPORT                                  
         ICM   R1,15,SORTBNET      BILLED NET FROM THIS SORT REC                
         ICM   R2,15,SAVEBNET      RUNNING TOTAL BILLED NET FOR KEY             
         AR    R2,R1               ADD THIS ENTRY                               
         STCM  R2,15,SAVEBNET      NEW RUNNING TOTAL                            
         B     FINLUP              GET NEXT SORT REC                            
*                                                                               
FIN10AA  LA    R5,SORTRCSV         REPORT THE SAVED SORT RECORD                 
         MVC   SORTBNET,SAVEBNET   REPORT ONCE WITH RUNNING TOTAL               
         AP    SORTCNT,=P'1'       COUNT OF SORT RECORDS                        
*                                                                               
FIN10A   LA    R7,P                POINT TO PRINT LINE                          
         USING BLINED,R7           BLINED DSECT TO COVER PRINT LINE             
         MVC   BLMEDIA,SORTNMED    MEDIA                                        
         MVC   BLCLT,SORTCLT       CLIENT                                       
         MVC   BLCLNAME,SORTCNAM   CLIENT NAME                                  
         MVC   BLPRD,SORTPRD       PRODUCT                                      
         MVC   BLEST,SORTENAM      ESTIMATE                                     
         MVC   BLINVNO,SORTINVF    FULL INVOICE NUMBER                          
         MVC   BLINVD,SORTINVD     INVOICE DATE                                 
         EDIT  SORTINV$,(13,BLGROSS),2,MINUS=YES                                
         MVC   BLUCOMM,SORTCOM4    4TH ESTIMATE UCOMM                           
         MVC   BLCMADMA,SORTDMA    CMA/DMA CODE                                 
         CLC   SORTDMA,=C'COM5'    NETWORK RADIO                                
         BE    *+12                YES                                          
         CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   *+10                NO                                           
         MVC   BLCMADMA,SORTCOM5   YES - REPORT ESTIMATE UCOMM5                 
         BRAS  RE,PRNTLINE         SEE IF WE SHOULD PRINT THIS LINE             
         BNE   FIN10B              NO - IT'S A DUPLICATE                        
         GOTO1 REPORT              REPORT ON PRINT LINE                         
         DROP  R7                  DROP USING                                   
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
         MVC   0(2,R4),=C'"$'                                                   
*                                                                               
         CP    SORTINV$,=P'0'                                                   
         BL    FIN10N                                                           
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
         MVC   1(20,R4),SORTMEDN                                                
         LA    R4,20(R4)                                                        
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
         MVC   1(4,R4),SORTDMA     (MKTGROUP WW)                                
         CLC   SORTDMA,=C'COM5'    NETWORK RADIO                                
         BE    FIN41                                                            
         CLI   NETOPT,C'N'         OR IF NETWORK                                
         BNE   FIN42                                                            
FIN41    MVC   1(3,R4),SORTCOM5    ESTIMATE UCOMM5                              
*                                                                               
FIN42    MVC   4(2,R4),=C'",'                                                   
         LA    R4,6(R4)                                                         
         MVI   0(R4),C'"'                                                       
         MVC   1(10,R4),SORTSTA                                                 
         LA    R4,10(R4)                                                        
FIN45    CLI   0(R4),C' '    SCAN BACKWARD FOR NON-SPACE                        
         BH    FIN50                                                            
         BCT   R4,FIN45                                                         
*                                                                               
FIN50    MVC   1(2,R4),=C'",'                                                   
         LA    R4,3(R4)                                                         
         MVC   0(2,R4),=C'"$'                                                   
*                                                                               
         L     R0,SORTBNET                                                      
         C     R0,=F'0'                                                         
         BL    FIN50N                                                           
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
         BAS   RE,BLDREND                                                       
*                                                                               
         CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   FINLUP              NO - JUST GET NEXT SORT RECORD               
         CLI   LASTNET,C'Y'        LAST NET REC?                                
         BE    TOTALS              YES - GO PROCESS TOTALS                      
         LA    R5,SORTREC          POINT TO SORT RECORD                         
         MVC   SORTKYSV,SORTKEY    YES - SET SORTKYSV FIRST TIME IN             
         MVC   SORTRCSV,SORTREC    SAVE THE SORT RECORD                         
         MVC   SAVEBNET,SORTBNET   INIT RUNNING TOTALS                          
         B     FINLUP              GET NEXT SORT RECORD                         
         DROP  R5                  DROP SORT RECORD USING                       
*                                                                               
FIN50X   CLI   NETOPT,C'N'         NETWORK?                                     
         BNE   TOTALS              NO - GO TO TOTALS                            
         MVI   LASTNET,C'Y'        LAST NET REC FLAG                            
         B     FIN10AA             REPORT LAST INVOICE                          
*                                                                               
TOTALS   LA    RE,OUTREC           CLEAR OUTREC                                 
         LH    RF,=H'400'                                                       
         XCEF                                                                   
*                                                                               
         LA    R4,OUTREC                                                        
         MVI   0(R4),C'"'                                                       
         MVC   1(5,R4),=C'TRL",'                                                
         LA    R4,6(R4)                                                         
         MVC   0(4,R4),=C'"",'     EMPTY FIELD                                  
         LA    R4,3(R4)                                                         
         MVC   0(2,R4),=C'"$'                                                   
*                                                                               
         CP    BILTOT,=P'0'                                                     
         BL    TOT05N                                                           
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
         BAS   RE,BLDREND                                                       
*                                                                               
TOT30    MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,90                                                      
         CLI   NETOPT,C'N'                                                      
         BNE   *+8                                                              
         MVI   RCSUBPRG,110                                                     
*                                                                               
         AP    TRLCNT,=P'1'        TRAILER COUNT - ALWAYS 1                     
         LA    R4,TITLES                                                        
         LA    R5,HDRCNT                                                        
         LA    R3,4                FOR BCT                                      
TOT50    MVC   P1+7(17),0(R4)                                                   
         EDIT  (P4,0(R5)),(9,P1+26),0,COMMAS=YES                                
         GOTO1 APRNT,DMCB,(RC)                                                  
         LA    R4,17(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R3,TOT50                                                         
         GOTO1 APRNT,DMCB,(RC)     SKIP A LINE                                  
         MVC   P1+7(19),=C'FILE RECORDS OUTPUT'                                 
         EDIT  TOTCNT,(9,P1+26),0,COMMAS=YES                                    
         MVI   P1+35,C'*'                                                       
         GOTO1 APRNT,DMCB,(RC)                                                  
*                                                                               
         GOTO1 APRNT,DMCB,(RC) A LINE                                           
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
         GOTO1 APRNT,DMCB,(RC)                                                  
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
TOT60X   GOTO1 APRNT,DMCB,(RC)                                                  
*                                                                               
         CP    BILTOT,DTLTOT                                                    
         BE    TOT70                                                            
         GOTO1 APRNT,DMCB,(RC)                                                  
         MVC   P1+1(41),=C'*****************************************'           
         MVC   P2+1(41),=C'*** WARNING-INVOICE AND DETAIL $ MISMATCH'           
         MVC   P3+1(41),=C'*****************************************'           
         GOTO1 APRNT,DMCB,(RC)                                                  
TOT70    DS    0H                                                               
*                                                                               
         CLI   SVQOPT6,C'Y'        TEST RUN ?                                   
         BE    EXIT                YES - NO CLOSE                               
         CLOSE SAITAPE                                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
BLDREND  LR    R0,RE                                                            
         LA    RE,OUTREC-4                                                      
         LR    RF,R4                                                            
         SR    RF,RE      DIFFERENCE SHOULD BE RECORD LENGTH                    
         STH   RF,OUTREC-4                                                      
         BAS   RE,MWRITE                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
MWRITE   NTR1                      FIND RECORD LENGHT IN LENTAB                 
*                                                                               
         MVI   RCSUBPRG,10                                                      
         CLI   NETOPT,C'N'                                                      
         BNE   *+8                                                              
         MVI   RCSUBPRG,30                                                      
*                                                                               
         CLI   SVQOPT7,C'P'                                                     
         BE    WRIT1                                                            
         CLI   QOPT7,C'P'                                                       
         BNE   WRIT2                                                            
*                                                                               
WRIT1    MVC   P1+1(125),OUTREC                                                 
         MVC   P2+1(125),OUTREC+125                                             
         MVC   P3+1(125),OUTREC+250                                             
         MVC   P4+1(25),OUTREC+375                                              
         MVI   P2,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P3,0       SO LINE WILL ALWAYS PRINT                             
         MVI   P4,0       SO LINE WILL ALWAYS PRINT                             
*                                                                               
WRIT1A   MVI   SPACING,2                                                        
         GOTO1 APRNT,DMCB,(RC)                                                  
***      MVI   RCSUBPRG,10                                                      
***      GOTO1 HEXOUT,DMCB,OUTREC-4,P1+10,54,=C'N'                              
***      GOTO1 (RF),(R1),OUTREC+50,P2+18,50,=C'N'                               
***      GOTO1 (RF),(R1),OUTREC+100,P3+18,50,=C'N'                              
***      GOTO1 (RF),(R1),OUTREC+150,P4+18,50,=C'N'                              
***      GOTO1 (RF),(R1),OUTREC+200,P5+18,50,=C'N'                              
***      GOTO1 (RF),(R1),OUTREC+250,P6+18,50,=C'N'                              
***      GOTO1 (RF),(R1),OUTREC+300,P7+18,50,=C'N'                              
***      GOTO1 (RF),(R1),OUTREC+350,P8+18,50,=C'N'                              
***T1C   MVC   P1+1(7),=C'001-050'                                              
***      MVC   P2+1(7),=C'051-100'                                              
***      MVC   P3+1(7),=C'101-150'                                              
***      MVC   P4+1(7),=C'151-200'                                              
***      MVC   P5+1(7),=C'201-250'                                              
***      MVC   P6+1(7),=C'251-300'                                              
***      MVC   P7+1(7),=C'301-350'                                              
***      MVC   P8+1(7),=C'351-400'                                              
***T1E   MVI   SPACING,2                                                        
***      GOTO1 APRNT,DMCB,(RC)                                                  
WRIT2    DS    0H                                                               
         CLI   SVQOPT6,C'Y'     SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         LAY   R1,SAITAPE                                                       
*                                                                               
WRIT2B   LA    R0,OUTREC-4                                                      
         PRINT GEN                                                              
         PUT   (1),(0)                                                          
         PRINT NOGEN                                                            
WRIT3    AP    TOTCNT,=P'1'                                                     
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
ACONS    DS    0F                                                               
         DC    A(PRNT)                                                          
         DC    A(PRBILL)                                                        
         DC    A(STABUCKC)                                                      
         DC    V(SPFMTINO)                                                      
         DC    V(GETUSER)                                                       
         DC    A(PROCNET)                                                       
         DC    A(NETBLK)                                                        
         DC    V(NETNET)                                                        
         DC    V(NETACC)                                                        
         DC    A(RNBILL)                                                        
         DC    V(SORTER)                                                        
         DC    V(BINSRCH)                                                       
         DC    V(DDUCOM)                                                        
ACONSX   EQU   *                                                                
*                                                                               
TITLES   DS    0C                                                               
         DC    CL17'COLUMN HEADERS'                                             
         DC    CL17'INVOICES'                                                   
         DC    CL17'LINE ITEMS'                                                 
         DC    CL17'TRAILERS'                                                   
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=228'                                   
*                                                                               
         LTORG                                                                  
*                                                                               
SPDYNDSN DC    CL20'SPTTAPE.SP0AIAG1'                                           
NEDYNDSN DC    CL20'NETTAPE.NE0AIAG1'                                           
*                                                                               
SAITAPE  DCB   DDNAME=SAITAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=400,                                              X        
               BLKSIZE=4000,                                           X        
               MACRF=PM                                                         
         EJECT                                                                  
*                                                                               
PRNTLINE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING BLINED,R7           BLINED DSECT TO COVER PRINT LINE             
         CLC   SVBLINE,BLINE       SAME AS PREVIOUS RECORD PRINTED?             
         BNE   PL50                NO                                           
         CLI   NETOPT,C'N'         NETPAK?                                      
         BE    PLNEQ               YES - SET CC NEQ TO NOT PRINT LINE           
*                                                                               
         LA    RE,SVCMADMA         LIST OF SAVED CMA/DMA CODES                  
         LA    RF,MAXCMA           MAX CMA/DMA CODES IN THE LIST                
*                                                                               
PL10     OC    0(3,RE),0(RE)       EMPTY SLOT?                                  
         BZ    PL20                YES                                          
         CLC   0(3,RE),BLCMADMA    MATCH ON CMA/DMA CODE?                       
         BE    PLNEQ               YES - SET CC NEQ TO NOT PRINT LINE           
         LA    RE,3(RE)            BUMP TO NEXT SLOT                            
         BCT   RF,PL10             TRY NEXT SLOT                                
         DC    H'0'                EXPAND MAXCMA                                
*                                                                               
PL20     MVC   0(3,RE),BLCMADMA    SAVE THE CMA/DMA CODE                        
         B     PLEQU               PRINT IT                                     
*                                                                               
PL50     MVC   SVBLINE,BLINE       SAVE OFF LAST RECORD                         
         LA    RE,SVCMADMA         CLEAR SVCMADMA                               
         LH    RF,=AL2(MAXCMA*3)   LENGTH OF SVCMADMA                           
         XCEF                            SVCMADMA                               
*                                                                               
         MVC   SVCMADMA(4),BLCMADMA                                             
*                                                                               
PLEQU    CR    RE,RE               SET CC EQU                                   
         B     *+6                 RETURN                                       
PLNEQ    LTR   RE,RE               SET CC NOT EQ                                
         J     EXIT                RETURN                                       
         DROP  R7                  DROP USING                                   
*                                                                               
         LTORG                                                                  
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
         DC    C'"ATT REGION CLUSTERS"'       NO FINAL COMMA                    
COLHDX   EQU   *                                                                
COLHDLEN EQU   *-COLHDS                                                         
*                                                                               
         LTORG                                                                  
         TITLE 'RNBILL - READ SPOT BILLING DETAIL RECORDS'                      
RNBILL   CSECT                                                                  
         NMOD1 0,RNBILL                                                         
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPAIWRKD,R8                                                      
*                                                                               
*        RC AND RA FOR SPWORKD                                                  
*                                                                               
         SPACE 2                                                                
         LA    R0,BINKEY                                                        
         ST    R0,SBINVALS                                                      
         MVC   STTRECNT,SVBINCNT        SET RECORD COUNT                        
*                                                                               
         MVC   KEY1,KEY                 SAVE KEY                                
         XC    KEY,KEY                                                          
RNB2     DS    0H                                                               
         MVC   KEY(2),=X'0E01'          RECORD TYPE                             
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         CLI   BPRD,0                                                           
         BE    RNB3                SKIP REST OF KEY NOW IF MULTI-PRD            
         MVC   KEY+5(1),BPRD                                                    
RNB2EST  DS    0H                                                               
         MVC   KEY+6(1),BEST                                                    
         XC    KEY+7(6),KEY+7                                                   
         CLI   BEST,0                                                           
         BE    RNB3                SKIP REST OF KEY NOW IF MULTI-EST            
RNB2MKT  DS    0H                                                               
         MVC   KEY+7(2),KMKT                                                    
         XC    KEY+9(4),KEY+9                                                   
         OC    KMKT,KMKT                                                        
         BZ    RNB3                SKIP STA NOW IF MULTI-MKT                    
RNB2STA  DS    0H                                                               
         MVC   KEY+9(3),KSTA                                                    
RNB3     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RNB4B                                                            
RNB4     DS    0H                                                               
         GOTO1 SEQ                                                              
RNB4B    DS    0H                                                               
         CLC   KEY(5),KEYSAVE      AGM/CLT                                      
         BNE   RNB40                                                            
         CLI   BPRD,0                                                           
         BE    RNB5                                                             
         CLC   BPRD,KEY+5          ONE PROD MUST BE EQUAL                       
         BNE   RNB40                                                            
*                                                                               
RNB5     DS    0H                                                               
         CLI   BEST,0                                                           
         BE    RNB7                                                             
*                                  ONE EST OR SERIES                            
         CLC   KEY+6(1),BEST                                                    
         BL    RNB2EST                                                          
         BE    RNB7                                                             
         CLI   BESTEND,0                                                        
         BE    RNB6D                                                            
         CLC   KEY+6(1),BESTEND                                                 
         BNH   RNB7                                                             
*                                                                               
RNB6D    DS    0H                  EST NOT OK                                   
         CLI   BPRD,0                                                           
         BNE   RNB40               DONE IF ONE PRD                              
         IC    RF,KEY+5            ELSE NEXT PROD                               
         LA    RF,1(RF)                                                         
         STC   RF,KEY+5                                                         
         B     RNB2EST                                                          
*                                                                               
RNB6H    DS    0H                  BUMP TO NEXT EST                             
         ZIC   RF,KEY+6                                                         
         LA    RF,1(RF)                                                         
         STC   RF,KEY+6                                                         
         B     RNB2MKT                                                          
*                                                                               
RNB7     DS    0H                  MARKET                                       
         OC    KMKT,KMKT                                                        
         BZ    RNB8                                                             
         CLC   KEY+7(2),KMKT       ONE MKT                                      
         BE    RNB9                                                             
         BL    RNB2MKT                                                          
*                                  MKT HIGH                                     
*                                  IF MULT-EST BUMP TO NEXT EST                 
RNB7B    DS    0H                                                               
         CLI   BEST,0                                                           
         BE    RNB6H                                                            
         CLI   BESTEND,0                                                        
         BNE   RNB6H                                                            
         B     RNB6D               NEXT PRD (IF MULTI-PRD)                      
*                                                                               
RNB8     DS    0H                  MULTI-MKT                                    
RNB9     DS    0H                                                               
         OC    KSTA,KSTA           STATION                                      
         BZ    RNB10                                                            
         CLC   KSTA,KEY+9          ONE STA                                      
         BL    RNB2STA                                                          
         BH    RNB7B               NEXT EST -PRD (IF MULTI)                     
         SPACE 3                                                                
*                                  HAVE GOOD KEY                                
RNB10    DS    0H                                                               
*                                                                               
         L     R7,ADSTABUC                                                      
         ST    R7,AREC                                                          
         USING STABUCK,R7                                                       
         GOTO1 GET                                                              
*                                       FIND PRD CODE                           
         L     RF,VCLIST                                                        
*                                                                               
RNB10A   DS    0H                                                               
         CLC   STABKPRD,3(RF)                                                   
         BE    RNB10B                                                           
         LA    RF,4(RF)                                                         
         CLI   0(RF),0       END OF LIST?                                       
         BNE   RNB10A                                                           
         DC    H'0'          MUST FIND THE PRODUCT                              
*                                                                               
RNB10B   DS    0H                                                               
         MVC   MYBKPRD,0(RF)            SAVE PRODUCT                            
*                                                                               
         MVI   CKESTREC,C'K'          SET FROM BUCKET                           
         GOTO1 =A(CKEST),DMCB,(RC)                                              
*                                                                               
         MVI   ELCODE,X'0E'                                                     
         LA    R2,STABELEM                                                      
         USING STABELEM,R2                                                      
*                                                                               
RNB11    DS    0H                                                               
         CLC   ELCODE,0(R2)                                                     
         BE    RNB13                                                            
*                                                                               
RNB12    DS    0H                                                               
         BAS   RE,RNBNXTEL                                                      
         BNE   RNB30                                                            
*                                                                               
RNB13    DS    0H                                                               
*                                  TEST IN REQ PERIOD                           
         CLC   STABBDT,BQSTARTP                                                 
         BL    RNB12                                                            
         CLC   STABBDT,BQENDP                                                   
         BH    RNB12                                                            
*                                                                               
RNB13T   DS    0H                                                               
         XC    BINREC,BINREC            CLEAR OLD DATA                          
         LA    R4,SORTREC                                                       
         USING SORTRECD,R4                                                      
         MVC   SORTMED,BAGYMD                                                   
         MVC   SORTCLT,CLT                                                      
*                                                                               
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
         MVC   SORTCNAM,CNAME                                                   
         DROP  RF                                                               
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTENAM(3),DUB+6(2)                                             
         MVI   SORTENAM+3,X'40'         FORCE A SPACE IN BETWEEN                
         MVC   SORTENAM+4(20),EDESC     ESTIMATE NAME                           
         DROP  RF                                                               
*                                                                               
         OC    SORTENAM(L'SORTENAM),SPACES                                      
*                                                                               
         LA    RE,SORTENAM                                                      
         LA    RF,24                                                            
RNB22    CLI   0(RE),C','        CHANGE COMMAS TO SPACES                        
         BNE   RNB22C                                                           
         MVI   0(RE),C' '                                                       
         B     RNB23                                                            
*                                                                               
RNB22C   CLI   0(RE),C'&&'        CHANGE & TO /                                 
         BNE   RNB23                                                            
         MVI   0(RE),C'/'                                                       
         B     RNB23                                                            
*                                                                               
RNB23    LA    RE,1(RE)                                                         
         BCT   RF,RNB22                                                         
*                                                                               
         MVC   SORTINV(2),STABINV                                               
         NI    SORTINV,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                
         MVC   SORTPRD,MYBKPRD                                                  
         MVC   SORTPNAM(3),MYBKPRD    NOW JUST CODE                             
         MVC   SORTMEDN,=CL20'SPOT TV'                                          
         CLI   QMED,C'T'                                                        
         BE    RNB24                                                            
         MVC   SORTMEDN,=CL20'SPOT RADIO'                                       
         CLI   QMED,C'R'                                                        
         BE    RNB24                                                            
         MVC   SORTMEDN,=CL20'NETWORK RADIO'                                    
         CLI   QMED,C'X'                                                        
         BE    RNB24                                                            
         DC    H'0'         UNKNOWN MEDIA                                       
*                                                                               
RNB24    MVC   WORK+10(2),STABPER   YM                                          
         MVI   WORK+12,X'01'      SET DAY TO 1                                  
         GOTO1 DATCON,DMCB,(3,WORK+10),(20,WORK)                                
         LA    RE,SORTMOS                                                       
         MVC   0(2,RE),WORK+4        MM                                         
         MVI   2(RE),C'/'                                                       
         MVC   3(2,RE),WORK+6        DD                                         
         MVI   5(RE),C'/'                                                       
         MVC   6(4,RE),WORK          YYYY                                       
*                                                                               
         MVC   SORTBDAT,STABBDT                                                 
         GOTO1 MSUNPK,DMCB,STABKMKT,WORK,WORK+8                                 
         MVC   SORTSTA(4),WORK+8     STATION                                    
         MVI   SORTSTA+4,C'-'        DASH AFTER STATION                         
         MVC   SORTSTA+5(1),WORK+12  BAND FOR RADIO                             
         MVI   SORTSTA+6,C'M'        FOLLOWED BY "M'                            
         CLI   QMED,C'T'             MEDIA T?                                   
         BNE   RNB24A                NO                                         
         MVC   SORTSTA+5(2),=C'TV'   YES - ADD "TV" AFTER STATION               
         CLI   WORK+12,C'D'          IS THIS "DV"?                              
         BNE   *+8                   NO                                         
         MVI   SORTSTA+5,C'D'        YES - ADD "DV" AFTER STATION               
RNB24A   MVC   SORTBNET,STABNET                                                 
         XC    SORTNTWK,SORTNTWK    THIS IS FOR NET ONLY                        
*                                                                               
         L     R0,STABNET                                                       
         CVD   R0,DUB                                                           
         AP    DTLTOT,DUB           SUM OF DETAILS                              
*                                                                               
         XC    PPGKEY,PPGKEY        SAVE KEY OF STABUCK C                       
         L     RF,ADSTABUC                                                      
         MVC   PPGKEY(13),0(RF)                                                 
*                                                                               
         CLI   QMED,C'X'        NETWORK RADIO USE ECOM5                         
         BE    RNB25                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING MKARECD,R3                                                       
         MVC   MKAKTYP,=X'0D03'                                                 
         MVC   MKAKAGMD,BAGYMD                                                  
         MVC   MKAKMKT,STABKMKT                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GMGRNO                                                           
*                                                                               
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
*                                                                               
         L     R3,AREC                                                          
         LA    R3,MKAEL            FIRST ELEM                                   
*                                                                               
GMGR04   DS    0H                                                               
         CLI   0(R3),0             EOR                                          
         BE    GMGRNO                                                           
         CLI   0(R3),5             05 ELEMENT                                   
         BNE   GMGR20                                                           
         USING MKAEL05,R3                                                       
         CLI   MKAMGRP,X'EE'      SCHEME WW?                                    
         BNE   GMGR20                                                           
*                                                                               
*        GRPPCODE  IS PWOS                                                      
*                                                                               
         ICM   R1,B'1100',MKAMGRP+1                                             
         SRL   R1,12                  DD DD ?? ??  =>  00 0D DD D?              
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'           00 0D DD DS                               
         UNPK  CODECHAR(5),FULL+1(3)               =>  Z0 ZD ZD ZD ZD           
         LH    R1,=H'4'               THEY USE 4                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SORTDMA(0),CODECHAR+1  CODE TO SCREEN LINE BLANK PADDED          
         MVC   SAVDMA(4),SORTDMA                                                
         B     GMGR30                                                           
*                                                                               
GMGR20   DS    0H                                                               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GMGR04                                                           
*                                                                               
GMGRNO   MVC   SORTDMA,=C'    '       NOT FOUND                                 
*                                                                               
GMGR30   DS    0H                                                               
         DROP   R3                                                              
         B      RNB25X                                                          
*                                                                               
RNB25    MVC    SORTDMA,=C'COM5'   WILL BE ALTERD TO ECOMM5                     
*                                  IN THE SORT                                  
RNB25X   LA     R5,BINKEY                                                       
         USING  ATTKEYD,R5                                                      
         MVC    ATTMED,BAGYMD                                                   
         MVC    ATTCLI,CLT                                                      
         MVC    ATTPRO,MYBKPRD      FROM BILLING ELEMENT                        
         L      RF,ADEST                                                        
         USING  ESTHDR,RF                                                       
         MVC    ATTEST(1),EKEYEST                                               
         DROP   RF                                                              
         MVC    ATTINVYM,STABBDT       BILLING MONTH FROM ELEMENT               
         MVC    ATTINVN,SORTINV        INVOICE # FROM ELEMENT                   
*                                                                               
         LA     R0,BINKEY                                                       
         ST     R0,SBINVALS                                                     
         MVI    SBINVALS,0          SEARCH ONLY - DON'T ADD                     
         GOTO1  VBINS2,SBINVALS                                                 
         CLI    SBINVALS,X'01'       NOT FOUND - MUST DIE                       
         BNE    *+6                                                             
         DC     H'0'                SOMETHING VERY WRONG                        
*                                                                               
         L      RF,SBINVALS               ADDRESS OF FOUND RECORD               
         LA     RF,L'ATTKEY(RF)          PAST KEY                               
         MVI    SBINVALS,1           RESET TO ADD IF NOT FOUND                  
*                                                                               
         DROP   R5                                                              
*                                                                               
         MVC    SORTINV$,0(RF)                                                  
         MVC    SORTINVF,6(RF)                                                  
         MVC    SORTINVD,16(RF)    6+10                                         
         MVC    SORTCOM4,26(RF)    6+10+10                                      
         MVC    SORTCOM5,58(RF)    6+10+10+32                                   
         MVC    SORTNMED,QMED                                                   
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
         B     RNB12                                                            
*                                                                               
RNB30    DS    0H                                                               
         MVC   KEY,PPGKEY      RESTORE FOR SEQ READING                          
         GOTO1 HIGH                                                             
         B     RNB4                                                             
*                                                                               
RNB40    DS    0H                                                               
RNBX     DS    0H                                                               
         MVC   KEY,KEY1       SPONSOR'S KEY                                     
         GOTO1 HIGH           RESTORE FOR SEQ READ                              
         XIT1                                                                   
         SPACE 2                                                                
RNBNXTEL DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    RNBNXTL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     RNBNXTEL                                                         
RNBNXTL2 DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
         DROP  R4,R7                                                            
         LTORG                                                                  
SBINVALS DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(0)             WILL BE ADDRESS OF BINKEY                     
         DC    A(ATTTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
STTRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    F'104'             LEN OF RECORD ( 91 + 13(L'KEY) )              
         DC    F'13'              KEY SIZE                                      
         DC    F'2000'            MAX NUMBER OF RECORDS                         
*                                                                               
SAOFATTT DC    A(ATTTABLE)                                                      
         EJECT                                                                  
         TITLE 'PROCNET - PROCESS NETPAK UNIT RECORDS'                          
PROCNET  CSECT                                                                  
         NMOD1 0,PROCNET                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPAIWRKD,R8                                                      
*                                                                               
*        RC AND RA FOR SPWORKD                                                  
*                                                                               
*                                                                               
         L     R6,ANETBLK                                                       
         USING NETBLOCK,R6                                                      
*                                                                               
*                                                                               
         LA    RE,NETBLOCK                                                      
         LH    RF,=Y(NBBLKEND-NETBLOCK)                                         
         XCEF                                                                   
*                                                                               
*                                  SET SELECT OPTIONS                           
         MVI   NBUSER+13,C'N'     ALWAYS PASS PRE-EMPTED UNITS TO A8            
         MVC   NBSELAGY(3),QAGY    AGY/MED                                      
         MVC   NBSELCLI,CLT        CLT                                          
         MVC   NBSELPRD,=C'POL'    ALWAYS DO ALL PRDS                           
         MVC   NBSELEST(2),BEST    END ST/END                                   
         CLC   =C'NO',QEST                                                      
         BNE   *+10                                                             
         MVC   NBSELEFL,QESTEND    EST FILTER                                   
         MVC   NBSELSTR(12),=C'750101991231'                                    
         MVI   NBSELSTR+6,X'FB'                                                 
*                          THESE DATES ARE REALLY 1975 - 2019                   
*                          X'FC'  CAUSED NETIO TO RETURN                        
*                          END BEFORE START ERROR                               
*                          GOOD LUCK TO FUTURE GENERATIONS OF                   
*                          PROGRAMMERS                                          
*                                  SET DATA OPTIONS                             
         MVI   NBDATA,C'U'         UNITS                                        
         MVI   NBSEQ,C'D'          DATE SEQ                                     
         MVC   NBTRCOPT,RCTRACE    TRACE                                        
         MVI   NBFUNCT,NBFNORM     NORMAL FUNCTION                              
         MVI   NBSELPST,C'B'       PASS LOCKED PACKAGES                         
*                                                                               
         MVC   NBAIO,=A(VIRTLREC)  USE VIRTUAL REC AREA                         
         MVC   NBPRINT,PRINT                                                    
         MVC   NBLOADER,LOADER                                                  
         MVC   NBACOM,ACOMFACS                                                  
*                                                                               
         LA    R4,SORTREC                                                       
         USING SORTRECD,R4                                                      
         MVC   SORTMED,QMED                                                     
         MVC   SORTCLT,CLT                                                      
*                                                                               
NTU10    DS    0H                                                               
         GOTO1 NETIO,DMCB,NETBLOCK                                              
         CLI   NBERROR,NBINVEST                                                 
         BE    NTUXX                                                            
         CLI   NBERROR,NBINVPRD                                                 
         BE    NTUXX                                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BE    NTU12                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    NTUXX                                                            
         CLI   NBMODE,NBVALCLI     SEE IF I JUST VALIDATED CLIENT               
         BNE   NTU10                                                            
         MVI   NBUSER+13,C'N'      ALWAYS PASS PRE-EMPTED UNITS                 
         B     NTU10                                                            
***                                                                             
* PROCESS UNIT                                                                  
***                                                                             
NTU12    DS    0H                                                               
         LA    R0,BINKEY                                                        
         ST    R0,NBINVALS                                                      
         MVC   NTTRECNT,SVBINCNT    SET RECORD COUNT                            
*                                                                               
         L     R7,NBAIO                                                         
         USING NURECD,R7                                                        
*                                                                               
         CLI   NUPRD,0              SEE IF UNALLOCATED                          
         BE    NTU10                IF SO SKIP                                  
*                                                                               
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
         MVC   SORTCNAM,CNAME                                                   
         DROP  RF                                                               
*                                                                               
         MVI   CKESTREC,C'U'         SET FROM UNIT                              
         GOTO1 =A(CKEST),DMCB,(RC)                                              
*                                                                               
PROCN3   DS    0H                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTENAM(3),DUB+6(2)                                             
         MVI   SORTENAM+3,X'40'  FORCE A SPACE IN BETWEEN                       
         MVC   SORTENAM+4(20),EDESC                                             
*                                                                               
         LA    RE,SORTENAM                                                      
         LA    RF,24                                                            
ROB22    CLI   0(RE),C','        CHANGE COMMAS TO SPACES                        
         BNE   ROB22C                                                           
         MVI   0(RE),C' '                                                       
         B     ROB23                                                            
*                                                                               
ROB22C   CLI   0(RE),C'&&'       CHANGE & TO /                                  
         BNE   ROB23                                                            
         MVI   0(RE),C'/'                                                       
         B     ROB23                                                            
*                                                                               
ROB23    LA    RE,1(RE)                                                         
         BCT   RF,ROB22                                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
PROCN5   DS    0H                                                               
         MVC   SVNUPRD2,NUPRD2       SAVE SECOND PRODUCT                        
*                                                                               
*        PROCESS BILLING ELEMENTS                                               
*                                                                               
NTU12E   DS    0H                                                               
         LA    R2,NUDATA                                                        
         MVI   ELCODE,X'10'        BILL ELEM                                    
*                                                                               
NTU13    DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   NTU14                                                            
*                                                                               
         USING NUBILD,R2                                                        
         TM    NUBILST,X'20'       SEE IF UNBILLED                              
         BO    NTU13               YES SKIP THIS ELEM                           
         OC    NUBILDAT,NUBILDAT   SEE IF BILLED                                
         BZ    NTU13               NO SKIP                                      
         CLC   NUBILDAT,BQENDP     SEE IF BILLED AFTER END DATE                 
         BH    NTU13               YES BYPASS                                   
         CLC   NUBILDAT,BQSTARTP   SEE IF BILLED BEFORE START DATE              
         BL    NTU13               YES BYPASS                                   
         CLI   NUBILPRD,0          SEE IF EXPANDED PRODUCT                      
         BNE   NTU13B                                                           
         CLI   NUBILLEN,28                                                      
         BH    *+6                                                              
         DC    H'0'                LENGTH SHOULD BE AT LEAST 29                 
*                                  ELSE BAD ELEMENT                             
         MVC   WPRD3,NUBILPRC    (WILL BE NUBILPRC WHEN ELENA'S READY)          
         BAS   RE,CKPRD3           TEST NEED THIS PRODUCT                       
         BNE   NTU13                                                            
         B     NTU13BX                                                          
*                                                                               
NTU13B   DS    0H                                                               
         MVC   WPRD,NUBILPRD                                                    
         BAS   RE,CKPRD            TEST NEED THIS PRODUCT                       
         BNE   NTU13                                                            
*                                                                               
NTU13BX  DS    0H                                                               
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'U',NUBILEL),SPBVALD,0                             
*                                                                               
*        SET EFFECTIVE VALUES INTO ELEM                                         
*        CAN DO SINCE RECORD IS NOT WRITTEN BACK                                
*                                                                               
         MVC   NUBILGRS,SPBVEGRS                                                
         MVC   NUBILNET,SPBVENET                                                
*                                                                               
*                                                                               
*                                                                               
NTU13D   DS    0H                                                               
*              MUST GET PRD CODE FROM CLIENT HEADER                             
*              IF NUBILPRD IS NOT ZERO                                          
*                                                                               
         CLI   NUBILPRD,0      IF ZERO USE NUBILGR2+4 (3 CHAR PRD)              
         BNE   NTU13D2         (NUBILPRC - IN FUTURE)                           
         MVC   SORTPRD,NUBILPRC                                                 
         MVC   SORTPNAM(3),NUBILPRC   NOW JUST CODE                             
         B     NTU13DX                                                          
*                                                                               
NTU13D2  L     R5,VCLIST       COMBINED CLIST AND CLIST2                        
NTU13D5  CLC   3(1,R5),NUBILPRD                                                 
         BE    NTU13D8      FOUND                                               
         LA    R5,4(R5)                                                         
         CLI   0(R5),0      END OF LIST                                         
         BNE   NTU13D5                                                          
         DC    H'0'               MUST BE ABLE TO FIND THE PRODUCT              
*                                                                               
NTU13D8  MVC   SORTPRD,0(R5)                                                    
         MVC   SORTPNAM(3),0(R5)      NOW JUST CODE                             
*              PASS BILLING ELEM TO SORT                                        
NTU13DX  MVC   SORTBNET,NUBILNET    THEY BILL NET                               
*                                                                               
*                                                                               
         L     R0,NUBILNET                                                      
         CVD   R0,DUB                                                           
         AP    DTLTOT,DUB           SUM OF DETAILS                              
*                                                                               
         GOTO1 AFMTINO,DMCB,0,(C'P',NUBILNUM)                                   
         L     RE,DMCB+4                                                        
         MVC   SORTINV,0(RE)        BINARY INVOICE NUMBER RETURNED              
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(3,SORTBDAT)                            
*                                                                               
NTU13X   DS    0H                                                               
         MVC   SORTSTA(L'NUKNET),NUKNET  NETWORK                                
         MVC   SORTNTWK,NUKNET                                                  
*                                                                               
*        FIND INVOICE IN BINSRCH TABLE                                          
*        AND  PUT RECORD TO SORT                                                
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         LA     R5,BINKEY                                                       
         USING  ATTKEYD,R5                                                      
         MVC    ATTMED,BAGYMD                                                   
         MVC    ATTCLI,CLT                                                      
         MVC    ATTPRO,SORTPRD     FROM BILLING ELEMENT                         
         MVC    ATTEST(1),EKEYEST                                               
         MVC    ATTINVYM,NUBILDAT     BILLING MONTH FROM ELEMENT                
         MVC    ATTINVN,SORTINV       INVOICE # FROM ELEMENT                    
         DROP   RF                                                              
*                                                                               
         LA     R0,BINKEY           KEY WE'RE SEARCHING FOR                     
         ST     R0,NBINVALS         PUT A(KEY) IN PARM1                         
         MVI    NBINVALS,0          SEARCH ONLY - DON'T ADD                     
         GOTO1  VBINS2,NBINVALS                                                 
         CLI    NBINVALS,X'01'       NOT FOUND - MUST DIE                       
         BNE    *+6                                                             
         DC     H'0'                SOMETHING VERY WRONG                        
*                                                                               
         L      RF,NBINVALS               ADDRESS OF FOUND RECORD               
         LA     RF,L'ATTKEY(RF)          PAST KEY                               
         MVI    NBINVALS,1           RESET TO ADD IF NOT FOUND                  
         DROP   R5                                                              
*                                                                               
         MVC    SORTINV$,0(RF)                                                  
         MVC    SORTINVF,6(RF)                                                  
         MVC    SORTINVD,16(RF)    6+10                                         
         MVC    SORTCOM4,26(RF)    6+10+10                                      
         MVC    SORTCOM5,58(RF)    6+10+10+32                                   
         MVC    SORTNMED,90(RF)                                                 
*                                                                               
         GOTO1  DATCON,DMCB,(2,NUKDATE),(3,WORK+20)                             
         MVC    SORTMEDN,=CL20'NETWORK'                                         
         CLI    SORTNMED,C'N'                                                   
         BE     NTU13XX                                                         
*                                                                               
         GOTO1  DATCON,DMCB,(2,NUKDATE),(0,WORK)                                
         GOTO1  GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                      
         GOTO1  DATCON,DMCB,(0,WORK+12),(3,WORK+20)                             
*                                                                               
         MVC    SORTMEDN,=CL20'CABLE'                                           
         CLI    SORTNMED,C'C'                                                   
         BE     NTU13XX                                                         
         MVC    SORTMEDN,=CL20'SYNDICATION'                                     
         CLI    SORTNMED,C'S'                                                   
         BE     NTU13XX                                                         
         CLI    SORTNMED,C' '                                                   
         BE     NTU13XX                                                         
         CLI    SORTNMED,X'0'                                                   
         BE     NTU13XX                                                         
         MVC    SORTMEDN,=CL20'OTHER'    ALL OTHERS                             
*                                                                               
NTU13XX  MVI    WORK+22,X'01'      SET DAY TO 1                                 
         MVC    SORTKMOS,WORK+20   MOS FOR KEY                                  
         GOTO1  DATCON,DMCB,(3,WORK+20),(20,WORK)                               
         LA     RE,SORTMOS                                                      
         MVC    0(2,RE),WORK+4        MM                                        
         MVI    2(RE),C'/'                                                      
         MVC    3(2,RE),WORK+6        DD                                        
         MVI    5(RE),C'/'                                                      
         MVC    6(4,RE),WORK          YYYY                                      
*                                                                               
         GOTO1  VSORTER,DMCB,=C'PUT',SORTREC                                    
         B      NTU13          GO DO NEXT BILLING ELEMENT                       
*                                                                               
NTU14    DS     0H             DONE WITH UNIT                                   
         B      NTU10                                                           
*                                                                               
         DROP   R6                COVERED NETBLOCK                              
         DROP   R7                                                              
         DROP   R4                                                              
*                                                                               
NTUXX    XIT1                      REST IS SAME AS SPOT                         
         SPACE 2                                                                
NXTEL    DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NXTEL2                                                           
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NXTEL                                                            
*                                                                               
NXTEL2   DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
*                                                                               
CKPRD    CLC   QPRD,=C'ALL'                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'   '                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'POL'                                                     
         BE    CKPYES                                                           
         CLC   WPRD,BPRD                                                        
         BE    CKPYES              PRD NOT OK - RETURN WITH CC NE               
         BR    RE                                                               
*                                                                               
CKPRD3   CLC   QPRD,=C'ALL'                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'   '                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'POL'                                                     
         BE    CKPYES                                                           
         CLC   WPRD3,PRD           MATCH 3 CHARACTER CODES                      
         BE    CKPYES              PRD NOT OK - RETURN WITH CC NE               
         BR    RE                                                               
*                                                                               
CKPYES   CR    RE,RE               PRD OK                                       
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
NBINVALS DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(0)             WILL BE ADDRESS OF BINKEY                     
         DC    A(ATTTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
NTTRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    F'104'             LEN OF RECORD ( 91 + 13(L'KEY) )              
         DC    F'13'              KEY SIZE                                      
         DC    F'2000'            MAX NUMBER OF RECORDS                         
*                                                                               
NAOFATTT DC    A(ATTTABLE)                                                      
         EJECT                                                                  
         TITLE 'PRBILL  - PROCESS  BILL RECORDS'                                
PRBILL   CSECT                                                                  
         NMOD1 0,PRBILL                                                         
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPAIWRKD,R8                                                      
*                                                                               
*        RC AND RA FOR SPWORKD                                                  
         TM    KEY+13,X'80'         SKIP DELETED                                
         BNZ   ROBX                                                             
         LA    R0,BINKEY                                                        
         ST    R0,BINVALS                                                       
*                                                                               
*                                                                               
******** CLC   PBILKBMN,MYSTRTB                                                 
******** BL    ROBX                                                             
******** CLC   PBILKBMN,MYENDB                                                  
******** BH    ROBX                                                             
*                                  PASS DATA TO SORT                            
ROB10    DS    0H                                                               
*                                                                               
         L     R6,ADBILL                                                        
         USING BILLREC,R6                                                       
*                                                                               
         CLC   BDATE,MYSTART                                                    
         BL    ROBX                                                             
         CLC   BDATE,MYEND                                                      
         BH    ROBX                                                             
         TM    BILSTAT,X'20'       SKIP AOR BILLS                               
         BNZ   ROBX                SKIP NET (USED FOR SOMETHING ELSE)           
*                                                                               
ROB10B   DS    0H                                                               
         MVI   CKESTREC,C'L'       FROM BILL                                    
         GOTO1 =A(CKEST),DMCB,(RC) MIGHT NEED TO RE-READ ESTIMATE               
*                                                                               
BILL20   DS    0H                  PROCESS BILL RECORD                          
*                                  USE BINSRCH TO ADD TO INVTAB                 
*                                                                               
*  AT THIS POINT MUST ADD INVOICES TO TABLE AND IF THERE IS A                   
*  DUPLICATE ADD THEM TOGETHER                                                  
*       CREATE KEY                                                              
*                                                                               
         LA     R5,BINKEY                                                       
         USING  ATTKEYD,R5                                                      
         MVC    ATTMED,BKEYAM         AGY/MED                                   
         MVC    ATTCLI,CLT            CHARACTERS                                
         MVC    ATTPRO,BKEYPRD                                                  
         MVC    ATTEST(1),BKEYEST                                               
         GOTO1  DATCON,DMCB,(0,BDATE),(2,ATTINVYM)                              
         MVC    ATTINVN,BKEYINV     (OVERWRITES DAY)                            
         DROP   R5                                                              
*                                                                               
*        CODE BELOW REFELECTS THE DATA IN THE FILE                              
*        THAT APPLYS TO ALL OUTPUT RECS FOR AN INVOICE                          
*                                                                               
         LA    R5,BINREC                                                        
         USING ATTRECD,R5                                                       
         MVI   ATTREC,C' '         SPACE-FILL OUTPUT RECORD                     
         MVC   ATTREC+1(L'ATTREC-1),ATTREC                                      
         MVC   ATTREC(6),BACTP    STORE AMOUNT DUE (NET FOR THEM)               
*                                                                               
BILL22   DS    0H                                                               
         LA    R1,B1PROF           B1 PROFILE                                   
         ST    R1,DMCB+8           PARM 3                                       
         MVC   DMCB+8(1),MED       MEDIA IS HOB OF PARM 4                       
         CLI   NETOPT,C'N'         NETPAK?                                      
         BNE   BILL23              NO                                           
         CLI   BLMED,C' '          MEDIA GIVEN?                                 
         BNH   BILL23              NO - USE MEDIA FROM MED                      
         MVC   DMCB+8(1),BLMED     YES - MEDIA IS HOB OF PARM 4                 
*                                                                               
BILL23   LA    R1,B1XPROF          B1X PROFILE                                  
         ST    R1,DMCB+12          PARM 4                                       
         ST    R6,DMCB+16          A(BILL HEADER RECORD)                        
*                                                                               
         GOTO1 AFMTINO,DMCB,(C'B',BDATE),(2,BKEYINV)                            
***      GOTO1 AFMTINO,DMCB,BDATE,(2,BKEYINV),(MED,B1PROF),B1XPROF              
*                                                                               
         L     RF,DMCB                                                          
         MVC   ATTINV,0(RF)          FULL INVOICE NUMBER                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,BQDATE),(20,WORK)                                 
         LA    R4,ATTINVD                                                       
         MVC   0(2,R4),WORK+4        MM                                         
         MVI   2(R4),C'/'                                                       
         MVC   3(2,R4),WORK+6        DD                                         
         MVI   5(R4),C'/'                                                       
         MVC   6(4,R4),WORK          YYYY                                       
         MVC   ATTECOM4,SVEUCOM4      4TH ESTIMATE UCOMM                        
         MVC   ATTECOM5,SVEUCOM5      5TH ESTIMATE UCOMM                        
         MVC   ATTNMED,BLMED          NETWORK SUBMEDIA                          
         CLI   BLMED,C' '             HAVE NETWORK SUBMED ON BILL REC?          
         BH    *+10                   YES                                       
         MVC   ATTNMED,MED            NO - USE MEDIA FROM MED                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         ZAP   DUB,BACTP                                                        
*                                                                               
         AP    BILTOT,DUB          ADD TO BILLING (INVOICE) SUM                 
*                                                                               
         L      R2,AOFATTT        ADDRESS OF ATTTABLE                           
         MVI    BINVALS,1        RESET TO INSERT RECORD IF NOT FOUND            
         GOTO1  VBINS2,BINVALS                                                  
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
         AP      0(6,RF),BACTP            ADD AMOUNT DUE                        
*                                         NET FOR THEM                          
GOTOXIT  DS    0H                                                               
         LA    R0,BINKEY                                                        
         ST    R0,BINVALS                                                       
         MVI   BINVALS,1                                                        
         MVC   SVBINCNT,ATTRECNT    SAVE AND USE IN RNBILL + NET                
*                                                                               
ROBX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         LTORG                                                                  
BINVALS  DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(0)             WILL BE ADDRESS OF BINKEY                     
         DC    A(ATTTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
ATTRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    F'104'             LEN OF RECORD ( 91 + 13(L'KEY) )              
         DC    F'13'              KEY SIZE                                      
         DC    F'2000'            MAX NUMBER OF RECORDS                         
*                                                                               
AOFATTT  DC    A(ATTTABLE)                                                      
*                                                                               
         TITLE 'CKEST - READ ESTIMATE FOR VARIOUS RECORDS'                      
CKEST    CSECT                                                                  
         NMOD1 0,CKEST                                                          
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPAIWRKD,R8                                                      
*                                                                               
*        RC AND RA FOR SPWORKD                                                  
*                                                                               
         DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
         MVC   PPGAREC,AREC                                                     
         MVI   CKESTSW,0    WILL BE SET TO X'01' IF I READ SOMETHING            
         XC    KEY,KEY                                                          
         L     RF,ADBUY                                                         
         USING BUYREC,RF                                                        
         MVC   KEY+1(1),BUYKAM    A/M                                           
         MVC   KEY+2(2),BUYKCLT   CLT                                           
         MVC   KEY+4(3),PRD       PRD                                           
         MVC   KEY+7(1),BUYKEST   EST                                           
         CLI   CKESTREC,C'B'        FROM SPOT BUY                               
         BE    CKEST3                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         XC    KEY,KEY                                                          
         L     RF,ADSTABUC                                                      
         USING STABUCK,RF                                                       
         MVC   KEY+1(1),STABKAM   A/M                                           
         MVC   KEY+2(2),STABKCLT  CLT                                           
         MVC   KEY+4(3),MYBKPRD   PRD                                           
         MVC   KEY+7(1),STABKEST  EST                                           
         CLI   CKESTREC,C'K'        FROM STATION BUCKET REC                     
         BE    CKEST3                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     RF,ADBILL                                                        
         USING BILLREC,RF                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(8),BILLREC      00/A/M/CLT/PRD/EST                           
         CLI   CKESTREC,C'L'        FROM BILL                                   
         BE    CKEST3                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R4,ANETBLK                                                       
         USING NETBLOCK,R4                                                      
         L     RF,NBAIO                                                         
         USING NURECD,RF                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NUKAM                                                   
         MVC   KEY+2(2),NUKCLT                                                  
         MVC   KEY+4(3),=C'POL'     PRODUCT POL                                 
         MVC   KEY+7(1),NUKEST                                                  
         CLI   CKESTREC,C'U'        FROM A UNIT                                 
         BE    CKEST3                                                           
*                                                                               
         DC    H'0'                 ERROR - UNKNOWN RECORD TYPE                 
*                                                                               
         DROP  R4                                                               
         DROP  RF                                                               
*                                                                               
CKEST3   L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         CLC   ESTHDR(8),KEY        SEE IF I ALREADY HAVE EST                   
         BE    CKEST5                                                           
         MVI   CKESTSW,1                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                EST MUST BE ON FILE                          
         GOTO1 GETEST                                                           
***                                                                             
* now we've just clobbered the bill record because the estimate buffer          
* (SPESTBUF) in SPREPFILEC is only definied as 1000X'00' while the              
* estimate record is more than likely 1,274 bytes (as of 9/26/16                
* anyway) and will blow away the start of the billing record (SPBILBUF)         
* which comes right after it.                                                   
***                                                                             
         CLI   CKESTREC,C'L'       PROCESSING BILLS?                            
         BNE   CKEST5              NO                                           
         MVC   KEY(13),PPGKEY      SAVED BILL KEY                               
*                                                                               
         GOTO1 HIGH                READ HIGH FOR THE BILL KEY                   
*                                                                               
         CLC   KEY(13),KEYSAVE     IS THE BILL THERE?                           
         BE    *+6                 YES                                          
         DC    H'0'                BILL MUST BE ON FILE                         
*                                                                               
         GOTO1 GETBILL             ADBILL IS NOW RESTORED AFTER GETEST          
*                                                                               
CKEST5   DS    0H                                                               
         CLI   CKESTREC,C'L'       READING FOR A BILL?                          
         BNE   CKEST80                                                          
         MVI   CKESTSW,1                                                        
         LA    R3,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         USING DDUCOMD,R3                                                       
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   UCSYS,C'N'        SYSTEM TO PRINT (NET)                          
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   UCSAM,EKEYAM      AGENCY/MEDIA                                   
         MVC   UCSCLT,EKEYCLT    PACKED CLIENT                                  
         MVC   UCPRD,EKEYPRD     PRD CODE                                       
         MVC   UCSEST,EKEYEST    ESTIMATE                                       
         CLC   SVUCKEY,UCSAM     DID I JUST CHECK THIS?                         
         BE    CKEST80                                                          
*                                                                               
         MVC   SVUCKEY,UCSAM                                                    
         MVC   SVEUCOM1,SPACES   INIT IN CASE OF ERROR                          
         MVC   SVEUCOM2,SPACES   INIT IN CASE OF ERROR                          
         MVC   SVEUCOM3,SPACES   INIT IN CASE OF ERROR                          
         MVC   SVEUCOM4,SPACES   INIT IN CASE OF ERROR                          
         MVC   SVEUCOM5,SPACES   INIT IN CASE OF ERROR                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         OI    UCOPT,UCO8EST     RETURN 8 ESTIMATE UCOMMS                       
         GOTO1 =V(DDUCOM),UCOMBLK                                               
*                                                                               
         CLI   UCERROR,0                                                        
         BNE   BILL21E      ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BO    BILL21E      NO EST DATA                                         
*                                                                               
         L     R1,UCEDATA     EST DATA                                          
         LA    RF,UCELENS     LENGTHS                                           
         LA    RE,SVEUCOM1    START OF 1ST EST UCOMM                            
         LHI   R0,4           4 UCOMMS                                          
BILL21C  CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    *+16                                                             
         MVC   0(32,RE),0(R1)                                                   
         OC    0(32,RE),=32C' '                                                 
         LA    RE,32(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,BILL21C                                                       
*                                                                               
*        NOW GET 5TH ESTIMATE UCOMM IN PRODUCT DATA                             
*                                                                               
         L     R1,UCPDATA     EST DATA IN PRODUCT                               
         LA    RF,UCPLENS     LENGTHS                                           
         LA    RE,SVEUCOM5    START AT 5TH EST UCOMM                            
         LHI   R0,1           1 UCOMMS - JUST THE 5TH EST ONE                   
BILL21C5 CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    *+16                                                             
         MVC   0(32,RE),0(R1)                                                   
         OC    0(32,RE),=32C' '                                                 
         LA    RE,32(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,BILL21C5                                                      
         B     BILL21X                                                          
*                                                                               
         DROP  R3                                                               
*                                                                               
BILL21E  DS    0H                                                               
*                                                                               
BILL21X  DS    0H                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   NETOPT,C'N'                                                      
         BNE   *+8                                                              
         MVI   RCSUBPRG,20                                                      
*                                                                               
         CLC   SVEUCOM4,SPACES                                                  
         BH    BILL21X5                                                         
         MVC   P1(23),=C'*** MISSING EST. UCOMM4'                               
         MVC   P1+26(1),QMED                                                    
         MVC   P1+28(3),CLT                                                     
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   P1+32(3),EKEYPRD                                                 
         ZIC   R0,EKEYEST                                                       
         DROP  RF                                                               
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+36(3),DUB+6(2)                                                
*                                                                               
         GOTO1 APRNT,DMCB,(RC)                                                  
*                                  USE BINSRCH TO ADD TO INVTAB                 
BILL21X5 CLC   SVEUCOM5,SPACES                                                  
         BH    BILL21X6                                                         
         CLI   QMED,C'T'          TV USES MKT GROUP                             
         BE    BILL21X6                                                         
         CLI   QMED,C'R'          RADIO UES MKT GROUP                           
         BE    BILL21X6                                                         
*                                                                               
         MVC   P1(23),=C'*** MISSING EST. UCOMM5'                               
         MVC   P1+26(1),QMED                                                    
         MVC   P1+28(3),CLT                                                     
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   P1+32(3),EKEYPRD                                                 
         ZIC   R0,EKEYEST                                                       
         DROP  RF                                                               
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+36(3),DUB+6(2)                                                
         MVI   RCSUBPRG,0                                                       
         CLI   NETOPT,C'N'                                                      
         BNE   *+8                                                              
         MVI   RCSUBPRG,20                                                      
*                                                                               
         GOTO1 APRNT,DMCB,(RC)                                                  
BILL21X6 DS    0H                                                               
******************************************************************              
CKEST80  DS    0H                                                               
         MVC   KEY,PPGKEY        RESTORE KEY                                    
         MVC   AREC,PPGAREC                                                     
         CLI   CKESTSW,1          DID I READ SOMETHING?                         
         BNE   CKESTXX                                                          
         GOTO1 HIGH                                                             
CKESTXX  XIT1                                                                   
         LTORG                                                                  
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
         EJECT                                                                  
         TITLE 'PRNT - PRINT CONTROL MODULE'                                    
PRNT     CSECT                                                                  
         NMOD1 0,PRNT                                                           
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         LA    R8,SPACEND                                                       
         USING SPAIWRKD,R8                                                      
*                                                                               
*        RC AND RA FOR SPWORKD                                                  
*                                                                               
PRNT5    CLI   QOPT6,C'Y'              SEE IF TEST RUN                          
         BE    PRNT6                                                            
         CLI   SVQOPT6,C'Y'            SEE IF TEST RUN                          
         BNE   *+10                                                             
*                                                                               
PRNT6    MVC   HEAD1+05(12),=C'**TEST RUN**'                                    
*                                                                               
         MVC   HEAD3+46(4),=C'FROM'                                             
         GOTO1 DATCON,DMCB,(0,MYSTART),(5,HEAD3+51)                             
         MVC   HEAD3+60(2),=C'TO'                                               
         GOTO1 (RF),(R1),(0,MYEND),(5,HEAD3+63)                                 
         GOTO1 REPORT                                                           
*                                                                               
PRNTX    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
SPAI02   CSECT                                                                  
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
BLCMADMA DS    CL3                                                              
*                                                                               
*       BILLING WORK AREA DSECT                                                 
SPAIWRKD DSECT                                                                  
*                                                                               
*                    RELOACTED ADDRESSES                                        
RCONS    DS    0F                                                               
APRNT    DS    A                                                                
APRBILL  DS    A                                                                
ADSTABUC DS    A                                                                
AFMTINO  DS    A                                                                
VGETUSER DS    A                                                                
VPROCNET DS    A                                                                
ANETBLK  DS    A                                                                
ANETNET  DS    A                                                                
ANETACC  DS    A                                                                
ARNBILL  DS    A                                                                
VSORTER  DS    A                                                                
VBINS2   DS    A                                                                
VDDUCOM  DS    A                                                                
*                                                                               
ELCODE   DS    CL1                                                              
NETOPT   DS    CL1    N- IF NETPAK                                              
SVUCKEY  DS    CL8    UCOMM AGY/MED, CLT, PRD, EST                              
*                     FROM LAST UCOMM CALL                                      
INVPARS  DS    6F               FOR INVTAB BINSRCH                              
*                                                                               
INVMAX   EQU   10000            ALLOW 10000 INVOICES PER CLIENT                 
TODAYY   DS    CL8              YYYYMMDD                                        
MYSTART  DS    CL6               ORIGINAL START AND END                         
MYEND    DS    CL6                                                              
MYSTRTB  DS    CL3               ORIGINAL START AND END - BINARY                
MYENDB   DS    CL3                                                              
KPRD     DS    X                                                                
KMKT     DS    XL2                                                              
KSTA     DS    XL3                                                              
HOLDPRD  DS    XL1                                                              
HOLDPRD2 DS    XL1                                                              
SAVR1    DS    F                                                                
MYTODAY  DS    CL10             MM/DD/YYYY                                      
CKESTREC DS    CL1                                                              
MYBKPRD  DS    CL3                                                              
REQPRD   DS    XL1        SPONSOR'S BPRD                                        
PPGKEY   DS    CL32                                                             
PPGAREC  DS    A                                                                
BINKEY   DS    CL13       BINSRCH KEY                                           
BINREC   DS    CL91       BINSRCH RECORD DATA                                   
*                                                                               
SVBINCNT DS    F                                                                
         DS    F          FOR ALIGNMENT                                         
WPRD     DS    CL1                                                              
WPRD3    DS    CL3                                                              
SVNUPRD2 DS    CL1            SAVED NUPRD2                                      
SDUB     DS    D              USED FOR SHARES IN PROCNET                        
SNETDUB  DS    D              USED FOR SHARES IN PROCNET                        
SGRSDUB  DS    D              UESD FOR SHARES                                   
*                                                                               
NETDUB   DS    D                                                                
GRSDUB   DS    D                                                                
*                                                                               
NEACCNET DS    CL9         1 + 8 BYTE PACKED                                    
NEACCGRS DS    CL9         1 + 8 BYTE PACKED                                    
NEACCGOS DS    CL9         1 + 8 BYTE PACKED                                    
*                                                                               
MYFULL   DS    F                                                                
*                                                                               
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
SAVPRD   DS    X                                                                
*                                                                               
W        DS    CL132                                                            
*                                                                               
*        DEFAULT TO X'0000' AND X'FFFF'                                         
*        IF EST=ALL OR MISSING                                                  
SVQELOW  DS    H        EST LOW                                                 
SVQEHI   DS    H        EST HIGH - EQUAL TO LOW IF ONE EST                      
*                                                                               
SVBLINE  DS    CL(BKEYLENQ)                                                     
SVCMADMA DS    CL(3*MAXCMA) ROOM FOR UP TO 500 UNIQUE CMA CODES PER KEY         
MAXCMA   EQU   500                                                              
*                                                                               
SVBILEL  DS    F                                                                
SVEUCOM1 DS    CL32         SAVED 1ST ESTIMATE UCOMM                            
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
CKESTSW  DS    XL1                                                              
MYDUB    DS    D                                                                
WRKDATE  DS    CL8                                                              
SAVDMA   DS    CL4                                                              
CODECHAR DS    CL5                                                              
FRSTSW   DS    X                   Y=FIRST TIME THROUGH HAS HAPPENED            
         DS    F                                                                
OUTREC   DS    CL400                                                            
         DS    0D                                                               
SORTREC  DS    XL(SORTBLEN)                                                     
*                                                                               
SORTKYSV DS    XL(L'SORTKEY)    FOR NET WE'LL DO OUR OWN ROLL-UP                
SORTRCSV DS    XL(SORTBLEN)     SAVES THE LAST SORT RECORD FOR NET              
SAVEBNET DS    XL4              RUNNING TOTAL OF BILLED NET                     
FIRSTNET DS    CL1              FIRST NET RECORD FLAG (Y/N)                     
LASTNET  DS    CL1              LAST NET RECORD FLAG (Y/N)                      
*                                                                               
       ++INCLUDE SPBVALD                                                        
*                                                                               
SORTRECD DSECT                                                                  
SORTKEY  DS    0XL19                                                            
SORTMED  DS    CL1              AGY/MED                                         
SORTCLT  DS    CL3                                                              
SORTPRD  DS    CL3                                                              
SORTINV  DS    XL2              INVOICE NO                                      
SORTBDAT DS    XL3              BILLED DATE (MAY NOT NEED)                      
SORTNTWK DS    XL4              NETWORK FOR MEDIA N REQUESTS                    
SORTKMOS DS    XL3              MOS FOR MEDIA N REQUESTS                        
*                                                                               
*  NEXT SEVERAL FIELDS FROM BINSRCH INVOICE DATA                                
*                                                                               
SORTINV$ DS    PL6              FULL INVOICE $                                  
SORTINVF DS    CL10             FULL INVOICE #                                  
SORTINVD DS    CL10             DATE ON INVOICE                                 
SORTCOM4 DS    CL32             4TH ESTIMATE UCOMM                              
SORTCOM5 DS    CL32             5TH ESTIMATE UCOMM                              
SORTNMED DS    CL1                                                              
*                                                                               
*   FIELDS BELOW FOR EACH BILLING ELEMENT                                       
*                                                                               
SORTENAM DS    CL24             EST # AND NAME                                  
SORTPNAM DS    CL24             PRD CODE AND NAME                               
SORTMEDN DS    CL20             MEDIA NAME                                      
SORTMOS  DS    CL10             BUY BILLABLE DATE (DAY SET TO 1)                
SORTDMA  DS    CL4              MKT GROUP (SCHEME WW)                           
SORTSTA  DS    CL10             STATION OR NETWORK                              
         DS    XL2              FOR FULLWORD ALIGNMENT OF SORTBNET              
SORTBNET DS    F                BILLED NET FROM ELEMENT                         
SORTCNAM DS    CL20             CLIENT NAME                                     
SORTBLEN EQU   *-SORTKEY                                                        
*                                                                               
ATTKEYD  DSECT                                                                  
ATTKEY   DS    0XL13                                                            
ATTMED   DS    CL1                                                              
ATTCLI   DS    CL3                                                              
ATTPRO   DS    CL3                                                              
ATTEST   DS    XL2                                                              
ATTINVYM DS    XL2           INVOICE YYMMDD COMPRESSED                          
ATTINVN  DS    XL2           INVOICE NUMBER                                     
*            ______                                                             
*              13                                                               
ATTRECD  DSECT                                                                  
ATTREC   DS    0CL91                                                            
ATTNET   DS    PL6           AMOUNT DUE (NET FOR THEM)                          
ATTINV   DS    CL10          INVOICE # AS ON BILL                               
ATTINVD  DS    CL10          INVOICE DATE AS ON BILL                            
ATTECOM4 DS    CL32          4TH ESTIMATE UCOMM                                 
ATTECOM5 DS    CL32          5TH ESTIMATE UCOMM                                 
ATTNMED  DS    CL1           NETWORK MEDIA (FROM BILL HEADER)                   
ENDATTR  DS    0C                                                               
*                                                                               
ATTTABLE CSECT                                                                  
         DS    2000CL104                                                        
*                                                                               
*                                                                               
VIRTLREC CSECT                                                                  
         DS    20000C                                                           
*                                                                               
NETBLK   CSECT                                                                  
         DS    1200C                                                            
*                                                                               
STABUCKC CSECT                                                                  
         DS    2000C                                                            
*                                                                               
NETBLKD  DSECT                                                                  
*                                                                               
       ++INCLUDE NENETRATED                                                     
         PRINT OFF                                                              
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDREPMASTD                                                     
*                                                                               
         PRINT ON                                                               
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE SPGENMKA                                                       
       ++INCLUDE DDUCOMD                                                        
QOPT6    EQU   QGRP                                                             
QOPT7    EQU   QGRP+1                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPAI02 11/08/16'                                      
         END                                                                    
