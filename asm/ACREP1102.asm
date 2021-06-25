*          DATA SET ACREP1102  AT LEVEL 044 AS OF 04/06/05                      
*PHASE AC1102A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'DDS BILLING DOWNLOAD RECORDS'                                   
AC1102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC11**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC11D,RC                                                         
         USING BILDWND,DWNLINE                                                  
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEVAFRST                                                    
         BE    LEVA                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PRCTRN                                                           
         CLI   MODE,LEVALAST                                                    
         BE    LEVAL                                                            
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  RUN FIRST - SET BUFFALO, INITIALIZE DOWNLOAD                                 
******************************************************************              
*                                                                               
RUNF     MVC   VTYPES(VTYPLNQ),ADCONS     RELOCATE ADDRESSES                    
         L     RF,GETOPT           A(GETOPT)                                    
         MVC   0(2,RF),=X'07FE'    BR   RE                                      
*                                                                               
         XC    RCPFLG,RCPFLG                                                    
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  REQUEST FIRST                                                                
******************************************************************              
*                                                                               
REQF     DS    0H                                                               
         MVI   RCREQREP,C'N'      DONT PRINT ADDITIONAL REQUEST PAGES           
         OI    RCPFLG,NDAGYLI        NEED AGENCY LINE                           
         GOTO1 ADWNL,DMCB,(RC),DWNINIT    INITIALIZE THE DOWNLOAD               
*                                                                               
         TM    RCPFLG,SYSNOOUT    SYSTEM LINE PUT OUT?                          
         BO    REQF05             YES, SKIP                                     
         OI    RCPFLG,SYSNOOUT                                                  
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(8),=CL8'*SYSNUM='   SE NUMBER                             
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         EDIT  MCIDSENO,(3,DWNFLD+8),FILL=0,ZERO=NOBLANK                        
         DROP  R2                                                               
         MVI   PRTSIZE,11                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
REQF05   MVI   FCSUPOFC,C'Y'         SUPPRESS OFFICE FILTERING                  
         CLI   QOPT4,C'O'            OFFICE FILTERING                           
         BNE   *+8                                                              
         MVI   FCSUPOFC,C'N'         YES, OFFICE FILTERING                      
*                                                                               
         MVI   OFFSW,C'N'                                                       
         MVI   NEWOFF,C'N'                                                      
         L     RF,ADCMPEL                                                       
         USING ACCOMPD,RF                                                       
         TM    ACMPSTAT,X'20'                                                   
         BZ    *+8                                                              
         MVI   OFFSW,C'Y'                                                       
         TM    ACMPSTA4,X'01'      TEST COMPANY ON NEW OFFICES                  
         BZ    *+8                                                              
         MVI   NEWOFF,C'Y'                                                      
         CLI   QOPT3,C'Y'                                                       
         BNE   *+8                                                              
         MVI   OFFSW,C'Y'                                                       
*                                                                               
         MVC   MOS(1),QSTART+1                                                  
         MVC   MOS+1(1),QSTART+3                                                
         CLI   QSTART+2,C'1'                                                    
         BNE   REQF10                                                           
         MVI   MOS+1,C'A'                                                       
         CLI   QSTART+3,C'0'                                                    
         BE    REQF10                                                           
         MVI   MOS+1,C'B'                                                       
         CLI   QSTART+3,C'1'                                                    
         BE    REQF10                                                           
         MVI   MOS+1,C'C'                                                       
*                                                                               
REQF10   GOTO1 DATCON,DMCB,(0,QSTART),(9,MONTH)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(1,DATE3)                                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  LEVEL A FIRST - GET OFFICE, CLIENT CODE AND NAME                             
******************************************************************              
*                                                                               
LEVA     NI    RCPFLG,X'FF'-NDRECAP                                             
         ZAP   BDWNGBA,=P'0'                                                    
*                                                                               
         L     R7,ADHEIRA                                                       
         MVC   BUFCLI,3(R7)          CLIENT CODE                                
*                                                                               
         MVC   BUFOFF,SPACES                                                    
         CLI   OFFSW,C'Y'                                                       
         BNE   LEVA10                                                           
         L     RF,ADLVASUP                                                      
         USING ACPROFD,RF                                                       
         MVC   BUFOFF,ACPROFFC       CLIENT OFFICE                              
         DROP  RF                                                               
*                                                                               
LEVA10   MVC   BUFCOM,SPACES                                                    
         L     R7,ADLVANAM                                                      
         ZIC   RF,1(R7)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUFCOM(0),2(R7)        CLIENT NAME                               
*                                                                               
LEVAX    B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  PROCESS TRANSACTIONS                                                         
******************************************************************              
*                                                                               
PRCTRN   L     R2,ADTRANS                                                       
         USING TRANSD,R2                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   PRCTRNX                                                          
         CLC   TRNSANAL,=C'99'     FIND BILLING ELEMENTS                        
         BNE   PRCTRNX                                                          
         CLI   TRNSTYPE,X'13'      EXCLUDE TYPE 19 ONE SIDED POSTINGS           
         BE    PRCTRNX                                                          
         CLI   TRNSTYPE,X'01'      EXCLUDE TYPE 01 INVOICE/BILLABLE             
         BE    PRCTRNX                                                          
         CLI   TRNSTYPE,62         EXCLUDE TYPE 62                              
         BE    PRCTRNX                                                          
         MVC   SVDATE3,TRNSDATE                                                 
         LR    R5,R2                                                            
*                                                                               
PRTR10   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,1(R5)                    "                                    
         AR    R5,RF                       "                                    
         CLI   0(R5),X'00'         END OF RECORD ?                              
         BE    PRTR20              YES                                          
         CLI   0(R5),X'60'         NO, 60 ELEMENT ?                             
         BNE   PRTR10              NO, KEEP LOOKING                             
         USING TRSTATD,R5                                                       
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,SVDATE3)                             
*                                                                               
PRTR20   DS    0H                                                               
         CLC   DATE3,SVDATE3       TRANSACTION ADDED IN GIVEN MONTH             
         BNE   PRCTRNX                                                          
*                                                                               
         ZAP   MTRN,=P'0'                                                       
         OC    TRNSNARR+39(6),TRNSNARR+39   MEDIA COSTS INCLUDE                 
         BZ    *+10                                                             
         ZAP   MTRN,TRNSNARR+39(6)                                              
*                                                                               
         ZAP   GROSS,=P'0'                                                      
         AP    GROSS,TRNSAMNT         NET                                       
         AP    GROSS,TRNSNARR+15(6)   COMMISSION                                
         SP    GROSS,MTRN                                                       
*                                                                               
         L     R4,ADACC                                                         
*                                                                               
         MVC   BUFPRD,SPACES                                                    
         CLI   QOPT1,C'N'            EXCLUDE PRODUCT FROM OUTPUT?               
         BE    *+10                                                             
         MVC   BUFPRD,6(R4)          PRODUCT                                    
*                                                                               
         MVC   BUFMED,9(R4)          MEDIA                                      
         ZAP   BUFBIL,GROSS          GROSS BILL AMOUNT                          
         MVC   BUFAID,ALPHAID                                                   
         MVC   BUFDTE,SVDATE3                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
*                                                                               
         OI    RCPFLG,NDRECAP        HAS TRANS SO NEED A CLIENT RECAP           
         OI    RCPFLG,NDAGYLI        HAS TRANS SO NEED AGENCY LINE              
*                                                                               
PRCTRNX  B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*        CLIENT LAST - RECAP - DDS BILLING CLIENT NAME RECORDS                  
******************************************************************              
LEVAL    DS    0H                                                               
         TM    RCPFLG,NDRECAP                                                   
         BNO   LEVALX                                                           
*                                                                               
         MVC   BUFPRD,=X'FFFFFF'                                                
         MVC   BUFMED,=X'FFFFFF'                                                
         MVC   BUFDTE,=X'FFFFFF'                                                
         ZAP   BUFBIL,=P'1'          ADD TO ACCUM TO ENSURE RECORD              
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
*                                                                               
LEVALX   B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  REQUEST LAST - PRINT OFF DOWNLOAD RECORDS                                    
******************************************************************              
*                                                                               
REQL     DS    0H                                                               
         MVC   BUFKEY,SPACES                                                    
         NI    RCPFLG,X'FF'-NDRECAP                                             
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,1                            
REQL30   TM    DMCB+8,X'80'           ANY MORE RECORDS?                         
         BO    REQL50                                                           
*                                                                               
         CLC   BUFPRD,=X'FFFFFF'    IS THIS A CLIENT RECAP?                     
         BNE   REQL40               NO                                          
         TM    RCPFLG,NDRECAP       DETAIL OUPUT FOR THIS CLT YET?              
         BZ    REQL45                                                           
         NI    RCPFLG,X'FF'-NDRECAP                                             
         GOTO1 ADWNRTE2,DMCB,(RC)   YES, OUTPUT BILLING CLT NAME RECS           
         B     REQL45                                                           
*                                                                               
REQL40   BAS   RE,DETAIL            OUTPUT DDS BILLING DOWNLOAD RECS            
         OI    RCPFLG,NDRECAP       DETAIL OUPUT, NEED CLT RECAP                
*                                                                               
REQL45   GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,1                             
         B     REQL30                                                           
*                                                                               
REQL50   GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*  RUN LAST - END OF DOWNLOAD                                                   
******************************************************************              
*                                                                               
*                                                                               
RUNL     MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(11),=C'*END OF RUN' INDICATE END                          
         MVI   PRTSIZE,11                 LENGTH                                
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
         GOTO1 ADWNL,DMCB,(RC),DWNEOR     INITIALIZE THE DOWNLOAD               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
********************************************************************            
*  DETAIL - SET UP INFO FOR DOWNLOAD DETAIL RECORDS                             
******************************************************************              
*                                                                               
DETAIL   NTR1                                                                   
         MVC   DWNLINE,SPACES                                                   
*                                                                               
         MVI   BDWNRID,C'B'                                                     
         MVC   BDWNAID,BUFAID                                                   
         MVC   BDWNCLTC,BUFCLI        CLIENT                                    
         MVC   BDWNPRDC,BUFPRD        PRODUCT                                   
         MVC   BDWNMED,BUFMED         MEDIA (JOB)                               
         ZAP   BDWNGBA,BUFBIL         GROSS AMOUNT                              
         MVC   BDWNOFF,BUFOFF         OFFICE                                    
*                                                                               
         GOTO1 DATCON,DMCB,(1,BUFDTE),(23,WORK)                                 
         MVI   WORK+4,C'/'                                                      
         MVC   BDWNBLDT,WORK                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(1,DATE3),(23,WORK)                                  
         MVI   WORK+4,C'/'                                                      
         MVC   BDWNSRV,WORK                                                     
*                                                                               
         GOTO1 ADWNRTE1,DMCB,(RC)                                               
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* RELOCATABLES                                                        *         
***********************************************************************         
*                                                                               
ADCONS   DS    0F                                                               
         DC    A(DUMP)                                                          
         DC    A(BUFFALOC)                                                      
         DC    A(DWNL)             DOWNLOAD ROUTINE                             
         DC    A(DWNRTE1)          DOWNLOAD TOTALS ROUTINE                      
         DC    A(DWNRTE2)          DOWNLOAD RECAP ROUTINE                       
*                                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(DLFLD)            DOWNLOAD MODULE                              
         EJECT                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         BUFF  LINES=100,ROWS=1,COLUMNS=2,FLAVOR=PACKED,               X        
               COMMENT=37,KEYLIST=(13,A)                                        
********************************************************************            
* DOWNLOAD TOTALS                                                  *            
*     R2   - A(PACKED FIELDS FOR ALL SECTIONS EXCEPT A/R)          *            
*     R2   - FOR A/R A(PACKED FIELDS OR SRA BINTABLE IF BYTE=FF)   *            
*     R4   - A(PRINT LINE - A/R SECTION ONLY)                      *            
*     BYTE - TYPE OF TOTAL (CLI/DIV/CAT/RUN)                       *            
********************************************************************            
*                                                                               
DWNRTE1  DS    0D                                                               
         NMOD1 0,**DWNR**                                                       
         L     RC,0(R1)                                                         
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
*                                                                               
         TM    RCPFLG,NDAGYLI             NEED AGENCY LINE?                     
         BNO   DWNR110                    NO                                    
         NI    RCPFLG,X'FF'-NDAGYLI                                             
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,DWNFLD                                                        
         MVC   0(7,R3),=C'*AGENCY'        INDICATE COMPANY                      
         LA    R3,8(R3)                                                         
         MVC   0(L'ALPHAID,R3),ALPHAID    AGENCY ALPHA CODE                     
         LA    R3,L'ALPHAID+1(R3)                                               
         GOTO1 HEXOUT,DMCB,RCCOMPFL,(R3),1 AGENCY BINARY                        
         LA    R3,3(R3)                                                         
         MVC   0(L'ALPHAID,R3),ALPHAID    CORP IS AGY ALPHA                     
         LA    R3,L'ALPHAID(R3)                                                 
         LA    R4,DWNFLD                                                        
         SR    R3,R4                      GET LENGTH OF FIELD                   
         STC   R3,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
DWNR110  MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNRID),BDWNRID  RECORD IDENTIFIER                     
         LA    R1,L'BDWNRID               LENGTH                                
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNCID),BDWNCID  CORPORATE ID                          
         LA    R1,L'BDWNCID               LENGTH                                
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNAID),BDWNAID  AGENCY CODE                           
         LA    R1,L'BDWNAID               LENGTH                                
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVI   BDWNSYCD,C'A'                                                    
         MVC   DWNFLD(L'BDWNSYCD),BDWNSYCD SYSTEM CODE                          
         LA    R1,L'BDWNSYCD              LENGTH                                
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNMED),BDWNMED  MEDIA CODE                            
         LA    R1,L'BDWNMED               GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNOFF),BDWNOFF  OFFICE CODE                           
         LA    R1,L'BDWNOFF               GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNCLTC),BDWNCLTC CLIENT CODE                          
         LA    R1,L'BDWNCLTC              GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNPRDC),BDWNPRDC PRODUCT CODE                         
         LA    R1,L'BDWNPRDC              GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNEST),BDWNEST  ESTIMATE NUMBER                       
         LA    R1,L'BDWNEST               GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNBLDT),BDWNBLDT BILLING - YEAR/MONTH                 
         LA    R1,L'BDWNBLDT              GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNSRV),BDWNSRV  YEAR/MONTH OF SERVICE                 
         LA    R1,L'BDWNSRV               GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P10,BDWNGBA),(14,DWNFLD),0,ZERO=NOBLANK,MINUS=YES               
         LA    R1,14                      GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNNBA),BDWNNBA  NET BILL AMOUNT                       
         LA    R1,L'BDWNNBA               GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNSTF),BDWNSTF  SPOT TRAFFIC FLAG                     
         LA    R1,L'BDWNSTF               GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNSPAR),BDWNSPAR SPARE                                
         LA    R1,L'BDWNSPAR              GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* DOWNLOAD RECAP                                                   *            
*     R2   - A(PACKED FIELDS FOR ALL SECTIONS EXCEPT A/R)          *            
*     R2   - FOR A/R A(PACKED FIELDS OR SRA BINTABLE IF BYTE=FF)   *            
*     R4   - A(PRINT LINE - A/R SECTION ONLY)                      *            
*     BYTE - TYPE OF TOTAL (CLI/DIV/CAT/RUN)                       *            
********************************************************************            
*                                                                               
DWNRTE2  DS    0D                                                               
         NMOD1 0,**DWNR**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVI   DWNFLD,C'C'                RECORD IDENTIFIER                     
         LA    R1,1                       GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BDWNAID),BDWNAID  AGENCY CODE                           
         LA    R1,L'BDWNAID               LENGTH                                
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVI   BDWNSYCD,C'A'                                                    
         MVC   DWNFLD(L'BDWNSYCD),BDWNSYCD SYSTEM CODE                          
         LA    R1,L'BDWNSYCD              LENGTH                                
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R1,L'BDWNMED               GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BUFCLI),BUFCLI    CLIENT CODE                           
         LA    R1,L'BUFCLI                GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'BUFCOM),BUFCOM    CLIENT NAME                           
         LA    R1,L'BUFCOM                GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
         XMOD1                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* DOWNLOAD MODULE                                                   *           
*          PARM1 - RC                                               *           
*          PARM2 - ACTION                                           *           
*********************************************************************           
*                                                                               
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         LA    R5,DWNBUF                                                        
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWNL40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,P                PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'P)                                                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,X'FF'      NO HEADINGS FOR DOWNLOADING                  
*                                  TURN OFF DOWN-LOAD ROW FLDS AS C' '          
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVC   DLCBLEN,PRTSIZE     LEN OF FIELD                                 
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWNL40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,L'BDWNGBA   YES, USE MAXIMUM LENGTH OF NUMERICS          
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD    YES, MAKE SURE NUMERIC FLD NOT ZEROS          
         BNZ   DWNL50              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL50   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
*                                                                               
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
*                                                                               
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
*                                                                               
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
         LA    R5,=C'2D'                                                        
*                                                                               
         GOTO1 PRNTBL,DMCB,0,(R3),C'DUMP',(R4),(R5),(C'P',PRINT)                
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(ADDRESS OF DATA),(LENGTH OF DATA)               
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*              AGENCY TABLE                                                     
********************************************************************            
*                                                                               
*                                                                               
********************************************************************            
*              WORKING STORAGE DSECT                                            
********************************************************************            
*                                                                               
AC11D    DSECT                     WORKING STORAGE DSECT                        
*----------------------                                                         
VTYPES   DS    0A                                                               
ADUMP    DS    A                                                                
ADBUFC   DS    A                   BUFFALOC                                     
ADWNL    DS    A                   DOWNLOAD ROUTINE                             
ADWNRTE1 DS    A                   DOWNLOAD DETAIL ROUTINE                      
ADWNRTE2 DS    A                   DOWNLOAD RECAP ROUTINE                       
*                                                                               
PRNTBL   DS    V                   PRINT DATA                                   
DLFLD    DS    V                   DOWNLOAD MODULE                              
VTYPLNQ  EQU   *-VTYPES                                                         
*----------------------                                                         
MOS      DS    CL2                                                              
MONTH    DS    CL6                                                              
*                                                                               
OFFSW    DS    CL1                                                              
NEWOFF   DS    CL1                 Y=COMPANY ON NEW OFFICES                     
DATE3    DS    CL2                 Y/M ONLY                                     
         DS    CL1                                                              
SVDATE3  DS    CL3                                                              
         DS    CL1                                                              
*                                                                               
RCPFLG   DS    XL1                 RECAP FLAG                                   
NDRECAP  EQU   X'80'               CLT RECAP ONLY IF TRANS                      
NDAGYLI  EQU   X'40'               HAS TRANS SO NEED AGENCY LINE                
SYSNOOUT EQU   X'20'               SYSTEM NUMBER PUT OUT                        
*-------------------                                                            
BUFREC   DS    0CL58                                                            
BUFKEY   DS    0CL13                                                            
BUFAID   DS    CL2                 AGENCY ID                                    
BUFOFF   DS    CL2                 OFFICE                                       
BUFCLI   DS    CL3                 CLIENT CODE                                  
BUFPRD   DS    CL3                 PRODUCT CODE                                 
BUFMED   DS    CL1                 MEDIA                                        
BUFDTE   DS    0CL3                DATE                                         
         DS    CL2                 ONLY YEAR/MONTH IN KEY                       
         DS    CL1                 DAY                                          
BUFCOM   DS    CL36                CLIENT NAME                                  
BUFBIL   DS    PL8                 BILL AMOUNT                                  
*-------------------                                                            
GROSS    DS    PL8                                                              
MTRN     DS    PL6                                                              
*-------------------                                                            
DWNFLD   DS    CL36                SAVED AREA FOR FIELD TO BE DWNLOADED         
PRTSIZE  DS    CL1                 DOWNLOAD FLD PRINT SIZE FOR PADDING          
*                                                                               
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNINTZ  EQU   X'80'               DOWNLOAD INITIALIZED                         
DWNHDLN  EQU   X'40'               DOWNLOAD HEADLINES                           
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                    DOWN-LOAD INITIALIZATION                    
DWNEOL   EQU   2                    MARK END OF LINE                            
DWNEOR   EQU   3                    MARK END OF REPORT                          
DWNTEXT  EQU   4                    DOWN-LOAD TEXT                              
DWNNUM   EQU   5                    DOWN-LOAD NUMBER                            
DWNPACK  EQU   6                    DOWN-LOAD NUMBER (PACKED)                   
*                                                                               
DWNLINE  DS    CL(BILDWNQ)                                                      
*                                                                               
COMPLEN  DS    XL1                 COMPARE LENGTH                               
ELCODE   DS    XL1                 ELEMENT CODE FOR GETEL                       
*                                                                               
FLAG     DS    XL1                                                              
*                                                                               
DWNHDOPT DS    XL1                 DOWNLOAD ROUTINES HEADING OPTION             
DWNRG    EQU   X'80'               REGULAR RUN                                  
DWNRGOFF EQU   X'40'               REGULAR RUN BY OFFICE                        
DWNAP    EQU   X'20'               A/P BREAKDOWN                                
DWNAPOFF EQU   X'10'               A/P BREAKDOWN BY OFFICE                      
DWNAR    EQU   X'08'               A/R BREAKDOWN                                
*                                                                               
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DA       DS    F                   DISK ADDRESS                                 
*                                                                               
EOF      EQU   X'FF'               END OF FILE MARKER                           
*                                                                               
DWNBUF   DS    CL250               DOWNLOAD BUFFER                              
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
***********************************************************************         
*            DDS BILLING DOWNLOAD RECORD DSECT                                  
BILDWND  DSECT                                                                  
*                                                                               
BDWNRID  DS    CL1                 RECORD IDENTIFIER                            
BDWNCID  DS    CL2                 COPORATE ID                                  
BDWNAID  DS    CL2                 AGENCY CODE                                  
BDWNSYCD DS    CL1                 SYSTEM CODE                                  
BDWNMED  DS    CL1                 MEDIA CODE                                   
BDWNOFF  DS    CL2                 OFFICE CODE                                  
BDWNCLTC DS    CL3                 CLIENT CODE                                  
BDWNPRDC DS    CL3                 PRODUCT CODE                                 
BDWNEST  DS    CL3                 ESTIMATE NUMBER                              
BDWNBLDT DS    CL7                 BILLING - YEAR/MONTH                         
BDWNSRV  DS    CL7                 YEAR/MONTH OF SERVICE                        
BDWNGBA  DS    PL10                GROSS BILL AMOUNT                            
BDWNNBA  DS    PL10                NET BILL AMOUNT                              
BDWNSTF  DS    CL1                 SPOT TRAFFIC FLAG                            
BDWNSPAR DS    CL27                SPARE                                        
BILDWNQ  EQU    *-BILDWND                                                       
         EJECT                                                                  
*                                                                               
*        DDBUFFALOD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
*                                                                               
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*                                                                               
*        ACGENBOTH                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*                                                                               
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*                                                                               
*        ACGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*                                                                               
*        DDDLCB                                                                 
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
*                                                                               
*        ACREPPROFD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPPROFD                                                     
         PRINT ON                                                               
*                                                                               
*        DDMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044ACREP1102 04/06/05'                                      
         END                                                                    
